# Rule streaming

This document describes a new design for the production of build rules
in Dune. The new design aims to be more natural, easier to reason
about and to make existing features work well with newer ones such as
directory targets.

It was originally written by Jérémie Dimino as part of the
[streaming RFC](https://github.com/ocaml/dune/pull/5251), and later on moved
into the dev documentation.


## Problem

The [rule production](./rule-production.md) document exposes a concrete problem
with directory targets, but there is also a general sense of messiness in the
way things work. Generating rules for multiple directories at once is
natural, but the current encoding is odd.

## Proposal

The proposal is to add the following rule: `gen_rules ~dir` is allowed
to produced rules in `dir` or any of its descendant only. It is not
allowed to produce rules anywhere else.

`Load_rules.load_dir ~dir` will then always call itself recursively on
the parent of `dir` and take the union of the rules produced by
`gen_rules` for `dir` and the ones produced by the recursive
call. `gen_rules` will no longer have to redirect a call via
`Load_rules.load_dir_and_produce_its_rules`, which we would simply
remove.

This introduces a cycle with all `copy_files` stanza that copy files
from a sub-directory. We propose the break this cycle by introducing
laziness in the rule production code.

### Generating rules with a mask

The idea is that when we produce rules, we will produce rules under
a current active "mask" that tells us where we are allowed to generate
files or directories.  Trying to produce a rule with targets not
matched by this mask will be a runtime error.

When entering `gen_rules ~dir`, the initial mask will be: "any files
and directories that is a descendant of directory `dir`".

We can then narrow the mask to a sub-mask:

```ocaml
val narrow : Target_mask.t -> unit Memo.t -> unit Memo.t
```

With `narrow mask m`, `m` would only be allowed to produce rules whose
target are matched by the intersection of `mask` and the current
mask. `m` wouldn't be evaluated eagerly. Instead, `gen_rules` would
now return a set of direct rules as well as a list of
`(Target_mask.t * unit Memo.t)`. Let's call such a pair a
suspension. A suspension can be forced by evaluation its second
component. Doing so will yield a list of rules matched by the mask and
a new list of suspension.

### Staged rules loading

The next step is to stage `Load_rules.load_dir`. In addition to taking
a directory, `load_dir` will now also take a mask and will return the
set of rules for this mask. To do that, it might need to force a bunch of
suspensions recursively.


### How does that help?

We will put `copy_rules` under a `narrow <only file targets in current
dir>`. In order to determine if a directory is part of a directory
target in an ancestor directory, we wouldn't need to force this
suspension.

### Difficulties

Interpreting a `library` stanza requires knowing the set of `.ml`
files in the current directory. Knowing this requires interpreting
`copy_files` in the current directory. So the interpretation of
`library` stanzas will need to go under a `narrow` as well.

## Pull-based recursive rule loading

The implementation represents `Rules.t` as a tree of direct rules and memoized
suspended producers. Each suspension carries a `Target_mask.t` describing the
file targets, directory targets, and aliases it may produce. Masks combine
exact paths, subtrees, directory-local regions, filename predicates, and
extension families. Output declarations live beside their rule generators;
the engine does not impose a fixed source/compilation split.

`Rules.narrow mask (fun () -> ...)` registers a suspension. Forcing it may
produce both rules and further suspensions. Every enclosing mask applies to
the resulting outputs, and emitting a rule or alias outside their
intersection is an internal error. `Rules.defer` additionally exposes the
producer's memoized result, allowing compilation contexts to be shared without
generating their rules twice.

Each directory inherits its parent's rule tree and adds its own producers.
Rules for descendants are therefore available without redirecting directory
loading back to their generating ancestor. Generators remain restricted to
their own directory and its descendants.

Target, alias, and file-selection requests have separate masks. `Rules.load`
recursively forces only suspensions whose masks intersect the request.
Selecting a multi-target rule extends the request to its other outputs, then
repeats this process until all overlapping producers are loaded. This keeps
duplicate-rule checks and file/directory collisions independent of which
output was requested first. Source copies, promotion, and fallback retain the
same validation as complete loading.

Globs inside a directory target first materialize that target with a
directory-only request. File producers that depend on its contents, such as
`copy_files`, need not be evaluated before the directory exists. Ordinary
target requests still check file/directory conflicts after discovery; file
outputs of mixed file/directory rules retain the full multi-target closure.

Physical source discovery is separate from module selection and directory
mapping expansion. Module lists and directory mappings can therefore read
source files or generated inputs from their own directory or
`include_subdirs` group. File-selection requests also let `copy_files`
inspect matching outputs without loading unrelated compilation rules.
Generators such as Menhir and library subsystems participate through their
own declarations and suspensions. Dependencies on the compilation that
consumes these inputs remain genuine cycles.

Cleanup only considers entries present when a directory is first loaded. It
conservatively retains possible outputs of unforced suspensions and refines
that set as rules are revealed, without deleting fresh temporary files from
running actions. Full compatibility with unsandboxed actions is intentionally
deferred: a scratch path left by an earlier build can still be recreated before
a later query removes it. There is no language-version gate.
