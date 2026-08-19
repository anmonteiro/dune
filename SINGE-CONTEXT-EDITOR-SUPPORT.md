# Single-context editor support for OCaml and Melange

## Goal

A source file should not have an editor-selected compilation mode.

- An OCaml-only file is analyzed with its OCaml configuration.
- A Melange-only file is analyzed with its Melange configuration.
- A file compiled by both modes is analyzed with both configurations. The LSP
  combines the results according to the semantics of each feature.
- Existing clients that only understand one Merlin configuration continue to
  receive the applicable default configuration.

This replaces the earlier mode-switching design. In particular, there is no
active mode in document state and no user-facing request for selecting one.

The design is based on the discussion in the merged
[Dune PR #15493](https://github.com/ocaml/dune/pull/15493) and lessons from the
superseded prototype in
[Dune PR #14821](https://github.com/ocaml/dune/pull/14821).

## Meaning of “multiple results”

The right operation is not always a union. There are four result classes:

1. **Observational lists** use a normalized union. Definitions, references,
   diagnostics, and inlay hints can represent results from both modes.
2. **Single presentation values** combine the mode results into one LSP value.
   Hover is the main example: one hover contains OCaml and Melange sections.
3. **Portability suggestions** use an intersection. Completion and source edits
   should only suggest something known to work in every applicable mode.
4. **Mutations** require consensus. Rename and refactors are offered only when
   every applicable mode computes the same normalized edit.

This distinction is required by the
[LSP 3.17 result types](https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/).
For example, definition and references can return location arrays, while hover,
semantic tokens, selection ranges, and rename each have one aggregate result.

## End-to-end architecture

```text
            one stanza-owned .merlin-conf file
       +-------------------------------------------+
       | nonempty list of mode configurations     |
       | each configuration has its own file map  |
       +----------------------+--------------------+
                              |
                  File-Configurations <path>
                              |
                   Merlin dot-protocol parser
                              |
           +------------------+------------------+
           |                                     |
  standalone Merlin                       ocaml-lsp loader
  keeps using File                         returns a nonempty set
  and therefore one config                       |
                                             one pipeline
                                           per applicable config
                                                   |
                                      feature-specific aggregation
                                                   |
                                              one LSP response
```

Dune owns configuration discovery. Merlin owns the shared wire codec. ocaml-lsp
owns multi-configuration execution and LSP presentation policy. The editor does
not know about or choose compilation modes.

## File classification

Classification is a consequence of Dune's per-mode module maps, not a new flag
on the source file.

Consider a mixed-mode library containing these files:

```text
shared.ml
platform.ml
platform.melange.ml
melange_only.melange.ml
```

- `shared.ml` is present in both module maps, so its query returns OCaml and
  Melange configurations.
- `platform.ml` is the OCaml source selected for that module, so its query
  returns only the OCaml configuration.
- `platform.melange.ml` is the conditional Melange source, so its query returns
  only the Melange configuration.
- `melange_only.melange.ml` exists only in the Melange module map, so its query
  returns only the Melange configuration.

This is why each processed configuration must retain its own `per_file_config`.
It is also why filtering must happen in Dune, before ocaml-lsp sees the list.

## Current state after PR #15493

PR #15493 merged as the mode-agnostic foundation.

It does the following in `src/dune_rules/merlin/merlin.ml`:

- changes `Processed.t` from one configuration to
  `configuration Nonempty_list.t`;
- defines the first group member as the default without persisting a separate
  boolean;
- changes stanza rule generation to accept a nonempty `Merlin.group`;
- keeps `Processed.get` singular by selecting the first applicable
  configuration in group order;
- keeps `dump-dot-merlin` singular by projecting each stanza to one
  configuration before applying its existing merge behavior.

All call sites currently construct singleton groups. That is intentional: the
PR introduces the container and compatibility behavior without changing which
configurations Dune generates.

The singular fallback already implements the required exclusive-file behavior.
If the group's default configuration does not contain the requested file, but a
non-default configuration does, `Processed.get` returns that configuration.
For a shared file, old clients still receive the default OCaml configuration.

The later Dune change needs to persist `Compilation_mode.t` in each processed
configuration and `Ml_kind.t` in each per-file entry. Keeping those out of
#15493 preserves the foundation as a logical change, at the cost of one
additional persistent-format version bump. The files are build artifacts and
Dune already diagnoses incompatible versions. If maintainers prefer a single
format bump, both fields can be folded into #15493, but that is not required by
the architecture.

## Dune work

### 1. Generate every effective mode configuration

The former [Dune PR #15557](https://github.com/ocaml/dune/pull/15557) supplied
the implementation starting point. Its replacement is the rebased generation
commit `471df286eb`: it retains every mode configuration in one group and
implements the exact-match precedence described below. Do not revive the older
`54d21be196` prototype or its plain-list representation.

In `src/dune_rules/gen_rules.ml`, the library branch currently calls
`Compilation_mode.Per_mode.choose`. Replace that selection with collection of
all available `(Compilation_context.t * Merlin.t)` values and pass the Merlin
values to one `Merlin.group`.

The important invariants are:

- a group is nonempty;
- every member is produced for the same stanza and therefore has the same
  `Merlin_ident` and persisted stanza file;
- the producer contributes at most one member for each `Compilation_mode.t`;
- ordering is deterministic and the first member is the default;
- each member is built from its mode-specific modules, dependencies, object
  directories, flags, preprocessing, stdlib, suffixes, and indexes.

`src/dune_rules/lib_rules.ml` already constructs the mode-specific compilation
contexts and Merlin values with the same stanza identifier. When both library
modes are present, put OCaml first and Melange second. A library with only one
effective mode gets a singleton group. Executable and `melange.emit` call sites
remain singleton groups.

Do not merge mode configuration fields. In particular, OCaml's stdlib and byte
object directory must never be combined with Melange's stdlib, `.cmj` object
directory, libraries, preprocessing, or conditional source suffixes.

### 2. Persist mode and source-match metadata

Add `for_ : Compilation_mode.t` to `Processed.configuration`, the group-member
record, and serialize it with `Compilation_mode.repr`. Do not put it on
`Processed.config`, which is the mode-local directive payload. This requires
incrementing the persistent `merlin-conf` version from #15493's version 10 to
version 11. Store `Ml_kind.t` with each `per_file_config` entry in the same
version bump.

Expose a file-oriented view:

```ocaml
type source_kind =
  | Implementation
  | Interface

type file_configuration =
  { mode : Compilation_mode.t
  ; is_default : bool
  ; kind : source_kind
  ; counterpart : Path.t option
  ; directives : Sexp.t
  }

val configurations
  :  Processed.t
  -> file:Path.Build.t
  -> file_configuration Nonempty_list.t option
```

`is_default` in this file-oriented view is derived from whether the matched
member was the head of the persisted group. It is protocol metadata, not a
field stored in `Processed.configuration`.

`None` means that this stanza does not own the file. `Some` is nonempty by type,
so once Dune has selected a matching stanza there is no empty success state to
handle or accidentally serialize.

The lookup result must retain the matched `Ml_kind.t` together with its
`module_config`; do not reconstruct it later from a filename suffix. Set `kind`
from that matched source. Set `counterpart` from
`Module.source_without_pp module_` for the opposite `Ml_kind.t`, so it names the
exact existing authored implementation or interface selected by this mode.
`None` means that this mode's module has no existing opposite-kind source.

This requires constructing `per_file_config` entries with their source kind.
Exact and copy-line matches carry that entry through directly. The extensionless
fallback must also retain the selected entry's kind rather than discarding it.
If an extensionless key is ambiguous between implementation and interface
entries, use the query's final source extension to select the kind and return no
match if it is still ambiguous. Never invent `KIND` or `COUNTERPART` from the
first suffix that happens to match.

#### Preserve exact-match precedence across modes

`Processed` file lookup has two match classes:

- `Exact_or_copy`: the queried path is present in `per_file_config`, or following
  a copy-line directive finds an entry;
- `Extensionless`: the existing last-resort lookup finds an entry after removing
  the queried path's extension.

This precedence must be decided across the whole configuration group, not
independently for each mode. For example, in a mixed library containing
`platform.ml` and `platform.melange.ml`, a lookup of `platform.ml` can produce
an exact OCaml match and an extensionless fallback match from the Melange
configuration. Returning every configuration that individually matched would
incorrectly report that both modes apply to the file.

`configurations` must therefore collect each member together with its match
class and then:

1. return all `Exact_or_copy` matches, in group order, when at least one exists;
2. otherwise return all `Extensionless` matches, in group order;
3. return `None` when neither class has a match and `Some` of a nonempty list
   otherwise.

The rebased generation commit implements this classification and precedence
for the singular lookup. Preserve that implementation when extracting the
plural `configurations` API; do not implement the plural lookup as an
independent filter over group members.

Keep `Processed.get` as the compatibility projection of `configurations`:
choose the first applicable entry in group order.

### 3. Add an additive configuration-server request

Keep the existing request unchanged:

```lisp
(File "path/to/file.ml")
```

Add:

```lisp
(File-Configurations "path/to/file.ml")
```

The prototype's bare list is workable, but a tagged response makes capability
detection and errors unambiguous. Since this command has not shipped, prefer a
shape such as:

```lisp
(CONFIGURATIONS
 ((CONFIG
   (MODE "ocaml")
   (DEFAULT true)
   (KIND "implementation")
   (COUNTERPART "/workspace/lib/example.mli")
   (DIRECTIVES (...)))
  (CONFIG
   (MODE "melange")
   (DEFAULT false)
   (KIND "implementation")
   (COUNTERPART "/workspace/lib/example.melange.mli")
   (DIRECTIVES (...)))))
```

and, for a supported command that failed:

```lisp
(CONFIGURATIONS-ERROR "No config found for file ...")
```

An old Dune responds to the unknown command using its existing error/directive
shape. The new parser can classify that as `Unsupported` and retry `File`.
This is a stateless fallback: an ocaml-lsp lookup may try
`File-Configurations`, consume the old response, and then issue `File` on the
same process. No persistent capability state is required.

Only the recognized old-server response is `Unsupported`. A tagged
`CONFIGURATIONS-ERROR` and a malformed or empty plural response are errors.
They must not trigger legacy fallback or be converted into an empty or default
configuration. General configuration-server process recovery is outside this
protocol change.

The fields have these meanings:

- `MODE` is a required, stable compilation-mode key. Equal keys identify
  configurations that may be paired across documents. Mode keys must be unique
  within one `CONFIGURATIONS` response. Consumers should preserve and compare
  unknown future strings rather than rejecting the whole response. Human labels
  such as `OCaml` and `Melange` are derived from this key.
- `DEFAULT` only defines the legacy `File` projection and deterministic
  primary ordering. It does not define an active LSP mode.
- `KIND` is required and is exactly `"implementation"` or `"interface"`. It
  describes the source entry that Dune matched for this configuration.
- `COUNTERPART` is optional. When present, it is the absolute path of the exact
  existing authored source of the opposite kind in this mode. Dune obtains it
  from `Module.source_without_pp`; it is not guessed from `SUFFIX` directives.
  Dune does not resolve source-tree symlinks before serializing it.
- `DIRECTIVES` is one complete ordinary Merlin configuration.

The parser should ignore unknown fields inside `CONFIG` for forward
compatibility. It should require exactly one `MODE`, `DEFAULT`, `KIND`, and
`DIRECTIVES` field in each entry, permit at most one `COUNTERPART`, reject
duplicate mode keys, and reject more than one default in a response. A filtered
response may have no default. Reject an empty mode key and a relative
`COUNTERPART` path.

Make the new protocol typed at both construction boundaries:

```ocaml
type configurations_error =
  | Unsupported
  | Server_error of string
  | Protocol_error of read_error

val read_configurations
  :  request:file_configurations_request
  -> input
  -> (configuration Nonempty_list.t, configurations_error) result
```

Dune's success encoder accepts only a `file_configuration Nonempty_list.t`.
Merlin's decoder is the only constructor of a plural decoded set. The ocaml-lsp
loader exposes separate smart constructors for that decoded set and for one
successful legacy `File` configuration; the general nonempty-set representation
is private. ocaml-lsp exhaustively handles the three error variants. No caller
should inspect raw Csexp or turn a decoding failure into a singleton
configuration.

Add `Compilation_mode.to_wire` in Dune and use it at the one response-encoding
site:

```ocaml
val to_wire : t -> string

Ocaml   -> "ocaml"
Melange -> "melange"
```

This is deliberately separate from the persistent `Compilation_mode.repr`,
whose constructor names are `"Ocaml"` and `"Melange"`. The Merlin wire decoder
keeps the atom as an opaque string; ocaml-lsp maps the two exact lowercase names
to known constructors and preserves every other nonempty atom as `Other`.

`Unsupported` is not a new wire response. It is produced only when the decoder
recognizes the exact legacy response to the request that it just encoded:

```lisp
((ERROR "Bad input: (File-Configurations ...)"))
```

Build the expected message from the encoded request S-expression, including its
path, rather than formatting the path independently. A tagged
`CONFIGURATIONS-ERROR` is `Server_error`. Every other unexpected, malformed, or
empty response is `Protocol_error` and must not trigger `File`.

The abstract `Merlin.group` constructor takes one `default` and a list of
`alternatives`, so exactly one default is structural before persistence. Build
the alternatives in `Compilation_mode.Per_mode.to_list` order. File filtering
preserves that order; if the default applies, it remains first. Neither the wire
encoder nor decoder sorts the response.

### 4. Preserve stanza lookup semantics

`bin/ocaml/ocaml_merlin.ml` currently sorts `.merlin-conf` files and returns the
first stanza whose configuration applies to the file. The plural request must
return the matching modes from that one selected stanza. It must not silently
concatenate configurations from every stanza that happens to mention the file.
Broadening stanza ambiguity is separate work.

Use this search algorithm:

1. Search `.merlin-conf` files in the existing deterministic order.
2. If a file cannot be decoded because it was written by an incompatible Dune,
   remember the error and continue.
3. If a decoded stanza has one or more configurations applicable to the source
   file, return that nonempty set immediately.
4. If no stanza in the directory matches, continue with the parent directory.
5. If the whole search has no valid match, return a remembered load error if
   there is one; otherwise return the existing “No config found” error.

This prevents an unrelated stale v8/v9 file from masking a valid matching
configuration after a partial rebuild.

### 5. Keep debugging and legacy commands deliberate

- `File` returns the applicable default or sole configuration.
- `dump-dot-merlin` remains a legacy singular/debugging command. It projects
  each persisted group to its first/default configuration and otherwise keeps
  its existing behavior; mode-aware output and cross-stanza guarantees are not
  part of this work.
- `dune ocaml merlin dump-config` may print all mode configurations because it
  is debugging output.
- There is still one `.merlin-conf/<stanza-id>` file. Modes are entries inside
  it, not separately named files.

### 6. Dune tests

Extend `test/blackbox-tests/test-cases/melange/merlin.t` with one mixed-mode
library containing shared, conditional OCaml, conditional Melange, and
Melange-only sources.

Assert all of the following:

- one persisted file is generated for the stanza;
- the shared file returns two configurations in deterministic order;
- the plain conditional source returns only OCaml and does not leak the Melange
  extensionless fallback match;
- the `.melange.ml` conditional source returns only Melange and does not leak a
  fallback match from another mode;
- the Melange-only source returns only Melange;
- a preprocessed filename with no exact or copy-line match still uses the
  extensionless fallback;
- every response uses the exact lowercase mode wire name and reports the matched
  source kind;
- each existing implementation/interface pair reports the exact authored
  counterpart selected by that mode, including pairs whose conditional
  counterparts have different filenames;
- each response has the correct stdlib, object directory, preprocessing,
  libraries, source suffixes, and mode label;
- legacy `File` returns OCaml for the shared source and the only applicable
  configuration for each exclusive source;
- debug dumping still exposes both alternatives;
- an unrelated stale persistent file does not mask a later valid file;
- the first matching stanza behavior is unchanged when more than one stanza is
  present.

The existing Melange source lookup test should also be extended so that locate
results from both configurations resolve to authored files.

## Merlin work

### 1. Put the additive wire codec in `merlin_dot_protocol`

Add the configuration record, command encoder, and response parser in:

- `src/dot-protocol/merlin_dot_protocol.ml`
- `src/dot-protocol/merlin_dot_protocol.mli`

Expose the decoded success shape directly:

```ocaml
type source_kind =
  | Implementation
  | Interface

type configuration =
  { mode : string
  ; is_default : bool
  ; kind : source_kind
  ; counterpart : string option
  ; directives : Sexp.t
  }
```

The functorized codec is already shared by blocking Merlin and ocaml-lsp's
Fiber-based process client, so this is the correct common layer.

The wire mode should remain an extensible string at this layer. Higher layers
can map `"ocaml"` and `"melange"` to a variant while retaining an `Other` case.

Dune cannot depend on Merlin's codec, so keep one literal golden response in
sync across the repositories: Dune's blackbox test emits it and Merlin's codec
test parses that exact Csexp. Together with exhaustive typed encoders and
decoders on each side, this catches casing, field-name, optional-field, and
nesting drift.

### 2. Keep standalone Merlin singular

`src/kernel/mconfig_dot.ml` does not need to issue `File-Configurations`.
Standalone `ocamlmerlin` can continue to issue `File`, and new Dune preserves
that request's behavior. This avoids adding selection state or plural query
semantics to Merlin's existing command protocol.

The dot-merlin reader should never be sent the new request. Its exhaustive
command handler should nevertheless return an immediate error if that happens;
it must not consume the request without writing a response, which would hang a
client.

### 3. Do not make source maps a prerequisite

Current Dune main already adds original source directories and
mode-conditional `SUFFIX` directives for Merlin lookup. First test navigation
with those mechanisms.

The unrelated local `SOURCE_MAP` prototype should be kept out of the minimum
multi-configuration change. Add an explicit generated-to-authored directive
only if an end-to-end test demonstrates that a valid locate result still
escapes into `_build` or points at the wrong conditional source.

### 4. Make phase caches configuration-correct

Creating a fresh `Mpipeline` does not create fresh reader and PPX phase caches.
Those caches are process-global. The reader fingerprint currently contains the
source digest and completion position, even though parsing can also depend on
configuration fields such as suffixes, extensions, warnings, and external
readers.

Merlin should remain unaware of Dune compilation modes. Before ocaml-lsp starts
alternating configurations for one source file:

- include every parser-affecting `Mconfig.t` field in the reader fingerprint;
- retain reader and PPX entries in an LRU with capacity two in version 1, or
  provide equivalent configuration partitioning, so the regular
  `OCaml, Melange, OCaml, Melange` access pattern reuses both entries;
- keep PPX identity based on its actual command, arguments, working directory,
  and reader input rather than on a Dune mode name.

Add an `A, B, A` regression test using the same source with parser-affecting
configurations `A` and `B`. It must observe the correct parse for each
configuration and reuse the `A` entry on the final run. Add the corresponding
PPX test if the generic phase-cache implementation is changed.

### 5. Merlin tests

Add dot-protocol unit tests for:

- command encoding and decoding;
- a two-configuration round trip including kind and distinct exact counterpart
  paths;
- a sole non-default configuration;
- exact lowercase known mode names, unknown mode strings, and unknown fields;
- missing required fields, duplicate fields or mode keys, invalid kinds and
  booleans, empty responses, malformed Csexp, and tagged server errors;
- preserving the distinction between `Unsupported`, server errors, and protocol
  failures;
- detecting only the exact old-server response to the encoded request as
  unsupported, including request paths that require S-expression quoting,
  without desynchronizing the process;
- ensuring an accidental request to `dot-merlin-reader` gets a response.

Keep existing `Mconfig_dot` tests proving that `File` still loads one normal
configuration from both old and new Dune.

## ocaml-lsp work

### 1. Represent a nonempty configuration set

In `ocaml-lsp-server/src/merlin_config.ml`, introduce:

```ocaml
type mode =
  | Ocaml
  | Melange
  | Other of string

type source_kind =
  | Implementation
  | Interface

type configuration_origin =
  | Plural of
      { mode : mode
      ; kind : source_kind
      ; counterpart : Uri.t option
      }
  | Legacy_file

type configuration =
  { origin : configuration_origin
  ; is_default : bool
  ; config : Mconfig.t
  }

type configuration_set = private configuration Nonempty_list.t
```

`configurations` first sends `File-Configurations`. On an explicitly
unsupported response it sends legacy `File` and wraps the result as one
`Legacy_file` configuration. A plural response always produces `Plural` with a
required mode key and source kind; it converts an absolute counterpart path to a
canonical authored `Uri.t` when one is present. A supported server error remains
an error; it is not mistaken for feature negotiation.

The shared Csexp stream has no request IDs. Make the encoded request and its
response decoder one typed value, and make `Process.query` the only API that can
access the session:

```ocaml
type 'response request

val file : path:string -> legacy_response request
val file_configurations : path:string -> configurations_response request

val query
  :  Process.t
  -> 'response request
  -> ('response, query_error) result Fiber.t
```

The request constructors and `query` are implemented in terms of the shared
Merlin codec; callers cannot choose a decoder independently of the command they
sent. Keep raw session writes and reads private to this module. `Process.query`
holds a per-process transaction mutex while it writes the encoded request and
parses exactly one response with that request's decoder. This makes response
pairing and serialization correct by construction rather than a convention at
each call site. The plural attempt and a legacy retry are two such transactions;
no per-mode requests or protocol state machine are introduced.

On success, including legacy fallback, the internal set is always nonempty.
Malformed, empty, and failed responses remain errors rather than producing a
synthetic configuration. Construct `configuration_set` only through
`of_plural` and `of_legacy`; do not expose a constructor from an arbitrary list.

A deterministic `primary` helper selects the default if present and the first
entry otherwise. Keep a singular `config` projection temporarily for
compatibility during the handler migration, but audit every remaining use
before declaring the feature complete.

Canonicalize the document path when constructing `Merlin_config.t`, before
deriving its initial query or sending either `File-Configurations` or `File`.
For an opened symlink such as `link.ml -> original.ml`, Dune must be queried for
the canonical authored `original.ml`; canonicalizing only locations returned by
Merlin is insufficient. Use the same nearest-existing-parent behavior as result
URI canonicalization so a not-yet-existing final component remains stable.

### 2. Remove mode switching

Do not keep the current WIP's `active_config_id` in
`ocaml-lsp-server/src/document.ml`. Remove:

- active-configuration selection and mutation;
- `ocamllsp/selectMerlinConfiguration`;
- its advertised experimental capability;
- tests that expect existing requests to change after a selection.

An optional read-only debug request may list configurations, but it is not part
of the product UX and must not describe any configuration as active.

### 3. Make multi-configuration execution explicit

`Single_pipeline.use_with_config` already creates a fresh `Mpipeline` for a
specific `Mconfig.t`. Keep one Merlin worker thread and run the applicable
configurations sequentially. Scheduling two fibers on the same worker does not
provide useful parallelism and makes cancellation harder to reason about. The
Merlin phase-cache prerequisite below makes alternating fresh pipelines
configuration-correct.

Add a narrow primitive for simple Merlin commands:

```ocaml
type 'a configured_result =
  { configuration : Merlin_config.configuration
  ; result : ('a, Exn_with_backtrace.t) result
  }

val dispatch_all
  :  t
  -> configurations:Merlin_config.configuration_set
  -> 'a Query_protocol.t
  -> 'a configured_result Nonempty_list.t Fiber.t
```

The top-level LSP handler should fetch the set once and pass it to every helper
used by that request. `dispatch_all` preserves the supplied order, checks
cancellation between configurations, and retains per-configuration failures.
It does not fetch configurations itself. This keeps a multi-step request on one
configuration set without requiring a separate request-snapshot abstraction;
the existing `Document.t` already retains the request's text and version.

A handler is migrated only when its whole configuration-dependent computation
runs once per applicable mode. This includes configured parsing,
preprocessing, source-kind and comment checks, branch selection, typing,
documentation lookup, and edit construction. Running only the final Merlin
query per mode is insufficient because an earlier configured step may have
selected a different operation.

Source-text calculations and client-capability checks that do not inspect
Merlin configuration may run once. After each mode has produced its complete
result, the handler applies its documented union, composite, intersection, or
consensus policy. No helper inside that boundary may silently fetch or select a
singular configuration.

Only results equal under the feature's documented normalization key collapse.
Distinct mode results remain distinct or receive mode-labelled presentation
unless the feature deliberately uses intersection or consensus.

Do not put generic result merging in `Document`: the LSP handler knows whether
its operation is a union, intersection, composite, or consensus.

Handlers that need several Merlin commands in one pipeline, such as diagnostics
(`Errors` plus `Holes`) and completion (`Complete_prefix` plus `Construct`),
should loop over the configurations directly and keep those commands together
per pipeline.

### 4. Determine source kind without assuming the default mode

`Document.make_merlin` currently uses one selected configuration's suffixes to
infer implementation versus interface. For a plural response, use the required
`KIND` metadata instead. Every applicable configuration for one queried file
must report the same kind; report a configuration error if they disagree.

For a legacy `File` response, retain the current filename-and-suffix inference.
Do not freeze either result in the document when it is opened: derive it from
the configuration set loaded by the top-level request. Helpers receive the
resolved kind together with that set and do not infer it independently.

### 5. Normalize and retain provenance

Every configured result carries its configuration until aggregation is
complete. Protocol origin does not determine whether presentation needs a mode
label:

- with one applicable configuration, preserve the existing unlabelled response,
  whether it came from `File-Configurations` or legacy `File`;
- with multiple applicable configurations whose results collapse to one
  identical result, preserve the existing compact unlabelled response;
- with multiple applicable configurations and partial or divergent results, use
  human labels `OCaml`, `Melange`, and the raw string for future modes wherever
  the LSP result type permits provenance.

Represent each normalized aggregate value together with the set of mode keys
that contributed it. A value is universal exactly when that set equals the
request's applicable mode set; otherwise present the contributor set in
deterministic configuration order. Aggregators must fold an arbitrary nonempty
list and must not branch on “OCaml versus Melange.”

Normalize locations before deduplication:

- first apply real generated-to-authored mapping if one exists;
- resolve symlinks and canonicalize the resulting authored path;
- for a target that does not exist yet, resolve the nearest existing parent and
  append the remaining path components without following them;
- return that canonical authored URI in LSP results;
- preserve exact ranges;
- key a location by canonical URI and range.

The same canonical URI is used for identity, deduplication, presentation, and
workspace edits. Preserving an editor-opened symlink alias is deliberately out
of scope. Configuration discovery uses this canonical authored path too; input
queries and output locations must not disagree about the identity of a
symlinked source.

Do not clamp an out-of-range line to the end of a file. That can turn a broken
generated location into a plausible but incorrect definition. Either map the
location correctly, retain the generated URI with a diagnostic log, or discard
it as invalid according to the feature's policy.

### 6. Common failure policy

- Cancellation aborts the complete aggregate. Explicit requests return
  `RequestCancelled`; background diagnostics publish nothing from the cancelled
  generation. No result class returns a partial response after cancellation.
- For observational unions and composite presentations, return the successful
  mode results when at least one mode succeeds and log every failed mode. If
  every mode fails, return the primary failure and include the other
  mode-labelled failures in the server log.
- Diagnostics publish the successful modes from the current generation and log
  failed modes. If every mode fails, publish an empty Merlin diagnostic set for
  that generation so stale Merlin diagnostics do not remain visible; retain
  diagnostics owned by other producers.
- For portability suggestions represented by a list or option, any failed
  applicable mode produces an empty list or `None`. This applies to completion,
  constructs, and mode-dependent edit-producing code actions; a code-action
  response may still retain independently computed configuration-independent
  actions.
- For explicit mutations and required-value intersections, any failed
  applicable mode returns `RequestFailed` with the failed mode names. This
  applies to rename, selection ranges, and semantic tokens.
- Never silently substitute the default result for a failed shared-file query.

## LSP feature policies

### Navigation: definition, declaration, and type definition

Files:

- `ocaml-lsp-server/src/definition_query.ml`
- the corresponding request branches in `ocaml_lsp_server.ml`

Run `Locate` or `Locate_type` in every applicable configuration. Convert each
successful result to authored `Location`s, normalize, union, and deduplicate by
URI and range. LSP already permits multiple locations for these requests.

The current WIP's fan-out is the right basic direction, but replace JSON-string
deduplication with a typed location key and remove line clamping. Preserve all
successful locations; only fail the request when no configuration found a
location.

### References and document highlights

Run `Occurrences` in every configuration.

- References use the union of non-stale normalized locations.
- Document highlights use the union of normalized source ranges.
- Exact duplicates collapse.
- Merge occurrence index status separately. If one mode is out of sync, return
  usable locations and issue one warning naming the affected modes.

### Diagnostics and typed holes

File: `ocaml-lsp-server/src/diagnostics.ml`.

Within each configured pipeline, run `Errors` and `Holes`. Convert them to an
internal diagnostic paired with its mode, then merge before the single
`Diagnostics.set` call for that URI.

Use a canonical key containing range, severity, code, the exact message, tags,
and related information after canonicalizing its URIs. Do not otherwise rewrite
or whitespace-normalize the message. If a diagnostic is identical in every
mode, publish it once with source `ocamllsp`. If it is mode-specific, publish it
with source such as `ocamllsp (OCaml)` or `ocamllsp (Melange)`.

Store a versioned contributor-mode list in `Diagnostic.data` for every
ocaml-lsp-owned diagnostic:

```json
{"ocamllsp": {"version": 1, "modes": ["ocaml", "melange"]}}
```

Use the exact wire mode keys in deterministic configuration order. The
human-readable `source` is presentation only and must not be parsed back into
mode identity. Exclude this provenance object from the diagnostic deduplication
key; populate it after contributor sets have been merged.

Update Dune-versus-Merlin diagnostic deduplication so that it recognizes all
ocaml-lsp mode-labelled sources rather than testing equality with only the
literal `ocamllsp`.

Typed-hole diagnostics follow the same rule: identical hole range and type
collapse; differing types are displayed separately with mode provenance.

### Completion and completion resolve

File: `ocaml-lsp-server/src/compl.ml`.

Completion is the portable intersection, not a union.

For each configuration, keep comment detection and the completion operation in
the same configured pass. Run `Complete_prefix` and, when applicable,
`Construct` together in that mode's pipeline before intersecting mode results.
Represent each pass as `Suppressed | Items of raw_item list`. If any applicable
mode returns `Suppressed` because the position is inside a configured comment,
the aggregate is suppressed and configuration-independent keyword completions
are not appended.

For `Complete_prefix`, intersect raw entries using their edit semantics:
label, insertion text, insert-text format and mode, completion kind, replacement
range, additional text edits, and command. An entry with the same label but a
different kind or editing effect is not the same portable completion. Treat
detail and documentation as presentation fields that may be merged after the
intersection. Include application labels only when present in every applicable
configuration.

Order the intersection using the primary configuration's ranking filtered to
common entries. Add configuration-independent keyword completions once.

If a common entry has different type details or documentation, keep one
completion item but present mode-labelled detail or documentation. Do not show
one mode's type as if it were universal.

For typed-hole `Construct`, intersect exact replacement edits. A constructed
expression offered by only one backend is not known to be portable.

The current `CompletionItem.data` contains only `CompletionParams`. Extend it
with the common item key, originating document version, and the exact
`TextEdit` or `InsertReplaceEdit` selected for the item. During
`completionItem/resolve`, fetch the current configuration set and revalidate the
same common item against that set. Query documentation in every currently
applicable configuration after applying that stored edit; never reconstruct an
edit by inserting the display label at the old position. Collapse identical
documentation; otherwise emit mode-labelled sections.

Version 1 resolves documentation only, and attaches resolve data only when the
client lists `documentation` in
`completionItem.resolveSupport.properties`. Type detail is already present on
the initial item after mode-aware merging. Resolution must not change the
item's identity, ordering, insertion text, detail, or edit. If the original
document version has changed, or the item is no longer portable in the current
configuration set, return it unchanged rather than applying or replacing its
edit.

### Hover and extended hover

File: `ocaml-lsp-server/src/hover_req.ml`.

For each configuration, run the whole existing hover decision path in one
configured pass: obtain `reader_parsetree`, classify the node at the cursor, and
then run either the `Type_enclosing`/documentation/syntax-document sequence or
the PPX expansion path, including deriving attributes. Aggregate only the final
per-mode hover values.

- If all successful values are identical, return the current compact hover.
- Otherwise return one hover whose contents contain mode-labelled sections.
- Use a hover range only when all contributing modes agree on the normalized
  range; omit the optional range when they disagree.
- If a file has multiple applicable configurations but only one mode has a
  result, return it with a mode label so the limitation is visible. A file with
  only one applicable configuration keeps the existing unlabelled hover.

Extended-hover history must include the document version so an edit at the same
URI and position resets the verbosity sequence. It does not cache Merlin
results; each hover uses the current configuration set.

### Signature help

File: `ocaml-lsp-server/src/signature_help.ml`.

For each configuration, keep comment detection, application-signature
calculation, and documentation lookup in one configured pass.

LSP `SignatureHelp` contains a `signatures` array. Group results only when their
label, parameters, and active parameter agree. Merge documentation for such a
group, using mode-labelled sections when it differs. Results with different
active parameters remain separate signatures even when their labels are equal;
put their contributor-mode labels in documentation even when Merlin supplied no
documentation. Do not prefix the signature label, because Merlin's parameter
offsets refer to that label.

When the client advertises
`signatureInformation.activeParameterSupport`, set
`SignatureInformation.activeParameter` on each signature. Choose
`activeSignature` deterministically from the primary configuration after
grouping. For clients without per-signature support, the top-level
`activeParameter` can describe only that primary active signature.

### Inlay hints

File: `ocaml-lsp-server/src/inlay_hints.ml`.

Union exact `(position, kind, label)` hints. Collapse identical hints. When two
modes produce different labels at the same position, keep both and add mode
labels, preferably with `InlayHintLabelPart` rather than modifying the inferred
type text invisibly.

### Document symbols and code lenses

File: `ocaml-lsp-server/src/document_symbol.ml`, plus the code-lens request path
in `ocaml_lsp_server.ml`.

Union structural symbols by normalized range, normalized `selectionRange`, name,
and kind. Collapse equal trees recursively. Use `DocumentSymbol.detail` for
mode-specific differing type details where the client supports hierarchical
symbols. Avoid changing symbol names merely to encode provenance.

Do not discard `Query_protocol.outline_type` while converting each configured
outline to LSP values. Keep an intermediate tree whose nodes carry the mode key,
the optional outline type, the structural symbol fields, and the intermediate
children. Group nodes by the structural fields above, recursively merge their
child lists, and only then construct the final `DocumentSymbol.t`. In
particular, neither `detail` nor `children` is part of the structural key. Leave
`detail` absent when there is only one contributor or every contributor reports
the same optional type, preserving the existing compact response. When the
optional types differ, render every contributor in deterministic configuration
order as mode-labelled `detail`; represent a missing type explicitly rather
than presenting another mode's type as universal.

Exact normalized range remains part of symbol identity. Two outlines with the
same name and `selectionRange` but different full ranges remain distinct unless
real source mapping normalizes them to the same authored range. Do not weaken
the key to force a merge and then choose one mode's range silently.

Code lenses similarly collapse identical range/command pairs. Distinct inferred
types at one range may be returned as separately mode-labelled lenses.

### Workspace symbols

File: `ocaml-lsp-server/src/workspace_symbol.ml`.

Workspace symbols scan `.cmt` and `.cmti` files from the build tree rather than
using a document pipeline. Scan once, resolve source locations against the
workspace and build roots, canonicalize them, and union the results. Collapse
exact `(URI, range, name, kind, container)` duplicates produced by OCaml and
Melange object directories; retain genuinely distinct symbols. If an existing
CMT names only a generated preprocessed file and supplies no source map, retain
that canonical generated URI under the common location policy above. Do not
guess an authored filename from Dune's build-path naming conventions.

### Folding ranges

File: `ocaml-lsp-server/src/folding_range.ml`.

Folding is source-structural, but configured readers and preprocessing can still
produce different trees. Run the current parser path for every applicable
configuration, union exact `(range, kind, collapsedText)` values, and apply the
client's range limit only after deduplication.

### Selection ranges

The request is handled in `ocaml_lsp_server.ml` using Merlin `Enclosing`.

LSP requires one result at the same index as each requested position. For each
position, intersect the normalized enclosing-range chains and preserve nesting.
If the intersection is empty, return the required empty range at the requested
position. It is not valid to concatenate two chains.

### Semantic tokens

File: `ocaml-lsp-server/src/semantic_highlighting.ml`.

LSP has one non-overlapping encoded token stream and no mode-label field. Use a
conservative intersection of exact `(range, token type, modifiers)` tuples.
Drop a token when modes disagree about its classification. Encode only after
intersection so relative offsets remain valid.

Generate an opaque result ID for each encoded token array and retain that
association in a per-document two-entry LRU. Clear the history when the document
closes. When the client supplies `previousResultId`, diff the associated array
against the current array even if the configuration set changed. Fall back to a
full response only when the prior array has been evicted or a delta cannot be
produced.

### Prepare rename and rename

Files: `ocaml-lsp-server/src/rename.ml` and the prepare-rename request branch in
`ocaml_lsp_server.ml`.

These are mutations, so the first implementation should be conservative.

Version 1 uses structural edit consensus, not semantic equivalence. Before
comparison, rewrite every existing and not-yet-existing URI using the canonical
path rule above and reject overlapping text edits. Then compare the complete
typed `WorkspaceEdit` values exactly, including:

- whether the edit uses `changes` or `documentChanges`;
- document versions, text-edit ordering, and same-position insertion ordering;
- create, rename, and delete operation ordering and options;
- change annotations and annotation identifiers.

Do not reorder operations or convert between equivalent LSP representations.
Two semantically equivalent edits encoded differently are rejected in version
1. This is conservative: it can suppress an available mutation but cannot
silently accept two different effects.

- Prepare rename succeeds only when every applicable configuration accepts the
  position and returns the same normalized source range.
- Run renaming occurrences in every configuration and build a normalized
  `WorkspaceEdit` for each.
- Return the edit only when all edits are identical.
- If symbol graphs differ, reject the request with a message explaining that
  the applicable modes produced different rename targets.

Do not union edits initially. A union can rename unrelated symbols when the
identifier resolves differently by mode. A future design can relax this only
after proving that both graphs share the same source definition and that all
edits are nonconflicting.

### Code actions and refactors

File: `ocaml-lsp-server/src/code_actions.ml` and the action modules it invokes.

Run configuration-independent Dune actions once. Existing-counterpart
open-related actions are mode-dependent navigation actions: derive them from
the exact per-mode `COUNTERPART` values, then union and deduplicate their
canonical target URIs. Creating a missing counterpart follows the mutation rule
in the cross-document section below.

For each configured action pass, filter
`CodeActionParams.context.diagnostics` using the contributor-mode list stored in
`Diagnostic.data`: pass a diagnostic when its contributor set contains that
mode. Pass diagnostics not owned by ocaml-lsp unchanged, preserving their
existing behavior. Drop and log an ocaml-lsp-owned diagnostic whose provenance
object is absent or malformed rather than passing it to the wrong mode. For
Merlin-derived edits, normalize the resulting edits and keep only actions whose
edit is identical in every applicable configuration. This applies to construct,
destruct, type annotation, refactor-open, signature updates, inferred
interfaces, extraction, inline, and similar transformations.

Configured action preconditions, AST inspection, counterpart lookup, and edit
construction are all part of the per-mode computation. Do not select an action
with one mode and merely recompute its final edit with the others.

Pair configured actions first by provider identity and a provider-local stable
action key, never by title or edit equality. For a paired action, require
structural consensus for both its complete `WorkspaceEdit` and optional
`Command`. Require equal titles and kinds; set `isPreferred` only when every
mode does, union only diagnostics that passed the contributor filtering above,
and omit the action if any mode disables it.

Read-only navigation actions may be unioned and mode-labelled. An action that
cannot establish a portable edit should be omitted rather than offered for the
default mode.

`codeAction/resolve` currently returns its input unchanged and remains
configuration-independent. If it later computes a Merlin-derived edit, it must
carry enough data to apply the same consensus policy during resolution.

### Custom requests

Version 1 guarantees mode-aware behavior for the standard LSP handlers named in
this document. It does not require a new aggregate schema for every existing
ocaml-lsp custom request.

Apply this default to every custom request without a more specific policy in
this document:

- with one applicable configuration, preserve its existing behavior;
- with multiple applicable configurations, a read-only request deliberately
  uses the primary configuration and preserves its existing response schema;
- with multiple applicable configurations, an edit-producing request or
  execute command is unavailable and returns `RequestFailed` rather than
  applying a primary-only mutation.

The explicit exceptions are:

- `switchImplIntf` and open-related navigation use the exact counterpart union
  described below;
- raw `merlinCallCompatible` remains deliberately primary because arbitrary
  Merlin commands have no aggregate schema;
- formatting remains one ocamlformat operation and is configuration-independent;
- a retained read-only `merlinConfigurations` debug request returns only mode
  keys and the default marker. It has no configuration ID or active state.

Later PRs may give a custom request an additive or versioned aggregate schema.
Type search can then use a normalized union, type-enclosing and documentation
can use hover's composite policy, and edit-producing requests can opt into
structural consensus.

Before declaring version 1 complete, audit every LSP request branch, custom
request, `workspace/executeCommand` path, and remaining use of `with_pipeline`,
`dispatch`, `mconfig`, or source kind. Each site must either implement a named
mode-aware policy or visibly follow the singleton/primary/unavailable default
above; no site may select the primary configuration accidentally.

### Cross-document operations

Interface inference, signature updates, and other operations that open an
implementation and an interface must discover and pair counterparts by mode.
Counterpart discovery is part of the configured computation: a shared
`platform.mli` may pair with `platform.ml` in OCaml mode and
`platform.melange.ml` in Melange mode.

Starting from the initiating document's applicable configuration set:

1. For each plural configuration, take its exact optional `COUNTERPART` URI.
   Do not probe `SUFFIX` candidates for an existing file.
2. Discard a stale counterpart URI whose target no longer exists, then
   deduplicate the remaining URIs. Every lookup uses the plural request and
   therefore loads the complete configuration set for that counterpart; an
   implementation may reuse that set within the request, but correctness must
   not depend on such a cache.
3. For each initiating `MODE`, select the counterpart configuration with that
   exact mode key and verify that its `KIND` is the opposite of the initiating
   configuration's kind.
4. Run the operation with that matched pair. If the initiating configuration
   has no counterpart, or the counterpart response has no matching mode or the
   wrong kind, report that the operation is not applicable rather than guessing.

Two modes may select different counterpart URIs, or the same shared URI with
different configurations. Never run an OCaml implementation pipeline against a
Melange interface configuration, or vice versa. If an edit affects a document
compiled in multiple applicable modes, every one of those modes must produce
the same normalized edit. A missing counterpart or matching mode therefore
suppresses an edit that cannot be established for every applicable mode.

A legacy `File` response retains the current singular counterpart discovery and
has no mode key. Preserve compatibility by pairing it only when both sides have
exactly one candidate configuration. If one side is a legacy singleton and the
other has multiple plural configurations, report that the operation is not
applicable rather than guessing a mode.

`switchImplIntf` itself remains a filename/navigation operation, but counterpart
discovery uses the union of exact plural `COUNTERPART` URIs. For a legacy
singleton it retains the current suffix-based behavior.

When an operation deliberately offers to create a counterpart that does not yet
exist, `COUNTERPART` is necessarily absent. It may derive proposed filenames
from each configuration's suffix pairs, but creation is a mutation: offer it
only when every applicable mode proposes the same canonical target URI. Do not
use a guessed missing path as evidence that an existing counterpart applies to a
mode.

## Backward compatibility

| Client | Dune | Behavior |
| --- | --- | --- |
| Old Merlin or ocaml-lsp | New Dune | Uses unchanged `File`; shared files get the default, exclusive files get the only applicable config. |
| New Merlin/ocaml-lsp | Old Dune | `File-Configurations` is detected as unsupported and the client retries `File`. |
| New Merlin/ocaml-lsp | New Dune | Receives every configuration applicable to the file. |
| Standalone Merlin | Any Dune | Continues to use `File`; no mode-switching behavior is introduced. |

New ocaml-lsp builds use the Merlin codec that knows both requests; there is no
runtime negotiation with an older in-process Merlin library. Runtime fallback
only concerns an older external Dune configuration server. Older standalone
Merlin remains compatible because it never sends `File-Configurations`.

The compatibility promise is about the wire request. Persistent
`.merlin-conf` files still require the Dune server version that wrote them, as
they do today; stale-file handling must produce a useful rebuild error only
after looking for another valid matching stanza.

## Caching, cancellation, and performance

- Each top-level LSP request fetches the initiating document's configuration set
  once and passes it explicitly to its helpers. Cross-document operations use
  the full set returned for each counterpart lookup; they may repeat a cheap
  lookup instead of introducing request-local cache state.
- Initially avoid a long-lived configuration cache; the Csexp query is cheap
  compared with building Merlin pipelines, and reliable invalidation on Dune
  rebuilds is more important.
- Do not add a general aggregate-result cache in the initial implementation.
  The semantic-token result-ID history is a feature-specific response cache,
  not a configuration cache.
- Before storing Merlin diagnostics, discard the result if a newer document
  version or diagnostic generation has superseded the computation.
- Run mode pipelines sequentially on the existing Merlin thread and check
  cancellation between them.
- Label each configured pipeline timing with its mode. The existing outer LSP
  request timing is the aggregate; no second aggregate timer is required.
- Cancellation discards every partial union, composite, intersection, or
  consensus result.
- The design should accept an arbitrary nonempty configuration list even though
  the immediate use case has at most OCaml and Melange.

## Cross-repository test plan

### ocaml-lsp protocol fixture

Adapt the existing local fake `ocaml-merlin` fixture. It should support:

- a new tagged two-configuration response with kind and exact counterpart
  metadata;
- OCaml-only and Melange-only responses;
- an old-server response followed by a successful legacy `File` request;
- identical and deliberately divergent query outcomes by mode;
- malformed, empty, and tagged-error responses that never become a synthetic
  default configuration;
- two concurrent file lookups with deliberately delayed responses, proving that
  each complete write/read transaction receives its own response;
- command counting, so exclusive-file tests prove that only one pipeline was
  queried.

Add one fake-provider unit test with three mode keys. It need not be a real Dune
integration case; it proves that contributor accumulation and labels fold a list
rather than branching on exactly OCaml and Melange.

For a document-symbol test intended to exercise differing type details on one
structural symbol, make the fake preprocessor emit alternatives with equal
lexical spans. Padding a shorter token with trailing whitespace preserves later
offsets but does not equalize the typed-tree range of the enclosing declaration:
its end position stops at the last token. Unequal spans therefore correctly
produce two structural symbols under the exact-range policy above and do not
exercise type-detail merging.

### Feature matrix

For each migrated handler, cover these three cases:

1. **Exclusive file:** the sole applicable configuration drives the existing
   response without a mode label or UI choice.
2. **Shared, identical result:** both pipelines are queried and the result is
   deduplicated to the same response users see today.
3. **Shared, divergent result:** the handler follows its documented union,
   composite, intersection, or consensus rule.

At least one completion, hover, or signature-help test should make an earlier
configured step such as comment detection, parse-tree branch selection, or PPX
expansion diverge by mode. This proves that the whole handler was fanned out,
not only its final query.

At minimum, end-to-end tests must demonstrate:

- two definition locations;
- deduplicated and mode-specific diagnostics;
- diagnostic contributor data being preserved through a code-action request and
  filtered for each configured action pass;
- completion containing common entries but excluding mode-only entries, plus a
  configured-comment case where one suppressed mode suppresses the whole
  completion including keywords;
- completion resolve applying the stored exact edit and returning the item
  unchanged after a document-version change;
- hover with two sections;
- multiple signature-help entries;
- unioned references;
- intersection semantic tokens and selection ranges;
- rename rejection for different edits and success for identical edits;
- code-action suppression for a one-mode-only edit;
- cross-document operations pairing equal mode keys and rejecting disjoint mode
  sets or ambiguous legacy-to-plural pairing, including a shared interface whose
  OCaml and Melange configurations name different exact implementation
  counterparts and a shared implementation whose configurations name different
  exact interfaces;
- configuration changes causing completion resolve to revalidate the item and
  semantic tokens to issue a new result ID while still permitting a delta from
  a retained prior result;
- partial mode failures following each result class's policy, and cancellation
  discarding partial union and composite results as well as intersections;
- semantic-token history evicting the oldest of three results and being cleared
  when the document closes.

### Real integration fixture

Fake-server tests validate LSP aggregation, but the
`single-context-editor-demo` repository is the real Dune/Merlin/ocaml-lsp
fixture that catches path, suffix, preprocessing, and source-map mistakes. Its
flake follows the three editor-mode prototype branches so `nix flake update`
tests their current heads together.

`test/protocol.t` must exercise Dune's tagged plural response directly. It
covers shared and exclusive sources, narrowed module sets, exact per-mode
counterparts in both directions, distinct dependencies and preprocessors,
singleton OCaml and Melange libraries, the legacy projection, and tagged lookup
errors.

`test/lsp.t` must open the real sources through ocaml-lsp and cover filtered
configuration sets, definition union, canonical symlink lookup and results,
exclusive-file navigation, divergent hover and document-symbol types,
completion intersection, universal and mode-specific diagnostic provenance,
counterpart union in both directions, and rename consensus. Copy staged source
dependencies with symlinks dereferenced; otherwise the fixture accidentally
tests Dune's build-tree staging path instead of its own authored project.

Keep ownership clear across repositories:

- Dune tests configuration generation, file filtering, wire responses, and
  legacy behavior.
- Merlin tests the shared wire codec, singular compatibility, and phase-cache
  isolation across alternating configurations.
- ocaml-lsp tests execution and presentation policy with a fake provider.
- merge Dune first and Merlin second; the first ocaml-lsp slice adds the
  integration job against those merged implementations.

## Suggested PR sequence

1. **Dune foundation:** #15493 is merged.
2. **Dune generation:** group all library modes, implement group-wide exact
   match precedence, expose all members through `merlin dump-config`, and add
   OCaml-only/Melange-only/shared generation tests. Keep `File` and
   `dump-dot-merlin` singular.
3. **Dune protocol:** persist mode and source-kind metadata, add the tagged
   `File-Configurations` request, robust stanza search,
   mode/kind/counterpart encoding, stale-file tests, and legacy compatibility
   tests.
4. **Merlin protocol:** add the shared codec and tests without changing
   standalone Merlin selection behavior.
5. **Merlin cache readiness:** make reader fingerprints configuration-correct
   and retain enough reader and PPX entries for alternating configurations.
6. **ocaml-lsp execution foundation:** load a nonempty set, add `dispatch_all`,
   remove active-mode state and selection requests, and land definition plus
   the first real-stack fixture as the first vertical slice.
7. **Read-only aggregation:** navigation, references, diagnostics, hover,
   signature help, inlay hints, symbols, folding, and highlights.
8. **Portable suggestions:** completion intersection, construct intersection,
   and multi-mode completion resolve.
9. **Constrained single results and mutations:** selection ranges, semantic
   tokens, rename, code actions, and enforcement of the default custom-request
   policy.
10. **Integration hardening:** extend the real-stack fixture across
   representative features, add performance measurements, and test stale
   diagnostic suppression and semantic-token delta history.

Each PR should keep old single-configuration behavior passing. During the
ocaml-lsp migration, the singular `config` accessor may exist as scaffolding,
but the final audit must list and justify every remaining semantic use.

## Acceptance criteria

- There is no editor-visible mode switch and no mutable active configuration.
- Exclusive files transparently use exactly one correct configuration.
- Shared files execute every applicable configuration for each standard LSP
  handler covered by the version 1 feature policies.
- Alternating configurations cannot reuse a reader or PPX result produced for a
  different configuration.
- Every configured stage of a migrated handler executes for every applicable
  mode before feature-specific aggregation.
- Navigation and diagnostics expose both modes without duplicates.
- Completion contains only portable entries.
- Mutations never apply a one-mode-only edit silently.
- Cross-document operations use Dune's exact authored counterpart for each mode
  and pair configurations by exact mode key; legacy configurations are paired
  only when the correspondence is unambiguous.
- Legacy `File` clients continue to work.
- One stanza still owns one `.merlin-conf` file.
- Stale unrelated persistent files do not mask a valid configuration.
- Authored locations are produced through real source lookup or mapping, never
  fabricated by clamping ranges.
- Returned source URIs resolve symlinks to canonical authored files.
- Configuration lookup resolves an opened source symlink before querying Dune,
  so the query and every returned URI use the same authored file identity.
- Completion resolve fetches the current configuration set and revalidates the
  item rather than relying on configuration identity from the original request.
- Semantic-token result IDs name exact encoded arrays, and retained prior
  results can serve as the base of a delta after a configuration change.

## Deliberate non-goals

- A user-facing OCaml/Melange mode selector.
- Keeping two persistent Merlin pipelines alive per document.
- Parallel execution on the single Merlin worker thread.
- A generic “merge any Merlin result” abstraction.
- Aggregate schemas for custom ocaml-lsp requests that are not explicit
  version 1 exceptions above.
- Position-level mode applicability within a shared source file. Version 1
  treats every file-applicable mode as participating at every position, so
  conditional preprocessing can produce conservative empty intersections or
  rejected edits. A later Merlin-side applicability signal can narrow the
  participating modes without changing the Dune wire response.
- Changing ambiguous multi-stanza ownership semantics in the same work.
- Adding `SOURCE_MAP` without a failing authored-location integration test.
- General recovery of a crashed or malformed configuration-server process.
- Mode-aware or otherwise strengthened `dump-dot-merlin` behavior.
- Preserving editor-opened symlink aliases in returned URIs or workspace edits.
