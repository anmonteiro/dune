Tests stanzas produce aliases with the executable names

  $ make_dune_project 3.17

  $ cat > dune << EOF
  > (tests
  >  (names a b))
  > EOF

  $ cat > a.ml << EOF
  > let () = print_endline "a"
  > EOF

  $ cat > b.ml << EOF
  > let () = print_endline "b"
  > EOF

  $ dune build @runtest-a @runtest-b
  a
  b

Checking interaction with enabled_if

  $ cat > dune << EOF
  > (tests
  >  (names a b)
  >  (enabled_if false))
  > EOF

  $ dune build @a @b
  Error: Alias "a" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  Hint: did you mean all?
  Error: Alias "b" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  [1]

JS and Wasm test aliases inherit independently through workspace, context,
project and nested environments. These tests are disabled, so no JS or Wasm
compiler or runner is needed to inspect their aliases.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > (env
  >  (_
  >   (js_of_ocaml (runtest_alias runtest-workspace-js))
  >   (wasm_of_ocaml (runtest_alias runtest-workspace-wasm))))
  > (context
  >  (default
  >   (name aliases)
  >   (env
  >    (_
  >     (js_of_ocaml (runtest_alias runtest-context-js))
  >     (wasm_of_ocaml (runtest_alias runtest-context-wasm))))))
  > EOF
  $ cat >dune <<EOF
  > (env
  >  (dev
  >   (js_of_ocaml (runtest_alias runtest-project-js))
  >   (wasm_of_ocaml (runtest_alias runtest-project-wasm)))
  >  (release
  >   (js_of_ocaml (runtest_alias runtest-release-js))))
  > EOF
  $ mkdir inherited overridden
  $ for dir in inherited overridden; do
  >   cat >"$dir/dune" <<EOF
  > (test
  >  (name check)
  >  (modes js wasm)
  >  (enabled_if false)
  >  (js_of_ocaml
  >   (enabled_if true)
  >   (compilation_mode whole_program)
  >   (sourcemap no))
  >  (wasm_of_ocaml
  >   (enabled_if true)
  >   (compilation_mode whole_program)
  >   (sourcemap no)))
  > EOF
  >   touch "$dir/check.ml"
  > done
  $ cat >>overridden/dune <<EOF
  > (env
  >  (_
  >   (js_of_ocaml (runtest_alias runtest-child-js))
  >   (wasm_of_ocaml (runtest_alias runtest-child-wasm))))
  > EOF

Inspect Dune's exit status separately from filtering the unrelated aliases.

  $ dune show aliases inherited --context aliases > aliases 2>&1
  $ grep '^runtest-' aliases
  runtest-project-js
  runtest-project-wasm
  $ dune show aliases overridden --context aliases > aliases 2>&1
  $ grep '^runtest-' aliases
  runtest-child-js
  runtest-child-wasm

The release profile overrides only JS; Wasm falls back to the context setting.

  $ dune show aliases inherited --context aliases --profile release > aliases 2>&1
  $ grep '^runtest-' aliases
  runtest-context-wasm
  runtest-release-js

Overridden ancestor aliases remain standard aliases, so recursively requesting
one succeeds even though no test uses that name. Each command runs in a fresh
process and must register the ancestors while loading the test rules.

  $ dune build @overridden/runtest-workspace-js @overridden/runtest-context-js @overridden/runtest-project-js
  $ dune build @overridden/runtest-workspace-wasm @overridden/runtest-context-wasm @overridden/runtest-project-wasm

A nested project does not inherit its parent project's environment, but still
inherits the workspace and context.

  $ cp -R inherited nested
  $ cat >nested/dune-project <<EOF
  > (lang dune 3.17)
  > EOF
  $ dune show aliases nested --context aliases > aliases 2>&1
  $ grep '^runtest-' aliases
  runtest-context-js
  runtest-context-wasm

Without any environment aliases, both modes use the ordinary runtest alias.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.17)
  > EOF
  $ cat >dune <<EOF
  > EOF
  $ dune show aliases inherited > aliases 2>&1
  $ grep '^runtest' aliases
  runtest
