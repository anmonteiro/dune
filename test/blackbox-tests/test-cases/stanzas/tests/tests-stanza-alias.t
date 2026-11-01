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

JS and Wasm aliases overridden by a child environment must still be registered
as standard aliases. Disabled tests exercise alias registration without needing
either compiler or runner.

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
  >  (_
  >   (js_of_ocaml (runtest_alias runtest-project-js))
  >   (wasm_of_ocaml (runtest_alias runtest-project-wasm))))
  > EOF
  $ mkdir overridden
  $ cat >overridden/dune <<EOF
  > (env
  >  (_
  >   (js_of_ocaml (runtest_alias runtest-child-js))
  >   (wasm_of_ocaml (runtest_alias runtest-child-wasm))))
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
  $ touch overridden/check.ml

The child aliases are selected. Check Dune's exit status before filtering out
unrelated aliases.

  $ dune show aliases overridden --context aliases > aliases 2>&1
  $ grep '^runtest-' aliases
  runtest-child-js
  runtest-child-wasm

Each command starts a fresh process and only loads the overridden directory.
None of its tests use these ancestor aliases, but requesting them must succeed.

  $ dune build @overridden/runtest-workspace-js @overridden/runtest-context-js @overridden/runtest-project-js
  Error: Alias "runtest-context-js" specified on the command line is empty.
  It is not defined in overridden or any of its descendants.
  Error: Alias "runtest-project-js" specified on the command line is empty.
  It is not defined in overridden or any of its descendants.
  Error: Alias "runtest-workspace-js" specified on the command line is empty.
  It is not defined in overridden or any of its descendants.
  [1]
  $ dune build @overridden/runtest-workspace-wasm @overridden/runtest-context-wasm @overridden/runtest-project-wasm
  Error: Alias "runtest-context-wasm" specified on the command line is empty.
  It is not defined in overridden or any of its descendants.
  Error: Alias "runtest-project-wasm" specified on the command line is empty.
  It is not defined in overridden or any of its descendants.
  Error: Alias "runtest-workspace-wasm" specified on the command line is empty.
  It is not defined in overridden or any of its descendants.
  [1]
