  $ cat >test.ml <<EOF
  > (*TEST: assert (1 = 2) *)
  > EOF

  $ make_dune_project 2.6

  $ write_simple_inline_tests_backend
  $ cat >>dune <<EOF
  > 
  > (env
  >  (ignore-inline-tests (inline_tests ignored))
  >  (enable-inline-tests (inline_tests enabled))
  >  (disable-inline-tests (inline_tests disabled)))
  > EOF

  $ env -u OCAMLRUNPARAM dune runtest
  File "dune", lines 9-11, characters 1-57:
   9 |  (inline_tests
  10 |   (modes byte)
  11 |   (backend backend_simple)))
  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  [1]

Inline tests also generate an alias
  $ dune build @runtest-foo_simple
  File "dune", lines 9-11, characters 1-57:
   9 |  (inline_tests
  10 |   (modes byte)
  11 |   (backend backend_simple)))
  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  [1]

Make sure building both aliases doesn't build both
  $ dune build @runtest @lib-foo_simple
  Error: Alias "lib-foo_simple" specified on the command line is empty.
  It is not defined in . or any of its descendants.
  File "dune", lines 9-11, characters 1-57:
   9 |  (inline_tests
  10 |   (modes byte)
  11 |   (backend backend_simple)))
  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  [1]
This test demonstrates that the action is being run once
  $ dune trace cat | jq_dune '
  >   processes
  > | .args.prog
  > | select(contains("inline-test-runner.bc"))
  > '
  ".foo_simple.inline-tests/inline-test-runner.bc"

The expected behavior for the following three tests is to output nothing: the tests are disabled or ignored.
  $ env -u OCAMLRUNPARAM dune runtest --profile release

  $ env -u OCAMLRUNPARAM dune runtest --profile disable-inline-tests

  $ env -u OCAMLRUNPARAM dune runtest --profile ignore-inline-tests

  $ env -u OCAMLRUNPARAM dune runtest --profile enable-inline-tests
  File "dune", lines 9-11, characters 1-57:
   9 |  (inline_tests
  10 |   (modes byte)
  11 |   (backend backend_simple)))
  Fatal error: exception File ".foo_simple.inline-tests/main.ml-gen", line 1, characters 40-46: Assertion failed
  [1]

Inline-test rule generation must not prevent an independent rule from
providing the library's module list. Currently this dependency cycles.

  $ make_dune_project 3.25
  $ mkdir dynamic
  $ cat >dynamic/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (write-file %{target} test)))
  > (library
  >  (name dynamic_tests)
  >  (modules (:include lst))
  >  (inline_tests
  >   (modes byte)
  >   (backend backend_simple)))
  > EOF
  $ cat >dynamic/test.ml <<EOF
  > (*TEST: assert (1 = 1) *)
  > EOF
  $ dune build @dynamic/runtest
  Error: Dependency cycle between:
     (modules) field at dynamic/dune:4
  -> (:include _build/default/dynamic/lst) at dynamic/dune:6
  -> (modules) field at dynamic/dune:4
  [1]
