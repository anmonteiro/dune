An incorrect implementation must not prevent independent data rules from being
loaded in the same directory.

  $ make_dune_project 3.7

We define an invalid library along with an independent rule and an executable.
Compilation still shares module discovery with the invalid library.

  $ cat >dune <<EOF
  > (library
  >  (name foo)
  >  (modules :standard \ foo)
  >  (implements fake-dummy))
  > (rule (with-stdout-to test (echo foo)))
  > (executable
  >  (name foo)
  >  (modules foo))
  > EOF

  $ touch foo.ml
  $ dune build ./test
  $ dune build ./foo.exe
  File "dune", line 4, characters 13-23:
  4 |  (implements fake-dummy))
                   ^^^^^^^^^^
  Error: Library "fake-dummy" not found.
  [1]
