Test library modes field

  $ make_dune_project 3.7

  $ mkdir lib

  $ cat > lib/dune <<EOF
  > (library
  >  (modes :standard \ native)
  >  (name mylib))
  > EOF

  $ cat > lib/mylib.ml <<EOF
  > let some_binding = "string"
  > EOF

  $ cat > dune <<EOF
  > (executable
  >  (name hello)
  >  (libraries mylib))
  > EOF

  $ cat > hello.ml <<EOF
  > let () =
  >   print_endline Mylib.some_binding
  > EOF

Fails with an informative error message if we parsed OSL for modes
in a version of dune lang that does not support them

  $ dune build hello.exe
  File "lib/dune", lines 1-3, characters 0-51:
  1 | (library
  2 |  (modes :standard \ native)
  3 |  (name mylib))
  Error: Ordered set language for modes is only available since version 3.8 of
  the dune language. Please update your dune-project file to have (lang dune
  3.8).
  [1]

  $ make_dune_project 3.8

Works for the most recent version

  $ dune build hello.exe

An explicitly empty mode list is rejected.

  $ cat > lib/dune <<EOF
  > (library
  >  (modes)
  >  (name mylib))
  > EOF

  $ dune build
  File "lib/dune", line 2, characters 1-8:
  2 |  (modes)
       ^^^^^^^
  Error: No library mode defined
  [1]

The ordered set language cannot be used to remove every mode either.

  $ cat > lib/dune <<EOF
  > (library
  >  (modes :standard \ byte best)
  >  (name mylib))
  > EOF

  $ dune build
  File "lib/dune", line 2, characters 1-30:
  2 |  (modes :standard \ byte best)
       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: No library mode defined
  [1]
