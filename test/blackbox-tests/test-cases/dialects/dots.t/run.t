Test the (dialect ...) stanza inside the `dune-project` file.

  $ { echo '(lang dune 3.8)'; cat dune-project.in; } >dune-project

  $ dune build
  File "dune-project", line 5, characters 13-20:
  5 |   (extension cppo.ml)
                   ^^^^^^^
  Error: the possibility of defining extensions containing periods is only
  available since version 3.9 of the dune language. Please update your
  dune-project file to have (lang dune 3.9).
  [1]

  $ { echo '(lang dune 3.9)'; cat dune-project.in; } >dune-project

  $ dune build @show
  print_endline "Hello, World"

Dialect extensions are literal strings, even when they contain glob syntax.
An unused dialect with an unmatched bracket does not affect rule generation.

  $ mkdir unused-extension
  $ cd unused-extension
  $ cat >dune-project <<EOF
  > (lang dune 3.25)
  > (dialect
  >  (name custom)
  >  (implementation
  >   (extension "x[")
  >   (preprocess (cat %{input-file}))))
  > EOF
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF
  $ cat >foo.ml <<EOF
  > let x = 42
  > EOF
  $ dune build --root . foo.cma
  $ cd ..

A copied dialect source has the same literal preprocessing suffix even when
its filename is discovered dynamically.

  $ mkdir copied-extension
  $ cd copied-extension
  $ cat >dune-project <<EOF
  > (lang dune 3.25)
  > (dialect
  >  (name custom)
  >  (implementation
  >   (extension "x[ab]")
  >   (preprocess (cat %{input-file}))))
  > EOF
  $ cat >dune <<EOF
  > (library (name foo))
  > (copy_files %{env:SRC_DIR=inputs}/*)
  > EOF
  $ mkdir inputs
  $ cat >'inputs/foo.x[ab]' <<EOF
  > let x = 42
  > EOF
  $ dune build --root . foo.cma
  $ cd ..
