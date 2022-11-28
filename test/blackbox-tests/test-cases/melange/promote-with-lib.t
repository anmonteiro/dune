Test melange.emit promotion

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (modes melange)
  >  (name mylib))
  > EOF

  $ cat > lib/mylib.ml <<EOF
  > let some_binding = Some "string"
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias dist)
  >  (entries hello)
  >  (promote (until-clean))
  >  (libraries mylib)
  >  (module_system commonjs))
  > EOF

  $ cat > hello.ml <<EOF
  > let the_binding = Mylib.some_binding
  > let () =
  >   print_endline "hello"
  > EOF

  $ dune build @dist --auto-promote

Targets are promoted to the source tree

  $ ls .
  _build
  dune
  dune-project
  hello.js
  hello.ml
  lib
  $ ls ./lib
  dune
  mylib.js
  mylib.ml

  $ node ./hello.js
  hello

(until-clean) causes JS file targets to be deleted after calling dune clean

  $ dune clean
  $ ls .
  dune
  dune-project
  hello.ml
  lib
  $ ls ./lib
  dune
  mylib.ml
