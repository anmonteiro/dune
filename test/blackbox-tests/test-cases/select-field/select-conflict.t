We test the `(select)` field of the `(libraries)` field in the presence of
a conflicting rule

  $ make_dune_project 3.22

  $ cat > dune <<EOF
  > (ocamllex (modules lexer))
  > (library
  >  (name foo)
  >  (libraries
  >   (select lexer.ml from
  >    (unix -> lexer.unix.ml)
  >    (!unix -> lexer.nounix.ml))))
  > EOF
  $ make_trivial_ocamllex lexer.mll

  $ cat > lexer.unix.ml <<EOF
  > let () = print_endline "Test: Unix was found!"
  > EOF
  $ cat > lexer.nounix.ml <<EOF
  > let () = print_endline "Test: Unix was not found!"
  > EOF

  $ dune build foo.cma
  Error: Multiple rules generated for _build/default/lexer.ml:
  - dune:1
  - dune:5
  -> required by transitive deps of foo__Lexer.impl in _build/default
  -> required by _build/default/.foo.objs/byte/foo__Lexer.cmo
  -> required by _build/default/foo.cma
  [1]
