Test `(include_subdirs qualified)` in the presence of invalid module name
directories that don't contain source files

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > (using melange 1.0)
  > EOF

  $ mkdir -p a/b/c

  $ cat > a/dune <<EOF
  > (include_subdirs qualified)
  > (library (name foo) (modes melange))
  > EOF

  $ cat > a/b/c/dune <<EOF
  > (ocamllex lexer)
  > EOF
  $ cat > a/b/c/lexer.mll <<EOF
  > {
  > }
  > rule lex = parse
  >   | _   { true  }
  >   | eof { false }
  > EOF

  $ cat > a/foo.ml <<EOF
  > module L = B.C.Lexer
  > EOF


  $ dune build
  $ find _build/default/a/.melange_src | sort
  _build/default/a/.melange_src
  _build/default/a/.melange_src/b
  _build/default/a/.melange_src/b/c
  _build/default/a/.melange_src/b/c/lexer.ml
  _build/default/a/.melange_src/foo.ml
  _build/default/a/.melange_src/foo__.ml-gen
  _build/default/a/.melange_src/foo__B.ml-gen
  _build/default/a/.melange_src/foo__B__C.ml-gen
