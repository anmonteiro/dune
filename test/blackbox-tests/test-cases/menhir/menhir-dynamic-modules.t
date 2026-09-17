Show an edge case of `(include_subdirs ..)` and ocamllex / menhir

  $ make_menhir_project 3.22 3.0

  $ mkdir -p gen

We define rules that create files (in the syntax expected by `modules`) that
each contain a single module name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo my_parser)))
  > EOF

We add a `(menhir ..)` stanza in the group root dune file

  $ mkdir -p src/a
  $ cat > src/dune << EOF
  > (include_subdirs unqualified)
  > (library (name foo))
  > (ocamllex lexer)
  > EOF
  $ cat > src/a/dune << EOF
  > (menhir
  >  (modules %{read-lines:../gen/lst})
  >  (flags --dump))
  > EOF

  $ make_trivial_ocamllex src/lexer.mll
  $ cat >src/a/my_parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main:
  >   | EOF { () }
  > EOF

Show that the menhir stanza must live next to the source

  $ dune build

A generated module list in the parser's own directory can describe both the
Menhir inputs and the library modules. Currently Menhir forces complete rule
loading before the independent list-producing rule can be built.

  $ make_menhir_project 3.25 3.0
  $ mkdir same-dir
  $ cat >same-dir/dune <<EOF
  > (rule
  >  (target parser-lst)
  >  (action (copy modules.in %{target})))
  > (menhir (modules (:include parser-lst)))
  > (library
  >  (name dynamic_parser)
  >  (modes byte)
  >  (modules (:include parser-lst)))
  > EOF
  $ cp src/a/my_parser.mly same-dir/parser.mly
  $ cp src/a/my_parser.mly same-dir/other_parser.mly
  $ echo parser >same-dir/modules.in
  $ dune build same-dir/dynamic_parser.cma
  Error: Dependency cycle between:
     (:include _build/default/same-dir/parser-lst) at same-dir/dune:4
  [1]

Changes to the list must also update the selected parser without cleaning.

  $ echo other_parser >same-dir/modules.in
  $ dune build same-dir/dynamic_parser.cma
  Error: Dependency cycle between:
     (:include _build/default/same-dir/parser-lst) at same-dir/dune:4
  [1]
