The compilation of a menhir parser involves the use of `ocamlc -i` to infer
types which are required by menhir. This can cause issues as the inferred types
may refer to hidden modules.

  $ make_menhir_project 3.13 3.0

  $ cat > dune <<EOF
  > (menhir (modules parser))
  > (library (name mylib))
  > EOF

The setup to force `ocamlc -i` to infer hidden module names is a bit finicky,
as ocaml short-paths is generally smart enough to avoid them: This test is
likely to break if OCaml changes its heuristics.

  $ cat > ast.ml <<EOF
  > module Int_list = struct
  >   type t = int list
  > end
  > module Int_list_option = struct
  >   type t = Int_list.t option
  > end
  > EOF

It's common for AST parsers to come with a helper module, as an overlay over
the Ast module:

  $ cat > util.ml <<EOF
  > type ilo = Ast.Int_list_option.t
  > EOF

Finally the parser, such that the type of `main` is inferred from the type
annotation of `block`:

  $ cat > parser.mly <<'EOF'
  > %token A B Eof
  > %type <Util.ilo> block
  > %start <_> main
  > %%
  > let block :=
  >   | B; ~ = main; Eof; <Some>
  >   | Eof; { None }
  > 
  > let main :=
  >   | A; { [1] }
  >   | ~ = block; <Option.get>
  > %%
  > EOF

The first issue #2450 is that `ocamlc -i` may generate a reference to
`Mylib.Ast` in the inferred signature, which causes dune to detect a cycle:

  $ dune build
  Error: Module Parser in directory _build/default depends on Mylib.
  This doesn't make sense to me.
  
  Mylib is the main module of the library and is the only module exposed
  outside of the library. Consequently, it should be the one depending on all
  the other modules in the library.
  -> required by transitive deps of mylib__Parser.impl in _build/default
  -> required by _build/default/.mylib.objs/native/mylib__Parser.cmx
  -> required by _build/default/mylib.a
  -> required by alias all
  -> required by alias default
  [1]

  $ grep Mylib _build/default/parser.mli
  val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mylib.Ast.Int_list.t)

Issue #2450 describes this exact issue and propose a trick solution to add
`module Mylib = struct end` at the beginning such that `ocamlc -i` will
not generate a cyclic reference to `Mylib.Ast`:

  $ cat > parser.mly <<'EOF'
  > %{
  > module Mylib = struct end
  > %}
  > %token A B Eof
  > %type <Util.ilo> block
  > %start <_> main
  > %%
  > let block :=
  >   | B; ~ = main; Eof; <Some>
  >   | Eof; { None }
  > 
  > let main :=
  >   | A; { [1] }
  >   | ~ = block; <Option.get>
  > %%
  > EOF

However, in this setup, it instead produces a reference to the hidden
`Mylib__Ast`:

  $ dune build parser.mli
  $ grep Mylib _build/default/parser.mli
  val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Mylib__Ast.Int_list.t)

The dependency on `Mylib__Ast` must be tracked even though the inferred type
uses the physical module name:

  $ dune describe rules --format=json %{cmi:parser} \
  > | jq_dune '[ .[] | ruleDepFilePathsOfKind("In_build_dir") ]'
  [
    "_build/default/.mylib.objs/byte/mylib.cmi",
    "_build/default/.mylib.objs/byte/mylib__Ast.cmi",
    "_build/default/parser.mli",
    "_build/default/parser.mly"
  ]

Build the parser after its dependency is available:

  $ dune build %{cmi:ast}
  $ dune build

Updating that dependency must rebuild `parser.cmi` as well:

  $ echo 'let dummy = 42' >> ast.ml
  $ dune build
