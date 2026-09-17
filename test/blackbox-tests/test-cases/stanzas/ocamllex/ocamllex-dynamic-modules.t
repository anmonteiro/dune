Test the ability of `(modules ..)` to contain dynamic
forms such as `(:include)` and variables such as `"%{read-lines:}"` in the
ocamllex / ocamlyacc stanzas.

  $ make_dune_project 3.21

  $ mkdir -p gen

We define a rule that creates a file (in sexp syntax, to be passed to
`(:include)`) containing a single name:

  $ cat >gen/dune <<EOF
  > (rule (with-stdout-to lst (echo mod)))
  > EOF

The unit `mod.mll` is present in the working tree, `lib.ml` uses it:

  $ make_trivial_ocamllex mod.mll

  $ cat >foo.ml <<EOF
  > let x = Mod.lex
  > EOF
  $ cat >dune <<EOF
  > (ocamllex (:include gen/lst))
  > (library (name foo))
  > EOF

Building under dune 3.22 throws an error

  $ dune build foo.cma
  File "dune", line 1, characters 10-28:
  1 | (ocamllex (:include gen/lst))
                ^^^^^^^^^^^^^^^^^^
  Error: the ability to specify non-constant module lists is only available
  since version 3.22 of the dune language. Please update your dune-project file
  to have (lang dune 3.22).
  [1]

  $ make_dune_project 3.22
  $ dune build foo.cma

`%{read-lines:..}` also works

  $ dune clean
  $ cat >dune <<EOF
  > (ocamllex
  >  (modules (:include gen/lst)))
  > (library (name foo))
  > EOF

  $ dune build foo.cma

The `flags` field is available starting in Dune 3.25:

  $ make_dune_project 3.24
  $ cat >dune <<EOF
  > (ocamllex
  >  (modules mod)
  >  (flags -ml))
  > (library (name foo))
  > EOF

  $ dune build foo.cma
  File "dune", line 3, characters 1-12:
  3 |  (flags -ml))
       ^^^^^^^^^^^
  Error: 'flags' is only available since version 3.25 of the dune language.
  Please update your dune-project file to have (lang dune 3.25).
  [1]

Flags support the ordered set language and variable expansion. In particular,
`-ml` asks `ocamllex` to generate an OCaml-based automaton:

  $ make_dune_project 3.25
  $ echo '%{read-lines:gen/flag-value}' >gen/flags
  $ echo -ml >gen/flag-value
  $ cat >dune <<EOF
  > (ocamllex
  >  (modules mod)
  >  (flags (:include gen/flags)))
  > (library (name foo))
  > EOF

  $ dune build foo.cma
  $ dune trace cat \
  >   | jq_dune -c 'processesBrief | select(.prog == "ocamllex") | .args'
  ["-ml","-q","-o","mod.ml","mod.mll"]

Dynamic module names can refer to copied inputs.

  $ mkdir -p copied-input/inputs
  $ make_trivial_ocamllex copied-input/inputs/lexer.mll
  $ cat >copied-input/dune <<EOF
  > (copy_files inputs/*.mll)
  > (rule
  >  (target parsers)
  >  (action (write-file %{target} lexer)))
  > (ocamllex (modules (:include parsers)))
  > (library (name copied_input) (modes byte))
  > EOF
  $ cat >copied-input/copied_input.ml <<EOF
  > let lex = Lexer.lex
  > EOF
  $ dune build copied-input/copied_input.cma copied-input/lexer.ml

Explicit module names can refer to inputs from rules with inferred targets.

  $ mkdir -p inferred-input/inputs
  $ make_trivial_ocamllex inferred-input/inputs/lexer.mll
  $ cat >inferred-input/dune <<EOF
  > (rule (copy inputs/lexer.mll lexer.mll))
  > (ocamllex lexer)
  > (library (name inferred_input) (modes byte))
  > EOF
  $ cat >inferred-input/inferred_input.ml <<EOF
  > let lex = Lexer.lex
  > EOF
  $ dune build inferred-input/inferred_input.cma inferred-input/lexer.ml
