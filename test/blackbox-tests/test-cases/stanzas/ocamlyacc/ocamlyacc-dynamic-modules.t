Test the ability of `(modules ..)` to contain dynamic forms such as
`(:include)` and variables such as `%{read-lines:...}` in the `ocamlyacc`
stanza.

  $ make_dune_project 3.22

  $ mkdir -p gen

We define rules that create files (in the syntax expected by `modules`) that
each contain a single module name. Configuration files must already be available
to this generation-time action, without explicit configuration dependencies:

  $ cat >gen/dune <<'EOF'
  > (rule
  >  (progn
  >   (bash "test -s \"$INSIDE_DUNE/.dune/configurator\"")
  >   (bash "test -s \"$INSIDE_DUNE/.dune/configurator.v2\"")
  >   (with-stdout-to lst (echo my_parser))))
  > EOF

`.mly` unit present in the working tree. `lib.ml` references it

  $ cat >my_parser.mly <<'EOF'
  > %token EOF
  > %start main
  > %type <unit> main
  > %%
  > main:
  >   | EOF { () }
  > EOF

  $ cat >lib.ml <<'EOF'
  > let _ = My_parser.main
  > EOF

The `ocamlyacc` stanza uses `%{read-lines:..}` inside the `(modules ..)` field:

  $ cat >dune <<EOF
  > (library
  >  (name lib))
  > (ocamlyacc
  >  (modules %{read-lines:gen/lst}))
  > EOF

  $ dune build lib.cma
