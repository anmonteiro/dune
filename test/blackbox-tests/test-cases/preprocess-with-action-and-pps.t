Preprocess with an action chain and PPX

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > EOF
  $ cat > dune <<EOF
  > (library
  >  (name fooppx)
  >  (modules fooppx)
  >  (libraries ppxlib)
  >  (kind ppx_rewriter))
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (preprocess
  >   (action
  >    (progn
  >     (run cat %{input-file})
  >     (echo "let () = print_endline \"one more line\"")))
  >   (action
  >    (progn
  >     (run cat %{input-file})
  >     (echo "let () = print_endline \"last line\"")))
  >   (pps fooppx)))
  > EOF
  $ cat > foo.ml <<EOF
  > let () = print_endline "replaced with forty-two:"
  > let () = print_endline (string_of_int [%replace_with_42 1])
  > EOF

  $ cat >fooppx.ml <<EOF
  > open Ppxlib
  > let expand ~ctxt _i =
  >   let loc = Expansion_context.Extension.extension_point_loc ctxt in
  >   Ast_builder.Default.eint ~loc 42
  > let my_extension =
  >   Extension.V3.declare "replace_with_42" Extension.Context.expression
  >     Ast_pattern.(single_expr_payload (eint __))
  >     expand
  > let rule = Ppxlib.Context_free.Rule.extension my_extension
  > let () = Driver.register_transformation ~rules:[ rule ] "replace_with_42"
  > EOF

  $ dune exec ./foo.exe
  replaced with forty-two:
  42
  one more line
  last line
