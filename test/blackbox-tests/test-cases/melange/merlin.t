 Temporary special merlin support for melange only libs

  $ melc_where="$(melc -where)"
  $ export BUILD_PATH_PREFIX_MAP="/MELC_WHERE=$melc_where:$BUILD_PATH_PREFIX_MAP"
  $ melc_compiler="$(which melc)"
  $ export BUILD_PATH_PREFIX_MAP="/MELC_COMPILER=$melc_compiler:$BUILD_PATH_PREFIX_MAP"

  $ cat >dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ lib=foo
  $ cat >dune <<EOF
  > (library
  >  (name $lib)
  >  (private_modules bar)
  >  (modes melange))
  > EOF

  $ touch bar.ml $lib.ml
  $ dune build @check
  $ dune ocaml merlin dump-config "$PWD" | grep -i "$lib"
  Foo
    $TESTCASE_ROOT/_build/default/.foo.objs/melange)
     Foo__
    $TESTCASE_ROOT/_build/default/.foo.objs/melange)
     Foo__
  Foo__
    $TESTCASE_ROOT/_build/default/.foo.objs/melange)
     Foo__

All 3 entries (Foo, Foo__ and Bar) contain a ppx directive

  $ dune ocaml merlin dump-config $PWD | grep -i "ppx"
   (FLG (-ppx "/MELC_COMPILER -as-ppx -bs-jsx 3"))
   (FLG (-ppx "/MELC_COMPILER -as-ppx -bs-jsx 3"))
   (FLG (-ppx "/MELC_COMPILER -as-ppx -bs-jsx 3"))

  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs))
  > EOF

  $ touch output/main.ml
  $ dune build @check
  $ dune ocaml merlin dump-config "$PWD/output" | grep -i output
    $TESTCASE_ROOT/_build/default/output/.output.mobjs/melange)
    $TESTCASE_ROOT/output)

The melange.emit entry contains a ppx directive

  $ dune ocaml merlin dump-config $PWD/output | grep -i "ppx"
   (FLG (-ppx "/MELC_COMPILER -as-ppx -bs-jsx 3"))

  $ dune ocaml dump-dot-merlin $PWD
  EXCLUDE_QUERY_DIR
  STDLIB /MELC_WHERE
  B $TESTCASE_ROOT/_build/default/.foo.objs/melange
  S $TESTCASE_ROOT
  # FLG -ppx '/MELC_COMPILER -as-ppx -bs-jsx 3'
  # FLG -open Foo__ -w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs

  $ dune ocaml dump-dot-merlin $PWD/output
  EXCLUDE_QUERY_DIR
  STDLIB /MELC_WHERE
  B $TESTCASE_ROOT/_build/default/output/.output.mobjs/melange
  S $TESTCASE_ROOT/output
  # FLG -ppx '/MELC_COMPILER -as-ppx -bs-jsx 3'
  # FLG -w @1..3@5..28@30..39@43@46..47@49..57@61..62@67@69-40 -strict-sequence -strict-formats -short-paths -keep-locs

