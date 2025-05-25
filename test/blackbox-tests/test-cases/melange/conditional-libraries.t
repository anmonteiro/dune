Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir -p lib_for_melange lib_for_native app/lib

Set up 2 different libraries in 2 packages -- one has `(modes melange)`, the
other has `(modes :standard)`

  $ cat > lib_for_melange/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name lib_for_melange))
  > (using melange 0.1)
  > EOF
  $ cat > lib_for_melange/dune <<EOF
  > (library
  >  (modes melange)
  >  (public_name lib_for_melange))
  > EOF
  $ cat > lib_for_melange/foo.ml <<EOF
  > let x = "lib for melange"
  > EOF

  $ dune build --root lib_for_melange
  Entering directory 'lib_for_melange'
  Leaving directory 'lib_for_melange'

  $ dune install --root lib_for_melange --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/META
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange.cmi
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange.cmj
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange.cmt
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange.ml
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange__Foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_melange/melange/lib_for_melange__Foo.cmt

  $ cat prefix/lib/lib_for_melange/dune-package

  $ cat > lib_for_native/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name lib_for_native))
  > (using melange 0.1)
  > EOF
  $ cat > lib_for_native/dune <<EOF
  > (library
  >  (modes :standard)
  >  (public_name lib_for_native))
  > EOF
  $ cat > lib_for_native/foo.ml <<EOF
  > let x = "lib for native"
  > EOF

  $ dune build --root lib_for_native
  Entering directory 'lib_for_native'
  Leaving directory 'lib_for_native'

  $ dune install --root lib_for_native --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/META
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.a
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.cma
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.cmi
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.cmt
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.cmx
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.cmxa
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.ml
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native__Foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native__Foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native__Foo.cmx
  Installing $TESTCASE_ROOT/prefix/lib/lib_for_native/lib_for_native.cmxs

  $ cat > app/dune-project <<EOF
  > (lang dune 3.13)
  > (package (name app))
  > (using melange 0.1)
  > EOF

  $ cat > app/lib/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name lib_for_app)
  >  (libraries lib_for_native)
  >  (melange.libraries lib_for_melange)
  >  (melange.preprocess (pps melange.ppx))
  >  (public_name app))
  > EOF

  $ cat > app/lib/common_intf.mli <<EOF
  > val message : string
  > EOF
  $ cat > app/lib/common_intf.ml <<EOF
  > let message = Lib_for_native.Foo.x
  > EOF
  $ cat > app/lib/common_intf.melange.ml <<EOF
  > let message = Lib_for_melange.Foo.x
  > EOF
  $ cat > app/lib/lib_for_app.ml <<EOF
  > let say_hello () = Format.eprintf "message: %s@." Common_intf.message
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target out)
  >  (modules x)
  >  (libraries app))
  >
  > (executable
  >  (name x)
  >  (modules x)
  >  (libraries app))
  > EOF
  $ cat > app/x.ml <<EOF
  > let () = Lib_for_app.say_hello ()
  > EOF

  $ cd app
  $ OCAMLPATH=$PWD/../prefix/lib/:$OCAMLPATH dune build @all --display=short

  $ node ./_build/default/out/x.js

  $ ./_build/default/x.exe
