Test emitting JS for an installed Melange library with a private helper module

  $ mkdir -p lib app prefix

  $ cat > lib/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ cat > lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (private_modules helper))
  > EOF

  $ cat > lib/foo.ml <<EOF
  > let message = Helper.message
  > EOF

  $ cat > lib/helper.ml <<EOF
  > let message = "installed private helper"
  > EOF

  $ dune build --root lib @install
  Entering directory 'lib'
  Leaving directory 'lib'

  $ dune install --root lib --prefix $PWD/prefix --display short
  Installing $TESTCASE_ROOT/prefix/lib/foo/META
  Installing $TESTCASE_ROOT/prefix/lib/foo/dune-package
  Installing $TESTCASE_ROOT/prefix/lib/foo/foo.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/foo__.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/helper.ml
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/.private/foo__Helper.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/.private/foo__Helper.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo__.cmi
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo__.cmj
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo__.cmt
  Installing $TESTCASE_ROOT/prefix/lib/foo/melange/foo__Helper.cmj

  $ cat > app/dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > app/dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias dist)
  >  (emit_stdlib false)
  >  (libraries foo))
  > EOF

  $ cat > app/main.ml <<EOF
  > let () = Js.log Foo.message
  > EOF

  $ OCAMLPATH=$PWD/prefix/lib/:$OCAMLPATH dune build --root app @dist --display short 2>&1 | grep -v melange
  Entering directory 'app'
          melc dist/node_modules/foo/foo.js
          melc dist/node_modules/foo/foo__.js
          melc dist/node_modules/foo/helper.js
          melc dist/main.js
  Leaving directory 'app'

  $ node app/_build/default/dist/main.js
  installed private helper
