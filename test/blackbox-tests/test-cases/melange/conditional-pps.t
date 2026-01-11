Test that paths in `node_modules` are correct for sub-libraries of the
form `foo.bar.baz`

  $ mkdir a app
  $ cat > a/dune-project <<EOF
  > (lang dune 3.8)
  > (package (name a))
  > (using melange 0.1)
  > EOF
  $ cat > a/dune <<EOF
  > (library
  >  (modes melange :standard)
  >  (name a)
  >  (melange.preprocess (pps melange.ppx))
  >  (public_name a.sub))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/melange_only.melange.ml <<EOF
  > external x : unit -> < > Js.t = "" [@@mel.obj]
  > let x = x ()
  > EOF

  $ dune build --root a
  $ find ./a/_build/default -type f | censor | sort
  ./a/_build/default/.a.objs/a__Foo.impl.all-deps
  ./a/_build/default/.a.objs/byte/a.cmi
  ./a/_build/default/.a.objs/byte/a.cmo
  ./a/_build/default/.a.objs/byte/a.cmt
  ./a/_build/default/.a.objs/byte/a__Foo.cmi
  ./a/_build/default/.a.objs/byte/a__Foo.cmo
  ./a/_build/default/.a.objs/byte/a__Foo.cmt
  ./a/_build/default/.a.objs/melange/a.cmi
  ./a/_build/default/.a.objs/melange/a.cmj
  ./a/_build/default/.a.objs/melange/a.cmt
  ./a/_build/default/.a.objs/melange/a__Foo.cmi
  ./a/_build/default/.a.objs/melange/a__Foo.cmj
  ./a/_build/default/.a.objs/melange/a__Foo.cmt
  ./a/_build/default/.a.objs/melange/a__Foo.impl.all-deps
  ./a/_build/default/.a.objs/melange/a__Melange_only.cmi
  ./a/_build/default/.a.objs/melange/a__Melange_only.cmj
  ./a/_build/default/.a.objs/melange/a__Melange_only.cmt
  ./a/_build/default/.a.objs/melange/a__Melange_only.impl.all-deps
  ./a/_build/default/.a.objs/native/a.cmx
  ./a/_build/default/.a.objs/native/a.o
  ./a/_build/default/.a.objs/native/a__Foo.cmx
  ./a/_build/default/.a.objs/native/a__Foo.o
  ./a/_build/default/.dune/configurator
  ./a/_build/default/.dune/configurator.v2
  ./a/_build/default/.melange_src/a.ml-gen
  ./a/_build/default/.melange_src/foo.ml
  ./a/_build/default/.melange_src/foo.pp.ml
  ./a/_build/default/.melange_src/melange_only.ml
  ./a/_build/default/.melange_src/melange_only.pp.ml
  ./a/_build/default/.merlin-conf/lib-a.sub
  ./a/_build/default/.ppx/$DIGEST/_ppx.ml-gen
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.cmi
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.cmo
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.cmx
  ./a/_build/default/.ppx/$DIGEST/dune__exe___ppx.o
  ./a/_build/default/.ppx/$DIGEST/ppx.exe
  ./a/_build/default/META.a
  ./a/_build/default/a.a
  ./a/_build/default/a.cma
  ./a/_build/default/a.cmxa
  ./a/_build/default/a.cmxs
  ./a/_build/default/a.dune-package
  ./a/_build/default/a.install
  ./a/_build/default/a.ml-gen
  ./a/_build/default/foo.ml
  ./a/_build/default/melange_only.melange.ml
