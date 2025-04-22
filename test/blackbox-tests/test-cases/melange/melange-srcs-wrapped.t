
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package  (name pkg) (allow_empty))
  > (using melange 0.1)
  > EOF

  $ mkdir a

  $ cat > a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name a)
  >  (package pkg)
  >  (modes melange byte))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/bar.ml <<EOF
  > let x = "bar"
  > EOF

  $ dune build a/.melange_src/foo.ml
  $ dune build _build/default/a/.a.objs/melange/a.cmi
  $ tree -a _build/default/a
  _build/default/a
  |-- .a.objs
  |   `-- melange
  |       |-- a.cmi
  |       |-- a.cmj
  |       `-- a.cmt
  |-- .melange_src
  |   |-- a.ml-gen
  |   `-- foo.ml -> ../foo.ml
  |-- .merlin-conf
  |   `-- lib-a
  `-- foo.ml
  
  5 directories, 7 files

  $ dune build
  $ tree -a _build/default/a
  _build/default/a
  |-- .a.objs
  |   |-- a__Bar.impl.all-deps
  |   |-- a__Bar.impl.d
  |   |-- a__Foo.impl.all-deps
  |   |-- a__Foo.impl.d
  |   |-- byte
  |   |   |-- a.cmi
  |   |   |-- a.cmo
  |   |   |-- a.cmt
  |   |   |-- a__Bar.cmi
  |   |   |-- a__Bar.cmo
  |   |   |-- a__Bar.cmt
  |   |   |-- a__Foo.cmi
  |   |   |-- a__Foo.cmo
  |   |   `-- a__Foo.cmt
  |   `-- melange
  |       |-- a.cmi
  |       |-- a.cmj
  |       |-- a.cmt
  |       |-- a__Bar.cmi
  |       |-- a__Bar.cmj
  |       |-- a__Bar.cmt
  |       |-- a__Bar.impl.all-deps
  |       |-- a__Bar.impl.d
  |       |-- a__Foo.cmi
  |       |-- a__Foo.cmj
  |       |-- a__Foo.cmt
  |       |-- a__Foo.impl.all-deps
  |       `-- a__Foo.impl.d
  |-- .melange_src
  |   |-- a.ml-gen
  |   |-- bar.ml -> ../bar.ml
  |   `-- foo.ml -> ../foo.ml
  |-- .merlin-conf
  |   `-- lib-a
  |-- a.cma
  |-- a.ml-gen
  |-- bar.ml
  `-- foo.ml
  
  6 directories, 34 files

