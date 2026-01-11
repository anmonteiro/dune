
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package  (name pkg) (allow_empty))
  > (using melange 0.1)
  > EOF

  $ mkdir a a/b a/b/c

  $ cat > a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name a)
  >  (package pkg)
  >  (wrapped false)
  >  (modes melange byte))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/b/bar.ml <<EOF
  > let x = "bar"
  > EOF
  $ cat > a/b/c/qux.ml <<EOF
  > let x = "bar"
  > EOF

  $ dune build
  $ tree -a _build/default/a
  _build/default/a
  |-- .a.objs
  |   |-- bar.impl.all-deps
  |   |-- byte
  |   |   |-- bar.cmi
  |   |   |-- bar.cmo
  |   |   |-- bar.cmt
  |   |   |-- foo.cmi
  |   |   |-- foo.cmo
  |   |   |-- foo.cmt
  |   |   |-- qux.cmi
  |   |   |-- qux.cmo
  |   |   `-- qux.cmt
  |   |-- foo.impl.all-deps
  |   |-- melange
  |   |   |-- bar.cmi
  |   |   |-- bar.cmj
  |   |   |-- bar.cmt
  |   |   |-- bar.impl.all-deps
  |   |   |-- foo.cmi
  |   |   |-- foo.cmj
  |   |   |-- foo.cmt
  |   |   |-- foo.impl.all-deps
  |   |   |-- qux.cmi
  |   |   |-- qux.cmj
  |   |   |-- qux.cmt
  |   |   `-- qux.impl.all-deps
  |   `-- qux.impl.all-deps
  |-- .melange_src
  |   |-- b
  |   |   |-- bar.ml
  |   |   `-- c
  |   |       `-- qux.ml
  |   `-- foo.ml
  |-- .merlin-conf
  |   `-- lib-a
  |-- a.cma
  |-- b
  |   |-- bar.ml
  |   `-- c
  |       `-- qux.ml
  `-- foo.ml
  
  10 directories, 32 files
