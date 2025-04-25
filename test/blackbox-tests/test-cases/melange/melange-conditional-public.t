
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package (name pkg))
  > (using melange 0.1)
  > EOF

  $ mkdir a a/b

  $ cat > a/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name a)
  >  (public_name pkg)
  >  (modes :standard melange))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "ocaml"
  > EOF
  $ cat > a/b/bar.ml <<EOF
  > let x = "bar"
  > EOF
  $ cat > a/foo.melange.ml <<EOF
  > let x = "melange"
  > EOF
  $ cat > a/melange_only.melange.ml <<EOF
  > let x = "melange only file"
  > EOF

  $ dune build a/.melange_src/foo.ml
  $ cat _build/default/a/.melange_src/foo.ml
  let x = "melange"

  $ dune build @install --display short
        ocamlc a/.a.objs/byte/a.{cmi,cmo,cmt}
        ocamlc a/.a.objs/byte/a__B.{cmi,cmo,cmt}
      ocamldep a/.a.objs/a__Foo.impl.d
          melc a/.a.objs/melange/a.{cmi,cmj,cmt}
          melc a/.a.objs/melange/a__B.{cmi,cmj,cmt}
      ocamldep a/.a.objs/melange/a__Foo.impl.d
      ocamldep a/.a.objs/melange/a__Melange_only.impl.d
      ocamldep a/.a.objs/a__B__Bar.impl.d
      ocamldep a/.a.objs/melange/a__B__Bar.impl.d
      ocamlopt a/.a.objs/native/a.{cmx,o}
      ocamlopt a/.a.objs/native/a__B.{cmx,o}
        ocamlc a/.a.objs/byte/a__Foo.{cmi,cmo,cmt}
          melc a/.a.objs/melange/a__Foo.{cmi,cmj,cmt}
          melc a/.a.objs/melange/a__Melange_only.{cmi,cmj,cmt}
        ocamlc a/.a.objs/byte/a__B__Bar.{cmi,cmo,cmt}
          melc a/.a.objs/melange/a__B__Bar.{cmi,cmj,cmt}
      ocamlopt a/.a.objs/native/a__Foo.{cmx,o}
      ocamlopt a/.a.objs/native/a__B__Bar.{cmx,o}
        ocamlc a/a.cma
      ocamlopt a/a.{a,cmxa}
      ocamlopt a/a.cmxs

  $ cat _build/install/default/lib/pkg/foo.ml
  let x = "ocaml"
  $ cat _build/install/default/lib/pkg/melange/foo.ml
  let x = "melange"

  $ tree -a _build/install 2>&1
  _build/install
  `-- default
      `-- lib
          `-- pkg
              |-- META -> ../../../../default/META.pkg
              |-- a.a -> ../../../../default/a/a.a
              |-- a.cma -> ../../../../default/a/a.cma
              |-- a.cmi -> ../../../../default/a/.a.objs/byte/a.cmi
              |-- a.cmt -> ../../../../default/a/.a.objs/byte/a.cmt
              |-- a.cmx -> ../../../../default/a/.a.objs/native/a.cmx
              |-- a.cmxa -> ../../../../default/a/a.cmxa
              |-- a.cmxs -> ../../../../default/a/a.cmxs
              |-- a.ml -> ../../../../default/a/a.ml-gen
              |-- a__B.cmi -> ../../../../default/a/.a.objs/byte/a__B.cmi
              |-- a__B.cmt -> ../../../../default/a/.a.objs/byte/a__B.cmt
              |-- a__B.cmx -> ../../../../default/a/.a.objs/native/a__B.cmx
              |-- a__B__Bar.cmi -> ../../../../default/a/.a.objs/byte/a__B__Bar.cmi
              |-- a__B__Bar.cmt -> ../../../../default/a/.a.objs/byte/a__B__Bar.cmt
              |-- a__B__Bar.cmx -> ../../../../default/a/.a.objs/native/a__B__Bar.cmx
              |-- a__Foo.cmi -> ../../../../default/a/.a.objs/byte/a__Foo.cmi
              |-- a__Foo.cmt -> ../../../../default/a/.a.objs/byte/a__Foo.cmt
              |-- a__Foo.cmx -> ../../../../default/a/.a.objs/native/a__Foo.cmx
              |-- b
              |   |-- b.ml -> ../../../../../default/a/a__B.ml-gen
              |   `-- bar.ml -> ../../../../../default/a/b/bar.ml
              |-- dune-package -> ../../../../default/pkg.dune-package
              |-- foo.ml -> ../../../../default/a/foo.ml
              `-- melange
                  |-- a.cmi -> ../../../../../default/a/.a.objs/melange/a.cmi
                  |-- a.cmj -> ../../../../../default/a/.a.objs/melange/a.cmj
                  |-- a.cmt -> ../../../../../default/a/.a.objs/melange/a.cmt
                  |-- a.ml -> ../../../../../default/a/.melange_src/a.ml-gen
                  |-- a__B.cmi -> ../../../../../default/a/.a.objs/melange/a__B.cmi
                  |-- a__B.cmj -> ../../../../../default/a/.a.objs/melange/a__B.cmj
                  |-- a__B.cmt -> ../../../../../default/a/.a.objs/melange/a__B.cmt
                  |-- a__B__Bar.cmi -> ../../../../../default/a/.a.objs/melange/a__B__Bar.cmi
                  |-- a__B__Bar.cmj -> ../../../../../default/a/.a.objs/melange/a__B__Bar.cmj
                  |-- a__B__Bar.cmt -> ../../../../../default/a/.a.objs/melange/a__B__Bar.cmt
                  |-- a__Foo.cmi -> ../../../../../default/a/.a.objs/melange/a__Foo.cmi
                  |-- a__Foo.cmj -> ../../../../../default/a/.a.objs/melange/a__Foo.cmj
                  |-- a__Foo.cmt -> ../../../../../default/a/.a.objs/melange/a__Foo.cmt
                  |-- a__Melange_only.cmi -> ../../../../../default/a/.a.objs/melange/a__Melange_only.cmi
                  |-- a__Melange_only.cmj -> ../../../../../default/a/.a.objs/melange/a__Melange_only.cmj
                  |-- a__Melange_only.cmt -> ../../../../../default/a/.a.objs/melange/a__Melange_only.cmt
                  |-- b
                  |   |-- b.ml -> ../../../../../../default/a/.melange_src/a__B.ml-gen
                  |   `-- bar.ml -> ../../../../../../default/a/.melange_src/b/bar.ml
                  |-- foo.ml -> ../../../../../default/a/.melange_src/foo.ml
                  `-- melange_only.ml -> ../../../../../default/a/.melange_src/melange_only.ml
  
  7 directories, 42 files

