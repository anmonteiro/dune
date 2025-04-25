
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package  (name pkg) (allow_empty))
  > (using melange 0.1)
  > EOF

  $ mkdir a

  $ cat > a/dune <<EOF
  > (library
  >  (name a)
  >  (package pkg)
  >  (modes melange byte))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "ocaml"
  > EOF
  $ cat > a/foo.melange.ml <<EOF
  > let x = "melange"
  > EOF

  $ dune build a/.melange_src/foo.ml
  $ cat _build/default/a/.melange_src/foo.ml
  let x = "melange"


$ dune build _build/default/a/.a.objs/melange/a.cmi
$ tree -a _build/default/a

$ dune build
$ tree -a _build/default/a


