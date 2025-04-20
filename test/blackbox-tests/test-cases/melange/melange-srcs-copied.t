
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package  (name pkg))
  > (using melange 0.1)
  > EOF

  $ mkdir a a/b a/b/c

  $ cat > a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name a)
  >  (modes melange) ; byte
  >  (package pkg))
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
