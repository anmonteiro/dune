
  $ cat >dune-project <<EOF
  > (lang dune 3.18)
  > (package  (name pkg))
  > (using melange 0.1)
  > EOF

  $ mkdir a a/b

  $ cat > a/dune <<EOF
  > (include_subdirs unqualified)
  > (library
  >  (name a)
  >  (modes byte melange)
  >  (package pkg))
  > EOF

  $ cat > a/foo.ml <<EOF
  > let x = "foo"
  > EOF
  $ cat > a/b/bar.ml <<EOF
  > let x = "bar"
  > EOF

  $ dune build

  $ tree -a _build/default/a
