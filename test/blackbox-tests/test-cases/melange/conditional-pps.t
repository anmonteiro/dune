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
  Entering directory 'a'
  Leaving directory 'a'

