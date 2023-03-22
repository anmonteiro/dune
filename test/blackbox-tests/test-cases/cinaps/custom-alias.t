Custom alias for the cinaps

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using cinaps 1.2)
  > EOF

  $ cat > dune <<EOF
  > (cinaps
  >  (files foo.ml)
  >  (alias foo))
  > EOF

  $ touch foo.ml

  $ dune build @foo --display short
        cinaps .cinaps.e337c74a/cinaps.ml-gen
        ocamlc .cinaps.e337c74a/.cinaps.eobjs/byte/dune__exe__Cinaps.{cmi,cmo,cmt}
      ocamlopt .cinaps.e337c74a/.cinaps.eobjs/native/dune__exe__Cinaps.{cmx,o}
      ocamlopt .cinaps.e337c74a/cinaps.exe
        cinaps alias foo
