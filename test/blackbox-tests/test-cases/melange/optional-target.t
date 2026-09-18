Omitting the target should emit JavaScript beside the source's build-tree copy.

  $ make_melange_project 3.25 1.0
  $ cat >dune <<EOF
  > (melange.emit
  >  (emit_stdlib false))
  > EOF
  $ cat >main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

The target is currently required.

  $ dune build main.js
  File "dune", lines 1-2, characters 0-35:
  1 | (melange.emit
  2 |  (emit_stdlib false))
  Error: Field "target" is missing
  [1]
