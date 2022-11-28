Test (preprocess) field on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs)
  >  (preprocess
  >   (action
  >    (run cat %{input-file}))))
  > EOF

  $ cat > output/main.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

  $ output=output/output
  $ dune build $output/main.js
  $ node _build/default/$output/main.js
  hello
