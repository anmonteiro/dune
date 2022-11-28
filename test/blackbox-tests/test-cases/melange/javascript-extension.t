Test (javascript_extension) field on melange.emit stanzas

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

  $ mkdir output
  $ cat > output/hello.ml <<EOF
  > let () =
  >   print_endline "hello"
  > EOF

Can use extension with dots

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (module_system commonjs)
  >  (javascript_extension bs.js))
  > EOF

  $ output=output/output
  $ dune build $output/hello.bs.js
  $ node _build/default/$output/hello.bs.js
  hello

Errors out if extension starts with dot

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (module_system commonjs)
  >  (javascript_extension .bs.js))
  > EOF

  $ dune build $output/hello.js
  File "output/dune", line 3, characters 23-29:
  3 |  (javascript_extension .bs.js))
                             ^^^^^^
  Error: extension must not start with '.'
  [1]
