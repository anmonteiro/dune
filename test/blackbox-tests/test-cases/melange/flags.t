Test flags and compile_flags fields on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

Using flags field in melange.emit stanzas is not supported

  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs)
  >  (flags -w -14-26))
  > EOF

The code in main contains unused var (warning 26) and illegal backlash (warning 14)

  $ cat > output/main.ml <<EOF
  > let t = "\e\n" in
  > print_endline "hello"
  > EOF

Building should not fail as warnings are silenced

  $ dune build output/main.js
  File "output/dune", line 4, characters 2-7:
  4 |  (flags -w -14-26))
        ^^^^^
  Error: Unknown field flags
  [1]

Should use compile_flags

  $ cat > output/dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs)
  >  (compile_flags -w -14-26))
  > EOF

  $ output=output/output
  $ dune build $output/main.js
  $ node _build/default/$output/main.js
  hello
