Test flags and compile_flags fields on melange.emit stanza

  $ cat > dune-project <<EOF
  > (lang dune 3.6)
  > (using melange 0.1)
  > EOF

Using flags field in melange.emit stanzas is not supported

  $ cat > dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs)
  >  (flags -w -14-26))
  > EOF

  $ dune build main.js
  File "dune", line 4, characters 2-7:
  4 |  (flags -w -14-26))
        ^^^^^
  Error: Unknown field flags
  [1]

Adds a module that contains unused var (warning 26) and illegal backlash (warning 14)

  $ cat > main.ml <<EOF
  > let t = "\e\n" in
  > print_endline "hello"
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs))
  > EOF

Trying to build triggers both warnings

  $ dune build main.js
  File "main.ml", line 1, characters 9-11:
  1 | let t = "\e\n" in
               ^^
  Error (warning 14 [illegal-backslash]): illegal backslash escape in string.
  File "main.ml", line 1, characters 4-5:
  1 | let t = "\e\n" in
          ^
  Error (warning 26 [unused-var]): unused variable t.
  [1]

Let's ignore them using compile_flags

  $ cat > dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs)
  >  (compile_flags -w -14-26))
  > EOF

  $ dune build main.js
  $ node _build/default/main.js
  hello

Can also pass flags from the env stanza. Let's go back to failing state:

  $ cat > dune <<EOF
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs))
  > EOF

  $ dune build main.js
  File "main.ml", line 1, characters 9-11:
  1 | let t = "\e\n" in
               ^^
  Error (warning 14 [illegal-backslash]): illegal backslash escape in string.
  File "main.ml", line 1, characters 4-5:
  1 | let t = "\e\n" in
          ^
  Error (warning 26 [unused-var]): unused variable t.
  [1]

Adding env stanza with both warnings silenced allows the build to pass successfully

  $ cat > dune <<EOF
  > (env
  >  (_
  >   (melange.compile_flags -w -14-26)))
  > (melange.emit
  >  (entries main)
  >  (module_system commonjs))
  > EOF

  $ dune build main.js
  $ node _build/default/main.js
  hello
