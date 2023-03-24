Preprocess with an action chain and PPX

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (preprocess
  >   (action
  >    (progn
  >     (run cat %{input-file})
  >     (echo "let () = print_endline \"one more line\"")))
  >   (pps some_ppx)
  >   (action
  >    (progn
  >     (run cat %{input-file})
  >     (echo "let () = print_endline \"last line\"")))))
  > EOF
  $ dune exec ./foo.exe
  File "dune", line 9, characters 2-16:
  9 |   (pps some_ppx)
        ^^^^^^^^^^^^^^
  Error: Action chains only allow one final `pps'
  Hint: Move the `pps' specification to the end of the action chain
  [1]

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo)
  >  (preprocess
  >   no_preprocessing
  >   (pps some_ppx)))
  > EOF

  $ dune exec ./foo.exe
  File "dune", line 5, characters 2-35:
  5 |   no_preprocessing
  6 |   (pps some_ppx)))
  Error: `no_preprocessing' doesn't make sense in the presence of other
  preprocessor specifications.
  [1]
