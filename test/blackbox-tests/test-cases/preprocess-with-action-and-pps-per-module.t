Preprocess with an action chain and PPX

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > EOF
  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo bar)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (progn
  >       (run cat %{input-file})
  >       (echo "let () = print_endline \"one more line\"")))
  >     (action
  >      (progn
  >       (run cat %{input-file})
  >       (echo "let () = print_endline \"last line\"")))
  >     foo))))
  > EOF
  $ cat > bar.ml <<EOF
  > let x () = print_endline "no text after pp:"
  > EOF
  $ cat > foo.ml <<EOF
  > let () = Bar.x ()
  > let () = print_endline "text after pp:"
  > EOF

  $ dune exec ./foo.exe
  no text after pp:
  text after pp:
  one more line
  last line

Show an error, (pps .. ) cannot come after modules

  $ cat > dune <<EOF
  > (executable
  >  (name foo)
  >  (modules foo bar)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (progn
  >       (run cat %{input-file})
  >       (echo "let () = print_endline \"one more line\"")))
  >     foo
  >     (action (run cat %{input-file}))))))
  > EOF

  $ dune exec ./foo.exe
  File "dune", line 11, characters 4-36:
  11 |     (action (run cat %{input-file}))))))
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Expected only module names after preprocessor specification.
  Hint: Move this preprocessing specification before the list of module names.
  [1]

