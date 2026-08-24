A custom toplevel name must be a filename.

  $ make_dune_project 3.21

  $ cat >dune <<EOF
  > (toplevel
  >  (name ../foo))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-13:
  2 |  (name ../foo))
             ^^^^^^
  Error: name must be a valid filename
  [1]

  $ cat >dune <<EOF
  > (toplevel
  >  (name ""))
  > EOF

  $ dune build
  File "dune", line 2, characters 7-9:
  2 |  (name ""))
             ^^
  Error: name must be a valid filename
  [1]
