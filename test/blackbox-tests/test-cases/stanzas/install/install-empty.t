An install stanza must contain at least one entry.

  $ make_dune_project 3.11
  $ touch foo.opam

  $ cat >dune <<EOF
  > (install
  >  (section share)
  >  (files)
  >  (dirs)
  >  (source_trees)
  >  (package foo))
  > EOF

  $ dune build
  File "dune", lines 1-6, characters 0-74:
  1 | (install
  2 |  (section share)
  3 |  (files)
  4 |  (dirs)
  5 |  (source_trees)
  6 |  (package foo))
  Error: At least one of dirs, files, or source_trees must be non-empty
  [1]
