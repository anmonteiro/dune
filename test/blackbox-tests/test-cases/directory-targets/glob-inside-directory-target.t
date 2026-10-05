This test makes sure that whenever we evaluate a glob inside a directory target
that matches nothing, we still copy the directory and make it empty.

  $ make_directory_targets_project 3.0

  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (action (system "mkdir output && touch output/foo.txt")))
  > (rule
  >  (targets x)
  >  (deps (glob_files output/*.baz))
  >  (action (system "ls output/ && touch x")))
  > EOF

  $ DUNE_SANDBOX=copy dune build x

Direct directory requests build and validate their dependencies too.

  $ echo before > input
  $ cat > dune <<EOF
  > (rule
  >  (targets (dir output))
  >  (deps input)
  >  (action (system "mkdir output && cp input output/value")))
  > EOF
  $ dune build output
  $ cat _build/default/output/value
  before
  $ echo after > input
  $ dune build output output
  $ cat _build/default/output/value
  after
