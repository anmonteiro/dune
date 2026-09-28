Copy files from inside a directory target

  $ make_directory_targets_project 3.0

Copy from a generated sub-directory
-----------------------------------

Directory materialization does not need to enumerate the file rules that
depend on its contents.

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/x foo/y foo/z")))
  > (copy_files foo/*)
  > EOF

  $ dune build
  $ test -f _build/default/x
  $ test -f _build/default/y
  $ test -f _build/default/z

Copy from a generated directory somewhere else
----------------------------------------------

  $ rm -f dune
  $ mkdir a b
  $ cat >a/dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/x foo/y foo/z")))
  > EOF

  $ cat >b/dune <<EOF
  > (copy_files ../a/foo/*)
  > EOF

  $ dune build b

  $ ls _build/default/b
  x
  y
  z

Deferred file rules must still be checked for conflicts with directory targets
after their names are discovered.

  $ mkdir conflict
  $ cat >conflict/dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/foo")))
  > (copy_files foo/*)
  > EOF
  $ dune build conflict/foo
  Error: Multiple rules generated for _build/default/conflict/foo:
  - conflict/dune:5
  - conflict/dune:1
  [1]
