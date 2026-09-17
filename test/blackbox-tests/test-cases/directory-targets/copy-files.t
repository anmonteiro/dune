Copy files from inside a directory target

  $ make_directory_targets_project 3.0

Copy from a generated sub-directory
-----------------------------------

Copying from a generated sub-directory still causes a cycle: `copy_files`
needs complete directory enumeration before the source stage has finished
collecting the rule that produces that directory.

  $ cat >dune <<EOF
  > (rule
  >  (target (dir foo))
  >  (deps (sandbox always))
  >  (action (system "mkdir foo && touch foo/x foo/y foo/z")))
  > (copy_files foo/*)
  > EOF

  $ dune build
  Error: Dependency cycle between:
     Computing directory contents of _build/default
  -> { dir = In_build_dir "default/foo"
     ; predicate = Element (Glob "*")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default
  [1]

  $ test ! -e _build/default/foo

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
