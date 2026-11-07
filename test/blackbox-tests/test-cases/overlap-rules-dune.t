We demonstrate that users are allowed to generate rules in "private" dune
directories. That should be forbidden or dune should generate this stuff
elsewhere.

  $ make_dune_project 3.1

  $ cat >dune <<EOF
  > (dirs :standard .foo.eobjs)
  > (subdir .foo.eobjs
  >  (rule (with-stdout-to foo (echo "foo"))))
  > EOF

  $ target=".foo.eobjs/foo"
  $ dune build $target
  $ cat _build/default/$target
  foo

Compilation rules from the parent are still available in that directory.

  $ cat >>dune <<EOF
  > (executable
  >  (name foo))
  > EOF

  $ cat >foo.ml <<EOF
  > print_endline "42";;
  > EOF

  $ dune build ./foo.exe
