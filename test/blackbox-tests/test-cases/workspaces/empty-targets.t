An explicitly empty workspace target list is rejected.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > EOF

  $ cat >dune-workspace <<EOF
  > (lang dune 3.8)
  > (context (default (targets)))
  > EOF

  $ dune build
  File "dune-workspace", line 2, characters 18-27:
  2 | (context (default (targets)))
                        ^^^^^^^^^
  Error: No build target defined
  [1]
