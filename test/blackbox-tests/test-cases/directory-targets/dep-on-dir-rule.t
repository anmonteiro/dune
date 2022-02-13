Depend on a directory target.

  $ cat >dune-project <<EOF
  > (lang dune 3.0)
  > (using directory-targets 0.1)
  > EOF

  $ cat >dune <<EOF
  > (rule
  >  (deps (sandbox always))
  >  (target (dir output))
  >  (action (bash "mkdir %{target} && touch %{target}/example.txt")))
  > 
  > (rule
  >  (deps output/*)
  >  (target bar)
  >  (action (bash "ls -f %{deps} > %{target}")))
  > EOF

  $ dune build ./bar
  $  cat _build/default/bar
  .
  ..
  example.txt
