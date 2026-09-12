A plugin must load at least one library.

  $ cat >dune-project <<EOF
  > (lang dune 3.8)
  > (using dune_site 0.1)
  > (package
  >  (name foo)
  >  (sites (lib plugins)))
  > EOF

  $ cat >dune <<EOF
  > (plugin
  >  (name empty)
  >  (libraries)
  >  (site (foo plugins)))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-12:
  3 |  (libraries)
       ^^^^^^^^^^^
  Error: No plugin library defined
  [1]
