Entry points should not allow mli only modules as entry points.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat >dune <<EOF
  > (melange.emit
  >  (module_system commonjs)
  >  (modules_without_implementation foo)
  >  (alias melange))
  > EOF

  $ touch foo.mli bar.ml
  $ dune build @melange
  $ ls _build/default/*.js | sort
  _build/default/bar.js
