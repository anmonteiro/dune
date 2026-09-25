Nesting of dynamic_include stanzas

  $ mkdir a b c

  $ make_dune_project 3.14

Configuration files for every context must be available before an action used
to generate a dynamic include, without explicit configuration dependencies.

  $ cat >dune-workspace <<EOF
  > (lang dune 3.14)
  > (context (default))
  > (context (default (name other)))
  > EOF

  $ cat >a/dune <<EOF
  > (dynamic_include ../b/dune.inc)
  > EOF

  $ cat >b/dune <<'EOF'
  > (rule
  >  (progn
  >   (bash "test -s \"$INSIDE_DUNE/.dune/configurator\"")
  >   (bash "test -s \"$INSIDE_DUNE/.dune/configurator.v2\"")
  >   (bash "test -s \"$INSIDE_DUNE/../other/.dune/configurator\"")
  >   (bash "test -s \"$INSIDE_DUNE/../other/.dune/configurator.v2\"")
  >   (with-stdout-to dune.inc
  >    (echo "(dynamic_include ../c/dune.inc)"))))
  > EOF

  $ cat >c/dune <<'EOF'
  > (rule
  >  (progn
  >   (bash "test -s \"$INSIDE_DUNE/.dune/configurator\"")
  >   (bash "test -s \"$INSIDE_DUNE/.dune/configurator.v2\"")
  >   (with-stdout-to dune.inc
  >    (echo "(rule (with-stdout-to foo (echo bar)))"))))
  > EOF

Only the default context's user target is requested.

  $ dune build _build/default/a/foo
  $ test ! -e _build/other/a/foo
