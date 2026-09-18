Test generated inputs in the enabled_if field of libraries.

A library can read a file produced by an independent rule in the same directory.

  $ make_dune_project 3.25

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if %{read:foo}))
  > (rule (with-stdout-to foo (echo true)))
  > EOF

  $ dune build

Reading a file whose rule depends on the library still creates a dependency cycle.

  $ cat > dune << EOF
  > (library
  >  (name foo)
  >  (enabled_if %{read:foo}))
  > (rule
  >  (deps foo.cma)
  >  (action (with-stdout-to foo (echo true))))
  > EOF

  $ dune build
  Error: Dependency cycle between:
     library "foo" in _build/default
  -> _build/default/foo
  -> %{read:foo} at dune:3
  -> library "foo" in _build/default
  -> required by alias default
  [1]
