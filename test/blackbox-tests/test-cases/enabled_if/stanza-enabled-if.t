The `(enabled_if ...)` stanza can guard nested stanzas.

It should work for `(include ...)` and avoid evaluating disabled branches.

  $ cat > dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > dune <<EOF
  > (enabled_if
  >  (< %{ocaml_version} "0.0.0")
  >  (include disabled.inc))
  > (enabled_if
  >  (>= %{ocaml_version} "0.0.0")
  >  (include enabled.inc))
  > (rule
  >  (alias runtest)
  >  (deps selected.txt)
  >  (action (cat %{deps})))
  > (enabled_if true (library (name hello)))
  > EOF

  $ cat > disabled.inc <<EOF
  > (this_stanza_does_not_exist)
  > EOF

  $ cat > enabled.inc <<EOF
  > (rule
  >  (targets selected.txt)
  >  (action (with-stdout-to %{targets} (echo enabled))))
  > EOF

  $ dune runtest
  enabled

`(enabled_if ...)` supports multiple nested stanzas in one block.

  $ mkdir multi-stanzas
  $ cat > multi-stanzas/dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > multi-stanzas/dune <<EOF
  > (enabled_if
  >  false
  >  (this_stanza_does_not_exist)
  >  (neither_does_this))
  > (enabled_if
  >  true
  >  (rule
  >   (targets a.txt)
  >   (action (with-stdout-to %{targets} (echo a))))
  >  (rule
  >   (targets b.txt)
  >   (action (with-stdout-to %{targets} (echo b)))))
  > (rule
  >  (alias runtest)
  >  (deps a.txt b.txt)
  >  (action (cat %{deps})))
  > EOF

  $ (cd multi-stanzas && dune runtest)
  ab

`(enabled_if ...)` can use late-expansion forms such as `%{lib-available:...}`.

  $ mkdir lib-available
  $ cat > lib-available/dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > lib-available/dune <<EOF
  > (library (name foo))
  > (enabled_if
  >  %{lib-available:foo}
  >  (rule
  >   (targets from-lib.txt)
  >   (action (with-stdout-to %{targets} (echo ok)))))
  > (rule
  >  (alias runtest)
  >  (deps from-lib.txt)
  >  (action (cat %{deps})))
  > EOF

  $ (cd lib-available && dune runtest)
  ok

Wrapped library stanzas are visible to dependent stanzas when enabled.

  $ mkdir wrapped-library
  $ cat > wrapped-library/dune-project <<EOF
  > (lang dune 3.22)
  > EOF

  $ cat > wrapped-library/dune <<EOF
  > (enabled_if true
  >  (library
  >   (name hello)
  >   (modules hello)))
  > (executable
  >  (name main)
  >  (modules main)
  >  (libraries hello))
  > (rule
  >  (alias runtest)
  >  (action (run ./main.exe)))
  > EOF

  $ cat > wrapped-library/hello.ml <<EOF
  > let message = "ok"
  > EOF

  $ cat > wrapped-library/main.ml <<EOF
  > let () = print_endline Hello.message
  > EOF

  $ (cd wrapped-library && dune runtest)
  ok
