Build a binary using a %{bin:..} form.

  $ cat >dune-project <<EOF
  > (lang dune 3.7)
  > (package
  >  (name randompkg))
  > EOF

  $ mkdir bin
  $ touch bin/bar.ml
  $ cat >bin/dune <<EOF
  > (executable
  >  (public_name bar))
  > EOF

  $ dune build '%{bin:bar}'
  $ ls _build/default/bin/bar.exe
  _build/default/bin/bar.exe

Quoted text and escaped percent forms are literal target names.

  $ cat >dune <<'EOF'
  > (rule
  >  (target "space name")
  >  (action (write-file %{target} "space\n")))
  > (rule
  >  (target "literal\%{unknown}")
  >  (action (write-file %{target} "escaped\n")))
  > EOF

  $ dune build '(file "space name")'
  $ cat '_build/default/space name'
  space
  $ dune build '(file "literal\%{unknown}")'
  $ cat '_build/default/literal%{unknown}'
  escaped

Actual percent forms still expand and report a missing binary.

  $ dune build '%{bin:literal_target_missing_bin}'
  File "command line", line 1, characters 0-33:
  Error: Program literal_target_missing_bin not found in the tree or in PATH
   (context: default)
  [1]
