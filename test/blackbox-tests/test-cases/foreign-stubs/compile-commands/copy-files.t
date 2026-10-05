Compilation database discovery must not load directory contents while
registering root rules. Doing so creates a cycle when copy_files needs those
root rules to discover its inputs (issue #16516).

Source copies work both before and after compilation database generation was
introduced in language 3.23, including workspaces without foreign code.

  $ make_dune_project 3.22
  $ echo hello > file.txt
  $ mkdir foo
  $ echo '(copy_files ../file.txt)' > foo/dune
  $ dune build foo/file.txt @check
  $ cat _build/default/foo/file.txt
  hello
  $ test ! -f compile_commands.json

  $ make_dune_project 3.23
  $ dune build foo/file.txt @check
  $ test ! -f compile_commands.json

Selecting only source files also works. No compilation database should be
produced without foreign stanzas.

  $ echo '(copy_files (only_sources true) (files ../file.txt))' > foo/dune
  $ dune build foo/file.txt @check
  $ cat _build/default/foo/file.txt
  hello
  $ test ! -f compile_commands.json

Generated inputs must also work, without relying on the only_sources
workaround or prebuilding the input.

  $ cat > dune <<EOF
  > (rule (action (write-file generated.txt generated)))
  > EOF
  $ echo '(copy_files ../generated.txt)' > foo/dune
  $ dune build foo/generated.txt @check
  $ cat _build/default/foo/generated.txt
  generated
  $ test ! -f compile_commands.json

A foreign library with copied sources must also work. Skipping non-foreign
stanzas during database discovery would not be sufficient for this case.

  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > foo/dune <<EOF
  > (copy_files ../*.c)
  > (foreign_library (archive_name stubs) (language c))
  > EOF
  $ dune build foo/stub.c
  $ dune build compile_commands.json && jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]

The database can also be built through @check with only_sources.

  $ cat > foo/dune <<EOF
  > (copy_files (only_sources true) (files ../*.c))
  > (foreign_library (archive_name stubs) (language c))
  > EOF
  $ dune build @check
  $ jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]

Generated root sources must also appear in the database. Restricting database
source discovery to the source tree would incorrectly omit generated.c.

  $ cat > dune <<EOF
  > (rule
  >  (action (write-file generated.c "int generated(void) { return 1; }")))
  > EOF
  $ cat > foo/dune <<EOF
  > (copy_files ../*.c)
  > (foreign_library (archive_name stubs) (language c))
  > EOF
  $ test ! -f _build/default/generated.c
  $ dune build compile_commands.json
  $ jq '[.[].file] | sort' compile_commands.json
  [
    "generated.c",
    "stub.c"
  ]
  $ test ! -f _build/default/generated.c
  $ dune build foo/generated.c @check
  $ cat _build/default/foo/generated.c
  int generated(void) { return 1; }

Adding a source must invalidate the deferred discovery as well.

  $ echo 'int another(void) { return 2; }' > another.c
  $ dune build compile_commands.json && \
  >   jq '[.[].file] | sort' compile_commands.json
  [
    "another.c",
    "generated.c",
    "stub.c"
  ]

Evaluating a foreign stanza's enabled_if must not create a cycle when it
queries root build files, even with only_sources.

  $ cat > foo/dune <<'EOF'
  > (copy_files (only_sources true) (files ../stub.c))
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (enabled_if %{file-available:../stub.c}))
  > EOF
  $ dune build compile_commands.json && jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]

Unrelated generated files must not be inspected while collecting the database.
Here a report rule depends on the database and produces a directory target.
Enumerating that target for an unrelated copy_files stanza would create a
cycle, so its discovery must stay deferred.

  $ echo '(using directory-targets 0.1)' >> dune-project
  $ cat >> dune <<EOF
  > (rule
  >  (target (dir reports))
  >  (deps compile_commands.json)
  >  (action (system "mkdir reports && echo report > reports/result.txt")))
  > EOF
  $ mkdir unrelated
  $ echo '(copy_files ../reports/*.txt)' > unrelated/dune
  $ dune build compile_commands.json
  $ test ! -d _build/default/reports

The report and its copy should still build when explicitly requested.

  $ dune build unrelated/result.txt && cat _build/default/unrelated/result.txt
  report

A workspace containing only disabled foreign stanzas currently has no
compilation database target.

  $ mkdir disabled && cd disabled
  $ make_dune_project 3.23
  $ cat > dune <<EOF
  > (foreign_library
  >  (archive_name disabled)
  >  (language c)
  >  (names missing)
  >  (enabled_if false))
  > EOF
  $ dune build @check
  $ dune build compile_commands.json
  Error: Don't know how to build compile_commands.json
  [1]
  $ jq '.' compile_commands.json
  jq: error: Could not open file compile_commands.json: No such file or directory
  [2]
