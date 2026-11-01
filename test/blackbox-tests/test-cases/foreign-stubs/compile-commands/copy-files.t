Compilation database discovery must not load directory contents while
registering root rules. Doing so creates a cycle when copy_files needs those
root rules to discover its inputs (issue #16516).

The regression affects even workspaces without foreign code. Language 3.22
works because compilation database generation is disabled.

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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "file.txt")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]
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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "generated.txt")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]
  $ cat _build/default/foo/generated.txt
  cat: _build/default/foo/generated.txt: No such file or directory
  [1]
  $ test ! -f compile_commands.json

A foreign library with copied sources also cycles. Skipping non-foreign
stanzas during database discovery is not sufficient to fix this case.

  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > foo/dune <<EOF
  > (copy_files ../*.c)
  > (foreign_library (archive_name stubs) (language c))
  > EOF
  $ dune build foo/stub.c
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "*.c")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]
  $ dune build compile_commands.json && jq '[.[].file]' compile_commands.json
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "*.c")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]

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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "*.c")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]
  $ jq '[.[].file] | sort' compile_commands.json
  [
    "stub.c"
  ]
  $ test ! -f _build/default/generated.c
  $ dune build foo/generated.c @check
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "*.c")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]
  $ cat _build/default/foo/generated.c
  cat: _build/default/foo/generated.c: No such file or directory
  [1]

Adding a source must invalidate the deferred discovery as well.

  $ echo 'int another(void) { return 2; }' > another.c
  $ dune build compile_commands.json && \
  >   jq '[.[].file] | sort' compile_commands.json
  Error: Dependency cycle between:
     Computing directory contents of _build/default/foo
  -> { dir = In_build_dir "default"
     ; predicate = Element (Glob "*.c")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/foo
  [1]

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
  Error: Dependency cycle between:
     %{file-available:../stub.c} at foo/dune:5
  [1]

Unrelated generated files must not be inspected while collecting the database.
Here a report rule depends on the database and produces a directory target.
Enumerating that target for an unrelated copy_files stanza creates a cycle.

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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/unrelated
  -> { dir = In_build_dir "default/reports"
     ; predicate = Element (Glob "*.txt")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/unrelated
  [1]
  $ test ! -d _build/default/reports

The report and its copy should still build when explicitly requested.

  $ dune build unrelated/result.txt && cat _build/default/unrelated/result.txt
  Error: Dependency cycle between:
     %{file-available:../stub.c} at foo/dune:5
  -> required by { dir = In_build_dir "default/reports"
     ; predicate = Element (Glob "*.txt")
     ; only_generated_files = false
     }
  -> required by Computing directory contents of _build/default/unrelated
  [1]

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

An explicit database rule must take precedence over automatic generation,
even if there are foreign stanzas. Start with all of them disabled.

  $ mkdir ../owned && cd ../owned
  $ make_dune_project 3.23
  $ echo '[{"file":"external.c"}]' > external.json
  $ cat > dune <<EOF
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names missing)
  >  (enabled_if false))
  > (rule (action (copy external.json compile_commands.json)))
  > EOF
  $ dune build compile_commands.json @check && \
  >   cat _build/default/compile_commands.json
  [{"file":"external.c"}]
  $ test ! -f compile_commands.json

The explicit rule should also take precedence when foreign stanzas are enabled.

  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > dune <<EOF
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > (rule (action (copy external.json compile_commands.json)))
  > EOF
  $ dune build compile_commands.json && cat _build/default/compile_commands.json
  [{"file":"external.c"}]
  $ test ! -f compile_commands.json

Without an explicit rule, disabled foreign stanzas must not overwrite an
existing source-tree database or register it for deletion on clean.

  $ cat > dune <<EOF
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names missing)
  >  (enabled_if false))
  > EOF
  $ cp external.json compile_commands.json
  $ dune build @check
  $ cat compile_commands.json
  [{"file":"external.c"}]
  $ test ! -f _build/.to-delete-in-source-tree

Edits to that database must also update the build-tree copy.

  $ echo '[{"file":"updated.c"}]' > compile_commands.json
  $ dune build compile_commands.json
  $ cat _build/default/compile_commands.json
  [{"file":"updated.c"}]
  $ cat compile_commands.json
  [{"file":"updated.c"}]

Enabling a foreign stanza resumes automatic database generation, even though
there is already a database in the source tree.

  $ cat > dune <<EOF
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > EOF
  $ dune build compile_commands.json
  $ jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]

Disabling it again must leave the existing database alone, including when the
condition is not a literal false.

  $ cat > dune <<'EOF'
  > (foreign_library
  >  (archive_name stubs)
  >  (language c)
  >  (names stub)
  >  (enabled_if (= %{context_name} unused)))
  > EOF
  $ dune build @check
  $ jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]

A copy_files rule can own the database too. Its wildcard declaration must be
resolved before deciding whether to generate the built-in database.

  $ mkdir ../copy-owned && cd ../copy-owned
  $ make_dune_project 3.23
  $ mkdir inputs
  $ echo '[{"file":"external.c"}]' > inputs/compile_commands.json
  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > dune <<EOF
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > (copy_files inputs/*.json)
  > EOF
  $ dune build compile_commands.json @check
  $ cat _build/default/compile_commands.json
  [{"file":"external.c"}]
  $ test ! -f compile_commands.json

Conversely, a wildcard that can match the database name does not own it unless
it actually produces that file. The built-in database must then be generated.

  $ mv inputs/compile_commands.json inputs/other.json
  $ dune build compile_commands.json other.json @check
  $ jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]
  $ cat _build/default/other.json
  [{"file":"external.c"}]

A disabled rule declaring the database does not own it either.

  $ mkdir ../disabled-owner && cd ../disabled-owner
  $ make_dune_project 3.23
  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > dune <<EOF
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > (rule
  >  (target compile_commands.json)
  >  (enabled_if false)
  >  (action (write-file compile_commands.json disabled)))
  > EOF
  $ dune build compile_commands.json @check
  $ jq '[.[].file]' compile_commands.json
  [
    "stub.c"
  ]

Directory targets also take precedence over the built-in database.

  $ mkdir ../directory-owned && cd ../directory-owned
  $ make_dune_project 3.23
  $ echo '(using directory-targets 0.1)' >> dune-project
  $ echo 'int stub(void) { return 0; }' > stub.c
  $ cat > dune <<EOF
  > (foreign_library (archive_name stubs) (language c) (names stub))
  > (rule
  >  (target (dir compile_commands.json))
  >  (action
  >   (system "mkdir compile_commands.json && echo owned > compile_commands.json/marker")))
  > EOF
  $ dune build compile_commands.json @check
  $ cat _build/default/compile_commands.json/marker
  owned
  $ test ! -e compile_commands.json
