Reproduction case for #2499: dune doesn't cleanup stale directories

  $ make_dune_project 2.0

  $ cat >dune <<EOF
  > (data_only_dirs data)
  > (rule
  >  (deps (source_tree data))
  >  (action (with-stdout-to list (system "find data -type f | sort"))))
  > EOF

  $ mkdir -p data/a data/b; touch data/a/x data/b/x

  $ dune build list
  $ cat _build/default/list
  data/a/x
  data/b/x

  $ rm -rf data/b

  $ dune build list
  $ cat _build/default/list
  data/a/x

For source-copy ownership, a contents-only edit leaves the directory's ownership
unchanged. Undeclared entries survive until a source name is removed or a fresh
process loads the directory. This does not require reuse for stanza producers
whose dependencies cannot be proved unchanged without evaluation.

  $ mkdir watch-cleanup
  $ cd watch-cleanup
  $ make_dune_project 3.22
  $ echo one >input
  $ echo owned >old
  $ start_dune
  $ build input old
  Success

  $ echo stale >_build/default/stale
  $ ln -s input _build/default/stale-link
  $ mkdir _build/default/stale-dir
  $ touch _build/default/stale-dir/child
  $ echo two >input
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build input
  Success
  $ cat _build/default/input
  two
  $ test -f _build/default/old
  $ test -f _build/default/stale
  $ test -L _build/default/stale-link
  $ test -f _build/default/stale-dir/child

Removing a source name changes ownership and removes both its old copy and the
undeclared entries, without losing the source copy that is still owned.

  $ rm old
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build input
  Success
  $ test ! -e _build/default/old
  $ test ! -e _build/default/stale
  $ test ! -L _build/default/stale-link
  $ test ! -e _build/default/stale-dir
  $ cat _build/default/input
  two
  $ stop_dune_quiet

  $ echo recreated >_build/default/stale
  $ dune build input
  $ test ! -e _build/default/stale
  $ cd ..

Static rule ownership, including library and Merlin rules, is unchanged when only
an action input's contents change. Removing a rule still reloads ownership and
cleans stale entries.

  $ mkdir watch-stanza-cleanup
  $ cd watch-stanza-cleanup
  $ make_dune_project 3.22
  $ cat >dune <<'EOF'
  > (library (name helper) (modes byte))
  > (rule (target keep) (action (copy input %{target})))
  > (rule (target old) (action (write-file %{target} owned)))
  > EOF
  $ echo one >input
  $ echo 'let x = 1' >helper.ml
  $ dune build @all
  $ start_dune
  $ build '(alias_rec all)'
  Success
  $ echo stale >_build/default/stale
  $ ln -s keep _build/default/stale-link
  $ mkdir _build/default/stale-dir
  $ touch _build/default/stale-dir/child
  $ echo two >input
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build '(alias_rec all)'
  Success
  $ cat _build/default/keep
  two
  $ test -f _build/default/old
  $ test -f _build/default/stale
  $ test -L _build/default/stale-link
  $ test -f _build/default/stale-dir/child

  $ cat >dune <<'EOF'
  > (library (name helper) (modes byte))
  > (rule (target keep) (action (copy input %{target})))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build keep
  Success
  $ test ! -e _build/default/old
  $ test ! -e _build/default/stale
  $ test ! -L _build/default/stale-link
  $ test ! -e _build/default/stale-dir
  $ cat _build/default/keep
  two
  $ stop_dune_quiet
  $ cd ..
