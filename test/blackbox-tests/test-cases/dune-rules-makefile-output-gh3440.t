This test just makes sure that dune rules doesn't error out. In the past, we've
had bugs where this subcommand would stop working and nobody would notice as
it's not used very much.

  $ make_dune_project 2.5
  $ dune rules --root . --format=json -o Makefile

Reflection keeps file dependencies and deduplicates requested rules, whether
selected directly or through aliases, without executing their actions.

  $ cat >dune <<EOF
  > (rule
  >  (target producer)
  >  (action (write-file producer "not executed")))
  > (rule
  >  (target requested)
  >  (deps producer)
  >  (action (write-file requested "not executed")))
  > (alias
  >  (name inspect)
  >  (deps requested))
  > EOF

  $ dune rules --deps requested requested
  ((File (In_build_dir _build/default/producer)))

  $ dune rules --deps @inspect @@inspect
  ((File (In_build_dir _build/default/producer)))

  $ dune rules --deps requested @inspect @@inspect requested
  ((File (In_build_dir _build/default/producer)))

  $ dune rules --deps producer
  ()

  $ test ! -e _build/default/producer
  $ test ! -e _build/default/requested
