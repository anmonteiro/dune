This test just makes sure that dune rules doesn't error out. In the past, we've
had bugs where this subcommand would stop working and nobody would notice as
it's not used very much.

  $ make_dune_project 2.5
  $ dune rules --root . --format=json -o Makefile

Render paths relative to nested working directories, including extension payloads.
Missing programs can still be printed without executing the action.

  $ make_dune_project 2.6
  $ mkdir sub
  $ echo contents > input.ml
  $ cat > dune <<'EOF'
  > (env (_ (binaries (input.ml as link.ml))))
  > (rule
  >  (target printed)
  >  (action
  >   (no-infer
  >    (chdir sub
  >     (progn
  >      (copy# ../input.ml ../printed)
  >      (run missing-for-rules ../input.ml))))))
  > EOF
  $ dune rules printed --format=json |
  >   jq '.[0].action | .[2][2][1][1] |= contains("../input.ml ../printed")'
  [
    "chdir",
    "_build/default",
    [
      "chdir",
      "sub",
      [
        "progn",
        [
          "ext",
          true
        ],
        [
          "run",
          "missing-for-rules",
          "../input.ml"
        ]
      ]
    ]
  ]

Symlink sources are relative to the destination's parent, not the working
directory.

  $ dune rules .bin/link.ml --format=json | jq '.[0].action'
  [
    "chdir",
    "_build/default",
    [
      "symlink",
      "../input.ml",
      ".bin/link.ml"
    ]
  ]

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
