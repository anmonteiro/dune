Source copies must track source contents and changes to directory visibility
and target ownership during watch builds.

  $ make_directory_targets_project 3.23
  $ echo original > input
  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > EOF
  $ start_dune
  $ build output fallback
  Success
  $ cat _build/default/output _build/default/fallback
  original
  generated

Adding and removing another source does not change the copied contents.

  $ echo unrelated > other
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build output other
  Success
  $ cat _build/default/output
  original
  $ rm other
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build output
  Success
  $ test ! -e _build/default/other

Changes to an existing source still rebuild dependent targets.

  $ echo changed > input
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build output
  Success
  $ cat _build/default/output
  changed

Removing the source cleans its build-tree copy. Re-adding the same contents
must recreate that copy.

  $ rm input
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build fallback
  Success
  $ test ! -e _build/default/input
  $ echo changed > input
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build output
  Success
  $ cat _build/default/input _build/default/output
  changed
  changed

A source copy must not hide a fallback rule after its source is removed.

  $ echo source > fallback
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build fallback
  Success
  $ cat _build/default/fallback
  source
  $ rm fallback
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build fallback
  Success
  $ cat _build/default/fallback
  generated
  $ echo restored > fallback
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build fallback
  Success
  $ cat _build/default/fallback
  restored

Switching a target from a source copy to a promoted rule and back must restore
the source copy. Promotion into another directory leaves the original source
unchanged, so its digest alone cannot reveal that the build-tree copy changed.

  $ mkdir promoted
  $ echo original > switched
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build switched
  Success
  $ cat switched _build/default/switched
  original
  original

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > (rule
  >  (target switched)
  >  (mode (promote (into promoted)))
  >  (action (write-file %{target} "generated\n")))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build switched
  Success
  $ cat switched _build/default/switched promoted/switched
  original
  generated
  generated

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build switched
  Success
  $ cat switched _build/default/switched promoted/switched
  original
  original
  generated

A directory target can replace a source copy without loading the source
directory itself. Removing that rule must restore copies inside the directory.

  $ mkdir sub
  $ echo original > sub/value
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build sub/value
  Success
  $ cat sub/value _build/default/sub/value
  original
  original

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > (rule
  >  (target (dir sub))
  >  (mode (promote (into promoted)))
  >  (deps (sandbox always))
  >  (action (bash "mkdir sub; echo generated > sub/value")))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build sub
  Success
  $ cat sub/value _build/default/sub/value
  original
  generated

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build sub/value
  Success
  $ cat sub/value _build/default/sub/value
  original
  original

Ignoring a source directory removes its build-tree copies, not the source
files. Making the directory visible again must recreate those copies.

  $ cat >dune <<EOF
  > (dirs :standard \ sub)
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build fallback
  Success
  $ test ! -e _build/default/sub
  $ cat sub/value
  original

  $ cat >dune <<EOF
  > (rule
  >  (target output)
  >  (deps input)
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (mode fallback)
  >  (target fallback)
  >  (action (write-file %{target} "generated\n")))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build sub/value
  Success
  $ cat sub/value _build/default/sub/value
  original
  original

Repeated ownership changes must also retire copies held by cached point and
glob requests. An unrelated source change exercises their restoration without
changing either owner or copied source contents. Retirement must reach cached
nested descendants without affecting a sibling directory.

  $ mkdir -p sub/nested/deep sibling
  $ echo original > sub/nested/deep/value
  $ echo sibling > sibling/value
  $ cat >dune <<EOF
  > (rule
  >  (target globbed)
  >  (deps (glob_files sub/v*))
  >  (action (copy %{deps} %{target})))
  > EOF
  $ with_timeout dune rpc flush-file-watcher --wait
  $ build switched sub/value sub/nested/deep/value sibling/value globbed
  Success
  $ cat switched sub/value _build/default/globbed
  original
  original
  original
  $ cat _build/default/sub/nested/deep/value _build/default/sibling/value
  original
  sibling

  $ for round in first second; do
  > cat >dune <<EOF
  > (rule
  >  (target globbed)
  >  (deps (glob_files sub/v*))
  >  (action (copy %{deps} %{target})))
  > (rule
  >  (target switched)
  >  (mode (promote (into promoted)))
  >  (action (write-file %{target} "generated\n")))
  > (rule
  >  (target (dir sub))
  >  (mode (promote (into promoted)))
  >  (deps (sandbox always))
  >  (action
  >   (bash "mkdir -p sub/nested/deep
  >          echo generated > sub/value
  >          echo generated > sub/nested/deep/value")))
  > EOF
  > with_timeout dune rpc flush-file-watcher --wait
  > build switched sub
  > build globbed sub/value switched sub/nested/deep/value sibling/value
  > cat _build/default/switched _build/default/sub/value _build/default/globbed
  > cat sub/nested/deep/value _build/default/sub/nested/deep/value
  > cat _build/default/sibling/value
  > echo "$round" > epoch
  > with_timeout dune rpc flush-file-watcher --wait
  > build globbed switched sub/value sub/nested/deep/value sibling/value
  > cat _build/default/globbed _build/default/sub/nested/deep/value \
  >   _build/default/sibling/value
  > cat >dune <<EOF
  > (rule
  >  (target globbed)
  >  (deps (glob_files sub/v*))
  >  (action (copy %{deps} %{target})))
  > EOF
  > with_timeout dune rpc flush-file-watcher --wait
  > build globbed switched sub/value sub/nested/deep/value sibling/value
  > cat switched sub/value _build/default/switched _build/default/sub/value \
  >   _build/default/globbed
  > cat sub/nested/deep/value _build/default/sub/nested/deep/value
  > cat _build/default/sibling/value
  > echo "$round-restored" > epoch
  > with_timeout dune rpc flush-file-watcher --wait
  > build globbed switched sub/value sub/nested/deep/value sibling/value
  > cat _build/default/globbed _build/default/sub/nested/deep/value \
  >   _build/default/sibling/value
  > done
  Success
  Success
  generated
  generated
  generated
  original
  generated
  sibling
  Success
  generated
  generated
  sibling
  Success
  original
  original
  original
  original
  original
  original
  original
  sibling
  Success
  original
  original
  sibling
  Success
  Success
  generated
  generated
  generated
  original
  generated
  sibling
  Success
  generated
  generated
  sibling
  Success
  original
  original
  original
  original
  original
  original
  original
  sibling
  Success
  original
  original
  sibling
  $ stop_dune_quiet
