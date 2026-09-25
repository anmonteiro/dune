Loading data rules must not require evaluating the modules that consume them.
These examples also check the interactions between partial rule loading and
source copies, generated modules, rule modes, and target validation.

  $ make_dune_project 3.25

A source file in the library's own directory can describe its modules.
Changing the list must change the available modules without cleaning.

  $ mkdir source
  $ cat >source/dune <<EOF
  > (library
  >  (name source_list)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch source/first.ml source/second.ml
  $ echo first >source/lst
  $ dune build '%{cmi:source/First}'
  $ test -f _build/default/source/.source_list.objs/byte/source_list__First.cmi
  $ echo second >source/lst
  $ dune build '%{cmi:source/Second}'
  $ test ! -e _build/default/source/.source_list.objs/byte/source_list__First.cmi
  $ dune build '%{cmi:source/First}'
  File "command line", line 1, characters 0-19:
  Error: Module First does not exist.
  [1]

An input is not a compilation output just because its extension is `.ml`.

  $ cat >source/dune <<EOF
  > (library
  >  (name source_list)
  >  (modes byte)
  >  (modules (:include modules.ml)))
  > EOF
  $ echo first >source/modules.ml
  $ dune build '%{cmi:source/First}'

A direct request in an internal generated directory must inherit its parent's
ownership declarations without loading the complete parent directory first.

  $ dune build --build-dir _build-generated-dir \
  >   source/.source_list.objs/byte/source_list__First.cmi

An ordinary rule can generate the list alongside a rule generating an OCaml
source. Changing the input must discover and compile the newly selected module.

  $ mkdir generated
  $ cat >generated/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (copy lst.in %{target})))
  > (rule
  >  (target generated.ml)
  >  (action (write-file %{target} "let value = 42")))
  > (library
  >  (name generated_list)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch generated/handwritten.ml
  $ echo handwritten >generated/lst.in
  $ dune build generated/generated_list.cma
  $ echo generated >generated/lst.in
  $ dune build generated/generated_list.cma
  $ dune build '%{cmi:generated/Generated}'
  $ dune build '%{cmi:generated/Handwritten}'
  File "command line", line 1, characters 0-28:
  Error: Module Handwritten does not exist.
  [1]

Building only the data file must not delete still-valid compilation artifacts.

  $ dune build generated/lst
  $ test -f _build/default/generated/generated_list.cma
  $ test -f _build/default/generated/generated.ml
  $ test -d _build/default/generated/.generated_list.objs

Requesting the data and compilation targets in either order must agree, even
when neither target exists yet.

  $ dune build --build-dir _build-data-first \
  >   generated/lst generated/generated_list.cma
  $ dune build --build-dir _build-library-first \
  >   generated/generated_list.cma generated/lst

The same dependency set can request both the data and the library whose rule
generation reads it.

  $ cat >dune <<EOF
  > (alias
  >  (name paired)
  >  (deps generated/lst generated/generated_list.cma))
  > EOF
  $ dune build --build-dir _build-paired @paired

An already-known dependency set must not load an unrelated broken library.
When that library is requested, its rule-generation error must not prevent
an independent action from running and reporting its own failure.

  $ mkdir paired-errors
  $ cat >paired-errors/dune <<EOF
  > (library
  >  (name bad)
  >  (modes byte)
  >  (modules Missing))
  > (rule
  >  (targets first second)
  >  (action
  >   (progn
  >    (write-file first "one\n")
  >    (write-file second "two\n"))))
  > (rule
  >  (target action-error)
  >  (action
  >   (progn
  >    (echo "independent action failed\n")
  >    (run false))))
  > (alias
  >  (name good)
  >  (deps first second))
  > (alias
  >  (name errors)
  >  (deps bad.cma action-error))
  > EOF
  $ dune build @paired-errors/good
  $ cat _build/default/paired-errors/first _build/default/paired-errors/second
  one
  two
  $ dune build @paired-errors/errors >paired-errors.log 2>&1
  [1]
  $ grep '^Error: Module Missing' paired-errors.log
  Error: Module Missing doesn't exist.
  $ grep '^independent action failed$' paired-errors.log
  independent action failed
  $ grep -c '^Error: Module Missing' paired-errors.log
  1
  $ grep -c '^Command exited with code 1\.' paired-errors.log
  1

Removing the library must still allow stale compilation artifacts to be removed
when the directory's complete target set is requested.

  $ cat >generated/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (copy lst.in %{target})))
  > (rule
  >  (target generated.ml)
  >  (action (write-file %{target} "let value = 42")))
  > EOF
  $ dune build @generated/all
  $ test ! -e _build/default/generated/generated_list.cma

Mappings can likewise read source or generated data in the group root.

  $ mkdir -p mapping/internal
  $ cat >mapping/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as %{read:mapping})))
  > (library (name root_mapping) (modes byte))
  > EOF
  $ touch mapping/internal/leaf.ml
  $ printf public >mapping/mapping
  $ dune build '%{cmi:mapping/Public.Leaf}'
  $ rm mapping/mapping
  $ cat >mapping/dune <<EOF
  > (include_subdirs
  >  (mode qualified)
  >  (dirs (internal as %{read:mapping})))
  > (library (name root_mapping) (modes byte))
  > (rule
  >  (target mapping)
  >  (action (copy mapping.in %{target})))
  > EOF
  $ printf public >mapping/mapping.in
  $ dune build '%{cmi:mapping/Public.Leaf}'
  $ printf exposed >mapping/mapping.in
  $ dune build '%{cmi:mapping/Exposed.Leaf}'
  $ dune build '%{cmi:mapping/Public.Leaf}'
  File "command line", line 1, characters 0-26:
  Error: Module Public.Leaf does not exist.
  [1]

Fallback rules use their generated contents only while the source is absent.
Adding and removing the source file must update the selected modules.

  $ mkdir fallback
  $ cat >fallback/dune <<EOF
  > (rule
  >  (target lst)
  >  (mode fallback)
  >  (action (copy lst.in %{target})))
  > (library
  >  (name fallback_list)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch fallback/first.ml fallback/second.ml
  $ echo first >fallback/lst.in
  $ dune build '%{cmi:fallback/First}'
  $ echo second >fallback/lst
  $ dune build '%{cmi:fallback/Second}'
  $ rm fallback/lst
  $ dune build '%{cmi:fallback/First}'

Partially present fallback targets must still be rejected during an early
lookup of the source file.

  $ echo second >fallback/lst
  $ cat >fallback/dune <<EOF
  > (rule
  >  (targets lst other)
  >  (mode fallback)
  >  (action
  >   (progn
  >    (copy lst.in lst)
  >    (write-file other ""))))
  > (library
  >  (name fallback_list)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ dune build fallback/lst
  File "fallback/dune", lines 1-7, characters 0-110:
  1 | (rule
  2 |  (targets lst other)
  3 |  (mode fallback)
  4 |  (action
  5 |   (progn
  6 |    (copy lst.in lst)
  7 |    (write-file other ""))))
  Error: Some of the targets of this fallback rule are present in the source
  tree, and some are not. This is not allowed. Either none of the targets must
  be present in the source tree, either they must all be.
  
  The following targets are present:
  - fallback/lst
  
  The following targets are not:
  - fallback/other
  [1]

Promotion must use the generated contents, not an early copy of the old source.

  $ mkdir promotion
  $ cat >promotion/dune <<EOF
  > (rule
  >  (target lst)
  >  (mode promote)
  >  (action (copy lst.in %{target})))
  > (library
  >  (name promoted_list)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch promotion/first.ml promotion/second.ml
  $ echo first >promotion/lst
  $ echo second >promotion/lst.in
  $ dune build '%{cmi:promotion/Second}'
  $ cat promotion/lst
  second
  $ echo first >promotion/lst.in
  $ dune build '%{cmi:promotion/First}'
  $ cat promotion/lst
  first

In standard mode, the same source and generated target must conflict.

  $ cat >promotion/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (copy lst.in %{target})))
  > (library
  >  (name promoted_list)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ dune build promotion/lst
  Error: Multiple rules generated for _build/default/promotion/lst:
  - promotion/dune:1
  - file present in source tree
  Hint: rm -f promotion/lst
  [1]

A target from an ordinary rule cannot conceal the same target produced by a
later compilation stage. Direct target and alias requests must both reject it.

  $ mkdir duplicate
  $ cat >duplicate/dune <<EOF
  > (rule
  >  (target duplicate.cma)
  >  (action (write-file %{target} "not an archive")))
  > (library (name duplicate) (modes byte) (modules value))
  > EOF
  $ touch duplicate/value.ml
  $ dune build duplicate/duplicate.cma
  Error: Multiple rules generated for _build/default/duplicate/duplicate.cma:
  - duplicate/dune:4
  - duplicate/dune:1
  [1]
  $ dune build @duplicate/all
  Error: Multiple rules generated for _build/default/duplicate/duplicate.cma:
  - duplicate/dune:4
  - duplicate/dune:1
  -> required by { dir = In_build_dir "default/duplicate"
     ; predicate = True
     ; only_generated_files = true
     }
  -> required by alias duplicate/all
  [1]

The same validation applies to every target of a rule. Requesting its data
output must not hide a conflict on another output produced by that rule.

  $ mkdir mixed
  $ cat >mixed/dune <<EOF
  > (rule
  >  (targets lst mixed.cma)
  >  (action
  >   (progn
  >    (write-file lst value)
  >    (write-file mixed.cma "not an archive"))))
  > (library (name mixed) (modes byte) (modules value))
  > EOF
  $ touch mixed/value.ml
  $ dune build mixed/lst
  Error: Multiple rules generated for _build/default/mixed/mixed.cma:
  - mixed/dune:7
  - mixed/dune:1
  [1]
  $ dune build @mixed/all
  Error: Multiple rules generated for _build/default/mixed/mixed.cma:
  - mixed/dune:7
  - mixed/dune:1
  -> required by { dir = In_build_dir "default/mixed"
     ; predicate = True
     ; only_generated_files = true
     }
  -> required by alias mixed/all
  [1]

Fallback selection must likewise account for promotion by a later stage. Both
fallback outputs exist as sources, but promoting the executable suppresses one
of those sources, leaving only part of the fallback rule's targets present.

  $ mkdir fallback-promotion
  $ cat >fallback-promotion/dune <<EOF
  > (rule
  >  (targets lst promoted.bc)
  >  (mode fallback)
  >  (action
  >   (progn
  >    (write-file lst value)
  >    (write-file promoted.bc "not bytecode"))))
  > (executable (name promoted) (modes byte) (promote))
  > EOF
  $ touch fallback-promotion/promoted.ml
  $ echo value >fallback-promotion/lst
  $ echo source >fallback-promotion/promoted.bc
  $ dune build fallback-promotion/lst
  File "fallback-promotion/dune", lines 1-7, characters 0-139:
  1 | (rule
  2 |  (targets lst promoted.bc)
  3 |  (mode fallback)
  4 |  (action
  5 |   (progn
  6 |    (write-file lst value)
  7 |    (write-file promoted.bc "not bytecode"))))
  Error: Some of the targets of this fallback rule are present in the source
  tree, and some are not. This is not allowed. Either none of the targets must
  be present in the source tree, either they must all be.
  
  The following targets are present:
  - fallback-promotion/lst
  
  The following targets are not:
  - fallback-promotion/promoted.bc
  [1]

A real dependency cycle remains an error: generating the module list requires
building the very library whose module list it describes.

  $ mkdir cycle
  $ cat >cycle/dune <<EOF
  > (rule
  >  (target lst)
  >  (deps recursive.cma)
  >  (action (write-file %{target} value)))
  > (library
  >  (name recursive)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch cycle/value.ml
  $ dune build cycle/recursive.cma
  Error: Dependency cycle between:
     (modules) field at cycle/dune:5
  -> _build/default/cycle/lst
  -> (:include _build/default/cycle/lst) at cycle/dune:8
  -> (modules) field at cycle/dune:5
  [1]

Unsandboxed actions can leave temporary files, which must survive the build
that creates them when its rule loading has already completed.

  $ mkdir cleanup
  $ echo '(lang dune 3.22)' >cleanup/dune-project
  $ cat >cleanup/dune <<EOF
  > (rule
  >  (target lst)
  >  (deps (sandbox none))
  >  (action
  >   (progn
  >    (write-file lst value)
  >    (run touch scratch))))
  > (library
  >  (name cleanup)
  >  (modes byte)
  >  (modules value))
  > EOF
  $ touch cleanup/value.ml
  $ dune build cleanup/lst cleanup/cleanup.cma
  $ test -f _build/default/cleanup/scratch

On the next build, the undeclared file is stale and should be removed, even
when only the data target is requested.

  $ dune build cleanup/lst
  $ test ! -e _build/default/cleanup/scratch

In a staged directory, removing implicit interfaces must remove their old
files before compilation, even if the source stage loaded first.

  $ mkdir empty-interface
  $ cat >empty-interface/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (write-file %{target} b)))
  > (library
  >  (name empty_interface)
  >  (modes byte)
  >  (modules (:include lst))
  >  (empty_module_interface_if_absent))
  > EOF
  $ touch empty-interface/b.ml
  $ dune build empty-interface/empty_interface.cma
  $ test -f _build/default/empty-interface/b.mli

  $ cat >empty-interface/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (write-file %{target} b)))
  > (library
  >  (name empty_interface)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ dune build empty-interface/empty_interface.cma
  $ test ! -e _build/default/empty-interface/b.mli

Dynamic module lists are supported since 3.13. Reading an independent rule in
the same directory also works for projects that predate staged loading.

  $ mkdir legacy
  $ cat >legacy/dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >legacy/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (write-file %{target} value)))
  > (library
  >  (name legacy)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch legacy/value.ml
  $ dune build legacy/legacy.cma

A custom test action must not prevent its module list from being generated.

  $ mkdir custom-action
  $ cat >custom-action/dune <<EOF
  > (rule
  >  (target lst)
  >  (action (write-file %{target} test)))
  > (test
  >  (name test)
  >  (modes byte)
  >  (modules (:include lst))
  >  (action (run %{test})))
  > EOF
  $ cat >custom-action/test.ml <<EOF
  > let () = print_endline "custom test"
  > EOF
  $ dune build @custom-action/runtest
  custom test

Pulling a module list through several descendant directories must discover
all intermediate producers and track changes to the leaf input.

  $ mkdir -p nested/child/grandchild
  $ cat >nested/dune <<EOF
  > (include_subdirs unqualified)
  > (rule
  >  (target root-modules)
  >  (action (copy child/child-modules %{target})))
  > (library
  >  (name nested)
  >  (modes byte)
  >  (modules (:include root-modules)))
  > EOF
  $ cat >nested/child/dune <<EOF
  > (rule
  >  (target child-modules)
  >  (action (copy grandchild/leaf-modules %{target})))
  > EOF
  $ cat >nested/child/grandchild/dune <<EOF
  > (rule
  >  (target leaf-modules)
  >  (action (copy modules.in %{target})))
  > EOF
  $ touch nested/child/grandchild/first.ml nested/child/grandchild/second.ml
  $ echo first >nested/child/grandchild/modules.in
  $ dune build '%{cmi:nested/First}'
  $ echo second >nested/child/grandchild/modules.in
  $ dune build '%{cmi:nested/Second}'

Copying generated files upward must not force unrelated rules in the child.
The child can itself copy an independent parent output.

  $ mkdir -p copied-list/child
  $ cat >copied-list/dune <<EOF
  > (rule
  >  (target seed)
  >  (action (write-file %{target} value)))
  > (copy_files child/*.modules)
  > (library
  >  (name copied_list)
  >  (modes byte)
  >  (modules (:include selected.modules)))
  > EOF
  $ cat >copied-list/child/dune <<EOF
  > (copy_files ../seed)
  > (rule
  >  (target selected.modules)
  >  (action (copy seed %{target})))
  > EOF
  $ touch copied-list/value.ml
  $ dune build copied-list/copied_list.cma

Configurator runtime files are available even when the only action is attached
to an alias, without building a local executable first.

  $ mkdir configurator-alias
  $ cat >configurator-alias/dune-project <<EOF
  > (lang dune 3.13)
  > EOF
  $ cat >configurator-alias/dune <<EOF
  > (rule
  >  (alias check)
  >  (action
  >   (bash "test -f .dune/configurator && test -f .dune/configurator.v2")))
  > EOF
  $ (cd configurator-alias && dune build @check)

Alias-only requests also remove anonymous-action directories belonging to
source directories that no longer exist.

  $ mkdir -p anonymous-cleanup/child
  $ cat >anonymous-cleanup/dune <<EOF
  > (alias (name check))
  > EOF
  $ cat >anonymous-cleanup/child/dune <<EOF
  > (rule (alias check) (action (echo child-action)))
  > EOF
  $ dune build @anonymous-cleanup/check
  child-action
  $ test -d _build/.actions/default/anonymous-cleanup/child
  $ rm -r anonymous-cleanup/child
  $ dune build @anonymous-cleanup/check
  $ test ! -e _build/.actions/default/anonymous-cleanup/child

Explicit targets do not disable inference from the action. Both the explicit
target and an additional inferred target must find the same complete rule.

  $ mkdir static-inferred
  $ cat >static-inferred/dune <<EOF
  > (rule
  >  (targets declared)
  >  (action
  >   (progn
  >    (write-file declared explicit)
  >    (write-file extra inferred))))
  > EOF
  $ dune build --build-dir _build-inferred-extra static-inferred/extra
  $ cat _build-inferred-extra/default/static-inferred/declared
  explicit
  $ cat _build-inferred-extra/default/static-inferred/extra
  inferred
  $ dune build --build-dir _build-inferred-declared static-inferred/declared
  $ cat _build-inferred-declared/default/static-inferred/extra
  inferred

The additional inferred target may have a dynamic name even when the explicit
target declaration is static.

  $ mkdir dynamic-inferred
  $ cat >dynamic-inferred/dune <<EOF
  > (rule
  >  (target declared)
  >  (action
  >   (progn
  >    (write-file declared explicit)
  >    (write-file %{env:DUNE_STAGE_EXTRA_TARGET=extra} inferred))))
  > EOF
  $ DUNE_STAGE_EXTRA_TARGET=extra dune build dynamic-inferred/extra
  $ cat _build/default/dynamic-inferred/declared
  explicit
  $ cat _build/default/dynamic-inferred/extra
  inferred
  $ DUNE_STAGE_EXTRA_TARGET=renamed dune build dynamic-inferred/renamed
  $ cat _build/default/dynamic-inferred/renamed
  inferred
  $ test ! -e _build/default/dynamic-inferred/extra

Refining several overlapping dynamic producers must remove both stale outputs,
while retaining outputs confirmed by an earlier request in the same build.

  $ mkdir overlapping-cleanup
  $ cat >overlapping-cleanup/dune <<EOF
  > (rule
  >  (target first)
  >  (action
  >   (progn
  >    (write-file first first)
  >    (write-file %{env:DUNE_FIRST_OUTPUT=old-first} first))))
  > (rule
  >  (target second)
  >  (deps first)
  >  (action
  >   (progn
  >    (write-file second second)
  >    (write-file %{env:DUNE_SECOND_OUTPUT=old-second} second))))
  > EOF
  $ dune build overlapping-cleanup/first overlapping-cleanup/second
  $ DUNE_FIRST_OUTPUT=new-first DUNE_SECOND_OUTPUT=new-second dune build \
  >   overlapping-cleanup/first overlapping-cleanup/second
  $ cat _build/default/overlapping-cleanup/new-first
  first
  $ cat _build/default/overlapping-cleanup/new-second
  second
  $ test ! -e _build/default/overlapping-cleanup/old-first
  $ test ! -e _build/default/overlapping-cleanup/old-second

A combined request can rule out an artifact that either producer alone left
pending. Cleanup must reconsider their overlap even if both producers were
already requested separately earlier in the same build.

  $ mkdir -p combined-cleanup/left combined-cleanup/right
  $ touch combined-cleanup/left/a combined-cleanup/right/b
  $ cat >combined-cleanup/dune <<EOF
  > (copy_files left/{a,stale})
  > (copy_files right/{b,stale})
  > (rule
  >  (target pattern)
  >  (deps a b)
  >  (action (write-file %{target} "{a,b,stale}")))
  > (rule
  >  (target done)
  >  (deps (glob_files %{read:pattern}))
  >  (action (write-file %{target} done)))
  > EOF
  $ mkdir -p _build/default/combined-cleanup
  $ touch _build/default/combined-cleanup/stale
  $ dune build combined-cleanup/done
  $ test ! -e _build/default/combined-cleanup/stale

Sparse requests across several cleanup blocks must remove only the stale
entries belonging to the requested producer. Interleaved sibling entries stay
available until their own producer is refined.

  $ mkdir -p blocked-cleanup/left blocked-cleanup/right
  $ touch blocked-cleanup/left/a blocked-cleanup/left/c blocked-cleanup/left/e \
  >   blocked-cleanup/left/g blocked-cleanup/left/i blocked-cleanup/left/k
  $ touch blocked-cleanup/right/b blocked-cleanup/right/d blocked-cleanup/right/f \
  >   blocked-cleanup/right/h blocked-cleanup/right/j blocked-cleanup/right/l
  $ cat >blocked-cleanup/dune <<EOF
  > (copy_files left/{a,c,e,g,i,k,stale-left})
  > (copy_files right/{b,d,f,h,j,l,stale-right})
  > EOF
  $ mkdir -p _build/default/blocked-cleanup
  $ touch _build/default/blocked-cleanup/a _build/default/blocked-cleanup/b \
  >   _build/default/blocked-cleanup/c _build/default/blocked-cleanup/d \
  >   _build/default/blocked-cleanup/e _build/default/blocked-cleanup/f \
  >   _build/default/blocked-cleanup/g _build/default/blocked-cleanup/h \
  >   _build/default/blocked-cleanup/i _build/default/blocked-cleanup/j \
  >   _build/default/blocked-cleanup/k _build/default/blocked-cleanup/l \
  >   _build/default/blocked-cleanup/stale-left \
  >   _build/default/blocked-cleanup/stale-right
  $ dune build blocked-cleanup/a
  $ test ! -e _build/default/blocked-cleanup/stale-left
  $ test -f _build/default/blocked-cleanup/stale-right
  $ for name in a b c d e f g h i j k l; do
  >   test -f _build/default/blocked-cleanup/$name || exit 1
  > done
  $ dune build blocked-cleanup/b
  $ test ! -e _build/default/blocked-cleanup/stale-right
  $ for name in a b c d e f g h i j k l; do
  >   test -f _build/default/blocked-cleanup/$name || exit 1
  > done

Exact declarations also refine interleaved entries across several cleanup
blocks. Disabling one producer removes its old files without touching the
other producer's pending files.

  $ mkdir exact-cleanup
  $ echo false >exact-cleanup/enabled
  $ cat >exact-cleanup/dune <<EOF
  > (rule
  >  (targets a c e g i k m o)
  >  (enabled_if (= %{read:enabled} true))
  >  (action (run touch %{targets})))
  > (rule
  >  (targets b d f h j l n p)
  >  (enabled_if (= %{read:enabled} true))
  >  (action (run touch %{targets})))
  > (rule
  >  (target first)
  >  (deps (glob_files a))
  >  (action (write-file %{target} first)))
  > (rule
  >  (target second)
  >  (deps first (glob_files b))
  >  (action (write-file %{target} second)))
  > EOF
  $ mkdir -p _build/default/exact-cleanup
  $ for name in a b c d e f g h i j k l m n o p; do
  >   touch _build/default/exact-cleanup/$name
  > done
  $ dune build exact-cleanup/first
  $ for name in a c e g i k m o; do
  >   test ! -e _build/default/exact-cleanup/$name || exit 1
  > done
  $ for name in b d f h j l n p; do
  >   test -f _build/default/exact-cleanup/$name || exit 1
  > done
  $ dune build exact-cleanup/second
  $ for name in a b c d e f g h i j k l m n o p; do
  >   test ! -e _build/default/exact-cleanup/$name || exit 1
  > done

Contiguous declarations can empty one cleanup branch before the remaining
names in the same refinement have been visited.

  $ cat >exact-cleanup/dune <<EOF
  > (rule
  >  (targets a b c d e f g h)
  >  (enabled_if (= %{read:enabled} true))
  >  (action (run touch %{targets})))
  > (rule
  >  (targets i j k l m n o p)
  >  (enabled_if (= %{read:enabled} true))
  >  (action (run touch %{targets})))
  > (rule
  >  (target first)
  >  (deps (glob_files a))
  >  (action (write-file %{target} first)))
  > (rule
  >  (target second)
  >  (deps first (glob_files i))
  >  (action (write-file %{target} second)))
  > EOF
  $ for name in a b c d e f g h i j k l m n o p; do
  >   touch _build/default/exact-cleanup/$name
  > done
  $ dune build exact-cleanup/second
  $ for name in a b c d e f g h i j k l m n o p; do
  >   test ! -e _build/default/exact-cleanup/$name || exit 1
  > done

Target variables backed by a single literal declaration stay precise. Loading
the seed must not force the independent rules whose conditions read that seed.

  $ mkdir -p bound-targets/child
  $ cat >bound-targets/dune <<EOF
  > (rule
  >  (target seed)
  >  (action (write-file %{target} true)))
  > (rule
  >  (target single)
  >  (enabled_if (= %{read:seed} true))
  >  (action (write-file %{target} single)))
  > (rule
  >  (targets multiple)
  >  (enabled_if (= %{read:seed} true))
  >  (action (write-file %{targets} multiple)))
  > (rule
  >  (target from-child)
  >  (action (chdir child (write-file %{target} parent))))
  > EOF
  $ dune build bound-targets/seed
  $ dune build bound-targets/single bound-targets/multiple bound-targets/from-child
  $ cat _build/default/bound-targets/single
  single
  $ cat _build/default/bound-targets/multiple
  multiple
  $ cat _build/default/bound-targets/from-child
  parent

Quoted multi-target variables retain the ordinary target-directory validation.

  $ mkdir quoted-targets
  $ cat >quoted-targets/dune <<EOF
  > (rule
  >  (targets a b)
  >  (action
  >   (progn
  >    (write-file a first)
  >    (write-file b second)
  >    (write-file "%{targets}" joined))))
  > EOF
  $ dune build '"quoted-targets/a b"'
  File "quoted-targets/dune", line 7, characters 15-27:
  7 |    (write-file "%{targets}" joined))))
                     ^^^^^^^^^^^^
  Error: This action has targets in a different directory than the current one,
  this is not allowed by dune at the moment:
  - quoted-targets/a
  - "quoted-targets/a ./b"
  - quoted-targets/b
  [1]

Refining a directory's rules must not remove fresh temporary files created
after its initial cleanup. A later build still removes those stale files.
This also holds when the action replaces an old file removed by that cleanup.

  $ mkdir cleanup-inventory
  $ cat >cleanup-inventory/dune-project <<EOF
  > (lang dune 3.22)
  > EOF
  $ cat >cleanup-inventory/dune <<EOF
  > (rule
  >  (target lst)
  >  (deps (sandbox none))
  >  (action
  >   (progn
  >    (write-file %{target} value)
  >    (no-infer (write-file compiler.tmp temporary)))))
  > (library
  >  (name temporary_files)
  >  (modes byte)
  >  (modules (:include lst)))
  > EOF
  $ touch cleanup-inventory/value.ml
  $ mkdir -p _build/default/cleanup-inventory
  $ echo stale >_build/default/cleanup-inventory/compiler.tmp
  $ dune build cleanup-inventory/temporary_files.cma
  $ cat _build/default/cleanup-inventory/compiler.tmp
  temporary
  $ dune build cleanup-inventory/lst
  $ test ! -e _build/default/cleanup-inventory/compiler.tmp

Literal inferred targets must be as precise as explicit target declarations.
Loading seed must not expand the condition of an independent inferred rule.

  $ mkdir inferred-producers
  $ cat >inferred-producers/dune <<EOF
  > (rule
  >  (action (write-file seed true)))
  > (rule
  >  (enabled_if (= %{read:seed} true))
  >  (action (write-file result ready)))
  > EOF
  $ dune build inferred-producers/seed
  $ dune build inferred-producers/result
  $ test -f _build/default/inferred-producers/seed
  $ cat _build/default/inferred-producers/result
  ready

An alias used to generate a module list must not prepare the library that
consumes that list. Both the independent alias and the library should build.

  $ mkdir alias-producers
  $ cat >alias-producers/dune <<EOF
  > (alias (name ready))
  > (rule
  >  (target modules.list)
  >  (deps (alias ready))
  >  (action (write-file %{target} value)))
  > (library
  >  (name alias_consumer)
  >  (modes byte)
  >  (modules (:include modules.list)))
  > EOF
  $ touch alias-producers/value.ml
  $ dune build @alias-producers/ready
  $ dune build alias-producers/alias_consumer.cma

Discovering OCaml sources must not prepare unrelated data producers. The data
rule's condition can depend on the library whose sources are being discovered.

  $ mkdir source-producers
  $ cat >source-producers/dune <<EOF
  > (library
  >  (name source_consumer)
  >  (modes byte))
  > (rule
  >  (target stamp)
  >  (deps source_consumer.cma)
  >  (action (write-file %{target} true)))
  > (rule
  >  (target report.txt)
  >  (enabled_if (= %{read:stamp} true))
  >  (action (write-file %{target} ready)))
  > EOF
  $ touch source-producers/value.ml
  $ dune build source-producers/source_consumer.cma
  $ dune build source-producers/report.txt
  $ cat _build/default/source-producers/report.txt
  ready

The same separation applies to sources discovered in a qualified directory
group. A data producer in a child must not block the group's compilation.

  $ mkdir -p grouped-producers/child
  $ cat >grouped-producers/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name grouped_consumer)
  >  (modes byte))
  > (rule
  >  (target stamp)
  >  (deps grouped_consumer.cma)
  >  (action (write-file %{target} true)))
  > EOF
  $ cat >grouped-producers/child/dune <<EOF
  > (rule
  >  (target report.txt)
  >  (enabled_if (= %{read:../stamp} true))
  >  (action (write-file %{target} ready)))
  > EOF
  $ touch grouped-producers/child/value.ml
  $ dune build grouped-producers/grouped_consumer.cma
  $ dune build grouped-producers/child/report.txt
  $ cat _build/default/grouped-producers/child/report.txt
  ready

An independent alias must not prepare an unrelated library. The invalid module
list is still diagnosed when the library itself is requested.

  $ mkdir independent-alias
  $ cat >independent-alias/dune <<EOF
  > (alias (name ready))
  > (library
  >  (name broken)
  >  (modes byte)
  >  (modules (:include missing.list)))
  > EOF
  $ dune build @independent-alias/ready
  $ dune build independent-alias/broken.cma
  Error: No rule found for independent-alias/missing.list
  -> required by (:include _build/default/independent-alias/missing.list) at
     independent-alias/dune:5
  -> required by (modules) field at independent-alias/dune:2
  [1]

Merlin configurations must only prepare their own library. The other library's
missing foreign source remains an error when its configuration is requested.

  $ mkdir merlin-producers
  $ cat >merlin-producers/dune <<EOF
  > (library (name good) (modules good))
  > (library
  >  (name bad)
  >  (modules bad)
  >  (foreign_stubs (language c) (names missing)))
  > EOF
  $ touch merlin-producers/good.ml merlin-producers/bad.ml
  $ dune build merlin-producers/.merlin-conf/lib-good
  $ dune build merlin-producers/.merlin-conf/lib-bad
  File "merlin-producers/dune", line 5, characters 36-43:
  5 |  (foreign_stubs (language c) (names missing)))
                                          ^^^^^^^
  Error: Object "missing" has no source; "missing.c" must be present.
  [1]

Requesting one module reveals the compilation headers for its library, but
must not build unrelated modules. Action construction remains delayed.

  $ mkdir module-producers
  $ cat >module-producers/dune <<EOF
  > (library
  >  (name modules)
  >  (wrapped false)
  >  (modes byte))
  > EOF
  $ touch module-producers/a.ml module-producers/b.ml
  $ DUNE_TRACE=debug dune build module-producers/.modules.objs/byte/a.cmo
  $ dune trace cat | jq -sr '[.[] | select(.name == "rule_generated") | .args.target_files[]? | select(endswith(".cmo"))] | unique[]'
  _build/default/module-producers/.modules.objs/byte/a.cmo
  _build/default/module-producers/.modules.objs/byte/b.cmo
  $ test ! -e _build/default/module-producers/.modules.objs/byte/b.cmo
  $ test ! -e _build/default/module-producers/.modules.objs/byte/b.cmi

Selecting compilation headers must retain artifacts owned by an unrequested
sibling, including when switching between exact file and alias requests.

  $ dune build module-producers/.modules.objs/byte/b.cmo
  $ test -f _build/default/module-producers/.modules.objs/byte/a.cmo
  $ dune build module-producers/.modules.objs/byte/a.cmo
  $ test -f _build/default/module-producers/.modules.objs/byte/b.cmo
  $ dune build @module-producers/check
  $ test -f _build/default/module-producers/.modules.objs/byte/a.cmo
  $ test -f _build/default/module-producers/.modules.objs/byte/b.cmo

A wider shared header set must preserve that ownership across several point
lookups in one build, without building the unrequested siblings.

  $ for name in c d e f g h i j k l m n o p; do
  >   touch module-producers/$name.ml
  > done
  $ dune build module-producers/.modules.objs/byte/a.cmo \
  >   module-producers/.modules.objs/byte/c.cmo \
  >   module-producers/.modules.objs/byte/d.cmo
  $ test -f _build/default/module-producers/.modules.objs/byte/b.cmo
  $ test -f _build/default/module-producers/.modules.objs/byte/c.cmo
  $ test -f _build/default/module-producers/.modules.objs/byte/d.cmo
  $ test ! -e _build/default/module-producers/.modules.objs/byte/p.cmo

Ordinary compilation must not reveal JS or Wasm headers. Selecting one backend
module reveals all ordinary backend headers, but still delays the actions of
its siblings. A fake compiler avoids depending on optional backend tools;
its version predates the shape dependencies of newer js_of_ocaml releases.
Unwrapped executables avoid introducing a special alias module into this check.

  $ mkdir -p grouped-backends/bin
  $ cat >grouped-backends/bin/js_of_ocaml <<'EOF'
  > #!/bin/sh
  > while [ "$#" -gt 0 ]; do
  >   case "$1" in
  >     --version) echo 5.0.0; exit 0 ;;
  >     -o) printf 'fake backend output\n' >"$2"; exit 0 ;;
  >   esac
  >   shift
  > done
  > exit 1
  > EOF
  $ chmod +x grouped-backends/bin/js_of_ocaml
  $ cp grouped-backends/bin/js_of_ocaml grouped-backends/bin/wasm_of_ocaml
  $ cat >grouped-backends/dune-project <<EOF
  > (lang dune 3.25)
  > (wrapped_executables false)
  > EOF
  $ cat >grouped-backends/dune <<EOF
  > (executable
  >  (name a)
  >  (modules a b)
  >  (modes js wasm))
  > EOF
  $ echo 'let value = 42' >grouped-backends/a.ml
  $ echo 'let =' >grouped-backends/b.ml
  $ PATH="$PWD/grouped-backends/bin:$PATH" DUNE_TRACE=debug \
  >   dune build grouped-backends/.a.eobjs/byte/a.cmi
  $ dune trace cat | jq -sr '[.[] | select(.name == "rule_generated") | .args.target_files[]? | select(contains("/grouped-backends/.a.eobjs/jsoo/"))] | unique[]'
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/a.cmo.js
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/a.wasmo
  $ PATH="$PWD/grouped-backends/bin:$PATH" DUNE_TRACE=debug \
  >   dune build grouped-backends/.a.eobjs/jsoo/a.cmo.js
  $ dune trace cat | jq -sr '[.[] | select(.name == "rule_generated") | .args.target_files[]? | select(contains("/grouped-backends/.a.eobjs/jsoo/"))] | unique[]'
  _build/default/grouped-backends/.a.eobjs/jsoo/a.cmo.js
  _build/default/grouped-backends/.a.eobjs/jsoo/a.wasmo
  _build/default/grouped-backends/.a.eobjs/jsoo/b.cmo.js
  _build/default/grouped-backends/.a.eobjs/jsoo/b.wasmo
  $ cat _build/default/grouped-backends/.a.eobjs/jsoo/a.cmo.js
  fake backend output
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/a.wasmo
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/b.cmo.js
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/b.wasmo
  $ test ! -e _build/default/grouped-backends/.a.eobjs/byte/b.cmi

A later point request still removes an unowned backend artifact and preserves
the requested module's other backend output.

  $ touch _build/default/grouped-backends/.a.eobjs/jsoo/obsolete.cmo.js
  $ PATH="$PWD/grouped-backends/bin:$PATH" \
  >   dune build grouped-backends/.a.eobjs/jsoo/a.wasmo
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/obsolete.cmo.js
  $ test -f _build/default/grouped-backends/.a.eobjs/jsoo/a.cmo.js
  $ test -f _build/default/grouped-backends/.a.eobjs/jsoo/a.wasmo
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/b.cmo.js
  $ test ! -e _build/default/grouped-backends/.a.eobjs/jsoo/b.wasmo

The invalid sibling's source is diagnosed only when its backend is requested.

  $ PATH="$PWD/grouped-backends/bin:$PATH" \
  >   dune build grouped-backends/.a.eobjs/jsoo/b.cmo.js \
  >   >grouped-backend-error.log 2>&1
  [1]
  $ grep -q 'Syntax error' grouped-backend-error.log

Missing link and runtime flag inputs must not affect module compilation.

  $ cat >grouped-backends/dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (modes js wasm)
  >  (js_of_ocaml
  >   (link_flags (:include js-link-flags.sexp))
  >   (build_runtime_flags (:include js-runtime-flags.sexp)))
  >  (wasm_of_ocaml
  >   (link_flags (:include wasm-link-flags.sexp))
  >   (build_runtime_flags (:include wasm-runtime-flags.sexp))))
  > EOF
  $ PATH="$PWD/grouped-backends/bin:$PATH" \
  >   dune build grouped-backends/.a.eobjs/jsoo/a.cmo.js \
  >   grouped-backends/.a.eobjs/jsoo/a.wasmo

Selecting compilation flags must still report missing inputs, independently
for JS and Wasm.

  $ cat >grouped-backends/dune <<EOF
  > (executable
  >  (name a)
  >  (modules a)
  >  (modes js wasm)
  >  (js_of_ocaml (flags (:include js-compile-flags.sexp)))
  >  (wasm_of_ocaml (flags (:include wasm-compile-flags.sexp))))
  > EOF
  $ PATH="$PWD/grouped-backends/bin:$PATH" \
  >   dune build grouped-backends/.a.eobjs/jsoo/a.cmo.js \
  >   >grouped-backend-js-flags.log 2>&1
  [1]
  $ grep -q 'No rule found for grouped-backends/js-compile-flags.sexp' \
  >   grouped-backend-js-flags.log
  $ PATH="$PWD/grouped-backends/bin:$PATH" \
  >   dune build grouped-backends/.a.eobjs/jsoo/a.wasmo \
  >   >grouped-backend-wasm-flags.log 2>&1
  [1]
  $ grep -q 'No rule found for grouped-backends/wasm-compile-flags.sexp' \
  >   grouped-backend-wasm-flags.log

Source discovery still selects generated files in custom dialects. A data
producer depending on the resulting library remains independent.

  $ mkdir dialect-producers
  $ cat >dialect-producers/dune-project <<EOF
  > (lang dune 3.25)
  > (dialect
  >  (name copy)
  >  (implementation
  >   (extension copied)
  >   (preprocess (run cat %{input-file}))))
  > EOF
  $ cat >dialect-producers/dune <<EOF
  > (library (name dialect_consumer) (modes byte))
  > (rule
  >  (target value.copied)
  >  (action (write-file %{target} "let value = 42")))
  > (rule
  >  (target stamp)
  >  (deps dialect_consumer.cma)
  >  (action (write-file %{target} true)))
  > (rule
  >  (target report.txt)
  >  (enabled_if (= %{read:stamp} true))
  >  (action (write-file %{target} ready)))
  > EOF
  $ dune build dialect-producers/dialect_consumer.cma
  $ dune build dialect-producers/report.txt
  $ cat _build/default/dialect-producers/report.txt
  ready

Foreign compilation still discovers generated headers, and tests still discover
their generated expected output through their targeted filename requests.

  $ mkdir header-producers
  $ cat >header-producers/dune <<EOF
  > (library
  >  (name headers)
  >  (foreign_stubs (language c) (names stubs)))
  > (rule
  >  (target value.h)
  >  (action (write-file %{target} "#define VALUE 42")))
  > EOF
  $ touch header-producers/headers.ml
  $ cat >header-producers/stubs.c <<EOF
  > #include "value.h"
  > int value(void) { return VALUE; }
  > EOF
  $ dune build header-producers/libheaders_stubs.a

  $ mkdir expected-producers
  $ cat >expected-producers/dune <<EOF
  > (test (name check))
  > (rule
  >  (target check.expected)
  >  (action (write-file %{target} ready)))
  > EOF
  $ cat >expected-producers/check.ml <<EOF
  > let () = print_string "ready"
  > EOF
  $ dune build @expected-producers/runtest

Warm builds must preserve inferred flag files and wrapped interfaces when only
a sibling producer is requested, including after an implementation changes.

  $ mkdir -p warm-preservation/flags
  $ cat >warm-preservation/dune <<EOF
  > (library
  >  (name preserved)
  >  (modes byte)
  >  (flags (:standard (:include flags/flags.sexp))))
  > EOF
  $ cat >warm-preservation/flags/dune <<EOF
  > (rule
  >  (with-stdout-to flags.sexp (echo "()")))
  > (rule
  >  (with-stdout-to c_flags.sexp (echo "()")))
  > EOF
  $ cat >warm-preservation/a.ml <<EOF
  > let value = 1
  > EOF
  $ cat >warm-preservation/b.ml <<EOF
  > let value = A.value
  > EOF
  $ dune build warm-preservation/preserved.cma warm-preservation/flags/c_flags.sexp

Requesting one module leaves the other module and its inputs intact.

  $ dune build warm-preservation/.preserved.objs/byte/preserved__A.cmo
  $ test -f _build/default/warm-preservation/flags/flags.sexp
  $ test -f _build/default/warm-preservation/flags/c_flags.sexp
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__A.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__B.cmi

Requesting one inferred flag file leaves its sibling and the library intact.

  $ dune build warm-preservation/flags/c_flags.sexp
  $ test -f _build/default/warm-preservation/flags/flags.sexp
  $ test -f _build/default/warm-preservation/flags/c_flags.sexp
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__A.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__B.cmi

Switching to an alias request must preserve the same artifacts.

  $ dune build @warm-preservation/check
  $ test -f _build/default/warm-preservation/flags/flags.sexp
  $ test -f _build/default/warm-preservation/flags/c_flags.sexp
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__A.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__B.cmi

Rebuilding one implementation must also leave the sibling interface available.

  $ cat >warm-preservation/a.ml <<EOF
  > let value = 2
  > EOF
  $ dune build warm-preservation/.preserved.objs/byte/preserved__A.cmo
  $ test -f _build/default/warm-preservation/flags/flags.sexp
  $ test -f _build/default/warm-preservation/flags/c_flags.sexp
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__A.cmi
  $ test -f _build/default/warm-preservation/.preserved.objs/byte/preserved__B.cmi
  $ dune build warm-preservation/preserved.cma @warm-preservation/check

Requesting one interface must not parse or compile an unrelated implementation
with a syntax error, even if its compilation rules have been revealed. Native
action flags must also remain unevaluated while only bytecode is requested.

  $ mkdir delayed-module-actions
  $ cat >delayed-module-actions/dune <<EOF
  > (library
  >  (name delayed)
  >  (wrapped false)
  >  (modes byte)
  >  (ocamlopt_flags (:standard (:include native-flags.sexp))))
  > EOF
  $ cat >delayed-module-actions/a.ml <<EOF
  > let value = 42
  > EOF
  $ cat >delayed-module-actions/b.ml <<EOF
  > let =
  > EOF
  $ dune build delayed-module-actions/.delayed.objs/byte/a.cmi
  $ test -f _build/default/delayed-module-actions/.delayed.objs/byte/a.cmi
  $ test -f _build/default/delayed-module-actions/.delayed.objs/byte/a.cmo
  $ test -f _build/default/delayed-module-actions/.delayed.objs/byte/a.cmt
  $ test ! -e _build/default/delayed-module-actions/.delayed.objs/byte/b.cmi

The sibling's dependency parsing error is reported when it is requested.
After fixing its source, requesting its native output exposes the missing flag
input instead. Neither failure should have prevented the earlier atomic build.

  $ dune build delayed-module-actions/.delayed.objs/byte/b.cmi \
  >   >delayed-syntax.log 2>&1
  [1]
  $ grep -q 'Syntax error' delayed-syntax.log
  $ cat >delayed-module-actions/b.ml <<EOF
  > let value = 0
  > EOF
  $ dune build delayed-module-actions/.delayed.objs/native/b.cmx \
  >   >delayed-flags.log 2>&1
  [1]
  $ grep -q 'No rule found for delayed-module-actions/native-flags.sexp' \
  >   delayed-flags.log

A sibling's source generation and per-module preprocessing can both depend on
the requested interface. Neither action may run until the sibling is requested.

  $ mkdir delayed-module-inputs
  $ cat >delayed-module-inputs/dune <<'EOF'
  > (library
  >  (name delayed_inputs)
  >  (wrapped false)
  >  (modes byte)
  >  (preprocess
  >   (per_module
  >    ((action
  >      (progn
  >       (ignore-stdout (cat %{dep:.delayed_inputs.objs/byte/a.cmi}))
  >       (echo "(* preprocessed b *)\n")
  >       (cat %{input-file}))) B))))
  > (rule
  >  (target b.ml)
  >  (deps .delayed_inputs.objs/byte/a.cmi)
  >  (action (write-file %{target} "let value = A.value\n")))
  > EOF
  $ cat >delayed-module-inputs/a.ml <<EOF
  > let value = 42
  > EOF
  $ dune build delayed-module-inputs/.delayed_inputs.objs/byte/a.cmi
  $ test -f _build/default/delayed-module-inputs/.delayed_inputs.objs/byte/a.cmi
  $ test ! -e _build/default/delayed-module-inputs/b.ml
  $ test ! -e _build/default/delayed-module-inputs/b.pp.ml
  $ test ! -e _build/default/delayed-module-inputs/.delayed_inputs.objs/byte/b.cmi
  $ dune build delayed-module-inputs/.delayed_inputs.objs/byte/b.cmi
  $ test -f _build/default/delayed-module-inputs/b.ml
  $ test -f _build/default/delayed-module-inputs/b.pp.ml
  $ test -f _build/default/delayed-module-inputs/.delayed_inputs.objs/byte/b.cmi

Discovering a directory target through a descendant must not hide a same-name
file producer when the owning rule is subsequently loaded for building.

  $ mkdir directory-cache
  $ cat >directory-cache/dune-project <<EOF
  > (lang dune 3.25)
  > EOF
  $ cat >directory-cache/dune <<EOF
  > (rule
  >  (target output)
  >  (action (write-file %{target} file)))
  > (rule
  >  (targets (dir output))
  >  (deps (sandbox always))
  >  (action (bash "mkdir output && touch output/leaf")))
  > EOF
  $ dune build directory-cache/output/leaf
  Error: Multiple rules generated for _build/default/directory-cache/output:
  - directory-cache/dune:1
  - directory-cache/dune:4
  [1]

A mixed rule's file output must likewise validate the ownership of its directory
output, even though the requested file itself has only one producer.

  $ cat >directory-cache/dune <<EOF
  > (rule
  >  (target output)
  >  (action (write-file %{target} file)))
  > (rule
  >  (targets stamp (dir output))
  >  (deps (sandbox always))
  >  (action (bash "mkdir output && touch output/leaf stamp")))
  > EOF
  $ dune build directory-cache/stamp
  Error: Multiple rules generated for _build/default/directory-cache/output:
  - directory-cache/dune:1
  - directory-cache/dune:4
  [1]
