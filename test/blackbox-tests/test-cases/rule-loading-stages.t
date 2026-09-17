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
  $ echo second >source/lst
  $ dune build '%{cmi:source/Second}'
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

Older projects retain complete rule loading. Their unsandboxed actions can
leave temporary files, which must survive the build that creates them.

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
the same directory currently cycles when the project predates staged loading.

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
  Error: Dependency cycle between:
     (modules) field at legacy/dune:4
  -> (:include _build/default/legacy/lst) at legacy/dune:7
  -> (modules) field at legacy/dune:4
  [1]

A custom test action must not prevent its module list from being generated.
Currently the custom action makes the entire directory load eagerly and cycle.

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
  Error: Dependency cycle between:
     (modules) field at custom-action/dune:4
  -> (:include _build/default/custom-action/lst) at custom-action/dune:7
  -> (modules) field at custom-action/dune:4
  [1]

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
The child can itself copy an independent parent output. Currently complete
directory enumeration makes these two copy_files stanzas cycle.

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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/copied-list
  -> { dir = In_build_dir "default/copied-list"
     ; predicate = Element (Glob "seed")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/copied-list/child
  -> { dir = In_build_dir "default/copied-list/child"
     ; predicate = Element (Glob "*.modules")
     ; only_generated_files = false
     }
  -> Computing directory contents of _build/default/copied-list
  [1]
