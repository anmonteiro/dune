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
  Error: Dependency cycle between:
     (modules) field at source/dune:1
  -> (:include _build/default/source/lst) at source/dune:4
  -> (modules) field at source/dune:1
  -> required by %{cmi:source/First} at command line:1
  [1]
  $ echo second >source/lst
  $ dune build '%{cmi:source/Second}'
  Error: Dependency cycle between:
     (modules) field at source/dune:1
  -> (:include _build/default/source/lst) at source/dune:4
  -> (modules) field at source/dune:1
  -> required by %{cmi:source/Second} at command line:1
  [1]
  $ dune build '%{cmi:source/First}'
  Error: Dependency cycle between:
     (modules) field at source/dune:1
  -> (:include _build/default/source/lst) at source/dune:4
  -> (modules) field at source/dune:1
  -> required by %{cmi:source/First} at command line:1
  [1]

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
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  [1]
  $ echo generated >generated/lst.in
  $ dune build generated/generated_list.cma
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  [1]
  $ dune build '%{cmi:generated/Generated}'
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  -> required by %{cmi:generated/Generated} at command line:1
  [1]
  $ dune build '%{cmi:generated/Handwritten}'
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  -> required by %{cmi:generated/Handwritten} at command line:1
  [1]

Building only the data file must not delete still-valid compilation artifacts.

  $ dune build generated/lst
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  [1]
  $ test -f _build/default/generated/generated_list.cma
  [1]
  $ test -f _build/default/generated/generated.ml
  [1]

Requesting the data and compilation targets in either order must agree, even
when neither target exists yet.

  $ dune build --build-dir _build-data-first \
  >   generated/lst generated/generated_list.cma
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build-data-first/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  [1]
  $ dune build --build-dir _build-library-first \
  >   generated/generated_list.cma generated/lst
  Error: Dependency cycle between:
     (modules) field at generated/dune:7
  -> (:include _build-library-first/default/generated/lst) at generated/dune:10
  -> (modules) field at generated/dune:7
  [1]

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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/mapping
  -> %{read:mapping} at mapping/dune:3
  -> Computing directory contents of _build/default/mapping
  -> required by %{cmi:mapping/Public.Leaf} at command line:1
  [1]
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
  Error: Dependency cycle between:
     Computing directory contents of _build/default/mapping
  -> %{read:mapping} at mapping/dune:3
  -> Computing directory contents of _build/default/mapping
  -> required by %{cmi:mapping/Public.Leaf} at command line:1
  [1]
  $ printf exposed >mapping/mapping.in
  $ dune build '%{cmi:mapping/Exposed.Leaf}'
  Error: Dependency cycle between:
     Computing directory contents of _build/default/mapping
  -> %{read:mapping} at mapping/dune:3
  -> Computing directory contents of _build/default/mapping
  -> required by %{cmi:mapping/Exposed.Leaf} at command line:1
  [1]
  $ dune build '%{cmi:mapping/Public.Leaf}'
  Error: Dependency cycle between:
     Computing directory contents of _build/default/mapping
  -> %{read:mapping} at mapping/dune:3
  -> Computing directory contents of _build/default/mapping
  -> required by %{cmi:mapping/Public.Leaf} at command line:1
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
  Error: Dependency cycle between:
     (modules) field at fallback/dune:5
  -> (:include _build/default/fallback/lst) at fallback/dune:8
  -> (modules) field at fallback/dune:5
  -> required by %{cmi:fallback/First} at command line:1
  [1]
  $ echo second >fallback/lst
  $ dune build '%{cmi:fallback/Second}'
  Error: Dependency cycle between:
     (modules) field at fallback/dune:5
  -> (:include _build/default/fallback/lst) at fallback/dune:8
  -> (modules) field at fallback/dune:5
  -> required by %{cmi:fallback/Second} at command line:1
  [1]
  $ rm fallback/lst
  $ dune build '%{cmi:fallback/First}'
  Error: Dependency cycle between:
     (modules) field at fallback/dune:5
  -> (:include _build/default/fallback/lst) at fallback/dune:8
  -> (modules) field at fallback/dune:5
  -> required by %{cmi:fallback/First} at command line:1
  [1]

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
  Error: Dependency cycle between:
     (modules) field at fallback/dune:8
  -> (:include _build/default/fallback/lst) at fallback/dune:11
  -> (modules) field at fallback/dune:8
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
  Error: Dependency cycle between:
     (modules) field at promotion/dune:5
  -> (:include _build/default/promotion/lst) at promotion/dune:8
  -> (modules) field at promotion/dune:5
  -> required by %{cmi:promotion/Second} at command line:1
  [1]
  $ cat promotion/lst
  first
  $ echo first >promotion/lst.in
  $ dune build '%{cmi:promotion/First}'
  Error: Dependency cycle between:
     (modules) field at promotion/dune:5
  -> (:include _build/default/promotion/lst) at promotion/dune:8
  -> (modules) field at promotion/dune:5
  -> required by %{cmi:promotion/First} at command line:1
  [1]
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
  Error: Dependency cycle between:
     (modules) field at promotion/dune:4
  -> (:include _build/default/promotion/lst) at promotion/dune:7
  -> (modules) field at promotion/dune:4
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
  Error: Dependency cycle between:
     (modules) field at empty-interface/dune:4
  -> (:include _build/default/empty-interface/lst) at empty-interface/dune:7
  -> (modules) field at empty-interface/dune:4
  [1]
  $ test -f _build/default/empty-interface/b.mli
  [1]

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
  Error: Dependency cycle between:
     (modules) field at empty-interface/dune:4
  -> (:include _build/default/empty-interface/lst) at empty-interface/dune:7
  -> (modules) field at empty-interface/dune:4
  [1]
  $ test ! -e _build/default/empty-interface/b.mli
