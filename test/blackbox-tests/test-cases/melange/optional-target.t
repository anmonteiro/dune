Omitting the target emits JavaScript beside the source's build-tree copy.

  $ make_melange_project 3.25 1.0
  $ cat >dune <<EOF
  > (melange.emit
  >  (emit_stdlib false))
  > EOF
  $ cat >main.ml <<EOF
  > let () = Js.log "hello"
  > EOF

  $ dune build main.js
  $ node _build/default/main.js
  hello
  $ test ! -e main.js

Nested emits support generated module lists and sources, including subdirectories.
Looking up a nested JS file directly must not cycle through module discovery.

  $ mkdir -p app/child
  $ cat >app/dune <<EOF
  > (include_subdirs unqualified)
  > (rule
  >  (target modules.sexp)
  >  (action (write-file %{target} "(\"Main\" Generated Value)")))
  > (rule
  >  (target generated.ml)
  >  (action (write-file %{target} "let message = \"generated\"")))
  > (melange.emit
  >  (alias app)
  >  (modules (:include modules.sexp))
  >  (emit_stdlib false))
  > (rule
  >  (target local-path)
  >  (deps %{melange.emit:.})
  >  (action
  >   (with-stdout-to %{target} (echo "%{melange.emit:.}\n"))))
  > EOF
  $ cat >app/child/value.ml <<EOF
  > let message = "nested"
  > EOF
  $ cat >app/main.ml <<EOF
  > let () = Js.log (Generated.message ^ " " ^ Value.message)
  > EOF
  $ dune build app/child/value.js
  $ test -f _build/default/app/child/value.js

The default alias and an explicit output directory still work. The emit macro
resolves both the root emit and the nested emit, including its custom alias.

  $ cat >other.ml <<EOF
  > let () = Js.log "explicit target"
  > EOF
  $ cat >dune <<EOF
  > (melange.emit
  >  (modules main)
  >  (emit_stdlib false))
  > (melange.emit
  >  (target dist)
  >  (modules other)
  >  (emit_stdlib false))
  > (rule
  >  (target root-path)
  >  (deps %{melange.emit:.})
  >  (action
  >   (with-stdout-to %{target} (echo "%{melange.emit:.}\n"))))
  > (rule
  >  (target app-path)
  >  (deps %{melange.emit:app})
  >  (action
  >   (with-stdout-to %{target} (echo "%{melange.emit:app}\n"))))
  > EOF
  $ dune build @melange root-path app-path app/local-path
  $ cat _build/default/root-path _build/default/app-path _build/default/app/local-path
  .
  ./app
  .
  $ node _build/default/main.js
  hello
  $ node _build/default/dist/other.js
  explicit target
  $ node _build/default/app/main.js
  generated nested
  $ test ! -e app/main.js

Qualified groups preserve the physical source layout too.

  $ mkdir -p qualified/child
  $ cat >qualified/dune <<EOF
  > (include_subdirs qualified)
  > (melange.emit
  >  (emit_stdlib false))
  > EOF
  $ cat >qualified/child/value.ml <<EOF
  > let message = "qualified"
  > EOF
  $ cat >qualified/main.ml <<EOF
  > let () = Js.log Child.Value.message
  > EOF
  $ dune build qualified/main.js qualified/child/value.js
  $ test -f _build/default/qualified/child/value.js
  $ node _build/default/qualified/main.js
  qualified

Runtime assets already occupy the right build-tree location. They must be used
as dependencies, not copied onto themselves.

  $ mkdir -p runtime/data
  $ cat >runtime/data/message.js <<EOF
  > exports.message = "runtime asset";
  > EOF
  $ cat >runtime/main.ml <<EOF
  > external message : string = "message" [@@mel.module "./data/message.js"]
  > let () = Js.log message
  > EOF
  $ cat >runtime/dune <<EOF
  > (melange.emit
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps (glob_files data/*.js)))
  > EOF
  $ dune build @runtime/melange
  $ node _build/default/runtime/main.js
  runtime asset
  $ test ! -e runtime/main.js

Promotion is optional and requires no output-directory relocation. The same
relative asset import works in the checkout after promoting the JS.

  $ cat >runtime/dune <<EOF
  > (melange.emit
  >  (emit_stdlib false)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps (glob_files data/*.js))
  >  (promote))
  > EOF
  $ dune build @runtime/melange
  $ node runtime/main.js
  runtime asset
  $ cat runtime/data/message.js
  exports.message = "runtime asset";

Disabling an emit removes previously generated colocated JS.

  $ mkdir disabled
  $ cat >disabled/dune <<EOF
  > (melange.emit
  >  (enabled_if true)
  >  (emit_stdlib false))
  > EOF
  $ cp main.ml disabled/main.ml
  $ dune build @all
  $ node _build/default/disabled/main.js
  hello
  $ cat >disabled/dune <<EOF
  > (melange.emit
  >  (enabled_if false)
  >  (emit_stdlib false))
  > EOF
  $ dune build @all
  $ test ! -e _build/default/disabled/main.js

Only one emit in a directory may omit its target.

  $ mkdir conflicts
  $ cp dune-project conflicts/dune-project
  $ cat >conflicts/dune <<EOF
  > (melange.emit (emit_stdlib false))
  > (melange.emit (emit_stdlib false))
  > EOF
  $ (cd conflicts && dune build @melange)
  File "dune", line 2, characters 0-34:
  2 | (melange.emit (emit_stdlib false))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: A melange.emit stanza without a target appears for the second time in
  this directory
  [1]

Omitting the target requires Dune 3.25.

  $ mkdir old-language
  $ cat >old-language/dune-project <<EOF
  > (lang dune 3.24)
  > (using melange 1.0)
  > EOF
  $ cat >old-language/dune <<EOF
  > (melange.emit (emit_stdlib false))
  > EOF
  $ (cd old-language && dune build @melange)
  File "dune", line 1, characters 0-34:
  1 | (melange.emit (emit_stdlib false))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Omitting the target field in a melange.emit stanza is only available
  since version 3.25 of the dune language. Please update your dune-project file
  to have (lang dune 3.25).
  [1]

A targetless emit must not overwrite handwritten JavaScript.

  $ cat >conflicts/dune <<EOF
  > (melange.emit (emit_stdlib false))
  > EOF
  $ cp main.ml conflicts/main.ml
  $ cat >conflicts/main.js <<EOF
  > console.log("handwritten");
  > EOF
  $ (cd conflicts && dune build main.js)
  Error: Multiple rules generated for _build/default/main.js:
  - dune:1
  - file present in source tree
  Hint: rm -f main.js
  [1]
  $ cat conflicts/main.js
  console.log("handwritten");

Dynamically generated emits retain support for explicit target directories.

  $ mkdir -p dynamic/generated dynamic/app
  $ cp dune-project dynamic/dune-project
  $ cat >dynamic/generated/dune <<EOF
  > (rule
  >  (target emit.inc)
  >  (action
  >   (write-file %{target}
  >    "(melange.emit (target dist) (emit_stdlib false))")))
  > EOF
  $ cat >dynamic/app/dune <<EOF
  > (dynamic_include ../generated/emit.inc)
  > EOF
  $ cp main.ml dynamic/app/main.ml
  $ (cd dynamic && dune build app/dist/app/main.js)
  $ node dynamic/_build/default/app/dist/app/main.js
  hello

Omitting the target in a dynamic include is rejected with an explicit diagnostic.

  $ cat >dynamic/generated/dune <<EOF
  > (rule
  >  (target emit.inc)
  >  (action
  >   (write-file %{target} "(melange.emit (emit_stdlib false))")))
  > EOF
  $ (cd dynamic && dune build app/main.js)
  File "_build/default/generated/emit.inc", line 1, characters 0-34:
  1 | (melange.emit (emit_stdlib false))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Dynamically generated melange.emit stanzas require a target field.
  Hint: Declare the stanza in a dune file instead of using dynamic_include.
  [1]
