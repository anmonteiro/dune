  $ cat > dune-project <<EOF
  > (lang dune 3.13)
  > (package (name my-ppx))
  > (package (name foo))
  > (package (name mel-foo) (allow_empty))
  > (using melange 0.1)
  > EOF

Define a runtime library that the ppx will depend on via `(ppx_runtime_libraries)`

  $ mkdir runtime
  $ cat > runtime/dune <<EOF
  > (library
  >  (name runtime)
  >  (public_name my-ppx.runtime)
  >  (modules runtime)
  >  (modes melange))
  > (library
  >  (name native)
  >  (public_name my-ppx.native-runtime)
  >  (modules native))
  > EOF
  $ cat > runtime/runtime.ml <<EOF
  > let msg = "melange runtime"
  > EOF
  $ cat > runtime/native.ml <<EOF
  > let msg = "native runtime"
  > EOF

Define a PPX rewriter that has different runtime libs (native / melange)

  $ mkdir ppx
  $ cat > ppx/dune <<EOF
  > (library
  >  (name my_ppx)
  >  (public_name my-ppx)
  >  (kind ppx_rewriter)
  >  (libraries ppxlib)
  >  (ppx_runtime_libraries my-ppx.native-runtime)
  >  (melange.ppx_runtime_libraries my-ppx.runtime))
  > EOF
  $ cat > ppx/my_ppx.ml <<EOF
  > open Ppxlib
  > let () = Driver.register_transformation "my_ppx"
  > EOF

  $ mkdir src
  $ cat > src/dune <<EOF
  > (melange.emit
  >  (package mel-foo)
  >  (target js-out)
  >  (preprocess (pps my-ppx))
  >  (emit_stdlib false))
  > EOF
  $ cat > src/app.ml <<EOF
  > let () = Js.log Runtime.msg
  > EOF

  $ dune build --display=short @src/melange
        ocamlc ppx/.my_ppx.objs/byte/my_ppx.{cmi,cmo,cmt}
          melc runtime/.runtime.objs/melange/runtime.{cmi,cmj,cmt}
        ocamlc .ppx/589732a9cd64e9c94b9f031db5d1ea6b/dune__exe___ppx.{cmi,cmo}
      ocamlopt ppx/.my_ppx.objs/native/my_ppx.{cmx,o}
          melc src/js-out/node_modules/my-ppx.runtime/runtime.js
      ocamlopt .ppx/589732a9cd64e9c94b9f031db5d1ea6b/dune__exe___ppx.{cmx,o}
      ocamlopt ppx/my_ppx.{a,cmxa}
      ocamlopt .ppx/589732a9cd64e9c94b9f031db5d1ea6b/ppx.exe
           ppx src/.melange_src/app.pp.ml
          melc src/.js-out.mobjs/melange/melange__App.{cmi,cmj,cmt}
          melc src/js-out/src/app.js
  $ tree -a _build/default/src/js-out
  _build/default/src/js-out
  |-- node_modules
  |   `-- my-ppx.runtime
  |       `-- runtime.js
  `-- src
      `-- app.js
  
  4 directories, 2 files
  $ node _build/default/src/js-out/src/app.js
  melange runtime

  $ mkdir bin
  $ cat > bin/dune <<EOF
  > (executable
  >  (public_name app)
  >  (package foo)
  >  (preprocess (pps my-ppx)))
  > EOF
  $ cat > bin/app.ml <<EOF
  > let () = Format.eprintf "%s" Native.msg
  > EOF

  $ dune exec --display=short bin/app.exe
        ocamlc runtime/.native.objs/byte/native.{cmi,cmo,cmt}
           ppx bin/app.pp.ml
        ocamlc bin/.app.eobjs/byte/dune__exe__App.{cmi,cmti}
      ocamlopt runtime/.native.objs/native/native.{cmx,o}
      ocamlopt bin/.app.eobjs/native/dune__exe__App.{cmx,o}
      ocamlopt runtime/native.{a,cmxa}
      ocamlopt bin/app.exe
  native runtime

