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

  $ dune build @src/melange
  $ find _build/default/src/js-out -type f | sort
  _build/default/src/js-out/node_modules/my-ppx.runtime/runtime.js
  _build/default/src/js-out/src/app.js
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

  $ dune exec bin/app.exe
  native runtime

