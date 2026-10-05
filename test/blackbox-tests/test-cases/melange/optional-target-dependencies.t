Targetless emits place private-library outputs beside their source copies, even
for libraries outside the emit directory. Public-library outputs live in the
context's node_modules, so imports from sibling directories can resolve them.

  $ cat >dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > (package (name fixture))
  > EOF
  $ mkdir -p app/child lib public
  $ cat >public/dune <<EOF
  > (library
  >  (name public)
  >  (public_name fixture.public)
  >  (modes melange)
  >  (melange.runtime_deps asset.txt assets))
  > (rule
  >  (target (dir assets))
  >  (action
  >   (progn
  >    (run mkdir %{target})
  >    (no-infer
  >     (write-file %{target}/generated.txt "generated asset\n")))))
  > EOF
  $ cat >public/public.ml <<EOF
  > let message = "public"
  > EOF
  $ cat >public/asset.txt <<EOF
  > source asset
  > EOF
  $ cat >lib/dune <<EOF
  > (library
  >  (name private)
  >  (libraries fixture.public)
  >  (modes melange))
  > EOF
  $ cat >lib/private.ml <<EOF
  > let message = Public.message
  > EOF
  $ cat >app/child/dune <<EOF
  > (library
  >  (name child)
  >  (modes melange))
  > EOF
  $ cat >app/child/child.ml <<EOF
  > let message = "child"
  > EOF
  $ cat >app/dune <<EOF
  > (melange.emit
  >  (libraries private child)
  >  (emit_stdlib false)
  >  (module_systems commonjs (esm mjs)))
  > EOF
  $ cat >app/main.ml <<EOF
  > let () = Js.log (Private.message ^ " " ^ Child.message)
  > EOF

Dependency outputs can be requested directly, before loading the emit's alias.
This includes public-library runtime files and generated directory targets.

  $ dune build lib/private.js node_modules/fixture.public/public.js
  $ test -f _build/default/lib/private.js
  $ test -f _build/default/node_modules/fixture.public/public.js
  $ dune build node_modules/fixture.public/asset.txt \
  >   node_modules/fixture.public/assets/generated.txt
  $ cat _build/default/node_modules/fixture.public/asset.txt
  source asset
  $ cat _build/default/node_modules/fixture.public/assets/generated.txt
  generated asset
  $ dune build @app/melange
  $ test -f _build/default/app/child/child.js
  $ test ! -e _build/default/app/node_modules

Both CommonJS and ESM imports resolve against the same physical layout.

  $ node _build/default/app/main.js
  public child
  $ node _build/default/app/main.mjs
  public child

Installed libraries use that layout too. Default stdlib emission also works
without an explicit target directory.

  $ dune build @install
  $ dune install --prefix "$PWD/prefix" --display=quiet
  $ mkdir consumer
  $ cat >consumer/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ cat >consumer/dune <<EOF
  > (melange.emit
  >  (libraries fixture.public))
  > EOF
  $ cat >consumer/main.ml <<EOF
  > let () = Js.log (String.uppercase_ascii Public.message)
  > EOF
  $ OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" dune build --root consumer @melange
  $ test -f consumer/_build/default/node_modules/fixture.public/public.js
  $ cat consumer/_build/default/node_modules/fixture.public/asset.txt
  source asset
  $ cat consumer/_build/default/node_modules/fixture.public/assets/generated.txt
  generated asset
  $ node consumer/_build/default/main.js
  PUBLIC

Public workspace libraries may already live in node_modules. Their runtime
assets then occupy the targetless output location and must not be copied onto
themselves. An explicit target still needs to copy these assets.

  $ mkdir -p in-place/node_modules/pkg
  $ cat >in-place/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > (package (name pkg))
  > EOF
  $ cat >in-place/node_modules/pkg/dune <<EOF
  > (library
  >  (public_name pkg)
  >  (modes melange)
  >  (melange.runtime_deps asset.txt))
  > EOF
  $ cat >in-place/node_modules/pkg/pkg.ml <<EOF
  > let message = "package"
  > EOF
  $ cat >in-place/node_modules/pkg/asset.txt <<EOF
  > source asset
  > EOF
  $ cat >in-place/main.ml <<EOF
  > let () = Js.log Pkg.message
  > EOF
  $ cat >in-place/dune <<EOF
  > (subdir node_modules (dirs pkg))
  > (melange.emit
  >  (target dist)
  >  (libraries pkg)
  >  (emit_stdlib false))
  > EOF
  $ (cd in-place && dune build @melange)
  $ node in-place/_build/default/dist/main.js
  package
  $ cat in-place/_build/default/dist/node_modules/pkg/asset.txt
  source asset

The targetless form currently conflicts with the ordinary source-file copy.

  $ cat >in-place/dune <<EOF
  > (subdir node_modules (dirs pkg))
  > (melange.emit
  >  (libraries pkg)
  >  (emit_stdlib false))
  > EOF
  $ (cd in-place && dune build @melange)
  Error: Multiple rules generated for
  _build/default/node_modules/pkg/asset.txt:
  - dune:2
  - file present in source tree
  -> required by alias melange
  Hint: rm -f node_modules/pkg/asset.txt
  [1]
  $ cat in-place/node_modules/pkg/asset.txt
  source asset

A generated directory asset already at its destination must keep its original
producer too. Test it separately so the file conflict cannot hide this case.

  $ cat >in-place/node_modules/pkg/dune <<EOF
  > (library
  >  (public_name pkg)
  >  (modes melange)
  >  (melange.runtime_deps assets))
  > (rule
  >  (target (dir assets))
  >  (action
  >   (progn
  >    (run mkdir %{target})
  >    (no-infer
  >     (write-file %{target}/generated.txt "generated asset\n")))))
  > EOF
  $ cat >in-place/dune <<EOF
  > (subdir node_modules (dirs pkg))
  > (melange.emit
  >  (target dist)
  >  (libraries pkg)
  >  (emit_stdlib false))
  > EOF
  $ (cd in-place && dune build @melange)
  $ cat in-place/_build/default/dist/node_modules/pkg/assets/generated.txt
  generated asset
  $ cat >in-place/dune <<EOF
  > (subdir node_modules (dirs pkg))
  > (melange.emit
  >  (libraries pkg)
  >  (emit_stdlib false))
  > EOF
  $ (cd in-place && dune build @melange) >in-place/directory.log 2>&1
  [1]
  $ grep -q assets in-place/directory.log

Relocating a targetless emit must also preserve imports from private libraries
outside the emit directory. First check their unpromoted layout.

  $ mkdir -p private-promotion/app private-promotion/lib
  $ cat >private-promotion/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ cat >private-promotion/lib/dune <<EOF
  > (library
  >  (name helper)
  >  (modes melange))
  > EOF
  $ cat >private-promotion/lib/helper.ml <<EOF
  > let message () = "private"
  > let () = Js.log "loaded private"
  > EOF
  $ cat >private-promotion/app/main.ml <<EOF
  > let () = Js.log (Helper.message ())
  > EOF
  $ cat >private-promotion/app/dune <<EOF
  > (melange.emit
  >  (libraries helper)
  >  (emit_stdlib false))
  > EOF
  $ (cd private-promotion && dune build @app/melange)
  $ node private-promotion/_build/default/app/main.js
  loaded private
  private
  $ cat >private-promotion/app/dune <<EOF
  > (melange.emit
  >  (libraries helper)
  >  (emit_stdlib false)
  >  (promote (into ../relocated/app)))
  > EOF
  $ (cd private-promotion && dune build @app/melange)
  $ find private-promotion/relocated -name '*.js' | sort
  private-promotion/relocated/app/helper.js
  private-promotion/relocated/app/main.js
  $ node private-promotion/relocated/app/main.js 2>private-promotion/node.stderr
  [1]
  $ grep 'Cannot find module' private-promotion/node.stderr
  Error: Cannot find module '../lib/helper.js'
