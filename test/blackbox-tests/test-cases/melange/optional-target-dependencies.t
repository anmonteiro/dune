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
