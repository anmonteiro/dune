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

Without a target, the ordinary source-file copy still tracks asset updates.

  $ cat >in-place/node_modules/pkg/asset.txt <<EOF
  > updated asset
  > EOF
  $ cat >in-place/dune <<EOF
  > (subdir node_modules (dirs pkg))
  > (melange.emit
  >  (libraries pkg)
  >  (emit_stdlib false))
  > EOF
  $ (cd in-place && dune build @melange)
  $ cat in-place/node_modules/pkg/asset.txt
  updated asset
  $ cat in-place/_build/default/node_modules/pkg/asset.txt
  updated asset
  $ node in-place/_build/default/main.js
  package

A generated directory asset already at its destination keeps its original
producer. Changing that producer must update the asset used by the emit.

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
  >     (write-file %{target}/generated.txt "updated generated asset\n")))))
  > EOF
  $ cat >in-place/dune <<EOF
  > (subdir node_modules (dirs pkg))
  > (melange.emit
  >  (libraries pkg)
  >  (emit_stdlib false))
  > EOF
  $ (cd in-place && dune build @melange)
  $ cat in-place/_build/default/node_modules/pkg/assets/generated.txt
  updated generated asset

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
  private-promotion/relocated/app/main.js
  private-promotion/relocated/lib/helper.js
  $ node private-promotion/relocated/app/main.js
  loaded private
  private

Local-library outputs include generated root modules and selected sources.
Qualified selects use paths relative to the library's module-group root for
both the result and its branches.

  $ mkdir -p library-sources/app library-sources/dependency
  $ mkdir -p library-sources/rooted library-sources/selected/sub
  $ cat >library-sources/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > EOF
  $ cat >library-sources/dependency/dune <<EOF
  > (library
  >  (name dependency)
  >  (modes melange))
  > EOF
  $ cat >library-sources/dependency/dependency.ml <<EOF
  > let message () = "root module"
  > EOF
  $ cat >library-sources/rooted/dune <<EOF
  > (library
  >  (name rooted)
  >  (root_module root)
  >  (libraries dependency)
  >  (modes melange))
  > EOF
  $ cat >library-sources/rooted/dependency.ml <<EOF
  > let message () = "shadowed"
  > EOF
  $ cat >library-sources/rooted/rooted.ml <<EOF
  > let message () = Root.Dependency.message ()
  > EOF
  $ cat >library-sources/selected/dune <<EOF
  > (include_subdirs qualified)
  > (library
  >  (name selected)
  >  (modes melange)
  >  (libraries
  >   (select flat.ml from
  >    (-> flat.selected.ml))
  >   (select sub/message.ml from
  >    (-> sub/message.selected.ml))))
  > EOF
  $ cat >library-sources/selected/flat.selected.ml <<EOF
  > let message () = "flat"
  > EOF
  $ cat >library-sources/selected/sub/message.selected.ml <<EOF
  > let message () = "qualified"
  > EOF
  $ cat >library-sources/selected/selected.ml <<EOF
  > let message () = Flat.message () ^ " " ^ Sub.Message.message ()
  > EOF
  $ cat >library-sources/app/dune <<EOF
  > (melange.emit
  >  (libraries rooted selected)
  >  (emit_stdlib false))
  > EOF
  $ cat >library-sources/app/main.ml <<EOF
  > let () = Js.log (Rooted.message ())
  > let () = Js.log (Selected.message ())
  > EOF

Request the generated library outputs before the entry point or emit alias.

  $ (cd library-sources && dune build rooted/.melange_src/root.js)
  $ (cd library-sources && dune build selected/.melange_src/flat.js \
  >   selected/.melange_src/sub/message.js)
  $ (cd library-sources && dune build app/main.js rooted/rooted.js \
  >   dependency/dependency.js selected/selected.js)
  $ node library-sources/_build/default/app/main.js
  root module
  flat qualified

Both private and public virtual libraries can be emitted with implementations
of the same visibility. Private implementations of public virtual libraries
remain unsupported by Melange; that restriction is independent of the target.

  $ mkdir -p virtuals/app virtuals/private-vlib virtuals/private-impl
  $ mkdir -p virtuals/public-vlib virtuals/public-impl
  $ cat >virtuals/dune-project <<EOF
  > (lang dune 3.25)
  > (using melange 1.0)
  > (package (name virtuals))
  > EOF
  $ cat >virtuals/private-vlib/dune <<EOF
  > (library
  >  (name private_vlib)
  >  (modes melange)
  >  (virtual_modules virt))
  > EOF
  $ cat >virtuals/private-vlib/virt.mli <<EOF
  > val message : unit -> string
  > EOF
  $ cat >virtuals/private-vlib/private_vlib.ml <<EOF
  > let message () = Virt.message ()
  > EOF
  $ cat >virtuals/private-impl/dune <<EOF
  > (library
  >  (name private_impl)
  >  (implements private_vlib)
  >  (modes melange))
  > EOF
  $ cat >virtuals/private-impl/virt.ml <<EOF
  > let message () = "private virtual"
  > EOF
  $ cat >virtuals/public-vlib/dune <<EOF
  > (library
  >  (name public_vlib)
  >  (public_name virtuals.vlib)
  >  (modes melange)
  >  (virtual_modules virt))
  > EOF
  $ cat >virtuals/public-vlib/virt.mli <<EOF
  > val message : unit -> string
  > EOF
  $ cat >virtuals/public-vlib/public_vlib.ml <<EOF
  > let message () = Virt.message ()
  > EOF
  $ cat >virtuals/public-impl/dune <<EOF
  > (library
  >  (name public_impl)
  >  (public_name virtuals.impl)
  >  (implements virtuals.vlib)
  >  (modes melange))
  > EOF
  $ cat >virtuals/public-impl/virt.ml <<EOF
  > let message () = "public virtual"
  > EOF
  $ cat >virtuals/app/dune <<EOF
  > (melange.emit
  >  (libraries private_vlib private_impl virtuals.vlib virtuals.impl))
  > EOF
  $ cat >virtuals/app/main.ml <<EOF
  > let () = Js.log (Private_vlib.message ())
  > let () = Js.log (Public_vlib.message ())
  > EOF

Request both the virtual library's concrete modules and its implementation.

  $ (cd virtuals && dune build private-vlib/private_vlib.js private-impl/virt.js)
  $ (cd virtuals && dune build node_modules/virtuals.vlib/public_vlib.js \
  >   node_modules/virtuals.impl/virt.js)
  $ (cd virtuals && dune build app/main.js)
  $ (cd virtuals && dune build @app/melange)
  $ node virtuals/_build/default/app/main.js
  private virtual
  public virtual
