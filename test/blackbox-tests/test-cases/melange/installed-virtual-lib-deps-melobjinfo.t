When melobjinfo is available, Dune should use CMIs and CMJs to analyze an
installed Melange virtual library precisely.

  $ mkdir -p producer/vlib consumer/impl fake-bin

  $ cat > producer/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name repro))
  > EOF
  $ cat > producer/vlib/dune <<'EOF'
  > (library
  >  (name vlib)
  >  (public_name repro.vlib)
  >  (modes melange)
  >  (private_modules helper leaf type_only type_runtime unused)
  >  (virtual_modules virt other))
  > EOF
  $ cat > producer/vlib/virt.mli <<'EOF'
  > val run : unit -> int
  > EOF
  $ cat > producer/vlib/other.mli <<'EOF'
  > val run : unit -> int
  > EOF
  $ cat > producer/vlib/shared.ml <<'EOF'
  > type t = Type_only.t
  > let answer = Helper.answer + Other.run ()
  > EOF
  $ cat > producer/vlib/helper.ml <<'EOF'
  > let answer = Leaf.answer
  > EOF
  $ cat > producer/vlib/helper.mli <<'EOF'
  > val answer : int
  > EOF
  $ cat > producer/vlib/leaf.ml <<'EOF'
  > let answer = 42
  > EOF
  $ cat > producer/vlib/type_only.ml <<'EOF'
  > type t = int
  > let ignored = Type_runtime.value
  > EOF
  $ cat > producer/vlib/type_only.mli <<'EOF'
  > type t = int
  > val ignored : int
  > EOF
  $ cat > producer/vlib/type_runtime.ml <<'EOF'
  > let value = 0
  > EOF
  $ cat > producer/vlib/unused.ml <<'EOF'
  > let ignored = 0
  > EOF

  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix"

The fake follows melobjinfo's ocamlobjinfo-compatible output format. Every
input gets an output block so Dune can associate results positionally.

  $ cat > fake-bin/melobjinfo <<'EOF'
  > #!/bin/sh
  > set -eu
  > seen_shared=false
  > seen_helper=false
  > seen_type_only=false
  > for unit do
  >   printf 'File %s\n' "$unit"
  >   printf 'Implementations imported:\n'
  >   case "$unit" in
  >     *vlib__Shared.cmj)
  >       seen_shared=true
  >       printf '  --------------------------------  Vlib__Helper\n'
  >       printf '  --------------------------------  Vlib__Other\n'
  >       ;;
  >     *vlib__Helper.cmj)
  >       seen_helper=true
  >       printf '  --------------------------------  Vlib__Leaf\n'
  >       ;;
  >     *vlib__Type_only.cmj)
  >       seen_type_only=true
  >       printf '  --------------------------------  Vlib__Type_runtime\n'
  >       ;;
  >   esac
  > done
  > test "$seen_shared" = true
  > test "$seen_helper" = true
  > test "$seen_type_only" = true
  > EOF
  $ chmod +x fake-bin/melobjinfo

  $ cat > consumer/dune-project <<'EOF'
  > (lang dune 3.24)
  > (using melange 1.0)
  > (package (name consumer))
  > EOF
  $ cat > consumer/impl/dune <<'EOF'
  > (library
  >  (name impl)
  >  (public_name consumer.impl)
  >  (modes melange)
  >  (implements repro.vlib))
  > EOF
  $ cat > consumer/impl/virt.ml <<'EOF'
  > let _coerce (x : Shared.t) : int = x
  > let run () = Shared.answer
  > EOF
  $ cat > consumer/impl/other.ml <<'EOF'
  > let run () = 1
  > EOF
  $ cat > consumer/dune <<'EOF'
  > (melange.emit
  >  (target output)
  >  (emit_stdlib false)
  >  (compile_flags :standard --mel-cross-module-opt)
  >  (libraries impl))
  > EOF

melobjinfo is run once for all CMJs. An implementation edge to Helper follows
Helper's private Leaf dependency, while an interface edge to Type_only does not
follow its implementation dependency on Type_runtime.

  $ PATH="$PWD/fake-bin:$PATH" \
  > OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune build --root consumer --sandbox=symlink --trace-file "$PWD/trace" @melange
  $ dune trace cat --trace-file "$PWD/trace" \
  > | jq_dune -s \
  >   '[.[] | processesBrief | select(.prog == "melobjinfo")] | length'
  1

  $ PATH="$PWD/fake-bin:$PATH" \
  > OCAMLPATH="$PWD/prefix/lib:$OCAMLPATH" \
  > dune rules --root consumer --recursive --format=json --deps --display=quiet \
  > impl/.impl.objs/melange/vlib__Virt.cmj > deps.json
  $ jq_dune -r '
  >   [.[] | depsFilePaths
  >    | select(endswith("vlib__Helper.cmi")
  >             or endswith("vlib__Helper.cmj")
  >             or endswith("vlib__Leaf.cmi")
  >             or endswith("vlib__Leaf.cmj")
  >             or endswith("vlib__Other.cmi")
  >             or endswith("vlib__Other.cmj")
  >             or endswith("vlib__Type_only.cmi")
  >             or endswith("vlib__Type_only.cmj")
  >             or endswith("vlib__Type_runtime.cmi")
  >             or endswith("vlib__Type_runtime.cmj")
  >             or endswith("vlib__Unused.cmi")
  >             or endswith("vlib__Unused.cmj")
  >             or endswith("vlib__Virt.cmi")
  >             or endswith("vlib__Virt.cmj"))
  >    | select(startswith("_build/default/impl/.impl.objs/melange/"))]
  >   | unique[]
  > ' deps.json
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmi
  _build/default/impl/.impl.objs/melange/vlib__Helper.cmj
  _build/default/impl/.impl.objs/melange/vlib__Leaf.cmi
  _build/default/impl/.impl.objs/melange/vlib__Leaf.cmj
  _build/default/impl/.impl.objs/melange/vlib__Other.cmi
  _build/default/impl/.impl.objs/melange/vlib__Other.cmj
  _build/default/impl/.impl.objs/melange/vlib__Type_only.cmi
  _build/default/impl/.impl.objs/melange/vlib__Type_only.cmj
  _build/default/impl/.impl.objs/melange/vlib__Virt.cmi
