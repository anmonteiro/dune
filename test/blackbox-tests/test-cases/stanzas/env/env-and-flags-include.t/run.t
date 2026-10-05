Reproduction case for #1508: make sure that paths in `env` stanzas are
interpreted relative to the directory of the `env` stanza.

  $ dune printenv . --field flags
  (flags (-from-included-file))
  $ dune printenv src --field flags
  (flags (-from-included-file))

Compiler actions retain their declared outputs and dependencies when assembled
later. Requesting an implicit interface or an annotation must still select its
compiler rule, and an included flag file must invalidate the compiled actions.

  $ mkdir compiled
  $ cd compiled
  $ cat >dune-project <<EOF
  > (lang dune 3.17)
  > EOF
  $ cat >dune <<EOF
  > (env (_ (ocamlc_flags (:standard (:include flags.sexp)))))
  > (library
  >  (name example)
  >  (wrapped false)
  >  (modules first second signature)
  >  (modules_without_implementation signature))
  > (executable
  >  (name main)
  >  (modules main)
  >  (modes byte)
  >  (libraries example))
  > EOF
  $ echo '()' > flags.sexp
  $ echo 'let value = 42' > first.ml
  $ echo 'let value = First.value' > second.ml
  $ echo 'val value : int' > second.mli
  $ echo 'val value : int' > signature.mli
  $ cat >main.ml <<EOF
  > let () =
  >   assert (Second.value = 42);
  >   Printf.printf "%d\n" Second.value
  > EOF
  $ dune build .example.objs/byte/first.cmi .example.objs/byte/second.cmti .example.objs/byte/signature.cmti
  $ find _build/default/.example.objs/byte -name '*.cm*' | sort
  _build/default/.example.objs/byte/first.cmi
  _build/default/.example.objs/byte/first.cmo
  _build/default/.example.objs/byte/first.cmt
  _build/default/.example.objs/byte/second.cmi
  _build/default/.example.objs/byte/second.cmti
  _build/default/.example.objs/byte/signature.cmi
  _build/default/.example.objs/byte/signature.cmti
  $ dune exec ./main.bc
  42

Both the implementation dependency and the included flags take effect without
cleaning the build. In particular, the assertion would fail without -noassert.

  $ echo 'let value = 43' > first.ml
  $ echo '(-noassert)' > flags.sexp
  $ dune exec ./main.bc
  43
