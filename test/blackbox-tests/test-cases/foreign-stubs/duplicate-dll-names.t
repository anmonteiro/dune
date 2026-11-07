Two OCaml libraries may share one foreign archive. This is different from
requiring distinct DLLs with the same basename, which cannot both be selected
by a single directory search order. See #9979.

  $ make_dune_project 3.13
  $ ext_dll=$(ocamlc -config-var ext_dll)
  $ mkdir a b common
  $ cat >common/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > EOF
  $ cat >common/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(1); }
  > CAMLprim value b_value(value unit) { return Val_int(2); }
  > EOF
  $ cat >a/dune <<'EOF'
  > (library
  >  (name a)
  >  (foreign_archives ../common/shared))
  > EOF
  $ cat >b/dune <<'EOF'
  > (library
  >  (name b)
  >  (foreign_archives ../common/shared))
  > EOF
  $ echo 'external value : unit -> int = "a_value"' >a/a.ml
  $ echo 'external value : unit -> int = "b_value"' >b/b.ml
  $ cat >dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries a b))
  > EOF
  $ cat >main.ml <<'EOF'
  > let () = Printf.printf "%d %d\n" (A.value ()) (B.value ())
  > EOF

Repeated references to the same archive must remain valid.

  $ dune build main.bc
  $ ocamlrun -I _build/default/common _build/default/main.bc
  1 2

Now give each library its own DLL with the same basename. Both libraries are
dependencies of main; neither DLL is an unrelated build artifact.

  $ cat >a/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > (library (name a) (modules a) (foreign_archives shared))
  > EOF
  $ cat >b/dune <<'EOF'
  > (foreign_library (archive_name shared) (language c) (names stubs))
  > (library (name b) (modules b) (foreign_archives shared))
  > EOF
  $ cat >a/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value a_value(value unit) { return Val_int(1); }
  > EOF
  $ cat >b/stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value b_value(value unit) { return Val_int(2); }
  > EOF
  $ dune build main.bc >error 2>&1
  [1]
  $ sed "s/$ext_dll/.EXT_DLL/g" error
  File "dune", line 2, characters 7-11:
  2 |  (name main)
             ^^^^
  Error: Conflicting bytecode DLLs named "dllshared.EXT_DLL":
  - _build/default/a/dllshared.EXT_DLL
  - _build/default/b/dllshared.EXT_DLL

Reversing the order reports the same conflicting DLL providers.

  $ sed 's/(libraries a b)/(libraries b a)/' dune > dune.new
  $ mv dune.new dune
  $ dune build main.bc >error 2>&1
  [1]
  $ sed "s/$ext_dll/.EXT_DLL/g" error
  File "dune", line 2, characters 7-11:
  2 |  (name main)
             ^^^^
  Error: Conflicting bytecode DLLs named "dllshared.EXT_DLL":
  - _build/default/a/dllshared.EXT_DLL
  - _build/default/b/dllshared.EXT_DLL

Creating a bytecode plugin does not resolve the DLLs at link time. It must
remain possible to archive these libraries together.

  $ cat >dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes (byte plugin))
  >  (libraries a b)
  >  (embed_in_plugin_libraries a b))
  > EOF
  $ dune build main.cma
