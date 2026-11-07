The bytecode linker normally searches its working directory before any -I
directory. On OCaml 5 and later, -nocwd allows the selected DLLs to take
precedence without changing the working directory. See #9979.

  $ mkdir -p producer app relative-stubs
  $ (cd producer && make_dune_project_with_package 3.13 pkg)
  $ cat >producer/dune <<'EOF'
  > (library
  >  (name b)
  >  (public_name pkg.b)
  >  (foreign_stubs (language c) (names b_stubs)))
  > EOF
  $ echo 'external value : unit -> int = "selected_b"' >producer/b.ml
  $ cat >producer/b_stubs.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value selected_b(value unit) { return Val_int(2); }
  > EOF
  $ dune build --root producer @install
  $ dune install --root producer --prefix "$PWD/prefix" 2>/dev/null
  $ export OCAMLPATH="$PWD/prefix/lib"
  $ ext_dll=$(ocamlc -config-var ext_dll)

The root library is built separately and is not a dependency of main.

  $ make_dune_project 3.13
  $ cat >dune <<'EOF'
  > (data_only_dirs producer prefix)
  > (library
  >  (name b)
  >  (modules b)
  >  (foreign_stubs (language c) (names unrelated)))
  > EOF
  $ touch b.ml
  $ cat >unrelated.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value unrelated_b(value unit) { return Val_int(0); }
  > EOF
  $ dune build dllb_stubs$ext_dll

The executable also uses an explicit relative -I flag. Changing the linker's
working directory must preserve the meaning of this flag.

  $ cat >relative-stubs/dune <<'EOF'
  > (foreign_library
  >  (archive_name relative)
  >  (language c)
  >  (names relative))
  > EOF
  $ cat >relative-stubs/relative.c <<'EOF'
  > #include <caml/mlvalues.h>
  > CAMLprim value relative_value(value unit) { return Val_int(3); }
  > EOF
  $ cat >app/dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte)
  >  (libraries pkg.b)
  >  (link_deps ../relative-stubs/dllrelative%{ext_dll})
  >  (link_flags (:standard -I relative-stubs -dllib -lrelative)))
  > EOF
  $ cat >app/main.ml <<'EOF'
  > external relative : unit -> int = "relative_value"
  > let () = Printf.printf "%d %d\n" (B.value ()) (relative ())
  > EOF

OCaml 4 does not support -nocwd and retains its working-directory collision.
Check that limitation before removing the unrelated DLL for the remaining
compatibility checks on that compiler.

  $ if [ "$(ocamlc -version | cut -d. -f1)" -lt 5 ]; then
  >   dune build app/main.bc --sandbox=none >old-compiler.log 2>&1
  >   test $? -eq 1 || exit 1
  >   grep -q 'selected_b.*not available' old-compiler.log || exit 1
  >   rm _build/default/dllb_stubs$ext_dll
  > fi

DLL selection works without sandboxing.

  $ dune build app/main.bc --sandbox=none

An explicit sandbox constraint is also respected for executables using stubs.

  $ sed 's/(link_deps /(link_deps (sandbox none) /' app/dune >app/dune.new
  $ mv app/dune.new app/dune
  $ dune build app/main.bc

  $ sed 's/(sandbox none) //' app/dune >app/dune.new
  $ mv app/dune.new app/dune

Removing the unrelated DLL does not change the successful result.

  $ rm -f _build/default/dllb_stubs$ext_dll
  $ dune build app/main.bc --sandbox=none

The same sandbox constraint works when the bytecode executable has no stubs.

  $ cat >app/dune <<'EOF'
  > (executable
  >  (name plain)
  >  (modules plain)
  >  (modes byte exe)
  >  (link_deps (sandbox none)))
  > EOF
  $ echo 'let () = ()' >app/plain.ml
  $ dune build app/plain.bc

Native linking still accepts the same sandbox constraint.

  $ dune build app/plain.exe
