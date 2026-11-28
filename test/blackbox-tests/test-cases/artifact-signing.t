Signing after substitution must distinguish linked images from bytecode, scripts
and relocatable object files. Record signing attempts, forwarding linked images
to the real signer so executing them also checks that their signatures are valid.

  $ export REAL_CODESIGN=$(command -v codesign)
  $ export SIGN_LOG="$PWD/signed"
  $ mkdir bin
  $ cat > bin/codesign <<'EOF'
  > #!/bin/sh
  > printf '%s\n' "${4##*/}" >> "$SIGN_LOG"
  > case "$4" in
  >   *.o.dune-temp) exit 0 ;;
  > esac
  > exec "$REAL_CODESIGN" "$@"
  > EOF
  $ chmod +x bin/codesign
  $ export PATH="$PWD/bin:$PATH"

Build ordinary bytecode, native code and bytecode with an embedded runtime.
All three contain a substitution, and are both promoted and installed.

  $ make_dune_project 3.6
  $ touch signing.opam
  $ cat > main.ml <<'EOF'
  > let () =
  >   print_endline
  >     (String.sub
  >        "%%DUNE_PLACEHOLDER:64:repeat:1:5:hello%%%%%%%%%%%%%%%%%%%%%%%%%%"
  >        0 8)
  > EOF
  $ cat > script <<'EOF'
  > #!/bin/sh
  > # %%DUNE_PLACEHOLDER:64:repeat:1:5:hello%%%%%%%%%%%%%%%%%%%%%%%%%%
  > echo script
  > EOF
  $ cat > object.c <<'EOF'
  > const char placeholder[] =
  >   "%%DUNE_PLACEHOLDER:64:repeat:1:5:hello%%%%%%%%%%%%%%%%%%%%%%%%%%";
  > EOF
  $ cc -c object.c -o object.o
  $ cat > dune <<'EOF'
  > (executable
  >  (name main)
  >  (modes byte byte_complete native)
  >  (promote (until-clean)))
  > (rule
  >  (targets promoted.o)
  >  (mode promote)
  >  (deps object.o)
  >  (action
  >   (progn
  >    (copy %{deps} %{targets})
  >    (run chmod +x %{targets}))))
  > (install
  >  (section bin)
  >  (files
  >   (main.exe as native)
  >   (main.bc as bytecode)
  >   (main.bc.exe as custom)
  >   object.o
  >   script))
  > EOF
  $ dune build @install promoted.o
  $ ./main.exe
  =5:hello
  $ ocamlrun main.bc
  =5:hello
  $ ./main.bc.exe
  =5:hello
  $ dune install --prefix _install
  $ _install/bin/native
  =5:hello
  $ ocamlrun _install/bin/bytecode
  =5:hello
  $ _install/bin/custom
  =5:hello
  $ _install/bin/script
  script

The magic-only check incorrectly signs both object files.

  $ grep '\.o\.dune-temp$' signed | sort -u
  .#object.o.dune-temp
  .#promoted.o.dune-temp

Signing failures for linked images must still fail installation.

  $ cat > bin/codesign <<'EOF'
  > #!/bin/sh
  > echo 'signature rejected' >&2
  > exit 17
  > EOF
  $ dune install --prefix _failed 2>error
  [1]
  $ grep 'signature rejected' error | sort -u
  signature rejected
