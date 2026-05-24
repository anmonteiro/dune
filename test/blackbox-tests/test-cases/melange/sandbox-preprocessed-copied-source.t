Melange copies sources into `.melange_src` before preprocessing. Some
compiler versions try to read the copied pre-PPX source from locations emitted
by action preprocessors, so that copied source must be present in the compile
sandbox.

  $ mkdir -p cases/fake-bin cases/melange-stdlib cases/src

  $ cat > cases/fake-bin/melc <<'EOF'
  > #!/bin/sh
  > set -eu
  > script_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
  > if [ "${1:-}" = "--where" ]; then
  >   printf '%s\n' "$script_dir/../melange-stdlib"
  >   exit 0
  > fi
  > out=
  > src=
  > while [ "$#" -gt 0 ]; do
  >   case "$1" in
  >     -o)
  >       shift
  >       out=$1
  >       ;;
  >     -impl|-intf)
  >       shift
  >       src=$1
  >       ;;
  >   esac
  >   shift
  > done
  > if [ -n "$src" ]; then
  >   source=$(sed -n '1s/^# [0-9][0-9]* "\([^"]*\)".*/\1/p' "$src")
  >   if [ -n "$source" ] && [ ! -f "$source" ]; then
  >     echo "File \"$source\", line 1:" >&2
  >     echo "Error: I/O error: $source: No such file or directory" >&2
  >     exit 2
  >   fi
  > fi
  > if [ -n "$out" ]; then
  >   mkdir -p "$(dirname -- "$out")"
  >   : > "$out"
  >   case "$out" in
  >     *.cmi) : > "${out%.cmi}.cmti" ;;
  >     *.cmj) : > "${out%.cmj}.cmt" ;;
  >   esac
  > fi
  > EOF

  $ chmod +x cases/fake-bin/melc
  $ export PATH="$PWD/cases/fake-bin:$PATH"

  $ cat > cases/dune-project <<'EOF'
  > (lang dune 3.18)
  > (using melange 0.1)
  > EOF

  $ cat > cases/src/dune <<'EOF'
  > (library
  >  (name copied_source)
  >  (modes melange)
  >  (wrapped false)
  >  (modules foo)
  >  (preprocess
  >   (action
  >    (run sh -c "printf '# 1 \"%s\"\\n%s\\n' \"$1\" \"$(cat \"$1\")\"" -- %{input-file}))))
  > EOF

  $ cat > cases/src/foo.mli <<'EOF'
  > val x : int
  > EOF

  $ cat > cases/src/foo.ml <<'EOF'
  > let x = 1
  > EOF

  $ dune build --root cases --sandbox=symlink @src/all
