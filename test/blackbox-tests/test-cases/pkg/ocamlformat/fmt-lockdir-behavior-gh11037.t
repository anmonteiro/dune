Formatting does not build the project, whether or not a lockdir is present.

  $ mkrepo
  $ make_project_with_dev_tool_lockdir

Make a fake ocamlformat package that appends a comment to the end of its input.
  $ mkpkg ocamlformat <<EOF
  > install: [
  >   [ "sh" "-c" "echo '#!/bin/sh' > %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'cat \$2' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "echo 'echo \$2 | grep .*.ml >/dev/null && echo \"(* formatted with fake ocamlformat *)\"' >> %{bin}%/ocamlformat" ]
  >   [ "sh" "-c" "chmod a+x %{bin}%/ocamlformat" ]
  > ]
  > EOF

The foo package depends on the bar package.
  $ cat > dune-project <<EOF
  > (lang dune 3.16)
  > (package
  >  (name foo)
  >  (depends bar))
  > EOF

The foo executable depends on the bar library.
  $ cat > dune <<EOF
  > (executable
  >  (public_name foo)
  >  (libraries bar))
  > EOF

Without a .ocamlformat `dune fmt` does nothing.
  $ touch .ocamlformat

Run `dune fmt` before creating a lockdir, and print the file foo.ml before and
after to demonstrate that it was formatted. Note that the package "bar" hasn't
yet been defined, so the fact that `dune fmt` works indicates that dune did not
attempt to build the package "foo".
  $ cat foo.ml
  let () = print_endline "Hello, world"
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  Solution for _build/.dev-tools.locks/ocamlformat:
  - ocamlformat.0.0.1
  File "foo.ml", line 1, characters 0-0:
  --- foo.ml
  +++ foo.ml.corrected
  @@ -1 +1,2 @@
   let () = print_endline "Hello, world"
  +(* formatted with fake ocamlformat *)
  Promoting _build/default/foo.ml.corrected to foo.ml.
  [1]
  $ cat foo.ml
  let () = print_endline "Hello, world"
  (* formatted with fake ocamlformat *)

Create a lockdir and define the package "bar". Note its install command is
`false` so it will fail to install.
  $ make_lockdir
  $ make_lockpkg bar <<EOF
  > (version 0.0.1)
  > (install (run false))
  > EOF

With a lockdir, formatting still does not build the executable or install its
dependency "bar".
  $ DUNE_CONFIG__LOCK_DEV_TOOL=enabled dune fmt
  File "foo.ml", line 1, characters 0-0:
  --- foo.ml
  +++ foo.ml.corrected
  @@ -1,2 +1,3 @@
   let () = print_endline "Hello, world"
   (* formatted with fake ocamlformat *)
  +(* formatted with fake ocamlformat *)
  Promoting _build/default/foo.ml.corrected to foo.ml.
  [1]
