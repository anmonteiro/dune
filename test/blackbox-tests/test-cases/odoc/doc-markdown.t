  $ cat > dune-project << EOF
  > (lang dune 3.10)
  > 
  > (package
  >  (name mylib))
  > EOF

  $ cat > dune << EOF
  > (library
  >  (public_name mylib))
  > EOF

  $ cat > mylib.ml << EOF
  > (** This is the main module for mylib *)
  > 
  > (** A simple type definition *)
  > type t = int
  > 
  > (** A function that adds one *)
  > val add_one : int -> int
  > let add_one x = x + 1
  > 
  > module SubModule = struct
  >   (** A nested module *)
  >   type nested = string
  > end
  > EOF

  $ cat > mylib.mli << EOF
  > (** This is the main module for mylib *)
  > 
  > (** A simple type definition *)
  > type t = int
  > 
  > (** A function that adds one *)
  > val add_one : int -> int
  > 
  > module SubModule : sig
  >   (** A nested module *)
  >   type nested = string
  > end
  > EOF

  $ list_markdown_docs () {
  >   find _build/default/_doc/_markdown -name '*.md' | sort
  > }

Build markdown documentation:

  $ dune build @doc-markdown
  $ list_markdown_docs
  _build/default/_doc/_markdown/index.md
  _build/default/_doc/_markdown/mylib/Mylib-SubModule.md
  _build/default/_doc/_markdown/mylib/Mylib.md
  _build/default/_doc/_markdown/mylib/index.md

Check the top-level index contains markdown:

  $ cat _build/default/_doc/_markdown/index.md
  # OCaml Package Documentation
  
  - [mylib](mylib/index.md)

A package's Markdown producer must not discover modules in other packages.

  $ cat >dune-project <<EOF
  > (lang dune 3.25)
  > (package (name mylib))
  > (package (name other))
  > EOF
  $ mkdir other
  $ cat >other/dune <<EOF
  > (library
  >  (public_name other)
  >  (modules (:include missing.list)))
  > EOF
  $ DUNE_TRACE=debug dune build _doc/_markdown/mylib
  $ dune trace cat | jq -sr '[.[] | select(.name == "rule_generated") | .args.target_dirs[]? | select(contains("/_doc/_markdown/"))] | unique[]'
  _build/default/_doc/_markdown/mylib
  $ dune build _doc/_markdown/other
  Error: No rule found for other/missing.list
  -> required by (:include _build/default/other/missing.list) at other/dune:3
  -> required by (modules) field at other/dune:1
  [1]
