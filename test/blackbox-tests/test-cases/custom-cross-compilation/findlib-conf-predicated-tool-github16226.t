Reproduce #16226. A tool defined only for a Findlib toolchain causes Dune to
interpret its value as the empty string when initializing the default context.

  $ make_dune_project 2.7
  $ mkdir -p findlib.conf.d
  $ export OCAMLFIND_CONF=$PWD/findlib.conf
  $ cat >findlib.conf.d/solo5.conf <<EOF
  > ocamlmklib(solo5) = "/does/not/matter"
  > EOF
  $ cat >dune <<EOF
  > (library (name foo))
  > EOF
  $ touch foo.ml

The base configuration file does not need to exist for Dune to load snippets
from the corresponding .d directory.

  $ dune build foo.cma 2>&1 | grep 'Internal error'
  Internal error! Please report to https://github.com/ocaml/dune/issues,
  [1]
