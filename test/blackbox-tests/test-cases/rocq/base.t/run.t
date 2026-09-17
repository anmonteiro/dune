  $ dune build --display short --debug-dependency-path @all --always-show-command-line
          rocq .basic.theory.d
          rocq foo.{glob,vo}
          rocq bar.{glob,vo}

  $ dune build --debug-dependency-path @default
  lib: [
    "_build/install/default/lib/base/META"
    "_build/install/default/lib/base/dune-package"
    "_build/install/default/lib/base/opam"
  ]
  lib_root: [
    "_build/install/default/lib/coq/user-contrib/basic/bar.glob" {"coq/user-contrib/basic/bar.glob"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.v" {"coq/user-contrib/basic/bar.v"}
    "_build/install/default/lib/coq/user-contrib/basic/bar.vo" {"coq/user-contrib/basic/bar.vo"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.glob" {"coq/user-contrib/basic/foo.glob"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.v" {"coq/user-contrib/basic/foo.v"}
    "_build/install/default/lib/coq/user-contrib/basic/foo.vo" {"coq/user-contrib/basic/foo.vo"}
  ]

The standard module set includes copied theory sources.

  $ mkdir -p copied-input/inputs
  $ cat >copied-input/inputs/copied.v <<EOF
  > Definition value := 42.
  > EOF
  $ cat >copied-input/dune <<EOF
  > (copy_files inputs/*.v)
  > (rocq.theory
  >  (name copied)
  >  (modules :standard))
  > EOF
  $ dune build copied-input/copied.vo

Qualified groups include both physical and generated sources in a child. Copying
them into the group root must not pull the theory while discovering the inputs.

  $ mkdir -p qualified-input/inputs
  $ cat >qualified-input/dune <<EOF
  > (include_subdirs qualified)
  > (copy_files inputs/*.v)
  > (rocq.theory
  >  (name qualified_copy)
  >  (modules :standard))
  > EOF
  $ cat >qualified-input/inputs/copied.v <<EOF
  > Definition value := 42.
  > EOF
  $ cat >qualified-input/inputs/dune <<EOF
  > (rule
  >  (target generated.v)
  >  (action (write-file %{target} "Definition generated := 7.")))
  > EOF
  $ dune build qualified-input/copied.vo qualified-input/generated.vo \
  >   qualified-input/inputs/copied.vo qualified-input/inputs/generated.vo

Unqualified groups remain unsupported; copied sources must not obscure that
diagnostic with a rule-loading cycle.

  $ cat >qualified-input/dune <<EOF
  > (include_subdirs unqualified)
  > (copy_files inputs/*.v)
  > (rocq.theory
  >  (name qualified_copy)
  >  (modules :standard))
  > EOF
  $ dune build qualified-input/copied.vo
  File "qualified-input/dune", line 1, characters 0-29:
  1 | (include_subdirs unqualified)
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: (include_subdirs unqualified) is not supported yet with (rocq.theory
  ...) stanzas
  [1]
