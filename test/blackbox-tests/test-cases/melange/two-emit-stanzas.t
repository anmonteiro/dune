Building a project with 2 melange.emit stanzas should add rules to both aliases

  $ make_melange_project 3.8 0.1
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist)
  >  (alias mel)
  >  (promote (until-clean))
  >  (modules)
  >  (emit_stdlib false)
  >  (module_systems
  >   (commonjs js)))
  > 
  > (melange.emit
  >  (target dist-es6)
  >  (alias second)
  >  (promote (until-clean))
  >  (emit_stdlib false)
  >  (modules)
  >  (module_systems
  >   (es6 mjs)))
  > EOF

  $ dune build @mel
  $ dune build @second

A library source can be emitted by several enabled stanzas, each with its own
module systems and promotion destination.

  $ make_melange_project 3.25 1.0
  $ mkdir lib
  $ cat > lib/dune <<EOF
  > (library
  >  (name shared)
  >  (wrapped false)
  >  (modes melange))
  > EOF
  $ cat > lib/shared.ml <<EOF
  > let message = "shared"
  > EOF
  $ cat > entry.ml <<EOF
  > let message = Shared.message
  > EOF
  $ cat > dune <<EOF
  > (melange.emit
  >  (target dist-one)
  >  (modules entry)
  >  (libraries shared)
  >  (emit_stdlib false)
  >  (promote (until-clean) (into promoted-one))
  >  (module_systems
  >   (commonjs js)))
  > (melange.emit
  >  (target dist-two)
  >  (modules)
  >  (libraries shared)
  >  (emit_stdlib false)
  >  (promote (until-clean) (into promoted-two))
  >  (module_systems
  >   (commonjs cjs)
  >   (esm mjs)))
  > (melange.emit
  >  (target dist-unpromoted)
  >  (modules)
  >  (libraries shared)
  >  (emit_stdlib false)
  >  (module_systems
  >   (commonjs js)))
  > (melange.emit
  >  (target ignored)
  >  (modules)
  >  (libraries shared)
  >  (emit_stdlib false)
  >  (enabled_if false))
  > EOF

The command does not exist yet.

  $ dune describe melange-outputs lib/shared.ml >/dev/null 2>&1
  [1]
