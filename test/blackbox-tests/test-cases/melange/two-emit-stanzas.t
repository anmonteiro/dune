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

The command reports every output, including promoted paths, and ignores disabled
stanzas.

  $ dune describe melange-outputs lib/shared.ml
  (((module_system commonjs)
    (build_path _build/default/dist-one/lib/shared.js)
    (promoted_path (promoted-one/lib/shared.js)))
   ((module_system commonjs)
    (build_path _build/default/dist-two/lib/shared.cjs)
    (promoted_path (promoted-two/lib/shared.cjs)))
   ((module_system es6)
    (build_path _build/default/dist-two/lib/shared.mjs)
    (promoted_path (promoted-two/lib/shared.mjs)))
   ((module_system commonjs)
    (build_path _build/default/dist-unpromoted/lib/shared.js)
    (promoted_path ())))

Entry modules are included too.

  $ dune describe melange-outputs entry.ml
  (((module_system commonjs)
    (build_path _build/default/dist-one/entry.js)
    (promoted_path (promoted-one/entry.js))))
