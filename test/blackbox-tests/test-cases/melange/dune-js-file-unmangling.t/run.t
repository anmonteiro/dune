Test file unmangling with melange.emit that depends on a library, that depends on another library

  $ dune build entry_module.js
  $ node _build/default/entry_module.js
  1
