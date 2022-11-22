Test melange libs flow when using `modules_without_implementation` stanza

Build js files
  $ dune build --debug-load-dir @dist --display=short
  $ node _build/default/output/output/b.js
  buy it
