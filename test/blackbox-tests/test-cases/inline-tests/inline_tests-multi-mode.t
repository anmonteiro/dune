Test running inline tests in multiple modes at once

Reproduction case for #3347

  $ make_dune_project 2.0

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (libraries test_backend)
  >  (modules test)
  >  (inline_tests (modes byte native)))
  > 
  > (library
  >  (name test_backend)
  >  (modules ())
  >  (inline_tests.backend
  >   (generate_runner
  >    (progn
  >     (echo "Printf.eprintf \"Test %s\"\n")
  >     (echo "  (match Sys.backend_type with")
  >     (echo "   | Bytecode -> \"byte\"\n")
  >     (echo "   | Native -> \"native\"\n")
  >     (echo "   | Other s -> s)\n")))))
  > EOF

  $ touch test.ml

  $ dune runtest
  Test byte
  Test native

An explicitly empty mode list is rejected rather than silently disabling the
tests.

  $ cat >dune <<EOF
  > (library
  >  (name test)
  >  (modules test)
  >  (inline_tests (modes)))
  > EOF

  $ dune runtest
  File "dune", line 4, characters 15-22:
  4 |  (inline_tests (modes)))
                     ^^^^^^^
  Error: No inline test mode defined
  [1]
