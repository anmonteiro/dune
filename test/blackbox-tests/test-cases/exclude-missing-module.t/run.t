Rejects excluding modules that do not exist.

  $ dune build foo.cma
  File "dune", line 3, characters 22-26:
  3 |  (modules :standard \ fake))
                            ^^^^
  Error: Module Fake is excluded but it doesn't exist.
  [1]
