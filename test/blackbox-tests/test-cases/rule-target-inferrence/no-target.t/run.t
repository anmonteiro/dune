When an action has no targets, an helpful error message is displayed:

  $ dune build @all
  File "dune", line 1, characters 0-34:
  1 | (rule (action (echo "something")))
      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Rule has no targets specified
  [1]
