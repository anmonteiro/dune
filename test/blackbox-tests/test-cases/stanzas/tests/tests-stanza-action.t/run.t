If there is an (action) field, it is used to invoke to the executable (in both
regular and expect modes:

  $ dune build @explicit-regular/runtest
  argv[0] = "./my_test.exe"
  argv[1] = "arg1"
  argv[2] = "arg2"
  argv[3] = "arg3"

  $ dune build @explicit-expect/runtest

If there is no field, the program is run with no arguments:

  $ dune build @default/runtest
  argv[0] = "./my_test.exe"

Custom expect actions may produce additional inferred targets.

  $ mkdir additional-target
  $ cat >additional-target/dune <<EOF
  > (test
  >  (name main)
  >  (action
  >   (progn
  >    (with-stdout-to extra (echo "extra output"))
  >    (run %{test}))))
  > EOF
  $ cat >additional-target/main.ml <<EOF
  > let () = print_endline "test output"
  > EOF
  $ echo 'test output' >additional-target/main.expected
  $ dune build @additional-target/runtest
  $ cat _build/default/additional-target/extra
  extra output
