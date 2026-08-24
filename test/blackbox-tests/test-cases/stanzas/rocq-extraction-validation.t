Extraction outputs must be nonempty filenames.

  $ make_rocq_project 3.23 0.13

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_files))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-18:
  3 |  (extracted_files))
       ^^^^^^^^^^^^^^^^^
  Error: Field "extracted_files" must not be empty
  [1]

  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_files ../extr.ml))
  > EOF

  $ dune build
  File "dune", line 3, characters 18-28:
  3 |  (extracted_files ../extr.ml))
                        ^^^^^^^^^^
  Error: Entries in field "extracted_files" must be filenames
  [1]

The legacy extracted_modules field must also be nonempty.

  $ make_rocq_project 3.22 0.12
  $ cat >dune <<EOF
  > (rocq.extraction
  >  (prelude extr)
  >  (extracted_modules))
  > EOF

  $ dune build
  File "dune", line 3, characters 1-20:
  3 |  (extracted_modules))
       ^^^^^^^^^^^^^^^^^^^
  Error: Field "extracted_modules" must not be empty
  [1]
