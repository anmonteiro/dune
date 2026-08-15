Test simple interactions between melange.emit and copy_files

  $ make_melange_project 3.8 0.1

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (emit_stdlib false)
  >  (target output)
  >  (preprocess (pps melange.ppx))
  >  (runtime_deps assets/file.txt (glob_files_rec ./globbed/*.txt)))
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF
  $ mkdir globbed
  $ echo a.txt > globbed/a.txt
  $ echo b.txt > globbed/b.txt

  $ cat > main.ml <<EOF
  > external readFileSync : string -> encoding:string -> string = "readFileSync"
  > [@@mel.module "fs"]
  > let dirname = [%mel.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = readFileSync (dirname ^ "/" ^ file_path) ~encoding:"utf8"
  > let () = Js.log file_content
  > EOF

Rules created for the assets in the output directory

  $ dune build output/assets/file.txt
  $ find _build/default/output
  _build/default/output
  _build/default/output/assets
  _build/default/output/assets/file.txt
  $ dune clean

Alias is found even if source dir "output" isn't present

  $ dune rules --root . --format=json @mel |
  > jq_dune -r 'rulesMatchingTarget("output/assets/file.txt") | select(ruleHasCopy("assets/file.txt"; "output/assets/file.txt")) | ruleDepFilePaths'
  _build/default/assets/file.txt

  $ dune build @mel

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/
  assets
  globbed
  main.ml
  output
  $ ls _build/default/output
  assets
  globbed
  main.js

  $ dune build output/assets/file.txt
  $ ls _build/default/output
  assets
  globbed
  main.js
  $ ls _build/default/output/globbed
  a.txt
  b.txt

  $ node _build/default/output/main.js
  hello from file
  

Runtime dependencies can be renamed independently in multiple emit stanzas and
promoted into their respective output directories

  $ mkdir renamed-runtime-deps
  $ cd renamed-runtime-deps
  $ make_melange_project 3.25 1.0

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias output-a)
  >  (emit_stdlib false)
  >  (promote (until-clean) (into output_a))
  >  (target output_a)
  >  (runtime_deps
  >   (index_a.html as index.html)
  >   (generated.html as nested/generated.html)))
  > (melange.emit
  >  (alias output-b)
  >  (emit_stdlib false)
  >  (promote (until-clean) (into output_b))
  >  (target output_b)
  >  (runtime_deps (index_b.html as index.html)))
  > (rule
  >  (target generated.html)
  >  (action (with-stdout-to %{target} (echo generated))))
  > EOF

  $ echo index-a > index_a.html
  $ echo index-b > index_b.html

  $ dune build @output-a @output-b

  $ cat output_a/index.html
  index-a
  $ cat output_b/index.html
  index-b
  $ cat output_a/nested/generated.html
  generated
  $ cat _build/default/output_a/index.html
  index-a
  $ cat _build/default/output_b/index.html
  index-b
  $ cat _build/default/output_a/nested/generated.html
  generated

Renamed dependencies cannot escape the emit target

  $ cat >> dune <<EOF
  > (melange.emit
  >  (alias invalid-output)
  >  (emit_stdlib false)
  >  (target invalid-output)
  >  (runtime_deps (index_a.html as ../index.html)))
  > EOF

  $ dune build @invalid-output
  File "dune", line 22, characters 32-45:
  22 |  (runtime_deps (index_a.html as ../index.html)))
                                       ^^^^^^^^^^^^^
  Error: The destination path ../index.html must be relative to the Melange
  target directory.
  [1]

Renaming runtime dependencies requires Dune language 3.25

  $ make_melange_project 3.24 1.0
  $ dune build @output-a
  File "dune", line 7, characters 3-15:
  7 |   (index_a.html as index.html)
         ^^^^^^^^^^^^
  Error: Using (source as destination) in runtime_deps is only available since
  version 3.25 of the dune language. Please update your dune-project file to
  have (lang dune 3.25).
  [1]
