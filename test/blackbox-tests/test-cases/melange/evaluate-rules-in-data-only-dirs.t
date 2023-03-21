Test to show how data-only dirs are handled when evaluating rules

  $ cat > dune-project <<EOF
  > (lang dune 3.8)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (data_only_dirs assets)
  > (melange.emit
  >  (target output)
  >  (alias mel)
  >  (runtime_deps assets/file.txt))
  > EOF

  $ mkdir assets
  $ cat > assets/file.txt <<EOF
  > hello from file
  > EOF

  $ cat > main.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./assets/file.txt"
  > let file_content = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > let () = Js.log file_content
  > EOF

Dirs in data_only_dirs are visited

  $ dune build @mel --debug-load-dir 2>&1 | grep assets
  Loading build directory _build/default/output/assets
  Loading build directory _build/default/assets
  Loading build directory _build/default/output/assets/.bin
  Loading build directory _build/default/output/assets/.formatted
  Loading build directory _build/default/output/assets/.utop

Full list of dirs

  $ dune build @mel --debug-load-dir
  Loading build directory _build/default
  Loading build directory _build/default/.dune
  Loading build directory _build
  Loading build directory _build/default/output
  Loading build directory _build/default/.output.mobjs/melange
  Loading build directory _build/default/.output.mobjs
  Loading build directory _build/default/.merlin-conf
  Loading build directory _build/default/output/assets
  Loading build directory _build/default/assets
  Loading build directory _build/default/output/.bin
  Loading build directory _build/default/output/.formatted
  Loading build directory _build/default/output/.utop
  Loading build directory _build/default/output/assets/.bin
  Loading build directory _build/default/output/assets/.formatted
  Loading build directory _build/default/output/assets/.utop
