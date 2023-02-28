Test simple interactions between melange.emit and copy_files

  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (libraries foo)
  >  (runtime_deps assets/file.txt)
  >  (module_system commonjs))
  > EOF

  $ mkdir lib
  $ echo "Some text" > lib/index.txt
  $ cat > lib/dune <<EOF
  > (library
  >  (name foo)
  >  (modes melange)
  >  (melange.runtime_deps index.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let file_path = "./index.txt"
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
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
  > let () = Js.log (Foo.read_asset ())
  > EOF

  $ dune build @mel --display=short
          melc lib/.foo.objs/melange/foo.{cmi,cmj,cmt}
          melc ...mobjs/melange/melange__Main.{cmi,cmj,cmt}
          melc lib/foo.js
          melc main.js

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/lib
  foo.js
  foo.ml
  index.txt
  $ node _build/default/main.js
  hello from file
  
  Some text
  




  $ dune clean
  $ rm -rf dune
  $ mkdir output
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (libraries foo)
  >  (module_system commonjs))
  > EOF

  $ dune build @mel --display=short
          melc lib/.foo.objs/melange/foo.{cmi,cmj,cmt}
          melc output/.output.mobjs/melange/melange.{cmi,cmj,cmt}
          melc output/lib/foo.js
          melc output/output/.output.mobjs/melange.js

  $ ls _build/default/output/lib
  foo.js
  index.txt

  $ dune build output/lib/index.txt --display=short
  $ ls _build/default/output/lib
  foo.js
  index.txt

  $ dune clean
  $ cat > output/dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (libraries foo)
  >  (promote (until-clean))
  >  (module_system commonjs))
  > EOF

  $ dune build @mel --display=short
          melc lib/.foo.objs/melange/foo.{cmi,cmj,cmt}
          melc output/.output.mobjs/melange/melange.{cmi,cmj,cmt}
          melc output/lib/foo.js
          melc output/output/.output.mobjs/melange.js

Library static asset gets promoted

  $ ls output/lib/
  foo.js
  index.txt
