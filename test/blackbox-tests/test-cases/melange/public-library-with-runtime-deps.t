Test simple interactions between melange.emit and copy_files

  $ mkdir prefix
  $ cat > dune-project <<EOF
  > (lang dune 3.7)
  > (package (name foo))
  > (using melange 0.1)
  > EOF

  $ cat > dune <<EOF
  > (melange.emit
  >  (alias mel)
  >  (target output)
  >  (libraries foo)
  >  (runtime_deps assets/file.txt))
  > EOF

  $ mkdir -p lib/nested
  $ echo "Some text" > lib/index.txt
  $ echo "Some nested text" > lib/nested/hello.txt
  $ cat > lib/dune <<EOF
  > (library
  >  (public_name foo)
  >  (modes melange)
  >  (melange.runtime_deps index.txt nested/hello.txt))
  > EOF
  $ cat > lib/foo.ml <<EOF
  > let dirname = [%bs.raw "__dirname"]
  > let () = Js.log2 "dirname:" dirname
  > let file_path = "./index.txt"
  > let read_asset () = Node.Fs.readFileSync (dirname ^ "/" ^ file_path) \`utf8
  > EOF

  $ dune build
  $ dune install --prefix $PWD/prefix
  $ cat _build/default/foo.install
  lib: [
    "_build/install/default/lib/foo/META"
    "_build/install/default/lib/foo/dune-package"
    "_build/install/default/lib/foo/foo.ml"
    "_build/install/default/lib/foo/index.txt"
    "_build/install/default/lib/foo/melange/foo.cmi" {"melange/foo.cmi"}
    "_build/install/default/lib/foo/melange/foo.cmj" {"melange/foo.cmj"}
    "_build/install/default/lib/foo/melange/foo.cmt" {"melange/foo.cmt"}
    "_build/install/default/lib/foo/nested/hello.txt" {"nested/hello.txt"}
  ]

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

  $ mkdir -p output
  $ dune build @mel

The runtime_dep index.txt was copied to the build folder

  $ ls _build/default/lib
  foo.ml
  index.txt
  nested
  $ ls _build/default/output/node_modules/foo/
  foo.js
  index.txt
  nested
  $ node _build/default/output/main.js
  dirname: $TESTCASE_ROOT/_build/default/output/node_modules/foo
  hello from file
  
  Some text
  


