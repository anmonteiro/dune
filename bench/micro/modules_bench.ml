open Base
module Module = Dune_rules.Module
module Modules = Dune_rules.Modules
module Module_name = Dune_lang.Module_name
module Module_trie = Dune_rules.For_tests.Module_trie
module Path = Stdune.Path

let name s = Module_name.of_checked_string s
let obj_dir = Path.Build.relative Path.Build.root "modules-bench"

let generated path =
  Module.generated
    ~kind:Impl
    ~for_:Ocaml
    ~src_dir:obj_dir
    (List.map path ~f:name |> Stdune.Nonempty_list.of_list_exn)
;;

let add trie path =
  let key = List.map path ~f:name |> Stdune.Nonempty_list.of_list_exn in
  Module_trie.set trie key (generated path)
;;

let make_modules trie =
  Modules.lib
    ~obj_dir
    ~main_module_name:None
    ~wrapped:(Simple false)
    ~stdlib:None
    ~lib_name:(Dune_lang.Lib_name.Local.of_string "modules_bench")
    ~implements:false
    ~has_instances:false
    ~modules:trie
    ~for_:Ocaml
  |> Modules.With_vlib.modules
;;

let make_deep_parent_case () =
  let depth = 16 in
  let dependency_count = 128 in
  let unit_path =
    List.init depth ~f:(fun i -> Printf.sprintf "Level%02d" i) @ [ "Unit" ]
  in
  let unit = generated unit_path in
  let trie = add Module_trie.empty unit_path in
  let trie, names =
    List.fold (List.range 0 dependency_count) ~init:(trie, []) ~f:(fun (trie, names) i ->
      let dependency = Printf.sprintf "Dep%03d" i in
      add trie [ dependency ], name dependency :: names)
  in
  make_modules trie, unit, List.rev names
;;

let make_group_closure_case () =
  let group_count = 64 in
  let modules_per_group = 8 in
  let unit_path = [ "Current"; "Nested"; "Unit" ] in
  let unit = generated unit_path in
  let trie = add Module_trie.empty unit_path in
  let trie, names =
    List.fold (List.range 0 group_count) ~init:(trie, []) ~f:(fun (trie, names) i ->
      let group = Printf.sprintf "Group%02d" i in
      let trie =
        List.fold (List.range 0 modules_per_group) ~init:trie ~f:(fun trie j ->
          add trie [ group; Printf.sprintf "Child%02d" j ])
      in
      trie, name group :: names)
  in
  make_modules trie, unit, List.rev names
;;

let run (modules, unit, names) =
  ignore (Modules.With_vlib.find_deps modules ~of_:unit names)
;;

let warm make_case =
  let case = make_case () in
  run case;
  fun () -> run case
;;

let%bench_fun "qualified deps: reused deep-parent batch" = warm make_deep_parent_case
let%bench_fun "qualified deps: warm group-closure cache" = warm make_group_closure_case

let%bench_fun "qualified deps: cold first group-closure lookup (includes setup)" =
  fun () -> run (make_group_closure_case ())
;;
