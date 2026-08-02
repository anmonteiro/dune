open Base
module Bench = Dune_rules.For_benchmarks
module Alias_module = Bench.Alias_module
module Instances = Bench.Parameterised_instances
module Module_name = Dune_lang.Module_name

let use value = Sys.opaque_identity value |> ignore

let module_name prefix index =
  Module_name.of_checked_string (Printf.sprintf "%s_%04d" prefix index)
;;

let canonical_path index =
  Stdune.Nonempty_list.of_list_exn
    [ Module_name.of_checked_string "Bench"; module_name "Canonical" index ]
;;

let alias_module_input ~aliases ~shadowed =
  let aliases =
    List.init aliases ~f:(fun index ->
      ( canonical_path index
      , module_name "Local" index
      , Module_name.Unique.of_string (Printf.sprintf "Object_%04d" index) ))
  in
  let shadowed = List.init shadowed ~f:(module_name "Shadowed") in
  Alias_module.create_input ~aliases ~shadowed
;;

let%bench_fun
    ("alias module (prepare and render)"
     [@params
       size
       = [ "10 aliases", (10, 0)
         ; "100 aliases, 1 shadowed", (100, 1)
         ; "1000 aliases, 1 shadowed", (1_000, 1)
         ]])
  =
  let aliases, shadowed = size in
  let input = alias_module_input ~aliases ~shadowed in
  fun () -> Alias_module.prepare_and_render input |> use
;;

let arguments instance_index count =
  List.init count ~f:(fun argument_index ->
    ( module_name "Parameter" argument_index
    , module_name "Argument" ((instance_index * count) + argument_index) ))
;;

let instance ~arguments_count index =
  Instances.instance
    ~new_name:(module_name "Instance" index)
    ~lib_name:(module_name "Library" index)
    ~args:(arguments index arguments_count)
;;

let simple_instances ~instances ~arguments_count =
  List.init instances ~f:(fun index ->
    instance ~arguments_count index |> Instances.simple)
  |> Instances.concat
;;

let wrapped_instances ~groups ~instances_per_group ~arguments_count =
  List.init groups ~f:(fun group ->
    let instances =
      List.init instances_per_group ~f:(fun index ->
        instance ~arguments_count ((group * instances_per_group) + index))
    in
    Instances.wrapped ~name:(module_name "Wrapped" group) instances)
  |> Instances.concat
;;

let%bench_fun
    ("parameterised instances"
     [@params
       shape
       = [ "1 simple, 0 arguments", `Simple (1, 0)
         ; "32 simple, 4 arguments", `Simple (32, 4)
         ; "16 wrapped x 8 instances, 4 arguments", `Wrapped (16, 8, 4)
         ]])
  =
  let instances =
    match shape with
    | `Simple shape ->
      let instances, arguments_count = shape in
      simple_instances ~instances ~arguments_count
    | `Wrapped (groups, instances_per_group, arguments_count) ->
      wrapped_instances ~groups ~instances_per_group ~arguments_count
  in
  fun () -> Instances.to_ml instances |> use
;;
