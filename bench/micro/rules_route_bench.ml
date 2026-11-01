open Stdune
open Dune_engine

let run memo =
  Fiber.run (Memo.run memo) ~iter:(fun () ->
    Code_error.raise "Unexpected benchmark suspension" [])
;;

let rules_per_family = 16

type fixture =
  { tree : Rules.t
  ; dirs : Path.Build.t array
  ; targets : Path.Build.t array array
  ; rules : Rule.t array array
  ; runs : int array
  }

let fixture ~families ~producer ~shared =
  let root = Path.Build.of_string "default/route-bench" in
  let dirs =
    Array.init families ~f:(fun family ->
      if shared then root else Path.Build.relative root (sprintf ".lib-%d.objs" family))
  in
  let targets =
    Array.init families ~f:(fun family ->
      Array.init rules_per_family ~f:(fun rule ->
        Path.Build.relative dirs.(family) (sprintf "family-%d-rule-%d.cmo" family rule)))
  in
  let rules =
    Array.map targets ~f:(fun targets ->
      Array.map targets ~f:(fun target ->
        let sibling = Path.Build.set_extension target ~ext:Filename.Extension.cmi in
        let targets = Targets.Files.create (Path.Build.Set.of_list [ target; sibling ]) in
        Rule.make ~targets (Action_builder.return (Action.Full.make Action.empty))))
  in
  let bodies =
    Array.map rules ~f:(fun rules ->
      if producer
      then
        run
          (Rules.collect_unit (fun () ->
             Memo.List.iter (Array.to_list rules) ~f:(fun rule ->
               Rules.narrow (Target_mask.of_targets rule.Rule.targets) (fun () ->
                 Rules.Produce.rule rule))))
      else Rules.of_rules (Array.to_list rules))
  in
  let runs = Array.make families 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Memo.List.iter (List.init families ~f:Fun.id) ~f:(fun family ->
           (* Non-exact outer masks exercise separate cached families, instead
              of turning the entire root into one exact-file posting family. *)
           let mask =
             if shared
             then
               Target_mask.files_matching
                 ~dir:root
                 (Predicate_lang.Glob.of_string (sprintf "family-%d-*" family))
             else Target_mask.subtree dirs.(family)
           in
           Rules.narrow mask (fun () ->
             runs.(family) <- runs.(family) + 1;
             Rules.produce bodies.(family)))))
  in
  { tree; dirs; targets; rules; runs }
;;

let query fixture family rule =
  run (Rules.load_path_with_pending fixture.tree fixture.targets.(family).(rule))
;;

let measure fixture ~producer ~shared ~phase ~rounds ~sample =
  let families = Array.length fixture.dirs in
  let iterations = families * rounds in
  let requests =
    Array.init iterations ~f:(fun index ->
      index mod families, index / families mod rules_per_family)
  in
  let results = Array.make iterations None in
  Gc.full_major ();
  let minor_before, promoted_before, major_before = Gc.counters () in
  let cpu_before = Sys.time () in
  let wall_before = Unix.gettimeofday () in
  Array.iteri requests ~f:(fun index (family, rule) ->
    results.(index) <- Some (Sys.opaque_identity (query fixture family rule)));
  let seconds = Unix.gettimeofday () -. wall_before in
  let cpu = Sys.time () -. cpu_before in
  let minor_after, promoted_after, major_after = Gc.counters () in
  let allocated_words =
    minor_after
    -. minor_before
    +. major_after
    -. major_before
    -. (promoted_after -. promoted_before)
  in
  let checksum = ref 0 in
  Array.iteri results ~f:(fun index result ->
    let family, rule = requests.(index) in
    let { Rules.selected; _ } = Option.value_exn result in
    let { Rules.Dir_rules.rules; aliases } =
      Rules.find selected (Path.build fixture.dirs.(family)) |> Rules.Dir_rules.consume
    in
    match rules with
    | [ selected ]
      when selected == fixture.rules.(family).(rule) && Alias.Name.Map.is_empty aliases ->
      incr checksum
    | _ -> Code_error.raise "Incorrect benchmark rule selection" []);
  if not (Array.for_all fixture.runs ~f:(Int.equal 1))
  then Code_error.raise "A benchmark family did not run exactly once" [];
  Printf.printf
    "{\"family\":%S,\"layout\":%S,\"phase\":%S,\"families\":%d,\"rules_per_family\":%d,\"sample\":%d,\"iterations\":%d,\"seconds\":%.9f,\"cpu\":%.9f,\"allocated_words\":%.0f,\"checksum\":%d}\n\
     %!"
    (if producer then "producer" else "direct")
    (if shared then "shared" else "split")
    phase
    families
    rules_per_family
    sample
    iterations
    seconds
    cpu
    allocated_words
    !checksum
;;

let () =
  let families = ref 256 in
  let rounds = ref 16 in
  let samples = ref 3 in
  Arg.parse
    [ "--families", Arg.Set_int families, "Number of families under one root"
    ; "--rounds", Arg.Set_int rounds, "Number of warmed round-robin passes"
    ; "--samples", Arg.Set_int samples, "Number of independently constructed samples"
    ]
    (fun _ -> raise (Arg.Bad "No positional arguments are accepted"))
    "rules_route_bench [--families N] [--rounds N] [--samples N]";
  if !families < 1 || !rounds < 1 || !samples < 1
  then Code_error.raise "Benchmark sizes must be positive" [];
  Path.set_root (Path.External.cwd ());
  Path.Build.set_build_dir (Path.Outside_build_dir.of_string "_build");
  Memo.set_incremental false;
  List.iter [ false; true ] ~f:(fun producer ->
    List.iter [ false; true ] ~f:(fun shared ->
      for sample = 1 to !samples do
        Memo.reset (Memo.Invalidation.invalidate_caches ~reason:Test);
        let fixture = fixture ~families:!families ~producer ~shared in
        measure fixture ~producer ~shared ~phase:"cold" ~rounds:1 ~sample;
        for rule = 0 to rules_per_family - 1 do
          for family = 0 to !families - 1 do
            ignore (query fixture family rule : Rules.loaded)
          done
        done;
        measure fixture ~producer ~shared ~phase:"warm" ~rounds:!rounds ~sample
      done))
;;
