open Stdune
open Dune_engine

let current_run_before_mode_selection = Memo.current_run ()

let () =
  Memo.set_incremental false;
  Dune_tests_common.init ()
;;

let run memo =
  try Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension") with
  | Memo.Error.E error -> raise (Memo.Error.get error)
;;

let%expect_test "completed targets after explicit batch invalidation" =
  let context =
    Build_context.create ~name:(Context_name.of_string "test-completed-target-epoch")
  in
  let a = Path.Build.relative context.build_dir "a" in
  let b = Path.Build.relative context.build_dir "b" in
  let second_run = ref false in
  let generator_runs = ref 0 in
  let module Rule_generator = struct
    let gen_rules _ ~dir _ =
      if not (Path.Build.equal dir context.build_dir)
      then Memo.return Build_config.Gen_rules.Gen_rules_result.no_rules
      else (
        incr generator_runs;
        let targets =
          Targets.Files.create
            (Path.Build.Set.of_list (if !second_run then [ a ] else [ a; b ]))
        in
        let rule =
          Rule.make ~targets (Action_builder.return (Action.Full.make Action.empty))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create (Memo.return (Rules.of_rules [ rule ])))))
    ;;
  end
  in
  let module Source_tree = struct
    module Dir = struct
      type t = unit

      let sub_dir_names () = Filename.Array.Set.empty
      let filenames () = Filename.Array.Set.empty
    end

    let find_dir _ = Memo.return None
  end
  in
  Build_config.set
    ~contexts:(Memo.Lazy.of_val [ context, Build_config.Context_type.Empty ])
    ~promote_source:(fun ~chmod:_ ~delete_dst_if_it_is_a_directory:_ ~src:_ ~dst:_ ->
      Fiber.return ())
    ~sandboxing_preference:[]
    ~rule_generator:(module Rule_generator)
    ~implicit_default_alias:(fun _ -> Memo.return None)
    ~execution_parameters:(fun _ ~dir:_ ->
      Memo.return Execution_parameters.builtin_default)
    ~source_tree:(module Source_tree);
  let lookup target = run (Load_rules.get_rule (Path.build target)) in
  let independent_misses () =
    List.for_all [ "missing-first"; "missing-second" ] ~f:(fun name ->
      let target = Path.Build.relative context.build_dir name in
      Option.is_none (lookup target))
  in
  printfn "independent misses before invalidation: %b" (independent_misses ());
  let first = Option.value_exn (lookup a) in
  printfn "first batch shares atomic siblings: %b" (first == Option.value_exn (lookup b));
  second_run := true;
  (* Batch mode records no dependencies. Explicitly invalidate every registered
     table so fresh generation does not rely on incremental restoration. *)
  Memo.reset (Memo.Invalidation.invalidate_caches ~reason:Test);
  printfn "independent misses after invalidation: %b" (independent_misses ());
  let second = Option.value_exn (lookup a) in
  let retired = lookup b in
  let complete =
    match run (Load_rules.load_dir ~dir:(Path.build context.build_dir)) with
    | Build { rules_here; _ } -> rules_here
    | _ -> Code_error.raise "Expected a build-directory view" []
  in
  let fresh = Path.Build.Map.find complete.by_file_targets a |> Option.value_exn in
  printfn
    "complete view independently observes the new declaration: %b"
    (fresh != first && not (Path.Build.Map.mem complete.by_file_targets b));
  printfn "point lookup uses the freshly generated rule: %b" (second == fresh);
  printfn "point lookup forgets the retired sibling: %b" (Option.is_none retired);
  printfn "generator runs: %d" !generator_runs;
  [%expect
    {|
    independent misses before invalidation: true
    first batch shares atomic siblings: true
    independent misses after invalidation: true
    complete view independently observes the new declaration: true
    point lookup uses the freshly generated rule: true
    point lookup forgets the retired sibling: true
    generator runs: 2
    |}]
;;

let%expect_test "batch current-run reads preserve checkpoints and epochs" =
  let exception Checkpoint_failure in
  let original_check_point = !Memo.check_point in
  Fun.protect
    ~finally:(fun () ->
      Memo.check_point := original_check_point;
      Memo.reset Memo.Invalidation.empty)
    (fun () ->
       Memo.reset Memo.Invalidation.empty;
       let checkpoint_runs = ref 0 in
       let failing_check_point =
         Fiber.of_thunk (fun () ->
           incr checkpoint_runs;
           raise Checkpoint_failure)
       in
       let read_fails () =
         match run current_run_before_mode_selection with
         | _ -> false
         | exception Checkpoint_failure -> true
       in
       Memo.check_point := failing_check_point;
       printfn "cold read runs checkpoint: %b" (read_fails ());
       Memo.check_point := Fiber.return ();
       printfn "same-epoch failure stays cached: %b" (read_fails ());
       Memo.reset Memo.Invalidation.empty;
       let recovered = run current_run_before_mode_selection in
       print_endline "reset recovers";
       Memo.check_point := failing_check_point;
       let warm = run current_run_before_mode_selection in
       printfn
         "warm read ignores new checkpoint: %b"
         (Memo.Run.For_tests.compare recovered warm = Eq);
       Memo.check_point := Fiber.return ();
       Memo.reset Memo.Invalidation.empty;
       let next = run current_run_before_mode_selection in
       printfn
         "preconstructed read observes next epoch: %b"
         (Memo.Run.For_tests.compare recovered next <> Eq);
       printfn "checkpoint executions: %d" !checkpoint_runs);
  [%expect
    {|
    cold read runs checkpoint: true
    same-epoch failure stays cached: true
    reset recovers
    warm read ignores new checkpoint: true
    preconstructed read observes next epoch: true
    checkpoint executions: 1
    |}]
;;
