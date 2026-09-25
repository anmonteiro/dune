open Stdune
open Dune_engine

let () = Dune_tests_common.init ()

let run memo =
  try Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension") with
  | Memo.Error.E error -> raise (Memo.Error.get error)
;;

let path = Path.Build.of_string

let rule targets =
  Rule.make ~targets (Action_builder.return (Action.Full.make Action.empty))
;;

let file_rule files = rule (Targets.Files.create (Path.Build.Set.of_list files))

let rules_in ~dir rules =
  let { Rules.Dir_rules.rules; _ } =
    Rules.find rules (Path.build dir) |> Rules.Dir_rules.consume
  in
  rules
;;

let print_rule_files label rules =
  let files =
    List.concat_map rules ~f:(fun rule -> Filename.Set.to_list rule.Rule.targets.files)
    |> List.map ~f:Filename.to_string
    |> List.sort ~compare:String.compare
  in
  printfn "%s: %s" label (String.concat files ~sep:", ")
;;

let print_files label ~dir rules = print_rule_files label (rules_in ~dir rules)

let expect_code_error label f =
  try
    f ();
    printfn "%s: no error" label
  with
  | Code_error.E _ -> printfn "%s: code error" label
;;

let%expect_test "ownership proofs follow nested producers without forcing siblings" =
  let dir = path "default/ownership-proof-nested" in
  let target = Path.Build.relative dir "target" in
  let unused = Path.Build.relative dir "unused" in
  let owned_rule = file_rule [ target ] in
  let input = Memo.Var.create 0 ~name:"ownership-proof-nested-input" in
  let outer_runs = ref 0 in
  let inner_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           incr outer_runs;
           let open Memo.O in
           let* () =
             Rules.narrow (Target_mask.files [ target ]) (fun () ->
               let* (_ : int) = Memo.Var.read input in
               incr inner_runs;
               Rules.Produce.rule owned_rule)
           in
           Rules.narrow (Target_mask.files [ unused ]) (fun () ->
             Code_error.raise "Ownership proof forced an unused producer" []))))
  in
  let since = Memo.Run.For_tests.current () in
  assert (Rules.unchanged_since tree ~since);
  assert (!outer_runs = 0 && !inner_runs = 0);
  let load () = run (Rules.load tree (Target_mask.path target)) in
  assert (List.equal ( == ) (rules_in ~dir (load ())) [ owned_rule ]);
  assert (!outer_runs = 1 && !inner_runs = 1);
  Memo.reset Memo.Invalidation.empty;
  assert (Rules.unchanged_since tree ~since);
  assert (!outer_runs = 1 && !inner_runs = 1);
  Memo.reset (Memo.Var.set input 1);
  assert (not (Rules.unchanged_since tree ~since));
  assert (!outer_runs = 1 && !inner_runs = 1);
  assert (List.equal ( == ) (rules_in ~dir (load ())) [ owned_rule ]);
  let current = Memo.Run.For_tests.current () in
  assert (Rules.unchanged_since tree ~since:current);
  assert (not (Rules.unchanged_since tree ~since));
  assert (!outer_runs = 1 && !inner_runs = 2);
  [%expect {| |}]
;;

let%expect_test "ownership proofs track deferred results behind unforced wrappers" =
  let dir = path "default/ownership-proof-deferred" in
  let target = Path.Build.relative dir "target" in
  let owned_rule = file_rule [ target ] in
  let input = Memo.Var.create 0 ~name:"ownership-proof-deferred-input" in
  let runs = ref 0 in
  let value, tree =
    run
      (Rules.collect (fun () ->
         Rules.defer (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           let* value = Memo.Var.read input in
           incr runs;
           if value < 0 then Code_error.raise "Ownership proof producer failed" [];
           let+ () = Rules.Produce.rule owned_rule in
           value)))
  in
  let restricted = Rules.restrict tree (Target_mask.files [ target ]) in
  let prefixed =
    run
      (Rules.collect_unit (fun () ->
         Rules.prefix_rules (Action_builder.return ()) ~f:(fun () -> Rules.produce tree)))
  in
  let views =
    [ tree; restricted; prefixed; Rules.restrict prefixed (Target_mask.files [ target ]) ]
  in
  let check ~since expected =
    List.iter views ~f:(fun tree ->
      assert (Bool.equal (Rules.unchanged_since tree ~since) expected))
  in
  let since = Memo.Run.For_tests.current () in
  check ~since true;
  assert (!runs = 0);
  (* Only the separate result is forced; all forwarding wrappers stay unforced. *)
  assert (run (Memo.Lazy.force value) = 0);
  Memo.reset Memo.Invalidation.empty;
  check ~since true;
  assert (!runs = 1);
  Memo.reset (Memo.Var.set input 1);
  check ~since false;
  assert (!runs = 1);
  assert (run (Memo.Lazy.force value) = 1);
  let current = Memo.Run.For_tests.current () in
  check ~since:current true;
  check ~since false;
  assert (!runs = 2);
  Memo.reset (Memo.Var.set input (-1));
  assert (
    try
      ignore (run (Memo.Lazy.force value) : int);
      false
    with
    | Code_error.E { message; _ } ->
      String.equal message "Ownership proof producer failed");
  check ~since:(Memo.Run.For_tests.current ()) false;
  Memo.reset Memo.Invalidation.empty;
  check ~since:(Memo.Run.For_tests.current ()) false;
  assert (!runs = 3);
  Memo.reset (Memo.Var.set input 2);
  assert (run (Memo.Lazy.force value) = 2);
  check ~since:(Memo.Run.For_tests.current ()) true;
  assert (!runs = 4);
  [%expect {| |}]
;;

let%expect_test "ownership proofs reject a cycle in produced rule trees" =
  let tree = ref Rules.empty in
  let value, produced =
    run
      (Rules.collect (fun () ->
         Rules.defer Target_mask.all (fun () -> Rules.produce !tree)))
  in
  tree := produced;
  run (Memo.Lazy.force value);
  assert (not (Rules.unchanged_since produced ~since:(Memo.Run.For_tests.current ())));
  [%expect {| |}]
;;

let%expect_test "prepared ownership tracks semantic inputs, not materialization" =
  let open Memo.O in
  let dir = path "default/ownership-proof-prepared" in
  let target = Path.Build.relative dir "target" in
  let unused = Path.Build.relative dir "unused" in
  let input = Memo.Var.create 0 ~name:"prepared-ownership-input" in
  let materialization = Memo.Var.Unit.create () in
  let preparations = ref 0 in
  let productions = ref 0 in
  let prepare =
    Memo.lazy_ ~name:"ownership preparation" ~cutoff:Unit.equal (fun () ->
      let+ () = Memo.Var.Unit.read materialization in
      incr preparations)
  in
  let value, tree =
    run
      (Rules.collect (fun () ->
         let* value =
           Rules.defer_after
             (Target_mask.subtree dir)
             ~prepare:(Memo.Lazy.force prepare)
             (fun () ->
                let* input = Memo.Var.read input in
                incr productions;
                let owned_rule = file_rule [ target ] in
                let+ () = Rules.Produce.rule owned_rule in
                input, owned_rule)
         in
         let+ _ =
           Rules.defer_after
             (Target_mask.files [ unused ])
             ~prepare:
               (Memo.of_thunk (fun () ->
                  Code_error.raise "Prepared an unused ownership producer" []))
             (fun () -> Memo.return ())
         in
         value))
  in
  let value = Rules.Deferred.result value in
  let restricted = Rules.restrict tree (Target_mask.files [ target ]) in
  let check ~since expected =
    List.iter [ tree; restricted ] ~f:(fun tree ->
      assert (Bool.equal (Rules.unchanged_since tree ~since) expected))
  in
  let since = Memo.Run.For_tests.current () in
  check ~since true;
  assert (!preparations = 0 && !productions = 0);
  let initial = run (Memo.Lazy.force value) in
  assert (fst initial = 0);
  assert (!preparations = 1 && !productions = 1);
  Memo.reset (Memo.Var.Unit.invalidate materialization ~reason:Test);
  check ~since true;
  assert (!preparations = 1 && !productions = 1);
  assert (run (Memo.Lazy.force value) == initial);
  assert (!preparations = 2 && !productions = 1);
  check ~since true;
  Memo.reset (Memo.Var.set input 1);
  check ~since false;
  assert (!preparations = 2 && !productions = 1);
  let current = run (Memo.Lazy.force value) in
  assert (fst current = 1 && snd current != snd initial);
  assert (!preparations = 2 && !productions = 2);
  check ~since false;
  check ~since:(Memo.Run.For_tests.current ()) true;
  let loaded = run (Rules.load restricted (Target_mask.path target)) in
  assert (List.equal ( == ) (rules_in ~dir loaded) [ snd current ]);
  assert (!preparations = 2 && !productions = 2);
  [%expect {| |}]
;;

let%expect_test "failed preparation invalidates old and never-produced ownership" =
  List.iter [ true; false ] ~f:(fun first_failure ->
    let dir = path "default/ownership-proof-preparation-failure" in
    let target = Path.Build.relative dir "target" in
    let owned_rule = file_rule [ target ] in
    let fail = ref first_failure in
    let replays = ref 0 in
    let productions = ref 0 in
    let prepare =
      Memo.create_with_replay
        "ownership preparation replay"
        ~input:(module Unit)
        ~cutoff:Unit.equal
        ~replay:(fun () () ->
          incr replays;
          if !fail then Code_error.raise "Ownership preparation failed" [])
        (fun () -> Memo.return ())
    in
    let value, tree =
      run
        (Rules.collect (fun () ->
           Rules.defer_after
             (Target_mask.subtree dir)
             ~prepare:(Memo.exec prepare ())
             (fun () ->
                incr productions;
                let open Memo.O in
                let+ () = Rules.Produce.rule owned_rule in
                owned_rule)))
    in
    let value = Rules.Deferred.result value in
    let restricted = Rules.restrict tree (Target_mask.files [ target ]) in
    let since = Memo.Run.For_tests.current () in
    assert (Rules.unchanged_since restricted ~since);
    assert (!replays = 0 && !productions = 0);
    if not first_failure then assert (run (Memo.Lazy.force value) == owned_rule);
    fail := true;
    Memo.reset Memo.Invalidation.empty;
    let read_failed () =
      let during_reporting = ref [] in
      let result =
        Fiber.run
          (Fiber.collect_errors (fun () ->
             Memo.run_with_error_handler
               (fun () -> Memo.Lazy.force value)
               ~handle_error_no_raise:(fun _ ->
                 during_reporting
                 := Rules.unchanged_since restricted ~since :: !during_reporting;
                 Fiber.return ())))
          ~iter:(fun () -> failwith "unexpected suspension")
      in
      assert (
        match result with
        | Error [ { Exn_with_backtrace.exn = error; _ } ] ->
          let error =
            match error with
            | Memo.Error.E error -> Memo.Error.get error
            | error -> error
          in
          (match error with
           | Code_error.E { message; _ } ->
             String.equal message "Ownership preparation failed"
           | _ -> false)
        | Ok _ | Error _ -> false);
      assert (not (List.is_empty !during_reporting));
      assert (List.for_all !during_reporting ~f:not);
      assert (
        not (Rules.unchanged_since restricted ~since:(Memo.Run.For_tests.current ())))
    in
    (* Replay fails while the old outer node is still restoring, before its
       body can replace the previous [Produced] observation. *)
    read_failed ();
    let callbacks = !replays in
    read_failed ();
    assert (!replays = callbacks);
    assert (!productions = if first_failure then 0 else 1);
    fail := false;
    Memo.reset Memo.Invalidation.empty;
    assert (run (Memo.Lazy.force value) == owned_rule);
    assert (!productions = 1);
    assert (Rules.unchanged_since restricted ~since:(Memo.Run.For_tests.current ()));
    let loaded = run (Rules.load restricted (Target_mask.path target)) in
    assert (List.equal ( == ) (rules_in ~dir loaded) [ owned_rule ]));
  [%expect {| |}]
;;

let%expect_test "in-flight preparation blocks proofs and shares both readers" =
  List.iter [ false; true ] ~f:(fun restoring ->
    let dir = path "default/ownership-proof-preparation-wait" in
    let target = Path.Build.relative dir "target" in
    let owned_rule = file_rule [ target ] in
    let input = Memo.Var.Unit.create () in
    let blocked = ref false in
    let entered = Fiber.Ivar.create () in
    let release = Fiber.Ivar.create () in
    let preparations = ref 0 in
    let productions = ref 0 in
    let prepare =
      Memo.lazy_ ~name:"blocked ownership preparation" ~cutoff:Unit.equal (fun () ->
        let open Memo.O in
        let* () = Memo.Var.Unit.read input in
        incr preparations;
        if not !blocked
        then Memo.return ()
        else
          Memo.of_reproducible_fiber
            (let open Fiber.O in
             let* () = Fiber.Ivar.fill entered () in
             Fiber.Ivar.read release))
    in
    let value, tree =
      run
        (Rules.collect (fun () ->
           Rules.defer_after
             (Target_mask.subtree dir)
             ~prepare:(Memo.Lazy.force prepare)
             (fun () ->
                incr productions;
                let open Memo.O in
                let+ () = Rules.Produce.rule owned_rule in
                owned_rule)))
    in
    let value = Rules.Deferred.result value in
    let restricted = Rules.restrict tree (Target_mask.files [ target ]) in
    if restoring then assert (run (Memo.Lazy.force value) == owned_rule);
    blocked := true;
    Memo.reset (Memo.Var.Unit.invalidate input ~reason:Test);
    let since = Memo.Run.For_tests.current () in
    assert (Rules.unchanged_since restricted ~since);
    let during_preparation = ref None in
    let blocked_before =
      Counter.read Memo.Metrics.Restore.blocked
      + Counter.read Memo.Metrics.Compute.blocked
    in
    let result, (loaded, ()) =
      run
        (Memo.fork_and_join
           (fun () -> Memo.Lazy.force value)
           (fun () ->
              Memo.fork_and_join
                (fun () -> Rules.load restricted (Target_mask.path target))
                (fun () ->
                   Memo.of_reproducible_fiber
                     (let open Fiber.O in
                      let* () = Fiber.Ivar.read entered in
                      during_preparation
                      := Some (Rules.unchanged_since restricted ~since, !productions);
                      Fiber.Ivar.fill release ()))))
    in
    assert (!during_preparation = Some (false, if restoring then 1 else 0));
    assert (
      Counter.read Memo.Metrics.Restore.blocked
      + Counter.read Memo.Metrics.Compute.blocked
      > blocked_before);
    assert (result == owned_rule);
    assert (List.equal ( == ) (rules_in ~dir loaded) [ owned_rule ]);
    assert (!preparations = if restoring then 2 else 1);
    assert (!productions = 1);
    assert (Rules.unchanged_since restricted ~since));
  [%expect {| |}]
;;

let%expect_test "preparation cannot emit implicit rules" =
  let dir = path "default/ownership-proof-preparation-output" in
  let target = Path.Build.relative dir "target" in
  let owned_rule = file_rule [ target ] in
  let productions = ref 0 in
  let value, tree =
    run
      (Rules.collect (fun () ->
         Rules.defer_after
           (Target_mask.subtree dir)
           ~prepare:(Rules.Produce.rule owned_rule)
           (fun () ->
              incr productions;
              Memo.return ())))
  in
  let value = Rules.Deferred.result value in
  assert (
    try
      run (Memo.Lazy.force value);
      false
    with
    | Code_error.E { message; _ } ->
      String.equal
        message
        "Implicit_output.produce called without any handler in dynamic scope");
  assert (!productions = 0);
  assert (not (Rules.unchanged_since tree ~since:(Memo.Run.For_tests.current ())));
  [%expect {| |}]
;;

let%expect_test "derived rules preserve order, ownership inputs, and shared outputs" =
  let open Memo.O in
  let dir = path "default/ownership-proof-derived" in
  let original_target = Path.Build.relative dir "original" in
  let first_target = Path.Build.relative dir "first" in
  let second_target = Path.Build.relative dir "second" in
  let original_rule = file_rule [ original_target ] in
  let first_rule = file_rule [ first_target ] in
  let second_rule = file_rule [ second_target ] in
  let input = Memo.Var.create 0 ~name:"derived-ownership-input" in
  let materialization = Memo.Var.Unit.create () in
  let trace = ref [] in
  let log event = trace := event :: !trace in
  let prepare =
    Memo.lazy_ ~name:"derived ownership preparation" ~cutoff:Unit.equal (fun () ->
      let* () = Memo.Var.Unit.read materialization in
      let+ (_ : int) = Memo.Var.read input in
      log "prepare")
  in
  let value, original =
    run
      (Rules.collect (fun () ->
         Rules.defer_after
           (Target_mask.files [ original_target ])
           ~prepare:(Memo.Lazy.force prepare)
           (fun () ->
              let* input = Memo.Var.read input in
              log "original";
              let+ () = Rules.Produce.rule original_rule in
              input)))
  in
  let mask = Target_mask.files [ first_target; second_target ] in
  let derived =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow_after mask value (fun input ->
           log "derived";
           Rules.Produce.rule (if input = 0 then first_rule else second_rule))))
  in
  let since = Memo.Run.For_tests.current () in
  assert (Rules.unchanged_since derived ~since);
  assert (List.is_empty !trace);
  let load () = run (Rules.load derived mask) |> rules_in ~dir in
  assert (List.equal ( == ) (load ()) [ first_rule ]);
  assert (List.equal String.equal (List.rev !trace) [ "prepare"; "original"; "derived" ]);
  let result = Rules.Deferred.result value in
  assert (run (Memo.Lazy.force result) = 0);
  let original = run (Rules.load original (Target_mask.path original_target)) in
  assert (List.equal ( == ) (rules_in ~dir original) [ original_rule ]);
  assert (List.equal ( == ) (load ()) [ first_rule ]);
  assert (List.length !trace = 3);
  trace := [];
  Memo.reset (Memo.Var.Unit.invalidate materialization ~reason:Test);
  assert (Rules.unchanged_since derived ~since);
  assert (List.is_empty !trace);
  assert (List.equal ( == ) (load ()) [ first_rule ]);
  assert (List.equal String.equal !trace [ "prepare" ]);
  trace := [];
  Memo.reset (Memo.Var.set input 1);
  assert (not (Rules.unchanged_since derived ~since));
  assert (List.is_empty !trace);
  assert (List.equal ( == ) (load ()) [ second_rule ]);
  assert (List.equal String.equal (List.rev !trace) [ "prepare"; "original"; "derived" ]);
  assert (run (Memo.Lazy.force result) = 1);
  assert (not (Rules.unchanged_since derived ~since));
  assert (Rules.unchanged_since derived ~since:(Memo.Run.For_tests.current ()));
  [%expect {| |}]
;;

let%expect_test "derived-only proofs retain an independently used origin" =
  let open Memo.O in
  let dir = path "default/ownership-proof-derived-origin" in
  let original_target = Path.Build.relative dir "original" in
  let derived_target = Path.Build.relative dir "derived" in
  let original_rule = file_rule [ original_target ] in
  let derived_rule = file_rule [ derived_target ] in
  let mode = Memo.Var.create 0 ~name:"derived-origin-preparation-mode" in
  let entered = Fiber.Ivar.create () in
  let release = Fiber.Ivar.create () in
  let original_runs = ref 0 in
  let derived_runs = ref 0 in
  let prepare =
    Memo.lazy_ ~name:"independently used preparation" ~cutoff:Unit.equal (fun () ->
      let* mode = Memo.Var.read mode in
      match mode with
      | 0 -> Memo.return ()
      | 1 ->
        Memo.of_reproducible_fiber
          (let open Fiber.O in
           let* () = Fiber.Ivar.fill entered () in
           Fiber.Ivar.read release)
      | _ -> Code_error.raise "Independent ownership preparation failed" [])
  in
  let value, _original =
    run
      (Rules.collect (fun () ->
         Rules.defer_after
           (Target_mask.files [ original_target ])
           ~prepare:(Memo.Lazy.force prepare)
           (fun () ->
              incr original_runs;
              let+ () = Rules.Produce.rule original_rule in
              original_rule)))
  in
  let derived =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow_after (Target_mask.files [ derived_target ]) value (fun original ->
           assert (original == original_rule);
           incr derived_runs;
           Rules.Produce.rule derived_rule)))
  in
  let since = Memo.Run.For_tests.current () in
  let load () =
    run (Rules.load derived (Target_mask.path derived_target)) |> rules_in ~dir
  in
  assert (List.equal ( == ) (load ()) [ derived_rule ]);
  let result = Rules.Deferred.result value in
  Memo.reset (Memo.Var.set mode 1);
  assert (Rules.unchanged_since derived ~since);
  let during_preparation = ref None in
  let original, () =
    run
      (Memo.fork_and_join
         (fun () -> Memo.Lazy.force result)
         (fun () ->
            Memo.of_reproducible_fiber
              (let open Fiber.O in
               let* () = Fiber.Ivar.read entered in
               during_preparation := Some (Rules.unchanged_since derived ~since);
               Fiber.Ivar.fill release ())))
  in
  assert (original == original_rule);
  assert (!during_preparation = Some false);
  assert (Rules.unchanged_since derived ~since);
  assert (!original_runs = 1 && !derived_runs = 1);
  Memo.reset (Memo.Var.set mode 2);
  assert (
    try
      ignore (run (Memo.Lazy.force result) : Rule.t);
      false
    with
    | Code_error.E { message; _ } ->
      String.equal message "Independent ownership preparation failed");
  (* The derived outer node has not been read since the initial success. Its
     own successful cache is insufficient after the separate origin failed. *)
  assert (not (Rules.unchanged_since derived ~since));
  Memo.reset Memo.Invalidation.empty;
  assert (not (Rules.unchanged_since derived ~since));
  Memo.reset (Memo.Var.set mode 0);
  assert (run (Memo.Lazy.force result) == original_rule);
  assert (Rules.unchanged_since derived ~since);
  assert (List.equal ( == ) (load ()) [ derived_rule ]);
  assert (!original_runs = 1 && !derived_runs = 1);
  [%expect {| |}]
;;

let%expect_test "target masks distinguish kinds and directory boundaries" =
  let dir = path "default/masks" in
  let child = Path.Build.relative dir "child" in
  let file = Path.Build.relative dir "x.ml" in
  let nested = Path.Build.relative child "x.ml" in
  let alias = Alias.make (Alias.Name.of_string "x.ml") ~dir in
  let files = Target_mask.files [ file ] in
  printfn
    "file/directory overlap: %b"
    (Target_mask.intersects files (Target_mask.directories [ file ]));
  printfn
    "file/alias overlap: %b"
    (Target_mask.intersects files (Target_mask.aliases [ alias ]));
  printfn
    "path covers file and directory: %b"
    (Target_mask.mem_file (Target_mask.path file) file
     && Target_mask.mem_directory (Target_mask.path file) file);
  let direct = Target_mask.files_in_directory dir in
  printfn "direct file: %b" (Target_mask.mem_file direct file);
  printfn "nested file: %b" (Target_mask.mem_file direct nested);
  let subtree = Target_mask.subtree dir in
  printfn "subtree nested file: %b" (Target_mask.mem_file subtree nested);
  printfn "subtree directory: %b" (Target_mask.mem_directory subtree child);
  printfn "subtree alias: %b" (Target_mask.mem_alias subtree alias);
  let extensions =
    Target_mask.file_extensions
      ~dir
      (Filename.Extension.Set.singleton Filename.Extension.ml)
  in
  printfn
    "matching extension: %b"
    (Target_mask.mem_file (Target_mask.inter files extensions) file);
  printfn "extension in child: %b" (Target_mask.mem_file extensions nested);
  printfn
    "disjoint intersection: %b"
    (Target_mask.is_empty (Target_mask.inter extensions (Target_mask.files [ nested ])));
  [%expect
    {|
    file/directory overlap: false
    file/alias overlap: false
    path covers file and directory: true
    direct file: true
    nested file: false
    subtree nested file: true
    subtree directory: true
    subtree alias: true
    matching extension: true
    extension in child: false
    disjoint intersection: true
    |}]
;;

let%expect_test "filename predicates preserve precise literal intersections" =
  let dir = path "default/predicates" in
  let matching pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern
    |> Predicate_lang.Glob.of_glob
    |> Target_mask.files_matching ~dir
  in
  let seed = matching "seed" in
  let modules = matching "*.modules" in
  let overlaps label a b =
    printfn "%s: %b / %b" label (Target_mask.intersects a b) (Target_mask.intersects b a)
  in
  overlaps "seed/glob" seed modules;
  overlaps "distinct exact names" seed (matching "other");
  overlaps "matching name/glob" (matching "selected.modules") modules;
  overlaps
    "literal set/glob"
    (Target_mask.files_matching ~dir (Predicate_lang.Glob.of_string_list [ "seed" ]))
    modules;
  printfn
    "disjoint intersections: %b / %b"
    (Target_mask.is_empty (Target_mask.inter seed modules))
    (Target_mask.is_empty (Target_mask.inter modules seed));
  List.iter
    [ "seed"; "selected.modules"; "selected.modules.backup"; "child/x.modules" ]
    ~f:(fun name ->
      printfn "%s: %b" name (Target_mask.mem_file modules (Path.Build.relative dir name)));
  [%expect
    {|
    seed/glob: false / false
    distinct exact names: false / false
    matching name/glob: true / true
    literal set/glob: false / false
    disjoint intersections: true / true
    seed: false
    selected.modules: true
    selected.modules.backup: false
    child/x.modules: false
    |}]
;;

let%expect_test "subtree extension masks preserve suffix and directory boundaries" =
  let dir = path "default/extensions" in
  let child = Path.Build.relative dir "child" in
  let extensions =
    Filename.Extension.Set.singleton (Filename.Extension.of_string_exn ".v.d")
  in
  let mask = Target_mask.file_extensions_in_subtree ~dir extensions in
  List.iter
    [ "default/extensions/x.v.d"
    ; "default/extensions/child/x.v.d"
    ; "default/extensions/child/x.d"
    ; "default/extensions/child/x.v.d.extra"
    ; "default/extensions-other/x.v.d"
    ; "default/x.v.d"
    ]
    ~f:(fun name -> printfn "%s: %b" name (Target_mask.mem_file mask (path name)));
  printfn
    "directory target: %b"
    (Target_mask.mem_directory mask (Path.Build.relative child "x.v.d"));
  printfn
    "alias: %b"
    (Target_mask.mem_alias mask (Alias.make (Alias.Name.of_string "x.v.d") ~dir:child));
  let direct =
    Target_mask.file_extensions
      ~dir:child
      (Filename.Extension.Set.singleton Filename.Extension.d)
  in
  let intersection = Target_mask.inter mask direct in
  let reverse = Target_mask.inter direct mask in
  List.iter [ "child/x.v.d"; "child/x.d"; "x.v.d" ] ~f:(fun name ->
    let file = Path.Build.relative dir name in
    printfn
      "intersection %s: %b / %b"
      name
      (Target_mask.mem_file intersection file)
      (Target_mask.mem_file reverse file));
  printfn
    "disjoint suffix: %b"
    (Target_mask.intersects
       mask
       (Target_mask.file_extensions_in_subtree
          ~dir
          (Filename.Extension.Set.singleton Filename.Extension.ml)));
  printfn
    "disjoint subtree: %b"
    (Target_mask.intersects
       mask
       (Target_mask.file_extensions_in_subtree
          ~dir:(path "default/extensions-other")
          extensions));
  [%expect
    {|
    default/extensions/x.v.d: true
    default/extensions/child/x.v.d: true
    default/extensions/child/x.d: false
    default/extensions/child/x.v.d.extra: false
    default/extensions-other/x.v.d: false
    default/x.v.d: false
    directory target: false
    alias: false
    intersection child/x.v.d: true / true
    intersection child/x.d: false / false
    intersection x.v.d: false / false
    disjoint suffix: false
    disjoint subtree: false
    |}]
;;

let%expect_test "filename predicates exclude disjoint output suffixes" =
  let dir = path "default/glob-suffixes" in
  let outputs =
    Target_mask.file_extensions_in_subtree
      ~dir
      (Filename.Extension.Set.singleton (Filename.Extension.of_string_exn ".vo"))
  in
  let matches predicate =
    let inputs =
      Target_mask.files_matching ~dir:(Path.Build.relative dir "inputs") predicate
    in
    Target_mask.intersects inputs outputs, Target_mask.intersects outputs inputs
  in
  let glob pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern |> Predicate_lang.Glob.of_glob
  in
  List.iter [ "*.v"; "*.vo"; "{foo,bar}.v"; "*.v[o]"; "*\\?.v"; "*" ] ~f:(fun pattern ->
    let forward, reverse = matches (glob pattern) in
    printfn "%s: %b / %b" pattern forward reverse);
  let print label predicate =
    let forward, reverse = matches predicate in
    printfn "%s: %b / %b" label forward reverse
  in
  print "union" (Predicate_lang.or_ [ glob "*.v"; glob "*.ml" ]);
  print "intersection" (Predicate_lang.and_ [ glob "*"; glob "*.v" ]);
  print "complement" (Predicate_lang.not (glob "*.v"));
  [%expect
    {|
    *.v: false / false
    *.vo: true / true
    {foo,bar}.v: false / false
    *.v[o]: true / true
    *\?.v: false / false
    *: true / true
    union: false / false
    intersection: false / false
    complement: true / true
    |}]
;;

let%expect_test "glob intersections preserve dot-prefixed extension matches" =
  let dir = path "default/hidden-extensions" in
  let extensions =
    Target_mask.file_extensions
      ~dir
      (Filename.Extension.Set.singleton (Filename.Extension.of_string_exn ".vo"))
  in
  List.iter [ "**"; ".*"; "*.vo" ] ~f:(fun pattern ->
    let glob =
      Dune_lang.Glob.of_string_exn Loc.none pattern
      |> Predicate_lang.Glob.of_glob
      |> Target_mask.files_matching ~dir
    in
    let forward = Target_mask.inter extensions glob in
    let reverse = Target_mask.inter glob extensions in
    List.iter [ ".vo"; ".hidden.vo"; "x.vo"; "x.vo.bak" ] ~f:(fun name ->
      let file = Path.Build.relative dir name in
      printfn
        "%s / %s: %b / %b"
        pattern
        name
        (Target_mask.mem_file forward file)
        (Target_mask.mem_file reverse file)));
  [%expect
    {|
    ** / .vo: true / true
    ** / .hidden.vo: true / true
    ** / x.vo: true / true
    ** / x.vo.bak: false / false
    .* / .vo: true / true
    .* / .hidden.vo: true / true
    .* / x.vo: false / false
    .* / x.vo.bak: false / false
    *.vo / .vo: false / false
    *.vo / .hidden.vo: false / false
    *.vo / x.vo: true / true
    *.vo / x.vo.bak: false / false
    |}]
;;

let%expect_test "alias directory projection ignores file-only regions" =
  let dir = path "default/alias-directories" in
  let child = Path.Build.relative dir "generated/deep" in
  let exact =
    Target_mask.aliases [ Alias.make (Alias.Name.of_string "check") ~dir:child ]
  in
  let print label mask =
    let directories = Target_mask.alias_directories mask ~dir in
    match Dir_set.toplevel_subdirs directories with
    | Infinite -> printfn "%s: infinite" label
    | Finite names ->
      let names =
        Filename.Set.to_list names
        |> List.map ~f:Filename.to_string
        |> String.concat ~sep:", "
      in
      printfn "%s: %s" label names
  in
  print "exact alias" exact;
  print "aliases in directory" (Target_mask.aliases_in_directory child);
  let files =
    Target_mask.file_extensions_in_subtree
      ~dir:(Path.Build.relative dir "files")
      (Filename.Extension.Set.singleton Filename.Extension.ml)
  in
  print "alias and file-only subtree" (Target_mask.union exact files);
  print "alias subtree" (Target_mask.subtree dir);
  [%expect
    {|
    exact alias: generated
    aliases in directory: generated
    alias and file-only subtree: generated
    alias subtree: infinite
    |}]
;;

let%expect_test "subtrees use alias directories, not alias names" =
  let dir = path "default/alias-boundaries" in
  let child = Path.Build.relative dir "child" in
  let parent_alias = Alias.make (Alias.Name.of_string "child") ~dir in
  let child_alias = Alias.make (Alias.Name.of_string "check") ~dir:child in
  let subtree = Target_mask.subtree child in
  let parent = Target_mask.aliases [ parent_alias ] in
  printfn "parent alias membership: %b" (Target_mask.mem_alias subtree parent_alias);
  printfn
    "parent alias intersection: %b / %b"
    (Target_mask.intersects subtree parent)
    (Target_mask.intersects parent subtree);
  printfn
    "parent alias ownership: %b"
    (Target_mask.alias_directories subtree ~dir |> Dir_set.here);
  printfn "child alias membership: %b" (Target_mask.mem_alias subtree child_alias);
  printfn
    "child alias ownership: %b"
    (Target_mask.alias_directories subtree ~dir:child |> Dir_set.here);
  [%expect
    {|
    parent alias membership: false
    parent alias intersection: false / false
    parent alias ownership: false
    child alias membership: true
    child alias ownership: true
    |}]
;;

let%expect_test "directory cleanup only considers contained outputs" =
  let dir = path "default/cleanup-boundaries" in
  let child = Path.Build.relative dir "child" in
  let alias name ~dir = Alias.make (Alias.Name.of_string name) ~dir in
  List.iter
    [ "parent aliases", Target_mask.aliases_in_directory dir
    ; "parent alias with same name", Target_mask.aliases [ alias "child" ~dir ]
    ; "file at root", Target_mask.files [ child ]
    ; "descendant file", Target_mask.files [ Path.Build.relative child "out" ]
    ; "exact directory", Target_mask.directories [ child ]
    ; "child alias", Target_mask.aliases [ alias "check" ~dir:child ]
    ; "sibling file", Target_mask.files [ Path.Build.relative dir "sibling/out" ]
    ]
    ~f:(fun (label, mask) ->
      printfn "%s: %b" label (Target_mask.intersects_directory mask child));
  [%expect
    {|
    parent aliases: false
    parent alias with same name: false
    file at root: false
    descendant file: true
    exact directory: true
    child alias: true
    sibling file: false
    |}]
;;

let%expect_test "target mask set operations preserve membership" =
  let dir = path "default/mask-laws" in
  let child = Path.Build.relative dir "child" in
  let relative = Path.Build.relative dir in
  let alias path =
    Alias.make
      (Alias.Name.of_string (Filename.to_string (Path.Build.basename path)))
      ~dir:(Path.Build.parent_exn path)
  in
  let glob pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern |> Predicate_lang.Glob.of_glob
  in
  let extensions names =
    List.map names ~f:Filename.Extension.of_string_exn |> Filename.Extension.Set.of_list
  in
  let masks =
    [ "empty", Target_mask.empty
    ; "all", Target_mask.all
    ; "subtree", Target_mask.subtree dir
    ; "child subtree", Target_mask.subtree child
    ; "single file", Target_mask.files [ relative "x.ml" ]
    ; "single path", Target_mask.path (relative "x.ml")
    ; "path at build root", Target_mask.path (path "default")
    ; "path at subtree root", Target_mask.path dir
    ; "path at child subtree root", Target_mask.path child
    ; ( "path names"
      , Target_mask.paths
          ~dir
          (List.map [ "x.ml"; "x.v.d" ] ~f:Filename.of_string_exn |> Filename.Set.of_list)
      )
    ; ( "shared locations"
      , Target_mask.union
          (Target_mask.path (relative "x.ml"))
          (Target_mask.path (relative "child/x.ml")) )
    ; ( "shared direct and recursive"
      , Target_mask.union (Target_mask.path (relative "x.ml")) (Target_mask.subtree child)
      )
    ; ( "equal file and directory owners"
      , Target_mask.union
          (Target_mask.files [ relative "x.ml" ])
          (Target_mask.directories [ relative "x.ml" ]) )
    ; ( "distinct file and directory owners"
      , Target_mask.union
          (Target_mask.files [ relative "x.ml" ])
          (Target_mask.directories [ child ]) )
    ; "single directory", Target_mask.directories [ child ]
    ; "single alias", Target_mask.aliases [ alias child ]
    ; ( "point with alias"
      , Target_mask.union
          (Target_mask.path (relative "x.ml"))
          (Target_mask.aliases [ alias child ]) )
    ; "file names", Target_mask.files [ dir; child; relative "x.ml"; relative "x.v.d" ]
    ; "other file names", Target_mask.files [ relative "x.ml"; relative "y.ml" ]
    ; "directory names", Target_mask.directories [ dir; child; relative "x.ml" ]
    ; ( "alias names"
      , Target_mask.aliases [ alias dir; alias child; alias (relative "x.ml") ] )
    ; "direct files", Target_mask.files_in_directory dir
    ; "child files", Target_mask.files_in_directory child
    ; "direct directories", Target_mask.directories_in_directory dir
    ; "direct aliases", Target_mask.aliases_in_directory dir
    ; "child aliases", Target_mask.aliases_in_directory child
    ; "extensions", Target_mask.file_extensions ~dir (extensions [ ".ml"; ".v.d" ])
    ; ( "subtree extensions"
      , Target_mask.file_extensions_in_subtree ~dir (extensions [ ".d" ]) )
    ; "glob", Target_mask.files_matching ~dir (glob "x*")
    ; "suffix glob", Target_mask.files_matching ~dir (glob "*.ml")
    ; "hidden glob", Target_mask.files_matching ~dir (glob ".*")
    ; "all files glob", Target_mask.files_matching ~dir (glob "**")
    ; ( "literal predicate"
      , Target_mask.files_matching
          ~dir
          (Predicate_lang.Glob.of_string_list [ "x.ml"; "x.v.d"; ".ml" ]) )
    ; "path predicate", Target_mask.paths_matching ~dir (glob "x*")
    ; ( "mixed selectors"
      , List.fold_left
          [ Target_mask.files [ relative "y.ml"; relative "child/x.ml" ]
          ; Target_mask.file_extensions ~dir (extensions [ ".mli" ])
          ; Target_mask.files_matching ~dir (glob "x*")
          ; Target_mask.files_matching ~dir (glob ".*")
          ]
          ~init:Target_mask.empty
          ~f:Target_mask.union )
    ; ( "overlapping subtrees"
      , Target_mask.union
          (Target_mask.file_extensions_in_subtree ~dir (extensions [ ".ml" ]))
          (Target_mask.subtree child) )
    ; ( "intersected globs"
      , Target_mask.inter
          (Target_mask.files_matching ~dir (glob "x*"))
          (Target_mask.files_matching ~dir (glob "*.ml")) )
    ; ( "disjoint globs"
      , Target_mask.inter
          (Target_mask.files_matching ~dir (glob "x*"))
          (Target_mask.files_matching ~dir (glob "y*")) )
    ]
  in
  let samples =
    [ path "default"
    ; dir
    ; child
    ; relative "x.ml"
    ; relative "y.ml"
    ; relative "x.mli"
    ; relative ".ml"
    ; relative ".hidden.ml"
    ; relative "x.v.d"
    ; relative "x.d"
    ; relative "child/x.ml"
    ; relative "child/x.v.d"
    ; relative "child/deep/y.ml"
    ; path "default/mask-laws-other/x.ml"
    ]
  in
  let kinds =
    [ "file", Target_mask.mem_file
    ; "directory", Target_mask.mem_directory
    ; ("alias", fun mask path -> Target_mask.mem_alias mask (alias path))
    ]
  in
  let check label expected actual =
    if Bool.equal expected actual then () else printfn "failed: %s" label
  in
  let check_path_membership label mask =
    List.iter samples ~f:(fun sample ->
      let dir = Path.Build.parent_exn sample in
      let name = Path.Build.basename sample in
      let expected =
        Target_mask.mem_file mask sample || Target_mask.mem_directory mask sample
      in
      check
        (label ^ " file or directory membership")
        expected
        (Target_mask.mem_path mask ~dir name);
      let point = Target_mask.path sample in
      check (label ^ " point intersection") expected (Target_mask.intersects point mask);
      check
        (label ^ " reversed point intersection")
        expected
        (Target_mask.intersects mask point))
  in
  List.iter masks ~f:(fun (a_name, a) ->
    check_path_membership a_name a;
    List.iter masks ~f:(fun (b_name, b) ->
      let union = Target_mask.union a b in
      let intersection = Target_mask.inter a b in
      let reverse = Target_mask.inter b a in
      let intersects = Target_mask.intersects a b in
      let pair = a_name ^ " / " ^ b_name in
      check_path_membership (pair ^ " union") union;
      check_path_membership (pair ^ " intersection") intersection;
      check_path_membership (pair ^ " reverse") reverse;
      check (pair ^ " intersection symmetry") intersects (Target_mask.intersects b a);
      check
        (pair ^ " nonempty intersection")
        intersects
        (not (Target_mask.is_empty intersection));
      List.iter kinds ~f:(fun (kind, mem) ->
        List.iter samples ~f:(fun sample ->
          let label = pair ^ " / " ^ kind ^ " / " ^ Path.Build.to_string sample in
          let in_a = mem a sample in
          let in_b = mem b sample in
          check (label ^ " union") (in_a || in_b) (mem union sample);
          check (label ^ " intersection") (in_a && in_b) (mem intersection sample);
          check (label ^ " reverse") (mem intersection sample) (mem reverse sample);
          if in_a && in_b then check (label ^ " overlap") true intersects))));
  [%expect {| |}]
;;

let%expect_test "exact file names preserve normalized selector sets" =
  let dir = path "default/exact-file-names" in
  let other = Path.Build.relative dir "other" in
  let names values = List.map values ~f:Filename.of_string_exn |> Filename.Set.of_list in
  let files values = Target_mask.files_named ~dir (names values) in
  let one values = Some [ dir, values ] in
  let finite = files [ "a.ml"; "b.txt"; "c.other"; "d.other" ] in
  let nonexact =
    Target_mask.union
      (Target_mask.file_extensions
         ~dir
         (Filename.Extension.Set.singleton Filename.Extension.ml))
      (Target_mask.files_matching ~dir (Predicate_lang.Glob.of_string "b*"))
  in
  let mixed = Target_mask.union (files [ "c.other" ]) nonexact in
  let other_kinds =
    Target_mask.union
      (Target_mask.directories_in_directory dir)
      (Target_mask.aliases_in_directory other)
  in
  List.iter
    [ "empty", Target_mask.empty, Some []
    ; "empty names", files [], Some []
    ; "single", files [ "a.ml" ], one [ "a.ml" ]
    ; "path", Target_mask.path (Path.Build.relative dir "a.ml"), one [ "a.ml" ]
    ; "multiple", finite, one [ "a.ml"; "b.txt"; "c.other"; "d.other" ]
    ; ( "paths in multiple directories"
      , Target_mask.files
          [ Path.Build.relative dir "a.ml"; Path.Build.relative other "b.txt" ]
      , Some [ dir, [ "a.ml" ]; other, [ "b.txt" ] ] )
    ; ( "finite predicate"
      , Target_mask.files_matching
          ~dir
          (Predicate_lang.Glob.of_string_list [ "c.other"; "a.ml" ])
      , one [ "a.ml"; "c.other" ] )
    ; ( "exact union"
      , Target_mask.union (files [ "a.ml"; "c.other" ]) (files [ "b.txt"; "a.ml" ])
      , one [ "a.ml"; "b.txt"; "c.other" ] )
    ; "mixed selectors", mixed, None
    ; "reversed mixed selectors", Target_mask.union nonexact (files [ "c.other" ]), None
    ; ( "mixed intersection"
      , Target_mask.inter mixed finite
      , one [ "a.ml"; "b.txt"; "c.other" ] )
    ; ( "reversed intersection"
      , Target_mask.inter finite mixed
      , one [ "a.ml"; "b.txt"; "c.other" ] )
    ; ( "empty intersection"
      , Target_mask.inter (files [ "a.ml" ]) (files [ "b.txt" ])
      , Some [] )
    ; "recursive", Target_mask.union finite (Target_mask.subtree other), None
    ; "other kinds", Target_mask.union (files [ "a.ml" ]) other_kinds, one [ "a.ml" ]
    ; "empty file component", other_kinds, Some []
    ]
    ~f:(fun (label, mask, expected) ->
      let expected =
        Option.map expected ~f:(fun entries ->
          List.fold_left entries ~init:Path.Build.Map.empty ~f:(fun acc (dir, values) ->
            Path.Build.Map.set acc dir (names values)))
      in
      let equal a b = Path.Build.Map.equal a b ~equal:Filename.Set.equal in
      if not (Option.equal equal (Target_mask.exact_file_names mask) expected)
      then printfn "failed: %s" label);
  [%expect {| |}]
;;

let%expect_test "point intersections with many recursive roots" =
  let dir = path "default/many-roots" in
  let roots =
    List.init 17 ~f:(fun index -> Path.Build.relative dir (Int.to_string index))
  in
  let filtered_roots = List.map [ "x.mli"; "y.other" ] ~f:(Path.Build.relative dir) in
  let filtered =
    List.map filtered_roots ~f:(fun dir ->
      Target_mask.file_extensions_in_subtree
        ~dir
        (Filename.Extension.Set.singleton Filename.Extension.mli))
  in
  let root_filter =
    Target_mask.file_extensions_in_subtree
      ~dir:Path.Build.root
      (Filename.Extension.Set.singleton Filename.Extension.ml)
  in
  let masks = root_filter :: (filtered @ List.map roots ~f:Target_mask.subtree) in
  let combined = List.fold_left masks ~init:Target_mask.empty ~f:Target_mask.union in
  let points =
    [ dir
    ; path "default/unrelated"
    ; path "default/unrelated/child.mli"
    ; path "root.ml"
    ; path "root.mli"
    ; Path.Build.relative dir "absent"
    ]
    @ List.concat_map (filtered_roots @ roots) ~f:(fun root ->
      [ root
      ; Path.Build.relative root "child.ml"
      ; Path.Build.relative root "child.mli"
      ; Path.Build.relative root "nested/child.mli"
      ])
  in
  let check masks combined =
    List.iter points ~f:(fun point ->
      let alias =
        Alias.make
          (Path.Build.basename point |> Filename.to_string |> Alias.Name.of_string)
          ~dir:(Path.Build.parent_exn point)
      in
      List.iter
        [ "file", Target_mask.files [ point ]
        ; "directory", Target_mask.directories [ point ]
        ; "path", Target_mask.path point
        ; "alias", Target_mask.aliases [ alias ]
        ]
        ~f:(fun (kind, request) ->
          let expected = List.exists masks ~f:(Target_mask.intersects request) in
          if
            not
              (Bool.equal expected (Target_mask.intersects request combined)
               && Bool.equal expected (Target_mask.intersects combined request))
          then printfn "%s: mismatch for %s" kind (Path.Build.to_string point)))
  in
  check masks combined;
  let extra = Target_mask.subtree (path "default/unrelated") in
  let extended = Target_mask.union combined extra in
  check (extra :: masks) extended;
  check masks combined;
  check masks (Target_mask.inter extended combined);
  [%expect {| |}]
;;

let%expect_test "relative target mask membership matches full paths" =
  let dir = path "default/relative-membership" in
  let child = Path.Build.relative dir "child" in
  let other = path "default/other" in
  let extensions dir extension =
    Target_mask.file_extensions_in_subtree
      ~dir
      (Filename.Extension.Set.singleton extension)
  in
  let files = Target_mask.files [ Path.Build.relative dir "x.ml" ] in
  let directories = Target_mask.directories [ child ] in
  let masks =
    [ "empty", Target_mask.empty
    ; "all", Target_mask.all
    ; "exact file", files
    ; "exact directory", directories
    ; "subtree", Target_mask.subtree child
    ; ( "multiple roots"
      , Target_mask.union (Target_mask.subtree child) (Target_mask.subtree other) )
    ; "extensions", extensions dir Filename.Extension.ml
    ; ( "different suffixes"
      , Target_mask.union
          (extensions child Filename.Extension.mli)
          (extensions dir Filename.Extension.ml) )
    ; ( "exact child root"
      , Target_mask.union
          (extensions (Path.Build.relative dir "x.mli") Filename.Extension.mli)
          (extensions other Filename.Extension.ml) )
    ; ( "mixed kinds"
      , Target_mask.union
          (Target_mask.union files directories)
          (Target_mask.union
             (extensions dir Filename.Extension.ml)
             (Target_mask.subtree other)) )
    ; "alias only", Target_mask.aliases [ Alias.make (Alias.Name.of_string "child") ~dir ]
    ]
  in
  List.iter masks ~f:(fun (label, mask) ->
    List.iter
      [ Path.Build.root
      ; path "default"
      ; dir
      ; child
      ; Path.Build.relative child "deep"
      ; other
      ; path "default/relative-membership-other"
      ]
      ~f:(fun dir ->
        List.iter
          [ "default"; "other"; "child"; "x.ml"; "x.mli"; ".hidden.ml"; "deep" ]
          ~f:(fun name ->
            let name = Filename.of_string_exn name in
            let file = Path.Build.relative_fname dir name in
            let is_file = Target_mask.mem_file mask file in
            let is_directory = Target_mask.mem_directory mask file in
            if
              not
                (Bool.equal is_file (Target_mask.mem_file_name mask ~dir name)
                 && Bool.equal
                      is_directory
                      (Target_mask.mem_directory_name mask ~dir name)
                 && Bool.equal
                      (is_file || is_directory)
                      (Target_mask.mem_path mask ~dir name))
            then printfn "%s: mismatch for %s" label (Path.Build.to_string file))));
  [%expect {| |}]
;;

let%expect_test "enclosing subtrees preserve child masks" =
  let dir = path "default/mask-sharing" in
  let child = Path.Build.relative dir "child" in
  let file = Path.Build.relative dir "x.ml" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  let parent = Target_mask.subtree dir in
  let extensions = Filename.Extension.Set.singleton Filename.Extension.ml in
  List.iter
    [ "file", Target_mask.files [ file ]
    ; "file at root", Target_mask.files [ dir ]
    ; "directory", Target_mask.directories [ child ]
    ; "directory at root", Target_mask.directories [ dir ]
    ; "alias", Target_mask.aliases [ alias ]
    ; "same subtree", Target_mask.subtree dir
    ; "child subtree", Target_mask.subtree child
    ; "extensions", Target_mask.file_extensions ~dir extensions
    ; "subtree extensions", Target_mask.file_extensions_in_subtree ~dir:child extensions
    ; "multiple directories", Target_mask.files [ file; Path.Build.relative child "x.ml" ]
    ; ( "mixed kinds"
      , Target_mask.union (Target_mask.path file) (Target_mask.aliases [ alias ]) )
    ]
    ~f:(fun (label, mask) ->
      printfn "%s: %b" label (Target_mask.inter parent mask == mask));
  let parent_alias =
    Alias.make (Alias.Name.of_string "mask-sharing") ~dir:(Path.Build.parent_exn dir)
  in
  printfn
    "parent alias excluded: %b"
    (Target_mask.inter parent (Target_mask.aliases [ parent_alias ])
     |> Target_mask.is_empty);
  let outside = path "default/mask-sharing-other/x.ml" in
  let narrowed = Target_mask.inter parent (Target_mask.files [ file; outside ]) in
  printfn "inside file retained: %b" (Target_mask.mem_file narrowed file);
  printfn "outside file excluded: %b" (not (Target_mask.mem_file narrowed outside));
  [%expect
    {|
    file: true
    file at root: true
    directory: true
    directory at root: true
    alias: true
    same subtree: true
    child subtree: true
    extensions: true
    subtree extensions: true
    multiple directories: true
    mixed kinds: true
    parent alias excluded: true
    inside file retained: true
    outside file excluded: true
    |}]
;;

let%expect_test "only a singleton filename fits a subtree root boundary" =
  let dir = path "default/root-boundary" in
  let root = Path.Build.relative dir "root.ml" in
  let sibling = Path.Build.relative dir "sibling.ml" in
  let parent = Target_mask.subtree root in
  let exact = Target_mask.files [ root ] in
  let multiple = Target_mask.files [ root; sibling ] in
  List.iter
    [ "single", exact
    ; "duplicate filenames", Target_mask.files [ root; root ]
    ; "duplicate masks", Target_mask.union exact (Target_mask.files [ root ])
    ; "narrowed set", Target_mask.inter multiple exact
    ]
    ~f:(fun (label, mask) ->
      printfn
        "%s: inter shared %b, union shared %b"
        label
        (Target_mask.inter parent mask == mask)
        (Target_mask.union parent mask == parent));
  let glob =
    Dune_lang.Glob.of_string_exn Loc.none "*.ml"
    |> Predicate_lang.Glob.of_glob
    |> Target_mask.files_matching ~dir
  in
  List.iter
    [ "multiple names", multiple
    ; "all names", Target_mask.files_in_directory dir
    ; ( "extensions"
      , Target_mask.file_extensions
          ~dir
          (Filename.Extension.Set.singleton Filename.Extension.ml) )
    ; "glob", glob
    ; "literal and glob", Target_mask.union exact glob
    ]
    ~f:(fun (label, mask) ->
      let intersection = Target_mask.inter parent mask in
      let union = Target_mask.union parent mask in
      printfn
        "%s: shared %b, root %b, sibling %b / %b"
        label
        (intersection == mask || union == parent)
        (Target_mask.mem_file intersection root)
        (Target_mask.mem_file intersection sibling)
        (Target_mask.mem_file union sibling));
  [%expect
    {|
    single: inter shared true, union shared true
    duplicate filenames: inter shared true, union shared true
    duplicate masks: inter shared true, union shared true
    narrowed set: inter shared true, union shared true
    multiple names: shared false, root true, sibling false / true
    all names: shared false, root true, sibling false / true
    extensions: shared false, root true, sibling false / true
    glob: shared false, root true, sibling false / true
    literal and glob: shared false, root true, sibling false / true
    |}]
;;

let%expect_test "direct masks distinguish immediate subtree roots" =
  List.iter
    [ "direct child", "default/parent", "default/parent/child"
    ; "nested child", "default/parent", "default/parent/child/deep"
    ; "same root", "default/parent", "default/parent"
    ; "build root child", ".", "./child"
    ; "build root nested child", ".", "child/deep"
    ; "build root itself", ".", "."
    ; "lookalike prefix", "default/parent", "default/parent-other/child"
    ; "ancestor root", "default/parent/child", "default/parent"
    ]
    ~f:(fun (label, dir, root) ->
      let dir = path dir in
      let subtree = Target_mask.subtree (path root) in
      let overlaps direct =
        let forward = Target_mask.intersects direct subtree in
        let reverse = Target_mask.intersects subtree direct in
        let nonempty = not (Target_mask.is_empty (Target_mask.inter direct subtree)) in
        if not (Bool.equal forward reverse && Bool.equal forward nonempty)
        then printfn "%s: intersection law failed" label;
        forward
      in
      printfn
        "%s: files %b, directories %b, aliases %b"
        label
        (overlaps (Target_mask.files_in_directory dir))
        (overlaps (Target_mask.directories_in_directory dir))
        (overlaps (Target_mask.aliases_in_directory dir)));
  [%expect
    {|
    direct child: files true, directories true, aliases false
    nested child: files false, directories false, aliases false
    same root: files true, directories true, aliases true
    build root child: files true, directories true, aliases false
    build root nested child: files false, directories false, aliases false
    build root itself: files true, directories true, aliases true
    lookalike prefix: files false, directories false, aliases false
    ancestor root: files true, directories true, aliases true
    |}]
;;

let%expect_test "action masks reuse target inference without expanding variables" =
  let module A = Dune_rules.For_tests.Action_unexpanded in
  let module S = Dune_lang.String_with_vars in
  let dir = path "default/action-targets" in
  let text = S.make_text Loc.none in
  let variable = S.make_pform Loc.none (Var (User_var "output")) in
  let write target = A.Write_file (target, Normal, text "contents") in
  let dynamic = write variable in
  let optional_diff =
    A.Diff
      { Action_types.Diff.file1 = text "expected"
      ; file2 = text "out"
      ; optional = true
      ; mode = Text
      ; directory_diffs = true
      }
  in
  List.iter
    [ "literal", write (text "out")
    ; "dynamic", dynamic
    ; "chdir", A.Chdir (text "child", write (text "../out"))
    ; "dynamic chdir", A.Chdir (variable, write (text "out"))
    ; "escaping target", write (text "../../../out")
    ; "escaping chdir", A.Chdir (text "../../../outside", write (text "out"))
    ; "no-infer", A.No_infer dynamic
    ; "run", A.run (S.make_pform Loc.none (Var Test)) []
    ; "line directive", A.Copy_and_add_line_directive (text "input", text "out")
    ; "format", A.Format_dune_file (text "input", text "out")
    ; "consumed", A.Progn [ write (text "out"); optional_diff ]
    ]
    ~f:(fun (label, action) ->
      let mask = A.rule_targets ~dir ~targets:Infer action in
      printfn
        "%s (out / other): %b / %b"
        label
        (Target_mask.mem_file mask (Path.Build.relative dir "out"))
        (Target_mask.mem_file mask (Path.Build.relative dir "other")));
  let dynamic = A.rule_targets ~dir ~targets:Infer dynamic in
  printfn
    "dynamic descendant: %b"
    (Target_mask.mem_file dynamic (Path.Build.relative dir "child/out"));
  printfn
    "dynamic directory: %b"
    (Target_mask.mem_directory dynamic (Path.Build.relative dir "out"));
  [%expect
    {|
    literal (out / other): true / false
    dynamic (out / other): true / true
    chdir (out / other): true / false
    dynamic chdir (out / other): true / true
    escaping target (out / other): true / true
    escaping chdir (out / other): true / true
    no-infer (out / other): false / false
    run (out / other): false / false
    line directive (out / other): true / false
    format (out / other): true / false
    consumed (out / other): false / false
    dynamic descendant: false
    dynamic directory: false
    |}]
;;

let%expect_test "action target variables preserve singleton declaration precision" =
  let module A = Dune_rules.For_tests.Action_unexpanded in
  let module S = Dune_lang.String_with_vars in
  let dir = path "default/declared-action-targets" in
  let text = S.make_text Loc.none in
  let target = S.make_pform Loc.none (Var Target) in
  let targets = S.make_pform Loc.none (Var Targets) in
  let declaration multiplicity names : _ Dune_lang.Targets_spec.t =
    Static
      { targets =
          List.map names ~f:(fun name -> text name, Dune_lang.Targets_spec.Kind.File)
      ; multiplicity
      }
  in
  let single = declaration One [ "out" ] in
  let multiple = declaration Multiple [ "out" ] in
  let write target = A.Write_file (target, Normal, text "contents") in
  List.iter
    [ "target", single, write target
    ; "targets", multiple, write targets
    ; "escaping target", declaration One [ "../../../out" ], write target
    ; "escaping targets", declaration Multiple [ "../../../out" ], write targets
    ; "chdir target", single, A.Chdir (text "child", write target)
    ; ( "dynamic chdir target"
      , single
      , A.Chdir (S.make_pform Loc.none (Var (User_var "dir")), write target) )
    ]
    ~f:(fun (label, targets, action) ->
      let mask = A.rule_targets ~dir ~targets action in
      printfn
        "%s (out / other): %b / %b"
        label
        (Target_mask.mem_file mask (Path.Build.relative dir "out"))
        (Target_mask.mem_file mask (Path.Build.relative dir "other")));
  let mask =
    A.rule_targets
      ~dir
      ~targets:(declaration Multiple [ "out"; "other" ])
      (write (S.make_pform ~quoted:true Loc.none (Var Targets)))
  in
  printfn
    "quoted multi-target joined filename: %b"
    (Target_mask.mem_file mask (Path.Build.relative dir "out other"));
  [%expect
    {|
    target (out / other): true / false
    targets (out / other): true / false
    escaping target (out / other): true / true
    escaping targets (out / other): true / true
    chdir target (out / other): true / false
    dynamic chdir target (out / other): true / false
    quoted multi-target joined filename: true
    |}]
;;

let%expect_test "nested stages are pulled selectively and shared" =
  let dir = path "default/nested" in
  let x = Path.Build.relative dir "x.ml" in
  let y = Path.Build.relative dir "y.ml" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           printfn "force outer";
           let* () =
             Rules.narrow
               (Target_mask.files [ x; y ])
               (fun () ->
                  printfn "force inner";
                  let* () =
                    Rules.narrow (Target_mask.files [ x ]) (fun () ->
                      printfn "force x";
                      Rules.Produce.rule (file_rule [ x ]))
                  in
                  Rules.narrow (Target_mask.files [ y ]) (fun () ->
                    printfn "force y";
                    Rules.Produce.rule (file_rule [ y ])))
           in
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             printfn "force alias";
             Rules.Produce.Alias.add_deps alias (Action_builder.return ()))))
     in
     printfn "collected";
     let* first = Rules.load tree (Target_mask.files [ x ]) in
     print_files "first" ~dir first;
     let* repeated = Rules.load tree (Target_mask.files [ x ]) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir repeated));
     let* second = Rules.load tree (Target_mask.files [ y ]) in
     print_files "second" ~dir second;
     let+ aliases = Rules.load tree (Target_mask.aliases [ alias ]) in
     let { Rules.Dir_rules.aliases; _ } =
       Rules.find aliases (Path.build dir) |> Rules.Dir_rules.consume
     in
     printfn "aliases: %d" (Alias.Name.Map.cardinal aliases));
  [%expect
    {|
    collected
    force outer
    force inner
    force x
    first: x.ml
    same rules: true
    force y
    second: y.ml
    force alias
    aliases: 1
    |}]
;;

let%expect_test "cleanup refinements retain producer identities across requests" =
  let dir = path "default/refinements" in
  let x = Path.Build.relative dir "x" in
  let y = Path.Build.relative dir "y" in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           Memo.parallel_iter [ x; y ] ~f:(fun target ->
             Rules.narrow (Target_mask.files [ target ]) (fun () ->
               Rules.Produce.rule (file_rule [ target ])))))
     in
     let ids (loaded : Rules.loaded) =
       List.map loaded.refinements ~f:(fun (refinement : Rules.refinement) ->
         refinement.id)
       |> Rules.Producer_id.Set.of_list
     in
     let* first = Rules.load_with_pending tree (Target_mask.path x) in
     let* repeated = Rules.load_with_pending tree (Target_mask.path x) in
     let+ second = Rules.load_with_pending tree (Target_mask.path y) in
     printfn "first refinements: %d" (List.length first.refinements);
     printfn
       "repeated identities: %b"
       (Rules.Producer_id.Set.equal (ids first) (ids repeated));
     printfn
       "shared ancestor: %d"
       (Rules.Producer_id.Set.inter (ids first) (ids second)
        |> Rules.Producer_id.Set.cardinal));
  [%expect
    {|
    first refinements: 2
    repeated identities: true
    shared ancestor: 1
    |}]
;;

let%expect_test "child masks are restricted by their parent" =
  let dir = path "default/owned" in
  let inside = Path.Build.relative dir "x" in
  let outside = path "default/other/x" in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow Target_mask.all (fun () ->
           let* () =
             Rules.narrow (Target_mask.files [ inside ]) (fun () ->
               printfn "force inside";
               Rules.Produce.rule (file_rule [ inside ]))
           in
           Rules.narrow (Target_mask.files [ outside ]) (fun () ->
             printfn "force outside";
             Rules.Produce.rule (file_rule [ outside ]))))
     in
     let tree = Rules.restrict tree (Target_mask.subtree dir) in
     printfn "outside declared: %b" (Target_mask.mem_file (Rules.targets tree) outside);
     let* outside_rules = Rules.load tree (Target_mask.files [ outside ]) in
     printfn "outside rules: %d" (List.length (rules_in ~dir outside_rules));
     let+ inside_rules = Rules.load tree (Target_mask.files [ inside ]) in
     print_files "inside" ~dir inside_rules);
  [%expect
    {|
    outside declared: false
    outside rules: 0
    force inside
    inside: x
    |}]
;;

let%expect_test "repeated restrictions preserve matching and selective loading" =
  let dir = path "default/repeated-restrictions" in
  let x = Path.Build.relative dir "x.ml" in
  let y = Path.Build.relative dir "y.ml" in
  let subtree = Target_mask.subtree dir in
  let duplicated = Target_mask.union subtree subtree in
  let matching pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern
    |> Predicate_lang.Glob.of_glob
    |> Target_mask.files_matching ~dir
  in
  let ml = matching "*.ml" in
  let starts_with_x = matching "x*" in
  let restrictions = [ duplicated; ml; starts_with_x; starts_with_x; ml; duplicated ] in
  let once = Target_mask.inter ml starts_with_x in
  let repeated = List.fold_left restrictions ~init:duplicated ~f:Target_mask.inter in
  List.iter [ "x.ml"; "y.ml"; "x.mli"; "child/x.ml" ] ~f:(fun name ->
    let file = Path.Build.relative dir name in
    printfn
      "%s: %b / %b"
      name
      (Target_mask.mem_file once file)
      (Target_mask.mem_file repeated file));
  printfn "directory: %b" (Target_mask.mem_directory repeated x);
  printfn
    "alias: %b"
    (Target_mask.mem_alias repeated (Alias.make (Alias.Name.of_string "x.ml") ~dir));
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow duplicated (fun () ->
           Rules.narrow duplicated (fun () ->
             let* () =
               Rules.narrow (Target_mask.files [ x ]) (fun () ->
                 printfn "force x";
                 Rules.Produce.rule (file_rule [ x ]))
             in
             Rules.narrow (Target_mask.files [ y ]) (fun () ->
               printfn "force y";
               Rules.Produce.rule (file_rule [ y ])))))
     in
     let tree = List.fold_left restrictions ~init:tree ~f:Rules.restrict in
     let* first = Rules.load tree (Target_mask.files [ x ]) in
     print_files "first" ~dir first;
     let* second = Rules.load tree (Target_mask.files [ x ]) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir second));
     let+ excluded = Rules.load tree (Target_mask.files [ y ]) in
     printfn "excluded rules: %d" (List.length (rules_in ~dir excluded)));
  [%expect
    {|
    x.ml: true / true
    y.ml: false / false
    x.mli: false / false
    child/x.ml: false / false
    directory: false
    alias: false
    force x
    first: x.ml
    same rules: true
    excluded rules: 0
    |}]
;;

let%expect_test "rules and aliases cannot escape a narrowed mask" =
  let dir = path "default/validated" in
  let inside = Path.Build.relative dir "x" in
  let outside = path "default/other/x" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  let reject label produce =
    expect_code_error label (fun () ->
      run
        (let open Memo.O in
         let* tree =
           Rules.collect_unit (fun () -> Rules.narrow Target_mask.all produce)
         in
         let tree = Rules.restrict tree (Target_mask.files [ inside ]) in
         let+ (_ : Rules.t) = Rules.load tree (Target_mask.files [ inside ]) in
         ()))
  in
  reject "file" (fun () -> Rules.Produce.rule (file_rule [ outside ]));
  reject "directory" (fun () ->
    let targets =
      Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton inside)
    in
    Rules.Produce.rule (rule targets));
  reject "alias" (fun () -> Rules.Produce.Alias.add_deps alias (Action_builder.return ()));
  [%expect
    {|
    file: code error
    directory: code error
    alias: code error
    |}]
;;

let%expect_test "multi-target closure pulls every overlapping producer" =
  let dir = path "default/multi" in
  let data = Path.Build.relative dir "data" in
  let shared = Path.Build.relative dir "shared" in
  let forced = ref [] in
  let produce label rule =
    forced := label :: !forced;
    Rules.Produce.rule rule
  in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let* () =
           Rules.narrow
             (Target_mask.files [ data; shared ])
             (fun () -> produce "data" (file_rule [ data; shared ]))
         in
         let* () =
           Rules.narrow (Target_mask.files [ shared ]) (fun () ->
             produce "file" (file_rule [ shared ]))
         in
         Rules.narrow (Target_mask.directories [ shared ]) (fun () ->
           let targets =
             Targets.create
               ~files:Path.Build.Set.empty
               ~dirs:(Path.Build.Set.singleton shared)
           in
           produce "directory" (rule targets)))
     in
     let+ loaded = Rules.load tree (Target_mask.files [ data ]) in
     printfn
       "forced: %s"
       (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
     printfn "rules: %d" (List.length (rules_in ~dir loaded));
     print_files "files" ~dir loaded;
     printfn
       "directory targets: %d"
       (Path.Build.Map.cardinal (Rules.directory_targets loaded)));
  [%expect
    {|
    forced: data, directory, file
    rules: 3
    files: data, shared, shared
    directory targets: 1
    |}]
;;

let%expect_test "directory materialization does not pull same-name file producers" =
  let dir = path "default/materialize" in
  let target = Path.Build.relative dir "generated" in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let* () =
           Rules.narrow (Target_mask.directories [ target ]) (fun () ->
             printfn "force directory";
             let targets =
               Targets.create
                 ~files:Path.Build.Set.empty
                 ~dirs:(Path.Build.Set.singleton target)
             in
             Rules.Produce.rule (rule targets))
         in
         Rules.narrow (Target_mask.files [ target ]) (fun () ->
           printfn "force file";
           Rules.Produce.rule (file_rule [ target ])))
     in
     printfn "materialize";
     let* loaded = Rules.load_directory_with_pending tree target in
     printfn "rules: %d" (List.length (rules_in ~dir loaded.selected));
     printfn "file pending: %b" (Rules.Pending.mem_file loaded.pending target);
     printfn "normal lookup";
     let+ loaded = Rules.load tree (Target_mask.path target) in
     printfn "rules: %d" (List.length (rules_in ~dir loaded)));
  [%expect
    {|
    materialize
    force directory
    rules: 1
    file pending: true
    normal lookup
    force file
    rules: 2
    |}]
;;

let%expect_test "directory materialization closes over mixed rule file outputs" =
  let dir = path "default/materialize-mixed" in
  let target = Path.Build.relative dir "generated" in
  let shared = Path.Build.relative dir "shared" in
  let forced = ref [] in
  let produce label rule =
    forced := label :: !forced;
    Rules.Produce.rule rule
  in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let* () =
           let mask =
             Target_mask.union
               (Target_mask.directories [ target ])
               (Target_mask.files [ shared ])
           in
           Rules.narrow mask (fun () ->
             let targets =
               Targets.create
                 ~files:(Path.Build.Set.singleton shared)
                 ~dirs:(Path.Build.Set.singleton target)
             in
             produce "mixed" (rule targets))
         in
         let* () =
           Rules.narrow (Target_mask.files [ target ]) (fun () ->
             produce "same-name file" (file_rule [ target ]))
         in
         let* () =
           Rules.narrow (Target_mask.files [ shared ]) (fun () ->
             produce "shared file" (file_rule [ shared ]))
         in
         Rules.narrow (Target_mask.directories [ shared ]) (fun () ->
           let targets =
             Targets.create
               ~files:Path.Build.Set.empty
               ~dirs:(Path.Build.Set.singleton shared)
           in
           produce "shared directory" (rule targets)))
     in
     let+ loaded = Rules.load_directory_with_pending tree target in
     printfn
       "forced: %s"
       (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
     printfn "rules: %d" (List.length (rules_in ~dir loaded.selected));
     printfn "same-name file pending: %b" (Rules.Pending.mem_file loaded.pending target));
  [%expect
    {|
    forced: mixed, shared directory, shared file
    rules: 3
    same-name file pending: true
    |}]
;;

let%expect_test "forcing a deferred result shares its rule production" =
  let dir = path "default/deferred" in
  let target = Path.Build.relative dir "result" in
  run
    (let open Memo.O in
     let* value, tree =
       Rules.collect (fun () ->
         Rules.defer (Target_mask.files [ target ]) (fun () ->
           printfn "force producer";
           let+ () = Rules.Produce.rule (file_rule [ target ]) in
           42))
     in
     let* value = Memo.Lazy.force value in
     printfn "value: %d" value;
     let* first = Rules.load tree (Target_mask.files [ target ]) in
     print_files "loaded" ~dir first;
     let+ repeated = Rules.load tree (Target_mask.files [ target ]) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir repeated)));
  [%expect
    {|
    force producer
    value: 42
    loaded: result
    same rules: true
    |}]
;;

let%expect_test "loading deferred rules shares the deferred result" =
  let dir = path "default/deferred-rules-first" in
  let target = Path.Build.relative dir "result" in
  run
    (let open Memo.O in
     let* value, tree =
       Rules.collect (fun () ->
         Rules.defer (Target_mask.files [ target ]) (fun () ->
           printfn "force producer";
           let+ () = Rules.Produce.rule (file_rule [ target ]) in
           42))
     in
     let* first = Rules.load tree (Target_mask.path target) in
     print_files "loaded" ~dir first;
     let* value = Memo.Lazy.force value in
     printfn "value: %d" value;
     let+ repeated = Rules.load tree (Target_mask.path target) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir repeated)));
  [%expect
    {|
    force producer
    loaded: result
    value: 42
    same rules: true
    |}]
;;

let%expect_test "direct target metadata follows unions, selections, and prefixes" =
  let dir = path "default/direct-metadata" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let child = Path.Build.relative dir "child" in
  let files = Rules.of_rules [ file_rule [ a; b ] ] in
  let directories =
    Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton child)
    |> rule
    |> List.singleton
    |> Rules.of_rules
  in
  let print label rules =
    let files, dirs = Rules.target_names rules ~dir in
    let names set =
      Filename.Set.to_list_map set ~f:Filename.to_string |> String.concat ~sep:", "
    in
    printfn
      "%s: files [%s]; directories [%s]; directory rules %d"
      label
      (names files)
      (names dirs)
      (Path.Build.Map.cardinal (Rules.directory_targets rules))
  in
  let tree = Rules.union files directories in
  print "union" tree;
  let tree = Rules.restrict tree (Target_mask.subtree dir) in
  print "restricted" tree;
  run
    (let open Memo.O in
     let+ selected = Rules.load tree (Target_mask.path a) in
     print "selected" selected);
  run
    (let open Memo.O in
     let* prefixed =
       Rules.collect_unit (fun () ->
         Rules.prefix_rules (Action_builder.return ()) ~f:(fun () -> Rules.produce tree))
     in
     print "prefixed" prefixed;
     let* selected = Rules.load prefixed (Target_mask.path a) in
     print "prefixed files" selected;
     let+ directory = Rules.load_directory_with_pending prefixed child in
     print "prefixed directory" directory.selected);
  [%expect
    {|
    union: files [a, b]; directories [child]; directory rules 1
    restricted: files [a, b]; directories [child]; directory rules 1
    selected: files [a, b]; directories []; directory rules 0
    prefixed: files [a, b]; directories [child]; directory rules 1
    prefixed files: files [a, b]; directories []; directory rules 0
    prefixed directory: files []; directories [child]; directory rules 1
    |}]
;;

let%expect_test "pending ownership stays local to each request" =
  let dir = path "default/pending-frontier" in
  let x = Path.Build.relative dir "x" in
  let y = Path.Build.relative dir "y" in
  let shared = Path.Build.relative dir "shared" in
  let x_dir = Path.Build.relative dir "x-dir" in
  let y_dir = Path.Build.relative dir "y-dir" in
  let alias =
    Alias.make (Alias.Name.of_string "check") ~dir:(Path.Build.relative dir "aliases")
  in
  let print label { Rules.pending; _ } =
    printfn
      "%s pending files: x %b; y %b; shared %b"
      label
      (Rules.Pending.mem_file pending x)
      (Rules.Pending.mem_file pending y)
      (Rules.Pending.mem_file pending shared);
    let alias_dirs =
      Rules.Pending.alias_directories pending ~dir
      |> Dir_set.toplevel_subdirs
      |> function
      | Infinite -> "all"
      | Finite names ->
        Filename.Set.to_list_map names ~f:Filename.to_string |> String.concat ~sep:", "
    in
    printfn
      "%s pending dirs: x %b; y %b; aliases [%s]"
      label
      (Rules.Pending.mem_directory pending x_dir)
      (Rules.Pending.intersects_directory pending y_dir)
      alias_dirs
  in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let producer name target directory =
             Rules.narrow
               (Target_mask.union
                  (Target_mask.files [ target; shared ])
                  (Target_mask.directories [ directory ]))
               (fun () ->
                  printfn "force %s" name;
                  Rules.Produce.rule (file_rule [ target ]))
           in
           let* () = producer "x" x x_dir in
           let* () = producer "y" y y_dir in
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             printfn "force alias";
             Rules.Produce.Alias.add_deps alias (Action_builder.return ()))))
     in
     let load label mask =
       let+ loaded = Rules.load_with_pending tree mask in
       print label loaded
     in
     let* () = load "x" (Target_mask.path x) in
     let* () = load "y" (Target_mask.path y) in
     let* () =
       load "both" (Target_mask.union (Target_mask.path x) (Target_mask.path y))
     in
     let* () = load "alias" (Target_mask.aliases [ alias ]) in
     load "x again" (Target_mask.path x));
  [%expect
    {|
    force x
    x pending files: x false; y true; shared true
    x pending dirs: x false; y true; aliases [aliases]
    force y
    y pending files: x true; y false; shared true
    y pending dirs: x true; y false; aliases [aliases]
    both pending files: x false; y false; shared false
    both pending dirs: x false; y false; aliases [aliases]
    force alias
    alias pending files: x true; y true; shared true
    alias pending dirs: x true; y true; aliases []
    x again pending files: x false; y true; shared true
    x again pending dirs: x false; y true; aliases [aliases]
    |}]
;;

let%expect_test "containing restrictions preserve producer identities" =
  let dir = path "default/redundant-restrictions" in
  let x = Path.Build.relative dir "x" in
  let y = Path.Build.relative dir "y" in
  let same_producers (a : Rules.loaded) (b : Rules.loaded) =
    let ids refinements = List.map refinements ~f:(fun { Rules.id; _ } -> id) in
    List.equal Rules.Producer_id.equal (ids a.refinements) (ids b.refinements)
  in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let producer name target =
           Rules.narrow (Target_mask.files [ target ]) (fun () ->
             printfn "force %s" name;
             Rules.Produce.rule (file_rule [ target ]))
         in
         let* () = producer "x" x in
         producer "y" y)
     in
     let declared = Rules.targets tree in
     let restricted =
       List.fold_left
         [ Target_mask.all
         ; Target_mask.subtree (Path.Build.parent_exn dir)
         ; Target_mask.subtree dir
         ]
         ~init:tree
         ~f:Rules.restrict
     in
     printfn "same declaration: %b" (Rules.targets restricted == declared);
     let* original_x = Rules.load_with_pending tree (Target_mask.path x) in
     let* restricted_x = Rules.load_with_pending restricted (Target_mask.path x) in
     printfn "x producer count: %d" (List.length original_x.refinements);
     printfn "same x producer: %b" (same_producers original_x restricted_x);
     let* original_y = Rules.load_with_pending tree (Target_mask.path y) in
     let+ restricted_y = Rules.load_with_pending restricted (Target_mask.path y) in
     printfn "y producer count: %d" (List.length original_y.refinements);
     printfn "same y producer: %b" (same_producers original_y restricted_y));
  [%expect
    {|
    same declaration: true
    force x
    x producer count: 1
    same x producer: true
    force y
    y producer count: 1
    same y producer: true
    |}]
;;

let%expect_test "output closure revisits revealed rules and follows new producers" =
  let dir = path "default/incremental-closure" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let d = Path.Build.relative dir "d" in
  let known = Path.Build.relative dir "known" in
  let missing = Path.Build.relative dir "missing" in
  let unrelated = Path.Build.relative dir "unrelated" in
  let forced = ref [] in
  let force name = forced := name :: !forced in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           force "outer";
           let* () = Rules.Produce.rule (file_rule [ a; b ]) in
           let* () = Rules.Produce.rule (file_rule [ b; c ]) in
           let* () = Rules.Produce.rule (file_rule [ known ]) in
           let* () =
             Rules.narrow
               (Target_mask.files [ c; d ])
               (fun () ->
                  force "child";
                  Rules.Produce.rule (file_rule [ c; d ]))
           in
           let* () =
             Rules.narrow
               (Target_mask.files [ d; missing ])
               (fun () ->
                  force "empty";
                  Memo.return ())
           in
           Rules.narrow (Target_mask.files [ unrelated ]) (fun () ->
             force "unrelated";
             Rules.Produce.rule (file_rule [ unrelated ]))))
     in
     let* first = Rules.load_with_pending tree (Target_mask.path a) in
     print_files "selected" ~dir first.selected;
     let { Rules.Dir_rules.rules; _ } =
       Rules.Revealed.find first.revealed ~dir |> Rules.Dir_rules.consume
     in
     print_rule_files "revealed" rules;
     printfn "followed producers: %d" (List.length first.refinements);
     printfn "empty claim pending: %b" (Rules.Pending.mem_file first.pending missing);
     printfn "unrelated pending: %b" (Rules.Pending.mem_file first.pending unrelated);
     let+ reverse = Rules.load_with_pending tree (Target_mask.path d) in
     printfn
       "reverse request selects the same rules: %b"
       (List.equal
          Rule.equal
          (rules_in ~dir first.selected)
          (rules_in ~dir reverse.selected));
     printfn
       "forced: %s"
       (List.sort !forced ~compare:String.compare |> String.concat ~sep:", "));
  [%expect
    {|
    selected: a, b, b, c, c, d
    revealed: a, b, b, c, c, d, known
    followed producers: 3
    empty claim pending: false
    unrelated pending: true
    reverse request selects the same rules: true
    forced: child, empty, outer
    |}]
;;

let%expect_test "exact mask bounds preserve range boundaries and holes" =
  let dir = path "default/mask-ranges" in
  let relative = Path.Build.relative dir in
  let mask names = Target_mask.files (List.map names ~f:relative) in
  let overlap label a b =
    let a = mask a in
    let b = mask b in
    printfn
      "%s: %b / %b / %b"
      label
      (Target_mask.intersects a b)
      (Target_mask.intersects b a)
      (not (Target_mask.is_empty (Target_mask.inter a b)))
  in
  overlap "disjoint ranges" [ "a"; "b" ] [ "c"; "d" ];
  overlap "touching ranges" [ "a"; "b" ] [ "b"; "c" ];
  overlap "interleaved disjoint names" [ "a"; "c" ] [ "b"; "d" ];
  overlap "interleaved shared name" [ "a"; "c"; "e" ] [ "b"; "c"; "d" ];
  let grouped = Target_mask.union (mask [ "m"; "n" ]) (mask [ "a" ]) in
  let grouped = Target_mask.union (mask [ "z" ]) grouped in
  let grouped = Target_mask.union grouped (mask [ "h"; "y" ]) in
  let grouped = Target_mask.union grouped (mask [ "h"; "y" ]) in
  let narrowed = Target_mask.inter grouped (mask [ "h"; "n"; "q"; "y" ]) in
  List.iter [ "0"; "a"; "h"; "m"; "n"; "q"; "y"; "z"; "zz" ] ~f:(fun name ->
    let file = relative name in
    printfn
      "%s: %b / %b"
      name
      (Target_mask.mem_file grouped file)
      (Target_mask.mem_file narrowed file));
  [%expect
    {|
    disjoint ranges: false / false / false
    touching ranges: true / true / true
    interleaved disjoint names: false / false / false
    interleaved shared name: true / true / true
    0: false / false
    a: true / false
    h: true / true
    m: true / false
    n: true / true
    q: false / false
    y: true / true
    z: true / false
    zz: false / false
    |}]
;;

let%expect_test "subtree unions discard only contained regions" =
  let dir = path "default/mask-union" in
  let child = Path.Build.relative dir "child" in
  let parent = Target_mask.subtree dir in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:child in
  let covered =
    Target_mask.union
      (Target_mask.files [ dir; Path.Build.relative child "out" ])
      (Target_mask.union
         (Target_mask.directories [ dir; child ])
         (Target_mask.aliases [ alias ]))
  in
  printfn
    "contained regions reuse subtree: %b / %b"
    (Target_mask.union parent covered == parent)
    (Target_mask.union covered parent == parent);
  let parent_alias =
    Alias.make (Alias.Name.of_string "mask-union") ~dir:(Path.Build.parent_exn dir)
  in
  let with_parent_alias =
    Target_mask.union parent (Target_mask.aliases [ parent_alias ])
  in
  printfn
    "parent alias retained: %b"
    (Target_mask.mem_alias with_parent_alias parent_alias);
  let outside = path "default/mask-union-other/out" in
  let with_outside = Target_mask.union parent (Target_mask.files [ outside ]) in
  printfn "outside file retained: %b" (Target_mask.mem_file with_outside outside);
  printfn
    "inside alias retained: %b"
    (Target_mask.mem_alias with_parent_alias alias
     && Target_mask.mem_alias with_outside alias);
  [%expect
    {|
    contained regions reuse subtree: true / true
    parent alias retained: true
    outside file retained: true
    inside alias retained: true
    |}]
;;

let%expect_test "validated target masks match path-based construction" =
  let dir = path "default/validated-mask" in
  let relative = Path.Build.relative dir in
  let samples =
    [ dir
    ; relative "x"
    ; relative "y"
    ; relative "one"
    ; relative "two"
    ; relative "x/nested"
    ; relative "one/nested"
    ; path "default/validated-mask-other/x"
    ]
  in
  List.iter
    [ "single file", [ "x" ], []
    ; "multiple files", [ "x"; "y" ], []
    ; "single directory", [], [ "one" ]
    ; "multiple directories", [], [ "one"; "two" ]
    ; "mixed singleton kinds", [ "x" ], [ "one" ]
    ; "mixed multiple kinds", [ "x"; "y" ], [ "one"; "two" ]
    ]
    ~f:(fun (label, file_names, directory_names) ->
      let files = List.map file_names ~f:relative in
      let directories = List.map directory_names ~f:relative in
      let { Rule.targets; _ } =
        rule
          (Targets.create
             ~files:(Path.Build.Set.of_list files)
             ~dirs:(Path.Build.Set.of_list directories))
      in
      let actual = Target_mask.of_targets targets in
      let expected =
        Target_mask.union (Target_mask.files files) (Target_mask.directories directories)
      in
      let same_membership =
        List.for_all samples ~f:(fun path ->
          Bool.equal
            (Target_mask.mem_file actual path)
            (Target_mask.mem_file expected path)
          && Bool.equal
               (Target_mask.mem_directory actual path)
               (Target_mask.mem_directory expected path)
          && Bool.equal
               (Target_mask.intersects_directory actual path)
               (Target_mask.intersects_directory expected path))
      in
      let alias = Alias.make (Alias.Name.of_string "x") ~dir in
      printfn "%s: %b" label (same_membership && not (Target_mask.mem_alias actual alias)));
  [%expect
    {|
    single file: true
    multiple files: true
    single directory: true
    multiple directories: true
    mixed singleton kinds: true
    mixed multiple kinds: true
    |}]
;;

let%expect_test "completed file views remain local to each Memo run" =
  let context =
    Build_context.create
      ~name:(Context_name.of_string "test-rule-loading-completed-file-views")
  in
  let selection_context =
    Build_context.create
      ~name:(Context_name.of_string "test-rule-loading-revealed-siblings")
  in
  let cleanup_context =
    Build_context.create
      ~name:(Context_name.of_string "test-rule-loading-restored-cleanup")
  in
  let inherited_context =
    Build_context.create
      ~name:(Context_name.of_string "test-rule-loading-inherited-declarations")
  in
  let coverage_context =
    Build_context.create
      ~name:(Context_name.of_string "test-rule-loading-initial-coverage")
  in
  let selector_context =
    Build_context.create
      ~name:(Context_name.of_string "test-rule-loading-selector-cleanup")
  in
  let selector_path name = Path.Build.relative selector_context.build_dir name in
  let selector_live = selector_path "live" in
  let selector_stale = selector_path "stale" in
  let selector_link = selector_path "old-link" in
  let selector_pending = selector_path "pending" in
  let selector_rule = file_rule [ selector_live ] in
  let selector_rule_input = Memo.Var.create selector_rule ~name:"selector-rule" in
  let selector_refresh = selector_path "refresh" in
  let selector_refresh_rule = file_rule [ selector_refresh ] in
  let selector_refresh_input = Memo.Var.create 0 ~name:"selector-refresh" in
  let selector_refresh_runs = ref 0 in
  let selector_runs = ref 0 in
  let selector_pending_runs = ref 0 in
  let file_selector ~dir pattern =
    File_selector.of_glob
      ~dir:(Path.build dir)
      (Dune_lang.Glob.of_string_exn Loc.none pattern)
  in
  let coverage_path name = Path.Build.relative coverage_context.build_dir name in
  let coverage_live = coverage_path "live" in
  let coverage_stale = coverage_path "stale" in
  let coverage_link = coverage_path "old-link" in
  let coverage_directory = coverage_path "old-directory" in
  let coverage_actions =
    Path.Build.append_local
      Dpath.Build.anonymous_actions_dir
      (Path.Build.local coverage_context.build_dir)
  in
  let coverage_action_file = Path.Build.relative coverage_actions "action-file" in
  let coverage_action_dir = Path.Build.relative coverage_actions "old-directory" in
  let coverage_initial = ref false in
  let coverage_rule = file_rule [ coverage_live ] in
  let inherited_dir name = Path.Build.relative inherited_context.build_dir name in
  let inherited_left = inherited_dir "left" in
  let inherited_leaf = inherited_dir "left/leaf" in
  let inherited_orphan = inherited_dir "orphan" in
  let inherited_phase = Memo.Var.create 0 ~name:"inherited-declarations-phase" in
  let cleanup_live = Path.Build.relative cleanup_context.build_dir "live" in
  let cleanup_stale = Path.Build.relative cleanup_context.build_dir "stale" in
  let cleanup_pending = Path.Build.relative cleanup_context.build_dir "pending" in
  let cleanup_kind = Path.Build.relative cleanup_context.build_dir "changed-kind" in
  let cleanup_kind_alias = Alias.make (Alias.Name.of_string "check") ~dir:cleanup_kind in
  let cleanup_kind_trigger =
    Path.Build.relative cleanup_context.build_dir "before-trigger"
  in
  let cleanup_kind_ghost = Path.Build.relative cleanup_kind "ghost" in
  let cleanup_kind_file_trigger =
    Path.Build.relative cleanup_context.build_dir "z-trigger"
  in
  let cleanup_kind_runs = ref 0 in
  let cleanup_kind_alias_runs = ref 0 in
  let cleanup_kind_file_runs = ref 0 in
  let cleanup_padding =
    List.init 16 ~f:(fun i ->
      Path.Build.relative cleanup_context.build_dir ("padding-" ^ Int.to_string i))
  in
  let cleanup_padding_runs = ref 0 in
  let cleanup_fallback = Path.Build.relative cleanup_context.build_dir "fallback" in
  let cleanup_promoted = Path.Build.relative cleanup_context.build_dir "promoted" in
  let cleanup_nested_a = Path.Build.relative cleanup_context.build_dir "nested-a" in
  let cleanup_nested_b = Path.Build.relative cleanup_context.build_dir "nested-b" in
  let cleanup_failure_target name =
    Path.Build.relative cleanup_context.build_dir ("cleanup-failure-" ^ name)
  in
  let cleanup_failure_ghost = cleanup_failure_target "ghost" in
  let cleanup_failure_first = cleanup_failure_target "first" in
  let cleanup_failure_retry = cleanup_failure_target "retry" in
  let cleanup_failure_enabled = Memo.Var.create false ~name:"cleanup-failure-enabled" in
  let cleanup_failure_actions =
    Path.Build.append_local
      Dpath.Build.anonymous_actions_dir
      (Path.Build.local cleanup_context.build_dir)
  in
  let cleanup_failure_saved =
    Path.Build.relative
      (Path.Build.parent_exn cleanup_failure_actions)
      (Filename.to_string (Path.Build.basename cleanup_failure_actions) ^ "-saved")
  in
  let cleanup_failure_moved = ref false in
  let cleanup_failure_runs = ref 0 in
  let cleanup_failure_aliases =
    List.map [ "b"; "c" ] ~f:(fun name ->
      Alias.make
        (Alias.Name.of_string "check")
        ~dir:(Path.Build.relative cleanup_context.build_dir name))
  in
  let metadata_live = Path.Build.relative cleanup_context.build_dir "metadata-live" in
  let metadata_stale = Path.Build.relative cleanup_context.build_dir "metadata-stale" in
  let metadata_rule = file_rule [ metadata_live ] in
  let metadata_replaced = Memo.Var.create false ~name:"cleanup-metadata-replaced" in
  let obsolete_metadata = Memo.Var.create false ~name:"cleanup-metadata-obsolete" in
  let metadata_old_runs = ref 0 in
  let cleanup_rule = file_rule [ cleanup_live ] in
  let cleanup_fallback_rule =
    Rule.make
      ~mode:Fallback
      ~targets:(Targets.File.create cleanup_fallback)
      (Action_builder.return (Action.Full.make Action.empty))
  in
  let cleanup_promoted_rule =
    Rule.make
      ~mode:(Promote { lifetime = Unlimited; into = None; only = None })
      ~targets:(Targets.File.create cleanup_promoted)
      (Action_builder.return (Action.Full.make Action.empty))
  in
  let cleanup_rule_input = Memo.Var.create cleanup_rule ~name:"restored-cleanup-rule" in
  let cleanup_sources =
    Memo.Var.create Filename.Array.Set.empty ~name:"restored-cleanup-sources"
  in
  let cleanup_enabled = Memo.Var.create true ~name:"restored-cleanup-enabled" in
  let cleanup_context_enabled =
    Memo.Var.create true ~name:"restored-cleanup-context-enabled"
  in
  let cleanup_unrelated = Memo.Var.create 0 ~name:"restored-cleanup-unrelated" in
  let cleanup_nested_split =
    Memo.Var.create false ~name:"restored-cleanup-nested-split"
  in
  let cleanup_generator_runs = ref 0 in
  let cleanup_producer_runs = ref 0 in
  let cleanup_consumer_runs = ref 0 in
  let cleanup_pending_runs = ref 0 in
  let cleanup_nested_runs = ref 0 in
  let cleanup_nested_consumer_runs = ref 0 in
  let cleanup_nested_forced = ref [] in
  let selection_dir name = Path.Build.relative selection_context.build_dir name in
  let selection_dirs =
    [ "cache-alternate"
    ; "cache-declarations-left"
    ; "cache-declarations-right"
    ; "cache-epoch-left"
    ; "cache-epoch-right"
    ; "cache-left"
    ; "cache-right"
    ]
  in
  let selection_left = selection_dir "cache-left" in
  let selection_right = selection_dir "cache-right" in
  let selection_alternate = selection_dir "cache-alternate" in
  let selection_seed = Path.Build.relative selection_left "seed" in
  let source_conflict = Path.Build.relative selection_right "conflict" in
  let fallback = Path.Build.relative selection_right "fallback" in
  let promoted = Path.Build.relative selection_right "promoted" in
  let alternate = Path.Build.relative selection_alternate "value" in
  let fallback_rule =
    Rule.make
      ~mode:Fallback
      ~targets:(Targets.File.create fallback)
      (Action_builder.return (Action.Full.make Action.empty))
  in
  let promoted_rule =
    Rule.make
      ~mode:(Promote { lifetime = Unlimited; into = None; only = None })
      ~targets:(Targets.File.create promoted)
      (Action_builder.return (Action.Full.make Action.empty))
  in
  let selection_runs = ref 0 in
  let alternate_runs = ref 0 in
  let epoch_left = Path.Build.relative (selection_dir "cache-epoch-left") "a" in
  let epoch_right = Path.Build.relative (selection_dir "cache-epoch-right") "b" in
  let epoch_split = ref false in
  let epoch_forced = ref [] in
  let epoch_consumer_runs = ref 0 in
  let declarations_left = selection_dir "cache-declarations-left" in
  let declarations_right = selection_dir "cache-declarations-right" in
  let publisher = Path.Build.relative declarations_left "publisher" in
  let candidate = Path.Build.relative declarations_right "candidate" in
  let generated = Path.Build.relative declarations_left "generated" in
  let generated_leaf = Path.Build.relative generated "leaf" in
  let generated_directory_enabled =
    Memo.Var.create true ~name:"revealed-sibling-directory-target"
  in
  let generated_directory_declared =
    Memo.Var.create true ~name:"revealed-sibling-directory-declared"
  in
  let generated_directory_rule =
    rule
      (Targets.create
         ~files:Path.Build.Set.empty
         ~dirs:(Path.Build.Set.singleton generated))
  in
  let generated_leaf_rule = file_rule [ generated_leaf ] in
  let directory_producer_runs = ref 0 in
  let conflicting_directory =
    Path.Build.relative declarations_left "conflicting-directory"
  in
  let conflicting_directory_rule =
    rule
      (Targets.create
         ~files:Path.Build.Set.empty
         ~dirs:(Path.Build.Set.singleton conflicting_directory))
  in
  let mixed_directory = Path.Build.relative declarations_left "mixed-directory" in
  let mixed_file = Path.Build.relative declarations_left "mixed-file" in
  let mixed_directory_rule =
    rule
      (Targets.create
         ~files:(Path.Build.Set.singleton mixed_file)
         ~dirs:(Path.Build.Set.singleton mixed_directory))
  in
  let directory_conflict_runs = ref 0 in
  let target name = Path.Build.relative context.build_dir name in
  let a = target "a" in
  let b = target "b" in
  let reverse_a = target "reverse-a" in
  let reverse_b = target "reverse-b" in
  let parallel_a = target "parallel-a" in
  let parallel_b = target "parallel-b" in
  let independent = target "independent" in
  let late = target "late" in
  let invalid = target "invalid" in
  let left_dir = target "left" in
  let right_dir = target "right" in
  let left = Path.Build.relative left_dir "same" in
  let right = Path.Build.relative right_dir "same" in
  let visibility_dir = target "visibility" in
  let visibility_first = Path.Build.relative visibility_dir "first" in
  let visibility_second = Path.Build.relative visibility_dir "second" in
  let visibility_missing = Path.Build.relative visibility_dir "missing" in
  let visibility_first_rule = file_rule [ visibility_first ] in
  let visibility_second_rule = file_rule [ visibility_second ] in
  let visibility_allowed = Memo.Var.create true ~name:"partial-visibility-allowed" in
  let visibility_parent_runs = ref 0 in
  let visibility_child_runs = ref 0 in
  let visibility_rules =
    Memo.lazy_ ~name:"partial-visibility-child-rules" (fun () ->
      Rules.collect_unit (fun () ->
        Memo.parallel_iter
          [ visibility_first_rule; visibility_second_rule ]
          ~f:(fun rule ->
            Rules.narrow (Target_mask.of_targets rule.Rule.targets) (fun () ->
              incr visibility_child_runs;
              Rules.Produce.rule rule))))
  in
  let release_shared = Fiber.Ivar.create () in
  let split = ref false in
  let late_enabled = ref false in
  let invalid_enabled = ref false in
  let forced = ref [] in
  let consumer_runs = ref 0 in
  let lookup target =
    let open Memo.O in
    let+ rule = Load_rules.get_rule (Path.build target) in
    Option.value_exn rule
  in
  let lookup_for_build =
    Memo.exec
      (Memo.create
         "completed-file-view-build-lookup"
         ~input:(module Path.Build)
         (fun target ->
            let open Memo.O in
            let+ rule = Load_rules.get_rule_or_source (Path.build target) in
            match rule with
            | Rule (_, rule) -> rule
            | Source _ -> Code_error.raise "Expected a build target" []))
  in
  let cleanup_consumer =
    Memo.create
      "restored-cleanup-consumer"
      ~input:(module Unit)
      (fun () ->
         incr cleanup_consumer_runs;
         lookup_for_build cleanup_live)
  in
  let cleanup_nested_consumer =
    Memo.create
      "restored-cleanup-nested-consumer"
      ~input:(module Unit)
      (fun () ->
         incr cleanup_nested_consumer_runs;
         lookup_for_build cleanup_nested_b)
  in
  let consumer =
    Memo.create
      "completed-file-view-consumer"
      ~input:(module Unit)
      (fun () ->
         incr consumer_runs;
         let open Memo.O in
         let+ rule, _ =
           Memo.fork_and_join
             (fun () -> lookup_for_build b)
             (fun () -> lookup_for_build independent)
         in
         rule)
  in
  let epoch_consumer =
    Memo.create
      "revealed-sibling-epoch-consumer"
      ~input:(module Unit)
      (fun () ->
         incr epoch_consumer_runs;
         lookup_for_build epoch_right)
  in
  let module Rule_generator = struct
    let gen_rules _ ~dir _ =
      let open Memo.O in
      let* () =
        if
          Path.Build.equal dir cleanup_context.build_dir
          || Path.Build.equal dir coverage_context.build_dir
          || Path.Build.equal dir selector_context.build_dir
          || Path.Build.is_descendant dir ~of_:inherited_context.build_dir
        then Memo.return ()
        else Memo.current_run () >>| ignore
      in
      if Path.Build.is_descendant dir ~of_:inherited_context.build_dir
      then
        let module Subdirs = Build_config.Gen_rules.Build_only_sub_dirs in
        let finite dir names =
          Subdirs.singleton
            ~dir
            (List.map names ~f:Filename.of_string_exn |> Subdir_set.of_list)
        in
        let+ build_dir_only_sub_dirs =
          if Path.Build.equal dir inherited_context.build_dir
          then
            let+ phase = Memo.Var.read inherited_phase in
            match phase with
            | 0 ->
              Subdirs.union
                (finite dir [ "left"; "right" ])
                (Subdirs.union
                   (finite inherited_left [ "leaf" ])
                   (finite inherited_leaf [ "tip" ]))
            | 1 -> Subdirs.singleton ~dir Subdir_set.all
            | _ -> Subdirs.empty
          else if Path.Build.equal dir inherited_left
          then Memo.return (finite dir [ "local" ])
          else if Path.Build.equal dir inherited_orphan
          then Memo.return (Subdirs.singleton ~dir Subdir_set.all)
          else Memo.return Subdirs.empty
        in
        let rules =
          if Path.Build.equal dir inherited_orphan
          then Rules.of_rules [ file_rule [ Path.Build.relative dir "probe" ] ]
          else Rules.empty
        in
        Build_config.Gen_rules.Gen_rules_result.rules_here
          (Build_config.Gen_rules.Rules.create
             ~build_dir_only_sub_dirs
             (Memo.return rules))
      else if Path.Build.equal dir selector_context.build_dir
      then (
        let rules =
          Rules.collect_unit (fun () ->
            let* () =
              Rules.narrow
                (Target_mask.files [ selector_live; selector_stale; selector_link ])
                (fun () ->
                   let* rule = Memo.Var.read selector_rule_input in
                   incr selector_runs;
                   Rules.Produce.rule rule)
            in
            let* () =
              Rules.narrow (Target_mask.files [ selector_pending ]) (fun () ->
                incr selector_pending_runs;
                Memo.return ())
            in
            Rules.narrow (Target_mask.files [ selector_refresh ]) (fun () ->
              let* (_ : int) = Memo.Var.read selector_refresh_input in
              incr selector_refresh_runs;
              Rules.Produce.rule selector_refresh_rule))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create rules)))
      else if Path.Build.equal dir coverage_context.build_dir
      then (
        let rules =
          Rules.collect_unit (fun () ->
            Rules.narrow (Target_mask.subtree dir) (fun () ->
              coverage_initial
              := List.for_all
                   [ coverage_live
                   ; coverage_stale
                   ; coverage_link
                   ; coverage_directory
                   ; coverage_action_file
                   ; coverage_action_dir
                   ]
                   ~f:(fun path -> Fpath.exists (Path.Build.to_string path));
              Rules.Produce.rule coverage_rule))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create rules)))
      else if Path.Build.equal dir cleanup_context.build_dir
      then (
        incr cleanup_generator_runs;
        let rules =
          Rules.collect_unit (fun () ->
            let* () =
              Rules.narrow (Target_mask.files_in_directory dir) (fun () ->
                let* enabled = Memo.Var.read cleanup_enabled in
                incr cleanup_producer_runs;
                if enabled
                then
                  let* rule = Memo.Var.read cleanup_rule_input in
                  Rules.produce
                    (Rules.of_rules
                       [ rule; cleanup_fallback_rule; cleanup_promoted_rule ])
                else Memo.return ())
            in
            let* () =
              Rules.narrow
                (Target_mask.union
                   (Target_mask.files
                      [ cleanup_failure_first
                      ; cleanup_failure_retry
                      ; cleanup_failure_ghost
                      ])
                   (Target_mask.aliases cleanup_failure_aliases))
                (fun () ->
                   let* enabled = Memo.Var.read cleanup_failure_enabled in
                   if not enabled
                   then Memo.return ()
                   else (
                     incr cleanup_failure_runs;
                     Unix.rename
                       (Path.Build.to_string cleanup_failure_actions)
                       (Path.Build.to_string cleanup_failure_saved);
                     cleanup_failure_moved := true;
                     Unix.symlink
                       (Filename.to_string (Path.Build.basename cleanup_failure_actions))
                       (Path.Build.to_string cleanup_failure_actions);
                     Rules.Produce.rule
                       (file_rule [ cleanup_failure_first; cleanup_failure_retry ])))
            in
            let* () =
              Rules.narrow
                (Target_mask.files [ metadata_live; metadata_stale ])
                (fun () ->
                   let* replaced = Memo.Var.read metadata_replaced in
                   if replaced
                   then
                     Rules.narrow (Target_mask.files [ metadata_live ]) (fun () ->
                       Rules.Produce.rule metadata_rule)
                   else
                     Rules.narrow
                       (Target_mask.files [ metadata_live; metadata_stale ])
                       (fun () ->
                          let* obsolete = Memo.Var.read obsolete_metadata in
                          incr metadata_old_runs;
                          if obsolete
                          then Code_error.raise "Restored obsolete cleanup producer" [];
                          Rules.produce
                            (Rules.of_rules
                               [ metadata_rule; file_rule [ metadata_stale ] ])))
            in
            let* () =
              Rules.narrow (Target_mask.files [ cleanup_pending ]) (fun () ->
                incr cleanup_pending_runs;
                let+ (_ : Rule.t) = Memo.exec cleanup_consumer () in
                ())
            in
            let* () =
              Rules.narrow (Target_mask.files [ cleanup_kind ]) (fun () ->
                incr cleanup_kind_runs;
                Memo.return ())
            in
            let* () =
              Rules.narrow
                (Target_mask.union
                   (Target_mask.aliases [ cleanup_kind_alias ])
                   (Target_mask.files [ cleanup_kind_trigger; cleanup_kind_file_trigger ]))
                (fun () ->
                   incr cleanup_kind_alias_runs;
                   Memo.return ())
            in
            let* () =
              Rules.narrow
                (Target_mask.files [ cleanup_kind_ghost; cleanup_kind_file_trigger ])
                (fun () ->
                   incr cleanup_kind_file_runs;
                   Memo.return ())
            in
            let* () =
              Rules.narrow (Target_mask.files cleanup_padding) (fun () ->
                incr cleanup_padding_runs;
                Memo.return ())
            in
            Rules.narrow
              (Target_mask.files [ cleanup_nested_a; cleanup_nested_b ])
              (fun () ->
                 let* split = Memo.Var.read cleanup_nested_split in
                 incr cleanup_nested_runs;
                 if split
                 then
                   let* () =
                     Rules.narrow (Target_mask.files [ cleanup_nested_a ]) (fun () ->
                       cleanup_nested_forced := "a" :: !cleanup_nested_forced;
                       let* (_ : Rule.t) = Memo.exec cleanup_nested_consumer () in
                       Rules.Produce.rule (file_rule [ cleanup_nested_a ]))
                   in
                   Rules.narrow (Target_mask.files [ cleanup_nested_b ]) (fun () ->
                     cleanup_nested_forced := "b" :: !cleanup_nested_forced;
                     Rules.Produce.rule (file_rule [ cleanup_nested_b ]))
                 else
                   Rules.narrow
                     (Target_mask.files [ cleanup_nested_a; cleanup_nested_b ])
                     (fun () ->
                        cleanup_nested_forced := "joined" :: !cleanup_nested_forced;
                        Rules.Produce.rule
                          (file_rule [ cleanup_nested_a; cleanup_nested_b ]))))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create rules)))
      else if Path.Build.equal dir selection_context.build_dir
      then (
        let split = !epoch_split in
        let rules =
          Rules.collect_unit (fun () ->
            let* () =
              Rules.narrow
                (Target_mask.files
                   [ selection_seed; source_conflict; fallback; promoted; alternate ])
                (fun () ->
                   incr selection_runs;
                   Rules.produce
                     (Rules.of_rules
                        [ file_rule [ selection_seed ]
                        ; file_rule [ source_conflict ]
                        ; fallback_rule
                        ; promoted_rule
                        ; file_rule [ alternate ]
                        ]))
            in
            let* () =
              if split
              then
                let* () =
                  Rules.narrow (Target_mask.files [ epoch_left ]) (fun () ->
                    epoch_forced := "left" :: !epoch_forced;
                    let* (_ : Rule.t) = Memo.exec epoch_consumer () in
                    Rules.Produce.rule (file_rule [ epoch_left ]))
                in
                Rules.narrow (Target_mask.files [ epoch_right ]) (fun () ->
                  epoch_forced := "right" :: !epoch_forced;
                  Rules.Produce.rule (file_rule [ epoch_right ]))
              else
                Rules.narrow
                  (Target_mask.files [ epoch_left; epoch_right ])
                  (fun () ->
                     epoch_forced := "shared" :: !epoch_forced;
                     Rules.produce
                       (Rules.of_rules
                          [ file_rule [ epoch_left ]; file_rule [ epoch_right ] ]))
            in
            let* () =
              Rules.narrow
                (Target_mask.files [ publisher; candidate ])
                (fun () ->
                   Rules.produce
                     (Rules.of_rules [ file_rule [ publisher ]; file_rule [ candidate ] ]))
            in
            Rules.narrow (Target_mask.subtree declarations_left) (fun () ->
              let* directory_target = Memo.Var.read generated_directory_enabled in
              incr directory_producer_runs;
              Rules.Produce.rule
                (if directory_target
                 then generated_directory_rule
                 else generated_leaf_rule)))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create
                ~build_dir_only_sub_dirs:
                  (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                     ~dir
                     (List.map selection_dirs ~f:Filename.of_string_exn
                      |> Subdir_set.of_list))
                rules)))
      else if Path.Build.equal dir selection_alternate
      then (
        let rules =
          Rules.collect_unit (fun () ->
            Rules.narrow (Target_mask.files [ alternate ]) (fun () ->
              incr alternate_runs;
              Rules.Produce.rule (file_rule [ alternate ])))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create rules)))
      else if Path.Build.equal dir declarations_left
      then
        let+ directory_target = Memo.Var.read generated_directory_enabled
        and+ declared = Memo.Var.read generated_directory_declared in
        let directory_targets =
          Path.Build.Map.of_list_exn
            [ conflicting_directory, Loc.none; mixed_directory, Loc.none ]
        in
        Build_config.Gen_rules.Gen_rules_result.rules_here
          (Build_config.Gen_rules.Rules.create
             ~directory_targets:
               (if directory_target && declared
                then Path.Build.Map.set directory_targets generated Loc.none
                else directory_targets)
             (Rules.collect_unit (fun () ->
                let* () =
                  Memo.parallel_iter
                    [ conflicting_directory_rule; mixed_directory_rule ]
                    ~f:(fun rule ->
                      Rules.narrow (Target_mask.of_targets rule.Rule.targets) (fun () ->
                        Rules.Produce.rule rule))
                in
                Memo.parallel_iter
                  [ conflicting_directory; mixed_directory ]
                  ~f:(fun target ->
                    Rules.narrow (Target_mask.files [ target ]) (fun () ->
                      incr directory_conflict_runs;
                      Rules.Produce.rule (file_rule [ target ]))))))
      else if Path.Build.equal dir visibility_dir
      then
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create (Memo.Lazy.force visibility_rules)))
      else if Path.Build.equal dir left_dir || Path.Build.equal dir right_dir
      then (
        let label, target =
          if Path.Build.equal dir left_dir then "left", left else "right", right
        in
        let rules =
          Rules.collect_unit (fun () ->
            Rules.narrow (Target_mask.files [ target ]) (fun () ->
              forced := label :: !forced;
              Rules.Produce.rule (file_rule [ target ])))
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create rules)))
      else if not (Path.Build.equal dir context.build_dir)
      then Memo.return Build_config.Gen_rules.Gen_rules_result.no_rules
      else (
        let split = !split in
        let late_enabled = !late_enabled in
        let invalid_enabled = !invalid_enabled in
        let rules =
          Rules.collect_unit (fun () ->
            let produce label targets =
              forced := label :: !forced;
              Rules.Produce.rule (file_rule targets)
            in
            let* visible = Memo.Var.read visibility_allowed in
            let* () =
              Rules.narrow
                (if visible
                 then
                   Target_mask.aliases
                     [ Alias.make (Alias.Name.of_string "visible") ~dir:visibility_dir ]
                 else Target_mask.empty)
                (fun () ->
                   incr visibility_parent_runs;
                   Memo.return ())
            in
            let* () =
              if not split
              then
                Rules.narrow
                  (Target_mask.files [ a; b ])
                  (fun () ->
                     let* () =
                       Memo.of_reproducible_fiber (Fiber.Ivar.read release_shared)
                     in
                     produce "shared" [ a; b ])
              else
                let* () =
                  Rules.narrow (Target_mask.files [ a ]) (fun () ->
                    forced := "a" :: !forced;
                    let* (_ : Rule.t) = Memo.exec consumer () in
                    Rules.Produce.rule (file_rule [ a ]))
                in
                Rules.narrow (Target_mask.files [ b ]) (fun () -> produce "b" [ b ])
            in
            let* () =
              Rules.narrow (Target_mask.files [ independent ]) (fun () ->
                produce "independent" [ independent ])
            in
            let* () =
              Rules.narrow
                (Target_mask.files [ reverse_a; reverse_b ])
                (fun () -> produce "reverse" [ reverse_a; reverse_b ])
            in
            let* () =
              Rules.narrow
                (Target_mask.files [ parallel_a; parallel_b ])
                (fun () -> produce "parallel" [ parallel_a; parallel_b ])
            in
            let* () =
              if late_enabled
              then
                Rules.narrow (Target_mask.files [ late ]) (fun () ->
                  produce "late" [ late ])
              else Memo.return ()
            in
            if invalid_enabled
            then
              Rules.narrow (Target_mask.files [ invalid ]) (fun () ->
                let* () = Rules.Produce.rule (file_rule [ invalid ]) in
                Rules.Produce.rule (file_rule [ invalid ]))
            else Memo.return ())
        in
        Memo.return
          (Build_config.Gen_rules.Gen_rules_result.rules_here
             (Build_config.Gen_rules.Rules.create
                ~build_dir_only_sub_dirs:
                  (Build_config.Gen_rules.Build_only_sub_dirs.singleton
                     ~dir
                     (Subdir_set.of_list
                        [ Path.Build.basename left_dir; Path.Build.basename right_dir ]))
                rules)))
    ;;
  end
  in
  let module Source_tree = struct
    module Dir = struct
      type t = Path.Source.t * Filename.Array.Set.t

      let sub_dir_names (dir, _) =
        if Path.Source.equal dir Path.Source.root
        then
          List.map selection_dirs ~f:Filename.of_string_exn
          |> Filename.Array.Set.of_sorted_list
        else Filename.Array.Set.empty
      ;;

      let filenames (_, filenames) = filenames
    end

    let find_dir dir =
      let open Memo.O in
      let+ filenames =
        if Path.Source.equal dir Path.Source.root
        then Memo.Var.read cleanup_sources
        else
          Memo.return
            (if String.equal (Path.Source.to_string dir) "cache-right"
             then
               List.map [ "conflict"; "fallback"; "promoted" ] ~f:Filename.of_string_exn
               |> Filename.Array.Set.of_sorted_list
             else Filename.Array.Set.empty)
      in
      Option.some_if
        (Path.Source.equal dir Path.Source.root
         || List.mem selection_dirs (Path.Source.to_string dir) ~equal:String.equal)
        (dir, filenames)
    ;;
  end
  in
  Build_config.set
    ~contexts:
      (Memo.lazy_ ~name:"completed-file-view-context" (fun () ->
         let open Memo.O in
         let+ enabled = Memo.Var.read cleanup_context_enabled in
         let contexts =
           [ context, Build_config.Context_type.Empty
           ; selection_context, Build_config.Context_type.With_sources
           ; inherited_context, Build_config.Context_type.Empty
           ; coverage_context, Build_config.Context_type.Empty
           ; selector_context, Build_config.Context_type.Empty
           ]
         in
         if enabled
         then contexts @ [ cleanup_context, Build_config.Context_type.With_sources ]
         else contexts))
    ~promote_source:(fun ~chmod:_ ~delete_dst_if_it_is_a_directory:_ ~src:_ ~dst:_ ->
      Fiber.return ())
    ~sandboxing_preference:[]
    ~rule_generator:(module Rule_generator)
    ~implicit_default_alias:(fun _ -> Memo.return None)
    ~execution_parameters:(fun _ ~dir:_ ->
      Memo.return Execution_parameters.builtin_default)
    ~source_tree:(module Source_tree);
  (* Release the producer only once both lookups have exhausted their runnable
     work. Both miss the entry cache and wait for the same atomic rule. *)
  let released = ref false in
  let first_a, first_b =
    Fiber.run
      (Memo.run
         (Memo.fork_and_join (fun () -> lookup a) (fun () -> Memo.exec consumer ())))
      ~iter:(fun () ->
        if !released then failwith "unexpected suspension";
        released := true;
        [ Fiber.Fill (release_shared, ()) ])
  in
  printfn "blocked siblings share a rule: %b" (first_a == first_b);
  let second = run (lookup reverse_b) in
  let first = run (lookup reverse_a) in
  printfn "reverse siblings share a rule: %b" (first == second);
  let first, (same, second) =
    run
      (Memo.fork_and_join
         (fun () -> lookup parallel_a)
         (fun () ->
            Memo.fork_and_join
              (fun () -> lookup_for_build parallel_a)
              (fun () -> lookup_for_build parallel_b)))
  in
  printfn "concurrent same-target lookups share a rule: %b" (first == same);
  printfn "concurrent siblings share a rule: %b" (first == second);
  printfn
    "first-run producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  [%expect
    {|
    blocked siblings share a rule: true
    reverse siblings share a rule: true
    concurrent same-target lookups share a rule: true
    concurrent siblings share a rule: true
    first-run producers: independent, parallel, reverse, shared
    |}];
  split := true;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  (* A sibling lookup must keep its own current-run dependency even when it
     reuses a completed result: the new a producer depends on this consumer. *)
  let split_b = run (Memo.exec consumer ()) in
  let names (rule : Rule.t) =
    Filename.Set.to_list_map rule.targets.files ~f:Filename.to_string
    |> String.concat ~sep:", "
  in
  printfn "consumer's new targets: %s" (names split_b);
  printfn "consumer runs: %d" !consumer_runs;
  printfn
    "restored producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  let split_a = run (lookup a) in
  printfn "a's new targets: %s" (names split_a);
  printfn "split rules are distinct: %b" (split_a != split_b);
  [%expect
    {|
    consumer's new targets: b
    consumer runs: 2
    restored producers: b, independent
    a's new targets: a
    split rules are distinct: true
    |}];
  split := false;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  let complete =
    match run (Load_rules.load_dir ~dir:(Path.build context.build_dir)) with
    | Build { rules_here; _ } ->
      Path.Build.Map.find rules_here.by_file_targets a |> Option.value_exn
    | _ -> Code_error.raise "Expected build-directory rules" []
  in
  let warmed_b = run (Memo.exec consumer ()) in
  let warmed_a = run (lookup a) in
  printfn "complete and point lookup share a rule: %b" (complete == warmed_b);
  printfn "warmed siblings share a rule: %b" (warmed_a == warmed_b);
  printfn "warmed consumer targets: %s" (names warmed_b);
  printfn
    "warmed producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  [%expect
    {|
    complete and point lookup share a rule: true
    warmed siblings share a rule: true
    warmed consumer targets: a, b
    warmed producers: independent, parallel, reverse, shared
    |}];
  split := true;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  (* Reusing the complete result must not retain a dependency on its broad
     query: restoring it would also force a and cycle through the consumer. *)
  let split_b = run (Memo.exec consumer ()) in
  printfn "after complete, consumer targets: %s" (names split_b);
  printfn "consumer runs: %d" !consumer_runs;
  printfn
    "after complete, restored producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  let split_a = run (lookup a) in
  printfn "after complete, a's targets: %s" (names split_a);
  printfn "split rules are distinct: %b" (split_a != split_b);
  [%expect
    {|
    after complete, consumer targets: b
    consumer runs: 4
    after complete, restored producers: b, independent
    after complete, a's targets: a
    split rules are distinct: true
    |}];
  split := false;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  let selector =
    File_selector.of_predicate_lang
      ~dir:(Path.build context.build_dir)
      (Predicate_lang.Glob.of_string_list [ "a"; "reverse-a" ])
  in
  let selected =
    match run (Load_rules.load_file_selector selector) with
    | Build { rules_here; _ } ->
      Path.Build.Map.find rules_here.by_file_targets a |> Option.value_exn
    | _ -> Code_error.raise "Expected file-selector rules" []
  in
  let warmed_b = run (Memo.exec consumer ()) in
  let warmed_a = run (lookup a) in
  printfn "selector and point lookup share a rule: %b" (selected == warmed_b);
  printfn "selector-warmed siblings share a rule: %b" (warmed_a == warmed_b);
  printfn
    "selector-warmed producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  [%expect
    {|
    selector and point lookup share a rule: true
    selector-warmed siblings share a rule: true
    selector-warmed producers: independent, reverse, shared
    |}];
  split := true;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  let split_b = run (Memo.exec consumer ()) in
  printfn "after selector, consumer targets: %s" (names split_b);
  printfn "consumer runs: %d" !consumer_runs;
  printfn
    "after selector, restored producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  let split_a = run (lookup a) in
  printfn "after selector, a's targets: %s" (names split_a);
  printfn "split rules are distinct: %b" (split_a != split_b);
  [%expect
    {|
    after selector, consumer targets: b
    consumer runs: 6
    after selector, restored producers: b, independent
    after selector, a's targets: a
    split rules are distinct: true
    |}];
  printfn
    "late target initially absent: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build late))));
  printfn
    "missing build target rejected: %b"
    (try
       ignore (run (lookup_for_build late) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with (User_message.to_string message) ~prefix:"No rule found for");
  late_enabled := true;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  let late_rule = run (lookup_for_build late) in
  printfn "late target after reset: %s" (names late_rule);
  printfn "late lookups share a rule: %b" (late_rule == run (lookup late));
  printfn
    "late lookup producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  [%expect
    {|
    late target initially absent: true
    missing build target rejected: true
    late target after reset: late
    late lookups share a rule: true
    late lookup producers: late
    |}];
  late_enabled := false;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  printfn
    "cached file disappears after reset: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build late))));
  printfn
    "removed build target rejected: %b"
    (try
       ignore (run (lookup_for_build late) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with (User_message.to_string message) ~prefix:"No rule found for");
  printfn "removed target forces no producers: %b" (List.is_empty !forced);
  [%expect
    {|
    cached file disappears after reset: true
    removed build target rejected: true
    removed target forces no producers: true
    |}];
  let left_rule = run (lookup_for_build left) in
  let right_rule = run (lookup right) in
  printfn
    "same filenames in different directories are distinct: %b"
    (left_rule != right_rule);
  printfn
    "both directory roots are preserved: %b"
    (Path.Build.equal left_rule.targets.root left_dir
     && Path.Build.equal right_rule.targets.root right_dir);
  printfn
    "each directory reuses its own completed rule: %b"
    (left_rule == run (lookup left) && right_rule == run (lookup_for_build right));
  printfn
    "cross-directory producers: %s"
    (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
  [%expect
    {|
    same filenames in different directories are distinct: true
    both directory roots are preserved: true
    each directory reuses its own completed rule: true
    cross-directory producers: left, right
    |}];
  invalid_enabled := true;
  Memo.reset Memo.Invalidation.empty;
  let reports = ref 0 in
  let errors =
    Fiber.run
      (Fiber.collect_errors (fun () ->
         Memo.run_with_error_handler
           (fun () ->
              Memo.fork_and_join
                (fun () -> lookup invalid)
                (fun () -> lookup_for_build invalid))
           ~handle_error_no_raise:(fun _ ->
             incr reports;
             Fiber.return ())))
      ~iter:(fun () -> failwith "unexpected suspension")
    |> function
    | Ok _ -> []
    | Error errors -> errors
  in
  printfn "concurrent lookup errors: %d" (List.length errors);
  printfn
    "conflicting targets rejected: %b"
    (List.for_all errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
       let exn =
         match exn with
         | Memo.Error.E error -> Memo.Error.get error
         | exn -> exn
       in
       match exn with
       | User_error.E message ->
         String.starts_with
           (User_message.to_string message)
           ~prefix:"Multiple rules generated"
       | _ -> false));
  printfn "error reports: %d" !reports;
  [%expect
    {|
    concurrent lookup errors: 1
    conflicting targets rejected: true
    error reports: 1
    |}];
  Memo.reset Memo.Invalidation.empty;
  ignore (run (lookup selection_seed) : Rule.t);
  let conflict_rejected target =
    try
      ignore (run (lookup target) : Rule.t);
      false
    with
    | User_error.E message ->
      String.starts_with
        (User_message.to_string message)
        ~prefix:"Multiple rules generated"
  in
  printfn
    "revealed sibling still checks destination sources: %b"
    (conflict_rejected source_conflict);
  let fallback_selected = run (lookup fallback) in
  printfn
    "revealed fallback uses the destination source: %b"
    (fallback_selected != fallback_rule
     &&
     match fallback_selected.info with
     | Source_file_copy _ -> true
     | Internal | From_dune_file _ -> false);
  printfn
    "revealed promotion suppresses the destination source: %b"
    (run (lookup promoted) == promoted_rule);
  printfn
    "nonempty child tree still checks its own producers: %b"
    (conflict_rejected alternate);
  printfn
    "revealed family runs: %d; alternate producer runs: %d"
    !selection_runs
    !alternate_runs;
  [%expect
    {|
    revealed sibling still checks destination sources: true
    revealed fallback uses the destination source: true
    revealed promotion suppresses the destination source: true
    nonempty child tree still checks its own producers: true
    revealed family runs: 1; alternate producer runs: 1
    |}];
  (* The publisher follows a coarse sibling producer whose directory target
     is declared only by the publisher's directory. The destination must use
     its own selection when that declaration is absent there. *)
  ignore (run (lookup publisher) : Rule.t);
  let candidate_rule = run (lookup candidate) in
  printfn
    "publisher-only declarations do not escape to a sibling: %b"
    (Path.Build.equal candidate_rule.targets.root declarations_right);
  printfn "directory producer runs: %d" !directory_producer_runs;
  [%expect
    {|
    publisher-only declarations do not escape to a sibling: true
    directory producer runs: 1
    |}];
  ignore (run (lookup epoch_left) : Rule.t);
  let first_epoch_right = run (Memo.exec epoch_consumer ()) in
  printfn
    "cross-directory consumer root: %b"
    (Path.Build.equal first_epoch_right.targets.root (Path.Build.parent_exn epoch_right));
  printfn "first epoch producers: %s" (List.rev !epoch_forced |> String.concat ~sep:", ");
  [%expect
    {|
    cross-directory consumer root: true
    first epoch producers: shared
    |}];
  epoch_split := true;
  epoch_forced := [];
  Memo.reset Memo.Invalidation.empty;
  (* Restoring this sibling consumer must not restore its publisher's old
     dependencies: the new left producer now depends on this consumer. *)
  let split_epoch_right = run (Memo.exec epoch_consumer ()) in
  printfn "cross-directory consumer runs: %d" !epoch_consumer_runs;
  printfn
    "restored cross-directory producers: %s"
    (List.rev !epoch_forced |> String.concat ~sep:", ");
  printfn "consumer sees the new sibling rule: %b" (split_epoch_right != first_epoch_right);
  let split_epoch_left = run (lookup_for_build epoch_left) in
  printfn
    "split cross-directory rules are distinct: %b"
    (split_epoch_left != split_epoch_right);
  printfn
    "split cross-directory producers: %s"
    (List.rev !epoch_forced |> String.concat ~sep:", ");
  [%expect
    {|
    cross-directory consumer runs: 2
    restored cross-directory producers: right
    consumer sees the new sibling rule: true
    split cross-directory rules are distinct: true
    split cross-directory producers: right, left
    |}];
  split := false;
  Memo.reset Memo.Invalidation.empty;
  let shared = run (lookup a) in
  let reflective_runs = ref 0 in
  let reflective_consumer =
    Memo.create
      "completed-file-view-reflective-consumer"
      ~input:(module Unit)
      (fun () ->
         incr reflective_runs;
         lookup b)
  in
  (* [lookup a] cached both outputs. This consumer uses [get_rule], so the
     positive result bypasses the target-specific Memo node entirely. *)
  printfn
    "reflective consumer reuses the cached sibling: %b"
    (run (Memo.exec reflective_consumer ()) == shared);
  split := true;
  forced := [];
  Memo.reset Memo.Invalidation.empty;
  let reflected = run (Memo.exec reflective_consumer ()) in
  printfn "restored reflective targets: %s" (names reflected);
  printfn "reflective consumer runs: %d" !reflective_runs;
  printfn "restored reflective rule is new: %b" (reflected != shared);
  printfn "restored reflective producers: %s" (String.concat (List.rev !forced) ~sep:", ");
  [%expect
    {|
    reflective consumer reuses the cached sibling: true
    restored reflective targets: b
    reflective consumer runs: 2
    restored reflective rule is new: true
    restored reflective producers: b
    |}];
  Path.mkdir_p (Path.build cleanup_context.build_dir);
  let write path = Io.write_file_exn (Path.build path) "existing output" in
  let exists path = Fpath.exists (Path.Build.to_string path) in
  write cleanup_live;
  write cleanup_stale;
  write cleanup_pending;
  let first = run (Memo.exec cleanup_consumer ()) in
  printfn "cleanup selects the stable rule: %b" (first == cleanup_rule);
  printfn "initial stale output removed: %b" (not (exists cleanup_stale));
  printfn "initial live output preserved: %b" (exists cleanup_live);
  printfn "cleanup producer runs: %d" !cleanup_producer_runs;
  printfn "cleanup consumer runs: %d" !cleanup_consumer_runs;
  [%expect
    {|
    cleanup selects the stable rule: true
    initial stale output removed: true
    initial live output preserved: true
    cleanup producer runs: 1
    cleanup consumer runs: 1
    |}];
  printfn "initial pending output preserved: %b" (exists cleanup_pending);
  printfn "initial pending producer remains unforced: %b" (!cleanup_pending_runs = 0);
  [%expect
    {|
    initial pending output preserved: true
    initial pending producer remains unforced: true
    |}];
  (* Unchanged ownership reuses cleanup. An external recreation is not observed
     until a relevant dependency changes. *)
  write cleanup_stale;
  Memo.reset (Memo.Var.set cleanup_unrelated 1);
  let restored = run (Memo.exec cleanup_consumer ()) in
  printfn "restored selection keeps rule identity: %b" (restored == first);
  printfn "unobserved recreated output preserved: %b" (exists cleanup_stale);
  printfn "restored live output preserved: %b" (exists cleanup_live);
  printfn "restored producer runs: %d" !cleanup_producer_runs;
  printfn "restored consumer runs: %d" !cleanup_consumer_runs;
  [%expect
    {|
    restored selection keeps rule identity: true
    unobserved recreated output preserved: true
    restored live output preserved: true
    restored producer runs: 1
    restored consumer runs: 1
    |}];
  printfn "restored pending output preserved: %b" (exists cleanup_pending);
  printfn "restored pending producer remains unforced: %b" (!cleanup_pending_runs = 0);
  [%expect
    {|
    restored pending output preserved: true
    restored pending producer remains unforced: true
    |}];
  write cleanup_stale;
  Memo.reset (Memo.Var.set cleanup_enabled false);
  printfn
    "disabled restored rule is rejected: %b"
    (try
       ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with (User_message.to_string message) ~prefix:"No rule found for");
  printfn "disabled output removed: %b" (not (exists cleanup_live));
  printfn "disabled producer removes stale output: %b" (not (exists cleanup_stale));
  printfn "disabled producer runs: %d" !cleanup_producer_runs;
  [%expect
    {|
    disabled restored rule is rejected: true
    disabled output removed: true
    disabled producer removes stale output: true
    disabled producer runs: 2
    |}];
  write cleanup_stale;
  Memo.reset (Memo.Var.set cleanup_unrelated 2);
  printfn
    "unchanged negative selection is rejected: %b"
    (try
       ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with (User_message.to_string message) ~prefix:"No rule found for");
  printfn
    "unchanged negative selection preserves unobserved recreation: %b"
    (exists cleanup_stale);
  printfn "unchanged negative producer runs: %d" !cleanup_producer_runs;
  [%expect
    {|
    unchanged negative selection is rejected: true
    unchanged negative selection preserves unobserved recreation: true
    unchanged negative producer runs: 2
    |}];
  write cleanup_stale;
  Memo.reset (Memo.Var.set cleanup_enabled true);
  printfn
    "re-enabled producer restores the stable rule: %b"
    (run (Memo.exec cleanup_consumer ()) == cleanup_rule);
  printfn "re-enabled producer removes stale output: %b" (not (exists cleanup_stale));
  printfn "re-enabled producer runs: %d" !cleanup_producer_runs;
  [%expect
    {|
    re-enabled producer restores the stable rule: true
    re-enabled producer removes stale output: true
    re-enabled producer runs: 3
    |}];
  let changed_rule =
    Rule.set_action
      cleanup_rule
      (Action_builder.return (Action.Full.make (Action.Echo [ "changed" ])))
  in
  let previous_consumer_runs = !cleanup_consumer_runs in
  Memo.reset (Memo.Var.set cleanup_rule_input changed_rule);
  let changed = run (Memo.exec cleanup_consumer ()) in
  let action, _ = run (Action_builder.evaluate_and_collect_deps changed.action) in
  printfn "changed action retains rule ID: %b" (Rule.equal changed cleanup_rule);
  printfn "changed action replaces the rule object: %b" (changed == changed_rule);
  printfn
    "consumer observes the changed action: %b"
    (match action.action with
     | Echo [ "changed" ] -> true
     | _ -> false);
  printfn
    "changed-action consumer runs: %d"
    (!cleanup_consumer_runs - previous_consumer_runs);
  printfn "changed-action producer runs: %d" !cleanup_producer_runs;
  [%expect
    {|
    changed action retains rule ID: true
    changed action replaces the rule object: true
    consumer observes the changed action: true
    changed-action consumer runs: 1
    changed-action producer runs: 4
    |}];
  let generated_fallback = run (lookup_for_build cleanup_fallback) in
  let generated_promoted = run (lookup_for_build cleanup_promoted) in
  let previous_producer_runs = !cleanup_producer_runs in
  (* Only source metadata changes. Reusing the producer's unchanged rules must
     still reapply fallback, promotion, and source-conflict validation. *)
  Memo.reset
    (Memo.Var.set
       cleanup_sources
       (List.map [ "fallback"; "live"; "promoted" ] ~f:Filename.of_string_exn
        |> Filename.Array.Set.of_sorted_list));
  let source_fallback = run (lookup_for_build cleanup_fallback) in
  printfn
    "added source overrides the stable fallback: %b"
    (generated_fallback == cleanup_fallback_rule
     &&
     match source_fallback.info with
     | Source_file_copy _ -> true
     | Internal | From_dune_file _ -> false);
  printfn
    "added source preserves the stable promotion: %b"
    (generated_promoted == cleanup_promoted_rule
     && run (lookup_for_build cleanup_promoted) == generated_promoted);
  printfn
    "added source conflicts with the stable standard rule: %b"
    (try
       ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with
         (User_message.to_string message)
         ~prefix:"Multiple rules generated");
  printfn
    "source reset keeps the producer cached: %b"
    (!cleanup_producer_runs = previous_producer_runs);
  [%expect
    {|
    added source overrides the stable fallback: true
    added source preserves the stable promotion: true
    added source conflicts with the stable standard rule: true
    source reset keeps the producer cached: true
    |}];
  Memo.reset (Memo.Var.set cleanup_sources Filename.Array.Set.empty);
  printfn
    "removed source restores the stable fallback: %b"
    (run (lookup_for_build cleanup_fallback) == generated_fallback);
  printfn
    "removed source preserves the stable promotion: %b"
    (run (lookup_for_build cleanup_promoted) == generated_promoted);
  printfn
    "removed source restores the stable standard rule: %b"
    (run (Memo.exec cleanup_consumer ()) == changed_rule);
  printfn
    "source removal keeps the producer cached: %b"
    (!cleanup_producer_runs = previous_producer_runs);
  [%expect
    {|
    removed source restores the stable fallback: true
    removed source preserves the stable promotion: true
    removed source restores the stable standard rule: true
    source removal keeps the producer cached: true
    |}];
  let joined = run (Memo.exec cleanup_nested_consumer ()) in
  printfn
    "nested targets share the joined rule: %b"
    (run (lookup_for_build cleanup_nested_a) == joined);
  printfn
    "initial nested producers: %s"
    (List.rev !cleanup_nested_forced |> String.concat ~sep:", ");
  [%expect
    {|
    nested targets share the joined rule: true
    initial nested producers: joined
    |}];
  let previous_generator_runs = !cleanup_generator_runs in
  let previous_nested_runs = !cleanup_nested_runs in
  cleanup_nested_forced := [];
  (* Only the parent producer changes: restoring the sibling consumer from
     the new [a] child must not restore the old joined selector's dependencies. *)
  Memo.reset (Memo.Var.set cleanup_nested_split true);
  let split_a = run (lookup_for_build cleanup_nested_a) in
  let split_b = run (Memo.exec cleanup_nested_consumer ()) in
  printfn "nested split a targets: %s" (names split_a);
  printfn "nested split b targets: %s" (names split_b);
  printfn "nested split rules are distinct: %b" (split_a != split_b);
  printfn "nested sibling consumer runs: %d" !cleanup_nested_consumer_runs;
  printfn
    "nested split producers: %s"
    (List.rev !cleanup_nested_forced |> String.concat ~sep:", ");
  printfn
    "nested split reuses the root generator: %b"
    (!cleanup_generator_runs = previous_generator_runs);
  printfn
    "nested parent producer reruns once: %b"
    (!cleanup_nested_runs = previous_nested_runs + 1);
  [%expect
    {|
    nested split a targets: nested-a
    nested split b targets: nested-b
    nested split rules are distinct: true
    nested sibling consumer runs: 2
    nested split producers: a, b
    nested split reuses the root generator: true
    nested parent producer reruns once: true
    |}];
  (* Warm a first so a family route would choose it. After splitting, only b
     is requested; a's producer would wait for this same b consumer. *)
  assert (Memo.is_incremental ());
  Memo.reset (Memo.Var.set cleanup_nested_split false);
  let joined = run (lookup_for_build cleanup_nested_a) in
  assert (run (Memo.exec cleanup_nested_consumer ()) == joined);
  let previous_generator_runs = !cleanup_generator_runs in
  let previous_nested_runs = !cleanup_nested_runs in
  let previous_consumer_runs = !cleanup_nested_consumer_runs in
  cleanup_nested_forced := [];
  Memo.reset (Memo.Var.set cleanup_nested_split true);
  let split_b = run (Memo.exec cleanup_nested_consumer ()) in
  printfn
    "b-only split uses its new owner: %b"
    (split_b != joined && String.equal (names split_b) "nested-b");
  printfn
    "b-only split avoids its former sibling: %b"
    (List.equal String.equal (List.rev !cleanup_nested_forced) [ "b" ]);
  printfn
    "b-only split keeps the root generator: %b"
    (!cleanup_generator_runs = previous_generator_runs);
  assert (!cleanup_nested_runs = previous_nested_runs + 1);
  assert (!cleanup_nested_consumer_runs = previous_consumer_runs + 1);
  [%expect
    {|
    b-only split uses its new owner: true
    b-only split avoids its former sibling: true
    b-only split keeps the root generator: true
    |}];
  let directory_transition_runs = ref 0 in
  let directory_transition_consumer =
    Memo.create
      "revealed-sibling-directory-transition"
      ~input:(module Unit)
      (fun () ->
         incr directory_transition_runs;
         lookup_for_build generated_leaf)
  in
  Memo.reset (Memo.Var.set generated_directory_enabled false);
  printfn
    "normal directory selects its file rule: %b"
    (run (Memo.exec directory_transition_consumer ()) == generated_leaf_rule);
  (* The parent subtree advertises [generated/leaf] in normal mode. Once the
     same directory becomes a target, its cached file view must be bypassed. *)
  Memo.reset (Memo.Var.set generated_directory_enabled true);
  printfn
    "new directory target replaces the cached file rule: %b"
    (run (Memo.exec directory_transition_consumer ()) == generated_directory_rule);
  Memo.reset (Memo.Var.set generated_directory_enabled false);
  printfn
    "removed directory target restores the file rule: %b"
    (run (Memo.exec directory_transition_consumer ()) == generated_leaf_rule);
  printfn "directory transition consumer runs: %d" !directory_transition_runs;
  [%expect
    {|
    normal directory selects its file rule: true
    new directory target replaces the cached file rule: true
    removed directory target restores the file rule: true
    directory transition consumer runs: 3
    |}];
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Var.set generated_directory_enabled true)
       (Memo.Var.set generated_directory_declared false));
  printfn
    "point lookup rejects an undeclared revealed directory: %b"
    (try
       ignore (run (lookup_for_build publisher) : Rule.t);
       false
     with
     | Code_error.E { message; _ } ->
       String.equal message "Rule stage produced an undeclared directory target");
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Var.set generated_directory_enabled false)
       (Memo.Var.set generated_directory_declared true));
  [%expect {| point lookup rejects an undeclared revealed directory: true |}];
  (* This child is neither a source directory nor an internal subdirectory.
     Only its parent's unforced alias mask permits rules in it. *)
  visibility_parent_runs := 0;
  printfn
    "parent alias ownership permits child rules: %b"
    (run (lookup_for_build visibility_first) == visibility_first_rule
     && run (lookup_for_build visibility_second) == visibility_second_rule);
  printfn "visibility child producers: %d" !visibility_child_runs;
  printfn "visibility parent producer remains unforced: %b" (!visibility_parent_runs = 0);
  [%expect
    {|
    parent alias ownership permits child rules: true
    visibility child producers: 2
    visibility parent producer remains unforced: true
    |}];
  Memo.reset (Memo.Var.set visibility_allowed false);
  printfn
    "empty selection skips visibility rejection: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build visibility_missing))));
  let visibility_error target expected_rule =
    try
      ignore (run (lookup_for_build target) : Rule.t);
      false
    with
    | Code_error.E { message; data; _ } ->
      let expected =
        Rules.find (Rules.of_rules [ expected_rule ]) (Path.build visibility_dir)
        |> Rules.Dir_rules.to_dyn
        |> Dyn.to_string
      in
      String.equal message "Generated rules in a directory not allowed by the parent"
      && List.exists data ~f:(function
        | "rules", actual -> String.equal (Dyn.to_string actual) expected
        | _ -> false)
  in
  printfn
    "disallowed first reports only its own rule: %b"
    (visibility_error visibility_first visibility_first_rule);
  printfn
    "disallowed second reports only its own rule: %b"
    (visibility_error visibility_second visibility_second_rule);
  printfn "visibility reset reuses child producers: %d" !visibility_child_runs;
  [%expect
    {|
    empty selection skips visibility rejection: true
    disallowed first reports only its own rule: true
    disallowed second reports only its own rule: true
    visibility reset reuses child producers: 2
    |}];
  Memo.reset (Memo.Var.set visibility_allowed true);
  printfn
    "restored parent ownership permits both rules again: %b"
    (run (lookup_for_build visibility_first) == visibility_first_rule
     && run (lookup_for_build visibility_second) == visibility_second_rule);
  printfn "restored visibility keeps child producers cached: %d" !visibility_child_runs;
  printfn
    "restored visibility keeps parent producer unforced: %b"
    (!visibility_parent_runs = 0);
  [%expect
    {|
    restored parent ownership permits both rules again: true
    restored visibility keeps child producers cached: 2
    restored visibility keeps parent producer unforced: true
    |}];
  let cached_sibling_runs = ref 0 in
  let cached_sibling_consumer =
    Memo.create
      "completed-file-view-warmed-sibling-consumer"
      ~input:(module Unit)
      (fun () ->
         incr cached_sibling_runs;
         lookup_for_build reverse_b)
  in
  let before_reset = run (lookup_for_build reverse_a) in
  printfn
    "sibling consumer initially shares the warmed rule: %b"
    (run (Memo.exec cached_sibling_consumer ()) == before_reset);
  Memo.reset Memo.Invalidation.empty;
  let after_reset = run (lookup_for_build reverse_a) in
  printfn "warmed rule changes after reset: %b" (after_reset != before_reset);
  printfn
    "restored sibling consumer sees the newly warmed rule: %b"
    (run (Memo.exec cached_sibling_consumer ()) == after_reset);
  printfn "warmed sibling consumer runs: %d" !cached_sibling_runs;
  [%expect
    {|
    sibling consumer initially shares the warmed rule: true
    warmed rule changes after reset: true
    restored sibling consumer sees the newly warmed rule: true
    warmed sibling consumer runs: 2
    |}];
  printfn
    "present context selects the stable rule: %b"
    (run (Memo.exec cleanup_consumer ()) == changed_rule);
  Memo.reset (Memo.Var.set cleanup_context_enabled false);
  printfn
    "removed context rejects the restored target: %b"
    (try
       ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with (User_message.to_string message) ~prefix:"Trying to build ");
  assert (
    try
      ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
      false
    with
    | User_error.E message ->
      String.starts_with (User_message.to_string message) ~prefix:"Trying to build ");
  Memo.reset (Memo.Var.set cleanup_context_enabled true);
  printfn
    "restored context selects the stable rule: %b"
    (run (Memo.exec cleanup_consumer ()) == changed_rule);
  [%expect
    {|
    present context selects the stable rule: true
    removed context rejects the restored target: true
    restored context selects the stable rule: true
    |}];
  write cleanup_pending;
  Memo.reset (Memo.Var.set cleanup_unrelated 3);
  ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
  printfn "unrequested pending output remains: %b" (exists cleanup_pending);
  printfn "unrequested pending producer remains unforced: %b" (!cleanup_pending_runs = 0);
  (* Refining the live rule must not force this pending producer: it depends
     on the live consumer whose restoration initiated that cleanup. *)
  printfn
    "requested empty producer can consult the live consumer: %b"
    (try
       ignore (run (lookup_for_build cleanup_pending) : Rule.t);
       false
     with
     | User_error.E message ->
       String.starts_with (User_message.to_string message) ~prefix:"No rule found for");
  printfn "requested pending output removed: %b" (not (exists cleanup_pending));
  printfn "requested pending producer runs: %d" !cleanup_pending_runs;
  [%expect
    {|
    unrequested pending output remains: true
    unrequested pending producer remains unforced: true
    requested empty producer can consult the live consumer: true
    requested pending output removed: true
    requested pending producer runs: 1
    |}];
  (* A broad-only query also starts a new cleanup generation. Do not reuse an
     older A+B receipt after an intervening A-only view preserved B's output. *)
  write cleanup_pending;
  Memo.reset (Memo.Var.set cleanup_unrelated 100);
  let live_selector =
    File_selector.of_predicate_lang
      ~dir:(Path.build cleanup_context.build_dir)
      (Predicate_lang.Glob.of_string_list [ "live" ])
  in
  ignore (run (Load_rules.load_file_selector live_selector) : Load_rules.Loaded.t);
  printfn "broad-only generation preserves pending output: %b" (exists cleanup_pending);
  Memo.reset (Memo.Var.set cleanup_unrelated 101);
  printfn
    "older combined selection remains negative: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build cleanup_pending))));
  printfn
    "older combined selection removes the intervening output: %b"
    (not (exists cleanup_pending));
  printfn
    "intervening generation keeps the empty producer cached: %d"
    !cleanup_pending_runs;
  [%expect
    {|
    broad-only generation preserves pending output: true
    older combined selection remains negative: true
    older combined selection removes the intervening output: true
    intervening generation keeps the empty producer cached: 1
    |}];
  let metadata_consumer_runs = ref 0 in
  let metadata_consumer =
    Memo.create
      "cleanup-metadata-consumer"
      ~input:(module Unit)
      (fun () ->
         incr metadata_consumer_runs;
         lookup_for_build metadata_live)
  in
  write metadata_stale;
  Memo.reset (Memo.Var.set cleanup_unrelated 4);
  printfn
    "initial metadata selects the stable rule: %b"
    (run (Memo.exec metadata_consumer ()) == metadata_rule);
  printfn "initial metadata preserves its sibling: %b" (exists metadata_stale);
  printfn "initial metadata consumer runs: %d" !metadata_consumer_runs;
  [%expect
    {|
    initial metadata selects the stable rule: true
    initial metadata preserves its sibling: true
    initial metadata consumer runs: 1
    |}];
  (* The result is unchanged, but its producer and cleanup proof are replaced.
     Even after a cutoff, replay must retain the new proof and dependencies. *)
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Var.set metadata_replaced true)
       (Memo.Var.set obsolete_metadata true));
  printfn
    "changed metadata keeps the requested rule: %b"
    (run (Memo.exec metadata_consumer ()) == metadata_rule);
  printfn "changed metadata removes its stale sibling: %b" (not (exists metadata_stale));
  printfn "changed metadata keeps the consumer restored: %d" !metadata_consumer_runs;
  printfn "changed metadata skips the obsolete producer: %d" !metadata_old_runs;
  [%expect
    {|
    changed metadata keeps the requested rule: true
    changed metadata removes its stale sibling: true
    changed metadata keeps the consumer restored: 1
    changed metadata skips the obsolete producer: 1
    |}];
  write metadata_stale;
  Memo.reset (Memo.Var.set cleanup_unrelated 5);
  printfn
    "restored metadata keeps the requested rule: %b"
    (run (Memo.exec metadata_consumer ()) == metadata_rule);
  printfn
    "restored metadata removes its recreated sibling: %b"
    (not (exists metadata_stale));
  printfn "restored metadata keeps the consumer restored: %d" !metadata_consumer_runs;
  printfn "restored metadata skips the obsolete producer: %d" !metadata_old_runs;
  [%expect
    {|
    restored metadata keeps the requested rule: true
    restored metadata removes its recreated sibling: true
    restored metadata keeps the consumer restored: 1
    restored metadata skips the obsolete producer: 1
    |}];
  (* Keep several inventory blocks pending while a file becomes a directory. *)
  List.iter cleanup_padding ~f:write;
  let padding_is_pending () =
    !cleanup_padding_runs = 0 && List.for_all cleanup_padding ~f:exists
  in
  write cleanup_kind;
  Memo.reset (Memo.Var.set cleanup_unrelated 6);
  ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
  printfn
    "kind-change inventory retains the pending file: %b"
    (exists cleanup_kind && !cleanup_kind_runs = 0);
  printfn "kind-change inventory retains unrequested padding: %b" (padding_is_pending ());
  Fpath.unlink_exn (Path.Build.to_string cleanup_kind);
  Path.mkdir_p (Path.build cleanup_kind);
  printfn
    "empty file producer leaves no rule: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build cleanup_kind))));
  printfn
    "pending alias retains the replacement directory: %b"
    (exists cleanup_kind
     && !cleanup_kind_runs = 1
     && !cleanup_kind_alias_runs = 0
     && padding_is_pending ());
  (* Refining the alias later in this run must use the inventory's new kind
     and widened subtree mask, without rereading or forgetting the entry. *)
  ignore (run (Load_rules.get_rule (Path.build cleanup_kind_trigger)) : Rule.t option);
  printfn
    "retired alias leaves the nested file pending: %b"
    (exists cleanup_kind
     && !cleanup_kind_alias_runs = 1
     && !cleanup_kind_file_runs = 0
     && padding_is_pending ());
  (* Both triggers select the broad file and alias producers, so only the
     exact nested-file declaration changes next. Its parent trigger is
     outside the inventory's filename bounds. *)
  printfn
    "empty nested-file producer leaves no rule: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build cleanup_kind_file_trigger))));
  printfn
    "pure-file refinement removes the replacement directory: %b"
    ((not (exists cleanup_kind)) && !cleanup_kind_file_runs = 1);
  printfn "cleanup refinements keep the padding unforced: %b" (padding_is_pending ());
  [%expect
    {|
    kind-change inventory retains the pending file: true
    kind-change inventory retains unrequested padding: true
    empty file producer leaves no rule: true
    pending alias retains the replacement directory: true
    retired alias leaves the nested file pending: true
    empty nested-file producer leaves no rule: true
    pure-file refinement removes the replacement directory: true
    cleanup refinements keep the padding unforced: true
    |}];
  (* Warm both trigger payloads while the file owner is still pending. Their
     unchanged payloads cannot skip a later file-to-directory refinement. *)
  write cleanup_kind;
  Memo.reset (Memo.Var.set cleanup_unrelated 102);
  ignore (run (Load_rules.get_rule (Path.build cleanup_kind_trigger)) : Rule.t option);
  ignore
    (run (Load_rules.get_rule (Path.build cleanup_kind_file_trigger)) : Rule.t option);
  printfn "warm trigger views preserve the pending file: %b" (exists cleanup_kind);
  Memo.reset (Memo.Var.set cleanup_unrelated 103);
  ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
  Fpath.unlink_exn (Path.Build.to_string cleanup_kind);
  Path.mkdir_p (Path.build cleanup_kind);
  ignore (run (Load_rules.get_rule (Path.build cleanup_kind)) : Rule.t option);
  printfn
    "restored file owner discovers the replacement directory: %b"
    (exists cleanup_kind && padding_is_pending ());
  ignore (run (Load_rules.get_rule (Path.build cleanup_kind_trigger)) : Rule.t option);
  printfn "restored alias view leaves the ghost pending: %b" (exists cleanup_kind);
  ignore
    (run (Load_rules.get_rule (Path.build cleanup_kind_file_trigger)) : Rule.t option);
  printfn
    "restored ghost view removes the grown directory: %b"
    ((not (exists cleanup_kind)) && padding_is_pending ());
  printfn
    "kind-change producers remain cached: %b"
    (!cleanup_kind_runs = 1 && !cleanup_kind_alias_runs = 1 && !cleanup_kind_file_runs = 1);
  [%expect
    {|
    warm trigger views preserve the pending file: true
    restored file owner discovers the replacement directory: true
    restored alias view leaves the ghost pending: true
    restored ghost view removes the grown directory: true
    kind-change producers remain cached: true
    |}];
  (* The initial entry still has the regular-file kind. Its grown copy was
     deleted, so that generation cannot certify this recreated file. *)
  write cleanup_kind;
  Memo.reset (Memo.Var.set cleanup_unrelated 106);
  printfn
    "previous growth invalidates the restored file-owner receipt: %b"
    (Option.is_none (run (Load_rules.get_rule (Path.build cleanup_kind)))
     && (not (exists cleanup_kind))
     && !cleanup_kind_runs = 1
     && padding_is_pending ());
  [%expect {| previous growth invalidates the restored file-owner receipt: true |}];
  (* A successful file owner protects its recorded kind in later views: it
     is either revealed or still covered by a pending ancestor declaration. *)
  write metadata_live;
  Memo.reset (Memo.Var.set cleanup_unrelated 104);
  ignore (run (Memo.exec metadata_consumer ()) : Rule.t);
  Memo.reset (Memo.Var.set cleanup_unrelated 105);
  ignore (run (Memo.exec metadata_consumer ()) : Rule.t);
  Fpath.unlink_exn (Path.Build.to_string metadata_live);
  Path.mkdir_p (Path.build metadata_live);
  let replacement_child = Path.Build.relative metadata_live "action-created" in
  write replacement_child;
  ignore (run (Load_rules.get_rule (Path.build metadata_stale)) : Rule.t option);
  printfn
    "partial parent preserves a live file's replacement directory: %b"
    (exists replacement_child);
  let metadata_selector =
    File_selector.of_predicate_lang
      ~dir:(Path.build cleanup_context.build_dir)
      (Predicate_lang.Glob.of_string_list [ "metadata-live" ])
  in
  ignore (run (Load_rules.load_file_selector metadata_selector) : Load_rules.Loaded.t);
  printfn
    "revealed owner preserves the replacement directory: %b"
    (exists replacement_child);
  printfn "replacement keeps the metadata consumer restored: %d" !metadata_consumer_runs;
  Fpath.unlink_exn (Path.Build.to_string replacement_child);
  Unix.rmdir (Path.Build.to_string metadata_live);
  write metadata_live;
  [%expect
    {|
    partial parent preserves a live file's replacement directory: true
    revealed owner preserves the replacement directory: true
    replacement keeps the metadata consumer restored: 1
    |}];
  let inherited_retention dir =
    let selector =
      File_selector.of_predicate_lang
        ~dir:(Path.build dir)
        ~only_generated_files:true
        Predicate_lang.true_
    in
    match run (Load_rules.load_file_selector selector) with
    | Build { allowed_subdirs; _ } -> allowed_subdirs
    | Source _ | External _ | Build_under_directory_target _ ->
      Code_error.raise "Expected inherited directory declarations" []
  in
  let keeps dirs name = Dir_set.mem dirs (Path.Local.of_string name) in
  let left = inherited_retention inherited_left in
  let leaf = inherited_retention inherited_leaf in
  let right = inherited_retention (inherited_dir "right") in
  printfn
    "finite declarations keep only named child roots: %b"
    (keeps left "."
     && keeps left "leaf"
     && keeps left "local"
     && not (keeps left "leaf/tip"));
  printfn
    "nested declarations stay local to their parents: %b"
    (keeps leaf "."
     && keeps leaf "tip"
     && (not (keeps leaf "tip/deep"))
     && keeps right "."
     && not (keeps right "leaf"));
  expect_code_error "a directory cannot authorize itself" (fun () ->
    ignore
      (run
         (Load_rules.get_rule (Path.build (Path.Build.relative inherited_orphan "probe")))
       : Rule.t option));
  [%expect
    {|
    finite declarations keep only named child roots: true
    nested declarations stay local to their parents: true
    a directory cannot authorize itself: code error
    |}];
  Memo.reset (Memo.Var.set inherited_phase 1);
  let leaf = inherited_retention inherited_leaf in
  let sibling = inherited_retention (inherited_dir "right/leaf") in
  printfn
    "all declarations reach arbitrary descendants: %b"
    (keeps leaf "." && keeps leaf "arbitrary/deep" && keeps sibling ".");
  Memo.reset (Memo.Var.set inherited_phase 2);
  (* Restore the cached child query without first loading its parent. *)
  let leaf = inherited_retention inherited_leaf in
  printfn
    "cached child observes revoked declarations: %b"
    ((not (keeps leaf ".")) && not (keeps leaf "tip"));
  Memo.reset (Memo.Var.set inherited_phase 0);
  let leaf = inherited_retention inherited_leaf in
  printfn
    "cached child observes restored declarations: %b"
    (keeps leaf "." && keeps leaf "tip" && not (keeps leaf "tip/deep"));
  [%expect
    {|
    all declarations reach arbitrary descendants: true
    cached child observes revoked declarations: true
    cached child observes restored declarations: true
    |}];
  Memo.reset (Memo.Var.set generated_directory_enabled true);
  let reflected_directory = run (lookup generated) in
  let previous_directory_runs = !directory_producer_runs in
  printfn
    "reflected directory and build lookup share a rule: %b"
    (run (lookup_for_build generated) == reflected_directory
     && reflected_directory == generated_directory_rule);
  printfn
    "raw directory lookup keeps its producer cached: %b"
    (!directory_producer_runs = previous_directory_runs);
  (* Warm the descendant classification before restoring its consumer alone. *)
  printfn
    "reflected descendant resolves its directory owner: %b"
    (run (lookup generated_leaf) == generated_directory_rule);
  let consumer =
    Memo.create
      "completed-directory-view-consumer"
      ~input:(module Unit)
      (fun () -> lookup_for_build generated_leaf)
  in
  printfn
    "descendant consumer resolves its directory owner: %b"
    (run (Memo.exec consumer ()) == generated_directory_rule);
  Memo.reset (Memo.Var.set generated_directory_enabled false);
  printfn
    "consumer alone observes the removed directory owner: %b"
    (run (Memo.exec consumer ()) == generated_leaf_rule);
  (* Directory discovery deliberately skips same-name file owners. Its result
         cannot answer an ordinary target lookup, even for mixed output rules. *)
  printfn
    "directory discovery selects only the directory rule: %b"
    (Option.value_exn (run (Load_rules.get_directory_rule conflicting_directory))
     == conflicting_directory_rule);
  printfn "directory discovery leaves file owners pending: %d" !directory_conflict_runs;
  let build_conflict target =
    try
      ignore (run (lookup_for_build target) : Rule.t);
      false
    with
    | User_error.E message ->
      String.starts_with
        (User_message.to_string message)
        ~prefix:"Multiple rules generated"
  in
  printfn
    "normal lookup rejects the discovered file-directory conflict: %b"
    (build_conflict conflicting_directory);
  printfn
    "mixed directory discovery preserves the atomic rule: %b"
    (Option.value_exn (run (Load_rules.get_directory_rule mixed_directory))
     == mixed_directory_rule);
  printfn "mixed discovery leaves its file owner pending: %d" !directory_conflict_runs;
  printfn
    "mixed file lookup validates its directory output: %b"
    (build_conflict mixed_file);
  printfn "normal lookups force both conflicting file owners: %d" !directory_conflict_runs;
  [%expect
    {|
    reflected directory and build lookup share a rule: true
    raw directory lookup keeps its producer cached: true
    reflected descendant resolves its directory owner: true
    descendant consumer resolves its directory owner: true
    consumer alone observes the removed directory owner: true
    directory discovery selects only the directory rule: true
    directory discovery leaves file owners pending: 0
    normal lookup rejects the discovered file-directory conflict: true
    mixed directory discovery preserves the atomic rule: true
    mixed discovery leaves its file owner pending: 1
    mixed file lookup validates its directory output: true
    normal lookups force both conflicting file owners: 2
    |}];
  let actions_child name = Path.Build.relative cleanup_failure_actions name in
  (* The producer changes only the anonymous-actions location after both
     inventories have been captured. Normal file refinement can still commit. *)
  List.iter [ "c"; "b" ] ~f:(fun name -> Path.mkdir_p (Path.build (actions_child name)));
  write cleanup_failure_ghost;
  Memo.reset (Memo.Var.set cleanup_failure_enabled true);
  let restore_actions () =
    if !cleanup_failure_moved
    then (
      (match Unix.unlink (Path.Build.to_string cleanup_failure_actions) with
       | () -> ()
       | exception Unix.Unix_error (ENOENT, _, _) -> ());
      Unix.rename
        (Path.Build.to_string cleanup_failure_saved)
        (Path.Build.to_string cleanup_failure_actions);
      cleanup_failure_moved := false)
  in
  Exn.protect ~finally:restore_actions ~f:(fun () ->
    printfn
      "anonymous cleanup fails on the first sorted stale entry: %b"
      (try
         ignore (run (lookup_for_build cleanup_failure_first) : Rule.t);
         false
       with
       | Unix.Unix_error (ELOOP, _, filename) ->
         String.equal filename (Path.Build.to_string (actions_child "b")));
    printfn
      "normal file cleanup committed before the failure: %b"
      (not (exists cleanup_failure_ghost));
    restore_actions ();
    printfn
      "failed anonymous cleanup leaves both entries available: %b"
      (exists (actions_child "b") && exists (actions_child "c"));
    write cleanup_failure_ghost;
    ignore (run (lookup_for_build cleanup_failure_retry) : Rule.t);
    printfn
      "same-epoch retry preserves the recreated normal file: %b"
      (exists cleanup_failure_ghost);
    printfn
      "same-epoch retry finishes anonymous cleanup: %b"
      ((not (exists (actions_child "b"))) && not (exists (actions_child "c")));
    printfn "cleanup retry reuses the producer: %b" (!cleanup_failure_runs = 1));
  Memo.reset Memo.Invalidation.empty;
  ignore (run (lookup_for_build cleanup_failure_first) : Rule.t);
  printfn
    "failed target retries cleanup before remembering its fresh receipt: %b"
    ((not (exists cleanup_failure_ghost))
     && (not (exists (actions_child "b")))
     && (not (exists (actions_child "c")))
     && !cleanup_failure_runs = 1);
  Memo.reset (Memo.Var.set cleanup_failure_enabled false);
  [%expect
    {|
    anonymous cleanup fails on the first sorted stale entry: true
    normal file cleanup committed before the failure: true
    failed anonymous cleanup leaves both entries available: true
    same-epoch retry preserves the recreated normal file: true
    same-epoch retry finishes anonymous cleanup: true
    cleanup retry reuses the producer: true
    failed target retries cleanup before remembering its fresh receipt: true
    |}];
  let failure_selector =
    file_selector ~dir:cleanup_context.build_dir "cleanup-failure-first"
  in
  ignore (run (Load_rules.load_file_selector failure_selector) : Load_rules.Loaded.t);
  List.iter [ "c"; "b" ] ~f:(fun name -> Path.mkdir_p (Path.build (actions_child name)));
  write cleanup_failure_ghost;
  let previous_failure_runs = !cleanup_failure_runs in
  Memo.reset (Memo.Var.set cleanup_failure_enabled true);
  Exn.protect ~finally:restore_actions ~f:(fun () ->
    assert (
      try
        ignore
          (run (Load_rules.load_file_selector failure_selector) : Load_rules.Loaded.t);
        false
      with
      | Unix.Unix_error (ELOOP, _, filename) ->
        String.equal filename (Path.Build.to_string (actions_child "b")));
    assert (not (exists cleanup_failure_ghost));
    restore_actions ();
    Memo.reset Memo.Invalidation.empty;
    ignore (run (Load_rules.load_file_selector failure_selector) : Load_rules.Loaded.t);
    assert (
      (not (exists (actions_child "b")))
      && (not (exists (actions_child "c")))
      && !cleanup_failure_runs = previous_failure_runs + 1));
  Memo.reset (Memo.Var.set cleanup_failure_enabled false);
  (* The initial Direct sets are empty, while the recursive declaration
     covers every file. Anonymous files and directory entries keep their
     distinct classification even when ordinary-file checks are elided. *)
  Path.mkdir_p (Path.build coverage_directory);
  Path.mkdir_p (Path.build coverage_action_dir);
  List.iter [ coverage_live; coverage_stale; coverage_action_file ] ~f:write;
  Unix.symlink "live" (Path.Build.to_string coverage_link);
  printfn
    "recursive coverage keeps entries pending until production: %b"
    (run (lookup_for_build coverage_live) == coverage_rule && !coverage_initial);
  printfn
    "recursive refinement removes stale files, links and directories: %b"
    (List.for_all
       [ coverage_stale; coverage_link; coverage_directory; coverage_action_dir ]
       ~f:(fun path -> not (exists path)));
  printfn
    "recursive refinement keeps live output and anonymous files: %b"
    (exists coverage_live && exists coverage_action_file);
  [%expect
    {|
    recursive coverage keeps entries pending until production: true
    recursive refinement removes stale files, links and directories: true
    recursive refinement keeps live output and anonymous files: true
    |}];
  (* Keep ownership cached across external recreations. Only the fixture replaces
     the old symlink; unchanged cleanup does not enumerate these entries again. *)
  List.iter
    [ "file-only inventory", coverage_stale
    ; "same-name recreation", coverage_stale
    ; "changed filename", coverage_path "later-stale"
    ]
    ~f:(fun (label, stale) ->
      write stale;
      if exists coverage_link then Unix.unlink (Path.Build.to_string coverage_link);
      Unix.symlink "live" (Path.Build.to_string coverage_link);
      Memo.reset Memo.Invalidation.empty;
      printfn
        "%s preserves unobserved recreated entries: %b"
        label
        (run (lookup_for_build coverage_live) == coverage_rule
         && exists stale
         && exists coverage_link
         && exists coverage_live
         && exists coverage_action_file));
  [%expect
    {|
    file-only inventory preserves unobserved recreated entries: true
    same-name recreation preserves unobserved recreated entries: true
    changed filename preserves unobserved recreated entries: true
    |}];
  List.iter [ 1; 2; 3 ] ~f:(fun epoch ->
    Memo.reset Memo.Invalidation.empty;
    printfn
      "unchanged inventory epoch %d preserves live entries: %b"
      epoch
      (run (lookup_for_build coverage_live) == coverage_rule
       && exists coverage_live
       && exists coverage_action_file));
  [%expect
    {|
    unchanged inventory epoch 1 preserves live entries: true
    unchanged inventory epoch 2 preserves live entries: true
    unchanged inventory epoch 3 preserves live entries: true
    |}];
  write coverage_stale;
  if exists coverage_link then Unix.unlink (Path.Build.to_string coverage_link);
  Unix.symlink "live" (Path.Build.to_string coverage_link);
  Memo.reset Memo.Invalidation.empty;
  printfn
    "recreation after unchanged inventories remains unobserved: %b"
    (run (lookup_for_build coverage_live) == coverage_rule
     && exists coverage_stale
     && exists coverage_link
     && exists coverage_live
     && exists coverage_action_file);
  [%expect
    {|
    recreation after unchanged inventories remains unobserved: true
    |}];
  (* No point lookup prepares this directory: the selector owns its cleanup. *)
  let selector = file_selector ~dir:selector_context.build_dir "live" in
  let selector_read =
    let incremental = Memo.is_incremental () in
    Memo.set_incremental false;
    Exn.protect
      ~f:(fun () -> Load_rules.load_file_selector selector)
      ~finally:(fun () -> Memo.set_incremental incremental)
  in
  let selector_consumer_runs = ref 0 in
  let wanted = Memo.Var.create true ~name:"selector-consumer-wanted" in
  let consumer =
    Memo.create
      "selector-cleanup-consumer"
      ~input:(module Unit)
      (fun () ->
         incr selector_consumer_runs;
         let open Memo.O in
         let* wanted = Memo.Var.read wanted in
         if not wanted
         then Memo.return selector_rule
         else
           let+ loaded = selector_read in
           match loaded with
           | Load_rules.Loaded.Build { rules_here; _ } ->
             Option.value_exn
               (Path.Build.Map.find rules_here.by_file_targets selector_live)
           | _ -> Code_error.raise "Expected selector build view" [])
  in
  Path.mkdir_p (Path.build selector_context.build_dir);
  List.iter [ selector_live; selector_stale; selector_pending ] ~f:write;
  Unix.symlink "live" (Path.Build.to_string selector_link);
  assert (!selector_runs = 0 && !selector_consumer_runs = 0);
  let clean () =
    (not (exists selector_stale))
    && (not (exists selector_link))
    && exists selector_live
    && exists selector_pending
    && !selector_pending_runs = 0
  in
  printfn
    "selector-only directory selects and cleans without unrelated production: %b"
    (run (Memo.exec consumer ()) == selector_rule && clean ());
  List.iter [ 1; 2 ] ~f:(fun epoch ->
    write selector_stale;
    if exists selector_link then Unix.unlink (Path.Build.to_string selector_link);
    Unix.symlink "live" (Path.Build.to_string selector_link);
    Memo.reset Memo.Invalidation.empty;
    printfn
      "selector epoch %d preserves unobserved recreated entries: %b"
      epoch
      (run (Memo.exec consumer ()) == selector_rule
       && exists selector_stale
       && exists selector_link
       && exists selector_live
       && exists selector_pending
       && !selector_pending_runs = 0
       && !selector_runs = 1
       && !selector_consumer_runs = 1));
  let changed_rule =
    Rule.set_action
      selector_rule
      (Action_builder.return (Action.Full.make (Action.Echo [ "selector-change" ])))
  in
  Memo.reset (Memo.Var.set selector_rule_input changed_rule);
  let changed = run (Memo.exec consumer ()) in
  printfn
    "selector observes changed action with the same rule ID: %b"
    (changed == changed_rule && Rule.equal changed selector_rule && !selector_runs = 2);
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Var.set wanted false)
       (Memo.Var.set selector_rule_input selector_rule));
  printfn
    "changed earlier dependency leaves the obsolete selector unforced: %b"
    (run (Memo.exec consumer ()) == selector_rule && !selector_runs = 2);
  write selector_stale;
  Unix.symlink "live" (Path.Build.to_string selector_link);
  Memo.reset (Memo.Var.set wanted true);
  printfn
    "selector resumes after a skipped epoch with fresh cleanup: %b"
    (run (Memo.exec consumer ()) == selector_rule && clean () && !selector_runs = 3);
  [%expect
    {|
    selector-only directory selects and cleans without unrelated production: true
    selector epoch 1 preserves unobserved recreated entries: true
    selector epoch 2 preserves unobserved recreated entries: true
    selector observes changed action with the same rule ID: true
    changed earlier dependency leaves the obsolete selector unforced: true
    selector resumes after a skipped epoch with fresh cleanup: true
    |}];
  write selector_stale;
  Unix.symlink "live" (Path.Build.to_string selector_link);
  let batch_read =
    Load_rules.load_file_selector (file_selector ~dir:selector_context.build_dir "li?e")
  in
  assert (exists selector_stale && exists selector_link);
  let incremental = Memo.is_incremental () in
  Memo.reset (Memo.Invalidation.invalidate_caches ~reason:Test);
  Memo.set_incremental false;
  Exn.protect
    ~f:(fun () ->
      let loaded = run batch_read in
      assert (loaded == run batch_read && clean ());
      match loaded with
      | Load_rules.Loaded.Build { rules_here; _ } ->
        assert (
          Option.value_exn (Path.Build.Map.find rules_here.by_file_targets selector_live)
          == selector_rule)
      | _ -> Code_error.raise "Expected batch selector build view" [])
    ~finally:(fun () ->
      Memo.set_incremental incremental;
      Memo.reset (Memo.Invalidation.invalidate_caches ~reason:Test));
  (* The initial snapshot cannot retain a name that does not exist yet. A later
     ownership withdrawal must scan again before restoring the changed producer. *)
  if exists cleanup_live then Fpath.unlink_exn (Path.Build.to_string cleanup_live);
  Memo.reset
    (Memo.Invalidation.combine
       (Memo.Var.set cleanup_enabled true)
       (Memo.Var.set cleanup_rule_input cleanup_rule));
  assert (run (Memo.exec cleanup_consumer ()) == cleanup_rule);
  assert (not (exists cleanup_live));
  let previous_generator_runs = !cleanup_generator_runs in
  let previous_producer_runs = !cleanup_producer_runs in
  write cleanup_live;
  Memo.reset (Memo.Var.set cleanup_enabled false);
  assert (
    try
      ignore (run (Memo.exec cleanup_consumer ()) : Rule.t);
      false
    with
    | User_error.E message ->
      String.starts_with (User_message.to_string message) ~prefix:"No rule found for");
  assert (not (exists cleanup_live));
  assert (!cleanup_generator_runs = previous_generator_runs);
  assert (!cleanup_producer_runs = previous_producer_runs + 1);
  (* Point and selector receipts start before another request changes the
     frontier. Replaying them must retain that frontier in the same inventory. *)
  let lookup_receipts () =
    let point = run (lookup_for_build selector_live) in
    let selected = run (Load_rules.load_file_selector selector) in
    match selected with
    | Load_rules.Loaded.Build { rules_here; _ } ->
      point == selector_rule
      && Option.value_exn (Path.Build.Map.find rules_here.by_file_targets selector_live)
         == selector_rule
    | _ -> false
  in
  let refresh () = run (lookup_for_build selector_refresh) == selector_refresh_rule in
  let reads () = Counter.read Stdune.Metrics.Directory_read.count in
  let initial_receipts = lookup_receipts () in
  let initial_refresh = refresh () in
  let empty_pending =
    Option.is_none (run (Load_rules.get_rule (Path.build selector_pending)))
  in
  let both =
    File_selector.of_predicate_lang
      ~dir:(Path.build selector_context.build_dir)
      (Predicate_lang.Glob.of_string_list [ "live"; "pending" ])
  in
  ignore (run (Load_rules.load_file_selector both) : Load_rules.Loaded.t);
  let producer_runs = !selector_runs in
  let pending_runs = !selector_pending_runs in
  printfn
    "receipt setup refines the pending sibling independently: %b"
    (initial_receipts && initial_refresh && empty_pending && not (exists selector_pending));
  Memo.reset Memo.Invalidation.empty;
  let before = reads () in
  let same_generation = lookup_receipts () in
  printfn
    "both receipts replay the changed frontier without a fresh inventory: %b"
    (same_generation && reads () = before && !selector_runs = producer_runs);
  (* Invalidate an already-started, disjoint producer. The unchanged point
     and selector payloads must advance their receipts across fresh inventories. *)
  List.iter [ 1; 2 ] ~f:(fun epoch ->
    Memo.reset (Memo.Var.set selector_refresh_input epoch);
    let before = reads () in
    let selected = lookup_receipts () in
    let scanned = reads () > before in
    let refreshed = refresh () in
    printfn
      "receipt transfer through fresh generation %d keeps producers cached: %b"
      epoch
      (selected
       && scanned
       && !selector_runs = producer_runs
       && !selector_pending_runs = pending_runs
       && refreshed));
  Memo.reset (Memo.Var.set selector_refresh_input 3);
  ignore (refresh () : bool);
  write selector_pending;
  Memo.reset (Memo.Var.set selector_refresh_input 4);
  let before = reads () in
  let refreshed = refresh () in
  printfn
    "skipped receipts leave the new inventory's sibling pending: %b"
    (refreshed && reads () > before && exists selector_pending);
  let before = reads () in
  let selected = lookup_receipts () in
  let pending = exists selector_pending in
  let absent = Option.is_none (run (Load_rules.get_rule (Path.build selector_pending))) in
  printfn
    "skipped point and selector receipts refine only their own requests: %b"
    (selected
     && pending
     && absent
     && (not (exists selector_pending))
     && reads () = before
     && !selector_runs = producer_runs
     && !selector_pending_runs = pending_runs
     && !selector_refresh_runs = 5);
  [%expect
    {|
    receipt setup refines the pending sibling independently: true
    both receipts replay the changed frontier without a fresh inventory: true
    receipt transfer through fresh generation 1 keeps producers cached: true
    receipt transfer through fresh generation 2 keeps producers cached: true
    skipped receipts leave the new inventory's sibling pending: true
    skipped point and selector receipts refine only their own requests: true
    |}];
  (* Preparation metadata may change without changing the requested rule. *)
  assert (run (Memo.exec metadata_consumer ()) == metadata_rule);
  let consumer_runs = !metadata_consumer_runs in
  let sources =
    Filename.Array.Set.of_sorted_list [ Filename.of_string_exn "unrelated-source" ]
  in
  Memo.reset (Memo.Var.set cleanup_sources sources);
  assert (run (Memo.exec metadata_consumer ()) == metadata_rule);
  assert (run (Memo.exec metadata_consumer ()) == metadata_rule);
  assert (!metadata_consumer_runs = consumer_runs);
  Memo.reset (Memo.Var.set cleanup_sources Filename.Array.Set.empty);
  assert (run (Memo.exec metadata_consumer ()) == metadata_rule);
  assert (run (Memo.exec metadata_consumer ()) == metadata_rule);
  assert (!metadata_consumer_runs = consumer_runs);
  let validation_runs = ref 0 in
  let validate =
    Memo.exec
      (Memo.create
         "cleanup-metadata-validation-consumer"
         ~input:(module Unit)
         (fun () ->
            incr validation_runs;
            Load_rules.is_target (Path.build metadata_live)))
      ()
  in
  assert (run validate = Load_rules.Yes Load_rules.File);
  let runs = !validation_runs in
  Memo.reset (Memo.Var.set cleanup_sources sources);
  assert (run validate = Load_rules.Yes Load_rules.File);
  assert (run validate = Load_rules.Yes Load_rules.File);
  assert (!validation_runs = runs);
  Memo.reset (Memo.Var.set cleanup_sources Filename.Array.Set.empty);
  assert (run validate = Load_rules.Yes Load_rules.File);
  assert (run validate = Load_rules.Yes Load_rules.File);
  assert (!validation_runs = runs);
  Memo.reset (Memo.Var.set cleanup_context_enabled false);
  assert (run validate = Load_rules.No);
  assert (run validate = Load_rules.No);
  Memo.reset (Memo.Var.set cleanup_context_enabled true);
  assert (run validate = Load_rules.Yes Load_rules.File);
  assert (run validate = Load_rules.Yes Load_rules.File)
;;

let%expect_test "prefixed producers share pure maps and track their inputs" =
  let dir = path "default/prefixed-producers" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let phase = Memo.Var.create 0 ~name:"prefixed-producer-phase" in
  let outer_runs = ref 0 in
  let producer_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           incr outer_runs;
           Rules.narrow
             (Target_mask.files [ a; b; c ])
             (fun () ->
                let open Memo.O in
                let* phase = Memo.Var.read phase in
                incr producer_runs;
                if phase = 2
                then Code_error.raise "Failed prefixed producer" []
                else (
                  let targets = if phase = 0 then [ a; b ] else [ a; c ] in
                  Rules.Produce.rule (file_rule targets))))))
  in
  let prefix tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.prefix_rules (Action_builder.return ()) ~f:(fun () -> Rules.produce tree)))
  in
  let prefixed = prefix tree in
  let second_view = prefix tree in
  let load tree target = Rules.load tree (Target_mask.path target) in
  let only_rule rules =
    match rules_in ~dir rules with
    | [ rule ] -> rule
    | _ -> Code_error.raise "Expected one prefixed rule" []
  in
  let consumer =
    Memo.create "prefixed-producer-consumer" ~input:(module Path.Build) (load prefixed)
  in
  let first = run (Memo.exec consumer a) in
  (* This consumer has no direct dependency on [phase]. It must still observe
     changes after a reset, even though the first lookup completed the producer. *)
  let second = run (Memo.exec consumer b) in
  printfn "siblings share a mapped rule: %b" (only_rule first == only_rule second);
  printfn
    "union keeps one rule: %d"
    (List.length (rules_in ~dir (Rules.union first second)));
  let parallel_a, parallel_b =
    run (Memo.fork_and_join (fun () -> load second_view a) (fun () -> load second_view b))
  in
  printfn
    "concurrent siblings share a mapped rule: %b"
    (only_rule parallel_a == only_rule parallel_b);
  let original = run (load tree a) in
  printfn
    "mapped and original views share rule identity: %b"
    (Rule.equal (only_rule first) (only_rule original));
  printfn "producer runs: outer %d; inner %d" !outer_runs !producer_runs;
  [%expect
    {|
    siblings share a mapped rule: true
    union keeps one rule: 1
    concurrent siblings share a mapped rule: true
    mapped and original views share rule identity: true
    producer runs: outer 1; inner 1
    |}];
  Memo.reset (Memo.Var.set phase 1);
  let removed = run (Memo.exec consumer b) in
  printfn "removed sibling has no rule: %b" (List.is_empty (rules_in ~dir removed));
  let changed = run (Memo.exec consumer a) in
  print_files "changed outputs" ~dir changed;
  printfn "changed rule is new: %b" (only_rule changed != only_rule first);
  printfn "producer runs: outer %d; inner %d" !outer_runs !producer_runs;
  [%expect
    {|
    removed sibling has no rule: true
    changed outputs: a, c
    changed rule is new: true
    producer runs: outer 1; inner 2
    |}];
  Memo.reset Memo.Invalidation.empty;
  let unchanged = run (load prefixed c) in
  printfn
    "unchanged input keeps mapped identity: %b"
    (only_rule unchanged == only_rule changed);
  printfn "producer runs: outer %d; inner %d" !outer_runs !producer_runs;
  [%expect
    {|
    unchanged input keeps mapped identity: true
    producer runs: outer 1; inner 2
    |}];
  Memo.reset (Memo.Var.set phase 2);
  expect_code_error "producer failure is not hidden by the map cache" (fun () ->
    ignore (run (Memo.exec consumer a) : Rules.t));
  printfn "producer runs: outer %d; inner %d" !outer_runs !producer_runs;
  [%expect
    {|
    producer failure is not hidden by the map cache: code error
    producer runs: outer 1; inner 3
    |}];
  Memo.reset (Memo.Var.set phase 0);
  let recovered = run (Memo.exec consumer a) in
  print_files "recovered outputs" ~dir recovered;
  printfn "producer runs: outer %d; inner %d" !outer_runs !producer_runs;
  [%expect
    {|
    recovered outputs: a, b
    producer runs: outer 1; inner 4
    |}]
;;

let%expect_test "multiple enclosing subtrees preserve covered masks" =
  let dir = path "default/multiple-subtrees" in
  let obj = Path.Build.relative dir ".obj" in
  let melange = Path.Build.relative dir ".melange" in
  let compat = Path.Build.relative dir "wrapped_compat" in
  let parent =
    List.fold_left [ obj; melange; compat ] ~init:Target_mask.empty ~f:(fun mask dir ->
      Target_mask.union mask (Target_mask.subtree dir))
  in
  let alias dir name = Alias.make (Alias.Name.of_string name) ~dir in
  let extensions = Filename.Extension.Set.singleton Filename.Extension.ml in
  List.iter
    [ ( "files"
      , Target_mask.files
          [ Path.Build.relative obj "x.cmo"
          ; Path.Build.relative melange "x.cmj"
          ; Path.Build.relative compat "x.ml"
          ] )
    ; ( "directories"
      , Target_mask.directories
          [ Path.Build.relative obj "byte"; Path.Build.relative melange "js" ] )
    ; "aliases", Target_mask.aliases [ alias obj "check"; alias melange "check" ]
    ; "file at root", Target_mask.files [ obj ]
    ; "directory at root", Target_mask.directories [ melange ]
    ; ( "child subtrees"
      , Target_mask.union
          (Target_mask.subtree (Path.Build.relative obj "byte"))
          (Target_mask.subtree (Path.Build.relative melange "js")) )
    ; ( "direct and recursive selectors"
      , Target_mask.union
          (Target_mask.files_in_directory obj)
          (Target_mask.file_extensions_in_subtree ~dir:melange extensions) )
    ; ( "mixed kinds"
      , Target_mask.union
          (Target_mask.path (Path.Build.relative obj "x.cmo"))
          (Target_mask.aliases [ alias compat "check" ]) )
    ]
    ~f:(fun (label, child) ->
      printfn
        "%s: intersection %b; union %b"
        label
        (Target_mask.inter parent child == child
         && Target_mask.inter child parent == child)
        (Target_mask.union parent child == parent
         && Target_mask.union child parent == parent));
  let inside = Path.Build.relative obj "x.ml" in
  let outside = Path.Build.relative dir "outside/x.ml" in
  let child = Target_mask.files [ inside; outside ] in
  let intersection = Target_mask.inter parent child in
  printfn
    "uncovered child narrowed: %b"
    (intersection != child
     && Target_mask.mem_file intersection inside
     && not (Target_mask.mem_file intersection outside));
  let union = Target_mask.union parent child in
  printfn
    "uncovered union retained: %b"
    (union != parent && Target_mask.mem_file union outside);
  let filtered =
    Target_mask.union
      (Target_mask.file_extensions_in_subtree ~dir:obj extensions)
      (Target_mask.subtree melange)
  in
  let excluded = Path.Build.relative obj "x.txt" in
  let unfiltered = Path.Build.relative melange "x.txt" in
  let child = Target_mask.files [ inside; excluded; unfiltered ] in
  let intersection = Target_mask.inter filtered child in
  printfn
    "filtered roots remain filtered: %b"
    (intersection != child
     && Target_mask.mem_file intersection inside
     && (not (Target_mask.mem_file intersection excluded))
     && Target_mask.mem_file intersection unfiltered);
  let at_roots = [ alias dir ".obj"; alias dir ".melange" ] in
  let child = Target_mask.aliases at_roots in
  printfn
    "aliases at subtree roots excluded: %b"
    (Target_mask.inter parent child |> Target_mask.is_empty);
  let union = Target_mask.union parent child in
  printfn
    "root aliases retained by union: %b"
    (union != parent && List.for_all at_roots ~f:(Target_mask.mem_alias union));
  [%expect
    {|
    files: intersection true; union true
    directories: intersection true; union true
    aliases: intersection true; union true
    file at root: intersection true; union true
    directory at root: intersection true; union true
    child subtrees: intersection true; union true
    direct and recursive selectors: intersection true; union true
    mixed kinds: intersection true; union true
    uncovered child narrowed: true
    uncovered union retained: true
    filtered roots remain filtered: true
    aliases at subtree roots excluded: true
    root aliases retained by union: true
    |}]
;;

let%expect_test "grouped point masks match individual paths" =
  let dir = path "default/grouped-points" in
  let relative = Path.Build.relative dir in
  let samples =
    [ dir
    ; relative "a"
    ; relative "b"
    ; relative "missing"
    ; relative "a/nested"
    ; path "default/grouped-points-other/a"
    ]
  in
  List.iter
    [ "empty", []; "singleton", [ "a" ]; "multiple", [ "a"; "b" ] ]
    ~f:(fun (label, names) ->
      let targets = List.map names ~f:relative in
      let names = List.map targets ~f:Path.Build.basename |> Filename.Set.of_list in
      let actual = Target_mask.paths ~dir names in
      let expected =
        List.fold_left targets ~init:Target_mask.empty ~f:(fun mask target ->
          Target_mask.union mask (Target_mask.path target))
      in
      let equivalent =
        List.for_all samples ~f:(fun path ->
          let alias =
            Alias.make
              (Alias.Name.of_string (Path.Build.basename path |> Filename.to_string))
              ~dir:(Path.Build.parent_exn path)
          in
          Bool.equal
            (Target_mask.mem_file actual path)
            (Target_mask.mem_file expected path)
          && Bool.equal
               (Target_mask.mem_directory actual path)
               (Target_mask.mem_directory expected path)
          && Bool.equal
               (Target_mask.mem_alias actual alias)
               (Target_mask.mem_alias expected alias)
          && Bool.equal
               (Target_mask.intersects actual (Target_mask.path path))
               (Target_mask.intersects expected (Target_mask.path path)))
      in
      printfn "%s: %b" label equivalent);
  printfn
    "canonical empty: %b"
    (Target_mask.paths ~dir Filename.Set.empty == Target_mask.empty);
  [%expect
    {|
    empty: true
    singleton: true
    multiple: true
    canonical empty: true
    |}]
;;

let%expect_test "shared path regions remain distinct from mixed target kinds" =
  let dir = path "default/shared-regions" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let alias name = Alias.make (Alias.Name.of_string name) ~dir in
  let shared_ab = Target_mask.union (Target_mask.path a) (Target_mask.path b) in
  let shared_bc = Target_mask.union (Target_mask.path b) (Target_mask.path c) in
  let mixed =
    Target_mask.union (Target_mask.files [ a ]) (Target_mask.directories [ c ])
  in
  let shared_with_alias =
    Target_mask.union shared_ab (Target_mask.aliases [ alias "c" ])
  in
  let samples = [ "a", a; "b", b; "c", c ] in
  let print label mask =
    let selected mem =
      List.filter_map samples ~f:(fun (name, path) -> Option.some_if (mem mask path) name)
      |> String.concat ~sep:", "
    in
    printfn
      "%s: files [%s]; directories [%s]; aliases [%s]"
      label
      (selected Target_mask.mem_file)
      (selected Target_mask.mem_directory)
      (selected (fun mask path ->
         Target_mask.mem_alias
           mask
           (alias (Path.Build.basename path |> Filename.to_string))))
  in
  print "shared union" (Target_mask.union shared_ab shared_bc);
  print "shared intersection" (Target_mask.inter shared_ab shared_bc);
  print "mixed union" (Target_mask.union shared_ab mixed);
  print "mixed intersection" (Target_mask.inter shared_ab mixed);
  print "reverse mixed intersection" (Target_mask.inter mixed shared_ab);
  print "union preserves aliases" (Target_mask.union shared_with_alias shared_bc);
  print "intersection excludes aliases" (Target_mask.inter shared_with_alias shared_bc);
  [%expect
    {|
    shared union: files [a, b, c]; directories [a, b, c]; aliases []
    shared intersection: files [b]; directories [b]; aliases []
    mixed union: files [a, b]; directories [a, b, c]; aliases []
    mixed intersection: files [a]; directories []; aliases []
    reverse mixed intersection: files [a]; directories []; aliases []
    union preserves aliases: files [a, b, c]; directories [a, b, c]; aliases [c]
    intersection excludes aliases: files [b]; directories [b]; aliases []
    |}]
;;

let%expect_test "direct frontiers preserve transitive outputs and aliases" =
  let dir = path "default/direct-frontiers" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let d = Path.Build.relative dir "d" in
  let unused = Path.Build.relative dir "unused" in
  let alias name = Alias.make (Alias.Name.of_string name) ~dir in
  let ready =
    Rules.of_rules
      [ file_rule [ a; b ]; file_rule [ b; c ]; file_rule [ c; d ]; file_rule [ unused ] ]
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce ready in
         Memo.parallel_iter
           [ alias "a"; alias "unused" ]
           ~f:(fun alias -> Rules.Produce.Alias.add_deps alias (Action_builder.return ()))))
  in
  let load mask = run (Rules.load tree mask) in
  let print_aliases label rules =
    let { Rules.Dir_rules.aliases; _ } =
      Rules.find rules (Path.build dir) |> Rules.Dir_rules.consume
    in
    printfn
      "%s: [%s]"
      label
      (Alias.Name.Map.keys aliases
       |> List.map ~f:Alias.Name.to_string
       |> String.concat ~sep:", ")
  in
  let selected = load (Target_mask.path a) in
  print_files "transitive files" ~dir selected;
  print_aliases "file lookup aliases" selected;
  let selected =
    load (Target_mask.union (Target_mask.path a) (Target_mask.aliases [ alias "unused" ]))
  in
  print_files "mixed lookup files" ~dir selected;
  print_aliases "mixed lookup aliases" selected;
  let selected = load (Target_mask.aliases [ alias "a" ]) in
  printfn "alias lookup rule count: %d" (List.length (rules_in ~dir selected));
  print_aliases "alias lookup aliases" selected;
  print_files "independent lookup" ~dir (load (Target_mask.path unused));
  let whole = load (Target_mask.subtree dir) in
  printfn "whole selection reuses the direct tree: %b" (whole == tree);
  print_files "whole files" ~dir whole;
  print_aliases "whole aliases" whole;
  let directory_rule target =
    rule
      (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton target))
  in
  List.iter
    [ "file", file_rule [ b ]
    ; "directory", directory_rule b
    ; "requested-directory", directory_rule a
    ]
    ~f:(fun (owner, blocker) ->
      List.iter
        [ "point", Target_mask.path a
        ; "file-only", Target_mask.files [ a ]
        ; ( "with-alias"
          , Target_mask.union
              (Target_mask.path a)
              (Target_mask.aliases [ alias "absent" ]) )
        ]
        ~f:(fun (request, mask) ->
          let expected = [ file_rule [ a; b ]; blocker ] in
          let forced = ref [] in
          let entries =
            List.mapi
              (expected @ [ file_rule [ unused ] ])
              ~f:(fun index rule -> index, rule)
          in
          let tree =
            run
              (Rules.collect_unit (fun () ->
                 Memo.parallel_iter entries ~f:(fun (index, rule) ->
                   Rules.narrow (Target_mask.of_targets rule.Rule.targets) (fun () ->
                     forced := index :: !forced;
                     Rules.Produce.rule rule))))
          in
          let complete =
            List.for_all
              [ mask; mask; Target_mask.union mask (Target_mask.path b) ]
              ~f:(fun mask ->
                let loaded = run (Rules.load_with_pending tree mask) in
                let actual = rules_in ~dir loaded.selected in
                List.length actual = List.length expected
                && List.for_all expected ~f:(fun rule ->
                  List.mem actual rule ~equal:( == ))
                && Rules.Pending.mem_file loaded.pending unused
                && List.equal
                     Int.equal
                     (List.rev !forced)
                     (List.mapi expected ~f:(fun index _ -> index)))
          in
          printfn
            "%s owner, %s request: complete ordered closure %b"
            owner
            request
            complete));
  [%expect
    {|
    transitive files: a, b, b, c, c, d
    file lookup aliases: []
    mixed lookup files: a, b, b, c, c, d
    mixed lookup aliases: [unused]
    alias lookup rule count: 0
    alias lookup aliases: [a]
    independent lookup: unused
    whole selection reuses the direct tree: true
    whole files: a, b, b, c, c, d, unused
    whole aliases: [a, unused]
    file owner, point request: complete ordered closure true
    file owner, file-only request: complete ordered closure true
    file owner, with-alias request: complete ordered closure true
    directory owner, point request: complete ordered closure true
    directory owner, file-only request: complete ordered closure true
    directory owner, with-alias request: complete ordered closure true
    requested-directory owner, point request: complete ordered closure true
    requested-directory owner, file-only request: complete ordered closure true
    requested-directory owner, with-alias request: complete ordered closure true
    |}]
;;

let%expect_test "direct frontiers remain local to independent Memo consumers" =
  let dir = path "default/direct-frontier-deps" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let independent = Path.Build.relative dir "independent" in
  let changed = Memo.Var.create false ~name:"direct-frontier-input" in
  let producer_runs = ref 0 in
  let consumer_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ a; b; c; independent ])
           (fun () ->
              let open Memo.O in
              let* changed = Memo.Var.read changed in
              incr producer_runs;
              let* () =
                Rules.Produce.rule (file_rule [ a; (if changed then c else b) ])
              in
              Rules.Produce.rule (file_rule [ independent ]))))
  in
  let consumer =
    Memo.create
      "direct-frontier-consumer"
      ~input:(module Path.Build)
      (fun target ->
         incr consumer_runs;
         Rules.load tree (Target_mask.path target))
  in
  print_files "initial outputs" ~dir (run (Memo.exec consumer a));
  print_files "independent outputs" ~dir (run (Memo.exec consumer independent));
  printfn "runs: producer %d; consumers %d" !producer_runs !consumer_runs;
  [%expect
    {|
    initial outputs: a, b
    independent outputs: independent
    runs: producer 1; consumers 2
    |}];
  Memo.reset (Memo.Var.set changed true);
  print_files "changed outputs" ~dir (run (Memo.exec consumer a));
  print_files "restored independent outputs" ~dir (run (Memo.exec consumer independent));
  printfn "runs: producer %d; consumers %d" !producer_runs !consumer_runs;
  [%expect
    {|
    changed outputs: a, c
    restored independent outputs: independent
    runs: producer 2; consumers 4
    |}]
;;

let%expect_test "revealed chunks share IDs and retain unselected metadata" =
  let dir = path "default/revealed-chunks" in
  let other = Path.Build.relative dir "other" in
  let alias_dir = Path.Build.relative dir "alias-only" in
  let x = Path.Build.relative dir "x" in
  let spare = Path.Build.relative dir "spare" in
  let extra = Path.Build.relative dir "extra" in
  let generated = Path.Build.relative dir "generated" in
  let hidden = Path.Build.relative dir "hidden" in
  let alias = Alias.make (Alias.Name.of_string "shared") ~dir in
  let hidden_runs = ref 0 in
  let shared =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.produce
             (Rules.of_rules
                [ file_rule [ x ]
                ; file_rule [ spare ]
                ; file_rule [ Path.Build.relative other "y" ]
                ; rule
                    (Targets.create
                       ~files:Path.Build.Set.empty
                       ~dirs:(Path.Build.Set.singleton generated))
                ])
         in
         let* () = Rules.Produce.Alias.add_deps alias (Action_builder.return ()) in
         let* () =
           Rules.Produce.Alias.add_deps
             (Alias.make (Alias.Name.of_string "only") ~dir:alias_dir)
             (Action_builder.return ())
         in
         Rules.narrow (Target_mask.files [ hidden ]) (fun () ->
           incr hidden_runs;
           Code_error.raise "hidden producer" [])))
  in
  let direct = Rules.of_rules [ file_rule [ extra ] ] in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce direct in
         let produce_shared () = Rules.produce shared in
         let* () = Rules.narrow (Target_mask.subtree dir) produce_shared in
         Rules.narrow (Target_mask.subtree dir) produce_shared))
  in
  let print_names label revealed ~dir =
    let files, dirs = Rules.Revealed.target_names revealed ~dir in
    let names set =
      Filename.Set.to_list_map set ~f:Filename.to_string |> String.concat ~sep:", "
    in
    printfn "%s: files [%s]; directories [%s]" label (names files) (names dirs)
  in
  print_names "initial" (Rules.Revealed.of_rules tree) ~dir;
  let loaded = run (Rules.load_with_pending tree (Target_mask.path x)) in
  print_files "selected" ~dir loaded.selected;
  print_names "root" loaded.revealed ~dir;
  print_names "other" loaded.revealed ~dir:other;
  print_names "alias-only" loaded.revealed ~dir:alias_dir;
  printfn
    "directories include unselected rules and aliases: %b"
    (Path.Build.Set.equal
       (Rules.Revealed.directories loaded.revealed)
       (Path.Build.Set.of_list [ dir; other; alias_dir ]));
  printfn
    "unselected directory target retained: %b"
    (Path.Build.Map.mem (Rules.Revealed.directory_targets loaded.revealed) generated);
  let { Rules.Dir_rules.rules; aliases } =
    Rules.Revealed.find loaded.revealed ~dir |> Rules.Dir_rules.consume
  in
  printfn
    "shared rules retain identity and order: %b"
    (List.equal Rule.equal rules (rules_in ~dir (Rules.union shared direct)));
  let alias_spec = Alias.Name.Map.find_exn aliases (Alias.name alias) in
  printfn "shared alias expansions: %d" (Appendable_list.length alias_spec.expansions);
  printfn
    "missing directory empty: %b"
    (Rules.Revealed.find loaded.revealed ~dir:(Path.Build.relative dir "missing")
     |> Rules.Dir_rules.is_empty);
  printfn "metadata readers forced hidden producer: %d" !hidden_runs;
  expect_code_error "hidden request" (fun () ->
    ignore (run (Rules.load tree (Target_mask.path hidden)) : Rules.t));
  printfn "hidden producer runs: %d" !hidden_runs;
  [%expect
    {|
    initial: files [extra]; directories []
    selected: x
    root: files [extra, spare, x]; directories [generated]
    other: files [y]; directories []
    alias-only: files []; directories []
    directories include unselected rules and aliases: true
    unselected directory target retained: true
    shared rules retain identity and order: true
    shared alias expansions: 1
    missing directory empty: true
    metadata readers forced hidden producer: 0
    hidden request: code error
    hidden producer runs: 1
    |}]
;;

let%expect_test "revealed directory locations follow original rule collection IDs" =
  let dir = path "default/revealed-directory-owners" in
  let target = Path.Build.relative dir "generated" in
  let make filename =
    Rule.make
      ~info:(Rule.Info.From_dune_file (Loc.of_pos (filename, 1, 0, 1)))
      ~targets:
        (Targets.create
           ~files:Path.Build.Set.empty
           ~dirs:(Path.Build.Set.singleton target))
      (Action_builder.return (Action.Full.make Action.empty))
  in
  let later_rule = make "later" in
  let first_rule = make "first" in
  let first = Rules.of_rules [ first_rule ] in
  let later = Rules.of_rules [ later_rule ] in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce later)
         in
         Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce first)))
  in
  let loaded = run (Rules.load_directory_with_pending tree target) in
  let loc =
    Path.Build.Map.find_exn (Rules.Revealed.directory_targets loaded.revealed) target
  in
  printfn "owner: %s" (Loc.start loc).pos_fname;
  printfn
    "same owner as merged rules: %b"
    (Loc.equal
       loc
       (Path.Build.Map.find_exn
          (Rules.directory_targets (Rules.union later first))
          target));
  let { Rules.Dir_rules.rules; _ } =
    Rules.Revealed.find loaded.revealed ~dir |> Rules.Dir_rules.consume
  in
  printfn
    "diagnostic rule order: %s"
    (List.map rules ~f:(fun rule -> (Loc.start (Rule.loc rule)).pos_fname)
     |> String.concat ~sep:", ");
  [%expect
    {|
    owner: first
    same owner as merged rules: true
    diagnostic rule order: first, later
    |}]
;;

let%expect_test "basename path membership matches file and directory queries" =
  let dir = path "default/prepared-membership" in
  let other = path "default/prepared-membership-other" in
  let relative = Path.Build.relative dir in
  let x = relative "x.ml" in
  let generated = relative "generated" in
  let extension_root = relative "root.ml" in
  let extensions = Filename.Extension.Set.singleton Filename.Extension.ml in
  let files = Target_mask.files [ x; Path.Build.relative other "x.ml" ] in
  let directories = Target_mask.directories [ generated ] in
  let samples =
    [ path "default"
    ; dir
    ; x
    ; generated
    ; relative "missing.txt"
    ; relative ".hidden.ml"
    ; relative "child/x.ml"
    ; extension_root
    ; Path.Build.relative extension_root "x.ml"
    ; Path.Build.relative extension_root "x.txt"
    ; relative "root.txt"
    ; relative "root.txt/x.ml"
    ; other
    ; Path.Build.relative other "x.ml"
    ; path "unrelated/x.ml"
    ]
  in
  List.iter
    [ "empty", Target_mask.empty
    ; "all", Target_mask.all
    ; "files in multiple directories", files
    ; "directory", directories
    ; "point", Target_mask.path x
    ; "mixed kinds", Target_mask.union files directories
    ; "direct files", Target_mask.files_in_directory dir
    ; "direct directories", Target_mask.directories_in_directory dir
    ; "direct extensions", Target_mask.file_extensions ~dir extensions
    ; ( "glob"
      , Target_mask.files_matching
          ~dir
          (Predicate_lang.Glob.of_glob (Dune_lang.Glob.of_string_exn Loc.none "*.ml")) )
    ; "subtree", Target_mask.subtree dir
    ; ( "multiple subtrees"
      , Target_mask.union (Target_mask.subtree dir) (Target_mask.subtree other) )
    ; ( "recursive extensions including root"
      , Target_mask.file_extensions_in_subtree ~dir:extension_root extensions )
    ; ( "recursive extensions excluding root"
      , Target_mask.file_extensions_in_subtree ~dir:(relative "root.txt") extensions )
    ; ( "multiple recursive extension roots"
      , Target_mask.union
          (Target_mask.file_extensions_in_subtree
             ~dir
             (Filename.Extension.Set.singleton Filename.Extension.mli))
          (Target_mask.file_extensions_in_subtree ~dir:extension_root extensions) )
    ; ( "intersection"
      , Target_mask.inter (Target_mask.union files directories) (Target_mask.subtree dir)
      )
    ; ( "aliases excluded"
      , Target_mask.union
          (Target_mask.aliases [ Alias.make (Alias.Name.of_string "x.ml") ~dir ])
          (Target_mask.aliases_in_directory other) )
    ]
    ~f:(fun (label, mask) ->
      let pending = Rules.Pending.of_mask mask in
      let same =
        List.for_all samples ~f:(fun path ->
          let dir = Path.Build.parent_exn path in
          let name = Path.Build.basename path in
          let file = Target_mask.mem_file mask path in
          let directory = Target_mask.mem_directory mask path in
          let expected = file || directory in
          let point = Target_mask.path path in
          Bool.equal (Target_mask.mem_file_name mask ~dir name) file
          && Bool.equal (Target_mask.mem_directory_name mask ~dir name) directory
          && Bool.equal (Target_mask.mem_path mask ~dir name) expected
          && Bool.equal (Rules.Pending.mem_path pending ~dir name) expected
          && Bool.equal (Target_mask.intersects point mask) expected
          && Bool.equal (Target_mask.intersects mask point) expected)
      in
      printfn "%s: %b" label same);
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:other in
  let aliases = Target_mask.aliases [ alias ] in
  let point_and_alias = Target_mask.union (Target_mask.path x) aliases in
  printfn
    "point with alias retains alias intersections: %b"
    (Target_mask.intersects point_and_alias aliases
     && Target_mask.intersects aliases point_and_alias);
  [%expect
    {|
    empty: true
    all: true
    files in multiple directories: true
    directory: true
    point: true
    mixed kinds: true
    direct files: true
    direct directories: true
    direct extensions: true
    glob: true
    subtree: true
    multiple subtrees: true
    recursive extensions including root: true
    recursive extensions excluding root: true
    multiple recursive extension roots: true
    intersection: true
    aliases excluded: true
    point with alias retains alias intersections: true
    |}]
;;

let%expect_test "recursive intersections specialize either singleton side" =
  let dir = path "default/recursive-intersections" in
  let relative = Path.Build.relative dir in
  let other = path "default/recursive-intersections-other" in
  let extensions names =
    List.map names ~f:Filename.Extension.of_string_exn |> Filename.Extension.Set.of_list
  in
  let many =
    List.fold_left
      [ Target_mask.subtree (relative "a.ml")
      ; Target_mask.file_extensions_in_subtree
          ~dir:(relative "b.ml")
          (extensions [ ".mli" ])
      ; Target_mask.subtree other
      ]
      ~init:Target_mask.empty
      ~f:Target_mask.union
  in
  let samples =
    [ dir
    ; relative "a.ml"
    ; relative "a.ml/x.ml"
    ; relative "a.ml/x.mli"
    ; relative "a.ml/deep/x.ml"
    ; relative "b.ml"
    ; relative "b.ml/x.ml"
    ; relative "b.ml/x.mli"
    ; relative "missing.ml"
    ; other
    ; Path.Build.relative other "x.ml"
    ]
  in
  let kinds =
    [ Target_mask.mem_file
    ; Target_mask.mem_directory
    ; (fun mask path ->
        Target_mask.mem_alias
          mask
          (Alias.make
             (Alias.Name.of_string (Filename.to_string (Path.Build.basename path)))
             ~dir:(Path.Build.parent_exn path)))
    ]
  in
  List.iter
    [ "ancestor subtree", Target_mask.subtree dir
    ; ( "filtered ancestor"
      , Target_mask.file_extensions_in_subtree ~dir (extensions [ ".ml" ]) )
    ; ( "filtered descendant"
      , Target_mask.file_extensions_in_subtree
          ~dir:(relative "b.ml")
          (extensions [ ".ml"; ".mli" ]) )
    ; "disjoint subtree", Target_mask.subtree (relative "missing.ml")
    ]
    ~f:(fun (label, singleton) ->
      let forward = Target_mask.inter many singleton in
      let reverse = Target_mask.inter singleton many in
      let matches =
        List.for_all kinds ~f:(fun mem ->
          List.for_all samples ~f:(fun sample ->
            let expected = mem many sample && mem singleton sample in
            Bool.equal (mem forward sample) expected
            && Bool.equal (mem reverse sample) expected))
      in
      printfn "%s: %b" label matches);
  [%expect
    {|
    ancestor subtree: true
    filtered ancestor: true
    filtered descendant: true
    disjoint subtree: true
    |}]
;;

let%expect_test "pending alias coverage stays relative to each queried directory" =
  let dir = path "default/alias-retention" in
  let child = Path.Build.relative dir "child" in
  let sibling = path "default/alias-retention-other" in
  let alias dir = Alias.make (Alias.Name.of_string "all") ~dir in
  let direct = Target_mask.aliases [ alias dir; alias child; alias sibling ] in
  let recursive =
    Target_mask.union (Target_mask.subtree child) (Target_mask.subtree sibling)
  in
  let queries =
    [ Path.Build.root
    ; path "default"
    ; dir
    ; child
    ; Path.Build.relative sibling "child"
    ; child
    ; Path.Build.relative child "deep"
    ; sibling
    ; path "default/elsewhere"
    ]
  in
  List.iter
    [ "empty", Target_mask.empty
    ; "files only", Target_mask.files [ child ]
    ; "directories only", Target_mask.directories [ child ]
    ; "direct", direct
    ; "recursive", recursive
    ; "all", Target_mask.all
    ; "mixed", Target_mask.union direct recursive
    ; "restricted", Target_mask.inter direct (Target_mask.subtree dir)
    ]
    ~f:(fun (label, mask) ->
      let pending = Rules.Pending.of_mask mask in
      let equivalent =
        List.for_all
          (queries @ List.rev queries)
          ~f:(fun dir ->
            let expected = Target_mask.alias_directories mask ~dir in
            let actual = Rules.Pending.alias_directories pending ~dir in
            Dir_set.is_subset expected ~of_:actual
            && Dir_set.is_subset actual ~of_:expected)
      in
      printfn "%s: %b" label equivalent);
  [%expect
    {|
    empty: true
    files only: true
    directories only: true
    direct: true
    recursive: true
    all: true
    mixed: true
    restricted: true
    |}]
;;

let%expect_test "shared producer indexes preserve ordered unions and overlaps" =
  let exercise label left right =
    let dir = path ("default/shared-index-" ^ label) in
    let forced = ref [] in
    let leaves =
      Array.init 8 ~f:(fun index ->
        let target = Path.Build.relative dir (Int.to_string index) in
        run
          (Rules.collect_unit (fun () ->
             Rules.narrow (Target_mask.files [ target ]) (fun () ->
               forced := index :: !forced;
               Rules.Produce.rule (file_rule [ target ])))))
    in
    let collect indices =
      List.fold_left indices ~init:Rules.empty ~f:(fun tree index ->
        let tree = Rules.union tree leaves.(index) in
        ignore (Rules.targets tree : Target_mask.t);
        tree)
    in
    let tree = Rules.union (collect left) (collect right) in
    ignore (Rules.targets tree : Target_mask.t);
    let direct = Rules.of_rules [ file_rule [ Path.Build.relative dir "direct" ] ] in
    let tree = Rules.union (Rules.union tree direct) (Rules.union direct tree) in
    let loaded = run (Rules.load_with_pending tree (Target_mask.subtree dir)) in
    printfn
      "%s: [%s]; rules %d; producers %d"
      label
      (List.rev !forced |> List.map ~f:Int.to_string |> String.concat ~sep:", ")
      (List.length (rules_in ~dir loaded.selected))
      (List.length loaded.refinements)
  in
  exercise "ordered" [ 0; 1; 2; 3 ] [ 4; 5; 6; 7 ];
  exercise "reverse" [ 7; 6; 5; 4 ] [ 3; 2; 1; 0 ];
  exercise "interleaved" [ 0; 2; 4; 6 ] [ 1; 3; 5; 7 ];
  exercise "overlap" [ 0; 1; 2; 3; 4 ] [ 3; 4; 5; 6; 7 ];
  [%expect
    {|
    ordered: [0, 1, 2, 3, 4, 5, 6, 7]; rules 9; producers 8
    reverse: [0, 1, 2, 3, 4, 5, 6, 7]; rules 9; producers 8
    interleaved: [0, 1, 2, 3, 4, 5, 6, 7]; rules 9; producers 8
    overlap: [0, 1, 2, 3, 4, 5, 6, 7]; rules 9; producers 8
    |}]
;;

let%expect_test "shared and rebuilt indexes preserve multi-hop producer order" =
  let exercise shared =
    let label = if shared then "shared" else "rebuilt" in
    let dir = path ("default/index-closure-" ^ label) in
    let target = Path.Build.relative dir in
    let start = target "start" in
    let middle = target "middle" in
    let late = target "late" in
    let forced = ref [] in
    let produce name targets =
      forced := name :: !forced;
      Rule.make
        ~info:(Rule.Info.From_dune_file (Loc.of_pos (name, 1, 0, 1)))
        ~targets:(Targets.Files.create (Path.Build.Set.of_list targets))
        (Action_builder.return (Action.Full.make Action.empty))
      |> Rules.Produce.rule
    in
    let leaves =
      List.init 8 ~f:(fun index ->
        run
          (Rules.collect_unit (fun () ->
             match index with
             | 0 ->
               Rules.narrow
                 (Target_mask.files [ start; middle; late ])
                 (fun () ->
                    let open Memo.O in
                    let* () = produce "start" [ start; middle ] in
                    Rules.narrow (Target_mask.files [ late ]) (fun () ->
                      produce "late" [ late ]))
             | 1 ->
               let targets =
                 middle
                 :: late
                 :: List.init 6 ~f:(fun index -> target (Int.to_string (index + 2)))
               in
               Rules.narrow (Target_mask.files targets) (fun () ->
                 produce "middle" targets)
             | index ->
               let name = Int.to_string index in
               let file = target name in
               Rules.narrow (Target_mask.files [ file ]) (fun () -> produce name [ file ]))))
    in
    let tree =
      List.fold_left leaves ~init:Rules.empty ~f:(fun tree leaf ->
        let tree = Rules.union tree leaf in
        if shared then ignore (Rules.targets tree : Target_mask.t);
        tree)
    in
    let loaded = run (Rules.load tree (Target_mask.path start)) in
    printfn "%s producers: %s" label (List.rev !forced |> String.concat ~sep:", ");
    printfn
      "%s diagnostic order: %s"
      label
      (rules_in ~dir loaded
       |> List.map ~f:(fun rule -> (Loc.start (Rule.loc rule)).pos_fname)
       |> String.concat ~sep:", ")
  in
  exercise false;
  exercise true;
  [%expect
    {|
    rebuilt producers: start, middle, 2, 3, 4, 5, 6, 7, late
    rebuilt diagnostic order: start, middle, 2, 3, 4, 5, 6, 7, late
    shared producers: start, middle, 2, 3, 4, 5, 6, 7, late
    shared diagnostic order: start, middle, 2, 3, 4, 5, 6, 7, late
    |}]
;;

let%expect_test "long cold union chains build one index without forcing producers" =
  let dir = path "default/cold-index-chain" in
  let target index = Path.Build.relative dir (Int.to_string index) in
  let forced = ref 0 in
  let leaves =
    run
      (Memo.parallel_map (List.init 10_000 ~f:Fun.id) ~f:(fun index ->
         let file = target index in
         Rules.collect_unit (fun () ->
           Rules.narrow (Target_mask.files [ file ]) (fun () ->
             incr forced;
             Rules.Produce.rule (file_rule [ file ])))))
  in
  let tree = List.fold_left leaves ~init:Rules.empty ~f:Rules.union in
  let mask = Rules.targets tree in
  printfn
    "first and last ownership retained: %b"
    (Target_mask.mem_file mask (target 0) && Target_mask.mem_file mask (target 9_999));
  printfn "producers after indexing: %d" !forced;
  let loaded = run (Rules.load_with_pending tree (Target_mask.path (target 5_000))) in
  printfn "selected rules: %d" (List.length (rules_in ~dir loaded.selected));
  printfn "producers after point lookup: %d" !forced;
  printfn
    "unrelated producer pending: %b"
    (Rules.Pending.mem_file loaded.pending (target 0));
  [%expect
    {|
    first and last ownership retained: true
    producers after indexing: 0
    selected rules: 1
    producers after point lookup: 1
    unrelated producer pending: true
    |}]
;;

let%expect_test "completed producers keep transformed views distinct" =
  let dir = path "default/completed-producer-views" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ a; b ])
           (fun () -> Rules.produce (Rules.of_rules [ file_rule [ a ]; file_rule [ b ] ]))))
  in
  let load tree target = run (Rules.load tree (Target_mask.path target)) in
  let only_rule rules =
    match rules_in ~dir rules with
    | [ rule ] -> rule
    | _ -> Code_error.raise "Expected one selected rule" []
  in
  let original = only_rule (load tree a) in
  let prefixed =
    run
      (Rules.collect_unit (fun () ->
         Rules.prefix_rules (Action_builder.return ()) ~f:(fun () -> Rules.produce tree)))
  in
  let mapped = only_rule (load prefixed a) in
  printfn "prefixed action changed: %b" (mapped.action != original.action);
  printfn "same rule identity: %b" (Rule.equal mapped original);
  let restricted = Rules.restrict tree (Target_mask.files [ a ]) in
  expect_code_error "restricted view validates the completed producer" (fun () ->
    ignore (load restricted a : Rules.t));
  print_files "original sibling after failed restriction" ~dir (load tree b);
  [%expect
    {|
    prefixed action changed: true
    same rule identity: true
    restricted view validates the completed producer: code error
    original sibling after failed restriction: b
    |}]
;;

let%expect_test "failed producers never publish partial rules" =
  let dir = path "default/completed-producer-failure" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let succeeds = Memo.Var.create false ~name:"completed-producer-succeeds" in
  let successful_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ a; b ])
           (fun () ->
              let open Memo.O in
              let* succeeds = Memo.Var.read succeeds in
              let* () = Rules.Produce.rule (file_rule [ a ]) in
              if not succeeds
              then Code_error.raise "Producer failed after emitting a rule" []
              else (
                incr successful_runs;
                Rules.Produce.rule (file_rule [ b ])))))
  in
  let load target = Rules.load tree (Target_mask.path target) in
  let fails label target =
    expect_code_error label (fun () -> ignore (run (load target) : Rules.t))
  in
  fails "first request" a;
  fails "sibling request after failure" b;
  Memo.reset (Memo.Var.set succeeds true);
  let first, sibling = run (Memo.fork_and_join (fun () -> load a) (fun () -> load b)) in
  print_files "recovered first" ~dir first;
  print_files "recovered sibling" ~dir sibling;
  printfn "successful producer runs: %d" !successful_runs;
  Memo.reset (Memo.Var.set succeeds false);
  fails "failure after completed requests" b;
  fails "partial rule remains unavailable" a;
  [%expect
    {|
    first request: code error
    sibling request after failure: code error
    recovered first: a
    recovered sibling: b
    successful producer runs: 1
    failure after completed requests: code error
    partial rule remains unavailable: code error
    |}]
;;

let%expect_test "warmed parents preserve cold-child order and parallel failures" =
  let exercise ~fails =
    let label = if fails then "failure" else "success" in
    let dir = path ("default/warmed-parent-" ^ label) in
    let seed = Path.Build.relative dir "seed" in
    let target = Path.Build.relative dir "target" in
    let parent_runs = ref 0 in
    let forced = ref [] in
    let produce name =
      forced := name :: !forced;
      if fails
      then User_error.raise [ Pp.text name ]
      else
        Rule.make
          ~info:(Rule.Info.From_dune_file (Loc.of_pos (name, 1, 0, 1)))
          ~targets:(Targets.File.create target)
          (Action_builder.return (Action.Full.make Action.empty))
        |> Rules.Produce.rule
    in
    let tree =
      run
        (Rules.collect_unit (fun () ->
           let open Memo.O in
           let* () =
             Rules.narrow
               (Target_mask.files [ seed; target ])
               (fun () ->
                  incr parent_runs;
                  let* () = Rules.Produce.rule (file_rule [ seed ]) in
                  Rules.narrow (Target_mask.files [ target ]) (fun () -> produce "child"))
           in
           Rules.narrow (Target_mask.files [ target ]) (fun () -> produce "sibling")))
    in
    (* The child is registered during warmup, after the root sibling. A later
       request must keep parent grouping instead of sorting all cold IDs. *)
    ignore (run (Rules.load tree (Target_mask.path seed)) : Rules.t);
    printfn "%s warmup: %d parent, %d leaves" label !parent_runs (List.length !forced);
    let result =
      Fiber.run
        (Fiber.collect_errors (fun () ->
           Memo.run (Rules.load tree (Target_mask.path target))))
        ~iter:(fun () -> failwith "unexpected suspension")
    in
    printfn "%s leaves: %s" label (List.rev !forced |> String.concat ~sep:", ");
    match result with
    | Ok selected ->
      printfn
        "%s diagnostic order: %s"
        label
        (rules_in ~dir selected
         |> List.map ~f:(fun rule -> (Loc.start (Rule.loc rule)).pos_fname)
         |> String.concat ~sep:", ")
    | Error errors ->
      let messages =
        List.map errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
          let exn =
            match exn with
            | Memo.Error.E error -> Memo.Error.get error
            | exn -> exn
          in
          match exn with
          | User_error.E message -> User_message.to_string message |> String.trim
          | exn -> raise exn)
        |> List.sort ~compare:String.compare
      in
      printfn "%s errors: %s" label (String.concat messages ~sep:", ")
  in
  exercise ~fails:false;
  exercise ~fails:true;
  [%expect
    {|
    success warmup: 1 parent, 0 leaves
    success leaves: child, sibling
    success diagnostic order: child, sibling
    failure warmup: 1 parent, 0 leaves
    failure leaves: child, sibling
    failure errors: child, sibling
    |}]
;;

let%expect_test "deep completed producer chains remain stack safe" =
  let dir = path "default/deep-completed-producers" in
  let target = Path.Build.relative dir "target" in
  let mask = Target_mask.files [ target ] in
  let depth = 1_024 in
  let forced = ref 0 in
  let rec produce remaining =
    if remaining = 0
    then Rules.Produce.rule (file_rule [ target ])
    else
      Rules.narrow mask (fun () ->
        incr forced;
        produce (remaining - 1))
  in
  let tree = run (Rules.collect_unit (fun () -> produce depth)) in
  let load () = run (Rules.load_with_pending tree (Target_mask.path target)) in
  let cold = load () in
  let cold_runs = !forced in
  let hot = load () in
  printfn "cold producer runs: %d" cold_runs;
  printfn "additional hot producer runs: %d" (!forced - cold_runs);
  printfn "hot refinements: %d" (List.length hot.refinements);
  printfn
    "hot and cold rules share identity: %b"
    (match rules_in ~dir cold.selected, rules_in ~dir hot.selected with
     | [ cold ], [ hot ] -> cold == hot
     | _ -> false);
  [%expect
    {|
    cold producer runs: 1024
    additional hot producer runs: 0
    hot refinements: 1024
    hot and cold rules share identity: true
    |}]
;;

let%expect_test "selected metadata preserves original owners and alias items" =
  let dir = path "default/selected-metadata" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let generated = Path.Build.relative dir "generated" in
  let ignored = Path.Build.relative dir "ignored" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  let older_loc = Loc.of_pos ("older", 1, 0, 1) in
  let newer_loc = Loc.of_pos ("newer", 1, 0, 1) in
  let make_rule target loc =
    Rule.make
      ~info:(Rule.Info.From_dune_file loc)
      ~targets:
        (Targets.create
           ~files:(Path.Build.Set.singleton target)
           ~dirs:(Path.Build.Set.singleton generated))
      (Action_builder.return (Action.Full.make Action.empty))
  in
  let older_rule = make_rule a older_loc in
  let newer_rule = make_rule b newer_loc in
  let older_deps = Action_builder.return () in
  let newer_deps = Action_builder.return () in
  let older =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule older_rule in
         let* () = Rules.Produce.Alias.add_deps alias ~loc:older_loc older_deps in
         Rules.Produce.rule (file_rule [ ignored ])))
  in
  let newer =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule newer_rule in
         Rules.Produce.Alias.add_deps alias ~loc:newer_loc newer_deps))
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Memo.sequential_iter [ newer; older; older ] ~f:(fun chunk ->
           Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce chunk))))
  in
  let requested =
    Target_mask.union (Target_mask.path a) (Target_mask.aliases [ alias ])
  in
  let { Rules.selected; _ } = run (Rules.load_with_pending tree requested) in
  let files, dirs = Rules.target_names selected ~dir in
  let names names =
    Filename.Set.to_list_map names ~f:Filename.to_string |> String.concat ~sep:", "
  in
  printfn "selected files: %s; directories: %s" (names files) (names dirs);
  let owner = Path.Build.Map.find (Rules.directory_targets selected) generated in
  printfn "directory owner: %s" (Loc.start (Option.value_exn owner)).pos_fname;
  let mask = Rules.targets selected in
  printfn
    "selected mask preserves kinds and excludes unselected rules: %b"
    (Target_mask.mem_file mask a
     && Target_mask.mem_file mask b
     && Target_mask.mem_directory mask generated
     && (not (Target_mask.mem_file mask generated))
     && (not (Target_mask.mem_file mask ignored))
     && Target_mask.mem_alias mask alias);
  let { Rules.Dir_rules.rules; aliases } =
    Rules.find selected (Path.build dir) |> Rules.Dir_rules.consume
  in
  printfn
    "rule identities retained without duplicate chunks: %b"
    (match rules with
     | [ older; newer ] -> older == older_rule && newer == newer_rule
     | _ -> false);
  let { Rules.Dir_rules.Alias_spec.expansions } =
    Alias.Name.Map.find aliases (Alias.name alias) |> Option.value_exn
  in
  let expansions = Appendable_list.to_list expansions in
  printfn
    "alias item order: %s"
    (List.map expansions ~f:(fun (loc, _) -> (Loc.start loc).pos_fname)
     |> String.concat ~sep:", ");
  printfn
    "alias item identities retained: %b"
    (match expansions with
     | [ (_, Deps newer); (_, Deps older) ] -> newer == newer_deps && older == older_deps
     | _ -> false);
  [%expect
    {|
    selected files: a, b; directories: generated
    directory owner: older
    selected mask preserves kinds and excludes unselected rules: true
    rule identities retained without duplicate chunks: true
    alias item order: newer, older
    alias item identities retained: true
    |}]
;;

let%expect_test "tiny direct chunks match indexed selection" =
  let dir = path "default/tiny-direct" in
  let child = Path.Build.relative dir "child" in
  let a = Path.Build.relative dir "a.ml" in
  let cmi = Path.Build.relative dir "a.cmi" in
  let nested = Path.Build.relative child "nested.ml" in
  let alias = Alias.make (Alias.Name.of_string "child") ~dir in
  let small =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ a; cmi ]) in
         let* () =
           Rules.Produce.rule
             (rule
                (Targets.create
                   ~files:Path.Build.Set.empty
                   ~dirs:(Path.Build.Set.singleton child)))
         in
         let* () = Rules.Produce.rule (file_rule [ nested ]) in
         Rules.Produce.Alias.add_deps alias (Action_builder.return ())))
  in
  let padding = file_rule [ path "default/tiny-direct-padding/unused" ] in
  let indexed = Rules.union small (Rules.of_rules [ padding ]) in
  let equivalent a b =
    List.for_all [ dir; child ] ~f:(fun dir ->
      let a = Rules.find a (Path.build dir) |> Rules.Dir_rules.consume in
      let b = Rules.find b (Path.build dir) |> Rules.Dir_rules.consume in
      List.equal ( == ) a.rules b.rules
      && Alias.Name.Map.equal a.aliases b.aliases ~equal:( == ))
  in
  List.iter
    [ "empty", Target_mask.empty
    ; "all", Target_mask.all
    ; "file", Target_mask.files [ a ]
    ; "wrong kind", Target_mask.directories [ a ]
    ; "alias", Target_mask.aliases [ alias ]
    ; "root file", Target_mask.files [ child ]
    ; "root directory", Target_mask.directories [ child ]
    ; "root path", Target_mask.path child
    ; "child subtree", Target_mask.subtree child
    ; "parent subtree", Target_mask.subtree dir
    ; "files in directory", Target_mask.files_in_directory dir
    ; "directories in directory", Target_mask.directories_in_directory dir
    ; "aliases in directory", Target_mask.aliases_in_directory dir
    ; ( "extension"
      , Target_mask.file_extensions
          ~dir
          (Filename.Extension.Set.singleton Filename.Extension.ml) )
    ; ( "glob"
      , Target_mask.files_matching
          ~dir
          (Dune_lang.Glob.of_string_exn Loc.none "a.*" |> Predicate_lang.Glob.of_glob) )
    ]
    ~f:(fun (label, mask) ->
      let selected = run (Rules.load small mask) in
      let indexed = run (Rules.load indexed mask) in
      printfn "%s: %b" label (equivalent selected indexed));
  let selected = run (Rules.load small (Target_mask.files [ a ])) in
  printfn "collection identities: %b" (equivalent small (Rules.union small selected));
  printfn "all-match identity: %b" (run (Rules.load small Target_mask.all) == small);
  let subtree = run (Rules.load small (Target_mask.subtree child)) in
  let { Rules.Dir_rules.aliases; _ } =
    Rules.find subtree (Path.build dir) |> Rules.Dir_rules.consume
  in
  printfn "parent alias excluded from child subtree: %b" (Alias.Name.Map.is_empty aliases);
  [%expect
    {|
    empty: true
    all: true
    file: true
    wrong kind: true
    alias: true
    root file: true
    root directory: true
    root path: true
    child subtree: true
    parent subtree: true
    files in directory: true
    directories in directory: true
    aliases in directory: true
    extension: true
    glob: true
    collection identities: true
    all-match identity: true
    parent alias excluded from child subtree: true
    |}]
;;

let%expect_test "tiny direct frontiers retain multi-output conflict closure" =
  let dir = path "default/tiny-direct-closure" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let unrelated = Path.Build.relative dir "unrelated" in
  let alias = Alias.make (Alias.Name.of_string "c") ~dir in
  let directory_rule target =
    rule
      (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton target))
  in
  let forced = ref [] in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ a; b ]) in
         let* () = Rules.Produce.rule (file_rule [ b; c ]) in
         let* () = Rules.Produce.rule (directory_rule c) in
         let* () = Rules.Produce.Alias.add_deps alias (Action_builder.return ()) in
         let* () =
           Rules.narrow (Target_mask.directories [ b ]) (fun () ->
             forced := "directory" :: !forced;
             Rules.Produce.rule (directory_rule b))
         in
         Rules.narrow (Target_mask.files [ unrelated ]) (fun () ->
           forced := "unrelated" :: !forced;
           Rules.Produce.rule (file_rule [ unrelated ]))))
  in
  let loaded = run (Rules.load_with_pending tree (Target_mask.files [ a ])) in
  print_files "selected files" ~dir loaded.selected;
  printfn
    "directory owners: %s"
    (Path.Build.Map.keys (Rules.directory_targets loaded.selected)
     |> List.map ~f:(fun path -> Path.Build.basename path |> Filename.to_string)
     |> String.concat ~sep:", ");
  printfn "forced: %s" (String.concat !forced ~sep:", ");
  printfn "unrelated pending: %b" (Rules.Pending.mem_file loaded.pending unrelated);
  let { Rules.Dir_rules.aliases; _ } =
    Rules.find loaded.selected (Path.build dir) |> Rules.Dir_rules.consume
  in
  printfn "same-name alias unselected: %b" (Alias.Name.Map.is_empty aliases);
  let shared = file_rule [ a ] in
  let twice = Rules.of_rules [ shared; shared ] in
  let twice = run (Rules.load twice (Target_mask.files [ a ])) in
  printfn "distinct collection IDs: %d" (List.length (rules_in ~dir twice));
  let many =
    List.init 17 ~f:(fun index -> Path.Build.relative dir ("many-" ^ Int.to_string index))
    |> file_rule
  in
  let many_tree = Rules.of_rules [ many; file_rule [ unrelated ] ] in
  let many_selected =
    run (Rules.load many_tree (Target_mask.files [ Path.Build.relative dir "many-0" ]))
  in
  printfn
    "many-output rule stays atomic: %b"
    (match rules_in ~dir many_selected with
     | [ selected ] ->
       selected == many && Filename.Set.cardinal selected.targets.files = 17
     | _ -> false);
  [%expect
    {|
    selected files: a, b, b, c
    directory owners: b, c
    forced: directory
    unrelated pending: true
    same-name alias unselected: true
    distinct collection IDs: 2
    many-output rule stays atomic: true
    |}]
;;

let%expect_test "named file masks match full-path file masks" =
  let dir = path "default/named-files" in
  let file name = Path.Build.relative dir name in
  let samples =
    [ dir
    ; file "a.ml"
    ; file "b.cmi"
    ; file "missing"
    ; file "child/a.ml"
    ; path "default/named-files-sibling/a.ml"
    ]
  in
  let alias = Alias.make (Alias.Name.of_string "a.ml") ~dir in
  let queries =
    [ Target_mask.subtree dir
    ; Target_mask.subtree (file "a.ml")
    ; Target_mask.subtree (file "child")
    ; Target_mask.path (file "a.ml")
    ; Target_mask.directories_in_directory dir
    ; Target_mask.aliases [ alias ]
    ; Target_mask.files_matching
        ~dir
        (Dune_lang.Glob.of_string_exn Loc.none "*.cmi" |> Predicate_lang.Glob.of_glob)
    ; Target_mask.file_extensions
        ~dir
        (Filename.Extension.Set.singleton Filename.Extension.ml)
    ]
  in
  List.iter
    [ "empty", []; "singleton", [ "a.ml" ]; "multiple", [ "a.ml"; "b.cmi" ] ]
    ~f:(fun (label, names) ->
      let names =
        Filename.Set.of_list_map names ~f:(fun name ->
          Filename.of_string name |> Option.value_exn)
      in
      let paths = Filename.Set.to_list_map names ~f:(Path.Build.relative_fname dir) in
      let expected = Path.Build.Set.of_list paths in
      let named = Target_mask.files_named ~dir names in
      let expanded = Target_mask.files paths in
      let exact_files =
        List.for_all samples ~f:(fun path ->
          Target_mask.mem_file named path = Path.Build.Set.mem expected path)
        && Target_mask.is_empty named = Filename.Set.is_empty names
      in
      let only_files =
        List.for_all samples ~f:(fun path -> not (Target_mask.mem_directory named path))
        && not (Target_mask.mem_alias named alias)
      in
      let intersections =
        List.for_all queries ~f:(fun query ->
          Target_mask.intersects named query = Target_mask.intersects expanded query
          && Target_mask.intersects query named = Target_mask.intersects query expanded
          && List.for_all samples ~f:(fun path ->
            Target_mask.mem_file (Target_mask.inter named query) path
            = Target_mask.mem_file (Target_mask.inter expanded query) path))
      in
      printfn
        "%s: exact files %b; file-only %b; intersections %b"
        label
        exact_files
        only_files
        intersections);
  [%expect
    {|
    empty: exact files true; file-only true; intersections true
    singleton: exact files true; file-only true; intersections true
    multiple: exact files true; file-only true; intersections true
    |}]
;;

let%expect_test "large finite producer groups preserve selection and pending ownership" =
  let dir = path "default/finite-producers" in
  let child = Path.Build.relative dir "child" in
  let file index ext =
    let dir = if index mod 2 = 0 then dir else child in
    Path.Build.relative dir ("m-" ^ Int.to_string index ^ ext)
  in
  let inputs =
    List.init 64 ~f:(fun index ->
      let outputs = [ file index ".cmo"; file index ".cmt" ] in
      index, outputs, file_rule outputs)
  in
  let forced = ref [] in
  let stage () =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Memo.parallel_iter inputs ~f:(fun (index, outputs, rule) ->
             Rules.narrow (Target_mask.files outputs) (fun () ->
               forced := index :: !forced;
               Rules.Produce.rule rule))
         in
         Rules.narrow Target_mask.empty (fun () ->
           Code_error.raise "An empty owner must not be forced" [])))
  in
  let staged = stage () in
  let absent =
    run
      (Rules.load_with_pending
         staged
         (Target_mask.files_matching
            ~dir
            (Dune_lang.Glob.of_string_exn Loc.none "*.absent"
             |> Predicate_lang.Glob.of_glob)))
  in
  printfn
    "broad miss preserves lazy owners in every directory: %b"
    (List.is_empty !forced
     && List.is_empty (rules_in ~dir absent.selected)
     && Rules.Pending.mem_file absent.pending (file 0 ".cmo")
     && Rules.Pending.mem_file absent.pending (file 7 ".cmo"));
  let eager = Rules.of_rules (List.map inputs ~f:(fun (_, _, rule) -> rule)) in
  let equivalent actual expected =
    List.for_all [ dir; child ] ~f:(fun dir ->
      let actual = rules_in ~dir actual in
      let expected = rules_in ~dir expected in
      List.length actual = List.length expected
      && List.for_all actual ~f:(fun rule -> List.exists expected ~f:(fun r -> r == rule)))
  in
  let first = run (Rules.load_with_pending staged (Target_mask.path (file 7 ".cmo"))) in
  printfn "point request forces one producer: %b" (!forced = [ 7 ]);
  printfn
    "both selected outputs leave pending ownership: %b"
    ((not (Rules.Pending.mem_file first.pending (file 7 ".cmo")))
     && not (Rules.Pending.mem_file first.pending (file 7 ".cmt")));
  printfn
    "unrelated pending ownership stays kind-specific: %b"
    (Rules.Pending.mem_file first.pending (file 9 ".cmo")
     && Rules.Pending.mem_path
          first.pending
          ~dir:child
          (Path.Build.basename (file 9 ".cmo"))
     && (not (Rules.Pending.mem_directory first.pending (file 9 ".cmo")))
     && Rules.Pending.intersects_directory first.pending child
     && not (Rules.Pending.intersects_directory first.pending (file 9 ".cmo")));
  let extra_target = file 100 ".new" in
  let extra_rule = file_rule [ extra_target ] in
  let extra =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.files [ extra_target ]) (fun () ->
           Rules.Produce.rule extra_rule)))
  in
  let inherited =
    run
      (Rules.load_with_pending
         (Rules.union staged extra)
         (Target_mask.files [ file 7 ".cmo"; extra_target ]))
  in
  printfn
    "extending an indexed group preserves identities and pending owners: %b"
    (List.equal
       ( == )
       (rules_in ~dir:child first.selected)
       (rules_in ~dir:child inherited.selected)
     && (match rules_in ~dir inherited.selected with
         | [ selected ] -> selected == extra_rule
         | _ -> false)
     && Rules.Pending.mem_file inherited.pending (file 9 ".cmo")
     && not (Rules.Pending.mem_file inherited.pending extra_target));
  List.iter
    [ "empty", Target_mask.empty
    ; "wrong kind", Target_mask.directories [ file 8 ".cmo" ]
    ; "same point", Target_mask.path (file 7 ".cmo")
    ; "several directories", Target_mask.files [ file 8 ".cmo"; file 9 ".cmt" ]
    ; ( "glob"
      , Target_mask.files_matching
          ~dir:child
          (Dune_lang.Glob.of_string_exn Loc.none "m-1?.cmo" |> Predicate_lang.Glob.of_glob)
      )
    ; "subtree", Target_mask.subtree child
    ; "all", Target_mask.all
    ; "point after all", Target_mask.files [ file 6 ".cmt" ]
    ]
    ~f:(fun (label, request) ->
      printfn
        "%s: %b"
        label
        (equivalent (run (Rules.load staged request)) (run (Rules.load eager request))));
  printfn "each producer evaluated once: %b" (List.length !forced = 64);
  printfn
    "broad first request: %b"
    (equivalent (run (Rules.load (stage ()) Target_mask.all)) eager);
  [%expect
    {|
    broad miss preserves lazy owners in every directory: true
    point request forces one producer: true
    both selected outputs leave pending ownership: true
    unrelated pending ownership stays kind-specific: true
    extending an indexed group preserves identities and pending owners: true
    empty: true
    wrong kind: true
    same point: true
    several directories: true
    glob: true
    subtree: true
    all: true
    point after all: true
    each producer evaluated once: true
    broad first request: true
    |}]
;;

let%expect_test "finite producer frontiers survive broader conflict closure" =
  let dir = path "default/finite-producer-closure" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let nested = Path.Build.relative b "nested" in
  let unused index = Path.Build.relative dir ("unused-" ^ Int.to_string index) in
  let forced = ref [] in
  let declarations =
    [ "first", [ a; b ]; "overlap", [ b; c ]; "nested", [ nested ] ]
    @ List.init 64 ~f:(fun index -> "unused", [ unused index ])
  in
  let finite =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter declarations ~f:(fun (label, outputs) ->
           Rules.narrow (Target_mask.files outputs) (fun () ->
             forced := label :: !forced;
             Rules.Produce.rule (file_rule outputs)))))
  in
  let directory_rule =
    rule (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton b))
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce finite)
         in
         Rules.narrow (Target_mask.directories [ b ]) (fun () ->
           Rules.Produce.rule directory_rule)))
  in
  let loaded = run (Rules.load_with_pending tree (Target_mask.files [ a ])) in
  print_files "root files" ~dir loaded.selected;
  print_files "nested files" ~dir:b loaded.selected;
  printfn
    "only the overlapping and descendant producers ran: %b"
    (List.sort !forced ~compare:String.compare = [ "first"; "nested"; "overlap" ]);
  printfn
    "directory owner included: %b"
    (Path.Build.Map.mem (Rules.directory_targets loaded.selected) b);
  printfn
    "pending ownership excludes completed closure: %b"
    (Rules.Pending.mem_file loaded.pending (unused 0)
     && (not (Rules.Pending.mem_file loaded.pending a))
     && (not (Rules.Pending.mem_file loaded.pending c))
     && not (Rules.Pending.intersects_directory loaded.pending b));
  let directory = run (Rules.load_directory_with_pending tree b) in
  printfn
    "directory-only lookup does not select file rules: %b"
    (match rules_in ~dir directory.selected, rules_in ~dir:b directory.selected with
     | [ selected ], [] -> selected == directory_rule
     | _ -> false);
  [%expect
    {|
    root files: a, b, b, c
    nested files: nested
    only the overlapping and descendant producers ran: true
    directory owner included: true
    pending ownership excludes completed closure: true
    directory-only lookup does not select file rules: true
    |}]
;;

let%expect_test "large finite producer lookups retain dependencies across resets" =
  let dir = path "default/finite-producer-reset" in
  let file index = Path.Build.relative dir ("m-" ^ Int.to_string index) in
  let phase = Memo.Var.create ~name:"finite-producer-phase" 0 in
  let runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 64 ~f:Fun.id) ~f:(fun index ->
           Rules.narrow
             (Target_mask.files [ file index ])
             (fun () ->
                let open Memo.O in
                let* phase = Memo.Var.read phase in
                incr runs;
                match phase with
                | 0 -> Rules.Produce.rule (file_rule [ file index ])
                | 1 -> Memo.return ()
                | _ -> Code_error.raise "Finite producer failed" []))))
  in
  let consumer =
    Memo.create
      "finite-producer-consumer"
      ~input:(module Unit)
      (fun () -> Rules.load tree (Target_mask.path (file 17)))
  in
  let lookup () = run (Memo.exec consumer ()) |> rules_in ~dir in
  let initial = lookup () in
  printfn "initial rules: %d; producer runs: %d" (List.length initial) !runs;
  Memo.reset (Memo.Var.set phase 1);
  let removed = lookup () in
  printfn "removed rules: %d; producer runs: %d" (List.length removed) !runs;
  Memo.reset (Memo.Var.set phase 2);
  expect_code_error "changed producer failure" (fun () ->
    ignore (lookup () : Rule.t list));
  Memo.reset (Memo.Var.set phase 0);
  let recovered = lookup () in
  printfn "recovered rules: %d; producer runs: %d" (List.length recovered) !runs;
  [%expect
    {|
    initial rules: 1; producer runs: 1
    removed rules: 0; producer runs: 2
    changed producer failure: code error
    recovered rules: 1; producer runs: 4
    |}]
;;

let%expect_test "finite pending ownership drops declarations by producer identity" =
  let dir = path "default/finite-pending" in
  let file name = Path.Build.relative dir name in
  let stale = file "stale" in
  let declared_only = Path.Build.relative stale "c" in
  let forced = ref [] in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow
             (Target_mask.files [ file "a"; file "b"; declared_only ])
             (fun () ->
                forced := "a" :: !forced;
                Rules.Produce.rule (file_rule [ file "a" ]))
         in
         let* () =
           Rules.narrow
             (Target_mask.files [ file "b"; file "d" ])
             (fun () ->
                forced := "b" :: !forced;
                Rules.Produce.rule (file_rule [ file "b"; file "d" ]))
         in
         Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
           let target = file ("unused-" ^ Int.to_string index) in
           Rules.narrow (Target_mask.files [ target ]) (fun () ->
             Rules.Produce.rule (file_rule [ target ])))))
  in
  let loaded = run (Rules.load_with_pending tree (Target_mask.path (file "a"))) in
  printfn "only the selected producer ran: %b" (!forced = [ "a" ]);
  printfn
    "overlapping unforced owner survives: %b"
    (Rules.Pending.mem_file loaded.pending (file "b")
     && Rules.Pending.mem_file loaded.pending (file "d"));
  printfn
    "consumed-only directory disappears: %b"
    ((not (Rules.Pending.mem_file loaded.pending declared_only))
     && not (Rules.Pending.intersects_directory loaded.pending stale));
  let again = run (Rules.load_with_pending tree (Target_mask.path (file "b"))) in
  print_files "later request" ~dir again.selected;
  printfn
    "old pending view is immutable: %b"
    (Rules.Pending.mem_file loaded.pending (file "b")
     && not (Rules.Pending.mem_file again.pending (file "b")));
  [%expect
    {|
    only the selected producer ran: true
    overlapping unforced owner survives: true
    consumed-only directory disappears: true
    later request: b, d
    old pending view is immutable: true
    |}]
;;

let%expect_test "large mixed producer groups preserve general owners" =
  let dir = path "default/mixed-producers" in
  let file index = Path.Build.relative dir ("m-" ^ Int.to_string index ^ ".out") in
  let target = file 0 in
  let alias = Alias.make (Alias.Name.of_string "m-0.out") ~dir in
  let alias_forced = ref false in
  let directory_rule =
    rule
      (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton target))
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
             Rules.narrow
               (Target_mask.files [ file index ])
               (fun () -> Rules.Produce.rule (file_rule [ file index ])))
         in
         let* () =
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             alias_forced := true;
             Rules.Produce.Alias.add_deps alias (Action_builder.return ()))
         in
         let* () =
           Rules.narrow (Target_mask.directories [ target ]) (fun () ->
             Rules.Produce.rule directory_rule)
         in
         Rules.narrow
           (Target_mask.files_matching
              ~dir
              (Dune_lang.Glob.of_string_exn Loc.none "*.out"
               |> Predicate_lang.Glob.of_glob))
           (fun () -> Rules.Produce.rule (file_rule [ target ]))))
  in
  let selected = run (Rules.load tree (Target_mask.path target)) in
  printfn "exact, directory and glob owners: %d" (List.length (rules_in ~dir selected));
  printfn "same-name alias remains deferred: %b" (not !alias_forced);
  let aliases = run (Rules.load tree (Target_mask.aliases [ alias ])) in
  let { Rules.Dir_rules.rules; aliases } =
    Rules.find aliases (Path.build dir) |> Rules.Dir_rules.consume
  in
  printfn
    "alias query keeps its kind: %b"
    (!alias_forced && List.is_empty rules && Alias.Name.Map.mem aliases (Alias.name alias));
  let directory = run (Rules.load_directory_with_pending tree target) in
  printfn
    "directory-only query keeps its kind: %b"
    (match rules_in ~dir directory.selected with
     | [ selected ] -> selected == directory_rule
     | _ -> false);
  [%expect
    {|
    exact, directory and glob owners: 3
    same-name alias remains deferred: true
    alias query keeps its kind: true
    directory-only query keeps its kind: true
    |}]
;;

let%expect_test "isolated file closure preserves the remaining frontier" =
  let check label producer_count direct_count =
    let dir = path ("default/isolated-closure-" ^ label) in
    let file name = Path.Build.relative dir name in
    let a = file "a" in
    let b = file "b" in
    let extra = file "extra" in
    let stale = file "stale" in
    let declared_only = Path.Build.relative stale "c" in
    let other_dir = file "other" in
    let other_b = Path.Build.relative other_dir "b" in
    let alias = Alias.make (Alias.Name.of_string "b") ~dir in
    let atomic = file_rule [ a; b ] in
    let producer_runs = ref 0 in
    let unused index = file ("unused-" ^ Int.to_string index) in
    let finite =
      run
        (Rules.collect_unit (fun () ->
           let open Memo.O in
           let* () =
             Rules.narrow
               (Target_mask.files [ a; b; extra; declared_only ])
               (fun () ->
                  incr producer_runs;
                  let* () = Rules.Produce.rule atomic in
                  Rules.Produce.rule (file_rule [ extra ]))
           in
           Memo.parallel_iter (List.init producer_count ~f:Fun.id) ~f:(fun index ->
             Rules.narrow
               (Target_mask.files [ unused index ])
               (fun () -> Code_error.raise "An unrelated file producer was forced" []))))
    in
    let direct_files =
      List.init direct_count ~f:(fun index -> file ("direct-" ^ Int.to_string index))
    in
    let tree =
      run
        (Rules.collect_unit (fun () ->
           let open Memo.O in
           let* () =
             Rules.produce
               (Rules.of_rules
                  (List.map (other_b :: direct_files) ~f:(fun target ->
                     file_rule [ target ])))
           in
           let* () = Rules.Produce.Alias.add_deps alias (Action_builder.return ()) in
           let* () =
             Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce finite)
           in
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             Code_error.raise "A same-name alias producer was forced" [])))
    in
    let load target = run (Rules.load_with_pending tree (Target_mask.files [ target ])) in
    let first = load a in
    let second = load b in
    printfn
      "%s: atomic rule and refinements preserved: %b"
      label
      (List.for_all [ first; second ] ~f:(fun loaded ->
         match rules_in ~dir loaded.Rules.selected with
         | [ selected ] -> selected == atomic
         | _ -> false)
       && List.length first.refinements = 2
       && List.equal
            (fun (a : Rules.refinement) (b : Rules.refinement) ->
               Rules.Producer_id.equal a.id b.id)
            first.refinements
            second.refinements
       && !producer_runs = 1);
    printfn
      "%s: pending owners preserved by identity: %b"
      label
      (Rules.Pending.mem_file first.pending (unused 0)
       && (not
             (List.exists
                [ a; b; extra; declared_only ]
                ~f:(Rules.Pending.mem_file first.pending)))
       && not (Rules.Pending.intersects_directory first.pending stale));
    let revealed_files, revealed_dirs = Rules.Revealed.target_names first.revealed ~dir in
    let other_files, _ = Rules.Revealed.target_names first.revealed ~dir:other_dir in
    printfn
      "%s: unselected direct rules stay revealed: %b"
      label
      (Filename.Set.equal
         revealed_files
         (Filename.Set.of_list_map
            (a :: b :: extra :: direct_files)
            ~f:Path.Build.basename)
       && Filename.Set.is_empty revealed_dirs
       && Filename.Set.mem other_files (Path.Build.basename other_b));
    let { Rules.Dir_rules.aliases = selected_aliases; _ } =
      Rules.find first.selected (Path.build dir) |> Rules.Dir_rules.consume
    in
    let { Rules.Dir_rules.aliases = revealed_aliases; _ } =
      Rules.Revealed.find first.revealed ~dir |> Rules.Dir_rules.consume
    in
    printfn
      "%s: same-name aliases remain unselected and pending: %b"
      label
      (Alias.Name.Map.is_empty selected_aliases
       && Alias.Name.Map.mem revealed_aliases (Alias.name alias)
       && Dir_set.here (Rules.Pending.alias_directories first.pending ~dir))
  in
  check "generic-small" 2 1;
  check "posted-indexed" 64 5;
  [%expect
    {|
    generic-small: atomic rule and refinements preserved: true
    generic-small: pending owners preserved by identity: true
    generic-small: unselected direct rules stay revealed: true
    generic-small: same-name aliases remain unselected and pending: true
    posted-indexed: atomic rule and refinements preserved: true
    posted-indexed: pending owners preserved by identity: true
    posted-indexed: unselected direct rules stay revealed: true
    posted-indexed: same-name aliases remain unselected and pending: true
    |}]
;;

let with_batch_rule_loading ~f =
  let incremental = Memo.is_incremental () in
  Memo.set_incremental false;
  Exn.protect ~f ~finally:(fun () -> Memo.set_incremental incremental)
;;

let%expect_test "atomic selections do not borrow unrelated query or root evidence" =
  with_batch_rule_loading ~f:(fun () ->
    let dir = path "default/atomic-selection-views" in
    let file name = Path.Build.relative dir name in
    let a, b, ghost, foreign = file "a", file "b", file "ghost", file "foreign" in
    let atomic = file_rule [ a; b ] in
    let base = Rules.of_rules [ atomic ] in
    let tree =
      run
        (Rules.collect_unit (fun () ->
           let open Memo.O in
           let* () = Rules.produce base in
           Rules.narrow
             (Target_mask.files [ ghost; foreign ])
             (fun () -> Rules.Produce.rule (file_rule [ foreign ]))))
    in
    let load tree mask = run (Rules.load_with_pending tree mask) in
    let foreign_is_revealed (loaded : Rules.loaded) =
      let files, _ = Rules.Revealed.target_names loaded.revealed ~dir in
      Filename.Set.mem files (Path.Build.basename foreign)
    in
    let broad_mask = Target_mask.files [ a; ghost ] in
    let broad = load tree broad_mask in
    let negative = load tree (Target_mask.path ghost) in
    printfn
      "a warmed negative point preserves its producer evidence: %b"
      (List.is_empty (rules_in ~dir negative.selected)
       && foreign_is_revealed negative
       && List.length negative.refinements = 1
       && List.equal
            (fun (a : Rules.refinement) (b : Rules.refinement) ->
               Rules.Producer_id.equal a.id b.id && a.mask == b.mask)
            broad.refinements
            negative.refinements
       && not (Rules.Pending.mem_file negative.pending foreign));
    let point = load tree (Target_mask.path b) in
    printfn
      "broad evidence is not borrowed by a sibling: %b"
      (foreign_is_revealed broad
       && (not (foreign_is_revealed point))
       && List.length broad.refinements = 1
       && List.is_empty point.refinements
       && Rules.Pending.mem_file point.pending foreign);
    let first, second =
      run
        (Memo.fork_and_join
           (fun () -> Rules.load_with_pending tree (Target_mask.path a))
           (fun () -> Rules.load_with_pending tree (Target_mask.path b)))
    in
    printfn
      "concurrent outputs keep the same atomic rule: %b"
      (List.for_all [ first; second ] ~f:(fun loaded ->
         match rules_in ~dir loaded.Rules.selected with
         | [ selected ] -> selected == atomic
         | _ -> false));
    let broad_again = load tree broad_mask in
    printfn
      "a later broad query retains its additional refinement: %b"
      (foreign_is_revealed broad_again
       && List.length broad_again.refinements = 1
       && not (Rules.Pending.mem_file broad_again.pending foreign));
    let other = file "other" in
    let extended = Rules.union tree (Rules.of_rules [ file_rule [ other ] ]) in
    let from_extended = load extended (Target_mask.path a) in
    let files, _ = Rules.Revealed.target_names from_extended.revealed ~dir in
    printfn
      "a distinct root retains its own revealed rules: %b"
      (Filename.Set.mem files (Path.Build.basename other));
    let check_owner label owner =
      let extended = Rules.union tree (Rules.of_rules [ owner ]) in
      let first = load extended (Target_mask.path a) in
      let second = load extended (Target_mask.path b) in
      printfn
        "%s owners survive both output queries: %b"
        label
        (List.for_all [ first; second ] ~f:(fun loaded ->
           List.length (rules_in ~dir loaded.Rules.selected) = 2))
    in
    check_owner "file" (file_rule [ b ]);
    check_owner
      "directory"
      (rule
         (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton b)));
    printfn
      "separate collections of the same rule remain distinct: %b"
      (List.length
         (rules_in
            ~dir
            (load (Rules.of_rules [ atomic; atomic ]) (Target_mask.path a)).selected)
       = 2));
  [%expect
    {|
    a warmed negative point preserves its producer evidence: true
    broad evidence is not borrowed by a sibling: true
    concurrent outputs keep the same atomic rule: true
    a later broad query retains its additional refinement: true
    a distinct root retains its own revealed rules: true
    file owners survive both output queries: true
    directory owners survive both output queries: true
    separate collections of the same rule remain distinct: true
    |}]
;;

let%expect_test "atomic closure reuse cannot conceal changed failures or cycles" =
  let dir = path "default/atomic-selection-epochs" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let atomic = file_rule [ a; b ] in
  let shared = Rules.of_rules [ atomic ] in
  let phase = Memo.Var.create ~name:"atomic-selection-phase" 0 in
  let recursive = ref (Memo.return ()) in
  let result, tree =
    run
      (Rules.collect (fun () ->
         let open Memo.O in
         let* () = Rules.produce shared in
         Rules.defer (Target_mask.files [ b ]) (fun () ->
           let* phase = Memo.Var.read phase in
           match phase with
           | 1 -> raise (Memo.Non_reproducible (Failure "changed producer"))
           | 2 -> !recursive
           | _ -> Memo.return ())))
  in
  (* Record this producer's input dependency before testing batch-only reuse.
     Ordinary batch nodes deliberately do not retain incremental dependencies. *)
  run (Memo.Lazy.force result);
  (recursive
   := let open Memo.O in
      let+ _ = Rules.load tree (Target_mask.path a) in
      ());
  with_batch_rule_loading ~f:(fun () ->
    let load target = run (Rules.load_with_pending tree (Target_mask.path target)) in
    let first = load b in
    let second = load a in
    printfn
      "empty overlapping ownership stays in the completed closure: %b"
      (List.length first.refinements = 1
       && List.length second.refinements = 1
       && not (Rules.Pending.mem_file second.pending b));
    Memo.reset (Memo.Var.set phase 1);
    printfn
      "changed producer failure is not hidden: %b"
      (try
         ignore (load a : Rules.loaded);
         false
       with
       | Failure message -> String.equal message "changed producer");
    Memo.reset (Memo.Var.set phase 2);
    printfn
      "changed producer cycle is not hidden: %b"
      (try
         ignore (load a : Rules.loaded);
         false
       with
       | Memo.Cycle_error.E _ -> true);
    Memo.reset (Memo.Var.set phase 0);
    let recovered = load a in
    printfn
      "failed closures recover after reset: %b"
      (List.length recovered.refinements = 1
       &&
       match rules_in ~dir recovered.selected with
       | [ selected ] -> selected == atomic
       | _ -> false));
  [%expect
    {|
    empty overlapping ownership stays in the completed closure: true
    changed producer failure is not hidden: true
    changed producer cycle is not hidden: true
    failed closures recover after reset: true
    |}]
;;

let%expect_test "preconstructed batch loads retain watch dependencies" =
  let dir = path "default/atomic-selection-mode" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let atomic = file_rule [ a; b ] in
  let phase = Memo.Var.create ~name:"atomic-selection-mode-phase" false in
  let unrelated = Memo.Var.create ~name:"atomic-selection-mode-unrelated" false in
  let result, tree =
    run
      (Rules.collect (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule atomic in
         Rules.defer (Target_mask.files [ a ]) (fun () ->
           let+ fail = Memo.Var.read phase in
           if fail then failwith "watch producer changed")))
  in
  run (Memo.Lazy.force result);
  let preconstructed =
    with_batch_rule_loading ~f:(fun () ->
      let load_b = Rules.load tree (Target_mask.path b) in
      ignore (run (Rules.load tree (Target_mask.path a)) : Rules.t);
      load_b)
  in
  let consumer_runs = ref 0 in
  let consumer =
    Memo.lazy_ ~name:"preconstructed-watch-consumer" (fun () ->
      incr consumer_runs;
      preconstructed)
  in
  let selected = run (Memo.Lazy.force consumer) in
  printfn
    "the preconstructed load selects its atomic rule: %b"
    (match rules_in ~dir selected with
     | [ selected ] -> selected == atomic
     | _ -> false);
  Memo.reset (Memo.Var.set unrelated true);
  ignore (run (Memo.Lazy.force consumer) : Rules.t);
  printfn "an unrelated reset restores the watch consumer: %b" (!consumer_runs = 1);
  Memo.reset (Memo.Var.set phase true);
  printfn
    "changed sibling ownership is still a tracked dependency: %b"
    (try
       ignore (run (Memo.Lazy.force consumer) : Rules.t);
       false
     with
     | Failure message -> String.equal message "watch producer changed");
  Memo.reset (Memo.Var.set phase false);
  ignore (run (Memo.Lazy.force consumer) : Rules.t);
  [%expect
    {|
    the preconstructed load selects its atomic rule: true
    an unrelated reset restores the watch consumer: true
    changed sibling ownership is still a tracked dependency: true
    |}]
;;

let%expect_test "cold atomic siblings do not share away ownership or failures" =
  with_batch_rule_loading ~f:(fun () ->
    let dir = path "default/atomic-selection-cold" in
    let a = Path.Build.relative dir "a" in
    let b = Path.Build.relative dir "b" in
    let alias = Alias.make (Alias.Name.of_string "b") ~dir in
    let release = Fiber.Ivar.create () in
    let directory_runs = ref 0 in
    let alias_requested = ref false in
    let tree =
      run
        (Rules.collect_unit (fun () ->
           let open Memo.O in
           let* () =
             Rules.narrow
               (Target_mask.files [ a; b ])
               (fun () ->
                  let* () = Memo.of_reproducible_fiber (Fiber.Ivar.read release) in
                  Rules.Produce.rule (file_rule [ a; b ]))
           in
           let* () =
             Rules.narrow (Target_mask.directories [ b ]) (fun () ->
               incr directory_runs;
               Memo.return ())
           in
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             alias_requested := true;
             failwith "requested alias failed")))
    in
    let load mask = Rules.load_with_pending tree mask in
    let released = ref false in
    let first, second =
      Fiber.run
        (Memo.run
           (Memo.fork_and_join
              (fun () -> load (Target_mask.path a))
              (fun () -> load (Target_mask.path b))))
        ~iter:(fun () ->
          if !released then failwith "unexpected suspension";
          released := true;
          [ Fiber.Fill (release, ()) ])
    in
    printfn
      "cold siblings retain completed directory ownership: %b"
      (!released
       && !directory_runs = 1
       && List.for_all [ first; second ] ~f:(fun (loaded : Rules.loaded) ->
         List.length loaded.refinements = 2
         && not (Rules.Pending.mem_directory loaded.pending b)));
    printfn
      "the same-name alias remains pending: %b"
      ((not !alias_requested)
       && List.for_all [ first; second ] ~f:(fun (loaded : Rules.loaded) ->
         Dir_set.here (Rules.Pending.alias_directories loaded.pending ~dir)));
    let mixed = Target_mask.union (Target_mask.path b) (Target_mask.aliases [ alias ]) in
    let fails () =
      try
        ignore (run (load mixed) : Rules.loaded);
        false
      with
      | Failure message -> String.equal message "requested alias failed"
    in
    let failed_first = fails () in
    let failed_again = fails () in
    printfn
      "repeated alias requests still fail in the same epoch: %b"
      (failed_first && failed_again);
    let after_failure = run (load (Target_mask.path a)) in
    printfn
      "a failed alias request leaves the file closure intact: %b"
      (List.length after_failure.refinements = 2
       && Dir_set.here (Rules.Pending.alias_directories after_failure.pending ~dir)));
  [%expect
    {|
    cold siblings retain completed directory ownership: true
    the same-name alias remains pending: true
    repeated alias requests still fail in the same epoch: true
    a failed alias request leaves the file closure intact: true
    |}]
;;

let%expect_test "exact point intersections agree with kind membership" =
  let locations = List.map [ "a"; "a/b.ml"; "a/b.ml/deep.ml"; "ab" ] ~f:path in
  let roots = Path.Build.root :: locations in
  let names = [ "x.ml"; ".hidden.ml"; "x.v.d"; "miss.txt" ] in
  let targets =
    locations
    @ List.concat_map roots ~f:(fun dir -> List.map names ~f:(Path.Build.relative dir))
  in
  let alias target =
    Alias.make
      (Alias.Name.of_string (Filename.to_string (Path.Build.basename target)))
      ~dir:(Path.Build.parent_exn target)
  in
  let glob pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern |> Predicate_lang.Glob.of_glob
  in
  let extensions =
    List.map [ ".ml"; ".v.d" ] ~f:Filename.Extension.of_string_exn
    |> Filename.Extension.Set.of_list
  in
  let compound =
    Predicate_lang.and_
      [ Predicate_lang.or_ [ glob "*.ml"; glob "*.v.d" ]
      ; Predicate_lang.not (glob ".hidden*")
      ]
  in
  let union masks = List.fold_left masks ~init:Target_mask.empty ~f:Target_mask.union in
  let masks =
    [ Target_mask.empty; Target_mask.all ]
    @ List.concat_map roots ~f:(fun dir ->
      let exact = List.map [ "x.ml"; "miss.txt" ] ~f:(Path.Build.relative dir) in
      [ Target_mask.subtree dir
      ; Target_mask.files_in_directory dir
      ; Target_mask.directories_in_directory dir
      ; Target_mask.aliases_in_directory dir
      ; Target_mask.files exact
      ; Target_mask.directories exact
      ; Target_mask.aliases (List.map exact ~f:alias)
      ; Target_mask.file_extensions ~dir extensions
      ; Target_mask.file_extensions_in_subtree ~dir extensions
      ; Target_mask.files_matching ~dir (glob "*.ml")
      ; Target_mask.files_matching ~dir compound
      ; Target_mask.paths_matching ~dir compound
      ])
    @ [ union
          [ Target_mask.files [ path "a/x.ml"; path "ab/miss.txt" ]
          ; Target_mask.directories [ path "a/b.ml"; path "ab/x.ml" ]
          ; Target_mask.aliases [ alias (path "a"); alias (path "a/b.ml/x.ml") ]
          ]
      ; union
          (List.map locations ~f:(fun dir ->
             Target_mask.file_extensions_in_subtree ~dir extensions))
      ; union (List.map locations ~f:Target_mask.files_in_directory)
      ; union [ Target_mask.subtree (path "a/b.ml"); Target_mask.subtree (path "ab") ]
      ]
  in
  List.iter [ `File; `Directory; `Alias; `Path ] ~f:(fun kind ->
    let correct =
      List.for_all masks ~f:(fun mask ->
        List.for_all targets ~f:(fun target ->
          let query, expected =
            match kind with
            | `File -> Target_mask.files [ target ], Target_mask.mem_file mask target
            | `Directory ->
              Target_mask.directories [ target ], Target_mask.mem_directory mask target
            | `Alias ->
              let alias = alias target in
              Target_mask.aliases [ alias ], Target_mask.mem_alias mask alias
            | `Path ->
              ( Target_mask.path target
              , Target_mask.mem_file mask target || Target_mask.mem_directory mask target
              )
          in
          Bool.equal (Target_mask.intersects query mask) expected
          && Bool.equal (Target_mask.intersects mask query) expected))
    in
    let label =
      match kind with
      | `File -> "files"
      | `Directory -> "directories"
      | `Alias -> "aliases"
      | `Path -> "paths"
    in
    printfn "%s: %b" label correct);
  let cleanup_invariant =
    List.for_all masks ~f:(fun mask ->
      List.for_all roots ~f:(fun scope ->
        let restricted = Target_mask.inter mask (Target_mask.subtree scope) in
        List.for_all names ~f:(fun name ->
          let target = Path.Build.relative scope name in
          Bool.equal
            (Target_mask.mem_file mask target)
            (Target_mask.mem_file restricted target)
          && Bool.equal
               (Target_mask.mem_directory mask target)
               (Target_mask.mem_directory restricted target)
          && Bool.equal
               (Target_mask.intersects_directory mask target)
               (Target_mask.intersects_directory restricted target))))
  in
  printfn "initial cleanup restriction preserves child queries: %b" cleanup_invariant;
  [%expect
    {|
    files: true
    directories: true
    aliases: true
    paths: true
    initial cleanup restriction preserves child queries: true
    |}]
;;

let%expect_test "one preconstructed load has independent executions" =
  let dir = path "default/preconstructed-load-executions" in
  let a = Path.Build.relative dir "a" in
  let b = Path.Build.relative dir "b" in
  let c = Path.Build.relative dir "c" in
  let unrelated = Path.Build.relative dir "unrelated" in
  let initial_rule = file_rule [ a; b ] in
  let changed_rule = file_rule [ a; c ] in
  let phase = Memo.Var.create 0 ~name:"preconstructed-load-phase" in
  let release = Fiber.Ivar.create () in
  let producer_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow
             (Target_mask.files [ a; b; c ])
             (fun () ->
                let* phase = Memo.Var.read phase in
                incr producer_runs;
                let* () =
                  if phase = 0
                  then Memo.of_reproducible_fiber (Fiber.Ivar.read release)
                  else Memo.return ()
                in
                match phase with
                | 0 -> Rules.Produce.rule initial_rule
                | 1 -> Rules.Produce.rule changed_rule
                | 2 -> Memo.return ()
                | _ -> Code_error.raise "Preconstructed producer failed" [])
         in
         Rules.narrow (Target_mask.files [ unrelated ]) (fun () ->
           Code_error.raise "Unrelated producer was forced" [])))
  in
  let preconstructed = Rules.load_with_pending tree (Target_mask.path a) in
  let released = ref false in
  let first, second =
    Fiber.run
      (Memo.run
         (Memo.fork_and_join (fun () -> preconstructed) (fun () -> preconstructed)))
      ~iter:(fun () ->
        if !released then failwith "unexpected suspension";
        released := true;
        [ Fiber.Fill (release, ()) ])
  in
  let complete (loaded : Rules.loaded) expected =
    (match rules_in ~dir loaded.selected with
     | [ selected ] -> selected == expected
     | _ -> false)
    && List.length loaded.refinements = 1
    && Rules.Pending.mem_file loaded.pending unrelated
    && List.for_all [ a; b; c ] ~f:(fun file ->
      not (Rules.Pending.mem_file loaded.pending file))
  in
  printfn
    "concurrent executions both complete: %b"
    (!released && complete first initial_rule && complete second initial_rule);
  printfn "concurrent executions share producer work: %b" (!producer_runs = 1);
  Memo.reset (Memo.Var.set phase 1);
  let changed = run preconstructed in
  printfn
    "reused expression observes changed outputs: %b"
    (complete changed changed_rule && !producer_runs = 2);
  printfn
    "earlier results retain their own selections: %b"
    (complete first initial_rule && complete second initial_rule);
  Memo.reset (Memo.Var.set phase 2);
  let removed = run preconstructed in
  printfn
    "reused expression observes removed outputs: %b"
    (List.is_empty (rules_in ~dir removed.selected)
     && List.length removed.refinements = 1
     && Rules.Pending.mem_file removed.pending unrelated
     && (not (Rules.Pending.mem_file removed.pending a))
     && !producer_runs = 3);
  Memo.reset (Memo.Var.set phase 3);
  expect_code_error "reused expression observes producer failure" (fun () ->
    ignore (run preconstructed : Rules.loaded));
  Memo.reset (Memo.Var.set phase 0);
  printfn
    "reused expression recovers after failure: %b"
    (complete (run preconstructed) initial_rule && !producer_runs = 5);
  [%expect
    {|
    concurrent executions both complete: true
    concurrent executions share producer work: true
    reused expression observes changed outputs: true
    earlier results retain their own selections: true
    reused expression observes removed outputs: true
    reused expression observes producer failure: code error
    reused expression recovers after failure: true
    |}]
;;

let%expect_test "awaited direct bodies keep atomic groups and kinds separate" =
  let dir = path "default/direct-body-groups" in
  let file = Path.Build.relative dir in
  let a, a_info = file "a", file "a-info" in
  let b, b_info, b_new = file "b", file "b-info", file "b-new" in
  let spill = file "spill" in
  let first_rule = file_rule [ a; a_info ] in
  let second_rule = file_rule [ b; b_info ] in
  let changed_rule = file_rule [ b; b_new ] in
  let overlap_rule = file_rule [ b_info; spill ] in
  let body =
    Memo.Var.create
      ~name:"direct-body-groups"
      (Rules.of_rules [ first_rule; second_rule ])
  in
  let body_runs = ref 0 in
  let overlap_runs = ref 0 in
  let leaf =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ a; a_info; b; b_info; b_new ])
           (fun () ->
              let open Memo.O in
              let* body = Memo.Var.read body in
              incr body_runs;
              Rules.produce body)))
  in
  let intermediate = Memo.Var.create ~name:"direct-body-ancestry" leaf in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           let* rules = Memo.Var.read intermediate in
           Rules.produce rules)))
  in
  let load target = Rules.load_with_pending tree (Target_mask.path target) in
  let selected_is (loaded : Rules.loaded) expected =
    let actual = rules_in ~dir loaded.selected in
    List.length actual = List.length expected
    && List.for_all expected ~f:(fun rule ->
      List.exists actual ~f:(fun candidate -> candidate == rule))
  in
  let first = run (load b_info) in
  let consumer = Memo.lazy_ ~name:"direct-body-sibling" (fun () -> load b) in
  let sibling = run (Memo.Lazy.force consumer) in
  printfn
    "siblings select only their atomic group: %b"
    (selected_is first [ second_rule ]
     && selected_is sibling [ second_rule ]
     && !body_runs = 1
     && !overlap_runs = 0);
  let revealed, _ = Rules.Revealed.target_names sibling.revealed ~dir in
  printfn
    "unselected groups stay revealed: %b"
    (Filename.Set.cardinal revealed = 4 && List.length sibling.refinements = 2);
  let other = run (load a) in
  printfn
    "a warm body selects another atomic group independently: %b"
    (selected_is other [ first_rule ] && !body_runs = 1 && !overlap_runs = 0);
  let alias = Alias.make (Alias.Name.of_string "b") ~dir in
  let alias = run (Rules.load_with_pending tree (Target_mask.aliases [ alias ])) in
  let directory = run (Rules.load_directory_with_pending tree b) in
  printfn
    "same-name alias and directory requests select no files: %b"
    (Path.Build.Map.is_empty (Rules.to_map alias.selected)
     && Path.Build.Map.is_empty (Rules.to_map directory.selected));
  let overlap =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ b_info; spill ])
           (fun () ->
              incr overlap_runs;
              Rules.Produce.rule overlap_rule)))
  in
  Memo.reset (Memo.Var.set intermediate (Rules.union leaf overlap));
  let extended = run (Memo.Lazy.force consumer) in
  printfn
    "the same root and body observe changed intermediate ownership: %b"
    (selected_is extended [ second_rule; overlap_rule ]
     && !body_runs = 1
     && !overlap_runs = 1
     && List.length extended.refinements = 3);
  let other = run (load a) in
  printfn
    "another group retains its own pending ancestor owner: %b"
    (selected_is other [ first_rule ]
     && Rules.Pending.mem_file other.pending b_info
     && List.length other.refinements = 2);
  Memo.reset (Memo.Var.set body (Rules.of_rules [ changed_rule ]));
  let changed = run (Memo.Lazy.force consumer) in
  let removed = run (load a) in
  printfn
    "a reused sibling consumer observes body replacement: %b"
    (selected_is changed [ changed_rule ] && selected_is removed [] && !body_runs = 2);
  printfn
    "earlier groups retain their original rules: %b"
    (selected_is first [ second_rule ]
     && selected_is extended [ second_rule; overlap_rule ]);
  [%expect
    {|
    siblings select only their atomic group: true
    unselected groups stay revealed: true
    a warm body selects another atomic group independently: true
    same-name alias and directory requests select no files: true
    the same root and body observe changed intermediate ownership: true
    another group retains its own pending ancestor owner: true
    a reused sibling consumer observes body replacement: true
    earlier groups retain their original rules: true
    |}]
;;

let check_concurrent_direct_body_groups () =
  let dir = path "default/direct-body-group-failures" in
  let file = Path.Build.relative dir in
  let a, a_info = file "a", file "a-info" in
  let b, b_info = file "b", file "b-info" in
  let first_rule = file_rule [ a; a_info ] in
  let second_rule = file_rule [ b; b_info ] in
  let release = Fiber.Ivar.create () in
  let body_runs = ref 0 in
  let overlap_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow
             (Target_mask.files [ a; a_info; b; b_info ])
             (fun () ->
                incr body_runs;
                let* () = Memo.of_reproducible_fiber (Fiber.Ivar.read release) in
                Rules.produce (Rules.of_rules [ first_rule; second_rule ]))
         in
         Rules.narrow (Target_mask.files [ b_info ]) (fun () ->
           incr overlap_runs;
           failwith "Direct-body sibling overlap failed")))
  in
  let load target = Rules.load_with_pending tree (Target_mask.path target) in
  let released = ref false in
  let first, second =
    Fiber.run
      (Fiber.fork_and_join
         (fun () -> Fiber.collect_errors (fun () -> Memo.run (load a)))
         (fun () -> Fiber.collect_errors (fun () -> Memo.run (load b))))
      ~iter:(fun () ->
        if !released then failwith "unexpected suspension";
        released := true;
        [ Fiber.Fill (release, ()) ])
  in
  let complete (loaded : Rules.loaded) =
    (match rules_in ~dir loaded.selected with
     | [ selected ] -> selected == first_rule
     | _ -> false)
    && List.length loaded.refinements = 1
    && Rules.Pending.mem_file loaded.pending b_info
  in
  printfn
    "concurrent groups share only the awaited producer: %b"
    (!released && !body_runs = 1 && !overlap_runs = 1);
  printfn
    "one group completes while the other's ancestor overlap fails: %b"
    (match first, second with
     | Ok first, Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
       complete first
       &&
         (match Memo.Error.get error with
         | Failure message -> String.equal message "Direct-body sibling overlap failed"
         | _ -> false)
     | _ -> false);
  printfn
    "a repeated successful group retains its pending owner: %b"
    (complete (run (load a_info)));
  printfn
    "a repeated failing group still checks its ancestor owner: %b"
    (try
       ignore (run (load b) : Rules.loaded);
       false
     with
     | Failure message -> String.equal message "Direct-body sibling overlap failed")
;;

let%expect_test "concurrent direct-body groups retain independent failures" =
  check_concurrent_direct_body_groups ();
  [%expect
    {|
    concurrent groups share only the awaited producer: true
    one group completes while the other's ancestor overlap fails: true
    a repeated successful group retains its pending owner: true
    a repeated failing group still checks its ancestor owner: true
    |}]
;;

let%expect_test "batch direct-body groups retain independent failures" =
  with_batch_rule_loading ~f:check_concurrent_direct_body_groups;
  [%expect
    {|
    concurrent groups share only the awaited producer: true
    one group completes while the other's ancestor overlap fails: true
    a repeated successful group retains its pending owner: true
    a repeated failing group still checks its ancestor owner: true
    |}]
;;

let%expect_test "forced mixed joins preserve frontiers across the component cap" =
  let dir = path "default/mixed-join-cap" in
  let file = Path.Build.relative dir in
  let target i = file ("module-" ^ Int.to_string i) in
  let info i = file ("info-" ^ Int.to_string i) in
  let child = file "aliases" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:child in
  let alias_mask = Target_mask.aliases [ alias ] in
  let forced = ref [] in
  let stage label mask emit =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow mask (fun () ->
           forced := label :: !forced;
           emit)))
  in
  let finite =
    List.init 32 ~f:(fun i ->
      let outputs = [ target i; info i ] in
      stage
        ("module-" ^ Int.to_string i)
        (Target_mask.files outputs)
        (Rules.Produce.rule (file_rule outputs)))
    |> List.fold_left ~init:Rules.empty ~f:Rules.union
  in
  let components =
    finite
    :: List.init 8 ~f:(function
      | 0 ->
        stage
          "alias"
          alias_mask
          (Rules.Produce.Alias.add_deps alias (Action_builder.return ()))
      | 1 ->
        let path = info 0 in
        stage
          "directory"
          (Target_mask.directories [ path ])
          (Rules.Produce.rule
             (rule
                (Targets.create
                   ~files:Path.Build.Set.empty
                   ~dirs:(Path.Build.Set.singleton path))))
      | i ->
        let label = "extra-" ^ Int.to_string i in
        let path = file label in
        stage label (Target_mask.files [ path ]) (Rules.Produce.rule (file_rule [ path ])))
    |> Array.of_list
  in
  let first count = List.init count ~f:(fun i -> components.(i)) in
  (* Capture cold rebuilt indexes before forcing any component or prefix. *)
  let rebuilt_before = List.fold_left (first 8) ~init:Rules.empty ~f:Rules.union in
  let rebuilt_after = List.fold_left (first 9) ~init:Rules.empty ~f:Rules.union in
  let extend tree component =
    ignore (Rules.targets tree : Target_mask.t);
    ignore (Rules.targets component : Target_mask.t);
    let tree = Rules.union tree component in
    ignore (Rules.targets tree : Target_mask.t);
    tree
  in
  let before = List.fold_left (first 8) ~init:Rules.empty ~f:extend in
  let after = extend before components.(8) in
  printfn "forcing every joined index leaves producers delayed: %b" (!forced = []);
  let ownership = Rules.targets after in
  printfn
    "mixed aggregate ownership retained: %b"
    (Target_mask.mem_file ownership (target 31)
     && Target_mask.mem_directory ownership (info 0)
     && Target_mask.mem_alias ownership alias);
  let compare tree rebuilt request =
    let actual = run (Rules.load_with_pending tree request) in
    let expected = run (Rules.load_with_pending rebuilt request) in
    let selected = rules_in ~dir actual.selected in
    let samples = [ target 0; target 1; target 31; info 0; file "extra-7"; child ] in
    let pending =
      List.for_all samples ~f:(fun path ->
        Rules.Pending.mem_file actual.pending path
        = Rules.Pending.mem_file expected.pending path
        && Rules.Pending.mem_directory actual.pending path
           = Rules.Pending.mem_directory expected.pending path
        && Rules.Pending.intersects_directory actual.pending path
           = Rules.Pending.intersects_directory expected.pending path)
    in
    let aliases = Rules.Pending.alias_directories actual.pending ~dir in
    let expected_aliases = Target_mask.alias_directories alias_mask ~dir in
    ( List.equal ( == ) selected (rules_in ~dir expected.selected)
      && List.length (rules_in ~dir (Rules.union actual.selected expected.selected))
         = List.length selected
      && pending
      && Dir_set.is_subset expected_aliases ~of_:aliases
      && Dir_set.is_subset aliases ~of_:expected_aliases
      && List.equal
           (fun (a : Rules.refinement) (b : Rules.refinement) ->
              Rules.Producer_id.equal a.id b.id && a.mask == b.mask)
           actual.refinements
           expected.refinements
      && List.length actual.refinements = 2
    , actual )
  in
  let same, _ = compare before rebuilt_before (Target_mask.path (target 0)) in
  printfn "eight components preserve identity, pending and refinements: %b" same;
  printfn "before-cap order: %s" (String.concat ~sep:", " (List.rev !forced));
  forced := [];
  let same, _ =
    compare after rebuilt_after (Target_mask.files [ target 1; file "extra-7" ])
  in
  printfn "nine components preserve identity, pending and refinements: %b" same;
  printfn "after-cap order: %s" (String.concat ~sep:", " (List.rev !forced));
  let same, again = compare after rebuilt_after (Target_mask.path (target 0)) in
  printfn
    "reverse lookup gets fresh exclusions: %b"
    (same
     && Rules.Pending.mem_file again.pending (target 1)
     && Rules.Pending.mem_file again.pending (file "extra-7"));
  [%expect
    {|
    forcing every joined index leaves producers delayed: true
    mixed aggregate ownership retained: true
    eight components preserve identity, pending and refinements: true
    before-cap order: module-0, directory
    nine components preserve identity, pending and refinements: true
    after-cap order: module-1, extra-7
    reverse lookup gets fresh exclusions: true
    |}]
;;

let%expect_test "preprocessing output families preserve filename transformations" =
  let dir = path "default/pp-families" in
  let relative name = Path.Build.relative dir name in
  let open Dune_lang in
  let dialects = Dialect.DB.add Dialect.DB.empty ~loc:Loc.none Dialect.ocaml in
  let dialects = Dialect.DB.add dialects ~loc:Loc.none Dialect.reason in
  let inputs = [ "x.ml"; "x.mli"; "x.re"; "x.rei" ] in
  let converted = [ "x.re.ml"; "x.rei.mli" ] in
  let preprocessed =
    [ "x.pp.ml"
    ; "x.pp.mli"
    ; "x.pp.re"
    ; "x.re.pp.ml"
    ; "x.pp.re.ml"
    ; "x.pp.rei"
    ; "x.rei.pp.mli"
    ; "x.pp.rei.mli"
    ]
  in
  let excluded =
    [ ".hidden.pp.ml"
    ; ".hidden.pp.mli"
    ; ".hidden.re.ml"
    ; ".hidden.rei.mli"
    ; ".pp.ml"
    ; ".pp.mli"
    ; ".re.ml"
    ; ".rei.mli"
    ]
  in
  let check label config expected =
    let preprocess = Module_reference.Per_item.for_all config in
    let concrete =
      Dune_rules.Pp_spec_rules.rule_targets
        ~dialects
        ~preprocess
        (List.map inputs ~f:relative)
    in
    let families =
      Dune_rules.Pp_spec_rules.rule_target_families
        ~dir
        ~dialects
        ~preprocess
        ~empty_intf:false
    in
    let family_expected = expected @ [ "x.pp.re.ml"; "x.pp.rei.mli" ] in
    printfn
      "%s outputs and original inputs: %b"
      label
      (List.for_all
         (inputs @ converted @ preprocessed @ [ "unrelated.ml" ])
         ~f:(fun name ->
           Target_mask.mem_file concrete (relative name)
           = List.mem expected name ~equal:String.equal
           && Target_mask.mem_file families (relative name)
              = List.mem family_expected name ~equal:String.equal));
    printfn
      "%s rejects dotfiles and empty wildcard prefixes: %b"
      label
      (List.for_all excluded ~f:(fun name ->
         not (Target_mask.mem_file families (relative name))))
  in
  check "none" Preprocess.No_preprocessing converted;
  check
    "staged"
    (Preprocess.Pps { loc = Loc.none; pps = []; flags = []; staged = true })
    converted;
  check
    "active"
    (Preprocess.Action (Loc.none, Dune_lang.Action.Progn []))
    (converted @ preprocessed);
  [%expect
    {|
    none outputs and original inputs: true
    none rejects dotfiles and empty wildcard prefixes: true
    staged outputs and original inputs: true
    staged rejects dotfiles and empty wildcard prefixes: true
    active outputs and original inputs: true
    active rejects dotfiles and empty wildcard prefixes: true
    |}]
;;

let%expect_test "target mask locations grow and shrink without mixing kinds" =
  let dir = path "default/location-transitions" in
  let a = Path.Build.relative dir "a/item" in
  let b = Path.Build.relative dir "b/item" in
  let c = Path.Build.relative dir "c/item" in
  let alias path =
    Alias.make
      (Alias.Name.of_string (Filename.to_string (Path.Build.basename path)))
      ~dir:(Path.Build.parent_exn path)
  in
  let kinds =
    [ "file", Target_mask.files, Target_mask.mem_file
    ; "directory", Target_mask.directories, Target_mask.mem_directory
    ; ( "alias"
      , (fun paths -> Target_mask.aliases (List.map paths ~f:alias))
      , fun mask path -> Target_mask.mem_alias mask (alias path) )
    ]
  in
  List.iter kinds ~f:(fun (kind, make, mem) ->
    let first = make [ a ] in
    let second = make [ b ] in
    let third = make [ c ] in
    let one = Target_mask.union Target_mask.empty first in
    let many = Target_mask.union one second in
    let narrowed = Target_mask.inter many first in
    let states =
      [ Target_mask.empty, []
      ; one, [ a ]
      ; many, [ a; b ]
      ; Target_mask.union many third, [ a; b; c ]
      ; Target_mask.union third many, [ a; b; c ]
      ; Target_mask.union many (Target_mask.union second third), [ a; b; c ]
      ; narrowed, [ a ]
      ; Target_mask.inter narrowed second, []
      ; Target_mask.union narrowed second, [ a; b ]
      ; ( Target_mask.inter many (Target_mask.subtree a)
        , if String.equal kind "alias" then [] else [ a ] )
      ]
    in
    let membership =
      List.for_all states ~f:(fun (mask, expected) ->
        List.for_all [ a; b; c ] ~f:(fun path ->
          mem mask path = List.mem expected path ~equal:Path.Build.equal))
    in
    let independent =
      List.for_all kinds ~f:(fun (other, _, mem) ->
        String.equal kind other
        || List.for_all states ~f:(fun (mask, _) ->
          List.for_all [ a; b; c ] ~f:(fun path -> not (mem mask path))))
    in
    let sharing =
      List.for_all [ one; many; narrowed ] ~f:(fun mask ->
        Target_mask.union mask Target_mask.empty == mask
        && Target_mask.union Target_mask.empty mask == mask
        && Target_mask.union mask mask == mask
        && Target_mask.inter mask mask == mask
        && Target_mask.inter (Target_mask.subtree dir) mask == mask)
    in
    printfn
      "%s transitions: %b; kinds: %b; sharing: %b"
      kind
      membership
      independent
      sharing);
  [%expect
    {|
    file transitions: true; kinds: true; sharing: true
    directory transitions: true; kinds: true; sharing: true
    alias transitions: true; kinds: true; sharing: true
    |}]
;;

let%expect_test "cached glob construction preserves validation and matching" =
  let module G = Predicate_lang.Glob in
  let original pattern = Dune_lang.Glob.of_string pattern |> G.of_glob in
  List.iter [ "literal"; "*.pp.ml"; "**.ml"; "foo.{ml,mli}" ] ~f:(fun pattern ->
    let cached = G.of_string pattern in
    let original = original pattern in
    let metadata () =
      G.equal cached original
      && G.hash cached = G.hash original
      && Option.equal
           String.Set.equal
           (G.finite_elements cached)
           (G.finite_elements original)
      && List.for_all [ ".ml"; ".mli"; ".pp.ml" ] ~f:(fun suffix ->
        G.may_match_suffix cached suffix = G.may_match_suffix original suffix)
    in
    let before = metadata () in
    let matches =
      List.for_all
        [ "literal"; "x.pp.ml"; ".hidden.pp.ml"; "foo.ml"; "foo.mli"; "child/x.ml" ]
        ~f:(fun name ->
          G.test cached ~standard:Predicate_lang.false_ name
          = G.test original ~standard:Predicate_lang.false_ name)
    in
    printfn
      "%S: metadata %b; matches %b; metadata after matching %b"
      pattern
      before
      matches
      (metadata ()));
  let error construct pattern =
    match construct pattern with
    | _ -> None
    | exception Invalid_argument message -> Some message
  in
  List.iter [ "["; "{"; "}" ] ~f:(fun pattern ->
    match error G.of_string pattern, error original pattern with
    | Some actual, Some expected ->
      printfn "%S: same eager error %b: %S" pattern (String.equal actual expected) actual
    | _ -> printfn "%S: missing construction error" pattern);
  [%expect
    {|
    "literal": metadata true; matches true; metadata after matching true
    "*.pp.ml": metadata true; matches true; metadata after matching true
    "**.ml": metadata true; matches true; metadata after matching true
    "foo.{ml,mli}": metadata true; matches true; metadata after matching true
    "[": same eager error true: "invalid glob: :unclosed character set"
    "{": same eager error true: "invalid glob: :unclosed '{'"
    "}": same eager error true: "invalid glob: :'}' without opening '{'"
    |}]
;;

let run_rule_loading_mode ~incremental memo =
  let previous = Memo.is_incremental () in
  Memo.set_incremental incremental;
  Exn.protect ~f:(fun () -> run memo) ~finally:(fun () -> Memo.set_incremental previous)
;;

let same_rule_loading_views ~dirs ~paths (a : Rules.loaded) (b : Rules.loaded) =
  let same_rules a b =
    let a = Rules.Dir_rules.consume a in
    let b = Rules.Dir_rules.consume b in
    List.equal ( == ) a.rules b.rules
    && Alias.Name.Map.equal a.aliases b.aliases ~equal:(fun a b ->
      List.equal
        (fun (a_loc, a) (b_loc, b) -> Loc.equal a_loc b_loc && a == b)
        (Appendable_list.to_list a.Rules.Dir_rules.Alias_spec.expansions)
        (Appendable_list.to_list b.Rules.Dir_rules.Alias_spec.expansions))
  in
  List.for_all dirs ~f:(fun dir ->
    same_rules
      (Rules.find a.selected (Path.build dir))
      (Rules.find b.selected (Path.build dir))
    && List.length (rules_in ~dir (Rules.union a.selected b.selected))
       = List.length (rules_in ~dir a.selected)
    && same_rules
         (Rules.Revealed.find a.revealed ~dir)
         (Rules.Revealed.find b.revealed ~dir)
    &&
    let a_files, a_dirs = Rules.Revealed.target_names a.revealed ~dir in
    let b_files, b_dirs = Rules.Revealed.target_names b.revealed ~dir in
    let a_aliases = Rules.Pending.alias_directories a.pending ~dir in
    let b_aliases = Rules.Pending.alias_directories b.pending ~dir in
    Filename.Set.equal a_files b_files
    && Filename.Set.equal a_dirs b_dirs
    && Dir_set.is_subset a_aliases ~of_:b_aliases
    && Dir_set.is_subset b_aliases ~of_:a_aliases)
  && Path.Build.Set.equal
       (Rules.Revealed.directories a.revealed)
       (Rules.Revealed.directories b.revealed)
  && Path.Build.Map.equal
       (Rules.Revealed.directory_targets a.revealed)
       (Rules.Revealed.directory_targets b.revealed)
       ~equal:Loc.equal
  && List.equal
       (fun (a : Rules.refinement) (b : Rules.refinement) ->
          Rules.Producer_id.equal a.id b.id && a.mask == b.mask)
       a.refinements
       b.refinements
  && List.for_all paths ~f:(fun path ->
    Rules.Pending.mem_file a.pending path = Rules.Pending.mem_file b.pending path
    && Rules.Pending.mem_directory a.pending path
       = Rules.Pending.mem_directory b.pending path
    && Rules.Pending.intersects_directory a.pending path
       = Rules.Pending.intersects_directory b.pending path)
;;

let%expect_test "large routed families preserve concurrent point provenance" =
  let dir = path "default/routed-family-first-wave" in
  let file = Path.Build.relative dir in
  let output index suffix = file ("module-" ^ Int.to_string index ^ suffix) in
  let runs = Array.make 32 0 in
  let byte_rules =
    Array.init 32 ~f:(fun index -> file_rule [ output index ".cmo"; output index ".cmi" ])
  in
  let native_rules =
    Array.init 32 ~f:(fun index -> file_rule [ output index ".cmx"; output index ".o" ])
  in
  let declared index =
    List.map [ ".cmo"; ".cmi"; ".cmx"; ".o"; ".missing" ] ~f:(output index)
  in
  let family =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
           Rules.narrow
             (Target_mask.files (declared index))
             (fun () ->
                runs.(index) <- runs.(index) + 1;
                Rules.produce
                  (Rules.of_rules [ byte_rules.(index); native_rules.(index) ])))))
  in
  let root_file = file "root-metadata" in
  let parent_file = file "parent-metadata" in
  let alias_dir = file "alias-only" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:alias_dir in
  let release = Fiber.Ivar.create () in
  let parent_runs = ref 0 in
  let parent =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           incr parent_runs;
           let* () = Memo.of_reproducible_fiber (Fiber.Ivar.read release) in
           Rules.produce
             (Rules.union (Rules.of_rules [ file_rule [ parent_file ] ]) family))))
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ root_file ]) in
         let* () = Rules.Produce.Alias.add_deps alias (Action_builder.return ()) in
         let* () =
           Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce parent)
         in
         Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
           Code_error.raise "A pending alias must not be forced" [])))
  in
  let load target = Rules.load_with_pending tree (Target_mask.path target) in
  let targets =
    [ output 0 ".cmo"
    ; output 0 ".cmi"
    ; output 0 ".cmx"
    ; output 0 ".missing"
    ; output 1 ".cmo"
    ]
  in
  let expressions = List.map targets ~f:load in
  let released = ref false in
  let actual =
    with_batch_rule_loading ~f:(fun () ->
      Fiber.run
        (Memo.run (Memo.all_concurrently expressions))
        ~iter:(fun () ->
          if !released then failwith "unexpected suspension";
          released := true;
          [ Fiber.Fill (release, ()) ]))
  in
  let paths =
    root_file
    :: parent_file
    :: alias_dir
    :: List.concat_map (List.init 32 ~f:Fun.id) ~f:declared
  in
  let same = same_rule_loading_views ~dirs:[ dir; alias_dir ] ~paths in
  let expected = List.map expressions ~f:(run_rule_loading_mode ~incremental:true) in
  printfn "first-wave views match the watch oracle: %b" (List.equal same actual expected);
  let expected_rules =
    [ [ byte_rules.(0) ]
    ; [ byte_rules.(0) ]
    ; [ native_rules.(0) ]
    ; []
    ; [ byte_rules.(1) ]
    ]
  in
  printfn
    "atomic groups and negative declarations stay separate: %b"
    (match
       List.for_all2 actual expected_rules ~f:(fun (loaded : Rules.loaded) expected ->
         List.equal ( == ) (rules_in ~dir loaded.selected) expected
         && List.length loaded.refinements = 3)
     with
     | Ok same -> same
     | Error _ -> false);
  printfn
    "the gated parent and requested leaves run once: %b"
    (!released
     && !parent_runs = 1
     && Array.to_list runs = 1 :: 1 :: List.init 30 ~f:(fun _ -> 0));
  printfn
    "concurrent sibling declarations remain query-local: %b"
    (match actual with
     | [ first; sibling; native; negative; other ] ->
       List.for_all
         [ first; sibling; native; negative ]
         ~f:(fun (loaded : Rules.loaded) ->
           Rules.Pending.mem_file loaded.pending (output 1 ".cmo")
           && not (Rules.Pending.mem_file loaded.pending (output 0 ".missing")))
       && Rules.Pending.mem_file other.pending (output 0 ".cmo")
     | _ -> false);
  let negative = run_rule_loading_mode ~incremental:false (load (output 2 ".missing")) in
  let negative_oracle =
    run_rule_loading_mode ~incremental:true (load (output 2 ".missing"))
  in
  let files, _ = Rules.Revealed.target_names negative.revealed ~dir in
  printfn
    "a cold negative name still reveals its producer: %b"
    (same negative negative_oracle
     && List.is_empty (rules_in ~dir negative.selected)
     && Filename.Set.mem files (Path.Build.basename (output 2 ".cmx"))
     && runs.(2) = 1
     && runs.(31) = 0);
  let warm = run_rule_loading_mode ~incremental:false (load (output 1 ".cmx")) in
  printfn
    "a warm route preserves another atomic group: %b"
    (same warm (run_rule_loading_mode ~incremental:true (load (output 1 ".cmx")))
     && List.equal ( == ) (rules_in ~dir warm.selected) [ native_rules.(1) ]
     && runs.(1) = 1);
  [%expect
    {|
    first-wave views match the watch oracle: true
    atomic groups and negative declarations stay separate: true
    the gated parent and requested leaves run once: true
    concurrent sibling declarations remain query-local: true
    a cold negative name still reveals its producer: true
    a warm route preserves another atomic group: true
    |}]
;;

let%expect_test "large routed families retain full-mask ancestor ownership" =
  let dir = path "default/routed-family-ancestors" in
  let file = Path.Build.relative dir in
  let output index suffix = file ("module-" ^ Int.to_string index ^ suffix) in
  let runs = Array.make 32 0 in
  let family =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
           let outputs = [ output index ".cmo"; output index ".cmi" ] in
           Rules.narrow (Target_mask.files outputs) (fun () ->
             runs.(index) <- runs.(index) + 1;
             Rules.Produce.rule (file_rule outputs)))))
  in
  let ancestor =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce family)))
  in
  let first_marker = file "first-root" in
  let second_marker = file "second-root" in
  let first_root = Rules.union ancestor (Rules.of_rules [ file_rule [ first_marker ] ]) in
  let directory_rule =
    rule
      (Targets.create
         ~files:Path.Build.Set.empty
         ~dirs:(Path.Build.Set.singleton (output 0 ".cmi")))
  in
  let file_owner = file_rule [ output 0 ".cmi" ] in
  let failures = ref 0 in
  let competing =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.produce
             (Rules.of_rules [ file_rule [ second_marker ]; directory_rule; file_owner ])
         in
         let* () =
           Rules.narrow
             (Target_mask.files [ output 1 ".cmi" ])
             (fun () ->
                incr failures;
                Code_error.raise "The ancestor file owner failed" [])
         in
         Rules.narrow
           (Target_mask.directories [ output 2 ".cmi" ])
           (fun () ->
              incr failures;
              Code_error.raise "The ancestor directory owner failed" [])))
  in
  let second_root = Rules.union ancestor competing in
  let load tree target = Rules.load_with_pending tree (Target_mask.path target) in
  let first =
    run_rule_loading_mode ~incremental:false (load first_root (output 0 ".cmo"))
  in
  let second =
    run_rule_loading_mode ~incremental:false (load second_root (output 0 ".cmo"))
  in
  let same =
    same_rule_loading_views
      ~dirs:[ dir ]
      ~paths:
        [ first_marker
        ; second_marker
        ; output 0 ".cmi"
        ; output 1 ".cmi"
        ; output 31 ".cmo"
        ]
  in
  printfn
    "shared children retain each root's original provenance: %b"
    (same
       first
       (run_rule_loading_mode ~incremental:true (load first_root (output 0 ".cmo")))
     && same
          second
          (run_rule_loading_mode ~incremental:true (load second_root (output 0 ".cmo")))
     &&
     let first_files, _ = Rules.Revealed.target_names first.revealed ~dir in
     let second_files, _ = Rules.Revealed.target_names second.revealed ~dir in
     Filename.Set.mem first_files (Path.Build.basename first_marker)
     && (not (Filename.Set.mem first_files (Path.Build.basename second_marker)))
     && Filename.Set.mem second_files (Path.Build.basename second_marker)
     && not (Filename.Set.mem second_files (Path.Build.basename first_marker)));
  printfn
    "other atomic outputs retain file and same-name directory owners: %b"
    (List.length (rules_in ~dir first.selected) = 1
     && List.length (rules_in ~dir second.selected) = 3
     && Path.Build.Map.mem (Rules.directory_targets second.selected) (output 0 ".cmi")
     && runs.(0) = 1);
  ignore
    (run_rule_loading_mode ~incremental:false (load second_root (output 3 ".cmo"))
     : Rules.loaded);
  List.iter
    [ "file", 1; "directory", 2 ]
    ~f:(fun (kind, index) ->
      expect_code_error
        ("batch " ^ kind ^ " owner")
        (fun () ->
           ignore
             (run_rule_loading_mode
                ~incremental:false
                (load second_root (output index ".cmo"))
              : Rules.loaded));
      expect_code_error
        ("watch " ^ kind ^ " owner")
        (fun () ->
           ignore
             (run_rule_loading_mode
                ~incremental:true
                (load second_root (output index ".cmo"))
              : Rules.loaded)));
  printfn
    "failed owners stay failed without forcing unrelated children: %b"
    (!failures = 2 && runs.(31) = 0);
  [%expect
    {|
    shared children retain each root's original provenance: true
    other atomic outputs retain file and same-name directory owners: true
    batch file owner: code error
    watch file owner: code error
    batch directory owner: code error
    watch directory owner: code error
    failed owners stay failed without forcing unrelated children: true
    |}]
;;

let%expect_test "large routed families respect epochs and execution modes" =
  let dir = path "default/routed-family-epochs" in
  let output index suffix =
    Path.Build.relative dir ("module-" ^ Int.to_string index ^ suffix)
  in
  let atomic = file_rule [ output 0 ".cmo"; output 0 ".cmi" ] in
  let family =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
           let outputs = [ output index ".cmo"; output index ".cmi" ] in
           Rules.narrow (Target_mask.files outputs) (fun () ->
             Rules.Produce.rule (if index = 0 then atomic else file_rule outputs)))))
  in
  let intermediate = Memo.Var.create ~name:"routed-family-intermediate" family in
  let unrelated = Memo.Var.create ~name:"routed-family-unrelated" false in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           let* rules = Memo.Var.read intermediate in
           Rules.produce rules)))
  in
  let load target = Rules.load_with_pending tree (Target_mask.path target) in
  (* Prime producer dependencies in watch mode before warming batch routes. *)
  ignore (run_rule_loading_mode ~incremental:true (load (output 0 ".cmo")) : Rules.loaded);
  let preconstructed =
    with_batch_rule_loading ~f:(fun () ->
      ignore (run (load (output 0 ".cmo")) : Rules.loaded);
      load (output 0 ".cmi"))
  in
  let consumer_runs = ref 0 in
  let consumer =
    Memo.lazy_ ~name:"routed-family-watch-consumer" (fun () ->
      incr consumer_runs;
      preconstructed)
  in
  let selected = run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) in
  printfn
    "a batch-constructed expression executes with watch provenance: %b"
    (List.equal ( == ) (rules_in ~dir selected.selected) [ atomic ]
     && List.length selected.refinements = 2);
  Memo.reset (Memo.Var.set unrelated true);
  ignore
    (run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) : Rules.loaded);
  printfn "an unrelated epoch restores the watch consumer: %b" (!consumer_runs = 1);
  let overlap =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ output 0 ".cmi" ])
           (fun () -> Code_error.raise "New intermediate ownership must be checked" [])))
  in
  Memo.reset (Memo.Var.set intermediate (Rules.union family overlap));
  expect_code_error "watch detects changed intermediate ownership" (fun () ->
    ignore
      (run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) : Rules.loaded));
  expect_code_error "batch discards the earlier epoch's route" (fun () ->
    ignore
      (run_rule_loading_mode ~incremental:false (load (output 0 ".cmo")) : Rules.loaded));
  Memo.reset (Memo.Var.set intermediate family);
  let recovered = run_rule_loading_mode ~incremental:false (load (output 0 ".cmi")) in
  printfn
    "the unchanged root and leaf recover after another epoch: %b"
    (List.equal ( == ) (rules_in ~dir recovered.selected) [ atomic ]
     && List.length recovered.refinements = 2);
  [%expect
    {|
    a batch-constructed expression executes with watch provenance: true
    an unrelated epoch restores the watch consumer: true
    watch detects changed intermediate ownership: code error
    batch discards the earlier epoch's route: code error
    the unchanged root and leaf recover after another epoch: true
    |}]
;;

let%expect_test "joined families retain batch selection provenance" =
  let dir = path "default/routed-family-joined" in
  let file = Path.Build.relative dir in
  let output group index suffix = file (group ^ "-" ^ Int.to_string index ^ suffix) in
  let a_output = output "a" in
  let b_output = output "b" in
  let a_runs = Array.make 32 0 in
  let b_runs = Array.make 32 0 in
  let a_rules =
    Array.init 32 ~f:(fun index ->
      file_rule [ a_output index ".cmo"; a_output index ".cmi" ])
  in
  let b_rules = Array.init 32 ~f:(fun index -> file_rule [ b_output index ".cmo" ]) in
  let a =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
           let owned =
             [ a_output index ".cmo"; a_output index ".cmi"; a_output index ".missing" ]
           in
           Rules.narrow (Target_mask.files owned) (fun () ->
             a_runs.(index) <- a_runs.(index) + 1;
             Rules.Produce.rule a_rules.(index)))))
  in
  let b =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 32 ~f:Fun.id) ~f:(fun index ->
           let owned =
             if index = 0
             then [ b_output index ".cmo"; a_output 0 ".cmi" ]
             else [ b_output index ".cmo" ]
           in
           Rules.narrow (Target_mask.files owned) (fun () ->
             b_runs.(index) <- b_runs.(index) + 1;
             Rules.Produce.rule b_rules.(index)))))
  in
  let alias_dir = file "pending-alias" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:alias_dir in
  let directory = file "pending-directory" in
  let other =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             Code_error.raise "The unrelated alias must remain pending" [])
         in
         Rules.narrow (Target_mask.directories [ directory ]) (fun () ->
           Code_error.raise "The unrelated directory must remain pending" [])))
  in
  let marker = file "metadata" in
  let direct = Rules.of_rules [ file_rule [ marker ] ] in
  let flat = Rules.union (Rules.union (Rules.union a b) other) direct in
  (* Force the separate indexes only after capturing the flat reference. *)
  List.iter [ a; b; other ] ~f:(fun rules -> ignore (Rules.targets rules : Target_mask.t));
  let joined = Rules.union a b in
  ignore (Rules.targets joined : Target_mask.t);
  let joined = Rules.union (Rules.union joined other) direct in
  let load tree target = Rules.load_with_pending tree (Target_mask.path target) in
  let targets =
    [ a_output 1 ".cmo"
    ; a_output 1 ".missing"
    ; b_output 1 ".cmo"
    ; a_output 0 ".cmo"
    ; b_output 0 ".cmo"
    ]
  in
  let actual =
    List.map targets ~f:(fun target ->
      run_rule_loading_mode ~incremental:false (load joined target))
  in
  let paths =
    marker
    :: directory
    :: alias_dir
    :: List.concat_map (List.init 32 ~f:Fun.id) ~f:(fun index ->
      [ a_output index ".cmo"
      ; a_output index ".cmi"
      ; a_output index ".missing"
      ; b_output index ".cmo"
      ])
  in
  let same = same_rule_loading_views ~dirs:[ dir; alias_dir ] ~paths in
  let oracle tree ~incremental =
    List.map targets ~f:(fun target ->
      run_rule_loading_mode ~incremental (load tree target))
  in
  printfn
    "joined views match flat and watch selection: %b"
    (List.equal same actual (oracle flat ~incremental:false)
     && List.equal same actual (oracle joined ~incremental:true));
  printfn
    "isolated and negative routes retain their original refinements: %b"
    (match actual with
     | [ isolated; negative; sibling; overlap; own ] ->
       List.equal ( == ) (rules_in ~dir isolated.selected) [ a_rules.(1) ]
       && List.is_empty (rules_in ~dir negative.selected)
       && List.equal ( == ) (rules_in ~dir sibling.selected) [ b_rules.(1) ]
       && List.equal ( == ) (rules_in ~dir overlap.selected) [ a_rules.(0) ]
       && List.equal ( == ) (rules_in ~dir own.selected) [ b_rules.(0) ]
       && List.length isolated.refinements = 1
       && List.length negative.refinements = 1
       && List.length overlap.refinements = 2
       && List.length own.refinements = 1
       && Rules.Pending.mem_file own.pending (a_output 0 ".cmi")
       && (not (Rules.Pending.mem_file overlap.pending (b_output 0 ".cmo")))
       &&
       let files, _ = Rules.Revealed.target_names overlap.revealed ~dir in
       Filename.Set.mem files (Path.Build.basename (b_output 0 ".cmo"))
     | _ -> false);
  printfn
    "indexed alias and directory owners remain pending: %b"
    (List.for_all actual ~f:(fun (loaded : Rules.loaded) ->
       Rules.Pending.mem_directory loaded.pending directory
       && Dir_set.here (Rules.Pending.alias_directories loaded.pending ~dir:alias_dir)
       && Rules.Pending.mem_file loaded.pending (a_output 31 ".cmo")
       && Rules.Pending.mem_file loaded.pending (b_output 31 ".cmo")));
  printfn
    "atomic closure follows the overlapping component exactly once: %b"
    (Array.to_list a_runs = 1 :: 1 :: List.init 30 ~f:(fun _ -> 0)
     && Array.to_list b_runs = 1 :: 1 :: List.init 30 ~f:(fun _ -> 0));
  [%expect
    {|
    joined views match flat and watch selection: true
    isolated and negative routes retain their original refinements: true
    indexed alias and directory owners remain pending: true
    atomic closure follows the overlapping component exactly once: true
    |}]
;;

let%expect_test "file name bounds agree with exact file names" =
  let dir = path "default/file-name-bounds" in
  let other = Path.Build.relative dir "other" in
  let absent = Path.Build.relative dir "absent" in
  let file = Path.Build.relative dir in
  let single = Target_mask.files [ file "middle.ml" ] in
  let multiple = Target_mask.files [ file "z.ml"; file "a.ml"; file "middle.ml" ] in
  let extensions = Filename.Extension.Set.singleton Filename.Extension.ml in
  let matching dir pattern =
    Target_mask.files_matching ~dir (Predicate_lang.Glob.of_string pattern)
  in
  let masks =
    [ "empty", Target_mask.empty
    ; "empty names", Target_mask.files_named ~dir Filename.Set.empty
    ; "single", single
    ; "multiple", multiple
    ; "same directory union", Target_mask.union multiple single
    ; ( "multiple directories"
      , Target_mask.union
          multiple
          (Target_mask.files [ Path.Build.relative other "b.ml" ]) )
    ; "direct wildcard", Target_mask.files_in_directory dir
    ; "extension", Target_mask.file_extensions ~dir extensions
    ; ( "extension elsewhere"
      , Target_mask.union single (Target_mask.file_extensions ~dir:other extensions) )
    ; "glob", matching dir "*.ml"
    ; "glob elsewhere", Target_mask.union single (matching other "*.ml")
    ; "finite glob", matching dir "{a,z}.ml"
    ; "filtered names", Target_mask.inter multiple (matching dir "m*.ml")
    ; "recursive", Target_mask.file_extensions_in_subtree ~dir extensions
    ; ( "recursive elsewhere"
      , Target_mask.union
          single
          (Target_mask.file_extensions_in_subtree ~dir:other extensions) )
    ; "directory only", Target_mask.directories_in_directory dir
    ; "alias only", Target_mask.aliases_in_directory dir
    ; ( "mixed kinds"
      , Target_mask.union
          multiple
          (Target_mask.union
             (Target_mask.directories_in_directory other)
             (Target_mask.aliases_in_directory dir)) )
    ]
  in
  let oracle mask ~dir =
    match Target_mask.exact_file_names mask with
    | None -> `Non_exact
    | Some locations ->
      (match Path.Build.Map.find locations dir with
       | None -> `Empty
       | Some names ->
         (match Filename.Set.min_elt names, Filename.Set.max_elt names with
          | Some first, Some last -> `Bounds (first, last)
          | _ -> `Empty))
  in
  let equal a b =
    match a, b with
    | `Non_exact, `Non_exact | `Empty, `Empty -> true
    | `Bounds (a_first, a_last), `Bounds (b_first, b_last) ->
      Filename.equal a_first b_first && Filename.equal a_last b_last
    | _ -> false
  in
  List.iter masks ~f:(fun (label, mask) ->
    printfn
      "%s: %b"
      label
      (List.for_all [ dir; other; absent ] ~f:(fun dir ->
         equal (Target_mask.file_name_bounds mask ~dir) (oracle mask ~dir))));
  [%expect
    {|
    empty: true
    empty names: true
    single: true
    multiple: true
    same directory union: true
    multiple directories: true
    direct wildcard: true
    extension: true
    extension elsewhere: true
    glob: true
    glob elsewhere: true
    finite glob: true
    filtered names: true
    recursive: true
    recursive elsewhere: true
    directory only: true
    alias only: true
    mixed kinds: true
    |}]
;;

let%expect_test "large direct file collections retain atomic overlap closure" =
  let dir = path "default/large-direct-files" in
  let file = Path.Build.relative dir in
  let a = file "a" in
  let b = file "b" in
  let c = file "c" in
  let first_rule = file_rule [ a; b ] in
  let second_rule = file_rule [ b; c ] in
  let first = Rules.of_rules [ first_rule ] in
  let second = Rules.of_rules [ second_rule ] in
  let duplicate = Rules.of_rules [ first_rule ] in
  let padding =
    List.init 13 ~f:(fun i ->
      Rules.of_rules [ file_rule [ file ("padding-" ^ Int.to_string i) ] ])
  in
  let nested_rule = file_rule [ Path.Build.relative c "nested" ] in
  let nested = Rules.of_rules [ nested_rule ] in
  let union rules = List.fold_left rules ~init:Rules.empty ~f:Rules.union in
  let direct = union (first :: second :: duplicate :: nested :: padding) in
  let directory_rule =
    rule (Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton c))
  in
  let directory = Rules.of_rules [ directory_rule ] in
  let directory_mask = Target_mask.directories [ c ] in
  let alias_dir = file "unrelated-alias" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:alias_dir in
  let alias_mask = Target_mask.aliases [ alias ] in
  let directory_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce direct in
         let* () =
           Rules.narrow directory_mask (fun () ->
             incr directory_runs;
             Rules.produce directory)
         in
         Rules.narrow alias_mask (fun () ->
           Code_error.raise "An unrelated alias must not be forced" [])))
  in
  let load mask = run (Rules.load_with_pending tree mask) in
  let selected = union [ first; second; duplicate; nested; directory ] in
  let overlap = load (Target_mask.path a) in
  let expected =
    { Rules.selected
    ; revealed = Rules.Revealed.of_rules (Rules.union direct directory)
    ; pending = Rules.Pending.of_mask alias_mask
    ; refinements = overlap.refinements
    }
  in
  let same =
    same_rule_loading_views
      ~dirs:[ dir; c; alias_dir ]
      ~paths:[ a; b; c; Path.Build.relative c "nested"; file "padding-0"; alias_dir ]
  in
  printfn
    "large direct collection retains distinct emissions: %b"
    (List.length (rules_in ~dir direct) = 16);
  printfn
    "atomic overlap preserves duplicate rules and collection order: %b"
    (List.equal
       ( == )
       (rules_in ~dir overlap.selected)
       [ first_rule; second_rule; first_rule; directory_rule ]
     && same overlap expected);
  printfn
    "same-name directory ownership widens closure to descendants: %b"
    (List.equal ( == ) (rules_in ~dir:c overlap.selected) [ nested_rule ]
     && !directory_runs = 1
     && List.length overlap.refinements = 1
     && List.for_all overlap.refinements ~f:(fun refinement ->
       refinement.Rules.mask == directory_mask));
  printfn
    "other atomic outputs preserve revealed and pending views: %b"
    (List.for_all [ b; c ] ~f:(fun target ->
       same (load (Target_mask.path target)) expected));
  let untouched selected =
    { Rules.selected
    ; revealed = Rules.Revealed.of_rules direct
    ; pending = Rules.Pending.of_mask (Target_mask.union directory_mask alias_mask)
    ; refinements = []
    }
  in
  printfn
    "an independent request keeps its original pending directory: %b"
    (same (load (Target_mask.path (file "padding-0"))) (untouched (List.hd padding)));
  let wrong_alias = Alias.make (Alias.Name.of_string "a") ~dir in
  printfn
    "wrong kinds and missing files do not select direct owners: %b"
    (List.for_all
       [ Target_mask.directories [ a ]
       ; Target_mask.aliases [ wrong_alias ]
       ; Target_mask.files [ file "missing" ]
       ]
       ~f:(fun mask -> same (load mask) (untouched Rules.empty)));
  printfn
    "a broad request retains all direct rules without forcing the alias: %b"
    (same
       (load (Target_mask.files_in_directory dir))
       { expected with selected = Rules.union direct directory }
     && !directory_runs = 1);
  let ancestor_runs = ref 0 in
  let ancestor_pending = file "ancestor-pending" in
  let ancestor_direct = Rules.of_rules [ file_rule [ file "ancestor-only" ] ] in
  let wrap body =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce ancestor_direct in
         let* () =
           Rules.narrow (Target_mask.files [ ancestor_pending ]) (fun () ->
             Code_error.raise "An unrelated ancestor owner must not be forced" [])
         in
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           incr ancestor_runs;
           Rules.produce body)))
  in
  let wrapped = wrap tree in
  let batch_views =
    List.map [ a; b ] ~f:(fun target ->
      let load () = Rules.load_with_pending wrapped (Target_mask.path target) in
      let actual = run_rule_loading_mode ~incremental:false (load ()) in
      let expected = run_rule_loading_mode ~incremental:true (load ()) in
      same actual expected
      && List.length actual.refinements = 2
      && Rules.Pending.mem_file actual.pending ancestor_pending)
  in
  printfn
    "batch fallback preserves awaited ancestry and atomic closure: %b"
    (!ancestor_runs = 1 && List.for_all batch_views ~f:Fun.id);
  let competing_runs = ref 0 in
  let competing =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.files [ a ]) (fun () ->
           incr competing_runs;
           failwith "competing direct owner")))
  in
  let failing = wrap (Rules.union tree competing) in
  let failed =
    try
      ignore
        (run_rule_loading_mode
           ~incremental:false
           (Rules.load_with_pending failing (Target_mask.path a))
         : Rules.loaded);
      false
    with
    | Failure message -> String.equal message "competing direct owner"
  in
  printfn
    "batch fallback still awaits a competing owner: %b"
    (failed && !competing_runs = 1);
  [%expect
    {|
    large direct collection retains distinct emissions: true
    atomic overlap preserves duplicate rules and collection order: true
    same-name directory ownership widens closure to descendants: true
    other atomic outputs preserve revealed and pending views: true
    an independent request keeps its original pending directory: true
    wrong kinds and missing files do not select direct owners: true
    a broad request retains all direct rules without forcing the alias: true
    batch fallback preserves awaited ancestry and atomic closure: true
    batch fallback still awaits a competing owner: true
    |}]
;;

let%expect_test "relative file targets preserve validation and enumeration" =
  let dir = path "default/relative-targets" in
  let make ~dir names =
    let names = List.map names ~f:Filename.of_string_exn |> Filename.Set.of_list in
    ( Targets.Files.create_in_dir ~dir names
    , Targets.Files.create
        (Filename.Set.to_list_map names ~f:(Path.Build.relative_fname dir)
         |> Path.Build.Set.of_list) )
  in
  let same_validation a b =
    let open Targets.Validation_result in
    match Targets.validate a, Targets.validate b with
    | Valid a, Valid b ->
      Path.Build.equal a.root b.root
      && Filename.Set.equal a.files b.files
      && Filename.Set.equal a.dirs b.dirs
    | No_targets, No_targets | Inconsistent_parent_dir, Inconsistent_parent_dir -> true
    | ( File_and_directory_target_with_the_same_name a
      , File_and_directory_target_with_the_same_name b ) -> Path.Build.equal a b
    | _ -> false
  in
  let iter t =
    let entries = ref [] in
    Targets.iter
      t
      ~file:(fun path -> entries := (false, path) :: !entries)
      ~dir:(fun path -> entries := (true, path) :: !entries);
    List.rev !entries
  in
  let same (a, b) =
    Bool.equal (Targets.is_empty a) (Targets.is_empty b)
    && Option.equal Path.Build.equal (Targets.head a) (Targets.head b)
    && List.equal Path.Build.equal (Targets.all a) (Targets.all b)
    && List.equal
         (fun (kind_a, a) (kind_b, b) -> Bool.equal kind_a kind_b && Path.Build.equal a b)
         (iter a)
         (iter b)
    && String.equal (Dyn.to_string (Targets.to_dyn a)) (Dyn.to_string (Targets.to_dyn b))
    && same_validation a b
  in
  let cases = List.map [ []; [ "z" ]; [ "z"; "a"; "m"; "a" ] ] ~f:(make ~dir) in
  printfn "empty, singleton and duplicate names: %b" (List.for_all cases ~f:same);
  let names =
    Filename.Set.of_list [ Filename.of_string_exn "a"; Filename.of_string_exn "z" ]
  in
  let relative = Targets.Files.create_in_dir ~dir names in
  printfn
    "validation retains the declared filenames: %b"
    (match Targets.validate relative with
     | Valid targets -> targets.files == names
     | _ -> false);
  printfn
    "empty and identical combinations retain identity: %b"
    (Targets.Files.create_in_dir ~dir Filename.Set.empty == Targets.empty
     && Targets.combine Targets.empty relative == relative
     && Targets.combine relative Targets.empty == relative
     && Targets.combine relative relative == relative);
  let generic ~files ~dirs =
    let paths names =
      List.map names ~f:(Path.Build.relative dir) |> Path.Build.Set.of_list
    in
    let targets = Targets.create ~files:(paths files) ~dirs:(paths dirs) in
    targets, targets
  in
  let cases =
    cases
    @ [ make ~dir [ "a"; "b" ]
      ; make ~dir:(Path.Build.relative dir "child") [ "a" ]
      ; make ~dir:(path "") [ "root-file" ]
      ; generic ~files:[ "a" ] ~dirs:[]
      ; generic ~files:[] ~dirs:[ "a" ]
      ; generic ~files:[ "z" ] ~dirs:[ "a"; "directory" ]
      ]
  in
  printfn
    "mixed kinds, roots and combination orders agree: %b"
    (List.for_all cases ~f:(fun (a, legacy_a) ->
       List.for_all cases ~f:(fun (b, legacy_b) ->
         same (Targets.combine a b, Targets.combine legacy_a legacy_b))));
  let invalid, _ = make ~dir [] in
  expect_code_error "empty rules remain invalid" (fun () ->
    ignore (rule invalid : Rule.t));
  [%expect
    {|
    empty, singleton and duplicate names: true
    validation retains the declared filenames: true
    empty and identical combinations retain identity: true
    mixed kinds, roots and combination orders agree: true
    empty rules remain invalid: code error
    |}]
;;

let%expect_test "ready direct families preserve concurrent point provenance" =
  let dir = path "default/ready-direct-first-wave" in
  let byte = Path.Build.relative dir "byte" in
  let native = Path.Build.relative dir "native" in
  let output dir index suffix =
    Path.Build.relative dir ("module-" ^ Int.to_string index ^ suffix)
  in
  let byte_rules =
    List.init 8 ~f:(fun i -> file_rule [ output byte i ".cmo"; output byte i ".cmi" ])
  in
  let native_rules =
    List.init 8 ~f:(fun i -> file_rule [ output native i ".cmx"; output native i ".o" ])
  in
  let direct = Rules.of_rules (byte_rules @ native_rules) in
  let alias_dir = Path.Build.relative dir "alias-only" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:alias_dir in
  let same_name_alias = Alias.make (Alias.Name.of_string "module-0.cmi") ~dir:byte in
  let pending_dir = Path.Build.relative dir "pending-directory" in
  let body =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce direct in
         let* () =
           Rules.narrow
             (Target_mask.aliases [ alias; same_name_alias ])
             (fun () -> Code_error.raise "An unrelated body alias must not run" [])
         in
         Rules.narrow (Target_mask.directories [ pending_dir ]) (fun () ->
           Code_error.raise "An unrelated body directory must not run" [])))
  in
  let release = Fiber.Ivar.create () in
  let body_runs = ref 0 in
  let parent =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           incr body_runs;
           let* () = Memo.of_reproducible_fiber (Fiber.Ivar.read release) in
           Rules.produce body)))
  in
  let marker = Path.Build.relative dir "ancestor-only" in
  let pending_file = Path.Build.relative dir "ancestor-pending" in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ marker ]) in
         let* () =
           Rules.Produce.rule
             (rule
                (Targets.create
                   ~files:Path.Build.Set.empty
                   ~dirs:(Path.Build.Set.singleton byte)))
         in
         let* () = Rules.Produce.Alias.add_deps alias (Action_builder.return ()) in
         let* () =
           Rules.narrow (Target_mask.files [ pending_file ]) (fun () ->
             Code_error.raise "An unrelated ancestor owner must not run" [])
         in
         Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce parent)))
  in
  let missing = output byte 0 ".declared-only" in
  let targets =
    [ output byte 0 ".cmo"
    ; output byte 0 ".cmi"
    ; output native 0 ".cmx"
    ; output byte 1 ".cmo"
    ; missing
    ]
  in
  let load target = Rules.load_path_with_pending tree target in
  let expressions = List.map targets ~f:load in
  let released = ref false in
  let actual =
    with_batch_rule_loading ~f:(fun () ->
      Fiber.run
        (Memo.run (Memo.all_concurrently expressions))
        ~iter:(fun () ->
          if !released then failwith "unexpected suspension";
          released := true;
          [ Fiber.Fill (release, ()) ]))
  in
  let same =
    same_rule_loading_views
      ~dirs:[ dir; byte; native; alias_dir ]
      ~paths:(marker :: pending_file :: pending_dir :: alias_dir :: targets)
  in
  let expected =
    List.map targets ~f:(fun target ->
      run_rule_loading_mode
        ~incremental:true
        (Rules.load_with_pending tree (Target_mask.path target)))
  in
  printfn
    "first-wave byte, native and negative views match: %b"
    (List.equal same actual expected && !released && !body_runs = 1);
  printfn
    "all requests retain the exact ancestor and body frontier: %b"
    (List.for_all actual ~f:(fun (loaded : Rules.loaded) ->
       List.length loaded.refinements = 2
       && Rules.Pending.mem_file loaded.pending pending_file
       && Rules.Pending.mem_directory loaded.pending pending_dir
       && Rules.Pending.intersects_directory loaded.pending alias_dir
       && Path.Build.Map.mem (Rules.Revealed.directory_targets loaded.revealed) byte
       && not (Rules.Pending.mem_file loaded.pending missing)));
  let warm = run_rule_loading_mode ~incremental:false (load (output native 1 ".o")) in
  printfn
    "a warm route keeps its original rule and collection identity: %b"
    (same
       warm
       (run_rule_loading_mode
          ~incremental:true
          (Rules.load_with_pending tree (Target_mask.path (output native 1 ".o"))))
     && List.equal
          ( == )
          (rules_in ~dir:native warm.selected)
          [ List.nth native_rules 1 |> Option.value_exn ]
     && !body_runs = 1);
  let broad = Rules.load_with_pending tree (Target_mask.files_in_directory byte) in
  printfn
    "broad requests keep the ordinary selection path: %b"
    (same
       (run_rule_loading_mode ~incremental:false broad)
       (run_rule_loading_mode ~incremental:true broad));
  expect_code_error "a same-name alias still takes its own producer" (fun () ->
    ignore
      (run_rule_loading_mode
         ~incremental:false
         (Rules.load_with_pending tree (Target_mask.aliases [ same_name_alias ]))
       : Rules.loaded));
  [%expect
    {|
    first-wave byte, native and negative views match: true
    all requests retain the exact ancestor and body frontier: true
    a warm route keeps its original rule and collection identity: true
    broad requests keep the ordinary selection path: true
    a same-name alias still takes its own producer: code error
    |}]
;;

let%expect_test "ready direct routes reject atomic and ancestor competitors" =
  let dir = path "default/ready-direct-conflicts" in
  let output index suffix =
    Path.Build.relative dir ("module-" ^ Int.to_string index ^ suffix)
  in
  let atomic =
    List.init 16 ~f:(fun i -> file_rule [ output i ".cmo"; output i ".cmi" ])
  in
  let direct = Rules.of_rules atomic in
  let failures = ref 0 in
  let body =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce direct in
         let fail kind =
           incr failures;
           Code_error.raise ("The body " ^ kind ^ " owner failed") []
         in
         let* () =
           Rules.narrow (Target_mask.files [ output 1 ".cmi" ]) (fun () -> fail "file")
         in
         Rules.narrow
           (Target_mask.directories [ output 2 ".cmi" ])
           (fun () -> fail "directory")))
  in
  let wrap body =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce body)))
  in
  let tree = wrap body in
  let load tree i = Rules.load_path_with_pending tree (output i ".cmo") in
  ignore (run_rule_loading_mode ~incremental:false (load tree 0) : Rules.loaded);
  List.iter
    [ "file", 1; "directory", 2 ]
    ~f:(fun (kind, i) ->
      List.iter [ false; true; false ] ~f:(fun incremental ->
        expect_code_error
          ((if incremental then "watch " else "batch ") ^ kind ^ " competitor")
          (fun () ->
             ignore (run_rule_loading_mode ~incremental (load tree i) : Rules.loaded))));
  printfn "failed owners are not replaced by a positive route: %b" (!failures = 2);
  let duplicate_rule = List.nth atomic 3 |> Option.value_exn in
  let duplicate = wrap (Rules.union direct (Rules.of_rules [ duplicate_rule ])) in
  let selected = run_rule_loading_mode ~incremental:false (load duplicate 3) in
  printfn
    "distinct emissions of the same Rule remain distinct: %b"
    (List.equal
       ( == )
       (rules_in ~dir selected.selected)
       [ duplicate_rule; duplicate_rule ]);
  let first_marker = Path.Build.relative dir "first-root" in
  let second_marker = Path.Build.relative dir "second-root" in
  let shared = wrap direct in
  let first = Rules.union shared (Rules.of_rules [ file_rule [ first_marker ] ]) in
  let nested = Path.Build.relative (output 0 ".cmi") "nested" in
  let directory_rule =
    rule
      (Targets.create
         ~files:Path.Build.Set.empty
         ~dirs:(Path.Build.Set.singleton (output 0 ".cmi")))
  in
  let second =
    Rules.union
      shared
      (Rules.of_rules
         [ file_rule [ second_marker ]; directory_rule; file_rule [ nested ] ])
  in
  let same =
    same_rule_loading_views
      ~dirs:[ dir; output 0 ".cmi" ]
      ~paths:[ first_marker; second_marker; output 0 ".cmo"; output 0 ".cmi"; nested ]
  in
  let first_view = run_rule_loading_mode ~incremental:false (load first 0) in
  let second_view = run_rule_loading_mode ~incremental:false (load second 0) in
  printfn
    "shared bodies keep root-specific directory closure and provenance: %b"
    (same
       first_view
       (run_rule_loading_mode
          ~incremental:true
          (Rules.load_with_pending first (Target_mask.path (output 0 ".cmo"))))
     && same
          second_view
          (run_rule_loading_mode
             ~incremental:true
             (Rules.load_with_pending second (Target_mask.path (output 0 ".cmo"))))
     && List.length (rules_in ~dir first_view.selected) = 1
     && List.length (rules_in ~dir second_view.selected) = 2
     && List.length (rules_in ~dir:(output 0 ".cmi") second_view.selected) = 1);
  let directory_view =
    run_rule_loading_mode
      ~incremental:false
      (Rules.load_path_with_pending second (output 0 ".cmi"))
  in
  printfn
    "path lookup keeps competing kinds and the directory's descendant closure: %b"
    (same
       directory_view
       (run_rule_loading_mode
          ~incremental:true
          (Rules.load_with_pending second (Target_mask.path (output 0 ".cmi"))))
     && List.length (rules_in ~dir directory_view.selected) = 2
     && List.length (rules_in ~dir:(output 0 ".cmi") directory_view.selected) = 1);
  [%expect
    {|
    batch file competitor: code error
    watch file competitor: code error
    batch file competitor: code error
    batch directory competitor: code error
    watch directory competitor: code error
    batch directory competitor: code error
    failed owners are not replaced by a positive route: true
    distinct emissions of the same Rule remain distinct: true
    shared bodies keep root-specific directory closure and provenance: true
    path lookup keeps competing kinds and the directory's descendant closure: true
    |}]
;;

let%expect_test "ready direct routes retain execution mode and epoch guards" =
  let dir = path "default/ready-direct-epochs" in
  let output i suffix = Path.Build.relative dir ("module-" ^ Int.to_string i ^ suffix) in
  let atomic = file_rule [ output 0 ".cmo"; output 0 ".cmi" ] in
  let direct =
    Rules.of_rules
      (atomic :: List.init 15 ~f:(fun i -> file_rule [ output (i + 1) ".cmo" ]))
  in
  let fail = Memo.Var.create ~name:"ready-direct-producer-fails" false in
  let leaf =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           let* fail = Memo.Var.read fail in
           if fail
           then Code_error.raise "The original direct producer failed" []
           else Rules.produce direct)))
  in
  let body = Memo.Var.create ~name:"ready-direct-body" leaf in
  let unrelated = Memo.Var.create ~name:"ready-direct-unrelated" false in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           let open Memo.O in
           let* body = Memo.Var.read body in
           Rules.produce body)))
  in
  let load suffix = Rules.load_path_with_pending tree (output 0 suffix) in
  ignore (run_rule_loading_mode ~incremental:true (load ".cmo") : Rules.loaded);
  let expression =
    with_batch_rule_loading ~f:(fun () ->
      ignore (run (load ".cmo") : Rules.loaded);
      load ".cmi")
  in
  let runs = ref 0 in
  let consumer =
    Memo.lazy_ ~name:"ready-direct-watch-consumer" (fun () ->
      incr runs;
      expression)
  in
  let selected = run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) in
  Memo.reset (Memo.Var.set unrelated true);
  ignore
    (run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) : Rules.loaded);
  printfn
    "batch construction does not hide watch dependencies: %b"
    (!runs = 1 && List.equal ( == ) (rules_in ~dir selected.selected) [ atomic ]);
  let competitor =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow
           (Target_mask.files [ output 0 ".cmi" ])
           (fun () -> Code_error.raise "Changed body ownership must be checked" [])))
  in
  Memo.reset (Memo.Var.set body (Rules.union leaf competitor));
  expect_code_error "watch checks the changed ancestor" (fun () ->
    ignore
      (run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) : Rules.loaded));
  expect_code_error "batch cannot reuse the previous epoch's singleton" (fun () ->
    ignore (run_rule_loading_mode ~incremental:false (load ".cmo") : Rules.loaded));
  Memo.reset (Memo.Var.set body leaf);
  let recovered = run_rule_loading_mode ~incremental:false (load ".cmi") in
  printfn
    "the same root recovers with its original collection ID: %b"
    (List.equal ( == ) (rules_in ~dir recovered.selected) [ atomic ]
     && List.length (rules_in ~dir (Rules.union recovered.selected direct)) = 16);
  Memo.reset (Memo.Var.set fail true);
  expect_code_error "watch awaits the failing original producer" (fun () ->
    ignore (run_rule_loading_mode ~incremental:true (load ".cmo") : Rules.loaded));
  expect_code_error "batch awaits the failing original producer" (fun () ->
    ignore (run_rule_loading_mode ~incremental:false (load ".cmi") : Rules.loaded));
  Memo.reset (Memo.Var.set fail false);
  let watch = run_rule_loading_mode ~incremental:true (load ".cmo") in
  let batch = run_rule_loading_mode ~incremental:false (load ".cmi") in
  printfn
    "the original producer and unchanged direct body recover: %b"
    (same_rule_loading_views
       ~dirs:[ dir ]
       ~paths:[ output 0 ".cmo"; output 0 ".cmi" ]
       watch
       batch);
  [%expect
    {|
    batch construction does not hide watch dependencies: true
    watch checks the changed ancestor: code error
    batch cannot reuse the previous epoch's singleton: code error
    the same root recovers with its original collection ID: true
    watch awaits the failing original producer: code error
    batch awaits the failing original producer: code error
    the original producer and unchanged direct body recover: true
    |}]
;;

let%expect_test "point routes validate ancestors before obsolete children" =
  List.iter [ false; true ] ~f:(fun incremental ->
    List.iter [ false; true ] ~f:(fun replace ->
      let mode = if incremental then "watch" else "batch" in
      let dir = path ("default/route-ancestor-order-" ^ mode) in
      let output i suffix =
        Path.Build.relative dir ("module-" ^ Int.to_string i ^ suffix)
      in
      let original = file_rule [ output 0 ".cmo"; output 0 ".cmi" ] in
      let replacement = file_rule [ output 0 ".cmo"; output 0 ".cmi" ] in
      let family atomic =
        Rules.of_rules
          (atomic :: List.init 15 ~f:(fun i -> file_rule [ output (i + 1) ".cmo" ]))
      in
      let original_body = family original in
      let replacement_body = family replacement in
      let phase = Memo.Var.create ~name:"route-ancestor-phase" `Original in
      let poison = Memo.Var.create ~name:"route-obsolete-child" false in
      let child_entries = ref 0 in
      let old_child =
        run
          (Rules.collect_unit (fun () ->
             Rules.narrow (Target_mask.subtree dir) (fun () ->
               incr child_entries;
               let open Memo.O in
               let* poison = Memo.Var.read poison in
               if poison then Code_error.raise "Entered an obsolete child" [];
               Rules.produce original_body)))
      in
      let ancestor =
        run
          (Rules.collect_unit (fun () ->
             Rules.narrow (Target_mask.subtree dir) (fun () ->
               let open Memo.O in
               let* phase = Memo.Var.read phase in
               match phase with
               | `Original -> Rules.produce old_child
               | `Fail -> Code_error.raise "The current ancestor failed" []
               | `Replacement -> Rules.produce replacement_body)))
      in
      let tree =
        run
          (Rules.collect_unit (fun () ->
             Rules.narrow (Target_mask.subtree dir) (fun () -> Rules.produce ancestor)))
      in
      let load suffix = Rules.load_path_with_pending tree (output 0 suffix) in
      (* Record producer dependencies before warming routes for the sibling. *)
      ignore (run_rule_loading_mode ~incremental:true (load ".cmo") : Rules.loaded);
      ignore (run_rule_loading_mode ~incremental:false (load ".cmo") : Rules.loaded);
      let consumer = Memo.lazy_ ~name:"route-ancestor-consumer" (fun () -> load ".cmi") in
      let initial = run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) in
      Memo.reset
        (Memo.Invalidation.combine
           (Memo.Var.set phase (if replace then `Replacement else `Fail))
           (Memo.Var.set poison true));
      let query () =
        run_rule_loading_mode
          ~incremental
          (if incremental then Memo.Lazy.force consumer else load ".cmi")
      in
      if replace
      then (
        let current = query () in
        printfn
          "%s: replacement bypasses the poisoned child: %b"
          mode
          (!child_entries = 1
           && List.equal ( == ) (rules_in ~dir current.selected) [ replacement ]
           && List.equal ( == ) (rules_in ~dir initial.selected) [ original ]))
      else (
        let ancestor_failed =
          try
            ignore (query () : Rules.loaded);
            false
          with
          | Code_error.E { message; _ } ->
            String.equal message "The current ancestor failed"
        in
        printfn
          "%s: failing ancestor leaves the old child untouched: %b"
          mode
          (ancestor_failed && !child_entries = 1))));
  [%expect
    {|
    batch: failing ancestor leaves the old child untouched: true
    batch: replacement bypasses the poisoned child: true
    watch: failing ancestor leaves the old child untouched: true
    watch: replacement bypasses the poisoned child: true
    |}]
;;

let%expect_test "warm watch routes validate independently across a suspension" =
  let dir = path "default/warm-route-interleaving" in
  let output dir i suffix =
    Path.Build.relative dir ("module-" ^ Int.to_string i ^ suffix)
  in
  let family name =
    let dir = Path.Build.relative dir name in
    let atomic = file_rule [ output dir 0 ".cmo"; output dir 0 ".cmi" ] in
    let body =
      Rules.of_rules
        (atomic :: List.init 15 ~f:(fun i -> file_rule [ output dir (i + 1) ".cmo" ]))
    in
    dir, atomic, body
  in
  let a_dir, a_rule, a_body = family "a" in
  let b_dir, b_rule, b_body = family "b" in
  let unrelated = Path.Build.relative dir "unrelated" in
  let wait = Memo.Var.create ~name:"warm-route-prepare-waits" false in
  let entered = Fiber.Ivar.create () in
  let release = Fiber.Ivar.create () in
  let a_prepares, a_produces = ref 0, ref 0 in
  let b_prepares, b_produces = ref 0, ref 0 in
  let prepare_a =
    let open Memo.O in
    let* wait = Memo.Var.read wait in
    incr a_prepares;
    if not wait
    then Memo.return ()
    else
      let* () = Memo.of_reproducible_fiber (Fiber.Ivar.fill entered ()) in
      Memo.of_reproducible_fiber (Fiber.Ivar.read release)
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* _ =
           Rules.defer_after (Target_mask.subtree a_dir) ~prepare:prepare_a (fun () ->
             incr a_produces;
             Rules.produce a_body)
         in
         let* _ =
           Rules.defer_after
             (Target_mask.subtree b_dir)
             ~prepare:
               (let+ () = Memo.return () in
                incr b_prepares)
             (fun () ->
                incr b_produces;
                Rules.produce b_body)
         in
         Rules.narrow (Target_mask.files [ unrelated ]) (fun () ->
           Code_error.raise "An unrelated owner was awaited" [])))
  in
  let load dir suffix =
    Memo.of_thunk (fun () ->
      assert (Memo.is_incremental ());
      Rules.load_path_with_pending tree (output dir 0 suffix))
  in
  let check (loaded : Rules.loaded) dir atomic body other =
    assert (List.equal ( == ) (rules_in ~dir loaded.selected) [ atomic ]);
    assert (List.length (rules_in ~dir (Rules.union loaded.selected body)) = 16);
    assert (Rules.Dir_rules.is_empty (Rules.find loaded.selected (Path.build other)));
    assert (List.length loaded.refinements = 1);
    assert (Rules.Pending.mem_file loaded.pending (output other 0 ".cmi"));
    assert (Rules.Pending.mem_file loaded.pending unrelated);
    assert (not (Rules.Pending.mem_file loaded.pending (output dir 0 ".cmi")))
  in
  let warm = run_rule_loading_mode ~incremental:true (load a_dir ".cmo") in
  check warm a_dir a_rule a_body b_dir;
  assert (!a_prepares = 1 && !a_produces = 1 && !b_prepares = 0 && !b_produces = 0);
  Memo.reset (Memo.Var.set wait true);
  let b_completed = ref false in
  let released = ref false in
  let a, b =
    Fiber.run
      (Fiber.fork_and_join
         (fun () -> Memo.run (load a_dir ".cmi"))
         (fun () ->
            let open Fiber.O in
            let* () = Fiber.Ivar.read entered in
            let+ loaded = Memo.run (load b_dir ".cmo") in
            check loaded b_dir b_rule b_body a_dir;
            assert (not !released);
            b_completed := true;
            loaded))
      ~iter:(fun () ->
        assert (!b_completed && not !released);
        assert (!a_prepares = 2 && !a_produces = 1);
        assert (!b_prepares = 1 && !b_produces = 1);
        released := true;
        [ Fiber.Fill (release, ()) ])
  in
  assert !released;
  check a a_dir a_rule a_body b_dir;
  check b b_dir b_rule b_body a_dir;
  assert (a.selected == warm.selected);
  check
    (run_rule_loading_mode ~incremental:true (load b_dir ".cmi"))
    b_dir
    b_rule
    b_body
    a_dir;
  check
    (run_rule_loading_mode ~incremental:true (load a_dir ".cmo"))
    a_dir
    a_rule
    a_body
    b_dir;
  assert (!a_prepares = 2 && !a_produces = 1);
  assert (!b_prepares = 1 && !b_produces = 1);
  [%expect {| |}]
;;

let%expect_test "point route lifetime after a failed ancestor" =
  let dir = path "default/route-failed-ancestor-lifetime" in
  let old_dir = Path.Build.relative dir "old" in
  let healthy_dir = Path.Build.relative dir "healthy" in
  let output dir i suffix =
    Path.Build.relative dir ("module-" ^ Int.to_string i ^ suffix)
  in
  let family dir atomic =
    Rules.of_rules
      (atomic :: List.init 15 ~f:(fun i -> file_rule [ output dir (i + 1) ".cmo" ]))
  in
  let weak = Weak.create 1 in
  let fail = Memo.Var.create ~name:"route-lifetime-ancestor-fails" false in
  let ancestor_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.narrow (Target_mask.subtree old_dir) (fun () ->
             incr ancestor_runs;
             let* fail = Memo.Var.read fail in
             if fail then Code_error.raise "The lifetime ancestor failed" [];
             (* The root callback never captures this body. *)
             let atomic =
               file_rule [ output old_dir 0 ".cmo"; output old_dir 0 ".cmi" ]
             in
             Weak.set weak 0 (Some atomic);
             let body = family old_dir atomic in
             Rules.narrow (Target_mask.subtree old_dir) (fun () -> Rules.produce body))
         in
         Rules.narrow (Target_mask.subtree healthy_dir) (fun () ->
           Rules.produce (family healthy_dir (file_rule [ output healthy_dir 0 ".cmo" ])))))
  in
  let load dir suffix =
    ignore
      (run_rule_loading_mode
         ~incremental:true
         (Rules.load_path_with_pending tree (output dir 0 suffix))
       : Rules.loaded)
  in
  load old_dir ".cmo";
  load old_dir ".cmi";
  assert (!ancestor_runs = 1 && Weak.check weak 0);
  Memo.reset (Memo.Var.set fail true);
  assert (
    try
      load old_dir ".cmi";
      false
    with
    | Code_error.E { message; _ } -> String.equal message "The lifetime ancestor failed");
  assert (!ancestor_runs = 2);
  let retained () =
    for _round = 1 to 3 do
      Gc.full_major ()
    done;
    Weak.check weak 0
  in
  printfn "retained after failed ancestor: %b" (retained ());
  (* Replacing just the route must not recompute the failed ancestor. *)
  load healthy_dir ".cmo";
  assert (!ancestor_runs = 2);
  printfn "retained after healthy route: %b" (retained ());
  ignore (Sys.opaque_identity tree);
  [%expect
    {|
    retained after failed ancestor: false
    retained after healthy route: false
    |}]
;;

let%expect_test "point route misses leave unrelated cached producers untouched" =
  List.iter [ false; true ] ~f:(fun incremental ->
    let mode = if incremental then "watch" else "batch" in
    let dir = path ("default/route-unrelated-prefix-" ^ mode) in
    let first_dir = Path.Build.relative dir "first" in
    let second_dir = Path.Build.relative dir "second" in
    let first i suffix =
      Path.Build.relative first_dir ("module-" ^ Int.to_string i ^ suffix)
    in
    let second = Path.Build.relative second_dir "target" in
    let second_rule = file_rule [ second ] in
    let first_body =
      Rules.of_rules
        (List.init 16 ~f:(fun i -> file_rule [ first i ".cmo"; first i ".cmi" ]))
    in
    let poison = Memo.Var.create ~name:"route-unrelated-prefix" false in
    let first_entries = ref 0 in
    let tree =
      run
        (Rules.collect_unit (fun () ->
           let open Memo.O in
           let* () =
             Rules.narrow (Target_mask.subtree first_dir) (fun () ->
               incr first_entries;
               let* poison = Memo.Var.read poison in
               if poison then Code_error.raise "Entered an unrelated prefix" [];
               Rules.produce first_body)
           in
           Rules.narrow (Target_mask.files [ second ]) (fun () ->
             Rules.Produce.rule second_rule)))
    in
    let load target = Rules.load_path_with_pending tree target in
    ignore
      (run_rule_loading_mode ~incremental:true (load (first 0 ".cmo")) : Rules.loaded);
    ignore (run_rule_loading_mode ~incremental (load (first 0 ".cmi")) : Rules.loaded);
    Memo.reset (Memo.Var.set poison true);
    let selected = run_rule_loading_mode ~incremental (load second) in
    printfn
      "%s: a disjoint target bypasses the poisoned prefix: %b"
      mode
      (!first_entries = 1
       && List.equal ( == ) (rules_in ~dir:second_dir selected.selected) [ second_rule ]
       && Rules.Pending.mem_file selected.pending (first 0 ".cmi")
       && Rules.Dir_rules.is_empty (Rules.Revealed.find selected.revealed ~dir:first_dir)
      ));
  [%expect
    {|
    batch: a disjoint target bypasses the poisoned prefix: true
    watch: a disjoint target bypasses the poisoned prefix: true
    |}]
;;

let%expect_test "bulk file restriction keeps exact validation and error order" =
  let parent = path "default/bulk-restriction" in
  let dir = Path.Build.relative parent "child" in
  let output name = Path.Build.relative dir name in
  let files = List.map [ "b.ml"; "z.ml"; "a.ml"; "c.ml"; "d.ml" ] ~f:output in
  let direct =
    Rules.of_rules
      (file_rule [ output "z.ml"; output "b.ml" ]
       :: List.map [ "a.ml"; "c.ml"; "d.ml" ] ~f:(fun name -> file_rule [ output name ]))
  in
  let forced = ref false in
  let child =
    run
      (Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           forced := true;
           Code_error.raise "Restriction must not force a child producer" [])))
  in
  let tree = Rules.union direct child in
  let broad =
    Target_mask.union
      (Target_mask.subtree parent)
      (Target_mask.subtree (path "default/elsewhere"))
  in
  printfn
    "broad coverage preserves the tree without forcing children: %b"
    (Rules.restrict tree broad == tree && not !forced);
  List.iter
    [ "finite", Target_mask.files files
    ; ( "extension"
      , Target_mask.file_extensions
          ~dir
          (Filename.Extension.Set.singleton Filename.Extension.ml) )
    ; "directory-wide", Target_mask.files_in_directory dir
    ]
    ~f:(fun (label, mask) ->
      printfn
        "%s coverage preserves the direct rules: %b"
        label
        (Rules.restrict direct mask == direct));
  let reject_target label target tree mask =
    try
      ignore (Rules.restrict tree mask : Rules.t);
      printfn "%s: false" label
    with
    | Code_error.E { message; data; _ } ->
      printfn
        "%s: %b"
        label
        (String.equal message "Rule stage produced a target outside its mask"
         && String.equal
              (Dyn.to_string (Dyn.Record data))
              (Dyn.to_string (Dyn.Record [ "target", Path.Build.to_dyn target ])))
  in
  reject_target
    "fallback keeps rule order and sorted filename order"
    (output "b.ml")
    direct
    (Target_mask.files (List.map [ "a.ml"; "c.ml"; "d.ml" ] ~f:output));
  let directory_rule =
    rule
      (Targets.create
         ~files:(Path.Build.Set.singleton (output "directory-owner.ml"))
         ~dirs:(Path.Build.Set.of_list [ output "z-directory"; output "a-directory" ]))
  in
  reject_target
    "file coverage does not admit directory outputs"
    (output "a-directory")
    (Rules.union direct (Rules.of_rules [ directory_rule ]))
    (Target_mask.files_in_directory dir);
  reject_target
    "fallback checks files before directories"
    (output "directory-owner.ml")
    (Rules.of_rules
       (directory_rule
        :: List.map [ "a.ml"; "c.ml"; "d.ml" ] ~f:(fun name -> file_rule [ output name ])
       ))
    (Target_mask.files files);
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  let alias_rules =
    run
      (Rules.collect_unit (fun () ->
         Rules.Produce.Alias.add_deps alias (Action_builder.return ())))
  in
  expect_code_error "file coverage does not admit aliases" (fun () ->
    ignore
      (Rules.restrict
         (Rules.union direct alias_rules)
         (Target_mask.files_in_directory dir)
       : Rules.t));
  let elsewhere = path "default/regrouped-rules" in
  let regrouped =
    Rules.of_dir_rules ~dir:elsewhere (Rules.find direct (Path.build dir))
  in
  reject_target
    "coverage uses the actual rule target root"
    (output "b.ml")
    regrouped
    (Target_mask.subtree elsewhere);
  [%expect
    {|
    broad coverage preserves the tree without forcing children: true
    finite coverage preserves the direct rules: true
    extension coverage preserves the direct rules: true
    directory-wide coverage preserves the direct rules: true
    fallback keeps rule order and sorted filename order: true
    file coverage does not admit directory outputs: true
    fallback checks files before directories: true
    file coverage does not admit aliases: code error
    coverage uses the actual rule target root: true
    |}]
;;

let%expect_test "ready direct directory proofs remain local and conservative" =
  let parent = path "default/ready-direct-directory-proof" in
  let clean = Path.Build.relative parent "clean" in
  let shared = Path.Build.relative parent "shared" in
  let pending = Path.Build.relative parent "pending" in
  let outside = Path.Build.relative parent "outside" in
  let aliases = Path.Build.relative parent "aliases" in
  let dirs = [ clean; shared; pending; outside; aliases ] in
  let file dir name = Path.Build.relative dir name in
  let direct =
    Rules.of_rules
      (file_rule [ file shared "b"; file shared "c" ]
       :: List.concat_map dirs ~f:(fun dir ->
         file_rule [ file dir "a"; file dir "b" ]
         :: List.init 3 ~f:(fun i ->
           file_rule [ file dir ("padding-" ^ Int.to_string i) ])))
  in
  let alias = Alias.make (Alias.Name.of_string "b") ~dir:aliases in
  let directory_runs = ref 0 in
  let alias_runs = ref 0 in
  let body =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce direct in
         let* () =
           Rules.narrow
             (Target_mask.directories [ file pending "b" ])
             (fun () ->
                incr directory_runs;
                Code_error.raise "A pending secondary directory owner must run" [])
         in
         Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
           incr alias_runs;
           Code_error.raise "Only an alias query may force this owner" [])))
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () =
           Rules.Produce.rule
             (rule
                (Targets.create
                   ~files:Path.Build.Set.empty
                   ~dirs:(Path.Build.Set.singleton (file outside "b"))))
         in
         Rules.narrow (Target_mask.subtree parent) (fun () -> Rules.produce body)))
  in
  let load dir = Rules.load_with_pending tree (Target_mask.path (file dir "a")) in
  (* Warm an isolated directory before querying other directories in its family. *)
  let clean_view = run_rule_loading_mode ~incremental:false (load clean) in
  let shared_view = run_rule_loading_mode ~incremental:false (load shared) in
  let outside_view = run_rule_loading_mode ~incremental:false (load outside) in
  let alias_view = run_rule_loading_mode ~incremental:false (load aliases) in
  let same =
    same_rule_loading_views
      ~dirs:(parent :: dirs)
      ~paths:
        (List.concat_map dirs ~f:(fun dir -> List.map [ "a"; "b"; "c" ] ~f:(file dir)))
  in
  printfn
    "directory-local proofs and fallbacks match ordinary views: %b"
    (List.for_all
       [ clean, clean_view
       ; shared, shared_view
       ; outside, outside_view
       ; aliases, alias_view
       ]
       ~f:(fun (dir, actual) ->
         same actual (run_rule_loading_mode ~incremental:true (load dir)))
     && List.length (rules_in ~dir:clean clean_view.selected) = 1);
  printfn
    "shared secondary outputs retain both original collections: %b"
    (List.length (rules_in ~dir:shared shared_view.selected) = 2
     && List.length (rules_in ~dir:shared (Rules.union shared_view.selected direct)) = 5);
  printfn
    "an outside same-name directory owner enters the closure: %b"
    (List.length (rules_in ~dir:outside outside_view.selected) = 2);
  printfn
    "same-name aliases remain pending and unrelated owners stay unforced: %b"
    (Rules.Pending.intersects_directory alias_view.pending aliases
     && List.length (rules_in ~dir:aliases alias_view.selected) = 1
     && !directory_runs = 0
     && !alias_runs = 0);
  expect_code_error "a pending secondary directory owner is still required" (fun () ->
    ignore (run_rule_loading_mode ~incremental:false (load pending) : Rules.loaded));
  expect_code_error "an explicit alias request still runs its producer" (fun () ->
    ignore
      (run_rule_loading_mode
         ~incremental:false
         (Rules.load_with_pending tree (Target_mask.aliases [ alias ]))
       : Rules.loaded));
  printfn
    "only the explicitly required competitors ran: %b"
    (!directory_runs = 1 && !alias_runs = 1);
  [%expect
    {|
    directory-local proofs and fallbacks match ordinary views: true
    shared secondary outputs retain both original collections: true
    an outside same-name directory owner enters the closure: true
    same-name aliases remain pending and unrelated owners stay unforced: true
    a pending secondary directory owner is still required: code error
    an explicit alias request still runs its producer: code error
    only the explicitly required competitors ran: true
    |}]
;;

let%expect_test "empty generated rule records preserve sharing and metadata errors" =
  let module Generated = Build_config.Gen_rules.Rules in
  let module Subdirs = Build_config.Gen_rules.Build_only_sub_dirs in
  let dir = path "default/empty-generated-union" in
  let target name = Path.Build.relative dir name in
  let calls = ref 0 in
  let make name =
    Generated.create
      ~build_dir_only_sub_dirs:
        (Subdirs.singleton ~dir (Subdir_set.of_list [ Filename.of_string_exn name ]))
      ~directory_targets:(Path.Build.Map.singleton (target name) Loc.none)
      (Rules.collect_unit (fun () ->
         incr calls;
         Rules.Produce.rule
           (rule
              (Targets.create
                 ~files:(Path.Build.Set.singleton (target (name ^ ".txt")))
                 ~dirs:(Path.Build.Set.singleton (target name))))))
  in
  let a = make "a" in
  let b = make "b" in
  let left = Generated.combine_exn Generated.empty a in
  let right = Generated.combine_exn a Generated.empty in
  let both = Generated.combine_exn a b in
  printfn "construction remains lazy: %b" (!calls = 0);
  printfn "empty records forward the original wrapper: %b" (left == a && right == a);
  let subdirs = Subdirs.find both.build_dir_only_sub_dirs dir in
  printfn
    "nonempty metadata is merged: %b"
    (List.for_all [ "a"; "b" ] ~f:(fun name ->
       Subdir_set.mem subdirs (Filename.of_string_exn name)
       && Path.Build.Map.mem both.directory_targets (target name)));
  expect_code_error "duplicate directory declarations are rejected" (fun () ->
    ignore (Generated.combine_exn a a : Generated.t));
  let declarations = { Generated.empty with directory_targets = a.directory_targets } in
  expect_code_error "an empty rule body does not discard declarations" (fun () ->
    ignore (Generated.combine_exn declarations a : Generated.t));
  printfn "metadata failures precede generation: %b" (!calls = 0);
  let first, second =
    run
      (let open Memo.O in
       let+ first = both.rules
       and+ second = both.rules in
       first, second)
  in
  let left = run left.rules in
  let right = run right.rules in
  printfn
    "two nonempty operands retain their shared result: %b"
    (first == second && !calls = 2);
  printfn
    "forwarded and combined views retain original collection IDs: %b"
    (List.length (rules_in ~dir (Rules.union first (Rules.union left right))) = 2);
  [%expect
    {|
    construction remains lazy: true
    empty records forward the original wrapper: true
    nonempty metadata is merged: true
    duplicate directory declarations are rejected: code error
    an empty rule body does not discard declarations: code error
    metadata failures precede generation: true
    two nonempty operands retain their shared result: true
    forwarded and combined views retain original collection IDs: true
    |}]
;;

let%expect_test "retention compares shared components without equating rule values" =
  let dir = path "default/retention-components" in
  let target name = Path.Build.relative dir name in
  let shared_rule = file_rule [ target "generated" ] in
  let direct = Rules.of_rules [ shared_rule ] in
  let first = Rules.Revealed.of_rules direct in
  let second = Rules.Revealed.of_rules direct in
  printfn
    "fresh wrappers around the same direct chunks match: %b"
    (first != second && Rules.Revealed.same_components first second);
  printfn
    "the same rule in a different collection does not match: %b"
    (not
       (Rules.Revealed.same_components
          first
          (Rules.Revealed.of_rules (Rules.of_rules [ shared_rule ]))));
  let alias_dir = target "child" in
  let alias = Alias.make (Alias.Name.of_string "pending") ~dir:alias_dir in
  let alias_mask = Target_mask.aliases [ alias ] in
  let alias_runs = ref 0 in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.produce direct in
         Rules.narrow alias_mask (fun () ->
           incr alias_runs;
           Memo.return ())))
  in
  let load tree name =
    run_rule_loading_mode
      ~incremental:true
      (Rules.load_with_pending tree (Target_mask.path (target name)))
  in
  let a = load tree "source-a" in
  let b = load tree "source-b" in
  printfn
    "fresh generic views share both component sequences: %b"
    (a.revealed != b.revealed
     && a.pending != b.pending
     && Rules.Revealed.same_components a.revealed b.revealed
     && Rules.Pending.same_components a.pending b.pending);
  ignore (Rules.Pending.alias_directories a.pending ~dir);
  ignore (Rules.Pending.alias_directories b.pending ~dir:alias_dir);
  printfn
    "changing an alias projection cache preserves component identity: %b"
    (Rules.Pending.same_components a.pending b.pending);
  printfn
    "a fresh summary of the same mask does not match: %b"
    (not (Rules.Pending.same_components a.pending (Rules.Pending.of_mask alias_mask)));
  let file_runs = ref 0 in
  let make_files () =
    run
      (Rules.collect_unit (fun () ->
         Memo.parallel_iter (List.init 16 ~f:Fun.id) ~f:(fun i ->
           Rules.narrow
             (Target_mask.files [ target (sprintf "file-%d" i) ])
             (fun () ->
                incr file_runs;
                Memo.return ()))))
  in
  let files = make_files () in
  let files_a = load files "source-a" in
  let files_b = load files "source-b" in
  printfn
    "fresh file frontiers share postings and empty exclusions: %b"
    (files_a.pending != files_b.pending
     && Rules.Pending.same_components files_a.pending files_b.pending);
  printfn
    "different postings for the same names do not match: %b"
    (not
       (Rules.Pending.same_components
          files_a.pending
          (load (make_files ()) "source-a").pending));
  printfn "unmatched producers remain unforced: %b" (!alias_runs = 0 && !file_runs = 0);
  let consumed_a = load files "file-0" in
  let consumed_b = load files "file-1" in
  printfn
    "different file exclusions do not match: %b"
    ((not (Rules.Pending.same_components files_a.pending consumed_a.pending))
     && not (Rules.Pending.same_components consumed_a.pending consumed_b.pending));
  printfn
    "different pending kinds do not match: %b"
    (not (Rules.Pending.same_components a.pending files_a.pending));
  printfn "only requested producers ran: %b" (!alias_runs = 0 && !file_runs = 2);
  [%expect
    {|
    fresh wrappers around the same direct chunks match: true
    the same rule in a different collection does not match: true
    fresh generic views share both component sequences: true
    changing an alias projection cache preserves component identity: true
    a fresh summary of the same mask does not match: true
    fresh file frontiers share postings and empty exclusions: true
    different postings for the same names do not match: true
    unmatched producers remain unforced: true
    different file exclusions do not match: true
    different pending kinds do not match: true
    only requested producers ran: true
    |}]
;;

let%expect_test "batch discovery misses preserve nested ownership and epoch dependencies" =
  let dir = path "default/discovery-miss" in
  let file = Path.Build.relative dir in
  let missing = file "missing" in
  let file_owner = file "pending-file" in
  let directory_owner = file "pending-directory" in
  let alias_dir = file "pending-alias" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir:alias_dir in
  let markers = List.map [ "root"; "middle"; "leaf" ] ~f:file in
  let revealed_directory = file "revealed-directory" in
  let state = Memo.Var.create ~name:"discovery-miss-state" 0 in
  let unrelated = Memo.Var.create ~name:"discovery-miss-unrelated" false in
  let parent_runs = ref 0 in
  let leaf_runs = ref 0 in
  let sibling_runs = ref 0 in
  let pending mask =
    Rules.narrow mask (fun () ->
      incr sibling_runs;
      Code_error.raise "Unrelated ownership must remain pending" [])
  in
  let leaf =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ file "leaf" ]) in
         let* () =
           Rules.Produce.rule
             (rule
                (Targets.create
                   ~files:Path.Build.Set.empty
                   ~dirs:(Path.Build.Set.singleton revealed_directory)))
         in
         pending (Target_mask.files [ file_owner ])))
  in
  let atomic = file_rule [ missing; file "missing.info" ] in
  let positive = Rules.union leaf (Rules.of_rules [ atomic ]) in
  let middle =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ file "middle" ]) in
         let* () = pending (Target_mask.directories [ directory_owner ]) in
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           incr leaf_runs;
           let* state = Memo.Var.read state in
           match state with
           | 0 -> Rules.produce leaf
           | 1 -> Rules.produce positive
           | _ -> Code_error.raise "A previously missing producer failed" [])))
  in
  let tree =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (file_rule [ file "root" ]) in
         let* () = pending (Target_mask.aliases [ alias ]) in
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           incr parent_runs;
           Rules.produce middle)))
  in
  let expression =
    with_batch_rule_loading ~f:(fun () -> Rules.load_path_with_pending tree missing)
  in
  let consumer_runs = ref 0 in
  let consumer =
    Memo.lazy_ ~name:"discovery-miss-consumer" (fun () ->
      incr consumer_runs;
      expression)
  in
  let watch () = run_rule_loading_mode ~incremental:true (Memo.Lazy.force consumer) in
  let batch () = run_rule_loading_mode ~incremental:false expression in
  let same =
    same_rule_loading_views
      ~dirs:[ dir; alias_dir ]
      ~paths:
        (missing
         :: file_owner
         :: directory_owner
         :: alias_dir
         :: revealed_directory
         :: markers)
  in
  let oracle = watch () in
  let actual = batch () in
  let files, _ = Rules.Revealed.target_names actual.revealed ~dir in
  printfn
    "a complete miss retains ancestor metadata and refinements: %b"
    (same actual oracle
     && List.is_empty (rules_in ~dir actual.selected)
     && List.length actual.refinements = 2
     && List.for_all markers ~f:(fun path ->
       Filename.Set.mem files (Path.Build.basename path))
     && Path.Build.Map.mem
          (Rules.Revealed.directory_targets actual.revealed)
          revealed_directory);
  printfn
    "file, directory and alias siblings remain pending and unforced: %b"
    (Rules.Pending.mem_file actual.pending file_owner
     && Rules.Pending.mem_directory actual.pending directory_owner
     && Rules.Pending.intersects_directory actual.pending alias_dir
     && (not (Rules.Pending.mem_file actual.pending missing))
     && !sibling_runs = 0
     && !parent_runs = 1
     && !leaf_runs = 1);
  Memo.reset (Memo.Var.set unrelated true);
  let restored = watch () in
  printfn
    "an unchanged epoch restores watch and rebuilds an equivalent batch view: %b"
    (same restored (batch ()) && !consumer_runs = 1 && !leaf_runs = 1);
  Memo.reset (Memo.Var.set state 1);
  let available = watch () in
  printfn
    "a newly available atomic output is not hidden by the old miss: %b"
    (same available (batch ())
     && List.equal ( == ) (rules_in ~dir available.selected) [ atomic ]
     && !consumer_runs = 2
     && !leaf_runs = 2);
  Memo.reset (Memo.Var.set state 2);
  expect_code_error "watch preserves the original producer failure" (fun () ->
    ignore (watch () : Rules.loaded));
  expect_code_error "batch preserves the original producer failure" (fun () ->
    ignore (batch () : Rules.loaded));
  Memo.reset (Memo.Var.set state 0);
  let recovered = watch () in
  printfn
    "a later miss recovers all original pending ownership: %b"
    (same recovered oracle && same recovered (batch ()));
  printfn
    "only the changed producer reruns across epochs: %b"
    (!consumer_runs = 4 && !leaf_runs = 4 && !parent_runs = 1 && !sibling_runs = 0);
  [%expect
    {|
    a complete miss retains ancestor metadata and refinements: true
    file, directory and alias siblings remain pending and unforced: true
    an unchanged epoch restores watch and rebuilds an equivalent batch view: true
    a newly available atomic output is not hidden by the old miss: true
    watch preserves the original producer failure: code error
    batch preserves the original producer failure: code error
    a later miss recovers all original pending ownership: true
    only the changed producer reruns across epochs: true
    |}]
;;

let%expect_test "direct unions preserve collection IDs and alias order" =
  let dir = path "default/singleton-union" in
  let target name = Path.Build.relative dir name in
  let get rules = Rules.find rules (Path.build dir) in
  let shared_rule = file_rule [ target "shared" ] in
  let shared = get (Rules.of_rules [ shared_rule ]) in
  let padding_rules =
    List.init 24 ~f:(fun i -> file_rule [ target ("padding-" ^ Int.to_string i) ])
  in
  let padding = get (Rules.of_rules padding_rules) in
  let duplicate = get (Rules.of_rules [ shared_rule ]) in
  let last_rule = file_rule [ target "last" ] in
  let last = get (Rules.of_rules [ last_rule ]) in
  let alias_name = Alias.Name.of_string "check" in
  let alias = Alias.make alias_name ~dir in
  let alias_chunk name =
    let loc = Loc.in_file (Path.of_string name) in
    let rules =
      run
        (Rules.collect_unit (fun () ->
           Rules.Produce.Alias.add_deps alias ~loc (Action_builder.return ())))
    in
    get rules, loc
  in
  let first_alias, first_loc = alias_chunk "first" in
  let second_alias, second_loc = alias_chunk "second" in
  let third_alias, third_loc = alias_chunk "third" in
  let chunks =
    [ padding; first_alias; shared; duplicate; second_alias; last; third_alias ]
  in
  let combine chunks =
    List.fold_left chunks ~init:Rules.Dir_rules.empty ~f:Rules.Dir_rules.union
  in
  let forward = combine chunks in
  let reverse = combine (List.rev chunks) in
  let reinserted =
    List.fold_left
      [ shared; second_alias; last; padding ]
      ~init:forward
      ~f:Rules.Dir_rules.union
  in
  let expected_rules = shared_rule :: (padding_rules @ [ shared_rule; last_rule ]) in
  let views = List.map [ forward; reverse; reinserted ] ~f:Rules.Dir_rules.consume in
  printfn
    "both operand orders and reinsertion preserve distinct collection IDs: %b"
    (List.for_all views ~f:(fun { Rules.Dir_rules.rules; _ } ->
       List.equal ( == ) rules expected_rules));
  printfn
    "aliases retain each original expansion exactly once and in order: %b"
    (List.for_all views ~f:(fun { Rules.Dir_rules.aliases; _ } ->
       let { Rules.Dir_rules.Alias_spec.expansions } =
         Alias.Name.Map.find aliases alias_name |> Option.value_exn
       in
       let locations = Appendable_list.to_list expansions |> List.map ~f:fst in
       List.equal ( == ) locations [ third_loc; second_loc; first_loc ]));
  printfn
    "empty and identical operands retain the original map: %b"
    (Rules.Dir_rules.union Rules.Dir_rules.empty forward == forward
     && Rules.Dir_rules.union forward Rules.Dir_rules.empty == forward
     && Rules.Dir_rules.union forward forward == forward);
  [%expect
    {|
    both operand orders and reinsertion preserve distinct collection IDs: true
    aliases retain each original expansion exactly once and in order: true
    empty and identical operands retain the original map: true
    |}]
;;
