open Stdune
open Dune_engine
module Gen_rules = Build_config.Gen_rules

let () = Dune_tests_common.init ()

let run rules =
  Fiber.run (Memo.run rules) ~iter:(fun () ->
    Code_error.raise "Unexpected suspension in generated rules test" [])
;;

let%expect_test "empty generated rule combinations remain deferred" =
  let calls = ref 0 in
  let effectful =
    (* Unlike [create], this raw fixture does not memoize its computation. *)
    Gen_rules.Rules
      { Gen_rules.Rules.empty with
        rules =
          Memo.of_thunk (fun () ->
            incr calls;
            Memo.return Rules.empty)
      }
  in
  List.iter
    [ "both empty", Gen_rules.no_rules, Gen_rules.no_rules
    ; "empty left", Gen_rules.no_rules, effectful
    ; "empty right", effectful, Gen_rules.no_rules
    ; "same effectful record", effectful, effectful
    ]
    ~f:(fun (name, left, right) ->
      calls := 0;
      let rules =
        match Gen_rules.combine left right with
        | Rules { rules; _ } -> rules
        | Unknown_context ->
          Code_error.raise "Expected a normal generated-rules result" []
      in
      let deferred = !calls = 0 in
      let result = run rules in
      printfn
        "%s: deferred=%b calls=%d empty=%b"
        name
        deferred
        !calls
        (Path.Build.Map.is_empty (Rules.to_map result)));
  [%expect
    {|
    both empty: deferred=true calls=0 empty=true
    empty left: deferred=true calls=1 empty=true
    empty right: deferred=true calls=1 empty=true
    same effectful record: deferred=true calls=2 empty=true
    |}]
;;

let%expect_test "created generated rules share dependency-tracked generation" =
  let open Memo.O in
  let calls = ref 0 in
  let input = Memo.Var.create 0 ~name:"generated-rules-input" in
  let generated =
    Gen_rules.make
      (let+ (_ : int) = Memo.Var.read input in
       incr calls;
       Rules.empty)
  in
  let force label generated =
    let rules =
      match generated with
      | Gen_rules.Rules { rules; _ } -> rules
      | Unknown_context ->
        Code_error.raise "Expected a normal generated-rules result" []
    in
    ignore (run rules : Rules.t);
    printfn "%s: calls=%d" label !calls
  in
  printfn "before forcing: calls=%d" !calls;
  force "first read" generated;
  force "cached read" generated;
  let combined = Gen_rules.combine generated generated in
  force "same record combined" combined;
  Memo.reset Memo.Invalidation.empty;
  force "unchanged reset" combined;
  Memo.reset (Memo.Var.set input 1);
  force "changed input" combined;
  force "cached after change" combined;
  [%expect
    {|
    before forcing: calls=0
    first read: calls=1
    cached read: calls=1
    same record combined: calls=1
    unchanged reset: calls=1
    changed input: calls=2
    cached after change: calls=2
    |}]
;;

let%expect_test "empty generated rules retain subdirectory declarations" =
  let dir = Path.Build.relative Path.Build.root "default/generated" in
  let build_dir_only_sub_dirs =
    Build_config.Gen_rules.Build_only_sub_dirs.singleton ~dir Subdir_set.empty
  in
  let declared =
    Gen_rules.make ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  in
  List.iter
    [ "empty left", Gen_rules.no_rules, declared
    ; "empty right", declared, Gen_rules.no_rules
    ]
    ~f:(fun (name, left, right) ->
      let { Gen_rules.Rules.build_dir_only_sub_dirs; directory_targets; _ } =
        match Gen_rules.combine left right with
        | Rules rules -> rules
        | Unknown_context ->
          Code_error.raise "Expected a normal generated-rules result" []
      in
      printfn
        "%s: declaration=%b directory_targets_empty=%b"
        name
        (Path.Build.Map.mem build_dir_only_sub_dirs dir)
        (Path.Build.Map.is_empty directory_targets));
  [%expect
    {|
    empty left: declaration=true directory_targets_empty=true
    empty right: declaration=true directory_targets_empty=true
    |}]
;;
