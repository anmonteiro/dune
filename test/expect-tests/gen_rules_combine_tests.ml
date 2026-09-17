open Stdune
open Dune_engine
module Generated_rules = Build_config.Gen_rules.Rules

let () = Dune_tests_common.init ()

let run rules =
  Fiber.run (Memo.run rules) ~iter:(fun () ->
    Code_error.raise "Unexpected suspension in generated rules test" [])
;;

let%expect_test "empty generated rule combinations remain deferred" =
  let calls = ref 0 in
  let effectful =
    (* Unlike [create], this raw fixture does not memoize its computation. *)
    { Generated_rules.empty with
      rules =
        Memo.of_thunk (fun () ->
          incr calls;
          Memo.return Rules.empty)
    }
  in
  List.iter
    [ "both empty", Generated_rules.empty, Generated_rules.empty
    ; "empty left", Generated_rules.empty, effectful
    ; "empty right", effectful, Generated_rules.empty
    ; "same effectful record", effectful, effectful
    ]
    ~f:(fun (name, left, right) ->
      calls := 0;
      let { Generated_rules.rules; _ } = Generated_rules.combine_exn left right in
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
    Generated_rules.create
      (let+ (_ : int) = Memo.Var.read input in
       incr calls;
       Rules.empty)
  in
  let force label rules =
    ignore (run rules : Rules.t);
    printfn "%s: calls=%d" label !calls
  in
  printfn "before forcing: calls=%d" !calls;
  force "first read" generated.rules;
  force "cached read" generated.rules;
  let combined = Generated_rules.combine_exn generated generated in
  force "same record combined" combined.rules;
  Memo.reset Memo.Invalidation.empty;
  force "unchanged reset" combined.rules;
  Memo.reset (Memo.Var.set input 1);
  force "changed input" combined.rules;
  force "cached after change" combined.rules;
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
    Generated_rules.create ~build_dir_only_sub_dirs (Memo.return Rules.empty)
  in
  List.iter
    [ "empty left", Generated_rules.empty, declared
    ; "empty right", declared, Generated_rules.empty
    ]
    ~f:(fun (name, left, right) ->
      let { Generated_rules.build_dir_only_sub_dirs; directory_targets; _ } =
        Generated_rules.combine_exn left right
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
