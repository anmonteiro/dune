open Stdune
open Dune_engine
module Gen_rules = Build_config.Gen_rules

let () = Dune_tests_common.init ()

let run = function
  | None -> Rules.empty
  | Some rules ->
    Fiber.run (Memo.run rules) ~iter:(fun () ->
      Code_error.raise "Unexpected suspension in generated rules test" [])
;;

let%expect_test "empty generated rule combinations remain deferred" =
  let calls = ref 0 in
  let effectful =
    Gen_rules.make
      (Memo.of_thunk (fun () ->
         incr calls;
         Memo.return Rules.empty))
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
        | Unknown_context | Redirect_to_parent _ ->
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

let%expect_test "empty contributions merge metadata without dropping effects" =
  let dir = Path.Build.relative Path.Build.root "default/generated" in
  let declaration subdir target =
    let target = Path.Build.relative dir target in
    { Gen_rules.Rules.empty with
      build_dir_only_sub_dirs =
        Gen_rules.Build_only_sub_dirs.singleton
          ~dir
          (Subdir_set.of_list [ Filename.of_string_exn subdir ])
    ; directory_targets =
        Path.Build.Map.singleton target (Loc.in_file (Path.build target))
    }
  in
  let bin = declaration ".bin" "left" in
  let utop = declaration ".utop" "right" in
  let calls = ref 0 in
  let deferred =
    Gen_rules.Rules.create
      (Memo.of_thunk (fun () ->
         incr calls;
         Memo.return Rules.empty))
  in
  let effectful_utop = { utop with rules = deferred.rules } in
  List.iter
    [ "both empty", bin, utop
    ; "empty left", bin, effectful_utop
    ; "empty right", effectful_utop, bin
    ]
    ~f:(fun (name, left, right) ->
      calls := 0;
      let { Gen_rules.Rules.build_dir_only_sub_dirs; directory_targets; rules } =
        match Gen_rules.combine (Rules left) (Rules right) with
        | Rules rules -> rules
        | Unknown_context | Redirect_to_parent _ ->
          Code_error.raise "Expected a normal generated-rules result" []
      in
      let deferred = !calls = 0 in
      let (_ : Rules.t) = run rules in
      let subdirs =
        Path.Build.Map.find build_dir_only_sub_dirs dir
        |> Option.value ~default:Subdir_set.empty
      in
      let allowed name = Subdir_set.mem subdirs (Filename.of_string_exn name) in
      let targets_preserved =
        Path.Build.Map.equal
          directory_targets
          (Path.Build.Map.union_exn bin.directory_targets utop.directory_targets)
          ~equal:Loc.equal
      in
      printfn
        "%s: deferred=%b calls=%d bin=%b utop=%b other=%b targets=%b"
        name
        deferred
        !calls
        (allowed ".bin")
        (allowed ".utop")
        (allowed "other")
        targets_preserved);
  (match Gen_rules.combine (Rules bin) (Rules bin) with
   | _ -> print_endline "duplicate target accepted"
   | exception Code_error.E _ -> print_endline "duplicate target rejected");
  [%expect
    {|
    both empty: deferred=true calls=0 bin=true utop=true other=false targets=true
    empty left: deferred=true calls=1 bin=true utop=true other=false targets=true
    empty right: deferred=true calls=1 bin=true utop=true other=false targets=true
    duplicate target rejected
    |}]
;;
