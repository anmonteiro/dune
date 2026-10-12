open Stdune
open Dune_rules

let () = Dune_tests_common.init ()

let expand args =
  let { Action_builder.With_targets.build; targets = _ } =
    Command.expand ~dir:Path.root args
  in
  let result, _deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps build))
      ~iter:(fun () -> assert false)
  in
  Appendable_list.to_list result
;;

let%expect_test "nested argument groups preserve order" =
  let open Command.Args in
  let args =
    S
      [ A "a"
      ; S []
      ; S [ As [ "b"; "c" ]; S [ A "d"; S []; S [ As [ "e"; "f" ] ] ] ]
      ; A "g"
      ]
  in
  List.iter (expand args) ~f:(printfn "%S");
  [%expect
    {|
    "a"
    "b"
    "c"
    "d"
    "e"
    "f"
    "g"
    |}]
;;

let%expect_test "static dependencies preserve lazy sets and eager facts" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let legacy deps = Action_builder.dyn_memo_deps (Memo.return (deps, ())) in
  let env = Dep.env (Env.Var.of_string "STATIC_DEPS_TEST") in
  let missing =
    Dep.file
      (Path.build (Path.Build.relative Path.Build.root "default/unbuilt-static-dep"))
  in
  let deps = Dep.Set.of_list [ missing; env; Dep.universe ] in
  (* No build configuration is installed, and lazy evaluation must not try to
     build the missing target. *)
  let static = Action_builder.deps deps in
  print_endline "constructed without a build configuration";
  let (), actual = run (Action_builder.evaluate_and_collect_deps static) in
  let (), previous = run (Action_builder.evaluate_and_collect_deps (legacy deps)) in
  printfn
    "lazy dependencies kept: %b"
    (Dep.Set.equal actual deps && Dep.Set.equal actual previous);
  let check_facts deps expected =
    let (), actual =
      run (Action_builder.evaluate_and_collect_facts (Action_builder.deps deps))
    in
    let (), previous = run (Action_builder.evaluate_and_collect_facts (legacy deps)) in
    Dep.Facts.equal actual expected && Dep.Facts.equal actual previous
  in
  printfn "empty eager facts kept: %b" (check_facts Dep.Set.empty Dep.Facts.empty);
  let expected =
    Dep.Facts.union
      (Dep.Facts.singleton env Dep.Fact.nothing)
      (Dep.Facts.singleton Dep.universe Dep.Fact.nothing)
  in
  printfn
    "nonempty eager facts kept: %b"
    (check_facts (Dep.Set.of_list [ env; Dep.universe ]) expected);
  [%expect
    {|
    constructed without a build configuration
    lazy dependencies kept: true
    empty eager facts kept: true
    nonempty eager facts kept: true
    |}]
;;

let%expect_test "singleton list maps preserve evaluation and dependencies" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let first, second = ref 1, ref 2 in
  let deps = Dep.Set.singleton Dep.universe in
  let facts = Dep.Facts.singleton Dep.universe Dep.Fact.nothing in
  List.iter
    [ []; [ first ]; [ first; second; first ] ]
    ~f:(fun values ->
      let calls = ref 0 in
      let fact_calls = ref 0 in
      let mapped =
        Action_builder.List.map values ~f:(fun value ->
          incr calls;
          Action_builder.record value deps ~f:(fun _ ->
            incr fact_calls;
            Memo.return Dep.Fact.nothing))
      in
      assert (!calls = 0 && !fact_calls = 0);
      let lazy_values, lazy_deps =
        run (Action_builder.evaluate_and_collect_deps mapped)
      in
      assert (List.equal ( == ) lazy_values values);
      assert (!calls = List.length values && !fact_calls = 0);
      assert (Dep.Set.equal lazy_deps (if values = [] then Dep.Set.empty else deps));
      let eager_values, eager_facts =
        run (Action_builder.evaluate_and_collect_facts mapped)
      in
      assert (List.equal ( == ) eager_values values);
      assert (!calls = 2 * List.length values && !fact_calls = List.length values);
      assert (Dep.Facts.equal eager_facts (if values = [] then Dep.Facts.empty else facts)));
  let release = Fiber.Ivar.create () in
  let calls = ref 0 in
  let suspended =
    Action_builder.List.map [ first ] ~f:(fun value ->
      incr calls;
      let open Fiber.O in
      Action_builder.of_memo
        (Memo.of_reproducible_fiber
           (let+ () = Fiber.Ivar.read release in
            value)))
  in
  assert (!calls = 0);
  let released = ref false in
  let values, deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps suspended))
      ~iter:(fun () ->
        assert (!calls = 1 && not !released);
        released := true;
        [ Fiber.Fill (release, ()) ])
  in
  assert (!released && !calls = 1);
  assert (List.equal ( == ) values [ first ] && Dep.Set.is_empty deps);
  let exception Failed in
  List.iter [ false; true ] ~f:(fun delayed ->
    let calls = ref 0 in
    let failing =
      Action_builder.List.map [ () ] ~f:(fun () ->
        incr calls;
        if delayed
        then Action_builder.of_memo (Memo.of_thunk (fun () -> raise Failed))
        else raise Failed)
    in
    assert (!calls = 0);
    let result =
      Fiber.run
        (Fiber.collect_errors (fun () ->
           Memo.run (Action_builder.evaluate_and_collect_deps failing)))
        ~iter:(fun () -> failwith "unexpected suspension")
    in
    assert (!calls = 1);
    assert (
      match result with
      | Error [ { Exn_with_backtrace.exn = Failed; _ } ] -> true
      | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
        (match Memo.Error.get error with
         | Failed -> true
         | _ -> false)
      | _ -> false));
  print_endline "singleton map invariants hold";
  [%expect {| singleton map invariants hold |}]
;;

let%expect_test "success prefixes preserve lazy evaluation, facts and invalidation" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  List.iter [ true; false ] ~f:(fun fused ->
    let prefix_value = Memo.Var.create ~name:"prefix-value" 0 in
    let action_value = Memo.Var.create ~name:"action-value" 10 in
    let prefix_calls = ref 0 in
    let prefix =
      Action_builder.record_success
        (let open Memo.O in
         let+ _ = Memo.Var.read prefix_value in
         incr prefix_calls)
    in
    let prefix = if fused then prefix else Action_builder.map prefix ~f:Fun.id in
    let deps = Dep.Set.singleton Dep.universe in
    let facts = Dep.Facts.singleton Dep.universe Dep.Fact.nothing in
    let action =
      let open Action_builder.O in
      let action =
        let* value = Action_builder.of_memo (Memo.Var.read action_value) in
        Action_builder.record value deps ~f:(fun _ -> Memo.return Dep.Fact.nothing)
      in
      prefix >>> action
    in
    let value, lazy_deps = run (Action_builder.evaluate_and_collect_deps action) in
    assert (value = 10 && Dep.Set.equal lazy_deps deps && !prefix_calls = 0);
    let evaluation =
      Memo.lazy_node ~name:"success-prefix-evaluation" (fun () ->
        Action_builder.evaluate_and_collect_facts action)
    in
    let check value calls =
      let result, actual_facts = run (Memo.Node.read evaluation) in
      assert (result = value && Dep.Facts.equal actual_facts facts);
      assert (!prefix_calls = calls)
    in
    check 10 1;
    Memo.reset (Memo.Var.set prefix_value 1);
    check 10 2;
    Memo.reset (Memo.Var.set action_value 20);
    check 20 3;
    Memo.reset Memo.Invalidation.empty;
    check 20 3);
  print_endline "lazy evaluation, facts and invalidation preserved";
  [%expect {| lazy evaluation, facts and invalidation preserved |}]
;;

let%expect_test "success prefixes collect both errors and retain their dependencies" =
  List.iter [ `Fused; `Unfused ] ~f:(fun kind ->
    let prefix_fails = Memo.Var.create ~name:"prefix-fails" true in
    let action_fails = Memo.Var.create ~name:"action-fails" true in
    let failing_node name var =
      Memo.lazy_node ~name (fun () ->
        let open Memo.O in
        let+ fails = Memo.Var.read var in
        if fails then failwith name)
    in
    let prefix_node = failing_node "prefix" prefix_fails in
    let prefix =
      match kind with
      | `Fused -> Action_builder.record_success (Memo.Node.read prefix_node)
      | `Unfused ->
        Action_builder.map
          (Action_builder.record_success (Memo.Node.read prefix_node))
          ~f:Fun.id
    in
    let action =
      let open Action_builder.O in
      let action = Memo.Node.read (failing_node "action" action_fails) in
      prefix >>> Action_builder.of_memo action
    in
    let evaluation =
      Memo.lazy_node ~name:"success-prefix-evaluation" (fun () ->
        Action_builder.evaluate_and_collect_facts action)
    in
    let check expected =
      let result =
        Fiber.run
          (Fiber.collect_errors (fun () -> Memo.run (Memo.Node.read evaluation)))
          ~iter:(fun () -> failwith "unexpected suspension")
      in
      let errors =
        match result with
        | Ok _ -> []
        | Error errors ->
          List.map errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
            match exn with
            | Memo.Error.E error ->
              assert (
                List.exists (Memo.Error.stack error) ~f:(fun frame ->
                  Memo.Stack_frame.name frame = Some "success-prefix-evaluation"));
              (match Memo.Error.get error with
               | Failure message -> message
               | exn -> raise exn)
            | exn -> raise exn)
      in
      assert (List.equal String.equal (List.sort errors ~compare:String.compare) expected)
    in
    check [ "action"; "prefix" ];
    check [ "action"; "prefix" ];
    Memo.reset (Memo.Var.set prefix_fails false);
    check [ "action" ];
    Memo.reset (Memo.Var.set action_fails false);
    check []);
  print_endline "both errors, stack frames and recovery preserved";
  [%expect {| both errors, stack frames and recovery preserved |}]
;;

let%expect_test "success prefixes let the action unblock the prefix" =
  List.iter [ `Fused; `Unfused ] ~f:(fun kind ->
    let gate = Fiber.Ivar.create () in
    let calls = ref [] in
    let record name = calls := name :: !calls in
    let memo =
      Memo.of_reproducible_fiber
        (let open Fiber.O in
         let* () =
           Fiber.of_thunk (fun () ->
             record "prefix";
             Fiber.return ())
         in
         let+ () = Fiber.Ivar.read gate in
         record "resumed")
    in
    let prefix =
      match kind with
      | `Fused -> Action_builder.record_success memo
      | `Unfused -> Action_builder.map (Action_builder.record_success memo) ~f:Fun.id
    in
    let action =
      let open Action_builder.O in
      prefix
      >>> Action_builder.of_memo
            (Memo.of_reproducible_fiber
               (Fiber.of_thunk (fun () ->
                  record "action";
                  Fiber.Ivar.fill gate ())))
    in
    let _, _ =
      Fiber.run
        (Memo.run (Action_builder.evaluate_and_collect_facts action))
        ~iter:(fun () -> failwith "action did not unblock prefix")
    in
    assert (List.equal String.equal (List.rev !calls) [ "prefix"; "action"; "resumed" ]));
  print_endline "parallel evaluation preserved";
  [%expect {| parallel evaluation preserved |}]
;;

let%expect_test "success prefixes preserve early errors outside a memoized node" =
  List.iter [ true; false ] ~f:(fun fused ->
    let gate = Fiber.Ivar.create () in
    let calls = ref [] in
    let record name = calls := name :: !calls in
    let prefix =
      Action_builder.record_success
        (Memo.of_thunk (fun () ->
           record "prefix";
           failwith "prefix"))
    in
    let prefix = if fused then prefix else Action_builder.map prefix ~f:Fun.id in
    let action =
      let open Action_builder.O in
      prefix
      >>> Action_builder.of_memo
            (Memo.of_reproducible_fiber
               (let open Fiber.O in
                let+ () = Fiber.Ivar.read gate in
                record "action"))
    in
    let result =
      Fiber.run
        (Fiber.collect_errors (fun () ->
           Memo.run_with_error_handler
             (fun () -> Action_builder.evaluate_and_collect_facts action)
             ~handle_error_no_raise:(fun _ ->
               record "reported";
               Fiber.Ivar.fill gate ())))
        ~iter:(fun () -> failwith "early error was not reported")
    in
    assert (
      match result with
      | Error [ _ ] -> true
      | Ok _ | Error _ -> false);
    assert (List.equal String.equal (List.rev !calls) [ "prefix"; "reported"; "action" ]));
  print_endline "early error reporting preserved";
  [%expect {| early error reporting preserved |}]
;;
