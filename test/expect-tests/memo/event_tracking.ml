(* Tests for tracking Memo node events. *)

open! Stdune
open! Memo.O
open Test_helpers.Make ()

let%expect_test _ =
  let divisor = Memo.Var.create ~name:"divisor" 2 in
  let table =
    Memo.create_rec
      "division via subtraction"
      ~input:(module Int)
      ~cutoff:Int.equal
      ~on_event:(fun input event ->
        let event =
          match (event : Memo.Event.t) with
          | Live -> "Live"
          | Validated -> "Validated"
        in
        printf "[on_event %d %s] called\n" input event)
      (fun f input ->
         let* divisor = Memo.Var.read divisor in
         if divisor < 0 then failwith "Negative divisors are not allowed!";
         if input < divisor
         then Memo.return 0
         else if input = divisor
         then Memo.return 1
         else
           let+ result = f (input - divisor) in
           result + 1)
  in
  List.iter [ 4; 5; 6; 4; 5; 6 ] ~f:(evaluate_and_print table);
  (* Nodes {1..6} become live; [Live] then [Validated] fire once for each live node. *)
  [%expect
    {|
    [on_event 4 Live] called
    [on_event 2 Live] called
    [on_event 2 Validated] called
    [on_event 4 Validated] called
    f 4 = Ok 2
    [on_event 5 Live] called
    [on_event 3 Live] called
    [on_event 1 Live] called
    [on_event 1 Validated] called
    [on_event 3 Validated] called
    [on_event 5 Validated] called
    f 5 = Ok 2
    [on_event 6 Live] called
    [on_event 6 Validated] called
    f 6 = Ok 3
    f 4 = Ok 2
    f 5 = Ok 2
    f 6 = Ok 3
    |}];
  Memo.reset (Memo.Var.set divisor 3);
  List.iter [ 3; 6; 9 ] ~f:(evaluate_and_print table);
  [%expect
    {|
    [on_event 3 Live] called
    [on_event 3 Validated] called
    f 3 = Ok 1
    [on_event 6 Live] called
    [on_event 6 Validated] called
    f 6 = Ok 2
    [on_event 9 Live] called
    [on_event 9 Validated] called
    f 9 = Ok 3
    |}];
  Memo.reset (Memo.Var.set divisor (-5));
  List.iter [ 8; 9; 9 ] ~f:(evaluate_and_print table);
  (* Liveness tracking works for nodes whose outputs are errors. *)
  [%expect
    {|
    [on_event 8 Live] called
    [on_event 8 Validated] called
    f 8 = Error
            [ { exn = "Failure(\"Negative divisors are not allowed!\")"
              ; backtrace = ""
              }
            ]
    [on_event 9 Live] called
    [on_event 9 Validated] called
    f 9 = Error
            [ { exn = "Failure(\"Negative divisors are not allowed!\")"
              ; backtrace = ""
              }
            ]
    f 9 = Error
            [ { exn = "Failure(\"Negative divisors are not allowed!\")"
              ; backtrace = ""
              }
            ]
    |}];
  Memo.reset (Memo.Var.set divisor 0);
  List.iter [ 9; 10; 9 ] ~f:(evaluate_and_print table);
  (* Liveness tracking works with dependency cycles too. *)
  [%expect
    {|
    [on_event 9 Live] called
    [on_event 9 Validated] called
    Dependency cycle detected:
    - ("division via subtraction", 9)
    f 9 = Error
            [ { exn = "Cycle_error.E [ (\"division via subtraction\", 9) ]"
              ; backtrace = ""
              }
            ]
    [on_event 10 Live] called
    [on_event 10 Validated] called
    Dependency cycle detected:
    - ("division via subtraction", 10)
    f 10 = Error
             [ { exn = "Cycle_error.E [ (\"division via subtraction\", 10) ]"
               ; backtrace = ""
               }
             ]
    Dependency cycle detected:
    - ("division via subtraction", 9)
    f 9 = Error
            [ { exn = "Cycle_error.E [ (\"division via subtraction\", 9) ]"
              ; backtrace = ""
              }
            ]
    |}]
;;

let%expect_test "cold Live notification failures remain retryable" =
  let fail_live = ref true in
  let live = ref 0 in
  let validated = ref 0 in
  let computations = ref 0 in
  let failure = Failure "Live notification failure" in
  let table =
    Memo.create
      "cold notifications"
      ~input:(module Unit)
      ~on_event:(fun () (event : Memo.Event.t) ->
        match event with
        | Live ->
          incr live;
          if !fail_live then raise failure
        | Validated -> incr validated)
      (fun () ->
         incr computations;
         Memo.return 7)
  in
  let check () =
    let result = Scheduler.run (run_collect_errors (fun () -> Memo.exec table ())) in
    let status =
      match result with
      | Ok value ->
        assert (Int.equal value 7);
        "ok"
      | Error [ { Exn_with_backtrace.exn; _ } ] ->
        assert (exn == failure);
        "caller caught failure"
      | Error _ -> Code_error.raise "Expected one notification failure" []
    in
    printfn
      "%s: live %d, validated %d, computations %d"
      status
      !live
      !validated
      !computations
  in
  check ();
  fail_live := false;
  check ();
  check ();
  Memo.reset (Memo.Invalidation.invalidate_table ~reason:Test table);
  fail_live := true;
  check ();
  fail_live := false;
  check ();
  check ();
  [%expect
    {|
    caller caught failure: live 1, validated 0, computations 0
    ok: live 2, validated 1, computations 1
    ok: live 2, validated 1, computations 1
    caller caught failure: live 3, validated 1, computations 1
    ok: live 4, validated 2, computations 2
    ok: live 4, validated 2, computations 2
    |}]
;;

let%expect_test "success replay runs once per run, including restoration" =
  let offset = Memo.Var.create ~name:"replay offset" 10 in
  let dependency =
    Memo.lazy_ ~name:"replay dependency" ~cutoff:Int.equal (fun () ->
      let* _ = Memo.current_run () in
      let* () = Memo.of_reproducible_fiber (Fiber.of_thunk Scheduler.yield) in
      Memo.Var.read offset)
  in
  let computations = ref 0 in
  let consumers = ref 0 in
  let replays = ref 0 in
  let table =
    Memo.create_with_replay
      "replayed computation"
      ~input:(module Int)
      ~cutoff:Int.equal
      ~replay:(fun input output ->
        incr replays;
        printfn "replay %d -> %d" input output)
      (fun input ->
         incr computations;
         let+ offset = Memo.Lazy.force dependency in
         input + offset)
  in
  let consumer =
    Memo.create
      "replay consumer"
      ~input:(module Int)
      (fun input ->
         incr consumers;
         Memo.exec table input)
  in
  let check () =
    let left, right =
      run
        (Memo.fork_and_join
           (fun () -> Memo.exec consumer 3)
           (fun () -> Memo.exec table 3))
    in
    let again = run (Memo.exec table 3) in
    printfn "results: %d, %d, %d" left right again;
    printfn
      "computations: %d; consumers: %d; replays: %d"
      !computations
      !consumers
      !replays
  in
  check ();
  check ();
  Memo.reset Memo.Invalidation.empty;
  check ();
  Memo.reset (Memo.Var.set offset 20);
  check ();
  [%expect
    {|
    replay 3 -> 13
    results: 13, 13, 13
    computations: 1; consumers: 1; replays: 1
    results: 13, 13, 13
    computations: 1; consumers: 1; replays: 1
    replay 3 -> 13
    results: 13, 13, 13
    computations: 1; consumers: 1; replays: 2
    replay 3 -> 23
    results: 23, 23, 23
    computations: 2; consumers: 2; replays: 3
    |}]
;;

let%expect_test "replay failures never publish a successful result" =
  let fail_replay = ref true in
  let computations = ref 0 in
  let replays = ref 0 in
  let observed = ref 0 in
  let table =
    Memo.create_with_replay
      "failing replay"
      ~input:(module Int)
      ~cutoff:Int.equal
      ~replay:(fun _input _output ->
        incr replays;
        if !fail_replay then failwith "replay failure")
      (fun input ->
         incr computations;
         let+ () = Memo.of_reproducible_fiber (Fiber.of_thunk Scheduler.yield) in
         input)
  in
  let consumer =
    Memo.create
      "failing replay consumer"
      ~input:(module Unit)
      (fun () -> Memo.exec table 5)
  in
  let check () =
    let result =
      Scheduler.run
        (run_collect_errors (fun () ->
           Memo.fork_and_join
             (fun () ->
                let+ value = Memo.exec consumer () in
                incr observed;
                value)
             (fun () ->
                let+ value = Memo.exec table 5 in
                incr observed;
                value)))
    in
    (match result with
     | Ok (left, right) -> printfn "results: %d, %d" left right
     | Error errors ->
       assert (not (List.is_empty errors));
       assert (
         List.for_all errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
           match exn with
           | Failure message -> String.equal message "replay failure"
           | _ -> false));
       printfn "replay failed");
    printfn
      "computations: %d; replays: %d; successful reads: %d"
      !computations
      !replays
      !observed
  in
  check ();
  fail_replay := false;
  check ();
  Memo.reset_if_necessary Memo.Invalidation.empty;
  check ();
  (* The next failure happens while restoring a previously successful value. *)
  fail_replay := true;
  Memo.reset Memo.Invalidation.empty;
  check ();
  fail_replay := false;
  check ();
  Memo.reset_if_necessary Memo.Invalidation.empty;
  check ();
  [%expect
    {|
    replay failed
    computations: 1; replays: 1; successful reads: 0
    replay failed
    computations: 1; replays: 1; successful reads: 0
    results: 5, 5
    computations: 2; replays: 2; successful reads: 2
    replay failed
    computations: 2; replays: 3; successful reads: 2
    replay failed
    computations: 2; replays: 3; successful reads: 2
    results: 5, 5
    computations: 3; replays: 4; successful reads: 4
    |}]
;;

let%expect_test "computation and cycle errors do not replay success" =
  let self = Fdecl.create (fun _ -> Dyn.Opaque) in
  let replays = ref 0 in
  let table =
    Memo.create_with_replay
      "replay errors"
      ~input:(module Int)
      ~cutoff:Int.equal
      ~replay:(fun _input _output -> incr replays)
      (fun input ->
         if input = 0
         then failwith "computation failure"
         else Memo.exec (Fdecl.get self) input)
  in
  Fdecl.set self table;
  let check input =
    let result = Scheduler.run (run_collect_errors (fun () -> Memo.exec table input)) in
    (match result with
     | Ok (_ : int) -> Code_error.raise "Expected a computation error" []
     | Error errors ->
       assert (not (List.is_empty errors));
       assert (
         List.for_all errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
           match input, exn with
           | 0, Failure message -> String.equal message "computation failure"
           | 1, Memo.Cycle_error.E _ -> true
           | _ -> false)));
    printfn "input %d failed; replays: %d" input !replays
  in
  check 0;
  check 1;
  Memo.reset_if_necessary Memo.Invalidation.empty;
  check 0;
  check 1;
  [%expect
    {|
    input 0 failed; replays: 0
    input 1 failed; replays: 0
    input 0 failed; replays: 0
    input 1 failed; replays: 0
    |}]
;;

let%expect_test "restoring replay waits for early error reporting" =
  let fail_replay = ref false in
  let computations = ref 0 in
  let replays = ref 0 in
  let table =
    Memo.create_with_replay
      "yielding replay error"
      ~input:(module Int)
      ~cutoff:Int.equal
      ~replay:(fun _input _output ->
        incr replays;
        if !fail_replay then failwith "replay failure")
      (fun input ->
         incr computations;
         Memo.return input)
  in
  assert (run (Memo.exec table 7) = 7);
  fail_replay := true;
  Memo.reset Memo.Invalidation.empty;
  let reporting_started = Fiber.Ivar.create () in
  let reporting_finished = ref false in
  let early = ref 0 in
  let late = ref 0 in
  let successes = ref 0 in
  let read () =
    let open Fiber.O in
    let+ result = run_collect_errors (fun () -> Memo.exec table 7) in
    assert !reporting_finished;
    match result with
    | Ok _ -> incr successes
    | Error errors ->
      assert (not (List.is_empty errors));
      incr late;
      printfn "late error"
  in
  Scheduler.run
    (Memo.run_with_error_handler
       (fun () ->
          Memo.of_reproducible_fiber
            (let open Fiber.O in
             let* (), () =
               Fiber.fork_and_join read (fun () ->
                 let* () = Fiber.Ivar.read reporting_started in
                 printfn "second reader started";
                 read ())
             in
             fail_replay := false;
             read ()))
       ~handle_error_no_raise:(fun _exn ->
         let open Fiber.O in
         incr early;
         printfn "early reporting started";
         let* () = Fiber.Ivar.fill reporting_started () in
         let+ () = Scheduler.yield () in
         reporting_finished := true;
         printfn "early reporting finished"));
  printfn "early: %d; late: %d; successes: %d" !early !late !successes;
  printfn "computations: %d; replays: %d" !computations !replays;
  Memo.reset_if_necessary Memo.Invalidation.empty;
  printfn "recovered: %d" (run (Memo.exec table 7));
  printfn "computations: %d; replays: %d" !computations !replays;
  [%expect
    {|
    early reporting started
    second reader started
    early reporting finished
    late error
    late error
    late error
    early: 1; late: 3; successes: 0
    computations: 1; replays: 2
    recovered: 7
    computations: 2; replays: 3
    |}]
;;

let%expect_test "restoring replay preserves wrapped and cycle failures" =
  let was_recording = Printexc.backtrace_status () in
  Fun.protect
    ~finally:(fun () -> Printexc.record_backtrace was_recording)
    (fun () ->
       Printexc.record_backtrace true;
       let cycle =
         Memo.create_rec
           "replay raised cycle"
           ~input:(module Unit)
           (fun self () -> self ())
       in
       let cycle_error =
         match
           Scheduler.run
             (run_collect_errors (fun () -> (Memo.exec cycle () : unit Memo.t)))
         with
         | Error [ { exn = Memo.Cycle_error.E _ as exn; _ } ] -> exn
         | _ -> Code_error.raise "Expected a cycle to replay" []
       in
       List.iter
         [ Memo.Non_reproducible (Failure "wrapped replay failure"); cycle_error ]
         ~f:(fun failure ->
           Memo.reset Memo.Invalidation.empty;
           let expected =
             match failure with
             | Memo.Non_reproducible exn -> exn
             | exn -> exn
           in
           let fail_replay = ref false in
           let computations = ref 0 in
           let replays = ref 0 in
           let original_slot = ref None in
           let table =
             Memo.create_with_replay
               "replay error identity"
               ~input:(module Unit)
               ~cutoff:Int.equal
               ~replay:(fun () _value ->
                 incr replays;
                 if !fail_replay
                 then (
                   let error =
                     Exn_with_backtrace.try_with_never_returns (fun () -> raise failure)
                   in
                   assert (Printexc.raw_backtrace_length error.backtrace > 0);
                   original_slot
                   := Some (Printexc.get_raw_backtrace_slot error.backtrace 0);
                   Exn_with_backtrace.reraise error))
               (fun () ->
                  incr computations;
                  Memo.return 7)
           in
           let check_error { Exn_with_backtrace.exn; backtrace } =
             (match expected, exn with
              | Memo.Cycle_error.E expected, Memo.Cycle_error.E actual ->
                let frames = Memo.Cycle_error.get in
                assert (frames expected == frames actual)
              | _ -> assert (exn == expected));
             assert (Printexc.raw_backtrace_length backtrace > 0);
             let slot = Printexc.get_raw_backtrace_slot backtrace 0 in
             assert (Some slot = !original_slot)
           in
           let check_result = function
             | Error [ error ] -> check_error error
             | _ -> Code_error.raise "Expected one replay failure" []
           in
           assert (run (Memo.exec table ()) = 7);
           fail_replay := true;
           Memo.reset Memo.Invalidation.empty;
           let result =
             Scheduler.run (run_collect_errors (fun () -> Memo.exec table ()))
           in
           check_result result;
           assert (!computations = 1 && !replays = 2);
           fail_replay := false;
           let result =
             Scheduler.run (run_collect_errors (fun () -> Memo.exec table ()))
           in
           check_result result;
           assert (!computations = 1 && !replays = 2);
           Memo.reset_if_necessary Memo.Invalidation.empty;
           assert (run (Memo.exec table ()) = 7);
           assert (!computations = 2 && !replays = 3)))
;;

let%expect_test "replay cutoffs retain fresh payloads and dependencies" =
  let use_left = Memo.Var.create ~name:"replay branch" true in
  let left = Memo.Var.create ~name:"left replay value" 1 in
  let right = Memo.Var.create ~name:"right replay value" 1 in
  let computations = ref 0 in
  let consumers = ref 0 in
  let table =
    Memo.create_with_replay
      "replay payload"
      ~input:(module Unit)
      ~cutoff:(fun (a, _) (b, _) -> Int.equal a b)
      ~replay:(fun () (value, branch) -> printfn "replay %s: %d" branch value)
      (fun () ->
         incr computations;
         let* use_left = Memo.Var.read use_left in
         let branch, variable = if use_left then "left", left else "right", right in
         let+ value = Memo.Var.read variable in
         value, branch)
  in
  let consumer =
    Memo.create
      "replay payload consumer"
      ~input:(module Unit)
      (fun () ->
         incr consumers;
         let+ value, _branch = Memo.exec table () in
         value)
  in
  let check () =
    let value = run (Memo.exec consumer ()) in
    let direct_value, branch = run (Memo.exec table ()) in
    assert (Int.equal value direct_value);
    printfn "value: %d; payload: %s" value branch;
    printfn "computations: %d; consumers: %d" !computations !consumers
  in
  check ();
  Memo.reset (Memo.Var.set use_left false);
  check ();
  Memo.reset Memo.Invalidation.empty;
  check ();
  Memo.reset (Memo.Var.set left 2);
  check ();
  Memo.reset (Memo.Var.set right 2);
  check ();
  [%expect
    {|
    replay left: 1
    value: 1; payload: left
    computations: 1; consumers: 1
    replay right: 1
    value: 1; payload: right
    computations: 2; consumers: 1
    replay right: 1
    value: 1; payload: right
    computations: 2; consumers: 1
    replay right: 1
    value: 1; payload: right
    computations: 2; consumers: 1
    replay right: 2
    value: 2; payload: right
    computations: 3; consumers: 2
    |}]
;;

let%expect_test "ordinary cutoffs keep their canonical output" =
  let revision = Memo.Var.create ~name:"ordinary cutoff revision" 0 in
  let computations = ref 0 in
  let table =
    Memo.create
      "ordinary cutoff payload"
      ~input:(module Unit)
      ~cutoff:(fun (a, _) (b, _) -> Int.equal a b)
      (fun () ->
         incr computations;
         let+ revision = Memo.Var.read revision in
         7, revision)
  in
  let first = run (Memo.exec table ()) in
  Memo.reset (Memo.Var.set revision 1);
  let second = run (Memo.exec table ()) in
  printfn "same object: %b; payload: %d" (first == second) (snd second);
  printfn "computations: %d" !computations;
  [%expect
    {|
    same object: true; payload: 0
    computations: 2
    |}]
;;

let%expect_test "equal recomputation cannot cut off a replay error" =
  let revision = Memo.Var.create ~name:"failed cutoff revision" 0 in
  let fail_replay = ref false in
  let computations = ref 0 in
  let replays = ref 0 in
  let consumers = ref 0 in
  let table =
    Memo.create_with_replay
      "failed equal replay"
      ~input:(module Unit)
      ~cutoff:(fun (a, _) (b, _) -> Int.equal a b)
      ~replay:(fun () _payload ->
        incr replays;
        if !fail_replay then failwith "equal replay failure")
      (fun () ->
         incr computations;
         let+ revision = Memo.Var.read revision in
         7, revision)
  in
  let consumer =
    Memo.create
      "failed equal replay consumer"
      ~input:(module Unit)
      (fun () ->
         incr consumers;
         let+ value, _revision = Memo.exec table () in
         value)
  in
  let check () =
    let result = Scheduler.run (run_collect_errors (fun () -> Memo.exec consumer ())) in
    (match result with
     | Ok value -> printfn "value: %d" value
     | Error errors ->
       assert (not (List.is_empty errors));
       assert (
         List.for_all errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
           match exn with
           | Failure message -> String.equal message "equal replay failure"
           | _ -> false));
       printfn "replay failed");
    printfn
      "computations: %d; replays: %d; consumers: %d"
      !computations
      !replays
      !consumers
  in
  check ();
  fail_replay := true;
  Memo.reset (Memo.Var.set revision 1);
  check ();
  fail_replay := false;
  check ();
  Memo.reset_if_necessary Memo.Invalidation.empty;
  check ();
  Memo.reset Memo.Invalidation.empty;
  check ();
  [%expect
    {|
    value: 7
    computations: 1; replays: 1; consumers: 1
    replay failed
    computations: 2; replays: 2; consumers: 2
    replay failed
    computations: 2; replays: 2; consumers: 2
    value: 7
    computations: 3; replays: 3; consumers: 3
    value: 7
    computations: 3; replays: 4; consumers: 3
    |}]
;;

let%expect_test "ready replay sequence preserves errors and waiting readers" =
  Memo.reset Memo.Invalidation.empty;
  let leaf = Memo.lazy_node ~name:"ready replay leaf" (fun () -> Memo.return ()) in
  let computations = ref 0 in
  let replays = ref 0 in
  let fail_replay = ref false in
  let table =
    Memo.create_with_replay
      "ready replay sequence"
      ~input:(module Unit)
      ~cutoff:(fun _ _ -> false)
      ~replay:(fun () _ ->
        incr replays;
        if !fail_replay then failwith "ready replay failure")
      (fun () ->
         incr computations;
         let* () = Memo.Node.read leaf in
         let* () = Memo.Node.read leaf in
         let+ () = Memo.Node.read leaf in
         ref 7)
  in
  let parent =
    Memo.lazy_node ~name:"ready replay parent" (fun () -> Memo.exec table ())
  in
  let read = Memo.Node.read parent in
  assert (!replays = 0);
  let original = run read in
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read leaf);
  Memo.Metrics.reset ();
  assert (run read == original);
  assert (run read == original);
  assert (!computations = 1 && !replays = 2);
  assert (Counter.read Memo.Metrics.Restore.nodes = 2);
  assert (Counter.read Memo.Metrics.Restore.edges = 4);
  assert (Counter.read Memo.Metrics.Compute.nodes = 0);
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read leaf);
  Memo.Metrics.reset ();
  fail_replay := true;
  let reporting_started = Fiber.Ivar.create () in
  let reporting_finished = ref false in
  let early = ref 0 in
  let late = ref 0 in
  let check () =
    let open Fiber.O in
    let+ result = run_collect_errors (fun () -> read) in
    assert !reporting_finished;
    match result with
    | Error [ { Exn_with_backtrace.exn = Failure message; _ } ] ->
      assert (String.equal message "ready replay failure");
      incr late
    | _ -> Code_error.raise "Expected one replay failure" []
  in
  let fiber =
    Memo.run_with_error_handler
      (fun () ->
         Memo.of_reproducible_fiber
           (let open Fiber.O in
            let* (), () =
              Fiber.fork_and_join check (fun () ->
                let* () = Fiber.Ivar.read reporting_started in
                check ())
            in
            fail_replay := false;
            check ()))
      ~handle_error_no_raise:(fun error ->
        (match error.Exn_with_backtrace.exn with
         | Memo.Error.E error ->
           let names =
             List.map (Memo.Error.stack error) ~f:(fun frame ->
               Option.value_exn (Memo.Stack_frame.name frame))
           in
           assert (
             List.equal
               String.equal
               names
               [ "ready replay sequence"; "ready replay parent" ])
         | _ -> Code_error.raise "Expected a named early replay failure" []);
        incr early;
        let open Fiber.O in
        let* () = Fiber.Ivar.fill reporting_started () in
        let+ () = Scheduler.yield () in
        reporting_finished := true)
  in
  assert (!replays = 2);
  Scheduler.run fiber;
  assert (!early = 1 && !late = 3);
  assert (!computations = 1 && !replays = 3);
  assert (Counter.read Memo.Metrics.Restore.nodes = 2);
  assert (Counter.read Memo.Metrics.Restore.edges = 4);
  assert (Counter.read Memo.Metrics.Restore.blocked > 0);
  Memo.reset_if_necessary Memo.Invalidation.empty;
  assert (!(run read) = 7);
  assert (!computations = 2 && !replays = 4);
  print_endline "duplicates, named error, waiting readers, and reset recovery";
  Memo.reset Memo.Invalidation.empty;
  [%expect {| duplicates, named error, waiting readers, and reset recovery |}]
;;

let%expect_test "ready replay prefixes preserve stale and changed fallbacks" =
  List.iter [ false; true ] ~f:(fun prewarm_changed ->
    Memo.reset Memo.Invalidation.empty;
    let input = Memo.Var.create ~name:"replay fallback input" 0 in
    let prefix = Memo.lazy_node ~name:"replay prefix" (fun () -> Memo.return ()) in
    let child =
      Memo.lazy_node ~name:"replay child" ~cutoff:Int.equal (fun () ->
        Memo.Var.read input)
    in
    let tail_live = ref 0 in
    let tail =
      Memo.lazy_node
        ~name:"unused replay tail"
        ~on_event:(function
          | Live -> incr tail_live
          | Validated -> ())
        (fun () -> Memo.return ())
    in
    let computations = ref 0 in
    let replays = ref 0 in
    let table =
      Memo.create_with_replay
        "replay fallback"
        ~input:(module Unit)
        ~cutoff:Int.equal
        ~replay:(fun () _ -> incr replays)
        (fun () ->
           incr computations;
           let* () = Memo.Node.read prefix in
           let* () = Memo.Node.read prefix in
           let* value = Memo.Node.read child in
           if value = 0
           then
             let+ () = Memo.Node.read tail in
             value
           else Memo.return value)
    in
    assert (run (Memo.exec table ()) = 0);
    Memo.reset (Memo.Var.set input 1);
    if prewarm_changed
    then (
      assert (run (Memo.Node.read child) = 1);
      Memo.reset Memo.Invalidation.empty);
    run (Memo.Node.read prefix);
    if prewarm_changed then assert (run (Memo.Node.read child) = 1);
    Memo.Metrics.reset ();
    assert (run (Memo.exec table ()) = 1);
    assert (!computations = 2 && !replays = 2 && !tail_live = 1);
    assert (Counter.read Memo.Metrics.Restore.nodes = if prewarm_changed then 1 else 2);
    assert (Counter.read Memo.Metrics.Restore.edges = if prewarm_changed then 3 else 4));
  Memo.reset Memo.Invalidation.empty
;;

let%expect_test "replay keeps parallel and nested dependency sections" =
  List.iter [ false; true ] ~f:(fun nested ->
    Memo.reset Memo.Invalidation.empty;
    let leaf = Memo.lazy_node ~name:"parallel replay leaf" (fun () -> Memo.return ()) in
    let computations = ref 0 in
    let replays = ref 0 in
    let read () = Memo.Node.read leaf in
    let read_twice () =
      let* () = read () in
      read ()
    in
    let table =
      Memo.create_with_replay
        "parallel replay"
        ~input:(module Unit)
        ~cutoff:Int.equal
        ~replay:(fun () _ -> incr replays)
        (fun () ->
           incr computations;
           if nested
           then
             let+ (), () = Memo.fork_and_join read_twice read_twice in
             7
           else
             let* () = read () in
             let+ (), () = Memo.fork_and_join read read in
             7)
    in
    assert (run (Memo.exec table ()) = 7);
    Memo.reset Memo.Invalidation.empty;
    run (read ());
    Memo.Metrics.reset ();
    assert (run (Memo.exec table ()) = 7);
    assert (!computations = 1 && !replays = 2);
    assert (Counter.read Memo.Metrics.Restore.nodes = 1);
    assert (Counter.read Memo.Metrics.Restore.edges = if nested then 4 else 3));
  Memo.reset Memo.Invalidation.empty
;;
