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

let%expect_test "dependency-free restoration preserves values and errors" =
  Memo.reset Memo.Invalidation.empty;
  let computes = ref 0 in
  let value =
    Memo.lazy_node ~name:"leaf value" (fun () ->
      incr computes;
      Memo.return (ref 7))
  in
  let errors = ref 0 in
  let error =
    Memo.lazy_node ~name:"leaf error" (fun () ->
      incr errors;
      failwith "leaf failure")
  in
  let check_error () =
    match
      Scheduler.run (Fiber.collect_errors (fun () -> Memo.run (Memo.Node.read error)))
    with
    | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
      let names =
        List.map (Memo.Error.stack error) ~f:(fun frame ->
          Option.value_exn (Memo.Stack_frame.name frame))
      in
      printfn "stack=%s errors=%d" (String.concat ~sep:" -> " names) !errors
    | Ok _ | Error _ -> Code_error.raise "Expected a named Memo error" []
  in
  let original = run (Memo.Node.read value) in
  check_error ();
  Memo.reset Memo.Invalidation.empty;
  Memo.Metrics.reset ();
  let restored = run (Memo.Node.read value) in
  printfn "same=%b computes=%d" (original == restored) !computes;
  check_error ();
  print_metrics ();
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    stack=leaf error errors=1
    same=true computes=1
    stack=leaf error errors=1
    Memo graph: 2/0/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

let%expect_test "dependency-free restoration preserves event notifications" =
  Memo.reset Memo.Invalidation.empty;
  let live = ref 0 in
  let validated = ref 0 in
  let computes = ref 0 in
  let node =
    Memo.lazy_node
      ~name:"observed leaf"
      ~on_event:(function
        | Live -> incr live
        | Validated -> incr validated)
      (fun () ->
         incr computes;
         Memo.return ())
  in
  run (Memo.Node.read node);
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read node);
  run (Memo.Node.read node);
  printfn "live=%d validated=%d computes=%d" !live !validated !computes;
  Memo.reset Memo.Invalidation.empty;
  [%expect {| live=2 validated=2 computes=1 |}]
;;
