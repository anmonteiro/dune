open! Stdune
open Test_helpers.Make ()

let in_non_incremental_mode ~f =
  Memo.Metrics.reset ();
  Memo.set_incremental false;
  Exn.protect ~f ~finally:(fun () ->
    Memo.Metrics.reset ();
    Memo.set_incremental true)
;;

let%expect_test "non-incremental mode discards dependencies but counts them" =
  in_non_incremental_mode ~f:(fun () ->
    let dependency =
      Memo.lazy_node ~name:"dependency" (fun () ->
        Scheduler.yield () |> Memo.of_reproducible_fiber)
    in
    let node =
      Memo.lazy_node ~name:"node" (fun () ->
        Memo.parallel_iter [ dependency; dependency ] ~f:Memo.Node.read)
    in
    let () = run (Memo.Node.read node) in
    let deps =
      Memo.For_tests.get_deps_structured node |> Option.value_exn |> Dyn.to_string
    in
    printfn "dependencies: %s" deps;
    printfn "edges: %d" (Counter.read Memo.Metrics.Compute.edges);
    printfn "cycle detection edges: %d" (Counter.read Memo.Metrics.Cycle_detection.edges);
    Memo.Metrics.assert_invariants ());
  [%expect
    {|
    dependencies: Empty
    edges: 2
    cycle detection edges: 1
    |}]
;;

let%expect_test "non-incremental mode preserves early errors outside Memo nodes" =
  in_non_incremental_mode ~f:(fun () ->
    let trace = ref [] in
    let log event = trace := event :: !trace in
    let (_ : (unit, unit) result) =
      Scheduler.run
        (Fiber.map_reduce_errors
           (module Monoid.Unit)
           ~on_error:(fun _exn ->
             log "late";
             Fiber.return ())
           (fun () ->
              Memo.run_with_error_handler
                (fun () ->
                   Memo.fork_and_join_unit
                     (fun () ->
                        Fiber.map (Scheduler.yield ()) ~f:(fun () -> failwith "error")
                        |> Memo.of_reproducible_fiber)
                     (fun () ->
                        Fiber.map (Scheduler.yield ()) ~f:(fun () -> log "other branch")
                        |> Memo.of_reproducible_fiber))
                ~handle_error_no_raise:(fun _exn ->
                  log "early";
                  Fiber.return ())))
    in
    List.rev !trace |> List.iter ~f:(fun event -> printfn "%s" event));
  [%expect
    {|
    early
    other branch
    late
    |}]
;;

let%expect_test "current run reads survive resets and incremental mode changes" =
  Memo.reset Memo.Invalidation.empty;
  let read = Memo.current_run () in
  let same_run left right =
    match Memo.Run.For_tests.compare left right with
    | Eq -> true
    | Lt | Gt -> false
  in
  in_non_incremental_mode ~f:(fun () ->
    let first = run read in
    let left, right = run (Memo.fork_and_join (fun () -> read) (fun () -> read)) in
    printfn "same run: %b" (same_run first left && same_run first right);
    Memo.reset Memo.Invalidation.empty;
    let next = run read in
    printfn "reset advanced: %b" (not (same_run first next));
    printfn "current: %b" (same_run next (Memo.Run.For_tests.current ()));
    Memo.For_tests.invalidate_memoization_caches ();
    printfn "cache invalidation preserved run: %b" (same_run next (run read)));
  let computes = ref 0 in
  let node =
    Memo.lazy_node ~name:"incremental current run consumer" (fun () ->
      incr computes;
      read)
  in
  let first = run (Memo.Node.read node) in
  let deps =
    Memo.For_tests.get_deps_structured node |> Option.value_exn |> Dyn.to_string
  in
  printfn "incremental dependencies: %s" deps;
  Memo.reset Memo.Invalidation.empty;
  let second = run (Memo.Node.read node) in
  printfn "consumer advanced: %b, computes: %d" (not (same_run first second)) !computes;
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    same run: true
    reset advanced: true
    current: true
    cache invalidation preserved run: true
    incremental dependencies: Singleton (Some "current-run", ())
    consumer advanced: true, computes: 2
    |}]
;;

let%expect_test "nonincremental current run reads preserve dependency metrics" =
  let open Memo.O in
  in_non_incremental_mode ~f:(fun () ->
    Memo.reset Memo.Invalidation.empty;
    let read = Memo.current_run () in
    let (_ : Memo.Run.t) = run read in
    Memo.Metrics.reset ();
    let (_ : Memo.Run.t) = run read in
    printfn "top-level edges: %d" (Counter.read Memo.Metrics.Compute.edges);
    let node =
      Memo.lazy_node ~name:"nonincremental current run consumer" (fun () ->
        let* (_ : Memo.Run.t) = read in
        let+ (_ : Memo.Run.t) = read in
        ())
    in
    run (Memo.Node.read node);
    printfn "nodes: %d" (Counter.read Memo.Metrics.Compute.nodes);
    printfn "edges: %d" (Counter.read Memo.Metrics.Compute.edges);
    let deps =
      Memo.For_tests.get_deps_structured node |> Option.value_exn |> Dyn.to_string
    in
    printfn "dependencies: %s" deps;
    Memo.Metrics.assert_invariants ();
    Memo.reset Memo.Invalidation.empty);
  [%expect
    {|
    top-level edges: 0
    nodes: 1
    edges: 2
    dependencies: Empty
    |}]
;;
