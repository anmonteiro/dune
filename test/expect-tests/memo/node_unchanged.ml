open! Stdune
open! Memo.O
open Test_helpers.Make ()

let current = Memo.Run.For_tests.current
let unchanged node since = Memo.Node.is_unchanged node ~since

let%expect_test "successful cache inspection does not validate dependencies" =
  let fail = Memo.Var.create ~name:"cached success input" false in
  let child =
    Memo.lazy_node ~name:"cached success dependency" (fun () ->
      let+ fail = Memo.Var.read fail in
      if fail then failwith "preparation failed")
  in
  let calls = ref 0 in
  let parent =
    Memo.lazy_node ~name:"cached success parent" (fun () ->
      incr calls;
      Memo.Node.read child)
  in
  let inspect label =
    printfn "%s: %b (%d calls)" label (Memo.Node.is_successfully_cached parent) !calls
  in
  inspect "uncomputed";
  run (Memo.Node.read parent);
  inspect "successful";
  Memo.reset (Memo.Var.set fail true);
  inspect "dependency invalidated";
  ignore (Scheduler.run (run_collect_errors (fun () -> Memo.Node.read parent)));
  inspect "restoration failed";
  Memo.reset Memo.Invalidation.empty;
  inspect "old error";
  Memo.reset (Memo.Var.set fail false);
  run (Memo.Node.read parent);
  inspect "recovered";
  Memo.reset (Memo.Node.invalidate parent ~reason:Test);
  inspect "directly invalidated";
  [%expect
    {|
    uncomputed: false (0 calls)
    successful: true (1 calls)
    dependency invalidated: true (1 calls)
    restoration failed: false (2 calls)
    old error: false (2 calls)
    recovered: true (3 calls)
    directly invalidated: false (3 calls)
    |}]
;;

let%expect_test "unchanged probes do not evaluate or validate old dependencies" =
  let calls = ref 0 in
  let leaf =
    Memo.lazy_node ~name:"probe leaf" (fun () ->
      incr calls;
      Memo.return 7)
  in
  let parent =
    Memo.lazy_node ~name:"probe parent" (fun () ->
      let* _ = Memo.Node.read leaf in
      Memo.fork_and_join (fun () -> Memo.Node.read leaf) (fun () -> Memo.Node.read leaf))
  in
  let since = current () in
  printfn "uncomputed: %b" (unchanged parent since);
  ignore (run (Memo.Node.read parent));
  printfn "current: %b" (unchanged parent since);
  Memo.reset Memo.Invalidation.empty;
  Memo.Metrics.reset ();
  printfn "old: %b, repeated: %b" (unchanged parent since) (unchanged parent since);
  printfn
    "validated: %b; computations: %d"
    (Option.is_some (Memo.For_tests.get_deps_structured parent))
    !calls;
  print_metrics ();
  let probe =
    Memo.create
      "probe without dependencies"
      ~input:(module Unit)
      (fun () -> Memo.return (unchanged parent since))
  in
  ignore (run (Memo.exec probe ()));
  printfn
    "probe dependencies: %s"
    (match Memo.For_tests.get_deps probe () with
     | Some [] -> "empty"
     | None | Some _ -> "unexpected");
  let future = Memo.Run.For_tests.of_int (Memo.Run.For_tests.to_int (current ()) + 1) in
  printfn "future: %b" (unchanged parent future);
  [%expect
    {|
    uncomputed: false
    current: true
    old: true, repeated: true
    validated: false; computations: 1
    Memo graph: 0/0/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    probe dependencies: empty
    future: false
    |}]
;;

let%expect_test "cutoffs can turn an unproved graph into an unchanged one" =
  let input = Memo.Var.create ~name:"probe input" 1 in
  let child =
    Memo.lazy_node ~name:"probe cutoff" ~cutoff:Int.equal (fun () ->
      let+ value = Memo.Var.read input in
      value mod 2)
  in
  let parent =
    Memo.lazy_node ~name:"probe skipped parent" ~cutoff:Int.equal (fun () ->
      Memo.Node.read child)
  in
  ignore (run (Memo.Node.read parent));
  let since = current () in
  printfn "initial: %b" (unchanged parent since);
  Memo.reset (Memo.Var.set input 3);
  printfn "invalid child: %b" (unchanged parent since);
  ignore (run (Memo.Node.read child));
  printfn "cutoff child: %b" (unchanged parent since);
  Memo.reset (Memo.Var.set input 4);
  ignore (run (Memo.Node.read child));
  printfn
    "changed current child: %b; skipped parent: %b"
    (unchanged child since)
    (unchanged parent since);
  ignore (run (Memo.Node.read parent));
  printfn
    "recomputed parent since old: %b; since current: %b"
    (unchanged parent since)
    (unchanged parent (current ()));
  [%expect
    {|
    initial: true
    invalid child: false
    cutoff child: true
    changed current child: false; skipped parent: false
    recomputed parent since old: false; since current: true
    |}]
;;

let%expect_test "replay and event callbacks are not invoked by a probe" =
  let replays = ref 0 in
  let events = ref 0 in
  let replay =
    Memo.create_with_replay
      "probe replay"
      ~input:(module Unit)
      ~cutoff:Int.equal
      ~replay:(fun () _ -> incr replays)
      (fun () -> Memo.return 7)
  in
  let replay = Memo.node replay () in
  let event =
    Memo.lazy_node
      ~name:"probe event"
      ~on_event:(fun _ -> incr events)
      (fun () -> Memo.return 7)
  in
  let parent =
    Memo.lazy_node ~name:"probe replay parent" (fun () -> Memo.Node.read replay)
  in
  ignore (run (Memo.Node.read parent));
  ignore (run (Memo.Node.read event));
  let since = current () in
  printfn "current: %b, %b" (unchanged replay since) (unchanged event since);
  Memo.reset Memo.Invalidation.empty;
  printfn
    "old: %b, %b; parent: %b"
    (unchanged replay since)
    (unchanged event since)
    (unchanged parent since);
  printfn "replays: %d; events: %d" !replays !events;
  ignore (run (Memo.Node.read replay));
  printfn "after ordinary replay: %b" (unchanged parent since);
  printfn "replays: %d; events: %d" !replays !events;
  [%expect
    {|
    current: true, true
    old: false, false; parent: false
    replays: 1; events: 2
    after ordinary replay: true
    replays: 2; events: 2
    |}]
;;

let%expect_test "errors and cycles are not successful cached proofs" =
  List.iter [ false; true ] ~f:(fun non_reproducible ->
    let node =
      Memo.lazy_node ~name:"probe failure" (fun () ->
        let error = Failure "probe failure" in
        raise_notrace (if non_reproducible then Memo.Non_reproducible error else error))
    in
    ignore (Scheduler.run (run_collect_errors (fun () -> Memo.Node.read node)));
    let since = current () in
    printfn "failed current: %b" (unchanged node since);
    Memo.reset Memo.Invalidation.empty;
    printfn "failed old: %b" (unchanged node since));
  let cycle =
    Memo.create_rec "probe cycle" ~input:(module Unit) (fun self () -> self ())
  in
  let node = Memo.node cycle () in
  ignore
    (Scheduler.run (run_collect_errors (fun () -> (Memo.Node.read node : unit Memo.t))));
  printfn "cycle: %b" (unchanged node (current ()));
  [%expect
    {|
    failed current: false
    failed old: false
    failed current: false
    failed old: false
    cycle: false
    |}]
;;

let%expect_test "in-flight restoration and computation are not proofs" =
  let input = Memo.Var.Unit.create () in
  let blocked = ref false in
  let entered = Fiber.Ivar.create () in
  let release = Fiber.Ivar.create () in
  let child =
    Memo.lazy_node ~name:"probe blocked child" ~cutoff:Unit.equal (fun () ->
      let* () = Memo.Var.Unit.read input in
      if not !blocked
      then Memo.return ()
      else
        Memo.of_reproducible_fiber
          (let open Fiber.O in
           let* () = Fiber.Ivar.fill entered () in
           Fiber.Ivar.read release))
  in
  let parent =
    Memo.lazy_node ~name:"probe restoring parent" (fun () -> Memo.Node.read child)
  in
  run (Memo.Node.read parent);
  let since = current () in
  blocked := true;
  Memo.reset (Memo.Var.Unit.invalidate input ~reason:Test);
  ignore
    (run
       (Memo.fork_and_join
          (fun () -> Memo.Node.read parent)
          (fun () ->
             Memo.of_reproducible_fiber
               (let open Fiber.O in
                let* () = Fiber.Ivar.read entered in
                printfn
                  "computing: %b; restoring: %b"
                  (unchanged child since)
                  (unchanged parent since);
                printfn
                  "cached computing: %b; cached restoring: %b"
                  (Memo.Node.is_successfully_cached child)
                  (Memo.Node.is_successfully_cached parent);
                Fiber.Ivar.fill release ()))));
  printfn "completed: %b, %b" (unchanged child since) (unchanged parent since);
  [%expect
    {|
    computing: false; restoring: false
    cached computing: false; cached restoring: false
    completed: true, true
    |}]
;;

let%expect_test "proofs are cleared when caches are invalidated" =
  let leaf = Memo.create "probe cached leaf" ~input:(module Unit) Memo.return in
  let parent = Memo.lazy_node ~name:"probe cache reset" (fun () -> Memo.exec leaf ()) in
  run (Memo.Node.read parent);
  let since = current () in
  Memo.reset Memo.Invalidation.empty;
  printfn "before invalidation: %b" (unchanged parent since);
  Memo.For_tests.invalidate_memoization_caches ();
  printfn "after invalidation: %b" (unchanged parent since);
  [%expect
    {|
    before invalidation: true
    after invalidation: false
    |}];
  Memo.reset Memo.Invalidation.empty
;;

let%expect_test "proofs require tracked dependencies across runs" =
  Fun.protect
    ~finally:(fun () -> Memo.set_incremental true)
    (fun () ->
       Memo.set_incremental false;
       let node = Memo.lazy_node ~name:"probe untracked" Memo.return in
       run (Memo.Node.read node);
       let since = current () in
       printfn "current: %b" (unchanged node since);
       Memo.reset Memo.Invalidation.empty;
       printfn "old: %b" (unchanged node since));
  [%expect
    {|
    current: true
    old: false
    |}]
;;

let%expect_test "deep shared graphs have bounded traversal stack and work" =
  let leaf = Memo.lazy_node ~name:"probe deep leaf" Memo.return in
  let root = ref leaf in
  run (Memo.Node.read leaf);
  for _ = 1 to 10_000 do
    let child = !root in
    let parent =
      Memo.lazy_node ~name:"probe deep parent" (fun () ->
        let* () = Memo.Node.read child in
        Memo.Node.read child)
    in
    run (Memo.Node.read parent);
    root := parent
  done;
  let since = current () in
  Memo.reset Memo.Invalidation.empty;
  Memo.Metrics.reset ();
  printfn "deep graph: %b, repeated: %b" (unchanged !root since) (unchanged !root since);
  print_metrics ();
  [%expect
    {|
    deep graph: true, repeated: true
    Memo graph: 0/0/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;
