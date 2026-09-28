(* Tests for the interaction of parallelism with cutoffs, focused on the *eagerness* /
   altitude contract during cache restore.

   The observable signal is the [printf] interleaving: each node's compute prints
   ["name+ "], then yields (via [Scheduler.yield]), then prints ["name- "] (or ["name! "]
   and raises, when [fail] is set). Nodes that run concurrently therefore interleave their
   markers (["x+ y+ x- y-"]) while sequential ones do not (["x+ x- y+ y-"]). The run-2
   ordering, observed after an invalidate-and-reset, is the evidence of eager vs deferred
   recomputation.

   Notes on scope:
   - [Memo.Map.parallel_iter] does not exist (only the [Map] functor exposes
     [parallel_map]), so the map-based cases are not covered here.
   - Memo errors carry no dependency subtrees, so the error case asserts only the marker
     interleaving and the raised exception, not dep structure.
   - [append_par] does not exist (only [append_par_init]). *)

open! Stdune
open! Memo.O
open Test_helpers.Make ()

(* A node bundles the memoized cell with the [unit]-valued variable it reads: invalidating
   the variable forces the node to recompute. *)
module Node = struct
  type t =
    { cell : (unit, unit) Memo.Node.t
    ; var : Memo.Var.Unit.t
    }

  let read t = Memo.Node.read t.cell
end

(* A computation with a [Scheduler.yield] between its start and end markers, making it
   possible to distinguish multiple computations running in parallel from ones running in
   sequence.

   Reading [var] makes the node depend on it, so invalidating [var] forces a
   recomputation. With [~cutoff:true] the recomputation produces the same value ([()]) and
   so does not invalidate callers; with [~cutoff:false] the node is always considered
   [Changed]. An optional [?dep] introduces a sequential dependency on another node, read
   before this node's own body runs. *)
let node ?dep ~cutoff ?(fail = false) name : Node.t =
  let var = Memo.Var.Unit.create () in
  let cutoff = if cutoff then Some (fun () () -> true) else None in
  let maybe_add_dep f () =
    match dep with
    | None -> f ()
    | Some dep ->
      let* () = Node.read dep in
      f ()
  in
  let table =
    Memo.create
      name
      ~input:(module Unit)
      ?cutoff
      (maybe_add_dep (fun () ->
         let* () = Memo.Var.Unit.read var in
         Memo.of_reproducible_fiber
         @@ Fiber.of_thunk (fun () ->
           let open Fiber.O in
           printf "%s+ " name;
           let+ () = Scheduler.yield () in
           if fail
           then (
             printf "%s! " name;
             raise_notrace (Failure name));
           printf "%s- " name)))
  in
  { cell = Memo.node table (); var }
;;

let evaluate (top : (unit, _) Memo.Node.t) : unit =
  run (Memo.map ~f:ignore (Memo.Node.read top))
;;

(* Like [evaluate], but reports (rather than raises) errors so the error case can observe
   both the marker interleaving and the raised exception. *)
let evaluate_and_log_errors (top : (unit, _) Memo.Node.t) : unit =
  match
    Scheduler.run
      (Fiber.collect_errors (fun () -> Memo.run (Memo.map ~f:ignore (Memo.Node.read top))))
  with
  | Ok () -> ()
  | Error exns ->
    List.iter exns ~f:(fun exn ->
      Format.printf "Error: %a@." Pp.to_fmt (Dyn.pp (Exn_with_backtrace.to_dyn exn)))
;;

(* Invalidate every node's variable, advance the run, and reset the metrics so the metrics
   printed after the 2nd run reflect that run alone. *)
let invalidate_and_reset ~nodes =
  let invalidation =
    List.fold_left nodes ~init:Memo.Invalidation.empty ~f:(fun acc (node : Node.t) ->
      Memo.Invalidation.combine acc (Memo.Var.Unit.invalidate node.var ~reason:Test))
  in
  Memo.reset invalidation;
  Memo.Metrics.reset ()
;;

let test ~nodes ~top =
  printf "1st run: ";
  evaluate top;
  invalidate_and_reset ~nodes;
  printf "\n2nd run: ";
  evaluate top;
  printf "\n\nMetrics for 2nd run:\n\n";
  print_metrics ()
;;

(* Basic combinator cases, establishing the harness. *)

let%expect_test "fork_and_join" =
  let a, b = node ~cutoff:true "a", node ~cutoff:true "b" in
  let top =
    Memo.lazy_node ~name:"top" (fun () ->
      Memo.fork_and_join (fun () -> Node.read a) (fun () -> Node.read b))
  in
  test ~nodes:[ a; b ] ~top;
  [%expect
    {|
    1st run: a+ b+ a- b-
    2nd run: a+ b+ a- b-

    Metrics for 2nd run:

    Memo graph: 3/4/0 nodes/edges/blocked (restore), 4/2/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

let%expect_test "fork_and_join_unit" =
  let a, b = node ~cutoff:true "a", node ~cutoff:true "b" in
  let top =
    Memo.lazy_node ~name:"top" (fun () ->
      Memo.fork_and_join_unit (fun () -> Node.read a) (fun () -> Node.read b))
  in
  test ~nodes:[ a; b ] ~top;
  [%expect
    {|
    1st run: a+ b+ a- b-
    2nd run: a+ b+ a- b-

    Metrics for 2nd run:

    Memo graph: 3/4/0 nodes/edges/blocked (restore), 4/2/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

let%expect_test "parallel_map" =
  let nodes = List.map [ "a"; "b"; "c"; "d"; "e" ] ~f:(node ~cutoff:true) in
  let top = Memo.lazy_node ~name:"top" (fun () -> Memo.parallel_map nodes ~f:Node.read) in
  test ~nodes ~top;
  [%expect
    {|
    1st run: a+ b+ c+ d+ e+ a- b- c- d- e-
    2nd run: a+ b+ c+ d+ e+ a- b- c- d- e-

    Metrics for 2nd run:

    Memo graph: 6/10/0 nodes/edges/blocked (restore), 10/5/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

(* Core eagerness / altitude cases. *)

(* Structure [Par [ Seq [a, b], c ]], with [a], [b] no-cutoff and [c] cutoff, all
   depending on [shared] (no cutoff). In run 2, [c] (cutoff, directly under [Par]) is
   eagerly recomputed during restore, but [a] and [b] (no cutoff, inside the [Seq]) are
   not: the [Seq] reports [Changed] immediately and defers them to the compute phase. *)
let%expect_test "seq inside par: no-cutoff nodes are not eagerly recomputed" =
  let shared = node ~cutoff:false "shared" in
  let a = node ~dep:shared ~cutoff:false "a" in
  let b = node ~dep:shared ~cutoff:false "b" in
  let c = node ~dep:shared ~cutoff:true "c" in
  let top =
    Memo.lazy_node ~name:"top" (fun () ->
      Memo.fork_and_join_unit
        (fun () ->
           let* () = Node.read a in
           Node.read b)
        (fun () -> Node.read c))
  in
  test ~nodes:[ shared ] ~top;
  [%expect
    {|
    1st run: shared+ shared- a+ c+ a- b+ c- b-
    2nd run: shared+ shared- c+ c- a+ a- b+ b-

    Metrics for 2nd run:

    Memo graph: 8/6/0 nodes/edges/blocked (restore), 6/10/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

(* Structure [Par [a, b]] with both no-cutoff. In run 2 both are recomputed in parallel
   during restore even without a cutoff, and [top] recomputes. *)
let%expect_test "par with all no-cutoff nodes preserves parallelism" =
  let shared = node ~cutoff:false "shared" in
  let a = node ~dep:shared ~cutoff:false "a" in
  let b = node ~dep:shared ~cutoff:false "b" in
  let top =
    Memo.lazy_node ~name:"top" (fun () ->
      Memo.fork_and_join_unit (fun () -> Node.read a) (fun () -> Node.read b))
  in
  test ~nodes:[ shared ] ~top;
  [%expect
    {|
    1st run: shared+ shared- a+ b+ a- b-
    2nd run: shared+ shared- a+ b+ a- b-

    Metrics for 2nd run:

    Memo graph: 6/5/0 nodes/edges/blocked (restore), 5/7/1 nodes/edges/blocked (compute)
    Memo cycle detection graph: 3/2/1 nodes/edges/paths
    |}]
;;

(* Structure [Par [a, b]] with [a] no-cutoff and failing. In run 2 [a] is eagerly
   recomputed in parallel with [b], fails, and the error propagates. *)
let%expect_test "error in no-cutoff node inside par" =
  let shared = node ~cutoff:false "shared" in
  let a = node ~dep:shared ~cutoff:false ~fail:true "a" in
  let b = node ~dep:shared ~cutoff:true "b" in
  let top =
    Memo.lazy_node ~name:"top" (fun () ->
      Memo.fork_and_join_unit (fun () -> Node.read a) (fun () -> Node.read b))
  in
  printf "1st run: ";
  evaluate_and_log_errors top;
  invalidate_and_reset ~nodes:[ shared ];
  printf "2nd run: ";
  evaluate_and_log_errors top;
  printf "\nMetrics for 2nd run:\n\n";
  print_metrics ();
  [%expect
    {|
    1st run: shared+ shared- a+ b+ a! b- Error: { exn =
               "Memo.Error.E { exn = \"Failure(\\\"a\\\")\"; stack = [ (\"a\", ()); (\"top\", ()) ] }"
           ; backtrace = ""
           }
    2nd run: shared+ shared- a+ b+ a! b- Error: { exn =
               "Memo.Error.E { exn = \"Failure(\\\"a\\\")\"; stack = [ (\"a\", ()); (\"top\", ()) ] }"
           ; backtrace = ""
           }

    Metrics for 2nd run:

    Memo graph: 6/5/0 nodes/edges/blocked (restore), 5/7/1 nodes/edges/blocked (compute)
    Memo cycle detection graph: 3/2/1 nodes/edges/paths
    |}]
;;

(* Structure [Par [ Seq [a, Par [b, c]], d ]], with [a], [d] cutoff and [b], [c]
   no-cutoff. In run 2 [a]'s cutoff fires so the [Seq] proceeds into the inner
   [Par [b, c]], where eagerness re-enables and [b], [c] recompute in parallel. *)
let%expect_test "nested par-seq-par: eagerness re-enables under a Par" =
  let shared = node ~cutoff:false "shared" in
  let a = node ~dep:shared ~cutoff:true "a" in
  let b = node ~dep:shared ~cutoff:false "b" in
  let c = node ~dep:shared ~cutoff:false "c" in
  let d = node ~dep:shared ~cutoff:true "d" in
  let top =
    Memo.lazy_node ~name:"top" (fun () ->
      Memo.fork_and_join_unit
        (fun () ->
           let* () = Node.read a in
           Memo.fork_and_join_unit (fun () -> Node.read b) (fun () -> Node.read c))
        (fun () -> Node.read d))
  in
  test ~nodes:[ shared ] ~top;
  [%expect
    {|
    1st run: shared+ shared- a+ d+ a- b+ c+ d- b- c-
    2nd run: shared+ shared- a+ d+ a- b+ c+ d- b- c-

    Metrics for 2nd run:

    Memo graph: 10/9/0 nodes/edges/blocked (restore), 7/13/1 nodes/edges/blocked (compute)
    Memo cycle detection graph: 3/2/1 nodes/edges/paths
    |}]
;;

let%expect_test "concurrent readers share successful restoration results" =
  Memo.reset Memo.Invalidation.empty;
  let input = Memo.Var.create ~name:"input" 0 in
  let prefix = Memo.lazy_node ~name:"ready prefix" (fun () -> Memo.return ()) in
  let suffix = Memo.lazy_node ~name:"ready suffix" (fun () -> Memo.return ()) in
  let leaf =
    Memo.lazy_node ~name:"leaf" ~cutoff:Int.equal (fun () ->
      let* value = Memo.Var.read input in
      let+ () = Memo.of_reproducible_fiber (Scheduler.yield ()) in
      value / 2)
  in
  let computes = ref 0 in
  let shared =
    Memo.lazy_node ~name:"shared" (fun () ->
      incr computes;
      let* () = Memo.Node.read prefix in
      let* () = Memo.Node.read prefix in
      let* value = Memo.Node.read leaf in
      let* () = Memo.Node.read suffix in
      let* () = Memo.Node.read suffix in
      let+ () = Memo.of_reproducible_fiber (Scheduler.yield ()) in
      ref value)
  in
  let read = Memo.Node.read shared in
  let original = run read in
  List.iter
    [ "equal", 1; "changed", 2 ]
    ~f:(fun (label, value) ->
      Memo.reset (Memo.Var.set input value);
      run (Memo.Node.read prefix);
      run (Memo.Node.read suffix);
      Memo.Metrics.reset ();
      let a, (b, c) =
        run
          (Memo.fork_and_join
             (fun () -> read)
             (fun () -> Memo.fork_and_join (fun () -> read) (fun () -> read)))
      in
      assert (a == c);
      assert (Counter.read Memo.Metrics.Restore.blocked >= 2);
      assert (Counter.read Memo.Metrics.Restore.edges = if value = 1 then 6 else 4);
      (* A changed leaf blocks readers during both restoration and computation.
         The two phases must use independent cycle-detection frames. *)
      if value = 2 then assert (Counter.read Memo.Metrics.Compute.blocked >= 2);
      printf
        "%s: value=%d shared=%b original=%b computes=%d blocked=%b\n"
        label
        !a
        (a == b)
        (a == original)
        !computes
        (Counter.read Memo.Metrics.Restore.blocked > 0));
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    equal: value=0 shared=true original=true computes=1 blocked=true
    changed: value=1 shared=true original=false computes=2 blocked=true
    |}]
;;

let%expect_test "last sequential dependency forwards cancellation" =
  Memo.reset Memo.Invalidation.empty;
  let cyclic = Memo.Var.create ~name:"last-section cyclic" false in
  let prefix = Memo.lazy_node ~name:"prefix" (fun () -> Memo.return ()) in
  let a_ref = Fdecl.create (fun _ -> Dyn.Opaque) in
  let a_computes = ref 0 in
  let b =
    Memo.lazy_node ~name:"B" ~cutoff:Int.equal (fun () ->
      let* cyclic = Memo.Var.read cyclic in
      if cyclic then Memo.Node.read (Fdecl.get a_ref) else Memo.return 0)
  in
  let a =
    Memo.lazy_node ~name:"A" (fun () ->
      incr a_computes;
      let* () = Memo.Node.read prefix in
      Memo.Node.read b)
  in
  Fdecl.set a_ref a;
  printfn "warm: %d" (run (Memo.Node.read a));
  Memo.reset (Memo.Var.set cyclic true);
  run (Memo.Node.read prefix);
  (* B computes while A restores its final B edge. Cancellation must not
     recompute A. *)
  (match Scheduler.run (run_collect_errors (fun () -> Memo.Node.read b)) with
   | Ok _ -> Code_error.raise "Expected the final dependency cycle" []
   | Error errors ->
     assert (not (List.is_empty errors));
     assert (
       List.for_all errors ~f:(fun { Exn_with_backtrace.exn; _ } ->
         match exn with
         | Memo.Cycle_error.E _ -> true
         | _ -> false)));
  assert (!a_computes = 1);
  print_endline "cancelled without recomputing A";
  Memo.reset (Memo.Var.set cyclic false);
  printfn "recovered: %d" (run (Memo.Node.read a));
  assert (!a_computes = 2);
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    warm: 0
    cancelled without recomputing A
    recovered: 0
    |}]
;;

let%expect_test "current sequential duplicates preserve the cached object" =
  Memo.reset Memo.Invalidation.empty;
  let leaf = Memo.lazy_node ~name:"sequence leaf" (fun () -> Memo.return ()) in
  let computes = ref 0 in
  let parent =
    Memo.lazy_node ~name:"sequence parent" (fun () ->
      incr computes;
      let* () = Memo.Node.read leaf in
      let* () = Memo.Node.read leaf in
      let+ () = Memo.Node.read leaf in
      ref 7)
  in
  let read = Memo.Node.read parent in
  let original = run read in
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read leaf);
  Memo.Metrics.reset ();
  let restored = run read in
  assert (restored == original);
  assert (run read == original);
  assert (!computes = 1);
  assert (Counter.read Memo.Metrics.Restore.nodes = 1);
  assert (Counter.read Memo.Metrics.Restore.edges = 3);
  assert (Counter.read Memo.Metrics.Compute.nodes = 0);
  print_endline "same object; three restored duplicate edges";
  Memo.reset Memo.Invalidation.empty;
  [%expect {| same object; three restored duplicate edges |}]
;;

let%expect_test "a changed current sequence stops before its unused tail" =
  Memo.reset Memo.Invalidation.empty;
  let input = Memo.Var.create ~name:"sequence input" 0 in
  let prefix = Memo.lazy_node ~name:"sequence prefix" (fun () -> Memo.return ()) in
  let child =
    Memo.lazy_node ~name:"sequence child" ~cutoff:Int.equal (fun () ->
      Memo.Var.read input)
  in
  let tail_live = ref 0 in
  let tail =
    Memo.lazy_node
      ~name:"unused sequence tail"
      ~on_event:(function
        | Live -> incr tail_live
        | Validated -> ())
      (fun () -> Memo.return ())
  in
  let computes = ref 0 in
  let parent =
    Memo.lazy_node ~name:"skipped sequence parent" (fun () ->
      incr computes;
      let* () = Memo.Node.read prefix in
      let* () = Memo.Node.read prefix in
      let* value = Memo.Node.read child in
      if value = 0
      then
        let+ () = Memo.Node.read tail in
        value
      else Memo.return value)
  in
  assert (run (Memo.Node.read parent) = 0);
  Memo.reset (Memo.Var.set input 1);
  assert (run (Memo.Node.read child) = 1);
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read prefix);
  assert (run (Memo.Node.read child) = 1);
  Memo.Metrics.reset ();
  assert (run (Memo.Node.read parent) = 1);
  assert (!computes = 2);
  assert (!tail_live = 1);
  assert (Counter.read Memo.Metrics.Restore.nodes = 1);
  assert (Counter.read Memo.Metrics.Restore.edges = 3);
  print_endline "changed child; three restored edges; tail untouched";
  Memo.reset Memo.Invalidation.empty;
  [%expect {| changed child; three restored edges; tail untouched |}]
;;

let%expect_test "current unchanged singleton preserves the cached object" =
  Memo.reset Memo.Invalidation.empty;
  let leaf = Memo.lazy_node ~name:"singleton leaf" (fun () -> Memo.return ()) in
  let computes = ref 0 in
  let parent =
    Memo.lazy_node ~name:"singleton parent" (fun () ->
      incr computes;
      let+ () = Memo.Node.read leaf in
      ref 7)
  in
  let original = run (Memo.Node.read parent) in
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read leaf);
  Memo.Metrics.reset ();
  let restored = run (Memo.Node.read parent) in
  assert (run (Memo.Node.read parent) == restored);
  printfn "same=%b computes=%d" (original == restored) !computes;
  print_metrics ();
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    same=true computes=1
    Memo graph: 1/1/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

let%expect_test "current singleton changed while its parent was unreachable" =
  Memo.reset Memo.Invalidation.empty;
  let input = Memo.Var.create ~name:"singleton input" 0 in
  let child =
    Memo.lazy_node ~name:"singleton child" ~cutoff:Int.equal (fun () ->
      Memo.Var.read input)
  in
  let computes = ref 0 in
  let parent =
    Memo.lazy_node ~name:"skipped singleton parent" (fun () ->
      incr computes;
      Memo.Node.read child)
  in
  assert (run (Memo.Node.read parent) = 0);
  Memo.reset (Memo.Var.set input 1);
  assert (run (Memo.Node.read child) = 1);
  Memo.reset Memo.Invalidation.empty;
  assert (run (Memo.Node.read child) = 1);
  Memo.Metrics.reset ();
  let value = run (Memo.Node.read parent) in
  printfn "value=%d computes=%d" value !computes;
  print_metrics ();
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    value=1 computes=2
    Memo graph: 1/1/0 nodes/edges/blocked (restore), 1/1/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

let%expect_test "current singleton preserves a cached error stack" =
  Memo.reset Memo.Invalidation.empty;
  let leaf = Memo.lazy_node ~name:"singleton error leaf" (fun () -> Memo.return ()) in
  let computes = ref 0 in
  let parent =
    Memo.lazy_node ~name:"singleton failing parent" (fun () ->
      incr computes;
      let+ () = Memo.Node.read leaf in
      failwith "singleton failure")
  in
  let read = Memo.Node.read parent in
  let check () =
    match Scheduler.run (Fiber.collect_errors (fun () -> Memo.run read)) with
    | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
      assert (
        match Memo.Error.get error with
        | Failure message -> String.equal message "singleton failure"
        | _ -> false);
      let names =
        List.map (Memo.Error.stack error) ~f:(fun frame ->
          Option.value_exn (Memo.Stack_frame.name frame))
      in
      printfn "stack=%s computes=%d" (String.concat ~sep:" -> " names) !computes
    | Ok _ | Error _ -> Code_error.raise "Expected a named Memo error" []
  in
  check ();
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read leaf);
  Memo.Metrics.reset ();
  check ();
  print_metrics ();
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    stack=singleton failing parent computes=1
    stack=singleton failing parent computes=1
    Memo graph: 1/1/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;

let%expect_test "current singleton preserves events and replay failures" =
  Memo.reset Memo.Invalidation.empty;
  let leaf = Memo.lazy_node ~name:"observed singleton leaf" (fun () -> Memo.return 7) in
  let live = ref 0 in
  let validated = ref 0 in
  let observed_computes = ref 0 in
  let observed =
    Memo.lazy_node
      ~name:"observed singleton parent"
      ~on_event:(function
        | Live -> incr live
        | Validated -> incr validated)
      (fun () ->
         incr observed_computes;
         Memo.Node.read leaf)
  in
  let replays = ref 0 in
  let replayed_computes = ref 0 in
  let fail_replay = ref false in
  let replayed =
    Memo.create_with_replay
      "replayed singleton parent"
      ~input:(module Unit)
      ~cutoff:Int.equal
      ~replay:(fun () _ ->
        incr replays;
        if !fail_replay then failwith "singleton replay failure")
      (fun () ->
         incr replayed_computes;
         Memo.Node.read leaf)
  in
  let check () =
    assert (run (Memo.Node.read observed) = 7);
    assert (run (Memo.exec replayed ()) = 7)
  in
  check ();
  Memo.reset Memo.Invalidation.empty;
  assert (run (Memo.Node.read leaf) = 7);
  Memo.Metrics.reset ();
  check ();
  printfn
    "live=%d validated=%d computes=%d/%d replays=%d"
    !live
    !validated
    !observed_computes
    !replayed_computes
    !replays;
  print_metrics ();
  fail_replay := true;
  Memo.reset Memo.Invalidation.empty;
  assert (run (Memo.Node.read leaf) = 7);
  Memo.Metrics.reset ();
  (match Scheduler.run (run_collect_errors (fun () -> Memo.exec replayed ())) with
   | Error [ { Exn_with_backtrace.exn = Failure message; _ } ] ->
     assert (String.equal message "singleton replay failure")
   | Ok _ | Error _ -> Code_error.raise "Expected a replay failure" []);
  printfn "replay failed: computes=%d replays=%d" !replayed_computes !replays;
  print_metrics ();
  (* The early report must retain both frames after a ready prefix. Checking
     only the final error could instead observe the parent's recomputation. *)
  let prefix = Memo.lazy_node ~name:"replay prefix" (fun () -> Memo.return ()) in
  let parent =
    Memo.lazy_node ~name:"replay sequence" (fun () ->
      let* () = Memo.Node.read prefix in
      let* () = Memo.Node.read prefix in
      Memo.exec replayed ())
  in
  let read = Memo.Node.read parent in
  fail_replay := false;
  Memo.reset Memo.Invalidation.empty;
  assert (run read = 7);
  Memo.reset Memo.Invalidation.empty;
  run (Memo.Node.read prefix);
  assert (run (Memo.Node.read leaf) = 7);
  fail_replay := true;
  let before = !replays in
  let reported = ref [] in
  let fiber =
    Memo.run_with_error_handler
      (fun () -> read)
      ~handle_error_no_raise:(fun error ->
        reported := error :: !reported;
        Fiber.return ())
  in
  assert (!replays = before);
  let result = Scheduler.run (Fiber.collect_errors (fun () -> fiber)) in
  (match result, !reported with
   | Error [ _ ], [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
     assert (
       match Memo.Error.get error with
       | Failure message -> String.equal message "singleton replay failure"
       | _ -> false);
     let names =
       List.map (Memo.Error.stack error) ~f:(fun frame ->
         Option.value_exn (Memo.Stack_frame.name frame))
     in
     assert (
       List.equal String.equal names [ "replayed singleton parent"; "replay sequence" ])
   | _ -> Code_error.raise "Expected a named early replay failure" []);
  assert (!replays = before + 1);
  Memo.reset Memo.Invalidation.empty;
  [%expect
    {|
    live=2 validated=2 computes=1/1 replays=2
    Memo graph: 2/2/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    replay failed: computes=1 replays=3
    Memo graph: 1/1/0 nodes/edges/blocked (restore), 0/0/0 nodes/edges/blocked (compute)
    Memo cycle detection graph: 0/0/0 nodes/edges/paths
    |}]
;;
