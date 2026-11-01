(* Tests for dependency-cycle detection and recovery across runs. *)

open! Stdune
open! Memo.O
open Test_helpers.Make ()

(* While [cyclic] is set, [node i] depends on itself, forming a direct dependency
   cycle during its own computation. Once the cycle is removed the node must be
   recomputed - not served the cached cycle error - which exercises the rule that
   dependency-cycle errors are non-reproducible. *)
let%expect_test "a self-dependency cycle is detected, then recomputed after removal" =
  let cyclic = Memo.Var.create ~name:"cyclic" true in
  let node = ref None in
  let table =
    Memo.create
      "node"
      ~input:(module Int)
      (fun i ->
         let* cyclic = Memo.Var.read cyclic in
         if cyclic then Memo.exec (Option.value_exn !node) i else Memo.return (i + 100))
  in
  node := Some table;
  evaluate_and_print table 0;
  [%expect
    {|
    Dependency cycle detected:
    - ("node", 0)
    f 0 = Error [ { exn = "Cycle_error.E [ (\"node\", 0) ]"; backtrace = "" } ]
    |}];
  Memo.reset (Memo.Var.set cyclic false);
  evaluate_and_print table 0;
  [%expect {| f 0 = Ok 100 |}];
  Memo.reset Memo.Invalidation.empty
;;

let%expect_test "the first cycle stops further DAG edges in the same run" =
  Memo.Metrics.reset ();
  let cyclic = Memo.Var.create ~name:"cyclic" true in
  let table =
    Memo.create_rec
      "node"
      ~input:(module Int)
      (fun f i ->
         let* cyclic = Memo.Var.read cyclic in
         if cyclic then f (if i mod 3 = 2 then i - 2 else i + 1) else Memo.return (i + 100))
  in
  let attempt i =
    let+ result =
      run_collect_errors (fun () -> Memo.exec table i) |> Memo.of_reproducible_fiber
    in
    (match result with
     | Error [ { Exn_with_backtrace.exn = Memo.Cycle_error.E error; _ } ] ->
       let members =
         Memo.Cycle_error.get error
         |> List.map ~f:(fun frame ->
           Memo.Stack_frame.as_instance_of frame ~of_:(Memo.Table.spec table)
           |> Option.value_exn)
         |> List.sort ~compare:Int.compare
       in
       printfn "f %d = cycle members %s" i (Dyn.to_string (Dyn.list Dyn.int members))
     | result -> print_result i result);
    printfn "cycle detection edges: %d" (Counter.read Memo.Metrics.Cycle_detection.edges);
    Memo.Metrics.assert_invariants ()
  in
  (* The first cycle accepts 2 -> 0 and 1 -> 2, but rejects 0 -> 1. The
     disjoint second cycle must reuse that error without adding 5 -> 3. Error
     propagation can rotate its frames, so compare members from the same table.
     Keep both attempts inside one top-level [Memo.run], which resets the latch. *)
  run
    (let* () = attempt 0 in
     attempt 3);
  [%expect
    {|
    f 0 = cycle members [ 0; 1; 2 ]
    cycle detection edges: 2
    f 3 = cycle members [ 0; 1; 2 ]
    cycle detection edges: 2
    |}];
  Memo.reset (Memo.Var.set cyclic false);
  Memo.Metrics.reset ();
  run
    (let* () = attempt 0 in
     attempt 3);
  [%expect
    {|
    f 0 = Ok 100
    cycle detection edges: 0
    f 3 = Ok 103
    cycle detection edges: 0
    |}];
  Memo.reset Memo.Invalidation.empty;
  Memo.Metrics.reset ()
;;

(* A cycle spanning several nodes: while the graph has 0 -> 1 -> 2 -> 0, node 0
   depends on itself transitively. Once the edge is broken, the formerly-cyclic
   node must recompute to a value rather than stay the cached cycle error. *)
let%expect_test "a multi-node dependency cycle is detected, then recovered after removal" =
  let graph : [ `Goto of int | `Stop of int ] array =
    Array.init 4 ~f:(function
      | 0 -> `Goto 1
      | 1 -> `Goto 2
      | 2 -> `Goto 0
      | _ -> `Stop 42)
  in
  let gate = Memo.Var.create ~name:"gate" () in
  let table =
    Memo.create_rec
      "node"
      ~input:(module Int)
      (fun f i ->
         let* () = Memo.Var.read gate in
         match graph.(i) with
         | `Goto j -> f j
         | `Stop result -> Memo.return result)
  in
  evaluate_and_print table 0;
  [%expect
    {|
    Dependency cycle detected:
    - ("node", 2)
    - called by ("node", 1)
    - called by ("node", 0)
    f 0 = Error
            [ { exn =
                  "Cycle_error.E [ (\"node\", 2); (\"node\", 1); (\"node\", 0) ]"
              ; backtrace = ""
              }
            ]
    |}];
  (* Break the cycle and advance the run so the cached cycle error is discarded. *)
  graph.(0) <- `Stop 42;
  Memo.reset (Memo.Var.set gate ());
  evaluate_and_print table 0;
  [%expect {| f 0 = Ok 42 |}];
  Memo.reset Memo.Invalidation.empty
;;
