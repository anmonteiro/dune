open Stdune

module Restore = struct
  let nodes = Counter.create ()
  let edges = Counter.create ()
  let blocked = Counter.create ()
  let reset () = List.iter [ nodes; edges; blocked ] ~f:Counter.reset
end

module Compute = struct
  let nodes = Counter.create ()
  let edges = Counter.create ()
  let blocked = Counter.create ()
  let reset () = List.iter [ nodes; edges; blocked ] ~f:Counter.reset
end

module Cache_validity = struct
  let queries = Counter.create ()
  let nodes = Counter.create ()
  let edges = Counter.create ()
  let cache_hits = Counter.create ()
  let reset () = List.iter [ queries; nodes; edges; cache_hits ] ~f:Counter.reset
end

module Cycle_detection = struct
  let nodes = Counter.create ()
  let edges = Counter.create ()
  let reset () = List.iter [ nodes; edges ] ~f:Counter.reset
end

let reset () =
  Restore.reset ();
  Compute.reset ();
  Cache_validity.reset ();
  Cycle_detection.reset ()
;;

let report ~reset_after_reporting =
  let memo =
    sprintf
      "Memo graph: %d/%d/%d nodes/edges/blocked (restore), %d/%d/%d nodes/edges/blocked \
       (compute)"
      (Counter.read Restore.nodes)
      (Counter.read Restore.edges)
      (Counter.read Restore.blocked)
      (Counter.read Compute.nodes)
      (Counter.read Compute.edges)
      (Counter.read Compute.blocked)
  in
  let cycle_detection =
    sprintf
      "Memo cycle detection graph: %d/%d/%d nodes/edges/paths"
      (Counter.read Cycle_detection.nodes)
      (Counter.read Cycle_detection.edges)
      (Counter.read Restore.blocked + Counter.read Compute.blocked)
  in
  let cache_validity =
    match Counter.read Cache_validity.queries with
    | 0 -> []
    | queries ->
      [ sprintf
          "Memo cache validity: %d/%d/%d/%d queries/nodes/edges/cache-hits"
          queries
          (Counter.read Cache_validity.nodes)
          (Counter.read Cache_validity.edges)
          (Counter.read Cache_validity.cache_hits)
      ]
  in
  if reset_after_reporting then reset ();
  String.concat ~sep:"\n" (memo :: cycle_detection :: cache_validity)
;;

let assert_invariants () =
  let nodes_in_cycle_detection_graph = Counter.read Cycle_detection.nodes in
  let nodes_restore = Counter.read Restore.nodes in
  let nodes_compute = Counter.read Compute.nodes in
  assert (nodes_in_cycle_detection_graph <= nodes_compute + nodes_restore);
  let edges_in_cycle_detection_graph = Counter.read Cycle_detection.edges in
  let edges_restore = Counter.read Restore.edges in
  let edges_compute = Counter.read Compute.edges in
  assert (edges_in_cycle_detection_graph <= edges_restore + edges_compute)
;;
