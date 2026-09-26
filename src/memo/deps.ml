module M = Metrics
open Stdune
module Metrics = M
open Fiber.O

(* Dependencies form a series-parallel graph. We alternate between sequential sections
   ([Seq], whose children are checked in order) and parallel sections ([Par], whose
   children are checked concurrently). [Seq]/[Par] arrays always have at least two
   elements; single elements are represented as [Singleton], and empty sections as
   [Empty]. *)
module Static = struct
  type 'node t =
    | Empty
    | Singleton of 'node
    | Seq of 'node t Array.Immutable.t
    | Par of 'node t Array.Immutable.t

  (* Restore chronological order from most-recent-first sections while
     flattening nested [Seq]s. *)
  let flatten_rev_seqs =
    let rec loop acc = function
      | [] -> acc
      | section :: rest ->
        let acc =
          match section with
          | Empty -> acc
          | Seq arr -> Array.Immutable.fold_right arr ~init:acc ~f:List.cons
          | (Singleton _ | Par _) as t -> t :: acc
        in
        loop acc rest
    in
    fun (sections : 'node t list) ->
      match loop [] sections with
      | [] -> Empty
      | [ t ] -> t
      | _ :: _ :: _ as flat -> Seq (Array.Immutable.of_list flat)
  ;;

  (* Flatten a parallel section of [num_threads] threads, where thread [i]'s section is
     [f i], flattening nested [Par]s: (x | (y | z)) = (x | y | z). Building the flattened
     list directly from [f] avoids allocating an intermediate list of the threads' sections. *)
  let flatten_pars_init num_threads ~f =
    let rec loop i acc =
      if i < 0
      then acc
      else (
        let acc =
          match f i with
          | Empty -> acc
          | Par arr -> Array.Immutable.fold_right arr ~init:acc ~f:List.cons
          | (Singleton _ | Seq _) as t -> t :: acc
        in
        loop (i - 1) acc)
    in
    match loop (num_threads - 1) [] with
    | [] -> Empty
    | [ t ] -> t
    | _ :: _ :: _ as flat -> Par (Array.Immutable.of_list flat)
  ;;
end

type 'node t = 'node Static.t

let empty = Static.Empty

let is_empty = function
  | Static.Empty -> true
  | Singleton _ | Seq _ | Par _ -> false
;;

module Dynamic = struct
  (* The most recently discovered section is at the head of the list. *)
  type 'node t = 'node Static.t list

  let empty = []

  let append_seq t ~node =
    Counter.incr Metrics.Compute.edges;
    Static.Singleton node :: t
  ;;

  let append_par_init t ~num_threads ~f =
    match Static.flatten_pars_init num_threads ~f with
    | Static.Empty -> t
    | section -> section :: t
  ;;

  let to_static (t : 'node t) : 'node Static.t = Static.flatten_rev_seqs t
end

(* Note that dependencies should be checked in the order in which they were depended on to
   avoid recomputations of dependencies that are no longer relevant, and to eliminate
   spurious dependency cycles. This is why [changed_or_not] checks sequential sections in
   order rather than in parallel. *)
let changed_or_not =
  let rec loop ~f ~ok_to_recompute_eagerly (t : 'node Static.t) =
    match t with
    | Empty -> Fiber.return Changed_or_not.Unchanged
    | Singleton node ->
      Counter.add Metrics.Restore.edges 1;
      f ~ok_to_recompute_eagerly node
    | Seq arr -> seq ~f arr 0
    | Par arr ->
      Fiber.map_reduce_array
        (Array.Immutable.to_array_unsafe arr)
        ~empty:Changed_or_not.Unchanged
        ~combine:Changed_or_not.combine
        ~f:(fun section ->
          match section with
          | Static.Singleton node ->
            Counter.add Metrics.Restore.edges 1;
            f ~ok_to_recompute_eagerly:true node
          | other -> loop ~f ~ok_to_recompute_eagerly:false other)
  and seq ~f arr index =
    let length = Array.Immutable.length arr in
    if index < length
    then (
      let result =
        loop ~f ~ok_to_recompute_eagerly:false (Array.Immutable.get arr index)
      in
      if index + 1 = length
      then result
      else
        result
        >>= function
        | Changed_or_not.Unchanged -> seq ~f arr (index + 1)
        | (Changed | Cancelled _) as res -> Fiber.return res)
    else Fiber.return Changed_or_not.Unchanged
  in
  fun (t : 'node t) ~f -> loop ~f ~ok_to_recompute_eagerly:false t
;;

module For_debugging = struct
  let to_list (t : 'node t) =
    let rec loop acc = function
      | Static.Empty -> acc
      | Singleton node -> node :: acc
      | Seq arr | Par arr ->
        Array.Immutable.fold_right arr ~init:acc ~f:(fun t acc -> loop acc t)
    in
    loop [] t
  ;;

  let to_dyn node_to_dyn t =
    let rec loop (t : _ Static.t) =
      match t with
      | Empty -> Dyn.variant "Empty" []
      | Singleton node -> Dyn.variant "Singleton" [ node_to_dyn node ]
      | Seq arr -> Dyn.variant "Seq" [ Dyn.list loop (Array.Immutable.to_list arr) ]
      | Par arr -> Dyn.variant "Par" [ Dyn.list loop (Array.Immutable.to_list arr) ]
    in
    loop t
  ;;
end
