open Import

module Event = struct
  type t =
    | Live
    | Validated
end

(** Memo nodes can have some special features, for example, [cutoff]
    predicates. *)
module Node_kind : sig
  type ('i, 'o) t

  val create
    :  cutoff:('o -> 'o -> bool) option
    -> on_event:('i -> Event.t -> unit) option
    -> ('i, 'o) t

  (** Does the [new_value] differ from the [old_value]? *)
  val output_changed : ('i, 'o) t -> old_value:'o -> new_value:'o -> bool

  (** If this function returns [false], [output_changed] is guaranteed to return
      [true] for any pair of values. *)
  val has_cutoff : _ t -> bool

  val with_replay : cutoff:('o -> 'o -> bool) -> ('i -> 'o -> unit) -> ('i, 'o) t
  val has_replay : _ t -> bool
  val has_on_event_or_replay : _ t -> bool
  val replay : ('i, 'o) t -> 'i -> 'o -> unit

  (** Notify the node about an event. *)
  val notify : ('i, _) t -> 'i -> Event.t -> unit
end = struct
  (* Ordinary nodes flatten two options to avoid unnecessary indirections.
     Replay nodes have their own variant and require an early cutoff. *)
  type ('i, 'o) t =
    | Vanilla
    | With_event_tracker of { on_event : 'i -> Event.t -> unit }
    | With_cutoff of { equal : 'o -> 'o -> bool }
    | With_cutoff_and_event_tracker of
        { equal : 'o -> 'o -> bool
        ; on_event : 'i -> Event.t -> unit
        }
    | With_replay of
        { equal : 'o -> 'o -> bool
        ; replay : 'i -> 'o -> unit
        }

  let create ~cutoff ~on_event =
    match cutoff, on_event with
    | None, None -> Vanilla
    | None, Some on_event -> With_event_tracker { on_event }
    | Some equal, None -> With_cutoff { equal }
    | Some equal, Some on_event -> With_cutoff_and_event_tracker { equal; on_event }
  ;;

  let output_changed t ~old_value ~new_value =
    match t with
    | Vanilla | With_event_tracker _ -> true
    | With_cutoff { equal }
    | With_cutoff_and_event_tracker { equal; on_event = _ }
    | With_replay { equal; replay = _ } -> not (equal old_value new_value)
  ;;

  let has_cutoff = function
    | Vanilla | With_event_tracker _ -> false
    | With_cutoff _ | With_cutoff_and_event_tracker _ | With_replay _ -> true
  ;;

  let notify t input event =
    match t with
    | Vanilla | With_cutoff _ | With_replay _ -> ()
    | With_event_tracker { on_event }
    | With_cutoff_and_event_tracker { on_event; equal = _ } -> on_event input event
  ;;

  let with_replay ~cutoff:equal replay = With_replay { equal; replay }

  let has_replay = function
    | With_replay _ -> true
    | Vanilla | With_event_tracker _ | With_cutoff _ | With_cutoff_and_event_tracker _ ->
      false
  ;;

  let replay t input output =
    match t with
    | With_replay { replay; equal = _ } -> replay input output
    | Vanilla | With_event_tracker _ | With_cutoff _ | With_cutoff_and_event_tracker _ ->
      ()
  ;;

  let has_on_event_or_replay = function
    | Vanilla | With_cutoff _ -> false
    | With_event_tracker _ | With_cutoff_and_event_tracker _ | With_replay _ -> true
  ;;
end

type ('i, 'o) t =
  { name : string option
  ; (* [witness] is [Some] only for named tables, which may be downcast to their
         input type via [as_instance_of]; it is [None] for lazy values, stack
         frames, and variables, avoiding a [Type_eq.Id.t] allocation per such cell.
         If [witness] precedes the functional values ([input], [f], and the
         closures inside [node_kind]), polymorphic comparison works for [Spec.t]s. *)
    witness : 'i Type_eq.Id.t option
  ; input : (module Store_intf.Input with type t = 'i)
  ; node_kind : ('i, 'o) Node_kind.t
  ; f : 'i -> 'o Fiber.t
  ; human_readable_description : ('i -> User_message.Style.t Pp.t option) option
  }

let create ~name ~input ~human_readable_description ~cutoff ?(witness = false) ?on_event f
  =
  let name =
    match name with
    | None when !Memo_debug.track_locations_of_lazy_values ->
      Option.map (Caller_id.get ~skip:[ __FILE__ ]) ~f:(fun loc ->
        sprintf "lazy value created at %s" (Loc.to_file_colon_line loc))
    | _ -> name
  in
  { name
  ; input
  ; node_kind = Node_kind.create ~cutoff ~on_event
  ; witness = (if witness then Some (Type_eq.Id.create ()) else None)
  ; f
  ; human_readable_description
  }
;;

let output_changed t ~old_value ~new_value =
  Node_kind.output_changed t.node_kind ~old_value ~new_value
;;

let has_cutoff t = Node_kind.has_cutoff t.node_kind
let notify t input event = Node_kind.notify t.node_kind input event

let create_with_replay ~name ~input ~cutoff ~replay f =
  let spec =
    create
      ~name:(Some name)
      ~input
      ~human_readable_description:None
      ~cutoff:None
      ~witness:true
      f
  in
  { spec with node_kind = Node_kind.with_replay ~cutoff replay }
;;

let has_replay t = Node_kind.has_replay t.node_kind
let has_on_event_or_replay t = Node_kind.has_on_event_or_replay t.node_kind
let replay t input output = Node_kind.replay t.node_kind input output
