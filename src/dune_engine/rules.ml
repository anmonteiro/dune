open Import
module Id = Id.Make ()
module Producer_id = Id

type producer =
  | Producer : (unit, 'a) Memo.Node.t -> producer
  | Prepared : (unit, 'a) Memo.Node.t * (unit, 'a) Memo.Node.t -> producer

type refinement =
  { id : Producer_id.t
  ; mask : Target_mask.t
  }

module Dir_rules = struct
  module Alias_spec = struct
    type item =
      | Deps of unit Action_builder.t
      | Action of Rule.Anonymous_action.t

    type t = { expansions : (Loc.t * item) Appendable_list.t } [@@unboxed]

    let union x y = { expansions = Appendable_list.( @ ) x.expansions y.expansions }
  end

  type alias =
    { name : Alias.Name.t
    ; spec : Alias_spec.t
    }

  type data =
    | Rule of Rule.t
    | Alias of alias

  type t = data Id.Map.t

  let dyn_of_data = function
    | Rule rule ->
      Dyn.Variant ("Rule", [ Record [ "targets", Targets.Validated.to_dyn rule.targets ] ])
    | Alias alias ->
      Dyn.Variant ("Alias", [ Record [ "name", Alias.Name.to_dyn alias.name ] ])
  ;;

  let to_dyn t = Dyn.(list dyn_of_data) (Id.Map.values t)

  type ready =
    { rules : Rule.t list
    ; aliases : Alias_spec.t Alias.Name.Map.t
    }

  let consume t =
    let rules, aliases =
      Id.Map.values t
      |> List.partition_map ~f:(function
        | Rule rule -> Left rule
        | Alias { name; spec } -> Right (name, spec))
    in
    let aliases =
      let add_item what = function
        | None -> Some what
        | Some base -> Some (Alias_spec.union what base)
      in
      (* This accumulates the aliases in reverse order, but there's another
         reversal whenever the expansion is inspected. The order doesn't really
         matter, but it does change the tests. So it's nice to maintain it if
         possible *)
      List.fold_left aliases ~init:Alias.Name.Map.empty ~f:(fun acc (name, item) ->
        Alias.Name.Map.update acc name ~f:(add_item item))
    in
    { rules; aliases }
  ;;

  let empty = Id.Map.empty
  let union_map a b ~f = Id.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union a b =
    if a == b
    then a
    else if Id.Map.is_empty a
    then b
    else (
      (* Most emissions are singletons. [Map.union] would split and rebuild
         that singleton at each level of the accumulated map. *)
      match Id.Map.min_binding b with
      | None -> a
      | Some (id, data) when Id.Map.for_alli b ~f:(fun key _ -> Id.equal key id) ->
        Id.Map.update a id ~f:(function
          | None -> Some data
          | Some previous as result ->
            assert (previous == data);
            result)
      | Some _ ->
        union_map a b ~f:(fun a b ->
          assert (a == b);
          a))
  ;;

  let singleton (data : data) =
    let id = Id.gen () in
    Id.Map.singleton id data
  ;;

  let add t data =
    let id = Id.gen () in
    Id.Map.set t id data
  ;;

  let is_empty = Id.Map.is_empty

  module Nonempty : sig
    type maybe_empty = t
    type t = private maybe_empty

    val create : maybe_empty -> t option
    val to_dyn : t -> Dyn.t
    val union : t -> t -> t
    val singleton : data -> t
    val add : t -> data -> t
  end = struct
    type maybe_empty = t
    type nonrec t = t

    let to_dyn = to_dyn
    let create t = if is_empty t then None else Some t
    let union = union
    let singleton = singleton
    let add = add
  end
end

let rule_mask (rule : Rule.t) = Target_mask.of_targets rule.targets

module Mask_index = struct
  (* Aggregate ownership lets a target request skip whole groups of unrelated
     producers. The index structure is immutable; its caches only store pure
     mask queries, never Memo computations. *)
  type summary =
    { mask : Target_mask.t
    ; mutable alias_directories : (Path.Build.t * Path.Unspecified.w Dir_set.t) option
    }

  let summarize mask = { mask; alias_directories = None }

  let alias_directories t ~dir =
    match t.alias_directories with
    | Some (previous_dir, directories) when Path.Build.equal previous_dir dir ->
      directories
    | None | Some _ ->
      let directories = Target_mask.alias_directories t.mask ~dir in
      t.alias_directories <- Some (dir, directories);
      directories
  ;;

  type 'a t =
    | Empty
    | Leaf of summary * 'a
    | Branch of summary * int * 'a t * 'a t

  let empty_summary = summarize Target_mask.empty

  let summary = function
    | Empty -> empty_summary
    | Leaf (summary, _) | Branch (summary, _, _, _) -> summary
  ;;

  let mask t = (summary t).mask

  let height = function
    | Empty -> 0
    | Leaf _ -> 1
    | Branch (_, height, _, _) -> height
  ;;

  let branch left right =
    match left, right with
    | Empty, _ -> right
    | _, Empty -> left
    | _ ->
      let left_summary = summary left in
      let right_summary = summary right in
      let mask = Target_mask.union left_summary.mask right_summary.mask in
      let summary =
        if mask == left_summary.mask
        then left_summary
        else if mask == right_summary.mask
        then right_summary
        else summarize mask
      in
      Branch (summary, 1 + Int.max (height left) (height right), left, right)
  ;;

  let balance left right =
    match left, right with
    | Branch (_, left_height, ll, lr), _ when left_height > height right + 1 ->
      (match lr with
       | Branch (_, _, lrl, lrr) when height lr > height ll ->
         branch (branch ll lrl) (branch lrr right)
       | _ -> branch ll (branch lr right))
    | _, Branch (_, right_height, rl, rr) when right_height > height left + 1 ->
      (match rl with
       | Branch (_, _, rll, rlr) when height rl > height rr ->
         branch (branch left rll) (branch rlr rr)
       | _ -> branch (branch left rl) rr)
    | _ -> branch left right
  ;;

  (* Concatenate ordered, disjoint leaves while keeping selection logarithmic
     through repeated inheritance. Existing subtrees retain their summaries. *)
  let rec concat left right =
    match left, right with
    | Empty, _ -> right
    | _, Empty -> left
    | Branch (_, left_height, ll, lr), _ when left_height > height right + 1 ->
      balance ll (concat lr right)
    | _, Branch (_, right_height, rl, rr) when right_height > height left + 1 ->
      balance (concat left rl) rr
    | _ -> branch left right
  ;;

  let of_list entries =
    let entries = Array.of_list entries in
    let rec build start length =
      match length with
      | 0 -> Empty
      | 1 ->
        let mask, entry = entries.(start) in
        Leaf (summarize mask, entry)
      | _ ->
        let left_length = length / 2 in
        let left = build start left_length in
        let right = build (start + left_length) (length - left_length) in
        branch left right
    in
    build 0 (Array.length entries)
  ;;
end

module File_postings = struct
  type directory =
    { names : (Filename.t, Id.t list) Table.t
    ; owners : Id.t list
    }

  type t = directory Path.Build.Map.t

  type pending =
    { postings : t Lazy.t
    ; excluded : Id.Set.t
    }

  let add directories id ~dir filenames =
    if not (Filename.Set.is_empty filenames)
    then (
      let names, owners =
        match Path.Build.Map.find !directories dir with
        | Some directory -> directory
        | None ->
          let directory = Table.create (module Filename) 32, ref [] in
          directories := Path.Build.Map.set !directories dir directory;
          directory
      in
      owners := id :: !owners;
      Filename.Set.iter filenames ~f:(fun name -> Table.Multi.cons names name id))
  ;;

  let finish directories =
    Path.Build.Map.map !directories ~f:(fun (names, owners) ->
      { names; owners = !owners })
  ;;

  let create entries =
    let rec declarations acc = function
      | [] -> Some (Array.of_list (List.rev acc))
      | (mask, (id, _)) :: rest ->
        if not (Target_mask.is_file_only mask)
        then None
        else (
          match Target_mask.exact_file_names mask with
          | None -> None
          | Some names -> declarations ((id, names) :: acc) rest)
    in
    Option.map (declarations [] entries) ~f:(fun declarations ->
      lazy
        (* Tables are populated synchronously, then remain read-only once this
           lazy value is published. Directory ownership need not scan names. *)
        (let directories = ref Path.Build.Map.empty in
         Array.iter declarations ~f:(fun (id, by_dir) ->
           Path.Build.Map.iteri by_dir ~f:(fun dir filenames ->
             add directories id ~dir filenames));
         finish directories))
  ;;

  let find t ~dir name =
    match Path.Build.Map.find t dir with
    | None -> []
    | Some { names; _ } -> Table.Multi.find names name
  ;;

  let mem_path { postings; excluded } ~dir name =
    List.exists
      (find (Lazy.force postings) ~dir name)
      ~f:(fun id -> not (Id.Set.mem excluded id))
  ;;

  let mem_file t path =
    match Path.Build.parent path with
    | None -> false
    | Some dir -> mem_path t ~dir (Path.Build.basename path)
  ;;

  let intersects_directory { postings; excluded } dir =
    Path.Build.Map.existsi (Lazy.force postings) ~f:(fun owner_dir { owners; _ } ->
      Path.Build.is_descendant owner_dir ~of_:dir
      && List.exists owners ~f:(fun id -> not (Id.Set.mem excluded id)))
  ;;
end

module Direct = struct
  type file_index =
    { by_dir : Dir_rules.Nonempty.t Path.Build.Map.t
    ; postings : File_postings.t Lazy.t
    ; rule_count : int
    }

  type index =
    | Small of
        { by_dir : Dir_rules.Nonempty.t Path.Build.Map.t
        ; fallback : (Path.Build.t * Dir_rules.Nonempty.t) Mask_index.t Lazy.t
        }
    | Indexed of (Path.Build.t * Dir_rules.Nonempty.t) Mask_index.t
    | Files of file_index

  type metadata =
    { target_names : (Filename.Set.t * Filename.Set.t) Path.Build.Map.t
    ; directory_targets : (Id.t * Loc.t) Path.Build.Map.t
    ; directory_target_locations : Loc.t Path.Build.Map.t Lazy.t
    }

  type t =
    { by_dir : Dir_rules.Nonempty.t Path.Build.Map.t
    ; index : index Lazy.t
    ; metadata : metadata Lazy.t
    }

  let metadata by_dir =
    let target_names, directory_targets =
      Path.Build.Map.foldi
        by_dir
        ~init:(Path.Build.Map.empty, Path.Build.Map.empty)
        ~f:(fun dir rules (target_names, directory_targets) ->
          let rules = (rules : Dir_rules.Nonempty.t :> Dir_rules.t) in
          let bulk_files =
            Id.Map.cardinal rules >= 16
            && Id.Map.for_all rules ~f:(function
              | Alias _ -> false
              | Rule rule ->
                Filename.Set.is_empty rule.targets.dirs
                && not (Filename.Set.is_empty rule.targets.files))
          in
          let files, dirs, directory_targets =
            if bulk_files
            then (
              let files =
                Id.Map.fold rules ~init:[] ~f:(fun data files ->
                  match data with
                  | Alias _ -> files
                  | Rule rule ->
                    Filename.Set.fold rule.targets.files ~init:files ~f:(fun name files ->
                      name :: files))
                |> Filename.Set.of_list
              in
              files, Filename.Set.empty, directory_targets)
            else
              Id.Map.foldi
                rules
                ~init:(Filename.Set.empty, Filename.Set.empty, directory_targets)
                ~f:(fun id data (files, dirs, directory_targets) ->
                  match data with
                  | Alias _ -> files, dirs, directory_targets
                  | Rule rule ->
                    let directory_targets =
                      Filename.Set.fold
                        rule.targets.dirs
                        ~init:directory_targets
                        ~f:(fun name acc ->
                          let target = Path.Build.relative_fname rule.targets.root name in
                          Path.Build.Map.update acc target ~f:(function
                            | None -> Some (id, Rule.loc rule)
                            | Some _ as previous -> previous))
                    in
                    ( Filename.Set.union files rule.targets.files
                    , Filename.Set.union dirs rule.targets.dirs
                    , directory_targets ))
          in
          let target_names =
            if Filename.Set.is_empty files && Filename.Set.is_empty dirs
            then target_names
            else Path.Build.Map.set target_names dir (files, dirs)
          in
          target_names, directory_targets)
    in
    { target_names
    ; directory_targets
    ; directory_target_locations = lazy (Path.Build.Map.map directory_targets ~f:snd)
    }
  ;;

  let build_index by_dir =
    Path.Build.Map.to_list_map by_dir ~f:(fun dir rules ->
      Id.Map.to_list_map
        (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
        ~f:(fun id data ->
          let mask =
            match data with
            | Rule rule -> rule_mask rule
            | Alias { name; _ } -> Target_mask.aliases [ Alias.make name ~dir ]
          in
          let rules =
            Id.Map.singleton id data |> Dir_rules.Nonempty.create |> Option.value_exn
          in
          mask, (dir, rules)))
    |> List.concat
    |> Mask_index.of_list
  ;;

  let file_index by_dir : file_index option =
    let count = ref 0 in
    let eligible =
      Path.Build.Map.for_alli by_dir ~f:(fun dir rules ->
        Id.Map.for_all
          (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
          ~f:(function
            | Alias _ -> false
            | Rule { targets = { root; files; dirs }; _ } ->
              incr count;
              Path.Build.equal root dir
              && Filename.Set.is_empty dirs
              && not (Filename.Set.is_empty files)))
    in
    if (not eligible) || !count < 16
    then None
    else
      Some
        { by_dir
        ; rule_count = !count
        ; postings =
            lazy
              (let directories = ref Path.Build.Map.empty in
               Path.Build.Map.iteri by_dir ~f:(fun dir rules ->
                 Id.Map.iteri
                   (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
                   ~f:(fun id -> function
                     | Rule { targets; _ } ->
                       File_postings.add directories id ~dir targets.files
                     | Alias _ -> Code_error.raise "Alias in a pure-file direct index" []));
               File_postings.finish directories)
        }
  ;;

  let index by_dir =
    (* A single rule with many outputs still needs indexed name lookup. *)
    let count = ref 0 in
    let names = ref 0 in
    let small_name _ =
      incr names;
      !names <= 16
    in
    let small =
      Path.Build.Map.for_all by_dir ~f:(fun rules ->
        Id.Map.for_all
          (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
          ~f:(fun data ->
            incr count;
            !count <= 4
            &&
            match data with
            | Alias _ -> small_name ()
            | Rule { targets = { files; dirs; _ }; _ } ->
              Filename.Set.for_all files ~f:small_name
              && Filename.Set.for_all dirs ~f:small_name))
    in
    if small
    then Small { by_dir; fallback = lazy (build_index by_dir) }
    else (
      match file_index by_dir with
      | Some files -> Files files
      | None -> Indexed (build_index by_dir))
  ;;

  (* Intermediate unions often only need their rule maps. Derive metadata
     from this map once, without retaining a chain of metadata unions. *)
  let make by_dir =
    { by_dir; metadata = lazy (metadata by_dir); index = lazy (index by_dir) }
  ;;

  let targets t =
    match Lazy.force t.index with
    | Indexed index -> Mask_index.mask index
    | Small { fallback; _ } -> Mask_index.mask (Lazy.force fallback)
    | Files _ ->
      Path.Build.Map.foldi
        (Lazy.force t.metadata).target_names
        ~init:Target_mask.empty
        ~f:(fun dir (files, _) mask ->
          Target_mask.union mask (Target_mask.files_named ~dir files))
  ;;

  let empty = make Path.Build.Map.empty
  let create by_dir = if Path.Build.Map.is_empty by_dir then empty else make by_dir
  let is_empty t = Path.Build.Map.is_empty t.by_dir
  let target_names t = (Lazy.force t.metadata).target_names
  let directory_targets t = (Lazy.force t.metadata).directory_targets

  let directory_target_locations t =
    Lazy.force (Lazy.force t.metadata).directory_target_locations
  ;;

  let union_directory_targets a b =
    Path.Build.Map.union a b ~f:(fun _ ((id_a, _) as a) ((id_b, _) as b) ->
      Some
        (match Id.compare id_a id_b with
         | Lt | Eq -> a
         | Gt -> b))
  ;;

  let union a b =
    if a == b || is_empty b
    then a
    else if is_empty a
    then b
    else
      make
        (Path.Build.Map.union a.by_dir b.by_dir ~f:(fun _ a b ->
           Some (Dir_rules.Nonempty.union a b)))
  ;;
end

type revealed =
  { chunks : Direct.t list Lazy.t
  ; directories : Path.Build.Set.t Lazy.t
  ; directory_targets : Loc.t Path.Build.Map.t Lazy.t
  }

type pending_item =
  | Summary of Mask_index.summary
  | Files of File_postings.pending

module T = struct
  type t =
    { direct : Direct.t
    ; suspensions : suspension Id.Map.t
    ; suspension_index : suspension_index Lazy.t
    ; targets : Target_mask.t Lazy.t
    ; mutable routing : routing option
    ; mutable watch_route : watch_route option
    }

  and suspension =
    { mask : Target_mask.t
    ; rules : t Memo.t
    ; producer : producer
    ; observed : observation ref
    ; underlying : suspension option
    }

  and observation =
    | Unforced
    | Evaluating
    | Produced of t

  and suspension_index =
    | Indexed of (Id.t * suspension) Mask_index.t
    | Large of large_suspensions
    | Joined of joined_suspensions

  and large_suspensions =
    { generic_index : (Id.t * suspension) Mask_index.t Lazy.t
    ; file_postings : File_postings.t Lazy.t option Lazy.t
    ; producers : suspension Id.Map.t
    ; producer_count : int
    }

  and joined_suspensions =
    { aggregate_mask : Target_mask.t
    ; components : suspension_index list
    ; initial_frontiers : suspension_frontier list
    ; fallback : (Id.t * suspension) Mask_index.t Lazy.t
    }

  and suspension_frontier =
    | Generic_suspensions of (Id.t * suspension) Mask_index.t
    | Unindexed_suspensions of large_suspensions
    | Posted_suspensions of large_suspensions * File_postings.pending

  and route_prefix =
    { chunks : Direct.t list
    ; pending : pending_item list
    ; refinements : refinement list
    }

  and route =
    { producer : suspension
    ; prefix : route_prefix
    }

  and route_family =
    { group : large_suspensions
    ; postings : File_postings.t Lazy.t
    ; prefix : route_prefix
    ; routes : (Id.t, route option) Table.t
    }

  and loaded =
    { selected : t
    ; revealed : revealed
    ; pending : pending_item list
    ; refinements : refinement list
    }

  and direct_route_family =
    { producing_id : Id.t
    ; producer : suspension
    ; body : t
    ; index : Direct.file_index
    ; outside : Direct.t list
    ; revealed : revealed
    ; pending : pending_item list
    ; refinements : refinement list
    ; selections : (Id.t, loaded option) Table.t
    }

  and routing =
    { run : Memo.Run.t
    ; mutable families : route_family list
    ; mutable direct_families : direct_route_family list
    ; mutable visited : Id.Set.t
    }

  and watch_route =
    { family : direct_route_family
    ; steps : watch_route_step list
    }

  and watch_route_step =
    { parent : t
    ; id : Id.t
    ; producer : suspension
    ; body : t
    ; pending : suspension_frontier list
    }

  let generic_index = function
    | Indexed index -> index
    | Large { generic_index; _ } -> Lazy.force generic_index
    | Joined { fallback; _ } -> Lazy.force fallback
  ;;

  let index_mask = function
    | Joined { aggregate_mask; _ } -> aggregate_mask
    | index -> Mask_index.mask (generic_index index)
  ;;

  let initial_frontiers = function
    | Indexed Mask_index.Empty -> []
    | Indexed index -> [ Generic_suspensions index ]
    | Large group -> [ Unindexed_suspensions group ]
    | Joined { initial_frontiers; _ } -> initial_frontiers
  ;;

  let empty =
    { direct = Direct.empty
    ; suspensions = Id.Map.empty
    ; suspension_index = lazy (Indexed Mask_index.Empty)
    ; targets = lazy Target_mask.empty
    ; routing = None
    ; watch_route = None
    }
  ;;

  let make ~direct ~suspensions ~suspension_index =
    if Direct.is_empty direct && Id.Map.is_empty suspensions
    then empty
    else
      { direct
      ; suspensions
      ; suspension_index
      ; routing = None
      ; watch_route = None
      ; targets =
          lazy
            (Target_mask.union
               (Direct.targets direct)
               (index_mask (Lazy.force suspension_index)))
      }
  ;;

  let index_suspensions suspensions =
    let entries =
      Id.Map.to_list_map suspensions ~f:(fun id suspension ->
        suspension.mask, (id, suspension))
    in
    let producer_count = List.length entries in
    if producer_count < 16
    then Indexed (Mask_index.of_list entries)
    else
      Large
        { generic_index = lazy (Mask_index.of_list entries)
        ; file_postings = lazy (File_postings.create entries)
        ; producers = suspensions
        ; producer_count =
            (* Empty owners cannot be followed or keep a frontier alive. *)
            List.fold_left entries ~init:0 ~f:(fun count (mask, _) ->
              if Target_mask.is_empty mask then count else count + 1)
        }
  ;;

  let create ~direct ~suspensions =
    let suspension_index =
      if Id.Map.is_empty suspensions
      then empty.suspension_index
      else lazy (index_suspensions suspensions)
    in
    make ~direct ~suspensions ~suspension_index
  ;;

  let is_empty t = Direct.is_empty t.direct && Id.Map.is_empty t.suspensions

  let precedes a b =
    match Id.Map.max_binding a, Id.Map.min_binding b with
    | Some (last, _), Some (first, _) ->
      (match Id.compare last first with
       | Lt -> true
       | Eq | Gt -> false)
    | _ -> false
  ;;

  let join a b =
    let components = function
      | Joined { components; _ } -> components
      | index -> [ index ]
    in
    let components = components a @ components b in
    if List.length components > 8
    then Indexed (Mask_index.concat (generic_index a) (generic_index b))
    else
      (* Keep small inherited groups separate: their exact ownership and
         postings already exist. Only the whole-group mask needs a new union. *)
      Joined
        { aggregate_mask = Target_mask.union (index_mask a) (index_mask b)
        ; components
        ; initial_frontiers = List.concat_map components ~f:initial_frontiers
        ; fallback =
            lazy
              (List.fold_left components ~init:Mask_index.Empty ~f:(fun index component ->
                 Mask_index.concat index (generic_index component)))
        }
  ;;

  let union a b =
    if a == b || is_empty b
    then a
    else if is_empty a
    then b
    else (
      let suspensions =
        if a.suspensions == b.suspensions
        then a.suspensions
        else
          Id.Map.union a.suspensions b.suspensions ~f:(fun _ a b ->
            assert (a == b);
            Some a)
      in
      let suspension_index =
        if suspensions == a.suspensions
        then a.suspension_index
        else if suspensions == b.suspensions
        then b.suspension_index
        else if not (Lazy.is_val a.suspension_index || Lazy.is_val b.suspension_index)
        then lazy (index_suspensions suspensions)
        else (
          (* Reuse already built ancestors, but never retain a chain of lazy
             joins that would build every intermediate union when forced. *)
          let a_suspensions = a.suspensions in
          let b_suspensions = b.suspensions in
          let flat_or_shared index suspensions =
            if Lazy.is_val index then index else lazy (index_suspensions suspensions)
          in
          let a_index = flat_or_shared a.suspension_index a_suspensions in
          let b_index = flat_or_shared b.suspension_index b_suspensions in
          lazy
            (let concat a b = join (Lazy.force a) (Lazy.force b) in
             if precedes a_suspensions b_suspensions
             then concat a_index b_index
             else if precedes b_suspensions a_suspensions
             then concat b_index a_index
             else index_suspensions suspensions))
      in
      make ~direct:(Direct.union a.direct b.direct) ~suspensions ~suspension_index)
  ;;

  let name = "Rules"
end

include T

let repr =
  Repr.record
    "rules"
    [ Repr.field
        "direct"
        (Repr.abstract (Path.Build.Map.to_dyn Dir_rules.Nonempty.to_dyn))
        ~get:(fun t -> t.direct.by_dir)
    ; Repr.field "suspensions" Repr.int ~get:(fun t -> Id.Map.cardinal t.suspensions)
    ]
;;

let to_dyn = Repr.to_dyn repr

let singleton_rule (rule : Rule.t) =
  let dir = rule.targets.root in
  create
    ~direct:
      (Path.Build.Map.singleton dir (Dir_rules.Nonempty.singleton (Rule rule))
       |> Direct.create)
    ~suspensions:Id.Map.empty
;;

let implicit_output = Memo.Implicit_output.add (module T)

let produce rules =
  if is_empty rules
  then Memo.return ()
  else Memo.Implicit_output.produce implicit_output rules
;;

module Produce = struct
  let rule (rule : Rule.t) =
    Dune_trace.emit Debug (fun () ->
      let { Targets.Validated.root; files; dirs } = rule.targets in
      Dune_trace.Event.rule_generated { root; files; dirs });
    produce (singleton_rule rule)
  ;;

  module Alias = struct
    type t = Alias.t

    let alias t spec =
      produce
        (let dir = Alias.dir t in
         let name = Alias.name t in
         create
           ~direct:
             (Path.Build.Map.singleton
                dir
                (Dir_rules.Nonempty.singleton (Alias { name; spec }))
              |> Direct.create)
           ~suspensions:Id.Map.empty)
    ;;

    let add_deps t ?(loc = Loc.none) expansion =
      alias
        t
        { expansions = Appendable_list.singleton (loc, Dir_rules.Alias_spec.Deps expansion)
        }
    ;;

    (* All aliases in [ts] are expected to share a directory: the shared
       anonymous action is created in the representative's directory. *)
    let add_action ts ~loc action =
      let representative =
        match ts with
        | [] -> Code_error.raise "Rules.Produce.Alias.add_action: empty list" []
        | r :: _ -> r
      in
      let anon = Rule.Anonymous_action.make ~loc ~dir:(Alias.dir representative) action in
      Memo.parallel_iter ts ~f:(fun t ->
        alias
          t
          { expansions = Appendable_list.singleton (loc, Dir_rules.Alias_spec.Action anon)
          })
    ;;
  end
end

let of_dir_rules ~dir rules =
  match Dir_rules.Nonempty.create rules with
  | None -> empty
  | Some rules ->
    create
      ~direct:(Direct.create (Path.Build.Map.singleton dir rules))
      ~suspensions:Id.Map.empty
;;

let of_rules rules =
  let direct =
    List.fold_left rules ~init:Path.Build.Map.empty ~f:(fun acc rule ->
      Path.Build.Map.update acc rule.Rule.targets.root ~f:(function
        | None -> Some (Dir_rules.Nonempty.singleton (Rule rule))
        | Some acc -> Some (Dir_rules.Nonempty.add acc (Rule rule))))
  in
  create ~direct:(Direct.create direct) ~suspensions:Id.Map.empty
;;

let directory_targets (rules : t) = Direct.directory_target_locations rules.direct

let target_names (rules : t) ~dir =
  Path.Build.Map.find (Direct.target_names rules.direct) dir
  |> Option.value ~default:(Filename.Set.empty, Filename.Set.empty)
;;

module Revealed = struct
  (* Cleanup needs metadata from all revealed rules, but only selected rules
     need a merged rule tree. Keep the original direct chunks shared. *)
  type t = revealed

  let create chunks : t =
    let chunks = lazy (Appendable_list.to_list chunks) in
    { chunks
    ; directories =
        lazy
          (List.fold_left
             (Lazy.force chunks)
             ~init:Path.Build.Set.empty
             ~f:(fun acc chunk ->
               Path.Build.Map.foldi chunk.Direct.by_dir ~init:acc ~f:(fun dir _ acc ->
                 Path.Build.Set.add acc dir)))
    ; directory_targets =
        lazy
          (List.fold_left
             (Lazy.force chunks)
             ~init:Path.Build.Map.empty
             ~f:(fun acc chunk ->
               Direct.union_directory_targets acc (Direct.directory_targets chunk))
           |> Path.Build.Map.map ~f:snd)
    }
  ;;

  let of_rules (rules : T.t) = create (Appendable_list.singleton rules.direct)
  let directories t = Lazy.force t.directories
  let directory_targets t = Lazy.force t.directory_targets

  let same_components (a : t) (b : t) =
    a == b || List.equal ( == ) (Lazy.force a.chunks) (Lazy.force b.chunks)
  ;;

  let target_names (t : t) ~dir =
    List.fold_left
      (Lazy.force t.chunks)
      ~init:(Filename.Set.empty, Filename.Set.empty)
      ~f:(fun ((files, dirs) as acc) chunk ->
        match Path.Build.Map.find (Direct.target_names chunk) dir with
        | None -> acc
        | Some (more_files, more_dirs) ->
          Filename.Set.union files more_files, Filename.Set.union dirs more_dirs)
  ;;

  let find (t : t) ~dir =
    List.fold_left (Lazy.force t.chunks) ~init:Dir_rules.empty ~f:(fun acc chunk ->
      match Path.Build.Map.find chunk.Direct.by_dir dir with
      | None -> acc
      | Some rules -> Dir_rules.union acc (rules : Dir_rules.Nonempty.t :> Dir_rules.t))
  ;;
end

let collect f =
  let open Memo.O in
  let+ result, out = Memo.Implicit_output.collect implicit_output f in
  result, Option.value out ~default:T.empty
;;

let collect_unit f =
  let open Memo.O in
  let+ (), rules = collect f in
  rules
;;

let rec restrict t mask =
  let check ~dir name matches =
    if not matches
    then
      Code_error.raise
        "Rule stage produced a target outside its mask"
        [ "target", Path.Build.to_dyn (Path.Build.relative_fname dir name) ]
  in
  Path.Build.Map.iteri t.direct.by_dir ~f:(fun dir rules ->
    let rules = (rules : Dir_rules.Nonempty.t :> Dir_rules.t) in
    let remaining = ref 4 in
    let few_rules =
      Id.Map.for_all rules ~f:(fun _ ->
        decr remaining;
        !remaining > 0)
    in
    let files_covered =
      (not few_rules)
      &&
      let all_files = Target_mask.files_in_directory dir in
      Target_mask.inter mask all_files == all_files
    in
    Id.Map.iter rules ~f:(function
      | Rule { targets = { root; files; dirs }; _ } ->
        if not (files_covered && Path.Build.equal root dir)
        then
          Filename.Set.iter files ~f:(fun name ->
            check ~dir:root name (Target_mask.mem_file_name mask ~dir:root name));
        Filename.Set.iter dirs ~f:(fun name ->
          check ~dir:root name (Target_mask.mem_directory_name mask ~dir:root name))
      | Alias { name; _ } ->
        let alias = Alias.make name ~dir in
        if not (Target_mask.mem_alias mask alias)
        then
          Code_error.raise
            "Rule stage produced an alias outside its mask"
            [ "alias", Alias.to_dyn alias ]));
  (* Each suspension already validates its own outputs. An unchanged mask
     does not need another memoized validation layer. *)
  let suspensions =
    Id.Map.foldi
      t.suspensions
      ~init:t.suspensions
      ~f:(fun id ({ mask = child_mask; rules; _ } as underlying) acc ->
        let mask = Target_mask.inter mask child_mask in
        if mask == child_mask
        then acc
        else (
          let observed = ref Unforced in
          let node, restricted =
            Memo.Lazy.Expert.create ~name:"restrict-rule-stage" (fun () ->
              observed := Evaluating;
              let open Memo.O in
              let+ rules = rules in
              let rules = restrict rules mask in
              observed := Produced rules;
              rules)
          in
          Id.Map.set
            (Id.Map.remove acc id)
            (Id.gen ())
            { mask
            ; rules = Memo.Lazy.force restricted
            ; producer = Producer node
            ; observed
            ; underlying = Some underlying
            }))
  in
  if suspensions == t.suspensions then t else create ~direct:t.direct ~suspensions
;;

module Deferred = struct
  type 'a t =
    { result : 'a Memo.Lazy.t
    ; production : ('a * T.t) Memo.Lazy.t
    ; origin : suspension
    }

  let result t = t.result
end

let deferred_suspension mask observed producer shared ~underlying =
  let open Memo.O in
  let rules =
    let+ _, rules = Memo.Lazy.force shared in
    rules
  in
  { mask; rules; producer; observed; underlying }
;;

let produce_deferred suspension shared =
  let open Memo.O in
  let+ () =
    produce
      (create ~direct:Direct.empty ~suspensions:(Id.Map.singleton (Id.gen ()) suspension))
  in
  Memo.lazy_ ~name:"deferred-rule-result" (fun () ->
    let+ result, _ = Memo.Lazy.force shared in
    result)
;;

let defer mask f =
  let open Memo.O in
  let* () = Memo.return () in
  let observed = ref Unforced in
  let node, shared =
    Memo.Lazy.Expert.create ~name:"deferred-rule-production" (fun () ->
      observed := Evaluating;
      let+ result, rules = collect f in
      let rules = restrict rules mask in
      observed := Produced rules;
      result, rules)
  in
  let suspension =
    deferred_suspension mask observed (Producer node) shared ~underlying:None
  in
  produce_deferred suspension shared
;;

let defer_after_with_origin mask ~prepare ~underlying f =
  let open Memo.O in
  let* () = Memo.return () in
  let observed = ref Unforced in
  let production, rules =
    Memo.Lazy.Expert.create ~name:"prepared-rule-production" (fun () ->
      let+ result, rules = collect f in
      result, restrict rules mask)
  in
  let preparation, shared =
    Memo.Lazy.Expert.create ~name:"prepare-rule-production" (fun () ->
      observed := Evaluating;
      let* () = Memo.Implicit_output.forbid (fun () -> prepare) in
      let+ result, rules = Memo.Lazy.force rules in
      observed := Produced rules;
      result, rules)
  in
  let origin =
    deferred_suspension
      mask
      observed
      (Prepared (preparation, production))
      shared
      ~underlying
  in
  let+ result = produce_deferred origin shared in
  { Deferred.result; production = rules; origin }
;;

let defer_after mask ~prepare f = defer_after_with_origin mask ~prepare ~underlying:None f

let narrow_after mask { Deferred.result; production; origin } f =
  let open Memo.O in
  let prepare =
    let+ _ = Memo.Lazy.force result in
    ()
  in
  let+ _ =
    defer_after_with_origin mask ~prepare ~underlying:(Some origin) (fun () ->
      let* result, _ = Memo.Lazy.force production in
      f result)
  in
  ()
;;

let narrow mask f =
  let open Memo.O in
  let* () = Memo.return () in
  let observed = ref Unforced in
  let node, rules =
    Memo.Lazy.Expert.create ~name:"deferred-rule-production" (fun () ->
      observed := Evaluating;
      let+ rules = collect_unit f in
      let rules = restrict rules mask in
      observed := Produced rules;
      rules)
  in
  let rules = Memo.Lazy.force rules in
  produce
    (create
       ~direct:Direct.empty
       ~suspensions:
         (Id.Map.singleton
            (Id.gen ())
            { mask; rules; producer = Producer node; observed; underlying = None }))
;;

let unchanged_since t ~since =
  let checked = Table.create (module Id) 16 in
  let rec unchanged t =
    Id.Map.for_alli t.suspensions ~f:(fun id producer ->
      match Table.find checked id with
      | Some unchanged -> unchanged
      | None ->
        Table.set checked id false;
        let unchanged = unchanged_producer producer in
        Table.set checked id unchanged;
        unchanged)
  and unchanged_producer { producer; observed; underlying; _ } =
    Option.forall underlying ~f:unchanged_producer
    &&
    match !observed with
    | Unforced -> true
    | Evaluating -> false
    | Produced rules ->
      (match producer with
       | Producer node -> Memo.Node.is_unchanged node ~since
       | Prepared (preparation, production) ->
         Memo.Node.is_successfully_cached preparation
         && Memo.Node.is_unchanged production ~since)
      && unchanged rules
  in
  unchanged t
;;

let rule_request (rule : Rule.t) ~directory_only =
  let { Targets.Validated.root; files; dirs } = rule.targets in
  let add_path name acc =
    Target_mask.union acc (Target_mask.path (Path.Build.relative_fname root name))
  in
  let mask = Target_mask.paths ~dir:root files in
  Filename.Set.fold dirs ~init:mask ~f:(fun name mask ->
    let path = Path.Build.relative_fname root name in
    if directory_only
    then Target_mask.union mask (Target_mask.directories [ path ])
    else Target_mask.union (add_path name mask) (Target_mask.subtree path))
;;

let direct_targets direct ~rule_mask =
  Path.Build.Map.foldi direct ~init:Target_mask.empty ~f:(fun dir rules targets ->
    Id.Map.fold
      (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
      ~init:targets
      ~f:(fun data targets ->
        let mask =
          match data with
          | Rule rule -> rule_mask rule
          | Alias { name; _ } -> Target_mask.aliases [ Alias.make name ~dir ]
        in
        Target_mask.union mask targets))
;;

let targets t = Lazy.force t.targets

type direct_frontier =
  | Whole_direct of Direct.t
  | Pruned_direct of (Path.Build.t * Dir_rules.Nonempty.t) Mask_index.t
  | Posted_direct of Direct.file_index * File_postings.pending

let rec filter_direct_index mask index ((acc, pending) as selected) =
  match index with
  | Mask_index.Empty -> selected
  | _ when not (Target_mask.intersects mask (Mask_index.mask index)) ->
    acc, Pruned_direct index :: pending
  | Leaf (_, (dir, rules)) ->
    ( Path.Build.Map.update acc dir ~f:(function
        | None -> Some rules
        | Some previous -> Some (Dir_rules.Nonempty.union previous rules))
    , pending )
  | Branch (_, _, left, right) ->
    filter_direct_index mask right (filter_direct_index mask left selected)
;;

type direct_selection =
  | No_match
  | Selected of Dir_rules.Nonempty.t Path.Build.Map.t * direct_frontier list

let small_direct_flags by_dir mask =
  (* Direct rules have finite exact targets, so membership avoids constructing
     per-rule masks while preserving the file, directory, and alias kinds. *)
  let matches ~dir = function
    | Dir_rules.Rule { targets = { root; files; dirs }; _ } ->
      Filename.Set.exists files ~f:(Target_mask.mem_file_name mask ~dir:root)
      || Filename.Set.exists dirs ~f:(Target_mask.mem_directory_name mask ~dir:root)
    | Alias { name; _ } -> Target_mask.mem_alias mask (Alias.make name ~dir)
  in
  Path.Build.Map.foldi by_dir ~init:0 ~f:(fun dir rules flags ->
    Id.Map.fold
      (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
      ~init:flags
      ~f:(fun data flags -> flags lor if matches ~dir data then 1 else 2))
;;

let filter_indexed_direct frontier index mask =
  match index with
  | Mask_index.Empty -> No_match
  | _ when not (Target_mask.intersects mask (Mask_index.mask index)) -> No_match
  | Leaf (_, (dir, rules)) ->
    let selected =
      match frontier with
      | Whole_direct direct -> direct.by_dir
      | Pruned_direct _ | Posted_direct _ -> Path.Build.Map.singleton dir rules
    in
    Selected (selected, [])
  | Branch (_, _, left, right) ->
    let selected, pending =
      filter_direct_index
        mask
        right
        (filter_direct_index mask left (Path.Build.Map.empty, []))
    in
    if Path.Build.Map.is_empty selected
    then No_match
    else (
      let selected =
        match frontier, pending with
        | Whole_direct direct, [] -> direct.by_dir
        | _ -> selected
      in
      Selected (selected, pending))
;;

let filter_posted_direct
      (index : Direct.file_index)
      ({ File_postings.excluded; _ } as files)
      mask
  =
  let { Direct.by_dir; postings; rule_count } = index in
  let selected = ref Path.Build.Map.empty in
  let remaining = ref excluded in
  let add ~dir id data =
    remaining := Id.Set.add !remaining id;
    selected
    := Path.Build.Map.update !selected dir ~f:(fun previous ->
         let previous =
           match previous with
           | None -> Dir_rules.empty
           | Some rules -> (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
         in
         Dir_rules.Nonempty.create (Id.Map.set previous id data))
  in
  let all_selected =
    match Target_mask.exact_file_names mask with
    | Some names ->
      if not (Path.Build.Map.is_empty names)
      then (
        let postings = Lazy.force postings in
        Path.Build.Map.iteri names ~f:(fun dir names ->
          match Path.Build.Map.find by_dir dir with
          | None -> ()
          | Some rules ->
            Filename.Set.iter names ~f:(fun name ->
              List.iter (File_postings.find postings ~dir name) ~f:(fun id ->
                if not (Id.Set.mem !remaining id)
                then
                  add
                    ~dir
                    id
                    (Id.Map.find (rules : Dir_rules.Nonempty.t :> Dir_rules.t) id
                     |> Option.value_exn)))));
      false
    | None ->
      let matches ~dir = function
        | Dir_rules.Rule { targets; _ } ->
          Filename.Set.exists targets.files ~f:(Target_mask.mem_file_name mask ~dir)
        | Alias _ -> false
      in
      if
        Id.Set.is_empty excluded
        && Path.Build.Map.for_alli by_dir ~f:(fun dir rules ->
          Id.Map.for_all (rules : Dir_rules.Nonempty.t :> Dir_rules.t) ~f:(matches ~dir))
      then true
      else (
        Path.Build.Map.iteri by_dir ~f:(fun dir rules ->
          Id.Map.iteri
            (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
            ~f:(fun id data ->
              if (not (Id.Set.mem excluded id)) && matches ~dir data then add ~dir id data));
        false)
  in
  if all_selected
  then Selected (by_dir, [])
  else if Path.Build.Map.is_empty !selected
  then No_match
  else if Id.Set.cardinal !remaining = rule_count
  then Selected ((if Id.Set.is_empty excluded then by_dir else !selected), [])
  else
    Selected (!selected, [ Posted_direct (index, { files with excluded = !remaining }) ])
;;

let filter_direct_frontier frontier mask =
  match frontier with
  | Pruned_direct index -> filter_indexed_direct frontier index mask
  | Posted_direct (index, files) -> filter_posted_direct index files mask
  | Whole_direct direct ->
    (match Lazy.force direct.index with
     | Small { by_dir; fallback } ->
       if Lazy.is_val fallback
       then filter_indexed_direct frontier (Lazy.force fallback) mask
       else (
         match small_direct_flags by_dir mask with
         | 0 | 2 -> No_match
         | 1 -> Selected (by_dir, [])
         | _ -> filter_indexed_direct frontier (Lazy.force fallback) mask)
     | Indexed index -> filter_indexed_direct frontier index mask
     | Files index ->
       filter_posted_direct
         index
         { File_postings.postings = index.postings; excluded = Id.Set.empty }
         mask)
;;

module Pending = struct
  (* Pruned index branches already summarize their unforced producers. Keep
     those summaries separate instead of copying their complete name sets for
     each target request. *)
  type item = pending_item =
    | Summary of Mask_index.summary
    | Files of File_postings.pending

  type t = item list

  let empty = []

  let same_component a b =
    a == b
    ||
    match a, b with
    | Summary a, Summary b -> a == b
    | Files a, Files b -> a.postings == b.postings && a.excluded == b.excluded
    | Summary _, Files _ | Files _, Summary _ -> false
  ;;

  let same_components a b = a == b || List.equal same_component a b

  let of_mask mask =
    if Target_mask.is_empty mask then [] else [ Summary (Mask_index.summarize mask) ]
  ;;

  let add_index t = function
    | Mask_index.Empty -> t
    | Leaf (summary, _) | Branch (summary, _, _, _) ->
      if Target_mask.is_empty summary.mask then t else Summary summary :: t
  ;;

  let add_frontier t = function
    | Generic_suspensions index -> add_index t index
    | Unindexed_suspensions group -> add_index t (Lazy.force group.generic_index)
    | Posted_suspensions (_, files) -> Files files :: t
  ;;

  let mem_file t path =
    List.exists t ~f:(function
      | Summary { mask; _ } -> Target_mask.mem_file mask path
      | Files files -> File_postings.mem_file files path)
  ;;

  let mem_directory t path =
    List.exists t ~f:(function
      | Summary { mask; _ } -> Target_mask.mem_directory mask path
      | Files _ -> false)
  ;;

  let rec mem_path t ~dir name =
    match t with
    | [] -> false
    | item :: rest ->
      (match item with
       | Summary { mask; _ } -> Target_mask.mem_path mask ~dir name
       | Files files -> File_postings.mem_path files ~dir name)
      || mem_path rest ~dir name
  ;;

  let intersects_directory t dir =
    List.exists t ~f:(function
      | Summary { mask; _ } -> Target_mask.intersects_directory mask dir
      | Files files -> File_postings.intersects_directory files dir)
  ;;

  let alias_directories t ~dir =
    List.fold_left t ~init:Dir_set.empty ~f:(fun acc -> function
      | Files _ -> acc
      | Summary summary ->
        let directories = Mask_index.alias_directories summary ~dir in
        if acc == directories then acc else Dir_set.union acc directories)
  ;;
end

let matching_suspensions indexes mask =
  match indexes with
  | [] | [ Generic_suspensions Mask_index.Empty ] -> [], []
  | [ Generic_suspensions (Leaf ({ mask = owned; _ }, suspension)) ] ->
    if Target_mask.intersects mask owned then [ suspension ], [] else [], indexes
  | _ :: _ ->
    let matching = ref [] in
    let pending = ref [] in
    let exact_names = lazy (Target_mask.exact_file_names mask) in
    let rec select_generic index =
      match index with
      | Mask_index.Empty -> ()
      | Leaf ({ mask = owned; _ }, suspension) when Target_mask.intersects mask owned ->
        matching := suspension :: !matching
      | Branch ({ mask = owned; _ }, _, left, right)
        when Target_mask.intersects mask owned ->
        select_generic left;
        select_generic right
      | _ -> pending := Generic_suspensions index :: !pending
    in
    let select_posted group ({ File_postings.postings; excluded } as files) =
      let remaining = ref excluded in
      let add id producer =
        remaining := Id.Set.add !remaining id;
        matching := (id, producer) :: !matching
      in
      (match Lazy.force exact_names with
       | Some names ->
         if not (Path.Build.Map.is_empty names)
         then (
           let postings = Lazy.force postings in
           Path.Build.Map.iteri names ~f:(fun dir names ->
             Filename.Set.iter names ~f:(fun name ->
               List.iter (File_postings.find postings ~dir name) ~f:(fun id ->
                 if not (Id.Set.mem !remaining id)
                 then add id (Id.Map.find group.producers id |> Option.value_exn)))))
       | None ->
         (* Broad requests can inspect exact owners without building either
            index. A widened request still excludes previously followed owners. *)
         let select id ({ mask = owned; _ } as producer) =
           if (not (Id.Set.mem !remaining id)) && Target_mask.intersects mask owned
           then add id producer
         in
         if Lazy.is_val group.generic_index
         then (
           let rec select_index = function
             | Mask_index.Empty -> ()
             | Leaf (_, (id, producer)) -> select id producer
             | Branch ({ mask = owned; _ }, _, left, right) ->
               if Target_mask.intersects mask owned
               then (
                 select_index left;
                 select_index right)
           in
           select_index (Lazy.force group.generic_index))
         else Id.Map.iteri group.producers ~f:select);
      if Id.Set.cardinal !remaining < group.producer_count
      then (
        let files =
          if !remaining == excluded
          then files
          else { File_postings.postings; excluded = !remaining }
        in
        pending := Posted_suspensions (group, files) :: !pending)
    in
    List.iter indexes ~f:(function
      | Generic_suspensions index -> select_generic index
      | Posted_suspensions (group, files) -> select_posted group files
      | Unindexed_suspensions group ->
        (match Lazy.force group.file_postings with
         | None -> select_generic (Lazy.force group.generic_index)
         | Some postings ->
           select_posted group { File_postings.postings; excluded = Id.Set.empty }));
    (* Later closure passes combine frontiers from several producers. Their
       matching order must not depend on how shared indexes were balanced. *)
    let rec ordered acc = function
      | [] -> acc
      | ((id, _) as producer) :: rest ->
        let compare_previous =
          match acc with
          | [] -> Lt
          | (next_id, _) :: _ -> Id.compare id next_id
        in
        (match compare_previous with
         | Lt | Eq -> ordered (producer :: acc) rest
         | Gt ->
           List.sort
             (List.rev_append rest (producer :: acc))
             ~compare:(fun (a, _) (b, _) -> Id.compare a b))
    in
    ordered [] !matching, !pending
;;

(* A growing request only revisits direct rules it has not selected and producer
   branches it has not followed. This frontier is local to one load, so every
   request still records its own Memo dependencies. *)
type partial =
  { selected : Dir_rules.Nonempty.t Path.Build.Map.t
  ; revealed : Direct.t Appendable_list.t
  ; pending : suspension_frontier list
  ; direct_frontier : direct_frontier list
  ; refinements : refinement list
  }

let files_have_unseen_owner (partial : partial) ~dir files =
  let in_mask mask = Filename.Set.exists files ~f:(Target_mask.mem_path mask ~dir) in
  List.exists partial.direct_frontier ~f:(function
    | Pruned_direct index -> in_mask (Mask_index.mask index)
    | Posted_direct (_, pending) ->
      Filename.Set.exists files ~f:(File_postings.mem_path pending ~dir)
    | Whole_direct direct ->
      (match Lazy.force direct.index with
       | Indexed index -> in_mask (Mask_index.mask index)
       | Files index ->
         let pending =
           { File_postings.postings = index.postings; excluded = Id.Set.empty }
         in
         Filename.Set.exists files ~f:(File_postings.mem_path pending ~dir)
       | Small { by_dir; fallback } ->
         if Lazy.is_val fallback
         then in_mask (Mask_index.mask (Lazy.force fallback))
         else
           Path.Build.Map.exists by_dir ~f:(fun rules ->
             Id.Map.exists
               (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
               ~f:(function
                 | Alias _ -> false
                 | Rule { targets; _ } ->
                   Path.Build.equal targets.root dir
                   && Filename.Set.exists files ~f:(fun name ->
                     Filename.Set.mem targets.files name
                     || Filename.Set.mem targets.dirs name)))))
  || List.exists partial.pending ~f:(function
    | Generic_suspensions index -> in_mask (Mask_index.mask index)
    | Posted_suspensions (_, pending) ->
      Filename.Set.exists files ~f:(File_postings.mem_path pending ~dir)
    | Unindexed_suspensions _ -> true)
;;

let single_file_rule_is_closed (partial : partial) ~mask =
  match Path.Build.Map.min_binding partial.selected with
  | None -> false
  | Some (_, rules) ->
    (match Id.Map.min_binding (rules : Dir_rules.Nonempty.t :> Dir_rules.t) with
     | None | Some (_, Alias _) -> false
     | Some (_, Rule { targets = { root = dir; files; dirs }; _ }) ->
       Filename.Set.is_empty dirs
       &&
       let files =
         match Target_mask.single_path mask with
         | Some (requested_dir, name) when Path.Build.equal requested_dir dir ->
           (* The completed pull already followed every owner of this path,
              including same-name directory targets. Check the other outputs. *)
           Filename.Set.remove files name
         | None | Some _ -> files
       in
       not (files_have_unseen_owner partial ~dir files))
;;

let union_selected a b =
  if a == b || Path.Build.Map.is_empty b
  then a
  else if Path.Build.Map.is_empty a
  then b
  else Path.Build.Map.union a b ~f:(fun _ a b -> Some (Dir_rules.Nonempty.union a b))
;;

let union_partial (a : partial) (b : partial) =
  { selected = union_selected a.selected b.selected
  ; revealed = Appendable_list.(a.revealed @ b.revealed)
  ; pending = List.rev_append b.pending a.pending
  ; direct_frontier = List.rev_append b.direct_frontier a.direct_frontier
  ; refinements = List.rev_append b.refinements a.refinements
  }
;;

let start t ~refinements =
  let pending = initial_frontiers (Lazy.force t.suspension_index) in
  { selected = Path.Build.Map.empty
  ; revealed =
      (if Direct.is_empty t.direct
       then Appendable_list.empty
       else Appendable_list.singleton t.direct)
  ; pending
  ; direct_frontier = (if Direct.is_empty t.direct then [] else [ Whole_direct t.direct ])
  ; refinements
  }
;;

let finish_requested t (partial : partial) : loaded =
  let refinements =
    List.sort_uniq partial.refinements ~compare:(fun a b -> Id.compare a.id b.id)
  in
  let pending =
    List.fold_left partial.pending ~init:Pending.empty ~f:Pending.add_frontier
  in
  let selected =
    if partial.selected == t.direct.by_dir && Id.Map.is_empty t.suspensions
    then t
    else create ~direct:(Direct.create partial.selected) ~suspensions:Id.Map.empty
  in
  { selected; revealed = Revealed.create partial.revealed; pending; refinements }
;;

let load_requested_from t requested ~directory_only initial =
  let open Memo.O in
  let rec pull (partial : partial) mask : partial Memo.t =
    let selected, direct_frontier =
      List.fold_left
        partial.direct_frontier
        ~init:(partial.selected, [])
        ~f:(fun (selected, remaining) frontier ->
          match filter_direct_frontier frontier mask with
          | No_match -> selected, frontier :: remaining
          | Selected (matched, pending) ->
            let remaining = List.rev_append pending remaining in
            union_selected selected matched, remaining)
    in
    let matching, pending = matching_suspensions partial.pending mask in
    let partial = { partial with selected; pending; direct_frontier } in
    match matching with
    | [] -> Memo.return partial
    | [ producer ] ->
      let+ child = pull_producer mask producer in
      union_partial partial child
    | _ :: _ :: _ ->
      let+ children = Memo.parallel_map matching ~f:(pull_producer mask) in
      List.fold_left children ~init:partial ~f:union_partial
  and pull_producer mask (id, { mask = owned; rules; _ }) =
    let* rules = rules in
    pull (start rules ~refinements:[ { id; mask = owned } ]) mask
  in
  let count rules =
    Path.Build.Map.fold rules ~init:0 ~f:(fun rules count ->
      count + Id.Map.cardinal (rules : Dir_rules.Nonempty.t :> Dir_rules.t))
  in
  let covers_outputs mask rules =
    (* File outputs must also pull same-name directory owners. Directory
       outputs of ordinary requests still need the subtree closure below. *)
    Path.Build.Map.for_all rules ~f:(fun rules ->
      Id.Map.for_all
        (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
        ~f:(function
          | Alias _ -> true
          | Rule { targets = { root = dir; files; dirs }; _ } ->
            Filename.Set.for_all files ~f:(fun name ->
              Target_mask.mem_file_name mask ~dir name
              && Target_mask.mem_directory_name mask ~dir name)
            &&
            if directory_only
            then
              Filename.Set.for_all dirs ~f:(fun name ->
                Target_mask.mem_directory_name mask ~dir name)
            else Filename.Set.is_empty dirs))
  in
  let rec close mask previous_count partial =
    let* partial = pull partial mask in
    let current_count = count partial.selected in
    if
      current_count = previous_count
      || covers_outputs mask partial.selected
      (* A single atomic file rule often closes without another pull. Probe
         existing frontiers, including same-name directory owners, before
         constructing the expanded request. No producers are forced here. *)
      || (current_count = 1 && single_file_rule_is_closed partial ~mask)
    then Memo.return (finish_requested t partial)
    else
      close
        (Target_mask.union
           requested
           (direct_targets partial.selected ~rule_mask:(rule_request ~directory_only)))
        current_count
        partial
  in
  close requested (-1) initial
;;

let load_requested t requested ~directory_only =
  load_requested_from t requested ~directory_only (start t ~refinements:[])
;;

module Batch_routes = struct
  let prefix rules ancestors refinements : Direct.t list * route_prefix =
    let outside =
      List.concat_map (List.rev ancestors) ~f:(fun (partial : partial) ->
        Appendable_list.to_list partial.revealed)
    in
    let chunks =
      if Direct.is_empty rules.direct then outside else outside @ [ rules.direct ]
    in
    let pending, refinements =
      List.fold_left
        ancestors
        ~init:(Pending.empty, refinements)
        ~f:(fun (pending, refinements) (partial : partial) ->
          ( List.fold_left partial.pending ~init:pending ~f:Pending.add_frontier
          , List.rev_append partial.refinements refinements ))
    in
    outside, { chunks; pending; refinements }
  ;;

  let register cache rules ancestors refinements =
    match Lazy.force rules.suspension_index with
    | Indexed _ | Joined _ -> ()
    | Large group ->
      (match Lazy.force group.file_postings with
       | None -> ()
       | Some postings ->
         let _, prefix = prefix rules ancestors refinements in
         let family = { group; postings; prefix; routes = Table.create (module Id) 32 } in
         cache.families <- family :: cache.families)
  ;;

  let direct_family body ancestors refinements (producing_id, producer) index =
    let outside, prefix = prefix body ancestors refinements in
    let pending =
      List.fold_left
        (initial_frontiers (Lazy.force body.suspension_index))
        ~init:prefix.pending
        ~f:(fun pending frontier ->
          match frontier with
          | Unindexed_suspensions group ->
            (match Lazy.force group.file_postings with
             | Some postings ->
               Files { File_postings.postings; excluded = Id.Set.empty } :: pending
             | None -> Pending.add_frontier pending frontier)
          | _ -> Pending.add_frontier pending frontier)
    in
    { producing_id
    ; producer
    ; body
    ; index
    ; outside
    ; revealed = Revealed.create (Appendable_list.of_list prefix.chunks)
    ; pending
    ; refinements =
        List.sort_uniq prefix.refinements ~compare:(fun a b -> Id.compare a.id b.id)
    ; selections = Table.create (module Id) 32
    }
  ;;

  let register_direct cache body ancestors refinements producer =
    match Lazy.force body.direct.index with
    | Small _ | Indexed _ -> ()
    | Files index ->
      let family = direct_family body ancestors refinements producer index in
      cache.direct_families <- family :: cache.direct_families
  ;;

  let direct_selection (family : direct_route_family) postings ~dir id =
    match Table.find family.selections id with
    | Some loaded -> loaded
    | None ->
      let loaded =
        let rules = Path.Build.Map.find family.index.by_dir dir |> Option.value_exn in
        let data =
          Id.Map.find (rules : Dir_rules.Nonempty.t :> Dir_rules.t) id |> Option.value_exn
        in
        match data with
        | Alias _ -> Code_error.raise "Alias in a pure-file direct route" []
        | Rule rule ->
          let isolated =
            Filename.Set.for_all rule.targets.files ~f:(fun name ->
              (match File_postings.find postings ~dir name with
               | [ owner ] -> Id.equal owner id
               | _ -> false)
              && (not (Pending.mem_path family.pending ~dir name))
              && not
                   (List.exists family.outside ~f:(fun direct ->
                      match Path.Build.Map.find (Direct.target_names direct) dir with
                      | None -> false
                      | Some (files, dirs) ->
                        Filename.Set.mem files name || Filename.Set.mem dirs name)))
          in
          if not isolated
          then None
          else (
            let selected =
              Id.Map.singleton id data
              |> Dir_rules.Nonempty.create
              |> Option.value_exn
              |> Path.Build.Map.singleton dir
              |> Direct.create
            in
            Some
              { selected = create ~direct:selected ~suspensions:Id.Map.empty
              ; revealed = family.revealed
              ; pending = family.pending
              ; refinements = family.refinements
              })
      in
      Table.set family.selections id loaded;
      loaded
  ;;

  let rec find_direct (families : direct_route_family list) ~dir name =
    match families with
    | [] -> None
    | family :: rest ->
      let postings = Lazy.force family.index.postings in
      let loaded =
        match File_postings.find postings ~dir name with
        | [] | _ :: _ :: _ -> None
        | [ id ] -> direct_selection family postings ~dir id
      in
      (match loaded with
       | None -> find_direct rest ~dir name
       | Some loaded -> Some (`Direct (family, loaded)))
  ;;

  let producer_route (family : route_family) postings id =
    match Table.find family.routes id with
    | Some route -> route
    | None ->
      let route =
        let producer = Id.Map.find family.group.producers id |> Option.value_exn in
        let names = Target_mask.exact_file_names producer.mask |> Option.value_exn in
        let isolated =
          Path.Build.Map.for_alli names ~f:(fun dir names ->
            Filename.Set.for_all names ~f:(fun name ->
              (match File_postings.find postings ~dir name with
               | [ owner ] -> Id.equal owner id
               | _ -> false)
              && (not (Pending.mem_path family.prefix.pending ~dir name))
              && not
                   (List.exists family.prefix.chunks ~f:(fun direct ->
                      match Path.Build.Map.find (Direct.target_names direct) dir with
                      | None -> false
                      | Some (files, dirs) ->
                        Filename.Set.mem files name || Filename.Set.mem dirs name))))
        in
        if not isolated
        then None
        else
          Some
            { producer
            ; prefix =
                { family.prefix with
                  pending =
                    Files { postings = family.postings; excluded = Id.Set.singleton id }
                    :: family.prefix.pending
                ; refinements = { id; mask = producer.mask } :: family.prefix.refinements
                }
            }
      in
      Table.set family.routes id route;
      route
  ;;

  let rec find_producer (families : route_family list) ~dir name =
    match families with
    | [] -> None
    | family :: rest ->
      let postings = Lazy.force family.postings in
      let route =
        match File_postings.find postings ~dir name with
        | [] | _ :: _ :: _ -> None
        | [ id ] -> producer_route family postings id
      in
      (match route with
       | None -> find_producer rest ~dir name
       | Some route -> Some (`Producer route))
  ;;

  let find cache ~dir name =
    match find_direct cache.direct_families ~dir name with
    | Some _ as route -> route
    | None -> find_producer cache.families ~dir name
  ;;

  let load_route (route : route) requested =
    let open Memo.O in
    (* A route only certifies the completed ancestors. Keep the original leaf
       await, including its errors, cycles, and validation of declared ownership. *)
    let* rules = route.producer.rules in
    let+ loaded = load_requested rules requested ~directory_only:false in
    let revealed =
      Revealed.create
        (Appendable_list.of_list
           (route.prefix.chunks @ Lazy.force loaded.revealed.chunks))
    in
    { loaded with
      revealed
    ; pending = List.rev_append loaded.pending route.prefix.pending
    ; refinements =
        List.sort_uniq
          (List.rev_append loaded.refinements route.prefix.refinements)
          ~compare:(fun a b -> Id.compare a.id b.id)
    }
  ;;

  let cache_for_run root run =
    match root.routing with
    | Some cache when cache.run == run -> cache
    | None | Some _ ->
      let cache = { run; families = []; direct_families = []; visited = Id.Set.empty } in
      register cache root [] [];
      root.routing <- Some cache;
      cache
  ;;

  let load_cached cache root requested ~dir name initial_route =
    let open Memo.O in
    let rec discover rules ancestors refinements =
      let with_ancestors initial =
        List.fold_left ancestors ~init:initial ~f:(fun partial ancestor ->
          union_partial ancestor partial)
      in
      let direct_selection =
        if Direct.is_empty rules.direct
        then No_match
        else filter_direct_frontier (Whole_direct rules.direct) requested
      in
      match direct_selection with
      | Selected (selected, direct_frontier) ->
        let initial = start rules ~refinements in
        Memo.return
          (Either.Right (with_ancestors { initial with selected; direct_frontier }))
      | No_match ->
        let matching, pending =
          matching_suspensions
            (initial_frontiers (Lazy.force rules.suspension_index))
            requested
        in
        (match matching with
         | [] ->
           (* Ancestors also selected nothing and already pruned this request.
              With no outputs to widen it, there is no closure pass to repeat. *)
           let initial = start rules ~refinements in
           let partial = with_ancestors { initial with pending } in
           Memo.return (Either.Left (`Ready (finish_requested root partial)))
         | _ :: _ :: _ ->
           Memo.return (Either.Right (with_ancestors (start rules ~refinements)))
         | [ (id, producer) ] ->
           let ancestor =
             { selected = Path.Build.Map.empty
             ; revealed =
                 (if Direct.is_empty rules.direct
                  then Appendable_list.empty
                  else Appendable_list.singleton rules.direct)
             ; pending
             ; direct_frontier =
                 (if Direct.is_empty rules.direct
                  then []
                  else [ Whole_direct rules.direct ])
             ; refinements
             }
           in
           let ancestors = ancestor :: ancestors in
           let* rules = producer.rules in
           let found route =
             match route with
             | `Direct (family, loaded)
               when Id.equal family.producing_id id && family.body == rules ->
               Memo.return (Either.Left (`Ready loaded))
             | route -> Memo.return (Either.Left (`Await route))
           in
           (* Other point requests can already be suspended on this ancestor.
             Redirect them as soon as it completes, before forcing any leaf. *)
           (match find cache ~dir name with
            | Some route -> found route
            | None ->
              let refinements = [ { id; mask = producer.mask } ] in
              if Id.Set.mem cache.visited id
              then discover rules ancestors refinements
              else (
                cache.visited <- Id.Set.add cache.visited id;
                register cache rules ancestors refinements;
                register_direct cache rules ancestors refinements (id, producer);
                match find cache ~dir name with
                | Some route -> found route
                | None -> discover rules ancestors refinements)))
    in
    let* route =
      match initial_route with
      | Some route -> Memo.return (Either.Left (`Await route))
      | None -> discover root [] []
    in
    match route with
    | Left (`Ready loaded) -> Memo.return loaded
    | Left (`Await (`Producer route)) -> load_route route requested
    | Left (`Await (`Direct (family, loaded))) ->
      (* Completed ancestors are certified only for this exact body. The original
         producer still supplies errors, cycles and declared-output validation. *)
      let* body = family.producer.rules in
      if body == family.body
      then Memo.return loaded
      else load_requested root requested ~directory_only:false
    | Right initial -> load_requested_from root requested ~directory_only:false initial
  ;;

  let load root requested ~dir name =
    let open Memo.O in
    let* run = Memo.current_run () in
    let cache = cache_for_run root run in
    load_cached cache root requested ~dir name (find cache ~dir name)
  ;;

  let load_path root target =
    let open Memo.O in
    let dir = Path.Build.parent_exn target in
    let name = Path.Build.basename target in
    let* run = Memo.current_run () in
    let cache = cache_for_run root run in
    match find cache ~dir name with
    | Some (`Direct (family, loaded)) ->
      let* body = family.producer.rules in
      if body == family.body
      then Memo.return loaded
      else load_requested root (Target_mask.path target) ~directory_only:false
    | initial_route ->
      load_cached cache root (Target_mask.path target) ~dir name initial_route
  ;;
end

module Watch_routes = struct
  let find (family : direct_route_family) ~dir name =
    let postings = Lazy.force family.index.postings in
    match File_postings.find postings ~dir name with
    | [] | _ :: _ :: _ -> None
    | [ id ] -> Batch_routes.direct_selection family postings ~dir id
  ;;

  let with_ancestors ancestors initial =
    List.fold_left ancestors ~init:initial ~f:(fun partial ancestor ->
      union_partial ancestor partial)
  ;;

  let rec discover root requested ~dir name body ancestors refinements steps =
    let direct =
      if Direct.is_empty body.direct
      then No_match
      else filter_direct_frontier (Whole_direct body.direct) requested
    in
    match direct with
    | Selected (selected, direct_frontier) ->
      let family =
        match steps, Lazy.force body.direct.index with
        | { id; producer; _ } :: _, Files index ->
          let family =
            Batch_routes.direct_family body ancestors refinements (id, producer) index
          in
          (match find family ~dir name with
           | None -> None
           | Some loaded -> Some (family, loaded))
        | [], _ | _ :: _, (Small _ | Indexed _) -> None
      in
      (match family with
       | Some (family, loaded) ->
         root.watch_route <- Some { family; steps = List.rev steps };
         Memo.return loaded
       | None ->
         let initial = { (start body ~refinements) with selected; direct_frontier } in
         load_requested_from
           root
           requested
           ~directory_only:false
           (with_ancestors ancestors initial))
    | No_match ->
      let matching, pending =
        matching_suspensions
          (initial_frontiers (Lazy.force body.suspension_index))
          requested
      in
      (match matching with
       | [] ->
         let initial = { (start body ~refinements) with pending } in
         Memo.return (finish_requested root (with_ancestors ancestors initial))
       | _ :: _ :: _ ->
         load_requested_from
           root
           requested
           ~directory_only:false
           (with_ancestors ancestors (start body ~refinements))
       | [ (id, producer) ] ->
         let ancestor = { (start body ~refinements) with pending } in
         let parent = body in
         let open Memo.O in
         let* body = producer.rules in
         let step = { parent; id; producer; body; pending } in
         discover
           root
           requested
           ~dir
           name
           body
           (ancestor :: ancestors)
           [ { id; mask = producer.mask } ]
           (step :: steps))
  ;;

  let resume root requested ~dir name route count body =
    (match root.watch_route with
     | Some current when current == route -> root.watch_route <- None
     | None | Some _ -> ());
    (* Rebuild only the already validated prefix. In particular, do not read
       the root again or enter any descendant from the obsolete body. *)
    let rec prefix count ancestors refinements validated = function
      | _ when count = 0 ->
        discover root requested ~dir name body ancestors refinements validated
      | [] -> Code_error.raise "Incomplete validated direct-route prefix" []
      | step :: rest ->
        let ancestor = { (start step.parent ~refinements) with pending = step.pending } in
        let step = if count = 1 then { step with body } else step in
        prefix
          (count - 1)
          (ancestor :: ancestors)
          [ { id = step.id; mask = step.producer.mask } ]
          (step :: validated)
          rest
    in
    prefix count [] [] [] route.steps
  ;;

  let load root requested ~dir name =
    let cold () = discover root requested ~dir name root [] [] [] in
    match root.watch_route with
    | None -> cold ()
    | Some route as cached ->
      (* Membership and all-output isolation precede every cached await. A
         miss must not force an unrelated, possibly failing old producer. *)
      (match find route.family ~dir name with
       | None -> cold ()
       | Some loaded ->
         (* Do not retain an obsolete body if ancestor validation fails. *)
         if root.watch_route == cached then root.watch_route <- None;
         let rec validate route parent count = function
           | [] ->
             (match root.watch_route with
              | None -> root.watch_route <- cached
              | Some _ -> ());
             Memo.return loaded
           | step :: rest ->
             (match Id.Map.find parent.suspensions step.id with
              | Some producer when parent == step.parent && producer == step.producer ->
                let open Memo.O in
                let* body = producer.rules in
                if body == step.body
                then validate route body (count + 1) rest
                else resume root requested ~dir name route (count + 1) body
              | None | Some _ -> resume root requested ~dir name route count parent)
         in
         validate route root 0 route.steps)
  ;;
end

let load_with_pending t requested =
  Memo.of_thunk (fun () ->
    if Id.Map.is_empty t.suspensions
    then load_requested t requested ~directory_only:false
    else (
      match Target_mask.single_path requested with
      | None -> load_requested t requested ~directory_only:false
      | Some (dir, name) when Memo.is_incremental () ->
        Watch_routes.load t requested ~dir name
      | Some (dir, name) -> Batch_routes.load t requested ~dir name))
;;

let load_path_with_pending t target =
  Memo.of_thunk (fun () ->
    if Id.Map.is_empty t.suspensions
    then load_requested t (Target_mask.path target) ~directory_only:false
    else if Memo.is_incremental ()
    then
      Watch_routes.load
        t
        (Target_mask.path target)
        ~dir:(Path.Build.parent_exn target)
        (Path.Build.basename target)
    else Batch_routes.load_path t target)
;;

let load_directory_with_pending t directory =
  load_requested t (Target_mask.directories [ directory ]) ~directory_only:true
;;

let load t requested =
  let open Memo.O in
  let+ loaded = load_with_pending t requested in
  loaded.selected
;;

let to_map x =
  (x.direct.by_dir
    : Dir_rules.Nonempty.t Path.Build.Map.t
    :> Dir_rules.t Path.Build.Map.t)
;;

let rec map t ~f =
  create
    ~direct:
      (Path.Build.Map.map t.direct.by_dir ~f:(fun m ->
         Id.Map.foldi
           (m : Dir_rules.Nonempty.t :> Dir_rules.t)
           ~init:Id.Map.empty
           ~f:(fun id data acc ->
             match f data with
             | `No_change -> Id.Map.set acc id data
             | `Changed data -> Id.Map.set acc (Id.gen ()) data)
         |> Dir_rules.Nonempty.create
         |> Option.value_exn)
       |> Direct.create)
    ~suspensions:
      (Id.Map.fold
         t.suspensions
         ~init:Id.Map.empty
         ~f:(fun ({ mask; rules; producer; _ } as underlying) acc ->
           let cached = ref None in
           let observed = ref Unforced in
           let rules =
             Memo.of_thunk (fun () ->
               observed := Evaluating;
               let open Memo.O in
               (* Always track the original producer before reusing its pure
                transformation. Sharing the transformed value preserves its IDs
                without adding another Memo node. *)
               let+ input = rules in
               let output =
                 match !cached with
                 | Some (previous, output) when previous == input -> output
                 | _ ->
                   let output = map input ~f in
                   cached := Some (input, output);
                   output
               in
               observed := Produced output;
               output)
           in
           Id.Map.set
             acc
             (Id.gen ())
             { mask; rules; producer; observed; underlying = Some underlying }))
;;

let map_rules t ~f =
  map t ~f:(function
    | (Alias _ : Dir_rules.data) -> `No_change
    | Rule r -> `Changed (Rule (f r) : Dir_rules.data))
;;

let find t p =
  match Path.as_in_build_dir p with
  | None -> Dir_rules.empty
  | Some p ->
    (match Path.Build.Map.find t.direct.by_dir p with
     | Some dir_rules -> (dir_rules : Dir_rules.Nonempty.t :> Dir_rules.t)
     | None -> Dir_rules.empty)
;;

let prefix_rules prefix ~f =
  let open Memo.O in
  let* res, rules = collect f in
  let+ () =
    produce
      (map_rules rules ~f:(fun (rule : Rule.t) ->
         let t =
           let open Action_builder.O in
           prefix >>> rule.action
         in
         Rule.set_action rule t))
  in
  res
;;
