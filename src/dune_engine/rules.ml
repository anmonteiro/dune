open Import
module Id = Id.Make ()

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
    else
      union_map a b ~f:(fun a b ->
        assert (a == b);
        a)
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

module T = struct
  type t =
    { direct : Dir_rules.Nonempty.t Path.Build.Map.t
    ; suspensions : suspension Id.Map.t
    }

  and suspension =
    { mask : Target_mask.t
    ; rules : t Memo.Lazy.t
    }

  let empty = { direct = Path.Build.Map.empty; suspensions = Id.Map.empty }
  let union_map a b ~f = Path.Build.Map.union a b ~f:(fun _key a b -> Some (f a b))

  let union a b =
    if a == b
    then a
    else
      { direct = union_map a.direct b.direct ~f:Dir_rules.Nonempty.union
      ; suspensions =
          Id.Map.union a.suspensions b.suspensions ~f:(fun _ a b ->
            assert (a == b);
            Some a)
      }
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
        ~get:(fun t -> t.direct)
    ; Repr.field "suspensions" Repr.int ~get:(fun t -> Id.Map.cardinal t.suspensions)
    ]
;;

let to_dyn = Repr.to_dyn repr

let singleton_rule (rule : Rule.t) =
  let dir = rule.targets.root in
  { empty with
    direct = Path.Build.Map.singleton dir (Dir_rules.Nonempty.singleton (Rule rule))
  }
;;

let implicit_output = Memo.Implicit_output.add (module T)

let produce rules =
  if Path.Build.Map.is_empty rules.direct && Id.Map.is_empty rules.suspensions
  then Memo.return ()
  else Memo.Implicit_output.produce implicit_output rules
;;

module Produce = struct
  let rule rule = produce (singleton_rule rule)

  module Alias = struct
    type t = Alias.t

    let alias t spec =
      produce
        (let dir = Alias.dir t in
         let name = Alias.name t in
         { empty with
           direct =
             Path.Build.Map.singleton
               dir
               (Dir_rules.Nonempty.singleton (Alias { name; spec }))
         })
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
  | Some rules -> { empty with direct = Path.Build.Map.singleton dir rules }
;;

let of_rules rules =
  let direct =
    List.fold_left rules ~init:Path.Build.Map.empty ~f:(fun acc rule ->
      Path.Build.Map.update acc rule.Rule.targets.root ~f:(function
        | None -> Some (Dir_rules.Nonempty.singleton (Rule rule))
        | Some acc -> Some (Dir_rules.Nonempty.add acc (Rule rule))))
  in
  { empty with direct }
;;

let directory_targets (rules : t) =
  Path.Build.Map.fold
    ~init:Path.Build.Map.empty
    rules.direct
    ~f:(fun (dir_rules : Dir_rules.Nonempty.t) acc ->
      (dir_rules :> Dir_rules.t)
      |> Id.Map.fold ~init:acc ~f:(fun (data : Dir_rules.data) acc ->
        match data with
        | Alias _ -> acc
        | Rule rule ->
          Filename.Set.fold ~init:acc rule.targets.dirs ~f:(fun target acc ->
            let target = Path.Build.relative_fname rule.targets.root target in
            Path.Build.Map.update acc target ~f:(function
              | None -> Some (Rule.loc rule)
              | Some loc -> Some loc))))
;;

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
  let check target matches =
    if not matches
    then
      Code_error.raise
        "Rule stage produced a target outside its mask"
        [ "target", Path.Build.to_dyn target ]
  in
  Path.Build.Map.iteri t.direct ~f:(fun dir rules ->
    Id.Map.iter
      (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
      ~f:(function
        | Rule rule ->
          Targets.Validated.iter
            rule.targets
            ~file:(fun target -> check target (Target_mask.mem_file mask target))
            ~dir:(fun target -> check target (Target_mask.mem_directory mask target))
        | Alias { name; _ } ->
          let alias = Alias.make name ~dir in
          if not (Target_mask.mem_alias mask alias)
          then
            Code_error.raise
              "Rule stage produced an alias outside its mask"
              [ "alias", Alias.to_dyn alias ]));
  { t with
    suspensions =
      Id.Map.values t.suspensions
      |> Id.Map.of_list_map_exn ~f:(fun { mask = child_mask; rules } ->
        let mask = Target_mask.inter mask child_mask in
        ( Id.gen ()
        , { mask
          ; rules =
              Memo.lazy_ ~name:"restrict-rule-stage" (fun () ->
                let open Memo.O in
                let+ rules = Memo.Lazy.force rules in
                restrict rules mask)
          } ))
  }
;;

let defer mask f =
  let open Memo.O in
  let* () = Memo.return () in
  let shared =
    Memo.lazy_ ~name:"deferred-rule-production" (fun () ->
      let+ result, rules = collect f in
      result, restrict rules mask)
  in
  let rules =
    Memo.lazy_ ~name:"deferred-rules" (fun () ->
      let+ _, rules = Memo.Lazy.force shared in
      rules)
  in
  let+ () =
    produce { empty with suspensions = Id.Map.singleton (Id.gen ()) { mask; rules } }
  in
  Memo.lazy_ ~name:"deferred-rule-result" (fun () ->
    let+ result, _ = Memo.Lazy.force shared in
    result)
;;

let narrow mask f =
  let open Memo.O in
  let+ (_ : unit Memo.Lazy.t) = defer mask f in
  ()
;;

let rule_mask (rule : Rule.t) =
  let { Targets.Validated.root; files; dirs } = rule.targets in
  Target_mask.union
    (Filename.Set.to_list_map files ~f:(Path.Build.relative_fname root)
     |> Target_mask.files)
    (Filename.Set.to_list_map dirs ~f:(Path.Build.relative_fname root)
     |> Target_mask.directories)
;;

let rule_request (rule : Rule.t) ~directory_only =
  let { Targets.Validated.root; files; dirs } = rule.targets in
  let add_path name acc =
    Target_mask.union acc (Target_mask.path (Path.Build.relative_fname root name))
  in
  let mask = Filename.Set.fold files ~init:Target_mask.empty ~f:add_path in
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

let targets t =
  Id.Map.fold
    t.suspensions
    ~init:(direct_targets t.direct ~rule_mask)
    ~f:(fun { mask; _ } targets -> Target_mask.union targets mask)
;;

let filter_direct direct mask =
  Path.Build.Map.filter_mapi direct ~f:(fun dir rules ->
    Id.Map.filter
      (rules : Dir_rules.Nonempty.t :> Dir_rules.t)
      ~f:(function
        | Rule rule -> Target_mask.intersects mask (rule_mask rule)
        | Alias { name; _ } -> Target_mask.mem_alias mask (Alias.make name ~dir))
    |> Dir_rules.Nonempty.create)
;;

type loaded =
  { selected : t
  ; revealed : t
  ; pending : Target_mask.t
  }

let load_requested t requested ~directory_only =
  let open Memo.O in
  let rec pull t mask =
    let+ children =
      Id.Map.values t.suspensions
      |> Memo.parallel_map ~f:(fun { mask = owned; rules } ->
        if Target_mask.intersects mask owned
        then Memo.Lazy.force rules >>= fun rules -> pull rules mask
        else Memo.return { selected = empty; revealed = empty; pending = owned })
    in
    List.fold_left
      children
      ~init:
        { selected = { empty with direct = filter_direct t.direct mask }
        ; revealed = { empty with direct = t.direct }
        ; pending = Target_mask.empty
        }
      ~f:(fun a b ->
        { selected = union a.selected b.selected
        ; revealed = union a.revealed b.revealed
        ; pending = Target_mask.union a.pending b.pending
        })
  in
  let count rules =
    Path.Build.Map.fold rules.direct ~init:0 ~f:(fun rules count ->
      count + Id.Map.cardinal (rules : Dir_rules.Nonempty.t :> Dir_rules.t))
  in
  let rec close mask previous_count =
    let* loaded = pull t mask in
    let current_count = count loaded.selected in
    if current_count = previous_count
    then Memo.return loaded
    else
      close
        (Target_mask.union
           requested
           (direct_targets
              loaded.selected.direct
              ~rule_mask:(rule_request ~directory_only)))
        current_count
  in
  close requested (-1)
;;

let load_with_pending t requested = load_requested t requested ~directory_only:false

let load_directory_with_pending t directory =
  load_requested t (Target_mask.directories [ directory ]) ~directory_only:true
;;

let load t requested =
  let open Memo.O in
  let+ loaded = load_with_pending t requested in
  loaded.selected
;;

let to_map x =
  (x.direct : Dir_rules.Nonempty.t Path.Build.Map.t :> Dir_rules.t Path.Build.Map.t)
;;

let rec map t ~f =
  { direct =
      Path.Build.Map.map t.direct ~f:(fun m ->
        Id.Map.to_list (m : Dir_rules.Nonempty.t :> Dir_rules.t)
        |> Id.Map.of_list_map_exn ~f:(fun (id, data) ->
          match f data with
          | `No_change -> id, data
          | `Changed data -> Id.gen (), data)
        |> Dir_rules.Nonempty.create
        |> Option.value_exn)
  ; suspensions =
      Id.Map.values t.suspensions
      |> Id.Map.of_list_map_exn ~f:(fun { mask; rules } ->
        ( Id.gen ()
        , { mask
          ; rules =
              Memo.lazy_ ~name:"map-rule-stage" (fun () ->
                let open Memo.O in
                let+ rules = Memo.Lazy.force rules in
                map rules ~f)
          } ))
  }
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
    (match Path.Build.Map.find t.direct p with
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
