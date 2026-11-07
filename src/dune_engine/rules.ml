open Import
module Id = Id.Make ()

module Dir_rules = struct
  module Alias_spec = struct
    type item =
      | Deps of unit Action_builder.t
      | Action of Rule.Anonymous_action.t

    type t = { expansions : (Loc.t * item) Appendable_list.t } [@@unboxed]

    let singleton loc item = { expansions = Appendable_list.singleton (loc, item) }
    let union x y = { expansions = Appendable_list.( @ ) x.expansions y.expansions }
    let to_list t = Appendable_list.to_list_rev t.expansions
  end

  type alias =
    { name : Alias.Name.t
    ; spec : Alias_spec.t
    }

  type data =
    | Rule of Rule.t
    | Alias of alias
    | Prefixed of
        { prefix : unit Action_builder.t
        ; rules : data Id.Map.t
        }

  type t = data Id.Map.t

  let data_repr =
    Repr.fix (fun data_repr ->
      Repr.variant
        "directory-rule"
        [ Repr.case
            "Rule"
            (Repr.record
               "rule"
               [ Repr.field
                   "targets"
                   (Repr.abstract Targets.Validated.to_dyn)
                   ~get:(fun rule -> rule.Rule.targets)
               ])
            ~proj:(function
              | Rule rule -> Some rule
              | Alias _ | Prefixed _ -> None)
        ; Repr.case
            "Alias"
            (Repr.record
               "alias"
               [ Repr.field "name" (Repr.abstract Alias.Name.to_dyn) ~get:(fun t ->
                   t.name)
               ])
            ~proj:(function
              | Alias alias -> Some alias
              | Rule _ | Prefixed _ -> None)
        ; Repr.case "Prefixed" (Repr.list data_repr) ~proj:(function
            | Prefixed { prefix = _; rules } -> Some (Id.Map.values rules)
            | Rule _ | Alias _ -> None)
        ])
  ;;

  let repr = Repr.view (Repr.list data_repr) ~to_:Id.Map.values
  let to_dyn = Repr.to_dyn repr

  type ready =
    { rules : Rule.t list
    ; aliases : Alias_spec.t Alias.Name.Map.t
    }

  let add_alias { name; spec } aliases =
    Alias.Name.Map.update aliases name ~f:(function
      | None -> Some spec
      | Some base -> Some (Alias_spec.union spec base))
  ;;

  let consume_prefixed t =
    let rec collect t ~prefixes ~init =
      Id.Map.foldi t ~init ~f:(fun id data ((rules, aliases) as acc) ->
        match data with
        | Rule rule ->
          let rule =
            match prefixes with
            | [] -> rule
            | _ :: _ ->
              let action =
                List.fold_left prefixes ~init:rule.action ~f:(fun action prefix ->
                  let open Action_builder.O in
                  prefix >>> action)
              in
              Rule.set_action rule action
          in
          rule :: rules, aliases
        | Alias alias -> rules, Id.Map.set aliases id alias
        | Prefixed { prefix; rules } ->
          collect rules ~prefixes:(prefix :: prefixes) ~init:acc)
    in
    let rules, aliases = collect t ~prefixes:[] ~init:([], Id.Map.empty) in
    let rules = List.rev rules in
    let aliases =
      (* This accumulates the aliases in reverse order, but there's another
         reversal whenever the expansion is inspected. The order doesn't really
         matter, but it does change the tests. So it's nice to maintain it if
         possible *)
      Id.Map.fold aliases ~init:Alias.Name.Map.empty ~f:add_alias
    in
    { rules; aliases }
  ;;

  let consume t =
    if
      Id.Map.exists t ~f:(function
        | Prefixed _ -> true
        | Rule _ | Alias _ -> false)
    then consume_prefixed t
    else (
      let rules, aliases =
        Id.Map.fold t ~init:([], Alias.Name.Map.empty) ~f:(fun data (rules, aliases) ->
          match data with
          | Rule rule -> rule :: rules, aliases
          | Alias alias -> rules, add_alias alias aliases
          | Prefixed _ ->
            Code_error.raise "Dir_rules.consume: unexpected prefixed rules" [])
      in
      { rules = List.rev rules; aliases })
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
    val prefix : t -> unit Action_builder.t -> t
  end = struct
    type maybe_empty = t
    type nonrec t = t

    let to_dyn = to_dyn
    let create t = if is_empty t then None else Some t
    let union = union
    let singleton = singleton
    let add = add
    let prefix rules prefix = singleton (Prefixed { prefix; rules })
  end
end

module T = struct
  type t = Dir_rules.Nonempty.t Path.Build.Map.t

  let empty = Path.Build.Map.empty
  let union_map a b ~f = Path.Build.Map.union a b ~f:(fun _key a b -> Some (f a b))
  let union a b = if a == b then a else union_map a b ~f:Dir_rules.Nonempty.union
  let name = "Rules"
end

include T

let to_dyn = Path.Build.Map.to_dyn Dir_rules.Nonempty.to_dyn

let singleton_rule (rule : Rule.t) =
  let dir = rule.targets.root in
  Path.Build.Map.singleton dir (Dir_rules.Nonempty.singleton (Rule rule))
;;

let implicit_output = Memo.Implicit_output.add (module T)

let produce rules =
  if Path.Build.Map.is_empty rules
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
         Path.Build.Map.singleton
           dir
           (Dir_rules.Nonempty.singleton (Alias { name; spec })))
    ;;

    let add_deps t ?(loc = Loc.none) expansion =
      alias t (Dir_rules.Alias_spec.singleton loc (Dir_rules.Alias_spec.Deps expansion))
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
        alias t (Dir_rules.Alias_spec.singleton loc (Dir_rules.Alias_spec.Action anon)))
    ;;
  end
end

let of_dir_rules ~dir rules =
  match Dir_rules.Nonempty.create rules with
  | None -> Path.Build.Map.empty
  | Some rules -> Path.Build.Map.singleton dir rules
;;

let of_rules rules =
  List.fold_left rules ~init:Path.Build.Map.empty ~f:(fun acc rule ->
    Path.Build.Map.update acc rule.Rule.targets.root ~f:(function
      | None -> Some (Dir_rules.Nonempty.singleton (Rule rule))
      | Some acc -> Some (Dir_rules.Nonempty.add acc (Rule rule))))
;;

let directory_targets (rules : t) =
  let rec collect rules acc =
    Id.Map.fold rules ~init:acc ~f:(fun (data : Dir_rules.data) acc ->
      match data with
      | Alias _ -> acc
      | Prefixed { prefix = _; rules } -> collect rules acc
      | Rule rule ->
        if Filename.Set.is_empty rule.targets.dirs
        then acc
        else
          Filename.Set.fold ~init:acc rule.targets.dirs ~f:(fun target acc ->
            let target = Path.Build.relative_fname rule.targets.root target in
            Path.Build.Map.update acc target ~f:(function
              | None -> Some (Rule.loc rule)
              | Some loc -> Some loc)))
  in
  Path.Build.Map.fold rules ~init:Path.Build.Map.empty ~f:(fun dir_rules acc ->
    collect (dir_rules : Dir_rules.Nonempty.t :> Dir_rules.t) acc)
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

let to_map x = (x : t :> Dir_rules.t Path.Build.Map.t)

let find t p =
  match Path.as_in_build_dir p with
  | None -> Dir_rules.empty
  | Some p ->
    (match Path.Build.Map.find t p with
     | Some dir_rules -> (dir_rules : Dir_rules.Nonempty.t :> Dir_rules.t)
     | None -> Dir_rules.empty)
;;

let prefix_rules prefix ~f =
  let open Memo.O in
  let* res, rules = collect f in
  let+ () =
    produce
      (Path.Build.Map.map rules ~f:(fun rules -> Dir_rules.Nonempty.prefix rules prefix))
  in
  res
;;
