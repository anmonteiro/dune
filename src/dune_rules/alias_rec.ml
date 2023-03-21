open Import

module Alias_status = struct
  module T = struct
    type t =
      | Defined
      | Not_defined

    let empty : t = Not_defined

    let combine : t -> t -> t =
     fun x y ->
      match (x, y) with
      | _, Defined | Defined, _ -> Defined
      | Not_defined, Not_defined -> Not_defined
  end

  include T
  include Monoid.Make (T)
end

module In_melange_target_dir = struct
  module Lookup_alias = struct
    type result =
      { alias_exists : Alias_status.t
      ; allowed_subdirs : Filename.Set.t
      }

    let of_dir_set ~exists dirs =
      let allowed_subdirs =
        match Dir_set.toplevel_subdirs dirs with
        | Infinite -> Filename.Set.empty
        | Finite sub_dirs -> sub_dirs
      in
      { alias_exists = exists; allowed_subdirs }
  end

  let dep_on_alias_if_exists alias =
    let open Action_builder.O in
    Action_builder.of_memo
      (Load_rules.load_dir ~dir:(Path.build (Alias.dir alias)))
    >>= function
    | Source _ | External _ ->
      Code_error.raise "Alias in a non-build dir"
        [ ("alias", Alias.to_dyn alias) ]
    | Build { aliases; allowed_subdirs; rules_here = _ } -> (
      match Alias.Name.Map.mem aliases (Alias.name alias) with
      | false ->
        Action_builder.return
          (Lookup_alias.of_dir_set ~exists:Not_defined allowed_subdirs)
      | true ->
        Action_builder.alias alias
        >>> Action_builder.return
              (Lookup_alias.of_dir_set ~exists:Defined allowed_subdirs))
    | Build_under_directory_target _ ->
      Action_builder.return
        { Lookup_alias.alias_exists = Not_defined
        ; allowed_subdirs = Filename.Set.empty
        }

  let dep_on_alias_rec name dir =
    let rec map_reduce dir ~f =
      let open Action_builder.O in
      let* { Lookup_alias.alias_exists; allowed_subdirs } = f dir in
      Action_builder.List.fold_left (String.Set.to_list allowed_subdirs)
        ~init:alias_exists ~f:(fun alias_exists s ->
          let+ alias_exists' = map_reduce (Path.Build.relative dir s) ~f in
          Alias_status.combine alias_exists alias_exists')
    in
    map_reduce dir ~f:(fun dir -> dep_on_alias_if_exists (Alias.make ~dir name))
end

let rec map_reduce ~build_dir ~name t ~f =
  match
    Sub_dirs.Status.(Map.find Set.normal_only (Source_tree.Dir.status t))
  with
  | false -> Action_builder.return Alias_status.empty
  | true -> (
    let open Action_builder.O in
    let* found_in_source =
      let+ here = f t
      and+ in_sub_dirs =
        Action_builder.List.map
          (Filename.Map.values (Source_tree.Dir.sub_dirs t))
          ~f:(fun s ->
            let* t = Action_builder.of_memo (Source_tree.Dir.sub_dir_as_t s) in
            map_reduce ~build_dir ~name t ~f)
      in
      List.fold_left in_sub_dirs ~init:here ~f:Alias_status.combine
    in
    let build_path =
      Path.Build.append_source build_dir (Source_tree.Dir.path t)
    in
    Action_builder.of_memo (Only_packages.stanzas_in_dir build_path)
    >>= function
    | None -> Action_builder.return found_in_source
    | Some stanzas ->
      let+ in_melange_target_dirs =
        let melange_target_dirs =
          List.filter_map stanzas.stanzas ~f:(function
            | Melange_stanzas.Emit.T mel ->
              Some (Melange_stanzas.Emit.target_dir ~dir:build_path mel)
            | _ -> None)
        in
        Action_builder.List.map melange_target_dirs ~f:(fun s ->
            In_melange_target_dir.dep_on_alias_rec name s)
      in
      List.fold_left in_melange_target_dirs ~init:found_in_source
        ~f:Alias_status.combine)

let dep_on_alias_if_exists alias =
  let open Action_builder.O in
  Action_builder.of_memo (Load_rules.alias_exists alias) >>= function
  | false -> Action_builder.return Alias_status.Not_defined
  | true ->
    let+ () = Action_builder.alias alias in
    Alias_status.Defined

let dep_on_alias_rec name dir =
  let ctx_name, src_dir = Path.Build.extract_build_context_exn dir in
  let build_dir = Context_name.build_dir (Context_name.of_string ctx_name) in
  let f dir =
    let path = Path.Build.append_source build_dir (Source_tree.Dir.path dir) in
    dep_on_alias_if_exists (Alias.make ~dir:path name)
  in
  let open Action_builder.O in
  Source_tree.find_dir src_dir |> Action_builder.of_memo >>= function
  | None -> Action_builder.return Alias_status.Not_defined
  | Some src_dir -> map_reduce ~build_dir ~name src_dir ~f
