open Import

module Context_type = struct
  type t =
    | Empty
    | With_sources
end

module Gen_rules = struct
  module Build_only_sub_dirs = struct
    type t = Subdir_set.t Path.Build.Map.t

    let singleton ~dir sub_dirs = Path.Build.Map.singleton dir sub_dirs
    let union a b = Path.Build.Map.union a b ~f:(fun _ a b -> Some (Subdir_set.union a b))
  end

  module Rule_targets = struct
    type t =
      | All
      | Declared of
          { files : Path.Build.Set.t
          ; subtrees : Path.Build.Set.t
          ; file_extensions : Filename.Extension.Set.t Path.Build.Map.t
          }

    let empty =
      Declared
        { files = Path.Build.Set.empty
        ; subtrees = Path.Build.Set.empty
        ; file_extensions = Path.Build.Map.empty
        }
    ;;

    let union a b =
      match a, b with
      | All, _ | _, All -> All
      | Declared a, Declared b ->
        Declared
          { files = Path.Build.Set.union a.files b.files
          ; subtrees = Path.Build.Set.union a.subtrees b.subtrees
          ; file_extensions =
              Path.Build.Map.union a.file_extensions b.file_extensions ~f:(fun _ a b ->
                Some (Filename.Extension.Set.union a b))
          }
    ;;

    let is_empty = function
      | All -> false
      | Declared { files; subtrees; file_extensions } ->
        Path.Build.Set.is_empty files
        && Path.Build.Set.is_empty subtrees
        && Path.Build.Map.for_all file_extensions ~f:Filename.Extension.Set.is_empty
    ;;

    let mem t path =
      match t with
      | All -> true
      | Declared { files; subtrees; file_extensions } ->
        Path.Build.Set.mem files path
        || Path.Build.Set.exists subtrees ~f:(fun of_ ->
          Path.Build.is_descendant path ~of_)
        ||
          (match
             ( Path.Build.parent path
             , Path.Build.extension path |> Filename.Extension.Or_empty.extension )
           with
          | Some dir, Some extension ->
            (match Path.Build.Map.find file_extensions dir with
             | None -> false
             | Some extensions -> Filename.Extension.Set.mem extensions extension)
          | None, _ | _, None -> false)
    ;;

    let intersects_directory t dir =
      match t with
      | All -> true
      | Declared { files; subtrees; file_extensions } ->
        mem t dir
        || Path.Build.Set.exists files ~f:(fun path ->
          Path.Build.is_descendant path ~of_:dir)
        || Path.Build.Set.exists subtrees ~f:(fun path ->
          Path.Build.is_descendant path ~of_:dir || Path.Build.is_descendant dir ~of_:path)
        || Path.Build.Map.existsi file_extensions ~f:(fun path extensions ->
          (not (Filename.Extension.Set.is_empty extensions))
          && Path.Build.is_descendant path ~of_:dir)
    ;;
  end

  module Rules = struct
    type stage =
      | Source of Rules.t Memo.Lazy.t
      | Compilation of
          { targets : Rule_targets.t Memo.Lazy.t
          ; rules : Rules.t Memo.Lazy.t
          }

    type nonrec t =
      { build_dir_only_sub_dirs : Build_only_sub_dirs.t
      ; directory_targets : Loc.t Path.Build.Map.t
      ; rules : Rules.t Memo.t
      ; stages : stage list
      }

    let empty =
      { build_dir_only_sub_dirs = Path.Build.Map.empty
      ; directory_targets = Path.Build.Map.empty
      ; rules = Memo.return Rules.empty
      ; stages = []
      }
    ;;

    let source_stage rules =
      Source (Memo.lazy_ ~name:"source-rule-stage" (fun () -> rules))
    ;;

    let compilation_stage ~targets rules =
      let targets = Memo.lazy_ ~name:"rule-stage-targets" (fun () -> targets) in
      let rules =
        Memo.lazy_ ~name:"rule-stage" (fun () ->
          let open Memo.O in
          let+ rules = rules
          and+ targets = Memo.Lazy.force targets in
          let check_target target =
            if not (Rule_targets.mem targets target)
            then
              Code_error.raise
                "Rule stage produced a target outside its declaration"
                [ "target", Path.Build.to_dyn target ]
          in
          Rules.to_map rules
          |> Path.Build.Map.iter ~f:(fun dir_rules ->
            let { Rules.Dir_rules.rules; aliases = _ } =
              Rules.Dir_rules.consume dir_rules
            in
            List.iter rules ~f:(fun rule ->
              Targets.Validated.iter
                rule.Rule.targets
                ~file:check_target
                ~dir:check_target));
          rules)
      in
      Compilation { targets; rules }
    ;;

    let collect_stages stages =
      let open Memo.O in
      let+ rules =
        Memo.parallel_map stages ~f:(function Source rules | Compilation { rules; _ } ->
            Memo.Lazy.force rules)
      in
      List.fold_left rules ~init:Rules.empty ~f:Rules.union
    ;;

    let of_stages ~build_dir_only_sub_dirs ~directory_targets stages =
      { build_dir_only_sub_dirs
      ; directory_targets
      ; rules = collect_stages stages
      ; stages
      }
    ;;

    let create
          ?(build_dir_only_sub_dirs = empty.build_dir_only_sub_dirs)
          ?(directory_targets = empty.directory_targets)
          rules
      =
      of_stages ~build_dir_only_sub_dirs ~directory_targets [ source_stage rules ]
    ;;

    let create_staged
          ?(build_dir_only_sub_dirs = empty.build_dir_only_sub_dirs)
          ?(directory_targets = empty.directory_targets)
          ~source_rules
          ~compilation_rules
          ~compilation_targets
          ()
      =
      of_stages
        ~build_dir_only_sub_dirs
        ~directory_targets
        [ source_stage source_rules
        ; compilation_stage ~targets:compilation_targets compilation_rules
        ]
    ;;

    let source_rules t =
      List.filter t.stages ~f:(function
        | Source _ -> true
        | Compilation _ -> false)
      |> collect_stages
    ;;

    let compilation_targets t =
      let open Memo.O in
      let+ targets =
        Memo.parallel_map t.stages ~f:(function
          | Source _ -> Memo.return Rule_targets.empty
          | Compilation { targets; _ } -> Memo.Lazy.force targets)
      in
      List.fold_left targets ~init:Rule_targets.empty ~f:Rule_targets.union
    ;;

    let combine_exn
          r
          ({ build_dir_only_sub_dirs; directory_targets; rules = _; stages } as other)
      =
      match phys_equal r empty with
      | true -> other
      | false ->
        (match phys_equal other empty with
         | true -> r
         | false ->
           of_stages
             ~build_dir_only_sub_dirs:
               (Build_only_sub_dirs.union r.build_dir_only_sub_dirs build_dir_only_sub_dirs)
             ~directory_targets:
               (Path.Build.Map.union_exn r.directory_targets directory_targets)
             (r.stages @ stages))
    ;;
  end

  type t =
    | Rules of Rules.t
    | Unknown_context
    | Redirect_to_parent of Rules.t

  let no_rules = Rules Rules.empty

  let make ?build_dir_only_sub_dirs ?directory_targets rules =
    Rules (Rules.create ?build_dir_only_sub_dirs ?directory_targets rules)
  ;;

  let make_empty ~dir subdirs =
    Rules
      { Rules.empty with
        build_dir_only_sub_dirs = Build_only_sub_dirs.singleton ~dir subdirs
      }
  ;;

  let combine x y =
    match x, y with
    | Unknown_context, _ | _, Unknown_context -> Unknown_context
    | Rules x, Rules y -> Rules (Rules.combine_exn x y)
    | Rules x, Redirect_to_parent y
    | Redirect_to_parent x, Rules y
    | Redirect_to_parent x, Redirect_to_parent y ->
      Redirect_to_parent (Rules.combine_exn x y)
  ;;
end

module type Source_tree = sig
  module Dir : sig
    type t

    val sub_dir_names : t -> Filename.Array.Set.t
    val filenames : t -> Filename.Array.Set.t
  end

  val find_dir : Path.Source.t -> Dir.t option Memo.t
end

type t =
  { contexts : Context_type.t Context_name.Map.t Memo.Lazy.t
  ; rule_generator :
      Context_name.t -> dir:Path.Build.t -> string list -> Gen_rules.t Memo.t
  ; sandboxing_preference : Sandbox_mode.t list
  ; promote_source :
      chmod:(Permissions.Mode.t -> Permissions.Mode.t)
      -> delete_dst_if_it_is_a_directory:bool
      -> src:Path.Build.t
      -> dst:Path.Source.t
      -> unit Fiber.t
  ; implicit_default_alias : Path.Build.t -> unit Action_builder.t option Memo.t
  ; execution_parameters :
      Context_name.t -> dir:Path.Build.t -> Execution_parameters.t Memo.t
  ; source_tree : (module Source_tree)
  }

let t : t Fdecl.t = Fdecl.create Dyn.opaque
let get () = Fdecl.get t

let set
      ~contexts
      ~promote_source
      ~sandboxing_preference
      ~rule_generator
      ~implicit_default_alias
      ~execution_parameters
      ~source_tree
  =
  Fdecl.set
    t
    { contexts
    ; rule_generator
    ; sandboxing_preference =
        sandboxing_preference @ Sandbox_mode.all_except_patch_back_source_tree
    ; promote_source
    ; implicit_default_alias
    ; execution_parameters
    ; source_tree
    }
;;
