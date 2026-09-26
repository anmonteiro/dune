open Import
open Memo.O
module Gen_rules = Build_config.Gen_rules
module Context_type = Build_config.Context_type
module Build_only_sub_dirs = Gen_rules.Build_only_sub_dirs

module type Rule_generator = Gen_rules.Rule_generator

module Current_rule_loc = struct
  let t = ref (fun () -> Memo.return None)
  let set f = t := f
  let get () = !t ()
end

let set_current_rule_loc = Current_rule_loc.set

module Loaded = struct
  (* CR-someday amokhov: Loaded rules are relative to the directory passed to [load_dir],
     so these maps should probably be indexed by [Filename.t]s rather than [Path.t]s. We
     could add [Filename_map.t] anchored to a specific directory like [Filename_set.t]. *)
  type rules_here =
    { by_file_targets : Rule.t Path.Build.Map.t
    ; by_directory_targets : Rule.t Path.Build.Map.t
    }

  let no_rules_here =
    { by_file_targets = Path.Build.Map.empty
    ; by_directory_targets = Path.Build.Map.empty
    }
  ;;

  type build =
    { allowed_subdirs : Path.Unspecified.w Dir_set.t
    ; rules_here : rules_here
    ; aliases : (Loc.t * Rules.Dir_rules.Alias_spec.item) list Alias.Name.Map.t
    }

  type t =
    | Source of { filenames : Filename.Array.Set.t }
    | External of { filenames : Filename.Array.Set.t }
    | Build of build
    | Build_under_directory_target of { directory_target_ancestor : Path.Build.t }

  let no_rules ~allowed_subdirs =
    Build { allowed_subdirs; rules_here = no_rules_here; aliases = Alias.Name.Map.empty }
  ;;
end

module Dir_triage = struct
  module Build_directory = struct
    (* invariant: [dir = context_name / sub_dir] *)
    type t =
      { dir : Path.Build.t
      ; context_name : Context_name.t
      ; context_type : Context_type.t
      ; sub_dir : Path.Source.t
      }

    (* It's ok to only compare and hash the [dir] field because of the
       invariant. *)
    let equal a b = Path.Build.equal a.dir b.dir
    let hash t = Path.Build.hash t.dir
    let to_dyn t = Path.Build.to_dyn t.dir

    let parent t =
      Option.map (Path.Source.parent t.sub_dir) ~f:(fun sub_dir ->
        { t with dir = Path.Build.parent_exn t.dir; sub_dir })
    ;;
  end

  type t =
    | Known of Loaded.t
    | Build_directory of Build_directory.t

  let empty_source = Known (Source { filenames = Filename.Array.Set.empty })
  let no_rules = Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
end

let get_dir_triage ~dir =
  match Dpath.analyse_dir dir with
  | Source dir ->
    let module Source_tree = (val (Build_config.get ()).source_tree) in
    Source_tree.find_dir dir
    >>| (function
     | None -> Dir_triage.empty_source
     | Some dir -> Dir_triage.Known (Source { filenames = Source_tree.Dir.filenames dir }))
  | External dir_ext ->
    let+ filenames =
      Fs_memo.dir_contents (External dir_ext)
      >>| function
      | Error (Unix.ENOENT, _, _) -> Filename.Array.Set.empty
      | Error unix_error ->
        User_warning.emit
          [ Pp.textf "Unable to read %s" (Path.to_string_maybe_quoted dir)
          ; Unix_error.Detailed.pp_reason unix_error
          ];
        Filename.Array.Set.empty
      | Ok filenames ->
        Fs_memo.Dir_contents.to_list filenames
        |> List.filter_map ~f:(fun (filename, kind) ->
          match kind with
          | Unix.S_DIR -> None
          | _ -> Some filename)
        |> Filename.Array.Set.of_list
    in
    Dir_triage.Known (External { filenames })
  | Build (Regular Root) ->
    let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
    let allowed_subdirs =
      [ Path.Build.basename Dpath.Build.anonymous_actions_dir ]
      @ (Context_name.Map.keys contexts
         |> List.map ~f:(fun name -> Filename.of_string_exn (Context_name.to_string name))
        )
      |> Subdir_set.of_list
      |> Subdir_set.to_dir_set
    in
    Dir_triage.Known (Loaded.no_rules ~allowed_subdirs)
  | Build (Anonymous_action p) ->
    let build_dir = Dpath.Target_dir.build_dir p in
    Code_error.raise
      "Called get_dir_triage on an anonymous action directory"
      [ "dir", Path.Build.to_dyn build_dir ]
  | Build (Invalid _) ->
    Memo.return @@ Dir_triage.Known (Loaded.no_rules ~allowed_subdirs:Dir_set.empty)
  | Build (Regular (With_context (context_name, sub_dir))) ->
    let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
    (match Context_name.Map.find contexts context_name with
     | None -> Dir_triage.no_rules
     | Some ((_ : Build_context.t), context_type) ->
       (* In this branch, [dir] is in the build directory. *)
       let dir = Path.as_in_build_dir_exn dir in
       Dir_triage.Build_directory { dir; context_name; context_type; sub_dir })
;;

let describe_rule (rule : Rule.t) =
  Pp.text
  @@
  match rule.info with
  | From_dune_file loc ->
    let start = Loc.start loc in
    start.pos_fname ^ ":" ^ string_of_int start.pos_lnum
  | Internal -> "<internal location>"
  | Source_file_copy _ -> "file present in source tree"
;;

let report_rule_src_dir_conflict dir fn (rule : Rule.t) =
  let loc =
    match rule.info with
    | From_dune_file loc -> loc
    | Internal | Source_file_copy _ ->
      let dir =
        match Path.Build.drop_build_context dir with
        | None -> Path.build dir
        | Some s -> Path.source s
      in
      Loc.in_dir dir
  in
  User_error.raise
    ~loc
    [ Pp.textf
        "This rule defines a target %S whose name conflicts with a source directory in \
         the same directory."
        fn
    ]
    ~hints:
      [ Pp.textf
          "If you want Dune to generate and replace %S, add (mode promote) to the rule \
           stanza. Alternatively, you can delete %S from the source tree or change the \
           rule to generate a different target."
          fn
          fn
      ]
;;

let report_rule_conflict fn (rule' : Rule.t) (rule : Rule.t) =
  let fn = Path.build fn in
  User_error.raise
    [ Pp.textf "Multiple rules generated for %s:" (Path.to_string_maybe_quoted fn)
    ; Pp.enumerate ~f:describe_rule [ rule'; rule ]
    ]
    ~hints:
      (match rule.info, rule'.info with
       | Source_file_copy _, _ | _, Source_file_copy _ ->
         [ Pp.textf
             "rm -f %s"
             (Path.to_string_maybe_quoted (Path.drop_optional_build_context fn))
         ]
       | _ -> [])
;;

let anonymous_actions_dir dir =
  Path.Build.append_local Dpath.Build.anonymous_actions_dir (Path.Build.local dir)
;;

let read_cleanup_entries ~dir =
  let read dir =
    match Path.Untracked.readdir_unsorted_with_kinds (Path.build dir) with
    | Error _ -> []
    | Ok entries -> entries
  in
  read dir, read (anonymous_actions_dir dir)
;;

module Cleanup = struct
  type entry =
    { name : Filename.t
    ; kind : Unix.file_kind
    ; mutable removed : bool
    }

  type generation = unit ref

  type snapshot =
    | Materialized of entry array
    | Pending_files of
        { raw_entries : (Filename.t * Unix.file_kind) list
        ; sorted_entries : entry array Lazy.t
        }

  let materialize = function
    | Materialized entries -> entries
    | Pending_files { sorted_entries; _ } -> Lazy.force sorted_entries
  ;;

  type replay =
    { generation : generation
    ; files : snapshot
    ; anonymous_actions : snapshot
    ; mutable previous : generation option
    ; mutable reusable : bool
    }

  type receipt =
    { mutable generation : generation option
    ; mutable refined : Rules.Producer_id.Set.t
    }

  type summary =
    | File_bounds of Filename.t * Filename.t
    | Mask of Target_mask.t

  type entries =
    | Empty
    | Block of
        { summary : summary
        ; entries : entry list
        }
    | Node of
        { summary : summary
        ; left : entries
        ; right : entries
        }

  let empty_inventory = Lazy.from_val Empty

  let ready_inventory = function
    | Empty -> empty_inventory
    | (Block _ | Node _) as entries -> Lazy.from_val entries
  ;;

  type t =
    { dir : Path.Build.t
    ; mutable files : entries Lazy.t
    ; mutable anonymous_actions : entries Lazy.t
    ; mutable refined : Rules.Producer_id.Set.t
    ; replay : replay option
    }

  type location =
    | Files
    | Anonymous_actions

  type targets =
    { files : Filename.Set.t
    ; directories : Filename.Set.t
    ; subdirs : Subdir_set.t
    ; pending : Rules.Pending.t
    }

  type status =
    | Live
    | Pending
    | Stale

  let empty_summary = Mask Target_mask.empty

  let summary = function
    | Empty -> empty_summary
    | Block block -> block.summary
    | Node node -> node.summary
  ;;

  let rec filenames entries names =
    match entries with
    | Empty -> names
    | Block { entries; _ } ->
      List.fold_left entries ~init:names ~f:(fun names entry ->
        Filename.Set.add names entry.name)
    | Node { left; right; _ } -> filenames right (filenames left names)
  ;;

  let mask ~dir entries =
    match summary entries with
    | Mask mask -> mask
    | File_bounds _ ->
      (* Only mixed ancestors need exact masks of their regular-file children. *)
      Target_mask.paths ~dir (filenames entries Filename.Set.empty)
  ;;

  let union_summary ~dir left right =
    match summary left, summary right with
    | File_bounds (first, _), File_bounds (_, last) -> File_bounds (first, last)
    | _ -> Mask (Target_mask.union (mask ~dir left) (mask ~dir right))
  ;;

  let block_mask ~dir entries =
    let files, directories =
      List.fold_left
        entries
        ~init:(Filename.Set.empty, Target_mask.empty)
        ~f:(fun (files, directories) { name; kind; _ } ->
          match kind with
          | Unix.S_DIR ->
            let path = Path.Build.relative_fname dir name in
            files, Target_mask.union directories (Target_mask.subtree path)
          | _ -> Filename.Set.add files name, directories)
    in
    Target_mask.union (Target_mask.paths ~dir files) directories
  ;;

  let block_summary ~dir entries =
    match entries with
    | [] -> empty_summary
    | first :: rest ->
      if List.exists entries ~f:(fun entry -> entry.kind = Unix.S_DIR)
      then Mask (block_mask ~dir entries)
      else (
        let last = List.fold_left rest ~init:first.name ~f:(fun _ entry -> entry.name) in
        File_bounds (first.name, last))
  ;;

  type changed_names =
    | Unknown
    | No_names
    | Name_range of Filename.t * Filename.t

  let changed_names ~dir changed =
    (* Bounds cover both file and same-name directory ownership. Only pure-file
       exact requests can omit other names; all other requests use entry checks. *)
    if not (Target_mask.is_file_only changed)
    then Unknown
    else (
      match Target_mask.file_name_bounds changed ~dir with
      | `Non_exact -> Unknown
      | `Empty -> No_names
      | `Bounds (first, last) -> Name_range (first, last))
  ;;

  let intersects_summary changed names = function
    | Mask mask -> Target_mask.intersects changed mask
    | File_bounds (first, last) ->
      (match names with
       | Unknown -> true
       | No_names -> false
       | Name_range (changed_first, changed_last) ->
         Filename.compare last changed_first <> Lt
         && Filename.compare changed_last first <> Lt)
  ;;

  let intersects_entry ~dir changed { name; kind; _ } =
    match kind with
    | Unix.S_DIR ->
      let path = Path.Build.relative_fname dir name in
      Target_mask.mem_file changed path || Target_mask.intersects_directory changed path
    | _ -> Target_mask.mem_path changed ~dir name
  ;;

  let status location ~initial targets ~dir ~name ~kind =
    match location, kind with
    | Anonymous_actions, (Unix.S_REG | S_CHR | S_BLK | S_LNK | S_FIFO | S_SOCK) ->
      (* Anonymous action files cannot be classified from their names. *)
      Live
    | Files, _ | Anonymous_actions, Unix.S_DIR ->
      if
        Filename.Set.mem targets.directories name
        || (kind <> Unix.S_DIR && Filename.Set.mem targets.files name)
      then Live
      else if kind = Unix.S_DIR && Subdir_set.mem targets.subdirs name
      then
        if
          (* Later requests also retain directories advertised by pending aliases.
           Only the initial declarations are known to be permanent. *)
          initial
        then Live
        else Pending
      else if
        match kind with
        | Unix.S_DIR ->
          let path = Path.Build.relative_fname dir name in
          Rules.Pending.intersects_directory targets.pending path
        | _ -> Rules.Pending.mem_path targets.pending ~dir name
      then Pending
      else Stale
  ;;

  let actual_path location path =
    match location with
    | Files -> path
    | Anonymous_actions -> anonymous_actions_dir path
  ;;

  let remove location ~path ~kind =
    let path = actual_path location path in
    match kind with
    | Unix.S_DIR ->
      Rule_cache.Workspace_local.remove_subtree path;
      Path.rm_rf (Path.build path)
    | _ ->
      Rule_cache.Workspace_local.remove_target path;
      Fpath.unlink_exn (Path.Build.to_string path)
  ;;

  let sort_entries entries =
    Array.stable_sort entries ~cmp:(fun a b ->
      Ordering.to_int (Filename.compare a.name b.name));
    entries
  ;;

  let create_entries ~dir ~files_pending location targets entries =
    let snapshot =
      if
        Memo.is_incremental ()
        && files_pending
        && List.for_all entries ~f:(fun (_, kind) -> kind <> Unix.S_DIR)
      then
        Pending_files
          { raw_entries = entries
          ; sorted_entries =
              lazy
                (List.map entries ~f:(fun (name, kind) -> { name; kind; removed = false })
                 |> Array.of_list
                 |> sort_entries)
          }
      else
        Materialized
          (List.filter_map entries ~f:(fun (name, kind) ->
             let status =
               if files_pending && kind <> Unix.S_DIR
               then Pending
               else status location ~initial:true targets ~dir ~name ~kind
             in
             match status with
             | Live -> None
             | Pending -> Some { name; kind; removed = false }
             | Stale ->
               let path = Path.Build.relative_fname dir name in
               remove location ~path ~kind;
               None)
           |> Array.of_list
           |> sort_entries)
    in
    let build () =
      let entries = materialize snapshot in
      let rec build offset length =
        match length with
        | 0 -> Empty
        | length when length <= 8 ->
          let entries = List.init length ~f:(fun i -> entries.(offset + i)) in
          Block { summary = block_summary ~dir entries; entries }
        | _ ->
          let left_length = length / 2 in
          let left = build offset left_length in
          let right = build (offset + left_length) (length - left_length) in
          Node { summary = union_summary ~dir left right; left; right }
      in
      build 0 (Array.length entries)
    in
    let inventory =
      match snapshot with
      | Materialized entries when Array.length entries = 0 -> empty_inventory
      | Materialized _ | Pending_files _ ->
        if Memo.is_incremental () then lazy (build ()) else Lazy.from_val (build ())
    in
    inventory, snapshot
  ;;

  let create
        ~dir
        ~entries:(files, anonymous_actions)
        ~file_targets
        ~directory_targets
        ~subdirs_to_keep
        ~targets_to_keep
    =
    let files_pending =
      (not (List.is_empty files))
      && Filename.Set.is_empty file_targets
      && Filename.Set.is_empty directory_targets
      &&
      let all_files = Target_mask.files_in_directory dir in
      Target_mask.inter targets_to_keep all_files == all_files
    in
    let targets =
      { files = file_targets
      ; directories = directory_targets
      ; subdirs = subdirs_to_keep
      ; pending = Rules.Pending.of_mask targets_to_keep
      }
    in
    let files, initial_files = create_entries ~dir ~files_pending Files targets files in
    let anonymous_actions, initial_anonymous_actions =
      create_entries ~dir ~files_pending:false Anonymous_actions targets anonymous_actions
    in
    (* After an ownership change, fresh snapshots certify unchanged survivors,
       including entries later forgotten as live. *)
    let replay =
      if Memo.is_incremental ()
      then
        Some
          { generation = ref ()
          ; files = initial_files
          ; anonymous_actions = initial_anonymous_actions
          ; previous = None
          ; reusable = true
          }
      else None
    in
    { dir; files; anonymous_actions; refined = Rules.Producer_id.Set.empty; replay }
  ;;

  let unchanged_array_survivors previous current =
    let rec loop previous_index current_index =
      if current_index = Array.length current
      then true
      else if previous_index = Array.length previous
      then false
      else (
        let old = previous.(previous_index) in
        let entry = current.(current_index) in
        match Filename.compare old.name entry.name with
        | Lt -> loop (previous_index + 1) current_index
        | Gt -> false
        | Eq ->
          (not old.removed)
          && old.kind = entry.kind
          && loop (previous_index + 1) (current_index + 1))
    in
    loop 0 0
  ;;

  let unchanged_survivors previous current =
    match previous, current with
    | Pending_files previous, Pending_files current
      when List.equal
             (fun (name, kind) (other_name, other_kind) ->
                Filename.equal name other_name && kind = other_kind)
             previous.raw_entries
             current.raw_entries
           && ((not (Lazy.is_val previous.sorted_entries))
               || Array.for_all (Lazy.force previous.sorted_entries) ~f:(fun entry ->
                 not entry.removed)) ->
      (* No removal can precede materialization. A fresh scan must still
         reject a removed-and-recreated entry with the same name and kind. *)
      true
    | _ -> unchanged_array_survivors (materialize previous) (materialize current)
  ;;

  let prepare_replay (t : t) ~(previous : t) =
    match t.replay, previous.replay with
    | Some current, Some previous
      when previous.reusable
           && unchanged_survivors previous.files current.files
           && unchanged_survivors previous.anonymous_actions current.anonymous_actions ->
      current.previous <- Some previous.generation
    | _ -> ()
  ;;

  let receipt () = { generation = None; refined = Rules.Producer_id.Set.empty }

  let reusable (t : t) =
    match t.replay with
    | Some { reusable = true; _ } -> true
    | None | Some _ -> false
  ;;

  let reuse (t : t) (receipt : receipt) =
    match t.replay, receipt.generation with
    | Some { generation = current; reusable = true; _ }, Some generation
      when current == generation ->
      (* Another request may have advanced this inventory's frontier. *)
      receipt.refined <- t.refined;
      true
    | ( Some { generation = current; previous = Some previous; reusable = true; _ }
      , Some generation )
      when previous == generation ->
      t.refined <- receipt.refined;
      receipt.generation <- Some current;
      true
    | _ -> false
  ;;

  let remember (t : t) (receipt : receipt) =
    match t.replay with
    | None -> ()
    | Some replay ->
      receipt.generation <- Some replay.generation;
      receipt.refined <- t.refined
  ;;

  type entry_change =
    | Unchanged
    | Removed
    | Changed of entry

  let refine_entry ~dir location targets ({ name; kind; _ } as entry) =
    match status location ~initial:false targets ~dir ~name ~kind with
    | Live -> Removed
    | Pending -> Unchanged
    | Stale ->
      (* Another cleanup may already have removed this initial entry. Check
         only candidates for removal, rather than rereading the directory. *)
      let path = Path.Build.relative_fname dir name in
      (match Path.Untracked.lstat (Path.build (actual_path location path)) with
       | Error ((Unix.ENOENT | ENOTDIR), _, _) ->
         entry.removed <- true;
         Removed
       | Error error -> Unix_error.Detailed.raise error
       | Ok { Unix.st_kind = kind; _ } ->
         (match status location ~initial:false targets ~dir ~name ~kind with
          | Live -> Removed
          | Pending ->
            if kind = entry.kind then Unchanged else Changed { entry with kind }
          | Stale ->
            remove location ~path ~kind;
            entry.removed <- true;
            Removed))
  ;;

  let rec refine_block ~dir location targets changed entries =
    match entries with
    | [] -> entries, false
    | entry :: rest ->
      let change =
        if intersects_entry ~dir changed entry
        then refine_entry ~dir location targets entry
        else Unchanged
      in
      let rest', grew = refine_block ~dir location targets changed rest in
      (match change with
       | Unchanged -> (if rest == rest' then entries else entry :: rest'), grew
       | Removed -> rest', grew
       | Changed entry -> entry :: rest', true)
  ;;

  let rec refine_entries ~dir location targets changed names entries =
    if not (intersects_summary changed names (summary entries))
    then entries, false
    else (
      match entries with
      | Empty -> Empty, false
      | Block { summary = old_summary; entries = block } ->
        let block', grew = refine_block ~dir location targets changed block in
        if block == block'
        then entries, false
        else (
          match block' with
          | [] -> Empty, false
          | _ :: _ ->
            let summary =
              if not grew
              then old_summary
              else (
                match old_summary with
                | File_bounds _ -> block_summary ~dir block'
                | Mask old_mask ->
                  Mask (Target_mask.union old_mask (block_mask ~dir block')))
            in
            Block { summary; entries = block' }, grew)
      | Node { summary = old_summary; left; right } ->
        let left', left_grew = refine_entries ~dir location targets changed names left in
        let right', right_grew =
          refine_entries ~dir location targets changed names right
        in
        let grew = left_grew || right_grew in
        let entries =
          if left == left' && right == right'
          then entries
          else (
            match left', right' with
            | Empty, entries | entries, Empty -> entries
            | _, _ ->
              (* Keep old bounds when deleting leaves. A file becoming a
                 directory needs the exact subtree-aware masks of all survivors. *)
              let summary =
                if not grew
                then old_summary
                else (
                  match old_summary with
                  | File_bounds _ -> union_summary ~dir left' right'
                  | Mask old_mask ->
                    Mask
                      (Target_mask.union
                         old_mask
                         (Target_mask.union (mask ~dir left') (mask ~dir right'))))
              in
              Node { summary; left = left'; right = right' })
        in
        entries, grew)
  ;;

  let changed (t : t) refinements =
    (* Empty scans and exhausted refinements share this value; checking
       emptiness must not force a nonempty inventory. *)
    if t.files == empty_inventory && t.anonymous_actions == empty_inventory
    then Rules.Producer_id.Set.empty, Target_mask.empty
    else (
      let previous = t.refined in
      (* The frontier is request-local: a later query can revisit a producer
         that was left pending by the previous one. Only producers shared with
         that previous view can be skipped, not all producers seen this run. *)
      List.fold_left
        refinements
        ~init:(Rules.Producer_id.Set.empty, Target_mask.empty)
        ~f:(fun (refined, changed) { Rules.id; mask } ->
          let refined = Rules.Producer_id.Set.add refined id in
          if Rules.Producer_id.Set.mem previous id
          then refined, changed
          else refined, Target_mask.union changed mask))
  ;;

  let refine
        (t : t)
        ~changed
        ~file_targets
        ~directory_targets
        ~subdirs_to_keep
        ~targets_to_keep
    =
    let targets =
      { files = file_targets
      ; directories = directory_targets
      ; subdirs = subdirs_to_keep
      ; pending = targets_to_keep
      }
    in
    (* A partial failure or an observed kind change invalidates survivor
       proofs. This also prevents a later replay from using stale tree bounds. *)
    let reusable =
      match t.replay with
      | None -> false
      | Some replay ->
        let reusable = replay.reusable in
        replay.reusable <- false;
        reusable
    in
    let names = changed_names ~dir:t.dir changed in
    let files, files_grew =
      refine_entries ~dir:t.dir Files targets changed names (Lazy.force t.files)
    in
    t.files <- ready_inventory files;
    let anonymous_actions, actions_grew =
      refine_entries
        ~dir:t.dir
        Anonymous_actions
        targets
        changed
        names
        (Lazy.force t.anonymous_actions)
    in
    t.anonymous_actions <- ready_inventory anonymous_actions;
    match t.replay with
    | None -> ()
    | Some replay -> replay.reusable <- reusable && not (files_grew || actions_grew)
  ;;
end

let no_rule_found ~loc fn =
  let+ contexts = Memo.Lazy.force (Build_config.get ()).contexts in
  let fail fn ~loc =
    User_error.raise ?loc [ Pp.textf "No rule found for %s" (Dpath.describe_target fn) ]
  in
  let hints ctx =
    let candidates =
      Context_name.Map.to_list_map contexts ~f:(fun name _ -> Context_name.to_string name)
    in
    User_message.did_you_mean (Context_name.to_string ctx) ~candidates
  in
  match Dpath.analyse_target fn with
  | Other _ -> fail fn ~loc
  | Regular (ctx, _) ->
    if Context_name.Map.mem contexts ctx
    then fail fn ~loc
    else
      User_error.raise
        [ Pp.textf
            "Trying to build %s but build context %s doesn't exist."
            (Path.Build.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx)
  | Alias (ctx, fn') ->
    if Context_name.Map.mem contexts ctx
    then fail fn ~loc
    else (
      let fn = Path.append_source (Path.build (Context_name.build_dir ctx)) fn' in
      User_error.raise
        [ Pp.textf
            "Trying to build alias %s but build context %s doesn't exist."
            (Path.to_string_maybe_quoted fn)
            (Context_name.to_string ctx)
        ]
        ~hints:(hints ctx))
  | Anonymous_action _ ->
    (* We never lookup such actions by target name, so this should be
       unreachable *)
    Code_error.raise
      ?loc
      "Build_system.no_rule_found got anonymous action path"
      [ "fn", Path.Build.to_dyn fn ]
;;

module rec Load_rules : sig
  val load_dir : dir:Path.t -> Loaded.t Memo.t
  val load_file_selector : File_selector.t -> Loaded.t Memo.t
  val load_alias : Alias.t -> Loaded.t Memo.t
  val load_dir_for_target_impl : Path.Build.t -> Loaded.t Memo.t
  val load_dir_for_target : Path.Build.t -> Loaded.t Memo.t
  val load_dir_for_directory_target : Path.Build.t -> Loaded.t Memo.t
  val is_under_directory_target : Path.t -> bool Memo.t

  val lookup_alias
    :  Alias.t
    -> (Loc.t * Rules.Dir_rules.Alias_spec.item) list option Memo.t
end = struct
  open Load_rules

  let copy_source_action ~src_path ~build_path : Action.Full.t Action_builder.t =
    let action =
      Action.Full.make
        (Action.copy (Path.source src_path) build_path)
        (* Sandboxing this action doesn't make much sense: if we can copy [src_path] to
           the sandbox, we might as well copy it to the build directory directly. *)
        ~sandbox:Sandbox_config.no_sandboxing
    in
    Action_builder.Expert.record_dep_on_source_file_exn
      action
      ~loc:Current_rule_loc.get
      src_path
  ;;

  let source_copy_rules =
    Memo.exec
      (Memo.create
         "source-copy-rules"
         ~input:(module Path.Build)
         (fun dir ->
            let module Source_tree = (val (Build_config.get ()).source_tree) in
            let src_dir = Path.Build.drop_build_context_exn dir in
            let+ source_dir = Source_tree.find_dir src_dir in
            match source_dir with
            | None -> Filename.Array.Map.empty
            | Some source_dir ->
              Filename.Array.Map.of_set
                (Source_tree.Dir.filenames source_dir)
                ~f:(fun filename ->
                  let src_path = Path.Source.relative_fname src_dir filename in
                  let build_path = Path.Build.relative_fname dir filename in
                  Rule.make
                    ~info:(Source_file_copy src_path)
                    ~targets:(Targets.File.create build_path)
                    (copy_source_action ~src_path ~build_path))))
  ;;

  let create_copy_rules ~dir ~ctx_dir ~non_target_source_filenames =
    if Filename.Array.Set.is_empty non_target_source_filenames
    then Memo.return []
    else
      (* Cache unfiltered copies: the source and complete views can ignore
         different source files but must share the same copy-rule identities. *)
      let+ rules = source_copy_rules (Path.Build.append_source ctx_dir dir) in
      Filename.Array.Set.to_list_map non_target_source_filenames ~f:(fun filename ->
        Filename.Array.Map.find rules filename |> Option.value_exn)
  ;;

  let compile_rules ~dir ~source_dirs rules =
    let check_for_source_dir_conflict rule target =
      if Filename.Array.Set.mem source_dirs target
      then report_rule_src_dir_conflict dir (Filename.to_string target) rule
    in
    let add_targets rules ~targets rule =
      Filename.Set.fold targets ~init:rules ~f:(fun target rules ->
        check_for_source_dir_conflict rule target;
        let target = Path.Build.relative_fname rule.targets.root target in
        Path.Build.Map.update rules target ~f:(function
          | None -> Some rule
          | Some other -> Some (report_rule_conflict target other rule)))
    in
    let by_file_targets, by_directory_targets =
      List.fold_left
        rules
        ~init:(Path.Build.Map.empty, Path.Build.Map.empty)
        ~f:(fun (by_file_targets, by_directory_targets) rule ->
          assert (Path.Build.( = ) dir rule.Rule.targets.root);
          ( add_targets by_file_targets ~targets:rule.targets.files rule
          , add_targets by_directory_targets ~targets:rule.targets.dirs rule ))
    in
    (match
       ( Path.Build.Map.is_empty by_file_targets
       , Path.Build.Map.is_empty by_directory_targets )
     with
     | true, _ | _, true -> ()
     | false, false ->
       Path.Build.Map.iter2
         by_file_targets
         by_directory_targets
         ~f:(fun target rule1 rule2 ->
           match rule1, rule2 with
           | None, _ | _, None -> ()
           | Some rule1, Some rule2 -> report_rule_conflict target rule1 rule2));
    { Loaded.by_file_targets; by_directory_targets }
  ;;

  let compute_alias_expansions ~(collected : Rules.Dir_rules.ready) ~dir =
    let+ aliases =
      let aliases = collected.aliases in
      if Alias.Name.Map.mem aliases Alias.Name.default
      then Memo.return aliases
      else
        (Build_config.get ()).implicit_default_alias dir
        >>| function
        | None -> aliases
        | Some expansion ->
          Alias.Name.Map.set
            aliases
            Alias.Name.default
            { expansions =
                Appendable_list.singleton
                  (Loc.none, Rules.Dir_rules.Alias_spec.Deps expansion)
            }
    in
    Alias.Name.Map.map aliases ~f:(fun { Rules.Dir_rules.Alias_spec.expansions } ->
      (* CR-soon rgrinberg: hide this reversal behind the interface from
         [Alias_spec]. The order doesn't really matter, as we're just
         collecting the dependencies that are attached to the alias *)
      Appendable_list.to_list_rev expansions)
  ;;

  let add_non_fallback_rules ~init ~dir ~source_filenames rules =
    List.fold_left rules ~init ~f:(fun acc (rule : Rule.t) ->
      match rule.mode with
      | Standard | Promote _ | Ignore_source_files -> rule :: acc
      | Fallback ->
        let source_filenames_for_targets =
          if not (Filename.Set.is_empty rule.targets.dirs)
          then
            Code_error.raise
              "Unexpected directory target in a Fallback rule"
              [ "targets", Targets.Validated.to_dyn rule.targets ];
          if Path.Build.equal dir rule.targets.root
          then
            rule.targets.files
            |> Filename.Set.to_list
            |> Filename.Array.Set.of_sorted_list
          else Filename.Array.Set.empty
        in
        if Filename.Array.Set.is_subset source_filenames_for_targets ~of_:source_filenames
        then (* All targets are present *)
          acc
        else if
          Filename.Array.Set.are_disjoint source_filenames_for_targets source_filenames
        then (* No target is present *)
          rule :: acc
        else (
          let absent_targets =
            Filename.Array.Set.diff source_filenames_for_targets source_filenames
          in
          let present_targets =
            Filename.Array.Set.diff source_filenames_for_targets absent_targets
          in
          let dir = Path.source (Path.Build.drop_build_context_exn rule.targets.root) in
          User_error.raise
            ~loc:(Rule.loc rule)
            [ Pp.text
                "Some of the targets of this fallback rule are present in the source \
                 tree, and some are not. This is not allowed. Either none of the targets \
                 must be present in the source tree, either they must all be."
            ; Pp.nop
            ; Pp.text "The following targets are present:"
            ; Pp.enumerate
                ~f:Path.pp
                (Filename.Array.Set.to_list_map
                   present_targets
                   ~f:(Path.relative_fname dir))
            ; Pp.nop
            ; Pp.text "The following targets are not:"
            ; Pp.enumerate
                ~f:Path.pp
                (Filename.Array.Set.to_list_map
                   absent_targets
                   ~f:(Path.relative_fname dir))
            ]))
  ;;

  (** A directory is only allowed to be generated if its parent knows about it.
      This restriction is necessary to prevent stale artifact deletion from
      removing that directory.

      This module encodes that restriction. *)
  module Generated_directory_restrictions : sig
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    (** Used by the child to ask about the restrictions placed by the parent. *)
    val allowed_by_parent : dir:Path.Build.t -> restriction Memo.t
  end = struct
    type restriction =
      | Unrestricted
      | Restricted of Path.Unspecified.w Dir_set.t Memo.Lazy.t

    let source_subdirs_of_build_dir ~dir =
      let module Source_tree = (val (Build_config.get ()).source_tree) in
      let corresponding_source_dir =
        match Dpath.analyse_target dir with
        | Alias _ | Anonymous_action _ | Other _ -> Memo.return None
        | Regular (_ctx, sub_dir) -> Source_tree.find_dir sub_dir
      in
      corresponding_source_dir
      >>| function
      | None -> Filename.Array.Set.empty
      | Some dir -> Source_tree.Dir.sub_dir_names dir
    ;;

    let allowed_dirs ~dir ~subdir : restriction Memo.t =
      let+ subdirs = source_subdirs_of_build_dir ~dir in
      if Filename.Array.Set.mem subdirs subdir
      then Unrestricted
      else
        Restricted
          (Memo.Lazy.create ~name:"allowed_dirs" (fun () ->
             load_dir ~dir:(Path.build dir)
             >>| function
             | External _ | Source _ -> Dir_set.just_the_root
             | Build { allowed_subdirs; _ } -> Dir_set.descend allowed_subdirs subdir
             | Build_under_directory_target _ -> Dir_set.empty))
    ;;

    let allowed_by_parent ~dir =
      allowed_dirs ~dir:(Path.Build.parent_exn dir) ~subdir:(Path.Build.basename dir)
    ;;
  end

  let declared_descendants ~dir ~inherited_descendants declarations =
    lazy
      (Dir_set.union
         (Lazy.force inherited_descendants)
         (Build_only_sub_dirs.find declarations dir |> Subdir_set.to_dir_set))
  ;;

  module Normal = struct
    type t =
      { build_dir_only_sub_dirs : Build_only_sub_dirs.t
      ; inherited_descendants : Path.Unspecified.w Dir_set.t Lazy.t
      ; declared_descendants : Path.Unspecified.w Dir_set.t Lazy.t
      ; directory_targets : Loc.t Path.Build.Map.t
      ; rules : Rules.t Memo.Lazy.t
      }

    let combine_exn ~dir r { build_dir_only_sub_dirs; directory_targets; rules; _ } =
      let build_dir_only_sub_dirs =
        Build_only_sub_dirs.union r.build_dir_only_sub_dirs build_dir_only_sub_dirs
      in
      (* The child cannot declare ancestors. Its inherited directories are
         exactly the projection of its immediate parent's declarations. *)
      let inherited_descendants =
        lazy
          (Dir_set.descend (Lazy.force r.declared_descendants) (Path.Build.basename dir)
           |> Dir_set.forget_root)
      in
      { build_dir_only_sub_dirs
      ; inherited_descendants
      ; declared_descendants =
          declared_descendants ~dir ~inherited_descendants build_dir_only_sub_dirs
      ; directory_targets = Path.Build.Map.union_exn r.directory_targets directory_targets
      ; rules =
          Memo.lazy_ ~name:"union-rules" (fun () ->
            let open Memo.O in
            let+ r = Memo.Lazy.force r.rules
            and+ r' = Memo.Lazy.force rules in
            Rules.union r r')
      }
    ;;

    let check_all_directory_targets_are_descendant ~of_:dir directory_targets =
      Path.Build.Map.iteri directory_targets ~f:(fun p _loc ->
        if not (Path.Build.is_descendant p ~of_:dir)
        then
          Code_error.raise
            "[gen_rules] returned directory target in a directory that is not a \
             descendant of the directory it was called for"
            [ "dir", Path.Build.to_dyn dir; "example", Path.Build.to_dyn p ])
    ;;

    let check_all_sub_dirs_rule_dirs_are_descendant ~of_:dir build_dir_only_sub_dirs =
      Build_only_sub_dirs.iter_dirs_containing_sub_dirs
        build_dir_only_sub_dirs
        ~f:(fun p ->
          if not (Path.Build.is_descendant p ~of_:dir)
          then
            Code_error.raise
              "[gen_rules] returned sub-directories in a directory that is not a \
               descendant of the directory it was called for"
              [ "dir", Path.Build.to_dyn dir; "example", Path.Build.to_dyn p ])
    ;;

    let make_rules_gen_result
          ~of_
          { Gen_rules.Rules.build_dir_only_sub_dirs; directory_targets; rules }
      =
      check_all_directory_targets_are_descendant ~of_ directory_targets;
      check_all_sub_dirs_rule_dirs_are_descendant ~of_ build_dir_only_sub_dirs;
      let rules =
        Memo.lazy_ ~name:"check-rules-are-descendant" (fun () ->
          let+ rules = rules in
          Rules.restrict_to_directory rules ~dir:of_)
      in
      let inherited_descendants = lazy Dir_set.empty in
      { build_dir_only_sub_dirs
      ; inherited_descendants
      ; declared_descendants =
          declared_descendants ~dir:of_ ~inherited_descendants build_dir_only_sub_dirs
      ; directory_targets
      ; rules
      }
    ;;
  end

  type gen_rules_result =
    | Under_directory_target of { directory_target_ancestor : Path.Build.t }
    | Normal of Normal.t

  module rec Gen_rules : sig
    val gen_rules : Dir_triage.Build_directory.t -> gen_rules_result Memo.t
  end = struct
    let call_rules_generator
          { Dir_triage.Build_directory.dir; context_name; context_type = _; sub_dir }
      =
      let (module RG : Rule_generator) = (Build_config.get ()).rule_generator in
      let sub_dir_components = Path.Source.explode sub_dir |> Filename.L.to_string in
      RG.gen_rules context_name ~dir sub_dir_components
      >>= function
      | Rules rules -> Memo.return (Normal.make_rules_gen_result ~of_:dir rules)
      | Unknown_context ->
        Code_error.raise
          "[gen_rules] did not specify rules for the context"
          [ "context_name", Context_name.to_dyn context_name ]
    ;;

    let gen_rules_impl d =
      match Dir_triage.Build_directory.parent d with
      | None -> call_rules_generator d >>| fun rules -> Normal rules
      | Some d' ->
        Gen_rules.gen_rules d'
        >>= (function
         | Under_directory_target _ as res -> Memo.return res
         | Normal rules ->
           if Path.Build.Map.mem rules.directory_targets d.dir
           then Memo.return (Under_directory_target { directory_target_ancestor = d.dir })
           else
             let+ child = call_rules_generator d in
             Normal (Normal.combine_exn ~dir:d.dir rules child))
    ;;

    let gen_rules =
      let memo =
        Memo.create ~input:(module Dir_triage.Build_directory) "gen-rules" gen_rules_impl
      in
      fun x -> Memo.exec memo x
    ;;
  end

  let report_rule_internal_dir_conflict target_name loc =
    User_error.raise
      ~loc
      [ Pp.textf
          "This rule defines a target %S whose name conflicts with an internal directory \
           used by Dune. Please use a different name."
          (Filename.to_string target_name)
      ]
  ;;

  type source_paths_to_ignore =
    { filenames : Filename.Array.Set.t
    ; dirnames : Filename.Array.Set.t
    }

  (* Compute source paths ignored by specific rules *)
  let source_paths_to_ignore ~dir build_dir_only_sub_dirs rules : source_paths_to_ignore =
    let of_filename_set set =
      Filename.Set.to_list set |> Filename.Array.Set.of_sorted_list
    in
    let rec iter ~filenames ~dirnames rules =
      match rules with
      | [] ->
        { filenames = of_filename_set filenames; dirnames = of_filename_set dirnames }
      | ({ Rule.targets; mode; _ } as rule) :: rules
        when Path.Build.equal dir targets.root ->
        let target_filenames = targets.files in
        let target_dirnames = targets.dirs in
        (* Check if this rule defines any file targets that conflict with internal Dune
           directories listed in [build_dir_only_sub_dirs]. We don't check directory
           targets as these are already checked earlier. *)
        (match
           Filename.Set.find target_filenames ~f:(Subdir_set.mem build_dir_only_sub_dirs)
         with
         | None -> ()
         | Some target_name ->
           report_rule_internal_dir_conflict target_name (Rule.loc rule));
        (match mode with
         | Standard | Fallback -> iter ~filenames ~dirnames rules
         | Ignore_source_files ->
           iter
             ~filenames:(Filename.Set.union filenames target_filenames)
             ~dirnames:(Filename.Set.union dirnames target_dirnames)
             rules
         | Promote { only; _ } ->
           (* Note that the [only] predicate applies to the files inside the
              directory targets rather than to directory names themselves. *)
           let target_filenames =
             match only with
             | None -> target_filenames
             | Some pred ->
               let is_promoted filename = Predicate.test pred filename in
               Filename.Set.filter target_filenames ~f:is_promoted
           in
           iter
             ~filenames:(Filename.Set.union filenames target_filenames)
             ~dirnames:(Filename.Set.union dirnames target_dirnames)
             rules)
      | _ :: rules -> iter ~filenames ~dirnames rules
    in
    iter ~filenames:Filename.Set.empty ~dirnames:Filename.Set.empty rules
  ;;

  module Source_files_and_dirs = struct
    type t =
      { source_filenames : Filename.Array.Set.t
      ; source_dirs : Filename.Array.Set.t
      ; exists : bool
      }

    let empty =
      { source_filenames = Filename.Array.Set.empty
      ; source_dirs = Filename.Array.Set.empty
      ; exists = false
      }
    ;;
  end

  let source_files_and_dirs source_files source_paths_to_ignore =
    if
      Filename.Array.Set.is_empty source_paths_to_ignore.filenames
      && Filename.Array.Set.is_empty source_paths_to_ignore.dirnames
    then source_files
    else (
      let { Source_files_and_dirs.source_filenames; source_dirs; exists } =
        source_files
      in
      { Source_files_and_dirs.source_filenames =
          Filename.Array.Set.diff source_filenames source_paths_to_ignore.filenames
      ; source_dirs = Filename.Array.Set.diff source_dirs source_paths_to_ignore.dirnames
      ; exists
      })
  ;;

  let local_subdirs_to_keep build_dir_only_sub_dirs ~source_dirs =
    let source_dirs_to_keep =
      Filename.Array.Set.fold source_dirs ~init:Dir_set.empty ~f:(fun path acc ->
        let path = Path.Local.relative_fname Path.Local.root path in
        Dir_set.union acc (Dir_set.singleton path))
    in
    Dir_set.union source_dirs_to_keep (Subdir_set.to_dir_set build_dir_only_sub_dirs)
  ;;

  let local_descendants_to_keep ~dir ~local_subdirs rules_produced =
    let rules_generated_in =
      Rules.Revealed.directories rules_produced
      |> Path.Build.Set.fold ~init:Dir_set.empty ~f:(fun p acc ->
        match Path.Local_gen.descendant ~of_:dir p with
        | None -> acc
        | Some p -> Dir_set.union acc (Dir_set.singleton p))
    in
    Dir_set.union rules_generated_in local_subdirs
  ;;

  let partial_directory_is_visible
        ({ Dir_triage.Build_directory.dir; _ } as build_dir)
        ~inherited_descendants
        ~is_source_dir
    =
    match Dir_triage.Build_directory.parent build_dir with
    | None -> Memo.return (lazy true)
    | Some _ when is_source_dir || Dir_set.here (Lazy.force inherited_descendants) ->
      Memo.return (lazy true)
    | Some parent ->
      let* generated = Gen_rules.gen_rules parent in
      (match generated with
       | Under_directory_target _ -> Memo.return (lazy true)
       | Normal { rules; _ } ->
         (* Initial cleanup already awaited this parent. Track its declarations
            in directory metadata; only the pure visibility test stays lazy. *)
         let+ rules = Memo.Lazy.force rules in
         lazy
           (let here =
              Target_mask.union
                (Target_mask.files_in_directory dir)
                (Target_mask.union
                   (Target_mask.directories_in_directory dir)
                   (Target_mask.aliases_in_directory dir))
            in
            Target_mask.intersects (Rules.targets rules) here))
  ;;

  let check_partial_directory_visibility ~dir ~visible rules_produced =
    if
      Path.Build.Set.mem (Rules.Revealed.directories rules_produced) dir
      && not (Lazy.force visible)
    then (
      let dir_rules = Rules.Revealed.find rules_produced ~dir in
      Code_error.raise
        "Generated rules in a directory not allowed by the parent"
        [ "dir", Path.Build.to_dyn dir; "rules", Rules.Dir_rules.to_dyn dir_rules ])
  ;;

  let descendants_to_keep
        { Dir_triage.Build_directory.dir; context_name = _; context_type; sub_dir }
        ~local_subdirs
        rules_produced
    =
    let* allowed_by_parent =
      match context_type, Path.Source.to_string sub_dir with
      | With_sources, ".dune" ->
        (* GROSS HACK: this is to avoid a cycle as the rules for all
           directories force the generation of ".dune/configurator". We need a
           better way to deal with such cases. *)
        Memo.return Generated_directory_restrictions.Unrestricted
      | _ -> Generated_directory_restrictions.allowed_by_parent ~dir
    in
    let* () =
      match allowed_by_parent with
      | Unrestricted -> Memo.return ()
      | Restricted restriction ->
        (match Path.Build.Set.mem (Rules.Revealed.directories rules_produced) dir with
         | false -> Memo.return ()
         | true ->
           let+ restriction = Memo.Lazy.force restriction in
           if not (Dir_set.here restriction)
           then (
             let rules = Rules.Revealed.find rules_produced ~dir in
             Code_error.raise
               "Generated rules in a directory not allowed by the parent"
               [ "dir", Path.Build.to_dyn dir
               ; "rules", Rules.Dir_rules.to_dyn rules
               ; "restriction", Dir_set.to_dyn restriction
               ]))
    in
    let local_descendants =
      local_descendants_to_keep ~dir ~local_subdirs rules_produced
    in
    let+ allowed_grand_descendants_of_parent =
      match allowed_by_parent with
      | Unrestricted ->
        (* In this case the parent isn't going to be able to create any
           generated grand descendant directories. Rules that attempt to do
           so may run into the [allowed_by_parent] check or will be simply
           ignored. *)
        Memo.return Dir_set.empty
      | Restricted restriction -> Memo.Lazy.force restriction
    in
    Dir_set.union local_descendants allowed_grand_descendants_of_parent
  ;;

  let validate_directory_targets ~dir ~real_directory_targets ~directory_targets =
    if
      not
        (Path.Build.Map.equal real_directory_targets directory_targets ~equal:(fun _ _ ->
           (* The locations should match if the declaration knows which
               rule will generate the directory, but it's not necessary
               as the rule's actual location has higher priority. *)
           true))
    then (
      let mismatched_directories =
        let error message loc =
          Dyn.record [ "message", Dyn.string message; "loc", Loc.to_dyn_hum loc ]
        in
        Path.Build.Map.merge
          real_directory_targets
          directory_targets
          ~f:(fun _ generated declared ->
            match generated, declared with
            | None, None | Some _, Some _ -> None
            | Some loc, None -> Some (error "not declared" loc)
            | None, Some loc -> Some (error "not generated" loc))
      in
      Code_error.raise
        "gen_rules returned a set of directory targets that doesn't match the set of \
         directory targets from returned rules"
        [ "dir", Path.Build.to_dyn dir
        ; "mismatched_directories", Path.Build.Map.to_dyn Fun.id mismatched_directories
        ])
  ;;

  type request =
    | Complete
    | Target of Path.Build.t
    | Directory_target of Path.Build.t
    | Files of File_selector.t
    | Alias of Alias.t

  let request_mask ~dir = function
    | Complete -> Target_mask.subtree dir
    | Target target -> Target_mask.path target
    | Directory_target target -> Target_mask.directories [ target ]
    | Files selector -> Target_mask.paths_matching ~dir (File_selector.predicate selector)
    | Alias alias -> Target_mask.aliases [ alias ]
  ;;

  let compile_directory_rules
        { Dir_triage.Build_directory.dir; context_name; context_type; sub_dir }
        ~build_dir_only_sub_dirs
        ~request
        ~source_files
        rules_produced
    =
    let collected =
      Rules.find rules_produced (Path.build dir) |> Rules.Dir_rules.consume
    in
    let rules = collected.rules in
    let { Source_files_and_dirs.source_filenames; source_dirs; _ } =
      match context_type with
      | Empty -> Source_files_and_dirs.empty
      | With_sources ->
        let source_paths_to_ignore =
          source_paths_to_ignore ~dir build_dir_only_sub_dirs rules
        in
        source_files_and_dirs source_files source_paths_to_ignore
    in
    let* rules =
      if Filename.Array.Set.is_empty source_filenames
      then Memo.return rules
      else (
        let ctx_dir = Context_name.build_dir context_name in
        let selected_targets =
          List.fold_left rules ~init:Filename.Set.empty ~f:(fun names (rule : Rule.t) ->
            Filename.Set.union
              names
              (Filename.Set.union rule.targets.files rule.targets.dirs))
        in
        let non_target_source_filenames =
          let select names =
            Filename.Set.to_list names
            |> List.filter ~f:(Filename.Array.Set.mem source_filenames)
            |> Filename.Array.Set.of_sorted_list
          in
          match request with
          | Complete -> source_filenames
          | Files selector ->
            Filename.Array.Set.filter source_filenames ~f:(fun filename ->
              Filename.Set.mem selected_targets filename
              || File_selector.test_basename selector ~basename:filename)
          | Target target ->
            select (Filename.Set.add selected_targets (Path.Build.basename target))
          | Directory_target _ | Alias _ -> select selected_targets
        in
        let+ copy_rules =
          create_copy_rules ~dir:sub_dir ~ctx_dir ~non_target_source_filenames
        in
        add_non_fallback_rules ~init:copy_rules ~dir ~source_filenames rules)
    in
    Memo.return (source_dirs, collected, compile_rules ~dir ~source_dirs rules)
  ;;

  let check_directory_targets ~dir ~build_dir_only_sub_dirs directory_targets =
    Path.Build.Map.iteri directory_targets ~f:(fun dir_target loc ->
      let name = Path.Build.basename dir_target in
      if
        Path.Build.equal (Path.Build.parent_exn dir_target) dir
        && Subdir_set.mem build_dir_only_sub_dirs name
      then report_rule_internal_dir_conflict name loc)
  ;;

  let cleanup_target_names ~dir ~source_filenames rules =
    let files, dirs = Rules.Revealed.target_names rules ~dir in
    Filename.Set.union source_filenames files, dirs
  ;;

  module Completed_targets = struct
    type t =
      { by_target : Loaded.t Path.Build.Table.t
      ; build_directories : Dir_triage.Build_directory.t Path.Build.Table.t
      }

    let get =
      let state : (Memo.Run.t * t) option ref = ref None in
      let+ run = Memo.current_run () in
      match !state with
      | Some (previous, table) when previous == run -> table
      | _ ->
        let table =
          { by_target = Path.Build.Table.create 16
          ; build_directories = Path.Build.Table.create 16
          }
        in
        state := Some (run, table);
        table
    ;;

    let remember { by_target = completed_targets; _ } (loaded : Loaded.build) =
      (* Successful file queries validate the whole output closure. Reuse
         positive answers only, without depending on the broader query:
         both this table and its consumers are invalidated on every run.
         Target queries only inspect the requested target's map entry, not
         the aliases or subdirectories of the saved view. *)
      let result = Loaded.Build loaded in
      Path.Build.Map.iteri loaded.rules_here.by_file_targets ~f:(fun target rule ->
        if
          Filename.Set.is_empty rule.Rule.targets.dirs
          && not (Path.Build.Table.mem completed_targets target)
        then Path.Build.Table.set completed_targets target result);
      result
    ;;

    let remember_target { by_target = completed_targets; _ } target loaded =
      let reusable =
        match loaded with
        | Loaded.Build { rules_here; _ } ->
          Path.Build.Map.mem rules_here.by_directory_targets target
        | Build_under_directory_target _ ->
          (* This only classifies the path. Resolving its owning rule still
             goes through an ordinary target lookup. *)
          true
        | Source _ | External _ -> false
      in
      if reusable && not (Path.Build.Table.mem completed_targets target)
      then Path.Build.Table.set completed_targets target loaded
    ;;
  end

  module Initial_directory = struct
    type partial_retention =
      { revealed : Rules.Revealed.t
      ; pending : Rules.Pending.t
      ; descendants : Path.Unspecified.w Dir_set.t
      }

    type info =
      { normal : Normal.t
      ; rules : Rules.t
      ; source_filenames : Filename.Set.t
      ; source_files : Source_files_and_dirs.t
      ; local_subdirs : Path.Unspecified.w Dir_set.t
      ; partial_directory_is_visible : bool Lazy.t
      ; mutable partial_retention : partial_retention option
      }

    type t =
      { info : info
      ; cleanup : Cleanup.t
      ; completed_targets : Completed_targets.t
      ; run : Memo.Run.t
      }

    type result =
      | Under_directory_target of { directory_target_ancestor : Path.Build.t }
      | Normal of t
  end

  module Cleanup_state = struct
    type t = { mutable current : Initial_directory.t option }

    let get =
      Memo.exec
        (Memo.create
           "rule-cleanup-state"
           ~input:(module Dir_triage.Build_directory)
           (fun _ -> Memo.return { current = None }))
    ;;
  end

  let read_directory_info
        ({ Dir_triage.Build_directory.dir; context_type; sub_dir; _ } as build_dir)
        normal
    =
    let module Source_tree = (val (Build_config.get ()).source_tree) in
    let* source_dir =
      match context_type with
      | Empty -> Memo.return None
      | With_sources -> Source_tree.find_dir sub_dir
    in
    let source_filenames, source_dirs =
      match source_dir with
      | None -> Filename.Array.Set.empty, Filename.Array.Set.empty
      | Some source_dir ->
        Source_tree.Dir.filenames source_dir, Source_tree.Dir.sub_dir_names source_dir
    in
    let source_files =
      { Source_files_and_dirs.source_filenames
      ; source_dirs
      ; exists = Option.is_some source_dir
      }
    in
    let* rules = Memo.Lazy.force normal.Normal.rules in
    let source_filenames =
      Filename.Array.Set.to_list source_filenames |> Filename.Set.of_list
    in
    let local_subdirs =
      local_subdirs_to_keep
        (Build_only_sub_dirs.find normal.build_dir_only_sub_dirs dir)
        ~source_dirs
    in
    let+ partial_directory_is_visible =
      partial_directory_is_visible
        build_dir
        ~inherited_descendants:normal.inherited_descendants
        ~is_source_dir:source_files.exists
    in
    { Initial_directory.normal
    ; rules
    ; source_filenames
    ; source_files
    ; local_subdirs
    ; partial_directory_is_visible
    ; partial_retention = None
    }
  ;;

  let directory_info =
    Memo.exec
      (Memo.create
         "initial-directory-info"
         ~input:(module Dir_triage.Build_directory)
         (fun build_dir ->
            Gen_rules.gen_rules build_dir
            >>= function
            | Under_directory_target _ ->
              Code_error.raise "Reading rule metadata inside a directory target" []
            | Normal normal -> read_directory_info build_dir normal))
  ;;

  let rec initial_cleanup build_dir =
    Memo.exec (Lazy.force initial_cleanup_memo) build_dir

  and initial_cleanup_memo =
    lazy
      (Memo.create
         "initial-rule-cleanup"
         ~input:(module Dir_triage.Build_directory)
         (fun ({ Dir_triage.Build_directory.dir; _ } as build_dir) ->
            let* run = Memo.current_run () in
            let* generated = Gen_rules.gen_rules build_dir in
            match generated with
            | Under_directory_target { directory_target_ancestor } ->
              Memo.return
                (Initial_directory.Under_directory_target { directory_target_ancestor })
            | Normal normal ->
              let build_dir_only_sub_dirs =
                Build_only_sub_dirs.find normal.build_dir_only_sub_dirs dir
              in
              check_directory_targets
                ~dir
                ~build_dir_only_sub_dirs
                normal.directory_targets;
              (* Rule inheritance does not load parent directories. Clean their
                 stale children before an action can use a descendant, without
                 pulling any of the parent's suspended producers. *)
              let* () =
                Memo.Option.iter
                  (Dir_triage.Build_directory.parent build_dir)
                  ~f:(fun parent -> initial_cleanup parent >>| ignore)
              in
              let* info =
                if Memo.is_incremental ()
                then directory_info build_dir
                else read_directory_info build_dir normal
              in
              let* cleanup_state =
                if Memo.is_incremental ()
                then Cleanup_state.get build_dir >>| Option.some
                else Memo.return None
              in
              let+ completed_targets = Completed_targets.get in
              let previous = Option.bind cleanup_state ~f:(fun state -> state.current) in
              let cleanup =
                match previous with
                | Some previous
                  when previous.info == info
                       && Cleanup.reusable previous.cleanup
                       && Rules.unchanged_since info.rules ~since:previous.run ->
                  (* The proof must precede producer restoration: checking after
                     generation could capture files created by this run's actions.
                     An unchanged ownership graph keeps the existing inventory,
                     including its committed removals and pending entries. *)
                  previous.cleanup
                | None | Some _ ->
                  let { Initial_directory.rules; source_filenames; local_subdirs; _ } =
                    info
                  in
                  let revealed = Rules.Revealed.of_rules rules in
                  let file_targets, directory_targets =
                    cleanup_target_names ~dir ~source_filenames revealed
                  in
                  let targets_to_keep = Rules.targets rules in
                  let subdirs_to_keep =
                    Dir_set.union
                      (local_descendants_to_keep ~dir ~local_subdirs revealed)
                      (Lazy.force normal.inherited_descendants)
                    |> Subdir_set.of_dir_set
                  in
                  let entries = read_cleanup_entries ~dir in
                  Cleanup.create
                    ~dir
                    ~entries
                    ~file_targets
                    ~directory_targets
                    ~subdirs_to_keep
                    ~targets_to_keep
              in
              let initial = { Initial_directory.info; cleanup; completed_targets; run } in
              (match cleanup_state with
               | None -> ()
               | Some state ->
                 (* Advance on broad and ancestor-only loads too, not just
                    when a point query prepares this directory. *)
                 (match state.current with
                  | Some previous
                    when previous.info == info && previous.cleanup != cleanup ->
                    Cleanup.prepare_replay cleanup ~previous:previous.cleanup
                  | _ -> ());
                 state.current <- Some initial);
              Initial_directory.Normal initial))
  ;;

  module Compiled_selection = struct
    type t =
      { loaded : Loaded.build
      ; collected : Rules.Dir_rules.ready
      ; refinements : Rules.refinement list
      ; cleanup_targets : Cleanup.targets Lazy.t
      }
  end

  let compile_selection
        ({ Dir_triage.Build_directory.dir; _ } as build_dir)
        ({ Initial_directory.normal
         ; source_filenames
         ; source_files
         ; local_subdirs
         ; partial_directory_is_visible
         ; _
         } as info)
        ~request
        { Rules.selected = rules_produced; revealed; pending; refinements }
    =
    let { Normal.rules = _
        ; build_dir_only_sub_dirs = declared_subdirs
        ; inherited_descendants
        ; directory_targets
        ; _
        }
      =
      normal
    in
    let build_dir_only_sub_dirs = Build_only_sub_dirs.find declared_subdirs dir in
    let* source_dirs, collected, rules_here =
      compile_directory_rules
        build_dir
        ~build_dir_only_sub_dirs
        ~request
        ~source_files
        rules_produced
    in
    let unchanged_source_dirs = source_dirs == source_files.source_dirs in
    let local_subdirs =
      (* Promotion can remove source directories from this request's view. *)
      if unchanged_source_dirs
      then local_subdirs
      else local_subdirs_to_keep build_dir_only_sub_dirs ~source_dirs
    in
    let revealed =
      (* Source-only requests can wrap the same immutable chunks afresh. *)
      match request, info.partial_retention with
      | (Target _ | Directory_target _ | Files _ | Alias _), Some cached
        when Rules.Revealed.same_components cached.revealed revealed -> cached.revealed
      | _ -> revealed
    in
    let+ descendants_to_keep =
      match request with
      | Complete -> descendants_to_keep build_dir ~local_subdirs revealed
      | Target _ | Directory_target _ | Files _ | Alias _ ->
        check_partial_directory_visibility
          ~dir
          ~visible:partial_directory_is_visible
          revealed;
        Memo.return
          (match info.partial_retention with
           | Some cached
             when unchanged_source_dirs
                  && cached.revealed == revealed
                  && Rules.Pending.same_components cached.pending pending ->
             cached.descendants
           | _ ->
             let descendants =
               Dir_set.union_all
                 [ local_descendants_to_keep ~dir ~local_subdirs revealed
                 ; Lazy.force inherited_descendants
                 ; Rules.Pending.alias_directories pending ~dir
                 ]
             in
             if unchanged_source_dirs
             then info.partial_retention <- Some { revealed; pending; descendants };
             descendants)
    in
    let real_directory_targets = Rules.Revealed.directory_targets revealed in
    Path.Build.Map.iteri real_directory_targets ~f:(fun target _ ->
      if not (Path.Build.Map.mem directory_targets target)
      then
        Code_error.raise
          "Rule stage produced an undeclared directory target"
          [ "target", Path.Build.to_dyn target ]);
    (match request with
     | Complete ->
       let mask = Target_mask.subtree dir in
       let within_request targets =
         Path.Build.Map.filteri targets ~f:(fun path _ ->
           Target_mask.mem_directory mask path)
       in
       validate_directory_targets
         ~dir
         ~real_directory_targets:(within_request real_directory_targets)
         ~directory_targets:(within_request directory_targets)
     | Target _ | Directory_target _ | Files _ | Alias _ -> ());
    let cleanup_targets =
      lazy
        (let files, directories = cleanup_target_names ~dir ~source_filenames revealed in
         { Cleanup.files
         ; directories
         ; subdirs = Subdir_set.of_dir_set descendants_to_keep
         ; pending
         })
    in
    { Compiled_selection.loaded =
        { Loaded.allowed_subdirs = descendants_to_keep
        ; rules_here
        ; aliases = Alias.Name.Map.empty
        }
    ; collected
    ; refinements
    ; cleanup_targets
    }
  ;;

  let refine_cleanup cleanup { Compiled_selection.refinements; cleanup_targets; _ } =
    let refined, changed = Cleanup.changed cleanup refinements in
    if not (Target_mask.is_empty changed)
    then (
      let { Cleanup.files; directories; subdirs; pending } = Lazy.force cleanup_targets in
      Cleanup.refine
        cleanup
        ~changed
        ~file_targets:files
        ~directory_targets:directories
        ~subdirs_to_keep:subdirs
        ~targets_to_keep:pending);
    cleanup.refined <- refined
  ;;

  module Prepared_directory = struct
    type state = Cleanup_state.t

    type t =
      | Known of Loaded.t
      | Under_directory_target of { directory_target_ancestor : Path.Build.t }
      | Normal of
          { build_dir : Dir_triage.Build_directory.t
          ; state : state
          ; info : Initial_directory.info
          }

    let equal a b =
      match a, b with
      | Known a, Known b -> a == b
      | Normal a, Normal b ->
        a.state == b.state
        && a.info == b.info
        && a.build_dir.context_type == b.build_dir.context_type
      | ( Under_directory_target { directory_target_ancestor = a }
        , Under_directory_target { directory_target_ancestor = b } ) ->
        Path.Build.equal a b
      | _ -> false
    ;;

    let prepare =
      Memo.exec
        (Memo.create
           "prepare-rule-cleanup"
           ~input:(module Path.Build)
           ~cutoff:equal
           (fun dir ->
              get_dir_triage ~dir:(Path.build dir)
              >>= function
              | Known loaded -> Memo.return (Known loaded)
              | Build_directory build_dir ->
                initial_cleanup build_dir
                >>= (function
                 | Initial_directory.Under_directory_target { directory_target_ancestor }
                   -> Memo.return (Under_directory_target { directory_target_ancestor })
                 | Initial_directory.Normal initial ->
                   let+ state = Cleanup_state.get build_dir in
                   Normal { build_dir; state; info = initial.info })))
    ;;
  end

  module Compiled_target = struct
    type t =
      | Loaded of Loaded.t
      | Selected of
          { target : Path.Build.t
          ; prepared : Prepared_directory.state
          ; compiled : Compiled_selection.t
          ; cleanup_receipt : Cleanup.receipt
          }

    let equal a b =
      match a, b with
      | Selected { target; compiled = a; _ }, Selected { compiled = b; _ } ->
        (* Only the requested rule and its kind escape this private lookup.
           Cleanup retains fresh metadata even when this cutoff holds.
           [Rule.set_action] preserves IDs, so compare the rules themselves. *)
        let equal_rule a b =
          Option.equal
            ( == )
            (Path.Build.Map.find a target)
            (Path.Build.Map.find b target)
        in
        equal_rule a.loaded.rules_here.by_file_targets b.loaded.rules_here.by_file_targets
        && equal_rule
             a.loaded.rules_here.by_directory_targets
             b.loaded.rules_here.by_directory_targets
      | Loaded a, Loaded b ->
        a == b
        ||
          (match a, b with
          | ( Build_under_directory_target { directory_target_ancestor = a }
            , Build_under_directory_target { directory_target_ancestor = b } ) ->
            Path.Build.equal a b
          | _ -> false)
      | Loaded _, Selected _ | Selected _, Loaded _ -> false
    ;;

    let replay target = function
      | Loaded _ -> ()
      | Selected { prepared; compiled; cleanup_receipt; _ } ->
        (match prepared.current with
         | None ->
           Code_error.raise
             "Target cleanup replayed before directory preparation"
             [ "target", Path.Build.to_dyn target ]
         | Some { Initial_directory.cleanup; _ } ->
           (* A receipt belongs to this full payload, not just its selected
              rule. Skipped live entries remain safe: later views either
              reveal their owner or retain a pending ancestor declaration. *)
           if not (Cleanup.reuse cleanup cleanup_receipt)
           then (
             refine_cleanup cleanup compiled;
             Cleanup.remember cleanup cleanup_receipt))
    ;;

    let loaded = function
      | Loaded loaded -> loaded
      | Selected { compiled; _ } -> Loaded.Build compiled.loaded
    ;;
  end

  let load_target_for_watch =
    let memo =
      Memo.create_with_replay
        "compile-target-rules"
        ~input:(module Path.Build)
        ~cutoff:Compiled_target.equal
        ~replay:Compiled_target.replay
        (fun target ->
           let dir = Path.Build.parent_exn target in
           Dune_trace.emit Debug (fun () -> Dune_trace.Event.load_dir (Path.build dir));
           (* Restore status and validate cleanup before the old producer graph.
              The cutoff compares immutable metadata separately from the
              stable cell carrying this run's mutable cleanup state. *)
           let* prepared = Prepared_directory.prepare dir in
           match prepared with
           | Known loaded -> Memo.return (Compiled_target.Loaded loaded)
           | Under_directory_target { directory_target_ancestor } ->
             Memo.return
               (Compiled_target.Loaded
                  (Loaded.Build_under_directory_target { directory_target_ancestor }))
           | Normal { build_dir; state = prepared; info } ->
             let* selection = Rules.load_path_with_pending info.rules target in
             let+ compiled =
               compile_selection build_dir info ~request:(Target target) selection
             in
             Compiled_target.Selected
               { target; prepared; compiled; cleanup_receipt = Cleanup.receipt () })
    in
    fun target -> Memo.exec memo target >>| Compiled_target.loaded
  ;;

  let finish_load
        { Dir_triage.Build_directory.dir; context_type; _ }
        request
        { Initial_directory.cleanup; completed_targets; _ }
        ({ Compiled_selection.loaded; collected; _ } as compiled)
    =
    refine_cleanup cleanup compiled;
    let+ loaded =
      match context_type, request with
      | With_sources, (Complete | Alias _) ->
        let+ aliases = compute_alias_expansions ~collected ~dir in
        { loaded with aliases }
      | Empty, _ | With_sources, (Target _ | Directory_target _ | Files _) ->
        Memo.return loaded
    in
    match Memo.is_incremental (), request with
    | false, (Complete | Files _) -> Completed_targets.remember completed_targets loaded
    | false, Target target ->
      let loaded = Completed_targets.remember completed_targets loaded in
      Completed_targets.remember_target completed_targets target loaded;
      loaded
    | true, _ | false, (Directory_target _ | Alias _) -> Loaded.Build loaded
  ;;

  let load_build_directory_exn
        ({ Dir_triage.Build_directory.dir; _ } as build_dir)
        request
    =
    initial_cleanup build_dir
    >>= function
    | Initial_directory.Under_directory_target { directory_target_ancestor } ->
      let loaded = Loaded.Build_under_directory_target { directory_target_ancestor } in
      (match request with
       | Target target when not (Memo.is_incremental ()) ->
         let+ completed_targets = Completed_targets.get in
         Completed_targets.remember_target completed_targets target loaded;
         loaded
       | _ -> Memo.return loaded)
    | Initial_directory.Normal ({ info; completed_targets; _ } as initial) ->
      let* selection =
        match request with
        | Target target -> Rules.load_path_with_pending info.rules target
        | Directory_target target -> Rules.load_directory_with_pending info.rules target
        | Complete | Files _ | Alias _ ->
          Rules.load_with_pending info.rules (request_mask ~dir request)
      in
      let finish () =
        let* compiled = compile_selection build_dir info ~request selection in
        finish_load build_dir request initial compiled
      in
      (match request with
       | Target target ->
         (* Another lookup may have completed while this one waited for a
            producer. The entry point already read this run's epoch. *)
         (match Path.Build.Table.find completed_targets.by_target target with
          | Some loaded -> Memo.return loaded
          | None -> finish ())
       | Complete | Directory_target _ | Files _ | Alias _ -> finish ())
  ;;

  let load_request ~dir request : Loaded.t Memo.t =
    Dune_trace.emit Debug (fun () -> Dune_trace.Event.load_dir dir);
    get_dir_triage ~dir
    >>= function
    | Known l -> Memo.return l
    | Build_directory x -> load_build_directory_exn x request
  ;;

  let load_dir =
    let load_dir_impl dir = load_request ~dir Complete in
    let memo =
      Memo.create_with_store
        "load-dir"
        ~store:(module Path.Table)
        ~input:(module Path)
        load_dir_impl
    in
    fun ~dir -> Memo.exec memo dir
  ;;

  let load_dir_for_target_impl target =
    if Memo.is_incremental ()
    then load_target_for_watch target
    else
      (* Ordinary builds can reuse a validated output closure. Watch lookups
         must retain each target's own dependencies across runs. *)
      let* completed_targets = Completed_targets.get in
      match Path.Build.Table.find completed_targets.by_target target with
      | Some loaded -> Memo.return loaded
      | None ->
        let dir = Path.Build.parent_exn target in
        Dune_trace.emit Debug (fun () -> Dune_trace.Event.load_dir (Path.build dir));
        let ready =
          if Memo.is_incremental ()
          then None
          else Path.Build.Table.find completed_targets.build_directories dir
        in
        (match ready with
         | Some build_dir -> load_build_directory_exn build_dir (Target target)
         | None ->
           get_dir_triage ~dir:(Path.build dir)
           >>= (function
            | Known loaded -> Memo.return loaded
            | Build_directory build_dir ->
              if not (Memo.is_incremental ())
              then Path.Build.Table.set completed_targets.build_directories dir build_dir;
              load_build_directory_exn build_dir (Target target)))
  ;;

  let load_dir_for_target =
    let memo =
      Memo.create "load-target-rules" ~input:(module Path.Build) load_dir_for_target_impl
    in
    fun target ->
      if Memo.is_incremental ()
      then load_target_for_watch target
      else
        let* completed_targets = Completed_targets.get in
        match Path.Build.Table.find completed_targets.by_target target with
        | Some loaded -> Memo.return loaded
        | None -> Memo.exec memo target
  ;;

  let load_dir_for_directory_target =
    Memo.exec
      (Memo.create
         "load-directory-target-rules"
         ~input:(module Path.Build)
         (fun target ->
            load_request
              ~dir:(Path.build (Path.Build.parent_exn target))
              (Directory_target target)))
  ;;

  module Compiled_file_selector = struct
    type t =
      | Loaded of Loaded.t
      | Selected of
          { prepared : Prepared_directory.state
          ; compiled : Compiled_selection.t
          ; cleanup_receipt : Cleanup.receipt
          }

    let replay selector = function
      | Loaded _ -> ()
      | Selected { prepared; compiled; cleanup_receipt } ->
        (match prepared.current with
         | None ->
           Code_error.raise
             "File selector cleanup replayed before directory preparation"
             [ "selector", File_selector.to_dyn selector ]
         | Some { Initial_directory.cleanup; _ } ->
           if not (Cleanup.reuse cleanup cleanup_receipt)
           then (
             refine_cleanup cleanup compiled;
             Cleanup.remember cleanup cleanup_receipt))
    ;;

    let loaded = function
      | Loaded loaded -> loaded
      | Selected { compiled; _ } -> Loaded.Build compiled.loaded
    ;;
  end

  let load_file_selector =
    let memo =
      Memo.create_with_replay
        "load-file-selector-rules"
        ~input:(module File_selector)
        ~cutoff:(fun _ _ -> false)
        ~replay:Compiled_file_selector.replay
        (fun selector ->
           let dir = File_selector.dir selector in
           match Memo.is_incremental (), Path.as_in_build_dir dir with
           | true, Some build_dir ->
             Dune_trace.emit Debug (fun () -> Dune_trace.Event.load_dir dir);
             let* prepared = Prepared_directory.prepare build_dir in
             (match prepared with
              | Known loaded -> Memo.return (Compiled_file_selector.Loaded loaded)
              | Under_directory_target { directory_target_ancestor } ->
                Memo.return
                  (Compiled_file_selector.Loaded
                     (Loaded.Build_under_directory_target { directory_target_ancestor }))
              | Normal { build_dir; state = prepared; info } ->
                let request = Files selector in
                let* selection =
                  Rules.load_with_pending
                    info.rules
                    (request_mask ~dir:build_dir.dir request)
                in
                let+ compiled = compile_selection build_dir info ~request selection in
                Compiled_file_selector.Selected
                  { prepared; compiled; cleanup_receipt = Cleanup.receipt () })
           | false, _ | true, None ->
             let+ loaded = load_request ~dir (Files selector) in
             Compiled_file_selector.Loaded loaded)
    in
    fun selector -> Memo.exec memo selector >>| Compiled_file_selector.loaded
  ;;

  let load_alias =
    Memo.exec
      (Memo.create
         "load-alias-rules"
         ~input:(module Alias)
         (fun alias -> load_request ~dir:(Path.build (Alias.dir alias)) (Alias alias)))
  ;;

  let lookup_alias alias =
    load_alias alias
    >>| function
    | Source _ | External _ ->
      Code_error.raise "Alias in a non-build dir" [ "alias", Alias.to_dyn alias ]
    | Build { aliases; _ } -> Alias.Name.Map.find aliases (Alias.name alias)
    | Build_under_directory_target _ -> None
  ;;

  let is_under_directory_target p =
    match Path.parent p with
    | None -> Memo.return false
    | Some dir ->
      get_dir_triage ~dir
      >>= (function
       | Known _ -> Memo.return false
       | Build_directory d ->
         Gen_rules.gen_rules d
         >>| (function
          | Under_directory_target _ -> true
          | Normal { directory_targets; _ } ->
            Path.Build.Map.mem directory_targets (Path.as_in_build_dir_exn p)))
  ;;
end

include Load_rules

let get_directory_rule path =
  let* declared = is_under_directory_target (Path.build path) in
  if not declared
  then Memo.return None
  else
    load_dir_for_directory_target path
    >>= function
    | Source _ | External _ ->
      Code_error.raise "Directory target outside the build tree" []
    | Build { rules_here; _ } ->
      Memo.return (Path.Build.Map.find rules_here.by_directory_targets path)
    | Build_under_directory_target { directory_target_ancestor } ->
      let+ loaded = load_dir_for_directory_target directory_target_ancestor in
      (match loaded with
       | Build { rules_here; _ } ->
         Path.Build.Map.find rules_here.by_directory_targets directory_target_ancestor
       | Source _ | External _ | Build_under_directory_target _ ->
         Code_error.raise "No owning directory target" [])
;;

let rule_of_loaded_dir path (loaded : Loaded.t) =
  match loaded with
  | External _ | Source _ -> assert false
  | Build { rules_here; _ } ->
    Memo.return
      (match Path.Build.Map.find rules_here.by_file_targets path with
       | Some _ as rule -> rule
       | None -> Path.Build.Map.find rules_here.by_directory_targets path)
  | Build_under_directory_target { directory_target_ancestor } ->
    load_dir_for_target directory_target_ancestor
    >>= (function
     | External _ | Source _ | Build_under_directory_target _ -> assert false
     | Build { rules_here; _ } ->
       Memo.return
         (Path.Build.Map.find rules_here.by_directory_targets directory_target_ancestor))
;;

let get_rule path =
  match Path.as_in_build_dir path with
  | None -> Memo.return None
  | Some path -> load_dir_for_target path >>= rule_of_loaded_dir path
;;

type rule_or_source =
  | Source of Digest.t
  | Rule of Path.Build.t * Rule.t

let get_build_rule path = load_dir_for_target_impl path >>= rule_of_loaded_dir path

let get_rule_or_source path =
  match Path.destruct_build_dir path with
  | `Outside path ->
    let+ digest = Fs_memo.file_digest_exn ~loc:Current_rule_loc.get path in
    Source digest
  | `Inside path ->
    get_build_rule path
    >>= (function
     | Some rule -> Memo.return (Rule (path, rule))
     | None ->
       let* loc = Current_rule_loc.get () in
       no_rule_found ~loc path)
;;

let get_alias_definition alias =
  lookup_alias alias
  >>= function
  | None ->
    let open Pp.O in
    let+ loc = Current_rule_loc.get () in
    User_error.raise ?loc [ Pp.text "No rule found for " ++ Alias.describe alias ]
  | Some x -> Memo.return x
;;

type target_type =
  | File
  | Directory

type is_target =
  | No
  | Yes of target_type
  | Under_directory_target_so_cannot_say

let is_target file =
  match Path.parent file with
  | None -> Memo.return No
  | Some dir ->
    (match Path.as_in_build_dir dir, Path.as_in_build_dir file with
     | Some _, Some file -> load_dir_for_target file
     | _ -> load_dir ~dir)
    >>| (function
     | External _ | Source _ -> No
     | Build { rules_here; _ } ->
       let file = Path.as_in_build_dir_exn file in
       (match Path.Build.Map.find rules_here.by_file_targets file with
        | Some _ -> Yes File
        | None ->
          (match Path.Build.Map.find rules_here.by_directory_targets file with
           | Some _ -> Yes Directory
           | None -> No))
     | Build_under_directory_target _ -> Under_directory_target_so_cannot_say)
;;
