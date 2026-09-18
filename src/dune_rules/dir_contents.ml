open Import

(* we need to convince ocamldep that we don't depend on the menhir rules *)
module Menhir = struct end
open Memo.O

let loc_of_dune_file st_dir =
  (match
     let open Option.O in
     let* dune_file = Source_tree.Dir.dune_file st_dir in
     (* TODO not really correct. we need to know the [(subdir ..)] that introduced this *)
     Source.Dune_file.path dune_file
   with
   | Some s -> s
   | None -> Path.Source.relative (Source_tree.Dir.path st_dir) "_unknown_")
  |> Path.source
  |> Loc.in_file
;;

module Source_files = struct
  type generated =
    { targets : Target_mask.t
    ; files : Filename.t list Memo.Lazy.t
    }

  type t =
    { dir : Path.Build.t
    ; known : Filename.Array.Set.t
    ; generated : generated list
    }

  let empty ~dir = { dir; known = Filename.Array.Set.empty; generated = [] }

  let get { dir; known; generated } ~mask =
    let+ generated =
      List.filter_map generated ~f:(fun { targets; files } ->
        Option.some_if (Target_mask.intersects targets mask) files)
      |> Memo.parallel_map ~f:Memo.Lazy.force
    in
    Filename.Array.Set.union known (Filename.Array.Set.of_list (List.concat generated))
    |> Filename.Array.Set.filter ~f:(fun file ->
      Target_mask.mem_file mask (Path.Build.relative_fname dir file))
  ;;
end

type t =
  { kind : kind
  ; dir : Path.Build.t
  ; dir_renames : (Filename.t list * Filename.t list) list
  ; text_files : Source_files.t
  ; foreign_sources : Foreign_sources.t Memo.Lazy.t
  ; mlds : (Documentation.t * Doc_sources.mld list) list Memo.Lazy.t
  ; rocq : Rocq_sources.t Memo.Lazy.t
  ; ocaml : Ml_sources.t Memo.Lazy.t
  ; melange : Ml_sources.t Memo.Lazy.t
  ; source_dir : Source_tree.Dir.t option
  }

and kind =
  | Standalone
  | Group_root of t list
  | Group_part

let empty kind ~dir ~source_dir =
  { kind
  ; dir
  ; dir_renames = []
  ; source_dir
  ; text_files = Source_files.empty ~dir
  ; ocaml = Memo.Lazy.of_val Ml_sources.empty
  ; melange = Memo.Lazy.of_val Ml_sources.empty
  ; mlds = Memo.Lazy.of_val []
  ; foreign_sources = Memo.Lazy.of_val Foreign_sources.empty
  ; rocq = Memo.Lazy.of_val Rocq_sources.empty
  }
;;

module Standalone_or_root = struct
  type nonrec standalone_or_root =
    { root : t
    ; subdirs : t Path.Build.Map.t
    }

  type physical =
    { dirs : (Source_file_dir.t * Source_files.t) Nonempty_list.t
    ; rules : Rules.t
    }

  type nonrec t =
    { physical : physical Memo.Lazy.t
    ; source_dirs : Source_file_dir.t Nonempty_list.t Memo.Lazy.t
    ; rules : Rules.t Memo.Lazy.t
    ; contents : standalone_or_root Memo.Lazy.t
    }

  let empty ~dir ~source_dir =
    let dirs =
      Nonempty_list.
        [ { Source_file_dir.dir
          ; path_to_root = []
          ; files =
              (match source_dir with
               | None -> Filename.Array.Set.empty
               | Some source_dir -> Source_tree.Dir.filenames source_dir)
          ; source_dir
          ; stanzas = []
          }
        ]
    in
    let physical_dirs =
      Nonempty_list.map dirs ~f:(fun source ->
        source, { Source_files.dir; known = source.files; generated = [] })
    in
    { source_dirs = Memo.Lazy.of_val dirs
    ; rules = Memo.Lazy.of_val Rules.empty
    ; physical = Memo.Lazy.of_val { dirs = physical_dirs; rules = Rules.empty }
    ; contents =
        Memo.Lazy.create ~name:"empty-dir-contents" (fun () ->
          Memo.return
            { root = empty Standalone ~dir ~source_dir; subdirs = Path.Build.Map.empty })
    }
  ;;

  let root t =
    let+ contents = Memo.Lazy.force t.contents in
    contents.root
  ;;

  let subdirs t =
    let+ contents = Memo.Lazy.force t.contents in
    Path.Build.Map.values contents.subdirs
  ;;

  let rules t = Memo.Lazy.force t.rules
  let source_directories t = Memo.Lazy.force t.source_dirs

  let source_files t =
    let* physical = Memo.Lazy.force t.physical in
    Memo.parallel_map
      (Nonempty_list.to_list physical.dirs)
      ~f:(fun ({ Source_file_dir.dir; _ }, files) ->
        let+ files = Source_files.get files ~mask:(Target_mask.files_in_directory dir) in
        dir, files)
  ;;
end

type triage =
  | Standalone_or_root of Standalone_or_root.t
  | Group_part of Path.Build.t

let dir t = t.dir
let dir_renames t = t.dir_renames
let source_dir t = t.source_dir
let rocq t = Memo.Lazy.force t.rocq

let ml =
  fun t ~for_ ->
  match for_ with
  | Compilation_mode.Ocaml -> Memo.Lazy.force t.ocaml
  | Melange -> Memo.Lazy.force t.melange
;;

let dirs t =
  match t.kind with
  | Standalone -> [ t ]
  | Group_root subs -> t :: subs
  | Group_part ->
    Code_error.raise
      "Dir_contents.dirs called on a group part"
      [ "dir", Path.Build.to_dyn t.dir ]
;;

let text_files t ~mask = Source_files.get t.text_files ~mask
let foreign_sources t = Memo.Lazy.force t.foreign_sources

let mlds t ~(stanza : Documentation.t) =
  let+ map = Memo.Lazy.force t.mlds in
  match
    List.find_map map ~f:(fun (stanza', x) ->
      Option.some_if (Loc.equal stanza.loc stanza'.loc) x)
  with
  | Some x -> x
  | None ->
    Code_error.raise
      "Dir_contents.mlds"
      [ "doc", Loc.to_dyn_hum stanza.loc
      ; ( "available"
        , Dyn.(list Loc.to_dyn_hum)
            (List.map map ~f:(fun ((d : Documentation.t), _) -> d.loc)) )
      ]
;;

module rec Load : sig
  val get : Super_context.t -> dir:Path.Build.t -> t Memo.t
  val triage : Super_context.t -> dir:Path.Build.t -> triage Memo.t
end = struct
  let select_deps_files ~dialects libraries =
    (* Keep non-OCaml `(select ..)` outputs in the directory contents so
       foreign stubs can depend on generated headers and similar files. *)
    List.filter_map libraries ~f:(fun dep ->
      match (dep : Lib_dep.t) with
      | Re_export _ | Direct _ | Instantiate _ -> None
      | Select s ->
        let ext = Path.Local.basename s.result_fn |> Filename.extension in
        (match Filename.Extension.Or_empty.extension ext with
         | None -> Some (Path.Local.to_string s.result_fn)
         | Some ext ->
           (match Dialect.DB.find_by_extension dialects ext with
            | Some _ -> None
            | None -> Some (Path.Local.to_string s.result_fn))))
  ;;

  let prepare_text_files sctx st_dir stanzas ~dir ~src_dir =
    Rules.collect (fun () ->
      let from_source = Source_tree.Dir.filenames st_dir in
      let* known =
        Memo.List.concat_map stanzas ~f:(fun stanza ->
          match Stanza.repr stanza with
          | Rocq_stanza.Rocqpp.T { modules; _ } ->
            Rocq_sources.mlg_files ~sctx ~dir ~modules
            >>| List.rev_map ~f:(fun mlg_file ->
              Path.Build.set_extension mlg_file ~ext:Filename.Extension.ml
              |> Path.Build.basename)
          | Rocq_stanza.Extraction.T s ->
            Memo.return
              (Rocq_stanza.Extraction.target_fnames s
               |> List.map ~f:Filename.of_string_exn)
          | Library.T { buildable; _ }
          | Executables.T { buildable; _ }
          | Tests.T { exes = { buildable; _ }; _ } ->
            let dialects = Source_tree.Dir.project st_dir |> Dune_project.dialects in
            let select_deps_files = select_deps_files ~dialects buildable.libraries in
            let ctypes_files =
              Option.map buildable.ctypes ~f:Ctypes_field.generated_ml_and_c_files
              |> Option.value ~default:[]
            in
            Memo.return
              (List.map (select_deps_files @ ctypes_files) ~f:Filename.of_string_exn)
          | _ -> Memo.return [])
      in
      let+ generated =
        match stanzas with
        | [] -> Memo.return []
        | _ :: _ ->
          let* expander = Super_context.expander sctx ~dir in
          Memo.parallel_map stanzas ~f:(fun stanza ->
            let producer =
              match Stanza.repr stanza with
              | Rule_conf.T rule -> Some (Simple_rules.rule_targets ~dir rule, `Rule rule)
              | Copy_files.T def ->
                Some (Simple_rules.copy_files_targets ~dir def, `Copy_files def)
              | Generate_sites_module_stanza.T def ->
                Some
                  ( Target_mask.files
                      [ Path.Build.relative dir (Module_name.to_string def.module_ ^ ".ml")
                      ]
                  , `Generate_sites_module def )
              | _ -> None
            in
            match producer with
            | None -> Memo.return None
            | Some (targets, producer) ->
              let+ files =
                Rules.defer targets (fun () ->
                  let* () = Memo.Lazy.force Configurator_rules.force_files in
                  match producer with
                  | `Rule rule ->
                    Simple_rules.user_rule sctx rule ~dir ~expander
                    >>| (function
                     | None -> []
                     | Some targets ->
                       (* CR-someday amokhov: Do not ignore directory targets. *)
                       Filename.Set.to_list targets.files)
                  | `Copy_files def ->
                    Simple_rules.copy_files sctx def ~src_dir ~dir ~expander
                    >>| Path.Set.to_list_map ~f:Path.basename
                  | `Generate_sites_module def ->
                    Generate_sites_module_rules.setup_rules sctx ~dir def
                    >>| fun fn -> [ Filename.of_string_exn fn ])
              in
              Some { Source_files.targets; files })
          >>| List.filter_opt
      in
      { Source_files.dir
      ; known = Filename.Array.Set.union from_source (Filename.Array.Set.of_list known)
      ; generated
      })
  ;;

  let prepare_sources sctx source_dirs =
    let prepared =
      Memo.lazy_ ~name:"source-rule-stages" (fun () ->
        let* source_dirs = Memo.Lazy.force source_dirs in
        Memo.parallel_map (Nonempty_list.to_list source_dirs) ~f:(fun source ->
          let { Source_file_dir.dir; source_dir; stanzas; _ } = source in
          let source_dir = Option.value_exn source_dir in
          let+ files, rules =
            prepare_text_files
              sctx
              source_dir
              stanzas
              ~dir
              ~src_dir:(Source_tree.Dir.path source_dir)
          in
          source, files, rules))
    in
    let rules =
      Memo.lazy_ ~name:"source-rules" (fun () ->
        let+ prepared = Memo.Lazy.force prepared in
        List.fold_left prepared ~init:Rules.empty ~f:(fun rules (_, _, more) ->
          Rules.union rules more))
    in
    let physical =
      Memo.lazy_ ~name:"physical-contents" (fun () ->
        let+ prepared = Memo.Lazy.force prepared
        and+ rules = Memo.Lazy.force rules in
        let dirs =
          List.map prepared ~f:(fun (source, files, _) -> source, files)
          |> Nonempty_list.of_list_exn
        in
        { Standalone_or_root.dirs; rules })
    in
    physical, rules
  ;;

  let source_dirs_with_files dirs ~extensions =
    let+ dirs =
      Memo.parallel_map
        (Nonempty_list.to_list dirs)
        ~f:(fun (({ Source_file_dir.dir; _ } as source), files) ->
          let+ files =
            Source_files.get files ~mask:(Target_mask.file_extensions ~dir extensions)
          in
          { source with Source_file_dir.files })
    in
    Nonempty_list.of_list_exn dirs
  ;;

  let ml_source_extensions project =
    let extensions =
      Dialect.DB.fold (Dune_project.dialects project) ~init:[] ~f:(fun dialect acc ->
        List.filter_map Ml_kind.all ~f:(Dialect.extension dialect) @ acc)
    in
    Filename.Extension.Set.of_list extensions
  ;;

  let foreign_source_extensions ~dune_version =
    Foreign_language.source_extensions
    |> String.Map.to_list
    |> List.filter_map ~f:(fun (extension, (_, since)) ->
      Option.some_if
        (dune_version >= since)
        (Filename.Extension.of_string_exn ("." ^ extension)))
    |> Filename.Extension.Set.of_list
  ;;

  module Key = struct
    module Super_context = Super_context.As_memo_key

    type t = Super_context.t * Path.Build.t

    let to_dyn (sctx, path) =
      Dyn.Tuple [ Super_context.to_dyn sctx; Path.Build.to_dyn path ]
    ;;

    let equal = Tuple.T2.equal Super_context.equal Path.Build.equal
    let hash = Tuple.T2.hash Super_context.hash Path.Build.hash
  end

  let lookup_vlib sctx ~current_dir ~loc ~dir ~for_ =
    match Path.Build.equal current_dir dir with
    | true ->
      User_error.raise
        ~loc
        [ Pp.text
            "Virtual library and its implementation(s) cannot be defined in the same \
             directory"
        ]
    | false -> Load.get sctx ~dir >>= ml ~for_
  ;;

  let human_readable_description dir =
    Pp.textf
      "Computing directory contents of %s"
      (Path.to_string_maybe_quoted (Path.build dir))
  ;;

  let mlds ~sctx ~dir ~dune_file ~files =
    Memo.lazy_ ~name:"documentation-sources" (fun () ->
      let* expander = Super_context.expander sctx ~dir
      and* files =
        Source_files.get
          files
          ~mask:
            (Target_mask.file_extensions
               ~dir
               (Filename.Extension.Set.singleton
                  (Filename.Extension.of_string_exn ".mld")))
      in
      Doc_sources.build_mlds_map dune_file ~dir ~files expander)
  ;;

  let ml_sources sctx ~dir ~project ~lib_config ~loc ~include_subdirs ~dirs ~for_ =
    Memo.lazy_ ~name:"ml-sources" (fun () ->
      let lookup_vlib = lookup_vlib sctx ~current_dir:dir ~for_ in
      let libs = Scope.DB.find_by_dir dir >>| Scope.libs in
      let* expander = Super_context.expander sctx ~dir
      and* dirs =
        source_dirs_with_files dirs ~extensions:(ml_source_extensions project)
      in
      Ml_sources.make
        ~expander
        ~libs
        ~for_
        ~project
        ~lib_config
        ~loc
        ~include_subdirs
        ~lookup_vlib
        dirs)
  ;;

  let language_sources sctx ~dir ~project ~lib_config ~loc ~include_subdirs ~dirs =
    ( ml_sources sctx ~dir ~project ~lib_config ~loc ~include_subdirs ~dirs ~for_:Ocaml
    , ml_sources sctx ~dir ~project ~lib_config ~loc ~include_subdirs ~dirs ~for_:Melange
    )
  ;;

  let make_standalone sctx st_dir ~dir (d : Dune_file.t) =
    let human_readable_description () = human_readable_description dir in
    let stanzas = Dune_file.stanzas d in
    let source_dirs =
      Memo.lazy_ ~name:"standalone-source-directory" (fun () ->
        let+ stanzas = stanzas in
        Nonempty_list.
          [ { Source_file_dir.dir
            ; path_to_root = []
            ; files = Source_tree.Dir.filenames st_dir
            ; source_dir = Some st_dir
            ; stanzas
            }
          ])
    in
    let physical, rules = prepare_sources sctx source_dirs in
    { Standalone_or_root.physical
    ; source_dirs
    ; rules
    ; contents =
        Memo.lazy_ ~name:"standalone-dir-contents" ~human_readable_description (fun () ->
          let include_subdirs = Loc.none, Include_subdirs.No in
          let ctx = Super_context.context sctx in
          let lib_config =
            let+ ocaml = Context.ocaml ctx in
            ocaml.lib_config
          in
          let project = Dune_file.project d in
          let+ { Standalone_or_root.dirs; rules = _ } = Memo.Lazy.force physical in
          let _, files = Nonempty_list.hd dirs in
          let loc = loc_of_dune_file st_dir in
          let ml, melange =
            language_sources sctx ~dir ~project ~lib_config ~loc ~include_subdirs ~dirs
          in
          let mlds = mlds ~sctx ~dir ~dune_file:d ~files in
          { Standalone_or_root.root =
              { kind = Standalone
              ; source_dir = Some st_dir
              ; dir
              ; dir_renames = []
              ; text_files = files
              ; ocaml = ml
              ; melange
              ; mlds
              ; foreign_sources =
                  Memo.lazy_ ~name:"standalone-foreign-sources" (fun () ->
                    let dune_version = Dune_project.dune_version project in
                    let* stanzas = stanzas
                    and* dirs =
                      source_dirs_with_files
                        dirs
                        ~extensions:(foreign_source_extensions ~dune_version)
                    in
                    Foreign_sources.make stanzas ~dir ~dune_version ~dirs)
              ; rocq =
                  Memo.lazy_ ~name:"standalone-rocq-sources" (fun () ->
                    let+ stanzas = stanzas
                    and+ dirs =
                      source_dirs_with_files
                        dirs
                        ~extensions:
                          (Filename.Extension.Set.of_list
                             [ Filename.Extension.v; Filename.Extension.expected ])
                    in
                    Rocq_sources.of_dir stanzas ~dir ~include_subdirs ~dirs)
              }
          ; subdirs = Path.Build.Map.empty
          })
    }
  ;;

  module Dir_renames : sig
    type t

    val empty : t

    val expand
      :  Super_context.t
      -> dir:Path.Build.t
      -> File_binding.Unexpanded.t list
      -> t Memo.t

    val translate : t -> Filename.t list -> Filename.t list
    val to_list : t -> (Filename.t list * Filename.t list) list
  end = struct
    type binding =
      { src : Filename.t list
      ; dst : Filename.t list
      }

    type t = binding list

    let empty : t = []

    let descendant_segments ~loc ~what path ~of_ =
      match Path.Local.descendant path ~of_ with
      | None ->
        User_error.raise
          ~loc
          [ Pp.textf
              "%s must be a descendant of the directory containing the (include_subdirs \
               ...) stanza."
              what
          ]
      | Some path ->
        (match Path.Local.explode path with
         | [] ->
           User_error.raise
             ~loc
             [ Pp.textf
                 "%s must not be the directory containing the (include_subdirs ...) \
                  stanza."
                 what
             ]
         | segments -> segments)
    ;;

    let expand_binding ~dir binding =
      match File_binding.Expanded.dst_with_loc binding with
      | None -> None
      | Some (dst_loc, dst) ->
        let root = Path.Build.local dir in
        let src =
          descendant_segments
            ~loc:(File_binding.Expanded.src_loc binding)
            ~what:"The source directory"
            (File_binding.Expanded.src binding |> Path.Build.local)
            ~of_:root
        in
        let dst =
          descendant_segments
            ~loc:dst_loc
            ~what:"The destination directory"
            (Path.Local.relative root dst)
            ~of_:root
        in
        if List.length src <> List.length dst
        then
          User_error.raise
            ~loc:dst_loc
            [ Pp.text
                "The source and destination directories must have the same number of \
                 path components."
            ]
        else Some (Path.Local.of_comps src, (dst_loc, { src; dst }))
    ;;

    let expand sctx ~dir dirs =
      let* expand =
        let+ expander = Super_context.expander sctx ~dir in
        Expander.expand_str expander
      in
      let+ bindings =
        Memo.parallel_map dirs ~f:(fun binding ->
          File_binding_expand.expand binding ~dir ~f:(fun sw ->
            Action_builder.evaluate_and_collect_facts (expand sw) >>| fst))
      in
      List.filter_map bindings ~f:(expand_binding ~dir)
      |> Path.Local.Map.of_list_reducei
           ~f:(fun src ((_, first) as previous) (loc, second) ->
             if List.equal Filename.equal first.dst second.dst
             then previous
             else
               User_error.raise
                 ~loc
                 [ Pp.textf
                     "The directory %s is mapped to both %s and %s."
                     (Path.Local.to_string_maybe_quoted src)
                     (Path.Local.of_comps first.dst |> Path.Local.to_string_maybe_quoted)
                     (Path.Local.of_comps second.dst |> Path.Local.to_string_maybe_quoted)
                 ])
      |> Path.Local.Map.values
      |> List.map ~f:snd
    ;;

    let rec drop_prefix path prefix =
      match path, prefix with
      | path, [] -> Some path
      | p :: path, prefix :: prefixes when Filename.equal p prefix ->
        drop_prefix path prefixes
      | [], _ :: _ | _ :: _, _ :: _ -> None
    ;;

    let translate (t : t) path =
      List.fold_left t ~init:None ~f:(fun best { src; dst } ->
        match drop_prefix path src with
        | None -> best
        | Some rest ->
          let src_len = List.length src in
          (match best with
           | None -> Some (src_len, dst, rest)
           | Some (best_len, _, _) when src_len > best_len -> Some (src_len, dst, rest)
           | Some _ -> best))
      |> function
      | None -> path
      | Some (_, dst, rest) -> dst @ rest
    ;;

    let to_list t = List.map t ~f:(fun { src; dst } -> src, dst)
  end

  let make_group_root
        sctx
        ~dir
        { Dir_status.Group_root.qualification; dune_file; source_dir; components }
    =
    let include_subdirs =
      let loc, qualif_mode = qualification in
      loc, Include_subdirs.Include qualif_mode
    in
    let loc = loc_of_dune_file source_dir in
    let stanzas = Dune_file.stanzas dune_file in
    let source_dirs =
      Memo.lazy_ ~name:"group-source-directories" (fun () ->
        let+ stanzas = stanzas
        and+ components = components in
        let root =
          { Source_file_dir.dir
          ; path_to_root = []
          ; files = Source_tree.Dir.filenames source_dir
          ; source_dir = Some source_dir
          ; stanzas
          }
        in
        let subdirs =
          List.map
            components
            ~f:
              (fun
                { Dir_status.Group_component.dir
                ; path_to_group_root
                ; source_dir
                ; stanzas
                }
              ->
              { Source_file_dir.dir
              ; path_to_root = path_to_group_root
              ; files = Source_tree.Dir.filenames source_dir
              ; source_dir = Some source_dir
              ; stanzas
              })
        in
        Nonempty_list.(root :: subdirs))
    in
    let physical, rules = prepare_sources sctx source_dirs in
    let contents =
      Memo.lazy_
        ~name:"group-dir-contents"
        ~human_readable_description:(fun () -> human_readable_description dir)
        (fun () ->
           let ctx = Super_context.context sctx in
           let project = Dune_file.project dune_file in
           let* { Standalone_or_root.dirs = (root, files) :: subdirs; rules = _ } =
             Memo.Lazy.force physical
           in
           let+ dir_renames =
             match snd qualification with
             | Unqualified | Qualified { dirs = [] } -> Memo.return Dir_renames.empty
             | Qualified { dirs } -> Dir_renames.expand sctx ~dir dirs
           in
           let subdirs =
             List.map subdirs ~f:(fun ((source : Source_file_dir.t), files) ->
               ( { source with
                   path_to_root = Dir_renames.translate dir_renames source.path_to_root
                 }
               , files ))
           in
           let dirs = Nonempty_list.((root, files) :: subdirs) in
           let lib_config =
             let+ ocaml = Context.ocaml ctx in
             ocaml.lib_config
           in
           let ml, melange =
             language_sources sctx ~dir ~project ~lib_config ~loc ~include_subdirs ~dirs
           in
           let foreign_sources =
             Memo.lazy_ ~name:"group-foreign-sources" (fun () ->
               let dune_version = Dune_project.dune_version project in
               let* stanzas = stanzas
               and* dirs =
                 source_dirs_with_files
                   dirs
                   ~extensions:(foreign_source_extensions ~dune_version)
               in
               Foreign_sources.make stanzas ~dir ~dune_version ~dirs)
           in
           let rocq =
             Memo.lazy_ ~name:"group-rocq-sources" (fun () ->
               let+ stanzas = stanzas
               and+ dirs =
                 source_dirs_with_files
                   dirs
                   ~extensions:
                     (Filename.Extension.Set.of_list
                        [ Filename.Extension.v; Filename.Extension.expected ])
               in
               Rocq_sources.of_dir stanzas ~dir ~dirs ~include_subdirs)
           in
           let mlds = mlds ~sctx ~dir ~dune_file ~files in
           let dir_renames = Dir_renames.to_list dir_renames in
           let subdirs =
             List.map
               subdirs
               ~f:
                 (fun
                   ( { Source_file_dir.dir
                     ; path_to_root = _
                     ; files = _
                     ; source_dir
                     ; stanzas = _
                     }
                   , files )
                 ->
                 { kind = Group_part
                 ; source_dir
                 ; dir
                 ; dir_renames
                 ; text_files = files
                 ; ocaml = ml
                 ; melange
                 ; foreign_sources
                 ; mlds
                 ; rocq
                 })
           in
           let root =
             { kind = Group_root subdirs
             ; source_dir = Some source_dir
             ; dir
             ; dir_renames
             ; text_files = files
             ; ocaml = ml
             ; melange
             ; foreign_sources
             ; mlds
             ; rocq
             }
           in
           { Standalone_or_root.root
           ; subdirs = Path.Build.Map.of_list_map_exn subdirs ~f:(fun x -> x.dir, x)
           })
    in
    { Standalone_or_root.physical; source_dirs; rules; contents }
  ;;

  let get0_impl (sctx, dir) : triage Memo.t =
    Dir_status.DB.get ~dir
    >>= function
    | Is_component_of_a_group_but_not_the_root { group_root; stanzas = _ } ->
      Memo.return @@ Group_part group_root
    | Generated ->
      Memo.return @@ Standalone_or_root (Standalone_or_root.empty ~dir ~source_dir:None)
    | Lock_dir source_dir | Source_only source_dir ->
      Memo.return
      @@ Standalone_or_root (Standalone_or_root.empty ~dir ~source_dir:(Some source_dir))
    | Standalone (st_dir, d) ->
      Memo.return @@ Standalone_or_root (make_standalone sctx st_dir ~dir d)
    | Group_root root ->
      Memo.return @@ Standalone_or_root (make_group_root sctx root ~dir)
  ;;

  let memo0 =
    Memo.create
      "dir-contents-get0"
      get0_impl
      ~input:(module Key)
      ~human_readable_description:(fun (_, dir) ->
        Pp.textf
          "Computing directory contents of %s"
          (Path.to_string_maybe_quoted (Path.build dir))
        |> Option.some)
  ;;

  let get sctx ~dir =
    Memo.exec memo0 (sctx, dir)
    >>= function
    | Standalone_or_root { contents; _ } ->
      let+ { root; subdirs = _ } = Memo.Lazy.force contents in
      root
    | Group_part group_root ->
      Memo.exec memo0 (sctx, group_root)
      >>= (function
       | Group_part _ -> assert false
       | Standalone_or_root { contents; _ } ->
         let+ { root; subdirs = _ } = Memo.Lazy.force contents in
         root)
  ;;

  let triage sctx ~dir = Memo.exec memo0 (sctx, dir)
end

include Load

let modules_of_local_lib sctx lib ~for_ =
  let info = Lib.Local.info lib in
  let dir = Lib_info.src_dir info in
  let* t = get sctx ~dir
  and* libs = Scope.DB.find_by_dir dir >>| Scope.libs in
  ml t ~for_
  >>= Ml_sources.modules
        ~libs
        ~for_:(Library (Lib_info.lib_id info |> Lib_id.to_local_exn))
;;

let modules_of_lib sctx lib ~for_ =
  match
    let info = Lib.info lib in
    Lib_info.modules info ~for_
  with
  | External modules -> Memo.return modules
  | Local ->
    let+ modules = modules_of_local_lib sctx (Lib.Local.of_lib_exn lib) ~for_ in
    Some (Modules.With_vlib.modules modules)
;;

let () =
  Fdecl.set Expander.lookup_artifacts (fun ~dir ~for_ ->
    let* t =
      Context.DB.by_dir dir >>| Context.name >>= Super_context.find_exn >>= Load.get ~dir
    in
    ml t ~for_ >>= Ml_sources.artifacts)
;;
