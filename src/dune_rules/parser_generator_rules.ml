open Import

let possible_basenames ~dir ~source_files ~source_extension ~modules =
  let physical =
    List.concat_map source_files ~f:(fun (source_dir, files) ->
      if not (Path.Build.equal dir source_dir)
      then []
      else
        Filename.Array.Set.to_list files
        |> List.filter_map ~f:(fun filename ->
          Option.some_if
            (Filename.Extension.Or_empty.check
               (Filename.extension filename)
               source_extension)
            (Filename.remove_extension filename |> Filename.to_string)))
  in
  Ordered_set_lang.Unexpanded.fold_strings modules ~init:physical ~f:(fun _pos sw acc ->
    match String_with_vars.text_only sw with
    | None -> acc
    | Some name -> name :: acc)
;;

let source_files ~dir ~source_files ~for_ =
  let { Parser_generators.modules; _ }, source_extension, target_extensions =
    match for_ with
    | Parser_generators.Ocamllex s -> s, Filename.Extension.mll, [ Filename.Extension.ml ]
    | Ocamlyacc s ->
      s, Filename.Extension.mly, [ Filename.Extension.ml; Filename.Extension.mli ]
  in
  possible_basenames ~dir ~source_files ~source_extension ~modules
  |> List.concat_map ~f:(fun name ->
    let path = Path.Build.relative dir name in
    List.map target_extensions ~f:(fun ext -> Path.Build.set_extension path ~ext))
;;

let rule_targets ~dir ~source_files:files ~for_ =
  let modules, extensions =
    match for_ with
    | Parser_generators.Ocamllex s -> s.modules, [ Filename.Extension.ml ]
    | Ocamlyacc s -> s.modules, [ Filename.Extension.ml; Filename.Extension.mli ]
  in
  let known = Target_mask.files (source_files ~dir ~source_files:files ~for_) in
  if Ordered_set_lang.Unexpanded.is_expanded modules
  then known
  else
    Target_mask.union
      known
      (Target_mask.file_extensions ~dir (Filename.Extension.Set.of_list extensions))
;;

let tool =
  let tool_bin sctx ~loc ~dir ~for_ =
    Super_context.resolve_program
      sctx
      ~loc:(Some loc)
      ~dir
      ~where:Original_path
      (Parser_generators.tool for_)
  in
  fun sctx ~loc ~dir args ~for_ ->
    let tool_bin = tool_bin sctx ~loc ~dir ~for_ in
    let build_dir = Super_context.context sctx |> Context.build_dir |> Path.build in
    Command.run_dyn_prog
      ~sandbox:Sandbox_config.needs_sandboxing
      ~dir:build_dir
      tool_bin
      args
;;

let add_rule sctx ~dir ~mode ~flags ~expander (loc, m) ~for_ =
  let args =
    let files = Module.Source.files m in
    let file = List.hd files in
    let src = Module.File.original_path file in
    match for_ with
    | Parser_generators.Ocamllex _ ->
      let dst = Module.File.path file |> Path.as_in_build_dir_exn in
      [ Command.Args.dyn flags; As [ "-q"; "-o" ]; Target dst; Dep src ]
    | Ocamlyacc _ ->
      let targets =
        List.map files ~f:(fun file -> Module.File.path file |> Path.as_in_build_dir_exn)
      in
      [ Command.Args.dyn flags; Command.Args.Dep src; Hidden_targets targets ]
  in
  let action = tool sctx ~loc ~dir args ~for_ in
  let open Memo.O in
  let* mode = Rule_mode_expand.expand_path ~expander ~dir mode in
  Super_context.add_rule sctx ~dir ~mode ~loc action
;;

let gen_rules sctx ~dir_contents ~dir ~for_ =
  let open Memo.O in
  let { Parser_generators.mode; flags; _ }, modules_for =
    match for_ with
    | Parser_generators.Ocamllex s -> s, Ml_sources.Parser_generators.Ocamllex s.loc
    | Ocamlyacc s -> s, Ocamlyacc s.loc
  in
  (* NOTE(anmonteiro): Parser generator rules run in the "OCaml module space":
    `Ml_sources` generates `foo.mll` -> `foo.ml`. Melange  *)
  let* { deps = _; targets } =
    Dir_contents.ml dir_contents ~for_:Ocaml
    >>| Ml_sources.Parser_generators.modules ~for_:modules_for
  in
  let* expander = Super_context.expander sctx ~dir in
  let flags =
    let standard = Action_builder.return [] in
    Expander.expand_and_eval_set expander flags ~standard
  in
  Module_trie.to_list targets
  |> Memo.parallel_iter ~f:(fun ((_, source) as module_) ->
    let targets =
      Module.Source.files source
      |> List.map ~f:(fun file -> Module.File.path file |> Path.as_in_build_dir_exn)
      |> Target_mask.files
    in
    Rules.narrow targets (fun () ->
      add_rule sctx ~dir ~mode ~flags ~expander module_ ~for_))
;;
