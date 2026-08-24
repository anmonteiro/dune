open Import
open Dune_rules
module Js_output = Melange_rules.Js_output

let output_to_dyn { Js_output.module_system; build_path; promoted_path } =
  Dyn.record
    [ "module_system", Dyn.string (Melange.Module_system.to_string module_system)
    ; "build_path", Dyn.string (Path.Build.to_string build_path)
    ; ( "promoted_path"
      , Dyn.option (fun path -> Dyn.string (Path.Source.to_string path)) promoted_path )
    ]
;;

let source_path file =
  let path = Path.of_filename_relative_to_initial_cwd file in
  let workspace_root = Path.to_absolute_filename Path.root |> Path.of_string in
  match Path.drop_prefix path ~prefix:workspace_root with
  | Some path -> Path.Source.of_local path
  | None ->
    User_error.raise
      [ Pp.textf
          "%s is not a source file in the workspace"
          (Path.to_string_maybe_quoted path)
      ]
;;

let term =
  let+ builder = Common.Builder.term
  and+ context_name = Common.context_arg ~doc:(Some "Build context to use.")
  and+ format = Describe_format.arg
  and+ file =
    Arg.(
      required
      & pos
          0
          (some string)
          None
          (info [] ~docv:"FILE" ~doc:(Some "OCaml source file to look up.")))
  in
  let common, config = Common.init builder in
  Scheduler_setup.go_with_rpc_server ~common ~config
  @@ fun () ->
  Build.build_memo_exn
  @@ fun () ->
  let open Memo.O in
  let source = source_path file in
  let* setup = Util.setup () in
  let sctx = Dune_rules.Main.find_scontext_exn setup ~name:context_name in
  let* outputs = Melange_rules.js_outputs_of_source ~sctx ~source in
  List.map outputs ~f:output_to_dyn |> Dyn.list Fun.id |> Describe_format.print_dyn format;
  Memo.return ()
;;

let command =
  let doc =
    "Print the JavaScript outputs generated for an OCaml source by enabled melange.emit \
     stanzas. The output format of this command is experimental and is subject to change \
     without warning"
  in
  Cmd.v (Cmd.info ~doc "melange-outputs") term
;;
