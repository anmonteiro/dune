open Import

type t =
  { config : Findlib.Config.t
  ; ocamlpath : Path.t list
  ; which : string -> Path.t option Memo.t
  ; toolchain : string option
  }

let ocamlpath_sep =
  if Sys.cygwin then (* because that's what ocamlfind expects *)
    ';'
  else Bin.path_sep

let path_var = Bin.parse_path ~sep:ocamlpath_sep

let ocamlpath env =
  match Env.get env "OCAMLPATH" with
  | None -> []
  | Some s -> path_var s

let toolchain t = t.toolchain

let set_toolchain t ~toolchain =
  match t.toolchain with
  | None ->
    { t with
      config = Findlib.Config.toolchain t.config ~toolchain
    ; toolchain = Some toolchain
    }
  | Some old_toolchain ->
    Code_error.raise "Ocamlfind.set_toolchain: cannot set toolchain twice"
      [ ("old_toolchain", Dyn.string old_toolchain)
      ; ("toolchain", Dyn.string toolchain)
      ]

let conf_path t =
  match Findlib.Config.get t.config "path" with
  | None -> t.ocamlpath
  | Some p -> t.ocamlpath @ path_var p

let tool t ~prog =
  match Findlib.Config.get t.config prog with
  | None -> Memo.return None
  | Some s -> (
    match Filename.analyze_program_name s with
    | In_path -> t.which s
    | Relative_to_current_dir ->
      User_error.raise
        [ Pp.textf
            "The effective Findlib configuration specifies the relative path \
             %S for the program %S. This is currently not supported."
            s prog
        ]
    | Absolute ->
      Memo.return (Some (Path.of_filename_relative_to_initial_cwd s)))

let ocamlfind_config_path ~env ~which =
  Memo.lazy_ ~cutoff:(Option.equal Path.External.equal) (fun () ->
      let open Memo.O in
      let+ path =
        match Env.get env "OCAMLFIND_CONF" with
        | Some s -> Memo.return (Some s)
        | None -> (
          which "ocamlfind" >>= function
          | None -> Memo.return None
          | Some fn ->
            Memo.of_reproducible_fiber
              (Process.run_capture_line ~display:Quiet ~env Strict fn
                 [ "printconf"; "conf" ])
            |> Memo.map ~f:Option.some)
      in
      Option.map path ~f:Path.External.of_filename_relative_to_initial_cwd)

let discover_from_env ~env ~ocamlpath ~which =
  let open Memo.O in
  Memo.Lazy.force (ocamlfind_config_path ~env ~which) >>= function
  | None -> Memo.return None
  | Some config ->
    let+ config = Findlib.Config.load (External config) in
    Some { config; ocamlpath; which; toolchain = None }

let extra_env t = Findlib.Config.env t.config
