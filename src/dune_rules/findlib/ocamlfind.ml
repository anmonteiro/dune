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

let ocamlfind_config_path ~env ~which ~findlib_toolchain =
  let open Memo.O in
  let+ path =
    match Env.get env "OCAMLFIND_CONF" with
    | Some s -> Memo.return (Some s)
    | None -> (
      match findlib_toolchain with
      | None -> Memo.return None
      | Some _ -> (
        which "ocamlfind" >>= function
        | None -> Memo.return None
        | Some fn ->
          Memo.of_reproducible_fiber
            (Process.run_capture_line ~display:Quiet ~env Strict fn
               [ "printconf"; "conf" ])
          |> Memo.map ~f:Option.some))
  in
  (* From http://projects.camlcity.org/projects/dl/findlib-1.9.6/doc/ref-html/r865.html
     This variable overrides the location of the configuration file
     findlib.conf. It must contain the absolute path name of this file. *)
  Option.map path ~f:Path.External.of_string

let discover_from_env ~env ~which ~ocamlpath ~findlib_toolchain =
  let open Memo.O in
  ocamlfind_config_path ~env ~which ~findlib_toolchain >>= function
  | None -> Memo.return None
  | Some config ->
    let+ config = Findlib.Config.load (External config) in
    let base = { config; ocamlpath; which; toolchain = None } in
    Some
      (match findlib_toolchain with
      | None -> base
      | Some toolchain ->
        let toolchain = Context_name.to_string toolchain in
        set_toolchain base ~toolchain)

let extra_env t = Findlib.Config.env t.config
