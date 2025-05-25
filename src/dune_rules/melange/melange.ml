open Import

module Module_system = struct
  type t =
    | ESM
    | CommonJS

  let default = CommonJS, ".js"

  let to_string = function
    | ESM -> "es6"
    | CommonJS -> "commonjs"
  ;;
end

module Cm_kind = Dune_lang.Melange.Cm_kind

module Source = struct
  let dir = ".melange_src"
end

module Install = struct
  let dir = "melange"

  let maybe_prepend_melange_install_dir =
    let melange_install_dir = dir in
    fun ~for_ dir ->
      match for_ with
      | Lib_mode.Ocaml _ -> dir
      | Melange ->
        let base = melange_install_dir in
        Option.map dir ~f:(fun dir ->
          Path.Local.relative (Path.Local.of_string base) dir |> Path.Local.to_string)
        |> Option.value ~default:base
        |> Option.some
  ;;
end
