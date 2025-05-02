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

module Cm_kind = struct
  type t =
    | Cmi
    | Cmj

  let source = function
    | Cmi -> Ocaml.Ml_kind.Intf
    | Cmj -> Impl
  ;;

  let ext = function
    | Cmi -> ".cmi"
    | Cmj -> ".cmj"
  ;;

  let to_dyn =
    let open Dyn in
    function
    | Cmi -> variant "cmi" []
    | Cmj -> variant "cmj" []
  ;;

  module Map = struct
    type 'a t =
      { cmi : 'a
      ; cmj : 'a
      }

    let make_all x = { cmi = x; cmj = x }
  end
end

module Source = struct
  let dir = ".melange_src"
end

module Install = struct
  let dir = "melange"
end
