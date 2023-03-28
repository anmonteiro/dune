open Stdune

type t

val conf_path : t -> Path.t list

val discover_from_env :
     env:Env.t
  -> ocamlpath:Path.t list
  -> which:(string -> Path.t option Memo.t)
  -> t option Memo.t

val tool : t -> prog:string -> Path.t option Memo.t

val ocamlpath_sep : char

val ocamlpath : Env.t -> Path.t list

val toolchain : t -> string option

val set_toolchain : t -> toolchain:string -> t

val extra_env : t -> Env.t
