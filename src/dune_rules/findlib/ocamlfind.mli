open Stdune

type t

val conf_path : t -> Path.t list

val tool : t -> prog:string -> Path.t option Memo.t

val ocamlpath_sep : char

val ocamlpath : Env.t -> Path.t list

val extra_env : t -> Env.t

val discover_from_env :
     env:Env.t
  -> which:(string -> Path.t option Memo.t)
  -> ocamlpath:Path.t list
  -> findlib_toolchain:Dune_engine.Context_name.t option
  -> t option Memo.t
