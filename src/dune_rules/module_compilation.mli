(** OCaml module compilation *)

open Import

(** Setup rules to build a single module.*)
val build_module
  :  ?force_write_cmi:bool
  -> ?precompiled_cmi:bool
  -> Compilation_context.t
  -> Module.t
  -> unit Memo.t

val ocamlc_i
  :  deps:Module.t list Action_builder.t Ml_kind.Dict.t
  -> Compilation_context.t
  -> Module.t
  -> output:Path.Build.t
  -> unit Memo.t

val build_all : Compilation_context.t -> unit Memo.t

val with_empty_intf
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> Module.t
  -> Module.t Memo.t

val melange_js_basename : Module.t -> Filename.t

module For_benchmarks : sig
  type input

  val create_input
    :  aliases:(Module_name.Path.t * Module_name.t * Module_name.Unique.t) list
    -> shadowed:Module_name.t list
    -> input

  (** Includes the alias preparation normally performed by [of_modules]. *)
  val prepare_and_render : input -> string
end
