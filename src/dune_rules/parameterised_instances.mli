open Import

type t

val none : t
val instances : sctx:Super_context.t -> db:Lib.db -> Lib_dep.t list -> t Resolve.Memo.t

(** [ml_source_length t] is the exact number of bytes appended by
    [add_ml_source _ t]. *)
val ml_source_length : t -> int

val add_ml_source : String_builder.t -> t -> unit

module For_benchmarks : sig
  type t
  type instance

  val instance
    :  new_name:Module_name.t
    -> lib_name:Module_name.t
    -> args:(Module_name.t * Module_name.t) list
    -> instance

  val simple : instance -> t
  val wrapped : name:Module_name.t -> instance list -> t
  val concat : t list -> t
  val to_ml : t -> string
end
