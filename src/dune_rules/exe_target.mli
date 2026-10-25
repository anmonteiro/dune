open Import

(** An executable or [melange.emit] target. *)
type t

val executables : (Loc.t * string) Nonempty_list.t -> t
val melange_emit : string option -> t
val compilation_mode : t -> Compilation_mode.t
val names : t -> string Nonempty_list.t

(** The first executable name, emit target, or ["."] for a targetless emit. *)
val first_name : t -> string

val description : t -> 'a Pp.t
val repr : t Repr.t
