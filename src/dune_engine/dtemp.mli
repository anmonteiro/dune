(** Temp directory used by dune processes *)

open Import

(** This returns a build path, but we don't rely on that *)
val file : prefix:string -> suffix:string -> Path.t

(** Serialize an environment with Dune's temp directory set. *)
val to_unix : Env.t -> string list

(** Destroy the temporary file or directory *)
val destroy : Temp.what -> Path.t -> unit

val clear : unit -> unit
