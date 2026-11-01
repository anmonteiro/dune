(** Menhir rules *)

open Import

val source_files
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> Menhir_stanza.t
  -> Path.Build.t list

val rule_targets
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> obj_dirs:Path.Build.t Obj_dir.t list
  -> Menhir_stanza.t
  -> Target_mask.t

(** Generate the rules for a [(menhir ...)] stanza. *)
val gen_rules
  :  dir:Path.Build.t
  -> module_path:Module_name.t list
  -> Compilation_context.t
  -> Menhir_stanza.t
  -> unit Memo.t

val menhir_env : dir:Path.Build.t -> string list Action_builder.t Menhir_env.t Memo.t
