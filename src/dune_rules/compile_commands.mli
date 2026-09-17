open Import

val rule_targets : dir:Path.Build.t -> Target_mask.t

(** Generate compile_commands.json rule for the workspace. *)
val gen_rules : Super_context.t -> unit Memo.t
