open Import

val rule_targets : dir:Path.Build.t -> Target_mask.t

(** Generate compile_commands.json unless [rules] already defines its target. *)
val gen_rules : Super_context.t -> rules:Dune_engine.Rules.t -> unit Memo.t
