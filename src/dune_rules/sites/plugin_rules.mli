open Import

val rule_targets : dir:Path.Build.t -> Plugin.t -> Target_mask.t
val setup_rules : sctx:Super_context.t -> dir:Path.Build.t -> Plugin.t -> unit Memo.t

val install_rules
  :  sctx:Super_context.t
  -> package_db:Package_db.t
  -> dir:Path.Build.t
  -> Plugin.t
  -> Install.Entry.Sourced.Unexpanded.t list Memo.t
