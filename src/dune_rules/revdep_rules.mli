open Import

val rule_targets : dir:Path.Build.t -> Target_mask.t

(** Add revdep alias rules (@revdep, @revdep-check, @revdep-runtest,
    @revdep-install) for a directory. These aliases build all reverse
    dependencies of libraries defined in the directory. *)
val add : sctx:Super_context.t -> dir:Path.Build.t -> unit Memo.t
