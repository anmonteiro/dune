open Import

val rule_targets
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> lib_config:Lib_config.t
  -> dialects:Dialect.DB.t
  -> project:Dune_project.t
  -> Tests.t
  -> Target_mask.t

val rules
  :  Tests.t
  -> sctx:Super_context.t
  -> dir:Path.Build.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> (Compilation_context.t * Merlin.t) Memo.t
