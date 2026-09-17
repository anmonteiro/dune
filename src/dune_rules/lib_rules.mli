open Import

val rule_targets
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> lib_config:Lib_config.t
  -> dialects:Dialect.DB.t
  -> Library.t
  -> Target_mask.t

val foreign_rule_targets
  :  dir:Path.Build.t
  -> lib_config:Lib_config.t
  -> Foreign_library.t
  -> Target_mask.t

val foreign_rules
  :  Foreign_library.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> unit Memo.t

val compile_context
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> for_:Compilation_mode.t
  -> Compilation_context.t Memo.t

val rules
  :  Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> (Compilation_context.t * Merlin.t) option Compilation_mode.Per_mode.t Memo.t
