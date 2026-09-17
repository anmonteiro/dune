open Import

val output_files
  :  dir:Path.Build.t
  -> lib_config:Lib_config.t
  -> Executables.t
  -> Path.Build.t list

val rule_targets
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> lib_config:Lib_config.t
  -> dialects:Dialect.DB.t
  -> project:Dune_project.t
  -> Executables.t
  -> Target_mask.t

val compile_info : scope:Scope.t -> Executables.t -> Lib.Compile.t Memo.t

val rules
  :  sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> Executables.t
  -> (Compilation_context.t * Merlin.t) Memo.t
