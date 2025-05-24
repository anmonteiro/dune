open Import

val maybe_prepend_melange_install_dir : for_:Lib_mode.t -> string option -> string option

val setup_melange_sources_copy_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> expander:'a
  -> modules:Modules.t
  -> unit Memo.t

val setup_emit_cmj_rules
  :  sctx:Super_context.t
  -> scope:Scope.t
  -> expander:Expander.t
  -> dir_contents:Dir_contents.t
  -> Melange_stanzas.Emit.t
  -> (Compilation_context.t * Merlin.t) Memo.t

val setup_emit_js_rules
  :  Super_context.t Memo.t
  -> dir:Path.Build.t
  -> Build_config.Gen_rules.t Memo.t
