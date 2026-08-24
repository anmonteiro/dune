open Import

module Js_output : sig
  type t =
    { module_system : Melange.Module_system.t
    ; build_path : Path.Build.t
    ; promoted_path : Path.Source.t option
    }
end

val js_outputs_of_source
  :  sctx:Super_context.t
  -> source:Path.Source.t
  -> Js_output.t list Memo.t

val setup_melange_sources_copy_rules
  :  sctx:Super_context.t
  -> dir:Path.Build.t
  -> preprocess:Preprocess.With_instrumentation.t Preprocess.Per_module.t
  -> Modules.t
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
