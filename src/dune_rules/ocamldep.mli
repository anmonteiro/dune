(** ocamldep management *)

open Import

module Mode : sig
  type t =
    | Standard
    | Transparent_aliases
end

(** [read_immediate_deps_of ~obj_dir ~modules ~ml_kind ~mode unit] returns the
    immediate dependencies found in the modules of [modules] for the file with
    kind [ml_kind] of the module [unit]. If there is no such file with kind
    [ml_kind], then an empty list of dependencies is returned.

    In [Transparent_aliases] mode, unused module aliases are omitted and
    physical object names are resolved before falling back to logical module
    names. *)
val read_immediate_deps_of
  :  sandbox:Sandbox_config.t
  -> sctx:Super_context.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> modules:Modules.With_vlib.t
  -> ml_kind:Ml_kind.t
  -> mode:Mode.t
  -> Module.t
  -> Module.t list Action_builder.t

(** [read_immediate_deps_raw_of ~sandbox ~sctx ~obj_dir ~ml_kind unit] returns
    the raw module names from ocamldep output without filtering against the
    stanza's module set. This preserves cross-library references that
    [read_immediate_deps_of] discards. *)
val read_immediate_deps_raw_of
  :  sandbox:Sandbox_config.t
  -> sctx:Super_context.t
  -> obj_dir:Path.Build.t Obj_dir.t
  -> ml_kind:Ml_kind.t
  -> Module.t
  -> Module_name.Set.t Action_builder.t
