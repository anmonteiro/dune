open Import

module Runtime_deps : sig
  type t =
    | Dependency of Dep_conf.t
    | File_binding of File_binding.Unexpanded.t
end

(** Stanza to produce JavaScript targets from Melange libraries *)
module Emit : sig
  type t =
    { loc : Loc.t
    ; target : string
    ; alias : Alias.Name.t option
    ; module_systems : (Melange.Module_system.t * Filename.Extension.t) list
    ; modules : Modules_settings.t
    ; emit_stdlib : bool
    ; libraries : Lib_dep.t list
    ; package : Package.t option
    ; preprocess : Preprocess.preprocess
    ; runtime_deps : Loc.t * Runtime_deps.t list
    ; lint : Preprocess.Without_instrumentation.t Preprocess.Per_module.t
    ; promote : Rule_mode.Promote.t option
    ; compile_flags : Ordered_set_lang.Unexpanded.t
    ; allow_overlapping_dependencies : bool
    ; enabled_if : Blang.t
    }

  include Stanza.S with type t := t

  val implicit_alias : Alias.Name.t
  val decode : t Dune_lang.Decoder.t
  val target_dir : t -> dir:Path.Build.t -> Path.Build.t
end
