open Import

type t

module Melange_emit : sig
  type t =
    { target_dir : Path.Build.t
    ; output_dir : Path.Build.t
    ; stanza_dir : Path.Build.t
    ; alias : Alias.Name.t
    }
end

val empty : t

val make
  :  dir:Path.Build.t
  -> expander:Expander0.t
  -> lib_config:Lib_config.t Memo.t
  -> libs:(Library.t * Modules.t * Path.Build.t Obj_dir.t) list
  -> exes:(Modules.t * Path.Build.t Obj_dir.t) list
  -> melange_emits:(Melange_stanzas.Emit.t * Path.Build.t) list
  -> t Memo.t

val lookup_module : t -> Path.Build.t -> (Path.Build.t Obj_dir.t * Module.t) option
val lookup_library : t -> Lib_name.t -> Lib_info.local option
val lookup_melange_emit : t -> Path.Build.t -> Melange_emit.t option
