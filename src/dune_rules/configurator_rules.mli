(** configurator rules *)

open Import

(** Generate the rules for producing the files needed by configurator. *)
val gen_rules : Context_name.t -> unit Memo.t

(** Track the contexts and toolchains that determine every configurator file,
    independently of whether those files need to be rebuilt. *)
val inputs : unit Memo.Lazy.t

(** Force the files required by configurator at runtime to be produced. *)
val force_files : unit Memo.Lazy.t
