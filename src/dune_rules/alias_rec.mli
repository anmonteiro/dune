open Import

module Alias_status : sig
  type t =
    | Defined
    | Not_defined

  include Monoid.S with type t := t
end

(** Depend on an alias recursively. Return [Defined] if the alias is defined in
    at least one directory, and [Not_defined] otherwise. *)
val dep_on_alias_rec :
  Alias.Name.t -> Path.Build.t -> Alias_status.t Action_builder.t
