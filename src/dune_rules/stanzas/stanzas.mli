open Import

module Include : sig
  type t = Loc.t * string

  include Stanza.S with type t := t
end

module Dynamic_include : sig
  type t = Include.t

  include Stanza.S with type t := t
end

module Enabled_if_stanza : sig
  module Include_context : sig
    type t =
      | Source of Path.Source.t
      | Build of Path.Build.t

    val key : t Univ_map.Key.t
  end

  type t =
    { loc : Loc.t
    ; enabled_if : Blang.t
    ; current_file : Include_context.t
    ; stanzas : Dune_lang.Ast.t list
    }

  include Stanza.S with type t := t
end

val stanza_package : Stanza.t -> Package.Id.t option
