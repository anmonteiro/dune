open Import

type t =
  | Ocaml of Ocaml.Mode.t
  | Melange

val decode : t Decoder.t
val equal : t -> t -> bool
val is_ocaml : t -> bool

module Cm_kind : sig
  type t =
    | Ocaml of Ocaml.Cm_kind.t
    | Melange of Melange.Cm_kind.t

  val source : t -> Ocaml.Ml_kind.t
  val ext : t -> string
  val cmi : t -> t
  val to_dyn : t -> Dyn.t

  module Map : sig
    type cm_kind := t

    type 'a t =
      { ocaml : 'a Ocaml.Cm_kind.Dict.t
      ; melange : 'a Melange.Cm_kind.Map.t
      }

    val get : 'a t -> cm_kind -> 'a
    val make_all : 'a -> 'a t
  end
end

val of_cm_kind : Cm_kind.t -> t

module By_mode : sig
  type mode := t

  type 'a t =
    { ocaml : 'a
    ; melange : 'a
    }

  val for_merlin : 'a option t -> 'a
  val to_list : 'a option t -> (mode * 'a) list
  val of_list : (mode * 'a) list -> 'a option t
end

module Map : sig
  type mode := t

  type 'a t =
    { ocaml : 'a Ocaml.Mode.Dict.t
    ; melange : 'a
    }

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val to_dyn : ('a -> Dyn.t) -> 'a t -> Dyn.t
  val get : 'a t -> mode -> 'a
  val map : 'a t -> f:('a -> 'b) -> 'b t
  val make_all : 'a -> 'a t
  val make : byte:'a -> native:'a -> melange:'a -> 'a t

  module Set : sig
    type nonrec t = bool t

    val encode : t -> Dune_sexp.t list
    val of_list : mode list -> t
    val to_dyn : t -> Dyn.t
    val equal : t -> t -> bool
    val for_merlin : t -> mode
  end
end
