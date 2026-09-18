(** DSL to define sets that are defined by a membership : 'a -> bool function. *)

open Stdune
open Dune_sexp

type 'a t

val of_list : 'a list -> 'a t
val element : 'a -> 'a t
val standard : 'a t
val diff : 'a t -> 'a t -> 'a t
val and_ : 'a t list -> 'a t
val not : 'a t -> 'a t
val or_ : 'a t list -> 'a t
val true_ : 'a t
val decode_one : 'a Decoder.t -> 'a t Decoder.t
val decode : 'a Decoder.t -> 'a t Decoder.t
val encode : 'a Encoder.t -> 'a t Encoder.t
val repr : 'a Repr.t -> 'a t Repr.t
val to_dyn : 'a Dyn.builder -> 'a t Dyn.builder
val test : 'a t -> standard:'a t -> test:('a -> 'b -> bool) -> 'b -> bool
val false_ : 'a t
val compare : ('a -> 'a -> Ordering.t) -> 'a t -> 'a t -> Ordering.t

module Glob : sig
  module Element : sig
    type t
  end

  type nonrec t = Element.t t

  val repr : t Repr.t
  val to_dyn : t -> Dyn.t
  val test : t -> standard:t -> string -> bool
  val of_glob : Dune_rpc.Private.Glob.t -> t

  (** [of_string_list xs] return an expression that will match any element
      inside the list [xs] *)
  val of_string_list : string list -> t

  (** [of_string_list xs] return an expression that will only match any element
      inside the set [xs] *)
  val of_string_set : String.Set.t -> t

  (** Recognise finite combinations of literal names. [None] means the set
      could not be determined without evaluating a general predicate. *)
  val finite_elements : t -> String.Set.t option

  (** Conservatively check whether any match can end in this suffix. *)
  val may_match_suffix : t -> string -> bool

  val compare : t -> t -> Ordering.t
  val equal : t -> t -> bool
  val hash : t -> int
  val decode : t Dune_sexp.Decoder.t
  val encode : t -> Dune_sexp.t
  val digest : t -> Dune_digest.t
end
