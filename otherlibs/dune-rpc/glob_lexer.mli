open Stdune

type t =
  | Literal of string
  | Re of
      { re : Re.t
      ; suffix : string
      }

val parse_string : string -> (t, int * string) Result.result
