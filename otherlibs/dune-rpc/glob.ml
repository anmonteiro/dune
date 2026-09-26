open Stdune

type t =
  | Re of
      { re : Re.re
      ; repr : string
      ; suffix : string
      }
  | Literal of string

let test t s =
  match t with
  | Literal t -> String.equal t s
  | Re { re; repr = _; suffix = _ } -> Re.execp re s
;;

let empty = Re { re = Re.compile Re.empty; repr = "\000"; suffix = "" }
let universal = Re { re = Re.compile (Re.rep Re.any); repr = "**"; suffix = "" }

let of_string_result repr =
  Glob_lexer.parse_string repr
  |> Result.map ~f:(function
    | Glob_lexer.Literal s -> Literal s
    | Re { re; suffix } -> Re { re = Re.compile re; repr; suffix })
;;

let of_string repr =
  match of_string_result repr with
  | Error (_, msg) -> invalid_arg (Printf.sprintf "invalid glob: :%s" msg)
  | Ok t -> t
;;

let to_string t =
  match t with
  | Re { repr; re = _; suffix = _ } -> repr
  | Literal s -> s
;;

let as_literal = function
  | Literal s -> Some s
  | Re _ -> None
;;

let literal_suffix = function
  | Literal s -> s
  | Re { suffix; _ } -> suffix
;;

let to_dyn t = Dyn.variant "Glob" [ Dyn.string (to_string t) ]

let of_string_exn loc repr =
  match of_string_result repr with
  | Error (_, msg) -> User_error.raise ~loc [ Pp.textf "invalid glob: %s" msg ]
  | Ok t -> t
;;

let compare x y = String.compare (to_string x) (to_string y)
let hash t = String.hash (to_string t)

let escape s =
  let buf = Buffer.create (String.length s) in
  String.iter s ~f:(fun c ->
    (match c with
     | '*' | '?' | '[' | ']' | '{' | '}' | ',' | '\\' -> Buffer.add_char buf '\\'
     | _ -> ());
    Buffer.add_char buf c);
  Buffer.contents buf
;;

let matching_extensions extensions =
  let extensions = List.map extensions ~f:Filename.Extension.to_string in
  let re =
    let open Re in
    [ rep any; List.map extensions ~f:str |> alt ] |> seq |> compile
  in
  Re
    { re
    ; suffix = ""
    ; repr =
        (match extensions with
         | [] -> Code_error.raise "empty list of extensions" []
         | [ x ] -> "*" ^ escape x
         | xs -> "*{" ^ String.concat (List.map xs ~f:escape) ~sep:"," ^ "}")
    }
;;
