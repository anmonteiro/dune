module Exn = struct
  let protectx x ~f ~finally =
    match f x with
    | y ->
      finally x;
      y
    | exception e ->
      finally x;
      raise e
  ;;
end

module Bytes = BytesLabels

let rec eagerly_input_acc ic s ~pos ~len acc =
  if len <= 0
  then acc
  else (
    let r = input ic s pos len in
    if r = 0 then acc else eagerly_input_acc ic s ~pos:(pos + r) ~len:(len - r) (acc + r))
;;

(* [eagerly_input_string ic len] tries to read [len] chars from the channel.
   Unlike [really_input_string], if the file ends before [len] characters are
   found, it returns the characters it was able to read instead of raising an
   exception.

   This can be detected by checking that the length of the resulting string is
   less than [len]. *)
let eagerly_input_string ic len =
  let buf = Bytes.create len in
  let r = eagerly_input_acc ic buf ~pos:0 ~len 0 in
  if r = len then Bytes.unsafe_to_string buf else Bytes.sub_string buf ~pos:0 ~len:r
;;

let too_big = Failure "file is too large"

(* We use 65536 because that is the size of OCaml's IO buffers. *)
let chunk_size = 65536

let read_all_unless_large =
  (* Generic function for channels such that seeking is unsupported or broken *)
  let read_all_generic t buffer =
    let rec loop () =
      Buffer.add_channel buffer t chunk_size;
      loop ()
    in
    try loop () with
    | End_of_file -> Ok (Buffer.contents buffer)
  in
  fun t ->
    (* Optimisation for regular files: if the channel supports seeking, we
       compute the length of the file so that we read exactly what we need and
       avoid an extra memory copy. We expect that most files Dune reads are
       regular files so this optimizations seems worth it. *)
    match in_channel_length t with
    | exception Sys_error _ -> read_all_generic t (Buffer.create chunk_size)
    | n when n > Sys.max_string_length -> Error too_big
    | n ->
      (* For some files [in_channel_length] returns an invalid value. For
         instance for files in /proc it returns [0] and on Windows the returned
         value is larger than expected (it counts linebreaks as 2 chars, even
         in text mode).

         To be robust in both directions, we: - use [eagerly_input_string]
         instead of [really_input_string] in case we reach the end of the file
         early - read one more character to make sure we did indeed reach the
         end of the file *)
      let s = eagerly_input_string t n in
      (match input_char t with
       | exception End_of_file -> Ok s
       | c ->
         (* The [+ chunk_size] is to make sure there is at least [chunk_size]
            free space so that the first [Buffer.add_channel buffer t
            chunk_size] in [read_all_generic] does not grow the buffer. *)
         let buffer = Buffer.create (String.length s + 1 + chunk_size) in
         Buffer.add_string buffer s;
         Buffer.add_char buffer c;
         read_all_generic t buffer)
;;

let read_all_fd =
  let rec read fd buf pos left =
    if left = 0
    then pos
    else (
      match Unix.read fd buf pos left with
      | 0 -> pos
      | n -> read fd buf (pos + n) (left - n))
  in
  let read_to_eof fd initial =
    let probe = Bytes.create 1 in
    match Unix.read fd probe 0 1 with
    | 0 -> Ok initial
    | _ ->
      let initial_length = String.length initial in
      if initial_length >= Sys.max_string_length
      then Error too_big
      else (
        let capacity =
          if initial_length > Sys.max_string_length - chunk_size - 1
          then Sys.max_string_length
          else initial_length + chunk_size + 1
        in
        let buffer = Buffer.create capacity in
        Buffer.add_string buffer initial;
        Buffer.add_char buffer (Bytes.get probe 0);
        let chunk = Bytes.create chunk_size in
        let rec loop () =
          match Unix.read fd chunk 0 chunk_size with
          | 0 -> Ok (Buffer.contents buffer)
          | n ->
            if n > Sys.max_string_length - Buffer.length buffer
            then Error too_big
            else (
              Buffer.add_subbytes buffer chunk 0 n;
              loop ())
        in
        loop ())
  in
  fun fd ->
    let { Unix.st_size; _ } = Unix.fstat fd in
    if st_size > Sys.max_string_length
    then Error too_big
    else (
      let b = Bytes.create st_size in
      let bytes_read = read fd b 0 st_size in
      if bytes_read < st_size
      then Ok (Bytes.sub_string b ~pos:0 ~len:bytes_read)
      else read_to_eof fd (Bytes.unsafe_to_string b))
;;

let with_file_in_fd fn ~f =
  Exn.protectx (Unix.openfile fn [ O_RDONLY; O_CLOEXEC ] 0) ~f ~finally:Unix.close
;;

let read_file fn =
  match with_file_in_fd fn ~f:read_all_fd with
  | result -> result
  | exception (Unix.Unix_error _ as exn) -> Error exn
;;

let write_file =
  let rec write fd str pos left =
    if left > 0
    then (
      let written = Unix.single_write_substring fd str pos left in
      write fd str (pos + written) (left - written))
  in
  fun ~perm ~path ~data ->
    match
      Unix.openfile path [ O_WRONLY; O_CLOEXEC; O_CREAT; O_TRUNC ] perm
      |> Exn.protectx ~finally:Unix.close ~f:(fun fd ->
        write fd data 0 (String.length data))
    with
    | exception exn -> Error exn
    | () -> Ok ()
;;
