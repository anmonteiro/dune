module File_kind = struct
  include File_kind

  module Option = struct
    [@@@warning "-37"]

    (* The values are constructed on the C-side *)
    type t =
      | S_REG
      | S_DIR
      | S_CHR
      | S_BLK
      | S_LNK
      | S_FIFO
      | S_SOCK
      | UNKNOWN
  end
end

module Readdir_result = struct
  [@@@warning "-37"]

  (* The values are constructed on the C-side *)
  type t =
    | End_of_directory
    | Entry of Filename.t * File_kind.Option.t

  module Batch = struct
    type entries = (Filename.t * File_kind.t) list

    type t =
      | Continue of entries
      | End_of_directory of entries
      | Unknown of Filename.t * entries
  end
end

external readdir_with_kind_if_available_unix
  :  Unix.dir_handle
  -> Readdir_result.t
  = "caml__dune_filesystem_stubs__readdir"

let rec readdir_with_kind_if_available_win32 : Unix.dir_handle -> Readdir_result.t =
  fun dir ->
  (* Windows also gives us the information about file kind and it's discarded by
     [readdir]. We could do better here, but the Windows code is more
     complicated. (there's an additional OCaml abstraction layer) *)
  match Unix.readdir dir with
  | exception End_of_file -> Readdir_result.End_of_directory
  | "." | ".." -> readdir_with_kind_if_available_win32 dir
  | entry -> Entry (Filename.of_string_exn entry, File_kind.Option.UNKNOWN)
;;

let readdir_with_kind_if_available =
  if Stdlib.Sys.win32
  then readdir_with_kind_if_available_win32
  else readdir_with_kind_if_available_unix
;;

let with_directory dir_path ~f =
  let start = Counter.Timer.start () in
  Counter.incr Metrics.Directory_read.count;
  let dir =
    match Unix.opendir dir_path with
    | dir -> dir
    | exception exn ->
      Counter.Timer.stop Metrics.Directory_read.time start;
      raise exn
  in
  Fun.protect
    ~finally:(fun () ->
      Unix.closedir dir;
      Counter.Timer.stop Metrics.Directory_read.time start)
    (fun () -> f dir)
;;

let read_directory_with_kinds_portable dir_path =
  with_directory dir_path ~f:(fun dir ->
    let rec loop acc =
      match readdir_with_kind_if_available dir with
      | End_of_directory -> acc
      | Entry (base, File_kind.Option.S_REG) -> loop ((base, Unix.S_REG) :: acc)
      | Entry (base, S_DIR) -> loop ((base, Unix.S_DIR) :: acc)
      | Entry (base, S_CHR) -> loop ((base, Unix.S_CHR) :: acc)
      | Entry (base, S_BLK) -> loop ((base, Unix.S_BLK) :: acc)
      | Entry (base, S_LNK) -> loop ((base, Unix.S_LNK) :: acc)
      | Entry (base, S_FIFO) -> loop ((base, Unix.S_FIFO) :: acc)
      | Entry (base, S_SOCK) -> loop ((base, Unix.S_SOCK) :: acc)
      | Entry (base, UNKNOWN) ->
        (match Unix.lstat (Filename.append dir_path base) with
         | exception Unix.Unix_error _ ->
           (* File disappeared between readdir & lstat system calls. Handle
              as if readdir never told us about it. *)
           loop acc
         | stat -> loop ((base, stat.st_kind) :: acc))
    in
    loop [])
;;

external readdir_batch
  :  Unix.dir_handle
  -> Readdir_result.Batch.entries
  -> Readdir_result.Batch.t
  = "caml__dune_filesystem_stubs__readdir_batch"

let read_directory_with_kinds_exn =
  if Stdlib.Sys.win32
  then read_directory_with_kinds_portable
  else
    fun dir_path ->
      with_directory dir_path ~f:(fun dir ->
        let rec loop acc =
          match readdir_batch dir acc with
          | Continue acc -> loop acc
          | End_of_directory acc -> acc
          | Unknown (base, acc) ->
            (match Unix.lstat (Filename.append dir_path base) with
             | exception Unix.Unix_error _ -> loop acc
             | stat -> loop ((base, stat.st_kind) :: acc))
        in
        loop [])
;;

let read_directory_with_kinds dir_path =
  Unix_error.Detailed.catch read_directory_with_kinds_exn dir_path
;;

let read_directory_exn dir_path =
  with_directory dir_path ~f:(fun dir ->
    let rec loop acc =
      match readdir_with_kind_if_available dir with
      | End_of_directory -> acc
      | Entry (base, _) -> loop (base :: acc)
    in
    loop [])
;;

let read_directory dir_path = Unix_error.Detailed.catch read_directory_exn dir_path
