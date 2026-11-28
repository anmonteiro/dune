open Stdune
open Dune_rules
open Dune_tests_common

let () = init ()

type byte_order =
  | Big
  | Little

let set_int32 order bytes pos value =
  match order with
  | Big -> Stdlib.Bytes.set_int32_be bytes pos value
  | Little -> Stdlib.Bytes.set_int32_le bytes pos value
;;

let set_int64 order bytes pos value =
  match order with
  | Big -> Stdlib.Bytes.set_int64_be bytes pos value
  | Little -> Stdlib.Bytes.set_int64_le bytes pos value
;;

let thin order ~wide file_type =
  let bytes = Bytes.make (if wide then 32 else 28) '\000' in
  set_int32 order bytes 0 (if wide then 0xfeedfacfl else 0xfeedfacel);
  set_int32 order bytes 4 (if wide then 0x0100000cl else 7l);
  set_int32 order bytes 12 (Int32.of_int file_type);
  Bytes.to_string bytes
;;

let fat order ~wide slices =
  let arch_size = if wide then 32 else 20 in
  let header_size = 8 + (List.length slices * arch_size) in
  let bytes = Bytes.make header_size '\000' in
  set_int32 order bytes 0 (if wide then 0xcafebabfl else 0xcafebabel);
  set_int32 order bytes 4 (Int32.of_int (List.length slices));
  let offset = ref header_size in
  List.iteri slices ~f:(fun i slice ->
    let pos = 8 + (i * arch_size) in
    let size = String.length slice in
    if wide
    then (
      set_int64 order bytes (pos + 8) (Int64.of_int !offset);
      set_int64 order bytes (pos + 16) (Int64.of_int size))
    else (
      set_int32 order bytes (pos + 8) (Int32.of_int !offset);
      set_int32 order bytes (pos + 12) (Int32.of_int size));
    offset := !offset + size);
  Bytes.to_string bytes ^ String.concat ~sep:"" slices
;;

let classify contents =
  let path = Temp.create File ~prefix:"mach-o" ~suffix:"test" in
  Exn.protect
    ~finally:(fun () -> Temp.destroy File path)
    ~f:(fun () ->
      Io.with_file_out ~binary:true path ~f:(fun oc -> output_string oc contents);
      match Mach_o.requires_signature path with
      | result -> string_of_bool result
      | exception User_error.E _ -> "invalid")
;;

let test name contents = Printf.printf "%s: %s\n" name (classify contents)

let%expect_test "ordinary files and short input" =
  test "empty" "";
  test "short" "\xcf\xfa\xed";
  test "script" "#!/bin/sh\necho hello\n";
  test "bytecode" "#!/usr/bin/ocamlrun\nCaml1999X030";
  [%expect
    {|
    empty: false
    short: false
    script: false
    bytecode: false |}]
;;

let%expect_test "thin images use the file type in either byte order" =
  List.iter [ false; true ] ~f:(fun wide ->
    List.iter [ Big; Little ] ~f:(fun order ->
      Printf.printf
        "%d-bit %s:"
        (if wide then 64 else 32)
        (match order with
         | Big -> "big"
         | Little -> "little");
      for file_type = 1 to 12 do
        match classify (thin order ~wide file_type) with
        | "true" -> Printf.printf " %d" file_type
        | "false" -> ()
        | result -> Printf.printf " %d=%s" file_type result
      done;
      print_newline ()));
  [%expect
    {|
    32-bit big: 2 5 6 7 8 11
    32-bit little: 2 5 6 7 8 11
    64-bit big: 2 5 6 7 8 11
    64-bit little: 2 5 6 7 8 11 |}]
;;

let%expect_test "universal images inspect each slice" =
  let object_file = thin Big ~wide:false 1 in
  let executable = thin Little ~wide:true 2 in
  List.iter [ false; true ] ~f:(fun wide ->
    List.iter [ Big; Little ] ~f:(fun order ->
      Printf.printf
        "%d-bit %s\n"
        (if wide then 64 else 32)
        (match order with
         | Big -> "big"
         | Little -> "little");
      let fat = fat order ~wide in
      test "objects" (fat [ object_file; object_file ]);
      test "mixed" (fat [ object_file; executable ]);
      test "archives" (fat [ "!<arch>\n"; "!<arch>\n" ]);
      test "invalid later slice" (fat [ executable; "oops" ])));
  [%expect
    {|
    32-bit big
    objects: false
    mixed: true
    archives: false
    invalid later slice: invalid
    32-bit little
    objects: false
    mixed: true
    archives: false
    invalid later slice: invalid
    64-bit big
    objects: false
    mixed: true
    archives: false
    invalid later slice: invalid
    64-bit little
    objects: false
    mixed: true
    archives: false
    invalid later slice: invalid |}]
;;

let patch_int32 contents pos value =
  let bytes = Bytes.of_string contents in
  set_int32 Big bytes pos value;
  Bytes.to_string bytes
;;

let patch_int64 contents pos value =
  let bytes = Bytes.of_string contents in
  set_int64 Big bytes pos value;
  Bytes.to_string bytes
;;

let%expect_test "malformed recognized headers are errors" =
  let executable = thin Big ~wide:true 2 in
  let universal = fat Big ~wide:false [ executable ] in
  let universal64 = fat Big ~wide:true [ executable ] in
  test "truncated thin header" (String.sub executable ~pos:0 ~len:31);
  test "oversized load commands" (patch_int32 executable 20 0xffff_ffffl);
  test "truncated fat header" "\xca\xfe\xba\xbe";
  test "empty fat" (fat Big ~wide:false []);
  test "oversized architecture table" (patch_int32 universal 4 0xffff_ffffl);
  test "slice overlaps table" (patch_int32 universal 16 8l);
  test "slice outside file" (patch_int32 universal 16 0xffff_ffffl);
  test "oversized slice" (patch_int32 universal 20 0xffff_ffffl);
  test "truncated slice header" (patch_int32 universal 20 4l);
  test "unsigned 64-bit offset" (patch_int64 universal64 16 Int64.min_int);
  test "oversized 64-bit slice" (patch_int64 universal64 24 Int64.max_int);
  [%expect
    {|
    truncated thin header: invalid
    oversized load commands: invalid
    truncated fat header: invalid
    empty fat: invalid
    oversized architecture table: invalid
    slice overlaps table: invalid
    slice outside file: invalid
    oversized slice: invalid
    truncated slice header: invalid
    unsigned 64-bit offset: invalid
    oversized 64-bit slice: invalid |}]
;;

let%expect_test "read errors are not classified as non-Mach-O" =
  let path = Temp.create File ~prefix:"mach-o" ~suffix:"missing" in
  Temp.destroy File path;
  (match Mach_o.requires_signature path with
   | result -> print_endline (string_of_bool result)
   | exception Sys_error _ -> print_endline "read error");
  [%expect {| read error |}]
;;
