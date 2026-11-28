open Stdune

type byte_order =
  | Big
  | Little

module Magic = struct
  let mh_magic = 0xfeedfacel
  let mh_cigam = 0xcefaedfel
  let mh_magic_64 = 0xfeedfacfl
  let mh_cigam_64 = 0xcffaedfel
  let fat_magic = 0xcafebabel
  let fat_cigam = 0xbebafecal
  let fat_magic_64 = 0xcafebabfl
  let fat_cigam_64 = 0xbfbafecal

  type t =
    | Thin of byte_order * int
    | Fat of byte_order * int

  let decode magic =
    if Int32.equal magic mh_magic
    then Some (Thin (Big, 28))
    else if Int32.equal magic mh_cigam
    then Some (Thin (Little, 28))
    else if Int32.equal magic mh_magic_64
    then Some (Thin (Big, 32))
    else if Int32.equal magic mh_cigam_64
    then Some (Thin (Little, 32))
    else if Int32.equal magic fat_magic
    then Some (Fat (Big, 20))
    else if Int32.equal magic fat_cigam
    then Some (Fat (Little, 20))
    else if Int32.equal magic fat_magic_64
    then Some (Fat (Big, 32))
    else if Int32.equal magic fat_cigam_64
    then Some (Fat (Little, 32))
    else None
  ;;
end

module File_type = struct
  let mh_execute = 0x2l
  let mh_preload = 0x5l
  let mh_dylib = 0x6l
  let mh_dylinker = 0x7l
  let mh_bundle = 0x8l
  let mh_kext_bundle = 0xbl

  (* Match sigtool's MachO::requiresSignature, rather than treating every Mach-O
     file (including relocatable objects) as a linked image. *)
  let requires_signature file_type =
    List.mem
      [ mh_execute; mh_preload; mh_dylib; mh_dylinker; mh_bundle; mh_kext_bundle ]
      file_type
      ~equal:Int32.equal
  ;;
end

let int32 byte_order bytes pos =
  match byte_order with
  | Big -> Stdlib.String.get_int32_be bytes pos
  | Little -> Stdlib.String.get_int32_le bytes pos
;;

let uint32 byte_order bytes pos =
  Int64.logand (Int64.of_int32 (int32 byte_order bytes pos)) 0xffff_ffffL
;;

let int64 byte_order bytes pos =
  match byte_order with
  | Big -> Stdlib.String.get_int64_be bytes pos
  | Little -> Stdlib.String.get_int64_le bytes pos
;;

let requires_signature path =
  Io.with_file_in ~binary:true path ~f:(fun ic ->
    let length = Stdlib.LargeFile.in_channel_length ic in
    let invalid message =
      User_error.raise
        [ Pp.textf "Invalid Mach-O file %s: %s" (Path.to_string path) message ]
    in
    let check_range ~offset ~size =
      if
        Int64.compare offset 0L < 0
        || Int64.compare size 0L < 0
        || Int64.compare offset length > 0
        || Int64.compare size (Int64.sub length offset) > 0
      then invalid "truncated header or invalid offset"
    in
    let read offset size =
      check_range ~offset ~size:(Int64.of_int size);
      Stdlib.LargeFile.seek_in ic offset;
      match really_input_string ic size with
      | bytes -> bytes
      | exception End_of_file -> invalid "truncated header"
    in
    let thin ~offset ~size byte_order header_size =
      if Int64.compare size (Int64.of_int header_size) < 0
      then invalid "truncated Mach-O header";
      let header = read offset header_size in
      let commands_size = uint32 byte_order header 20 in
      if Int64.compare commands_size (Int64.sub size (Int64.of_int header_size)) > 0
      then invalid "load commands extend beyond the image";
      File_type.requires_signature (int32 byte_order header 12)
    in
    if Int64.compare length 4L < 0
    then false
    else (
      match Magic.decode (int32 Big (read 0L 4) 0) with
      | None -> false
      | Some (Thin (byte_order, header_size)) ->
        thin ~offset:0L ~size:length byte_order header_size
      | Some (Fat (byte_order, arch_size)) ->
        let count = uint32 byte_order (read 0L 8) 4 in
        let arch_size64 = Int64.of_int arch_size in
        let table_size = Int64.mul count arch_size64 in
        check_range ~offset:8L ~size:table_size;
        if Int64.equal count 0L then invalid "empty architecture table";
        let table_end = Int64.add 8L table_size in
        let rec loop remaining entry_offset needs_signature =
          if Int64.equal remaining 0L
          then needs_signature
          else (
            let entry = read entry_offset arch_size in
            let offset, size =
              match arch_size with
              | 20 -> uint32 byte_order entry 8, uint32 byte_order entry 12
              | _ -> int64 byte_order entry 8, int64 byte_order entry 16
            in
            check_range ~offset ~size;
            if Int64.compare offset table_end < 0 || Int64.compare size 4L < 0
            then invalid "invalid slice extent";
            let magic = read offset 4 in
            let needs_signature_for_slice =
              match Magic.decode (int32 Big magic 0) with
              | Some (Thin (byte_order, header_size)) ->
                thin ~offset ~size byte_order header_size
              | _ ->
                (* Universal static libraries contain archives, not Mach-O
                   headers, in their architecture slices. *)
                if
                  String.equal magic "!<ar"
                  && Int64.compare size 8L >= 0
                  && String.equal (read offset 8) "!<arch>\n"
                then false
                else invalid "invalid Mach-O slice"
            in
            loop
              (Int64.pred remaining)
              (Int64.add entry_offset arch_size64)
              (needs_signature || needs_signature_for_slice))
        in
        loop count 8L false))
;;
