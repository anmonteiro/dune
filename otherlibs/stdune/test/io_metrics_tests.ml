open Stdune

let ok_exn = function
  | Ok value -> value
  | Error error -> Unix_error.Detailed.raise error
;;

let timer_changed timer before =
  match Time.Span.compare (Counter.Timer.read timer) before with
  | Gt -> true
  | Eq | Lt -> false
;;

let measure_write f =
  let file_read_before = Counter.Timer.read Metrics.File_read.time in
  let file_write_count_before = Counter.read Metrics.File_write.count in
  let file_write_before = Counter.Timer.read Metrics.File_write.time in
  let directory_read_before = Counter.Timer.read Metrics.Directory_read.time in
  f ();
  Dune_tests_common.print_dyn
    (Dyn.record
       [ ( "file_write_count"
         , Dyn.int (Counter.read Metrics.File_write.count - file_write_count_before) )
       ; ( "file_write_time_changed"
         , Dyn.bool (timer_changed Metrics.File_write.time file_write_before) )
       ; ( "file_read_time_changed"
         , Dyn.bool (timer_changed Metrics.File_read.time file_read_before) )
       ; ( "directory_read_time_changed"
         , Dyn.bool (timer_changed Metrics.Directory_read.time directory_read_before) )
       ])
;;

let%expect_test "IO metrics are attributed to the operation being measured" =
  let dir = Temp.create Dir ~prefix:"io-metrics" ~suffix:"test" in
  let file = Path.relative dir "file" in
  measure_write (fun () -> Io.write_file_exn file "contents");
  [%expect
    {|
    { file_write_count = 1
    ; file_write_time_changed = true
    ; file_read_time_changed = false
    ; directory_read_time_changed = false
    }
    |}];
  measure_write (fun () -> Io.write_lines file [ "one"; "two" ]);
  [%expect
    {|
    { file_write_count = 1
    ; file_write_time_changed = true
    ; file_read_time_changed = false
    ; directory_read_time_changed = false
    }
    |}]
;;

let%expect_test "writing a descriptor records IO metrics" =
  let dir = Temp.create Dir ~prefix:"io-metrics" ~suffix:"test" in
  Temp.with_temp_file_fd
    ~dir
    ~prefix:"file"
    ~suffix:"test"
    ~f:(fun result ->
      let _, fd = Result.ok_exn result in
      measure_write (fun () -> Io.write_fd fd "contents" |> Result.ok_exn))
    ();
  [%expect
    {|
    { file_write_count = 1
    ; file_write_time_changed = true
    ; file_read_time_changed = false
    ; directory_read_time_changed = false
    }
    |}]
;;

let%expect_test "directory metrics count directory scans" =
  let dir = Temp.create Dir ~prefix:"directory-metrics" ~suffix:"test" in
  let first = Path.relative dir "first" in
  let second = Path.relative dir "second" in
  Path.mkdir_p first;
  Path.mkdir_p second;
  let count_before = Counter.read Metrics.Directory_read.count in
  let time_before = Counter.Timer.read Metrics.Directory_read.time in
  ignore (Readdir.read_directory (Path.to_string first) |> ok_exn : Filename.t list);
  ignore
    (Readdir.read_directory_with_kinds (Path.to_string second) |> ok_exn
     : (Filename.t * Unix.file_kind) list);
  Dune_tests_common.print_dyn
    (Dyn.record
       [ "count", Dyn.int (Counter.read Metrics.Directory_read.count - count_before)
       ; "time_changed", Dyn.bool (timer_changed Metrics.Directory_read.time time_before)
       ]);
  [%expect
    {|
    { count = 2; time_changed = true }
    |}]
;;

let%expect_test "directory scans preserve kinds and report missing directories" =
  let dir = Temp.create Dir ~prefix:"directory-kinds" ~suffix:"test" in
  Io.write_file_exn (Path.relative dir "file") "";
  Path.mkdir_p (Path.relative dir "dir");
  Unix.symlink "file" (Path.to_string (Path.relative dir "link"));
  Unix.symlink "missing" (Path.to_string (Path.relative dir "broken-link"));
  Readdir.read_directory_with_kinds (Path.to_string dir)
  |> ok_exn
  |> List.sort ~compare:(fun (a, _) (b, _) -> Filename.compare a b)
  |> List.iter ~f:(fun (name, kind) ->
    Printf.printf "%s: %s\n" (Filename.to_string name) (File_kind.to_string kind));
  let missing = Path.to_string (Path.relative dir "missing") in
  (match Readdir.read_directory_with_kinds missing with
   | Error (Unix.ENOENT, _, _) -> print_endline "missing directory: ENOENT"
   | Error error -> Unix_error.Detailed.raise error
   | Ok _ -> failwith "unexpected directory listing");
  [%expect
    {|
    broken-link: S_LNK
    dir: S_DIR
    file: S_REG
    link: S_LNK
    missing directory: ENOENT
    |}]
;;

let%expect_test "directory scans preserve chunk boundaries and mixed kinds" =
  List.iter
    [ 0, 0
    ; 1, 0
    ; 63, 0
    ; 64, 0
    ; 65, 0
    ; 127, 0
    ; 128, 0
    ; 129, 0
    ; 255, 0
    ; 256, 0
    ; 257, 0
    ; 65, 123
    ; 65, 180
    ]
    ~f:(fun (count, width) ->
      let dir = Temp.create Dir ~prefix:"directory-chunks" ~suffix:"test" in
      let expected =
        List.init count ~f:(fun index ->
          let name =
            Printf.sprintf "%03d-%s" index (String.make width 'x')
            |> Filename.of_string_exn
          in
          let path = Path.relative dir (Filename.to_string name) in
          let kind =
            match index mod 3 with
            | 0 ->
              Io.write_file_exn path "";
              Unix.S_REG
            | 1 ->
              Path.mkdir_p path;
              Unix.S_DIR
            | _ ->
              Unix.symlink "missing" (Path.to_string path);
              Unix.S_LNK
          in
          name, kind)
      in
      let count_before = Counter.read Metrics.Directory_read.count in
      let names = Readdir.read_directory (Path.to_string dir) |> ok_exn in
      let entries = Readdir.read_directory_with_kinds (Path.to_string dir) |> ok_exn in
      let sorted =
        List.sort entries ~compare:(fun (a, _) (b, _) -> Filename.compare a b)
      in
      Printf.printf
        "%d/%d: contents=%b order=%b scans=%d\n"
        count
        width
        (List.equal
           (fun (a, ka) (b, kb) -> Filename.equal a b && ka = kb)
           sorted
           expected)
        (List.equal Filename.equal names (List.map entries ~f:fst))
        (Counter.read Metrics.Directory_read.count - count_before));
  [%expect
    {|
    0/0: contents=true order=true scans=2
    1/0: contents=true order=true scans=2
    63/0: contents=true order=true scans=2
    64/0: contents=true order=true scans=2
    65/0: contents=true order=true scans=2
    127/0: contents=true order=true scans=2
    128/0: contents=true order=true scans=2
    129/0: contents=true order=true scans=2
    255/0: contents=true order=true scans=2
    256/0: contents=true order=true scans=2
    257/0: contents=true order=true scans=2
    65/123: contents=true order=true scans=2
    65/180: contents=true order=true scans=2
    |}]
;;
