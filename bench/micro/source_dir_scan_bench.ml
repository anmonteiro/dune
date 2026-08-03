open Stdune
module Dir_contents = Source.Dir_contents

let () =
  Path.Build.set_build_dir (In_source_dir Path.Source.(relative root "_build"));
  ignore (Dune_engine.Fs_memo.init ~dune_file_watcher:None : Memo.Invalidation.t)
;;

let run memo =
  Memo.reset Memo.Invalidation.empty;
  Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "deadlock")
;;

let file_name i = Printf.sprintf "file-%05d" i

let create_file path =
  let fd = Unix.openfile path [ O_WRONLY; O_CREAT; O_EXCL ] 0o600 in
  Unix.close fd
;;

let create_fixture file_count =
  let basename =
    Printf.sprintf ".source-dir-scan-bench-%d-%d" (Unix.getpid ()) file_count
  in
  let dirname = Filename.concat (Sys.getcwd ()) basename in
  Unix.mkdir dirname 0o700;
  for i = 1 to file_count do
    create_file (Filename.concat dirname (file_name i))
  done;
  Stdlib.at_exit (fun () ->
    for i = 1 to file_count do
      Unix.unlink (Filename.concat dirname (file_name i))
    done;
    Unix.rmdir dirname);
  let path = Path.Source.relative Path.Source.root basename in
  let scan () =
    match run (Dir_contents.For_benchmarks.of_source_path path) with
    | Error _ -> failwith "unable to scan benchmark fixture"
    | Ok contents -> contents
  in
  let contents = scan () in
  let scanned_file_count =
    Dir_contents.files contents |> Filename.Array.Set.to_list |> List.length
  in
  if scanned_file_count <> file_count
  then
    failwith
      (Printf.sprintf
         "expected %d files in benchmark fixture, got %d"
         file_count
         scanned_file_count);
  scan
;;

let%bench_fun ("ordinary files" [@indexed file_count = [ 100; 1_000; 10_000 ]]) =
  let scan = create_fixture file_count in
  fun () -> ignore (scan () : Dir_contents.t)
;;
