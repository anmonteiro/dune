open Stdune
module At_rev = Dune_pkg.Rev_store.At_rev

let make_paths file_count =
  List.init file_count ~f:(fun i ->
    let directory = i mod 100 in
    Path.Local.of_string (sprintf "dir-%03d/file-%06d" directory i))
;;

let%bench_fun
    ("directory_entries create" [@indexed file_count = [ 1_000; 10_000; 100_000 ]])
  =
  let paths = make_paths file_count in
  fun () -> ignore (At_rev.For_tests.make_directory_entries paths)
;;

let%bench_fun
    ("directory_entries immediate" [@indexed file_count = [ 1_000; 10_000; 100_000 ]])
  =
  let entries = make_paths file_count |> At_rev.For_tests.make_directory_entries in
  let path = Path.Local.of_string "dir-000" in
  fun () -> ignore (At_rev.For_tests.directory_entries entries ~recursive:false path)
;;
