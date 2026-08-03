open Stdune

let make_env size =
  List.init size ~f:(fun i ->
    let prefix = if i mod 2 = 0 then 'A' else 'Z' in
    Printf.sprintf "%c_DUNE_BENCH_%04d=value-%04d" prefix i i)
  |> Array.of_list
  |> Env.of_unix
  |> Env.add ~var:Env.Var.temp_dir ~value:"original-temp-directory"
;;

let%bench_fun ("render environment override" [@indexed variables = [ 10; 100; 1_000 ]]) =
  let env = make_env variables in
  fun () ->
    Env.to_unix_with_override env ~var:Env.Var.temp_dir ~value:"dune-temp-directory"
    |> ignore
;;
