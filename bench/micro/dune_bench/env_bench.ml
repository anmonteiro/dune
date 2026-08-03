open Stdune

let make_env size ~temp_dir =
  let env =
    List.init size ~f:(fun i ->
      let prefix = if i mod 2 = 0 then 'A' else 'Z' in
      Printf.sprintf "%c_DUNE_BENCH_%04d=value-%04d" prefix i i)
    |> Array.of_list
    |> Env.of_unix
  in
  if temp_dir
  then Env.add env ~var:Env.Var.temp_dir ~value:"original-temp-directory"
  else env
;;

let render_environment_override ~temp_dir variables =
  let env = make_env variables ~temp_dir in
  fun () ->
    Env.to_unix_with_override env ~var:Env.Var.temp_dir ~value:"dune-temp-directory"
    |> ignore
;;

let%bench_fun
    ("render environment override: present" [@indexed variables = [ 10; 100; 1_000 ]])
  =
  render_environment_override ~temp_dir:true variables
;;

let%bench_fun
    ("render environment override: absent" [@indexed variables = [ 10; 100; 1_000 ]])
  =
  render_environment_override ~temp_dir:false variables
;;
