open Import

module Args0 = struct
  type without_targets = [ `Others ]

  type any =
    [ `Others
    | `Targets
    ]

  type expand = dir:Path.t -> string Appendable_list.t Action_builder.t

  type _ t =
    | A : string -> _ t
    | As : string list -> _ t
    | S : 'a t list -> 'a t
    | Concat : string * 'a t list -> 'a t
    | Dep : Path.t -> _ t
    | Deps : Path.t list -> _ t
    | Target : Path.Build.t -> [> `Targets ] t
    | Path : Path.t -> _ t
    | Paths : Path.t list -> _ t
    | Hidden_deps : Dep.Set.t -> _ t
    | Hidden_targets : Path.Build.t list -> [> `Targets ] t
    | Dyn : without_targets t Action_builder.t -> _ t
    | Expand : expand -> _ t

  let dyn args =
    let args = Action_builder.map args ~f:Appendable_list.of_list in
    Expand (fun ~dir:_ -> args)
  ;;

  let empty = S []

  let as_any : without_targets t -> any t = function
    | A _ as x -> (x :> any t)
    | As _ as x -> (x :> any t)
    (* We can't convince the type checker to do a cast, so we force it.
       See the discussion in https://github.com/ocaml/dune/pull/10278 to see why
       the pattern match is optimized away. *)
    | S _ as x -> Obj.magic x
    | Concat _ as x -> Obj.magic x
    | Dep _ as x -> (x :> any t)
    | Deps _ as x -> (x :> any t)
    | Path _ as x -> (x :> any t)
    | Paths _ as x -> (x :> any t)
    | Hidden_deps _ as x -> (x :> any t)
    | Dyn _ as x -> (x :> any t)
    | Expand _ as x -> (x :> any t)
  ;;
end

open Args0

let rec expand_build
  : type a.
    dir:Path.t
    -> targets:Targets.t ref
    -> a t
    -> string Appendable_list.t Action_builder.t
  =
  fun ~dir ~targets t ->
  match t with
  | A s -> Appendable_list.singleton s |> Action_builder.return
  | As l -> Appendable_list.of_list l |> Action_builder.return
  | Dep fn ->
    Action_builder.path fn
    |> Action_builder.map ~f:(fun () ->
      Appendable_list.singleton (Path.reach fn ~from:dir))
  | Path fn ->
    Path.reach fn ~from:dir |> Appendable_list.singleton |> Action_builder.return
  | Deps fns ->
    Action_builder.paths fns
    |> Action_builder.map ~f:(fun () ->
      Appendable_list.of_list @@ List.map fns ~f:(Path.reach ~from:dir))
  | Paths fns ->
    List.map fns ~f:(Path.reach ~from:dir)
    |> Appendable_list.of_list
    |> Action_builder.return
  | S ts -> expand_list_build ~dir ~targets ts
  | Concat (sep, ts) ->
    (* Keep grouped target unions instead of copying a growing outer set. *)
    let nested_targets = ref Targets.empty in
    let build =
      expand_list_build ~dir ~targets:nested_targets ts
      |> Action_builder.map ~f:(fun x ->
        Appendable_list.to_list x |> String.concat ~sep |> Appendable_list.singleton)
    in
    targets := Targets.combine !nested_targets !targets;
    build
  | Target fn ->
    let build =
      Path.build fn
      |> Path.reach ~from:dir
      |> Appendable_list.singleton
      |> Action_builder.return
    in
    targets := Targets.combine (Targets.File.create fn) !targets;
    build
  | Dyn dyn ->
    (* Dynamic arguments get their own collector at evaluation time. *)
    Action_builder.bind dyn ~f:(expand_no_targets ~dir)
  | Hidden_deps deps ->
    Action_builder.deps deps |> Action_builder.map ~f:(fun () -> Appendable_list.empty)
  | Hidden_targets fns ->
    let build = Action_builder.return Appendable_list.empty in
    targets
    := Targets.combine (Targets.Files.create (Path.Build.Set.of_list fns)) !targets;
    build
  | Expand f -> f ~dir

and expand_list_build
  : type a.
    dir:Path.t
    -> targets:Targets.t ref
    -> a t list
    -> string Appendable_list.t Action_builder.t
  =
  fun ~dir ~targets ts ->
  match ts with
  | [] -> Appendable_list.empty |> Action_builder.return
  | ts ->
    let flush static builds =
      match static with
      | [] -> builds
      | _ :: _ ->
        let build = Action_builder.return (Appendable_list.concat (List.rev static)) in
        build :: builds
    in
    let rec expand_all ts stack static builds =
      match ts with
      | [] ->
        (match stack with
         | ts :: stack -> expand_all ts stack static builds
         | [] -> Action_builder.all (List.rev (flush static builds)))
      | S nested :: ts -> expand_all nested (ts :: stack) static builds
      | A string :: ts ->
        let static = Appendable_list.singleton string :: static in
        expand_all ts stack static builds
      | As strings :: ts ->
        let static = Appendable_list.of_list strings :: static in
        expand_all ts stack static builds
      | Path fn :: ts ->
        let string = Path.reach fn ~from:dir in
        let static = Appendable_list.singleton string :: static in
        expand_all ts stack static builds
      | Paths fns :: ts ->
        let strings = List.map fns ~f:(Path.reach ~from:dir) in
        let static = Appendable_list.of_list strings :: static in
        expand_all ts stack static builds
      | t :: ts ->
        let builds = flush static builds in
        let build = expand_build ~dir ~targets t in
        expand_all ts stack [] (build :: builds)
    in
    expand_all ts [] [] [] |> Action_builder.map ~f:Appendable_list.concat

and expand_no_targets ~dir (t : without_targets t) =
  let targets = ref Targets.empty in
  let build = expand_build ~dir ~targets t in
  assert (Targets.is_empty !targets);
  build
;;

let expand ~dir t =
  let targets = ref Targets.empty in
  let build = expand_build ~dir ~targets t in
  Action_builder.with_targets build ~targets:!targets
;;

let expand_list ~dir ts =
  let targets = ref Targets.empty in
  let build = expand_list_build ~dir ~targets ts in
  Action_builder.with_targets build ~targets:!targets
;;

let expand_list_no_targets ~dir (ts : without_targets t list) =
  let targets = ref Targets.empty in
  let build = expand_list_build ~dir ~targets ts in
  assert (Targets.is_empty !targets);
  build
;;

let dep_prog = function
  | Ok p -> Action_builder.path p
  | Error _ -> Action_builder.return ()
;;

let run_dyn_prog ~dir ?sandbox ?stdout_to ?env ?(forbid_action_runner = false) prog args =
  Action_builder.With_targets.add
    ~file_targets:(Option.to_list stdout_to)
    (let open Action_builder.With_targets.O in
     let+ prog =
       Action_builder.with_no_targets
       @@
       let open Action_builder.O in
       let* prog = prog in
       let+ () = dep_prog prog in
       prog
     and+ args = expand_list ~dir args
     and+ env =
       Action_builder.with_no_targets
         (match env with
          | Some env -> Action_builder.map env ~f:Option.some
          | None -> Action_builder.return None)
     in
     let action =
       Action.Run { prog; args; can_run_in_action_runner = not forbid_action_runner }
     in
     let action =
       match stdout_to with
       | None -> action
       | Some path -> Action.with_stdout_to path action
     in
     Action.chdir dir action |> Action.Full.make ?sandbox ?env)
;;

let run ~dir ?sandbox ?stdout_to ?env ?forbid_action_runner prog args =
  run_dyn_prog
    ~dir
    ?sandbox
    ?stdout_to
    ?env
    ?forbid_action_runner
    (Action_builder.return prog)
    args
;;

let run' ?sandbox ?env ~dir ?(forbid_action_runner = false) prog args =
  let open Action_builder.O in
  let+ () = dep_prog prog
  and+ args = expand_list_no_targets ~dir args
  and+ env =
    match env with
    | Some env -> Action_builder.map env ~f:Option.some
    | None -> Action_builder.return None
  in
  Action.Run { prog; args; can_run_in_action_runner = not forbid_action_runner }
  |> Action.chdir dir
  |> Action.Full.make ?sandbox ?env
;;

let quote_args =
  let rec loop quote = function
    | [] -> []
    | arg :: args -> quote :: arg :: loop quote args
  in
  fun quote args -> As (loop quote args)
;;

module Args = struct
  include Args0

  let memo t =
    let memo =
      Action_builder.create_memo
        "Command.Args.memo"
        ~input:(module Path)
        (fun dir -> expand_no_targets ~dir t)
    in
    Expand (fun ~dir -> Action_builder.exec_memo memo dir)
  ;;
end

module Ml_kind = struct
  let flag t = Ml_kind.choose ~impl:(Args.A "-impl") ~intf:(A "-intf") t
  let ppx_driver_flag t = Ml_kind.choose ~impl:(Args.A "--impl") ~intf:(A "--intf") t
end
