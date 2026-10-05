open Stdune
open Dune_rules

let () = Dune_tests_common.init ()

let expand args =
  let { Action_builder.With_targets.build; targets = _ } =
    Command.expand ~dir:Path.root args
  in
  let result, _deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps build))
      ~iter:(fun () -> assert false)
  in
  Appendable_list.to_list result
;;

let%expect_test "nested argument groups preserve order" =
  let open Command.Args in
  let args =
    S
      [ A "a"
      ; S []
      ; S [ As [ "b"; "c" ]; S [ A "d"; S []; S [ As [ "e"; "f" ] ] ] ]
      ; A "g"
      ]
  in
  List.iter (expand args) ~f:(printfn "%S");
  [%expect
    {|
    "a"
    "b"
    "c"
    "d"
    "e"
    "f"
    "g"
    |}]
;;

let%expect_test "static dependencies preserve lazy sets and eager facts" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let legacy deps = Action_builder.dyn_memo_deps (Memo.return (deps, ())) in
  let env = Dep.env (Env.Var.of_string "STATIC_DEPS_TEST") in
  let missing =
    Dep.file
      (Path.build (Path.Build.relative Path.Build.root "default/unbuilt-static-dep"))
  in
  let deps = Dep.Set.of_list [ missing; env; Dep.universe ] in
  (* No build configuration is installed, and lazy evaluation must not try to
     build the missing target. *)
  let static = Action_builder.deps deps in
  print_endline "constructed without a build configuration";
  let (), actual = run (Action_builder.evaluate_and_collect_deps static) in
  let (), previous = run (Action_builder.evaluate_and_collect_deps (legacy deps)) in
  printfn
    "lazy dependencies kept: %b"
    (Dep.Set.equal actual deps && Dep.Set.equal actual previous);
  let check_facts deps expected =
    let (), actual =
      run (Action_builder.evaluate_and_collect_facts (Action_builder.deps deps))
    in
    let (), previous = run (Action_builder.evaluate_and_collect_facts (legacy deps)) in
    Dep.Facts.equal actual expected && Dep.Facts.equal actual previous
  in
  printfn "empty eager facts kept: %b" (check_facts Dep.Set.empty Dep.Facts.empty);
  let expected =
    Dep.Facts.union
      (Dep.Facts.singleton env Dep.Fact.nothing)
      (Dep.Facts.singleton Dep.universe Dep.Fact.nothing)
  in
  printfn
    "nonempty eager facts kept: %b"
    (check_facts (Dep.Set.of_list [ env; Dep.universe ]) expected);
  [%expect
    {|
    constructed without a build configuration
    lazy dependencies kept: true
    empty eager facts kept: true
    nonempty eager facts kept: true
    |}]
;;

let%expect_test "singleton list maps preserve evaluation and dependencies" =
  let module Dep = Dune_engine.Dep in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let first, second = ref 1, ref 2 in
  let deps = Dep.Set.singleton Dep.universe in
  let facts = Dep.Facts.singleton Dep.universe Dep.Fact.nothing in
  List.iter
    [ []; [ first ]; [ first; second; first ] ]
    ~f:(fun values ->
      let calls = ref 0 in
      let fact_calls = ref 0 in
      let mapped =
        Action_builder.List.map values ~f:(fun value ->
          incr calls;
          Action_builder.record value deps ~f:(fun _ ->
            incr fact_calls;
            Memo.return Dep.Fact.nothing))
      in
      assert (!calls = 0 && !fact_calls = 0);
      let lazy_values, lazy_deps =
        run (Action_builder.evaluate_and_collect_deps mapped)
      in
      assert (List.equal ( == ) lazy_values values);
      assert (!calls = List.length values && !fact_calls = 0);
      assert (Dep.Set.equal lazy_deps (if values = [] then Dep.Set.empty else deps));
      let eager_values, eager_facts =
        run (Action_builder.evaluate_and_collect_facts mapped)
      in
      assert (List.equal ( == ) eager_values values);
      assert (!calls = 2 * List.length values && !fact_calls = List.length values);
      assert (Dep.Facts.equal eager_facts (if values = [] then Dep.Facts.empty else facts)));
  let release = Fiber.Ivar.create () in
  let calls = ref 0 in
  let suspended =
    Action_builder.List.map [ first ] ~f:(fun value ->
      incr calls;
      let open Fiber.O in
      Action_builder.of_memo
        (Memo.of_reproducible_fiber
           (let+ () = Fiber.Ivar.read release in
            value)))
  in
  assert (!calls = 0);
  let released = ref false in
  let values, deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps suspended))
      ~iter:(fun () ->
        assert (!calls = 1 && not !released);
        released := true;
        [ Fiber.Fill (release, ()) ])
  in
  assert (!released && !calls = 1);
  assert (List.equal ( == ) values [ first ] && Dep.Set.is_empty deps);
  let exception Failed in
  List.iter [ false; true ] ~f:(fun delayed ->
    let calls = ref 0 in
    let failing =
      Action_builder.List.map [ () ] ~f:(fun () ->
        incr calls;
        if delayed
        then Action_builder.of_memo (Memo.of_thunk (fun () -> raise Failed))
        else raise Failed)
    in
    assert (!calls = 0);
    let result =
      Fiber.run
        (Fiber.collect_errors (fun () ->
           Memo.run (Action_builder.evaluate_and_collect_deps failing)))
        ~iter:(fun () -> failwith "unexpected suspension")
    in
    assert (!calls = 1);
    assert (
      match result with
      | Error [ { Exn_with_backtrace.exn = Failed; _ } ] -> true
      | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
        (match Memo.Error.get error with
         | Failed -> true
         | _ -> false)
      | _ -> false));
  print_endline "singleton map invariants hold";
  [%expect {| singleton map invariants hold |}]
;;

let%expect_test "deferred command actions preserve targets and dependencies" =
  let module Dep = Dune_engine.Dep in
  let module Sandbox_config = Dune_engine.Sandbox_config in
  let dir = Path.Build.relative Path.Build.root "default/explicit-command-targets" in
  let path name = Path.build (Path.Build.relative dir name) in
  let compiler = path "compiler" in
  let source = path "source.ml" in
  let hidden = path "source-before-pp.ml" in
  let destination = Path.Build.relative dir "output.cmo" in
  let cmi = Path.Build.relative dir "output.cmi" in
  let cmt = Path.Build.relative dir "output.cmt" in
  let filenames =
    List.map [ "output.cmo"; "output.cmi"; "output.cmt" ] ~f:Filename.of_string_exn
    |> Filename.Set.of_list
  in
  let env = Action_builder.return (Env.of_unix [| "COMMAND_TARGETS_TEST=value" |]) in
  let sandbox = Sandbox_config.needs_sandboxing in
  let common : Command.Args.without_targets Command.Args.t =
    S
      [ Command.Args.dyn (Action_builder.return [ "-opaque" ])
      ; A "-c"
      ; Dep source
      ; Hidden_deps (Dep.Set.of_files [ hidden ])
      ]
  in
  let legacy =
    Command.run
      ~dir:(Path.build dir)
      ~env
      ~sandbox
      ~forbid_action_runner:true
      (Ok compiler)
      [ Command.Args.as_any common
      ; S [ Hidden_targets [ cmt ]; A "-bin-annot" ]
      ; A "-o"
      ; Target destination
      ; Hidden_targets [ cmi ]
      ]
  in
  let calls = ref 0 in
  let explicit =
    (let open Action_builder.O in
     let* () = Action_builder.return () in
     incr calls;
     Command.run'
       ~dir:(Path.build dir)
       ~env
       ~sandbox
       ~forbid_action_runner:true
       (Ok compiler)
       [ common; A "-bin-annot"; A "-o"; Path (Path.build destination) ])
    |> Action_builder.with_targets
         ~targets:
           (Targets.Files.create (Path.Build.Set.of_list [ destination; cmi; cmt ]))
  in
  printfn "actions constructed before evaluation: %d" !calls;
  let evaluate { Action_builder.With_targets.build; targets } =
    let action, deps =
      Fiber.run
        (Memo.run (Action_builder.evaluate_and_collect_deps build))
        ~iter:(fun () -> failwith "unexpected suspension")
    in
    action, deps, Targets.validate targets
  in
  let previous, previous_deps, previous_targets = evaluate legacy in
  let actual, actual_deps, actual_targets = evaluate explicit in
  printfn "actions constructed after evaluation: %d" !calls;
  let same_action =
    match previous.action, actual.action with
    | Dune_engine.Action.Chdir (previous_dir, Run previous), Chdir (actual_dir, Run actual)
      ->
      Path.equal previous_dir actual_dir
      && Poly.equal previous.prog actual.prog
      && List.equal
           String.equal
           (Appendable_list.to_list previous.args)
           (Appendable_list.to_list actual.args)
      && Bool.equal previous.can_run_in_action_runner actual.can_run_in_action_runner
    | _ -> false
  in
  printfn
    "same action and execution properties: %b"
    (same_action && Poly.equal previous.props actual.props);
  printfn
    "same dependencies, excluding outputs: %b"
    (Dep.Set.equal previous_deps actual_deps
     && Dep.Set.equal actual_deps (Dep.Set.of_files [ compiler; source; hidden ]));
  printfn
    "same complete target set: %b"
    (match previous_targets, actual_targets with
     | Valid previous, Valid actual ->
       Path.Build.equal previous.root dir
       && Path.Build.equal actual.root dir
       && Filename.Set.equal previous.files filenames
       && Filename.Set.equal actual.files filenames
       && Filename.Set.is_empty previous.dirs
       && Filename.Set.is_empty actual.dirs
     | _ -> false);
  [%expect
    {|
    actions constructed before evaluation: 0
    actions constructed after evaluation: 1
    same action and execution properties: true
    same dependencies, excluding outputs: true
    same complete target set: true
    |}]
;;
