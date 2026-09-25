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

let%expect_test "request-style sequences agree with all_unit" =
  let module Dep = Dune_engine.Dep in
  let seq tasks =
    let open Action_builder.O in
    List.fold_left tasks ~init:(Action_builder.return ()) ~f:( >>> )
  in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let original_incremental = Memo.is_incremental () in
  Exn.protect
    ~finally:(fun () -> Memo.set_incremental original_incremental)
    ~f:(fun () ->
      Memo.set_incremental true;
      List.iter
        [ "fold", seq; "all_unit", Action_builder.all_unit ]
        ~f:(fun (name, all) ->
          let env = Dep.env (Env.Var.of_string "REQUEST_PARALLEL_TEST") in
          List.iter
            [ []; [ Dep.universe ]; [ Dep.universe; env; Dep.universe ] ]
            ~f:(fun deps ->
              let fact_calls = ref 0 in
              let request =
                all
                  (List.map deps ~f:(fun dep ->
                     Action_builder.record () (Dep.Set.singleton dep) ~f:(fun _ ->
                       incr fact_calls;
                       Memo.return Dep.Fact.nothing)))
              in
              let (), actual = run (Action_builder.evaluate_and_collect_deps request) in
              assert (Dep.Set.equal actual (Dep.Set.of_list deps) && !fact_calls = 0);
              let (), actual = run (Action_builder.evaluate_and_collect_facts request) in
              let expected =
                Dep.Facts.union_all
                  (List.map deps ~f:(fun dep -> Dep.Facts.singleton dep Dep.Fact.nothing))
              in
              assert (Dep.Facts.equal actual expected && !fact_calls = List.length deps));
          let exception Failed in
          let release = Fiber.Ivar.create () in
          let should_fail = Memo.Var.create ~name:"request failure" true in
          let calls = Array.make 3 0 in
          let events = ref [] in
          let log event = events := event :: !events in
          let a =
            Memo.lazy_node ~name:"request a" (fun () ->
              calls.(0) <- calls.(0) + 1;
              log "A+";
              let open Memo.O in
              let+ () = Memo.of_reproducible_fiber (Fiber.Ivar.read release) in
              log "A-")
          in
          let b =
            Memo.lazy_node ~name:"request b" (fun () ->
              calls.(1) <- calls.(1) + 1;
              log "B";
              let open Memo.O in
              let+ fail = Memo.Var.read should_fail in
              if fail then raise Failed)
          in
          let c =
            Memo.lazy_node ~name:"request c" (fun () ->
              calls.(2) <- calls.(2) + 1;
              log "C+";
              let open Memo.O in
              let+ () = Memo.of_reproducible_fiber (Fiber.Ivar.fill release ()) in
              log "C-")
          in
          let request =
            all
              (List.map [ a; b; c; b ] ~f:(fun node ->
                 Action_builder.of_memo (Memo.Node.read node)))
          in
          let attempt () =
            Fiber.run
              (Fiber.collect_errors (fun () ->
                 Memo.run (Action_builder.evaluate_and_collect_facts request)))
              ~iter:(fun () -> failwith "request siblings did not drain")
          in
          assert (
            match attempt () with
            | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
              (match Memo.Error.get error with
               | Failed -> true
               | _ -> false)
            | _ -> false);
          assert (
            List.equal String.equal (List.rev !events) [ "A+"; "B"; "C+"; "C-"; "A-" ]);
          assert (Array.to_list calls = [ 1; 1; 1 ]);
          events := [];
          Memo.reset (Memo.Var.set should_fail false);
          assert (
            match attempt () with
            | Ok ((), facts) -> Dep.Facts.equal facts Dep.Facts.empty
            | Error _ -> false);
          assert (!events = [ "B" ] && Array.to_list calls = [ 1; 2; 1 ]);
          printfn "%s: dependencies, failures, suspension and recovery agree" name));
  [%expect
    {|
    fold: dependencies, failures, suspension and recovery agree
    all_unit: dependencies, failures, suspension and recovery agree
    |}]
;;

let%expect_test "explicit command targets preserve actions and dependencies" =
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
  let explicit =
    Command.run'
      ~dir:(Path.build dir)
      ~env
      ~sandbox
      ~forbid_action_runner:true
      (Ok compiler)
      [ common; A "-bin-annot"; A "-o"; Path (Path.build destination) ]
    |> Action_builder.with_targets ~targets:(Targets.Files.create_in_dir ~dir filenames)
  in
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
    same action and execution properties: true
    same dependencies, excluding outputs: true
    same complete target set: true
    |}]
;;

let%expect_test "nested targets and delayed arguments stay independent" =
  let module Dep = Dune_engine.Dep in
  let open Command.Args in
  let dir = Path.Build.relative Path.Build.root "default/nested-command-targets" in
  let target name = Path.Build.relative dir name in
  let path name = Path.build (target name) in
  let source = path "input" in
  let hidden = path "hidden-input" in
  let calls = ref 0 in
  let expansions = ref 0 in
  let dynamic : without_targets t =
    Dyn
      (Action_builder.delayed (fun () ->
         incr calls;
         S
           [ A "dynamic"
           ; Dyn
               (Action_builder.return
                  (Concat
                     ( ":"
                     , [ Dep source
                       ; Hidden_deps (Dep.Set.of_files [ hidden ])
                       ; S [ Paths [ path "argument" ]; A "z" ]
                       ] )))
           ]))
  in
  let first =
    Command.expand
      ~dir:(Path.build dir)
      (S
         [ A "before"
         ; S
             [ As [ "literals" ]
             ; Path (path "argument")
             ; S [ Paths [ path "argument-2"; path "argument-3" ]; As []; Paths [] ]
             ]
         ; Expand
             (fun ~dir:_ ->
               incr expansions;
               Action_builder.return (Appendable_list.singleton "expanded"))
         ; Concat
             ( ":"
             , [ Target (target "a")
               ; S
                   [ Hidden_targets [ target "b"; target "b" ]
                   ; Concat ("/", [ Target (target "c"); Target (target "a") ])
                   ]
               ] )
         ; as_any dynamic
         ; A "after"
         ])
  in
  let second =
    Command.expand ~dir:(Path.build dir) (S [ Target (target "d"); as_any dynamic ])
  in
  let plain = Command.expand_no_targets ~dir:(Path.build dir) dynamic in
  printfn "expansions during construction: %d" !expansions;
  printfn "dynamic evaluations during construction: %d" !calls;
  let evaluate build =
    let args, deps =
      Fiber.run
        (Memo.run (Action_builder.evaluate_and_collect_deps build))
        ~iter:(fun () -> failwith "unexpected suspension")
    in
    Appendable_list.to_list args, deps
  in
  let args, first_deps = evaluate first.build in
  List.iter args ~f:(printfn "%S");
  let plain_args, plain_deps = evaluate plain in
  let second_args, second_deps = evaluate second.build in
  let targets_are targets names =
    match Targets.validate targets with
    | Valid actual ->
      Path.Build.equal actual.root dir
      && Filename.Set.equal
           actual.files
           (List.map names ~f:Filename.of_string_exn |> Filename.Set.of_list)
      && Filename.Set.is_empty actual.dirs
    | _ -> false
  in
  printfn
    "independent target sets: %b"
    (targets_are first.targets [ "a"; "b"; "c" ] && targets_are second.targets [ "d" ]);
  printfn
    "plain arguments and dependencies preserved: %b"
    (List.equal String.equal plain_args [ "dynamic"; "input:argument:z" ]
     && List.equal String.equal second_args ("d" :: plain_args)
     && Dep.Set.equal plain_deps (Dep.Set.of_files [ source; hidden ])
     && Dep.Set.equal first_deps plain_deps
     && Dep.Set.equal second_deps plain_deps);
  printfn "dynamic evaluations after all three builds: %d" !calls;
  [%expect
    {|
    expansions during construction: 1
    dynamic evaluations during construction: 0
    "before"
    "literals"
    "argument"
    "argument-2"
    "argument-3"
    "expanded"
    "a:c/a"
    "dynamic"
    "input:argument:z"
    "after"
    independent target sets: true
    plain arguments and dependencies preserved: true
    dynamic evaluations after all three builds: 3
    |}]
;;

let%expect_test "nested empty groups preserve argument boundaries" =
  let open Command.Args in
  let empty = S [ S []; S [ S []; S [] ] ] in
  printfn "nested groups stay empty: %b" (List.is_empty (expand empty));
  printfn
    "empty concat stays one argument: %b"
    (List.equal String.equal (expand (S [ Concat (":", [ empty ]) ])) [ "" ]);
  [%expect
    {|
    nested groups stay empty: true
    empty concat stays one argument: true
    |}]
;;

let%expect_test "empty and singleton arguments preserve dynamic error context" =
  let open Command.Args in
  let original_incremental = Memo.is_incremental () in
  Exn.protect
    ~finally:(fun () -> Memo.set_incremental original_incremental)
    ~f:(fun () ->
      List.iter [ false; true ] ~f:(fun incremental ->
        Memo.set_incremental incremental;
        let empty_arguments : (string * without_targets t * bool) list =
          [ "As", As [], false
          ; "Paths", Paths [], false
          ; "literal dyn", As [], true
          ; "sole Dyn", S [], false
          ; "sole literal dyn", S [], true
          ]
        in
        List.iter empty_arguments ~f:(fun (name, empty, literal) ->
          let should_fail = Memo.Var.create ~name:"command failure" true in
          let dynamic =
            Memo.create
              "command dynamic"
              ~input:(module Unit)
              (fun () ->
                 let open Memo.O in
                 let+ should_fail = Memo.Var.read should_fail in
                 if should_fail then failwith "dynamic failure";
                 [ "recovered" ])
          in
          let dynamic = Action_builder.of_memo (Memo.exec dynamic ()) in
          let dynamic =
            if literal
            then dyn dynamic
            else Dyn (Action_builder.map dynamic ~f:(fun args -> As args))
          in
          let build =
            Command.expand_no_targets ~dir:Path.root (S [ empty; S []; dynamic ])
          in
          let consumer =
            Memo.create
              "command consumer"
              ~input:(module Unit)
              (fun () -> Action_builder.evaluate_and_collect_deps build)
          in
          let run () =
            Fiber.run
              (Fiber.collect_errors (fun () -> Memo.run (Memo.exec consumer ())))
              ~iter:(fun () -> failwith "unexpected suspension")
          in
          let context_kept =
            match run () with
            | Error [ { Exn_with_backtrace.exn = Memo.Error.E error; _ } ] ->
              (match Memo.Error.get error with
               | Failure message -> String.equal message "dynamic failure"
               | _ -> false)
              && List.equal
                   (Option.equal String.equal)
                   (List.map (Memo.Error.stack error) ~f:Memo.Stack_frame.name)
                   [ Some "command dynamic"; Some "command consumer" ]
            | _ -> false
          in
          printfn "%s, incremental %b: error context %b" name incremental context_kept;
          if incremental
          then (
            Memo.reset (Memo.Var.set should_fail false);
            printfn
              "%s: failed dependency invalidated %b"
              name
              (match run () with
               | Ok (args, deps) ->
                 List.equal String.equal (Appendable_list.to_list args) [ "recovered" ]
                 && Dune_engine.Dep.Set.is_empty deps
               | Error _ -> false)))));
  [%expect
    {|
    As, incremental false: error context true
    Paths, incremental false: error context true
    literal dyn, incremental false: error context true
    sole Dyn, incremental false: error context true
    sole literal dyn, incremental false: error context true
    As, incremental true: error context true
    As: failed dependency invalidated true
    Paths, incremental true: error context true
    Paths: failed dependency invalidated true
    literal dyn, incremental true: error context true
    literal dyn: failed dependency invalidated true
    sole Dyn, incremental true: error context true
    sole Dyn: failed dependency invalidated true
    sole literal dyn, incremental true: error context true
    sole literal dyn: failed dependency invalidated true
    |}]
;;

let%expect_test "large static runs support downstream argument traversal" =
  let strings = List.init 20_000 ~f:Int.to_string in
  let build =
    Command.expand_no_targets
      ~dir:Path.root
      (S (List.map strings ~f:(fun string -> Command.Args.A string)))
  in
  let args, _deps =
    Fiber.run
      (Memo.run (Action_builder.evaluate_and_collect_deps build))
      ~iter:(fun () -> failwith "unexpected suspension")
  in
  let mapped = Appendable_list.map args ~f:(fun arg -> "mapped-" ^ arg) in
  printfn
    "mapped arguments stay ordered: %b"
    (List.equal
       String.equal
       (Appendable_list.to_list mapped)
       (List.map strings ~f:(fun arg -> "mapped-" ^ arg)));
  let visited = ref 0 in
  let found =
    Appendable_list.exists mapped ~f:(fun arg ->
      incr visited;
      String.equal arg "mapped-19999")
  in
  printfn "exists visits the entire run: %b" (found && !visited = 20_000);
  [%expect
    {|
    mapped arguments stay ordered: true
    exists visits the entire run: true
    |}]
;;

let%expect_test "literal dynamic arguments match the general dynamic oracle" =
  let module Dep = Dune_engine.Dep in
  let open Command.Args in
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let dir = Path.Build.relative Path.Build.root "default/literal-dynamic-args" in
  let target name = Path.Build.relative dir name in
  let targets_are targets name =
    match Targets.validate targets with
    | Valid targets ->
      Path.Build.equal targets.root dir
      && Filename.Set.equal
           targets.files
           (Filename.Set.singleton (Filename.of_string_exn name))
      && Filename.Set.is_empty targets.dirs
    | _ -> false
  in
  let env = Dep.env (Env.Var.of_string "LITERAL_DYNAMIC_ARGS") in
  let deps = Dep.Set.of_list [ env; Dep.universe ] in
  let facts =
    Dep.Facts.union
      (Dep.Facts.singleton env Dep.Fact.nothing)
      (Dep.Facts.singleton Dep.universe Dep.Fact.nothing)
  in
  List.iter
    [ []; [ "one" ]; [ "two"; ""; "words with spaces" ] ]
    ~f:(fun strings ->
      let calls = ref 0 in
      let args =
        Action_builder.map (Action_builder.deps deps) ~f:(fun () ->
          incr calls;
          strings)
      in
      let dynamic = Command.Args.dyn args in
      let first =
        Command.expand ~dir:(Path.build dir) (S [ Target (target "first"); dynamic ])
      in
      let second =
        Command.expand ~dir:Path.root (S [ Hidden_targets [ target "second" ]; dynamic ])
      in
      let legacy =
        Command.expand_no_targets
          ~dir:Path.root
          (Dyn (Action_builder.map args ~f:(fun args -> As args)))
      in
      let delayed = !calls = 0 in
      let first_args, first_deps =
        run (Action_builder.evaluate_and_collect_deps first.build)
      in
      let old_args, old_deps = run (Action_builder.evaluate_and_collect_deps legacy) in
      let second_args, second_facts =
        run (Action_builder.evaluate_and_collect_facts second.build)
      in
      let old_eager_args, old_facts =
        run (Action_builder.evaluate_and_collect_facts legacy)
      in
      let same_args args =
        List.equal String.equal (Appendable_list.to_list args) strings
      in
      printfn
        "%d literals: delayed %b; lazy %b; eager %b; targets %b; evaluations %d"
        (List.length strings)
        delayed
        (List.equal String.equal (Appendable_list.to_list first_args) ("first" :: strings)
         && same_args old_args
         && Dep.Set.equal first_deps deps
         && Dep.Set.equal first_deps old_deps)
        (same_args second_args
         && same_args old_eager_args
         && Dep.Facts.equal second_facts facts
         && Dep.Facts.equal second_facts old_facts)
        (targets_are first.targets "first" && targets_are second.targets "second")
        !calls);
  [%expect
    {|
    0 literals: delayed true; lazy true; eager true; targets true; evaluations 4
    1 literals: delayed true; lazy true; eager true; targets true; evaluations 4
    3 literals: delayed true; lazy true; eager true; targets true; evaluations 4
    |}]
;;
