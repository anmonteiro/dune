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
  Exn.protect
    ~finally:(fun () -> Memo.set_incremental true)
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
