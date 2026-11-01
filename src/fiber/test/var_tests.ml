open Fiber.O
open Dyn
open Common

let%expect_test "fiber vars are preserved across yields" =
  let var = Fiber.Var.create None in
  let fiber th () =
    let* v = Fiber.Var.get var in
    assert (v = None);
    Fiber.Var.set var (Some th) (fun () ->
      let* v = Fiber.Var.get var in
      assert (v = Some th);
      let* () = Scheduler.yield () in
      let+ v = Fiber.Var.get var in
      assert (v = Some th))
  in
  let run = Fiber.fork_and_join_unit (fiber 1) (fiber 2) in
  test unit run;
  [%expect
    {|
    () |}]
;;

let%expect_test "Var get_apply variants read the var and thread arguments" =
  let var = Fiber.Var.create 0 in
  let run =
    Fiber.Var.set var 10 (fun () ->
      let* sum = Fiber.Var.get_apply var (fun value x -> Fiber.return (value + x)) 5 in
      let* product = Fiber.Var.get_apply_map var (fun value x -> value * x) 5 in
      let+ sum3 = Fiber.Var.get_apply_map2 var (fun value x y -> value + x + y) 5 7 in
      printf "get_apply = %d, get_apply_map = %d, get_apply_map2 = %d\n" sum product sum3)
  in
  test unit run;
  [%expect
    {|
    get_apply = 15, get_apply_map = 50, get_apply_map2 = 22
    () |}]
;;

let%expect_test "Var.set_apply and update_apply scope the change and thread the argument" =
  let var = Fiber.Var.create 0 in
  let run =
    Fiber.Var.set var 1 (fun () ->
      let* set_inner, arg =
        Fiber.Var.set_apply
          var
          2
          (fun x ->
             let+ value = Fiber.Var.get var in
             value, x)
          100
      in
      let* set_outer = Fiber.Var.get var in
      let* update_inner =
        Fiber.Var.update_apply
          var
          ~f:(fun v -> v + 10)
          (fun x ->
             let+ value = Fiber.Var.get var in
             value + x)
          5
      in
      let+ update_outer = Fiber.Var.get var in
      printf
        "set_apply: inner=%d arg=%d outer=%d; update_apply: inner=%d outer=%d\n"
        set_inner
        arg
        set_outer
        update_inner
        update_outer)
  in
  test unit run;
  [%expect
    {|
    set_apply: inner=2 arg=100 outer=1; update_apply: inner=16 outer=1
    () |}]
;;

let%expect_test "Var map callbacks read their execution context" =
  let var = Fiber.Var.create 1 in
  let calls = ref 0 in
  let read =
    Fiber.Var.get_apply_map
      var
      (fun value x ->
         incr calls;
         value + x)
      2
  in
  let read2 =
    Fiber.Var.get_apply_map2
      var
      (fun value x y ->
         incr calls;
         value + x + y)
      2
      3
  in
  assert (!calls = 0);
  let run =
    let* initial = read in
    let* first, second =
      Fiber.Var.set var 10 (fun () ->
        let* () = Scheduler.yield () in
        let* first = read in
        let+ second = read2 in
        first, second)
    in
    let+ final = read2 in
    printf "initial=%d inner=%d,%d final=%d calls=%d\n" initial first second final !calls
  in
  test unit run;
  [%expect
    {|
    initial=3 inner=12,15 final=6 calls=4
    () |}]
;;

let%expect_test "Var map callbacks preserve exception handling" =
  let var = Fiber.Var.create 1 in
  let check fiber =
    test
      (backtrace_result unit)
      (Fiber.collect_errors (fun () -> Fiber.Var.set var 10 (fun () -> fiber)))
  in
  check
    (Fiber.Var.get_apply_map
       var
       (fun value x ->
          assert (value = 10 && x = 2);
          raise Exit)
       2);
  check
    (Fiber.Var.get_apply_map2
       var
       (fun value x y ->
          assert (value = 10 && x = 2 && y = 3);
          failwith "map2")
       2
       3);
  check
    (let+ value = Fiber.Var.get_apply_map2 var (fun value x y -> value + x + y) 2 3 in
     assert (value = 15);
     failwith "downstream");
  test int (Fiber.Var.get var);
  [%expect
    {|
    Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
    Error [ { exn = "Failure(\"map2\")"; backtrace = "" } ]
    Error [ { exn = "Failure(\"downstream\")"; backtrace = "" } ]
    1 |}]
;;

let%expect_test "unchanged Var sets preserve scopes, suspension, and reuse" =
  let check use_apply =
    let default = ref 0 in
    let shared = ref 1 in
    let changed = ref 2 in
    let var = Fiber.Var.create default in
    let calls = ref 0 in
    let set value f =
      if use_apply then Fiber.Var.set_apply var value f () else Fiber.Var.set var value f
    in
    let read expected =
      let+ actual = Fiber.Var.get var in
      assert (actual == expected)
    in
    let reusable =
      set shared (fun () ->
        incr calls;
        let* () = read shared in
        let* () = Scheduler.yield () in
        let* () = set changed (fun () -> read changed) in
        read shared)
    in
    let run =
      let* () = set default (fun () -> read default) in
      let* () = reusable in
      let* () = read default in
      let* () =
        set shared (fun () ->
          let* () = reusable in
          let gate = Fiber.Ivar.create () in
          Fiber.fork_and_join_unit
            (fun () ->
               set shared (fun () ->
                 let* () = Fiber.Ivar.read gate in
                 reusable))
            (fun () ->
               set changed (fun () ->
                 let* () = Scheduler.yield () in
                 let* () = read changed in
                 Fiber.Ivar.fill gate ())))
      in
      read default
    in
    assert (!calls = 0);
    Scheduler.run run;
    Scheduler.run run;
    printf "%s: calls=%d\n" (if use_apply then "set_apply" else "set") !calls
  in
  check false;
  check true;
  [%expect
    {|
    set: calls=6
    set_apply: calls=6 |}]
;;

let%expect_test "unchanged Var sets preserve immediate and deferred errors" =
  let check use_apply =
    let default = ref 0 in
    let shared = ref 1 in
    let var = Fiber.Var.create default in
    let calls = ref 0 in
    let set value f =
      if use_apply then Fiber.Var.set_apply var value f () else Fiber.Var.set var value f
    in
    let protected delayed =
      set shared (fun () ->
        let* result =
          Fiber.collect_errors (fun () ->
            Fiber.with_error_handler
              ~on_error:(fun exn ->
                let* value = Fiber.Var.get var in
                assert (value == shared);
                Fiber.reraise_all [ exn ])
              (fun () ->
                 set shared (fun () ->
                   incr calls;
                   if delayed
                   then
                     let* () = Scheduler.yield () in
                     failwith "deferred"
                   else raise Exit)))
        in
        let+ value = Fiber.Var.get var in
        assert (value == shared);
        result)
    in
    let immediate = protected false in
    let deferred = protected true in
    assert (!calls = 0);
    test (backtrace_result unit) immediate;
    test (backtrace_result unit) deferred;
    let restored = Scheduler.run (Fiber.Var.get var) in
    assert (restored == default && !calls = 2)
  in
  check false;
  check true;
  [%expect
    {|
    Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
    Error [ { exn = "Failure(\"deferred\")"; backtrace = "" } ]
    Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
    Error [ { exn = "Failure(\"deferred\")"; backtrace = "" } ] |}]
;;
