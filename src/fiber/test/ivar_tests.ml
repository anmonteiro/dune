open Fiber.O
open Dyn
open Common

let%expect_test "execution context of ivars" =
  (* The point of this test it show that the execution context is restored when
     a fiber that's blocked on an ivar is resumed. This means that fiber local
     variables are visible for example*)
  let open Fiber.O in
  let ivar = Fiber.Ivar.create () in
  let run_when_filled () =
    let var = Fiber.Var.create None in
    Fiber.Var.set var (Some 42) (fun () ->
      let peek = Fiber.Ivar.peek ivar in
      assert (peek = None);
      let* () = Fiber.Ivar.read ivar in
      let+ value = Fiber.Var.get_exn var in
      Printf.printf "var value %d\n" value)
  in
  let run = Fiber.fork_and_join_unit run_when_filled (Fiber.Ivar.fill ivar) in
  test unit run;
  [%expect
    {|
    var value 42
    () |}]
;;

let%expect_test "fill returns a fiber that executes before waiters are awoken" =
  let ivar = Fiber.Ivar.create () in
  let open Fiber.O in
  let waiters () =
    let waiter n () =
      let+ () = Fiber.Ivar.read ivar in
      Printf.printf "waiter %d resumed\n" n
    in
    Fiber.fork_and_join_unit (waiter 1) (waiter 2)
  in
  let run () =
    let* () = Scheduler.yield () in
    let* value =
      let+ () = Fiber.Ivar.fill ivar () in
      Printf.printf "ivar filled\n";
      42
    in
    assert (value = 42);
    Fiber.return ()
  in
  test unit (Fiber.fork_and_join_unit waiters run);
  [%expect
    {|
    ivar filled
    waiter 1 resumed
    waiter 2 resumed
    () |}]
;;

let%expect_test "stack usage with consecutive Ivar.fill" =
  let stack_size () = (Gc.stat ()).stack_size in
  let rec loop ~mapped acc prev n =
    if n = 0
    then acc, prev
    else (
      let next = Fiber.Ivar.create () in
      let fiber =
        let* () = Fiber.Ivar.read prev in
        let fill = Fiber.Ivar.fill next () in
        if mapped
        then
          let+ () = fill in
          ()
        else fill
      in
      loop ~mapped (fiber :: acc) next (n - 1))
  in
  let stack_usage ~mapped n =
    let first = Fiber.Ivar.create () in
    let fibers, final = loop ~mapped [] first n in
    let* () = Fiber.parallel_iter fibers ~f:Fun.id
    and* n =
      let init = stack_size () in
      let+ () = Fiber.Ivar.read final in
      stack_size () - init
    and* () = Fiber.Ivar.fill first () in
    Fiber.return n
  in
  let check mapped =
    let n0 = Scheduler.run (stack_usage ~mapped 0) in
    let n1000 = Scheduler.run (stack_usage ~mapped 1000) in
    printf "%s: " (if mapped then "mapped" else "bare");
    if n0 = n1000
    then printf "[PASS]\n"
    else
      printf
        "[FAIL]\nStack usage for n = 0:    %d words\nStack usage for n = 1000: %d words\n"
        n0
        n1000
  in
  check false;
  check true;
  [%expect
    {|
    bare: [PASS]
    mapped: [PASS] |}]
;;

let%expect_test "mapped fill errors preserve publication and contexts" =
  let var = Fiber.Var.create "outer" in
  let ivar = Fiber.Ivar.create () in
  let calls = ref 0 in
  let filled =
    let+ () = Fiber.Ivar.fill ivar 7 in
    incr calls;
    assert (Fiber.Ivar.peek ivar = Some 7);
    print_endline "published";
    raise Exit
  in
  assert (!calls = 0 && Fiber.Ivar.peek ivar = None);
  let reader name () =
    Fiber.Var.set var name (fun () ->
      let* value = Fiber.Ivar.read ivar in
      let+ context = Fiber.Var.get var in
      Printf.printf "reader %s: %d\n" context value)
  in
  let writer () =
    Fiber.Var.set var "handler" (fun () ->
      Fiber.with_error_handler
        (fun () ->
           Fiber.Var.set var "writer" (fun () ->
             let* () = Scheduler.yield () in
             filled))
        ~on_error:(fun exn ->
          let* context = Fiber.Var.get var in
          Printf.printf "handler context: %s\n" context;
          Stdune.Exn_with_backtrace.reraise exn))
  in
  test
    (backtrace_result unit)
    (Fiber.collect_errors (fun () ->
       Fiber.fork_and_join_unit
         (fun () -> Fiber.fork_and_join_unit (reader "first") (reader "second"))
         writer));
  assert (!calls = 1 && Fiber.Ivar.peek ivar = Some 7);
  assert (Scheduler.run (Fiber.Var.get var) = "outer");
  [%expect
    {|
    published
    handler context: handler
    reader first: 7
    reader second: 7
    Error [ { exn = "Stdlib.Exit"; backtrace = "" } ]
    |}]
;;
