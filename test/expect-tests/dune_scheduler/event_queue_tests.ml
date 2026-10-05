open Stdune
open Fiber.O
open Dune_scheduler

let () =
  (* Checkpoints bypass the event queue inside Dune. Re-exec so that this
     environment variable is absent when [Execution_env] is initialized. *)
  if Execution_env.inside_dune
  then (
    let env = Env.remove Env.initial ~var:Execution_env.Inside_dune.var in
    Proc.restore_cwd_and_execve Sys.executable_name [] ~env)
;;

let send_worker_event queue =
  let ivar = Fiber.Ivar.create () in
  Event.Queue.send_worker_tasks_completed queue [ Fill (ivar, ()) ];
  ivar
;;

let run queue fiber =
  let iterations = ref 0 in
  Fiber.run fiber ~iter:(fun () ->
    incr iterations;
    match Event.Queue.next queue with
    | Fiber_fill_ivar fill -> [ fill ]
    | Shutdown _ | Job_complete_ready -> Code_error.raise "unexpected event" []);
  !iterations
;;

let require_iterations queue fiber expected =
  let actual = run queue fiber in
  if actual <> expected
  then
    Code_error.raise
      "unexpected scheduler iterations"
      [ "expected", Dyn.int expected; "actual", Dyn.int actual ]
;;

let () =
  let queue = Event.Queue.create () in
  let checkpoint () = Event.Queue.yield_if_there_are_pending_events queue in
  require_iterations queue (checkpoint ()) 0;
  let worker = send_worker_event queue in
  require_iterations queue (Fiber.Ivar.read worker) 1;
  require_iterations
    queue
    (let* () = checkpoint () in
     checkpoint ())
    0;
  require_iterations queue (checkpoint ()) 0;
  (* A new event re-arms checkpoints and precedes the lower-priority yield. *)
  let worker = send_worker_event queue in
  require_iterations
    queue
    (let* () = checkpoint () in
     assert (Fiber.Ivar.peek worker = Some ());
     checkpoint ())
    2;
  (* Draining one event must retain the checkpoint while another remains. *)
  let first = send_worker_event queue in
  let second = send_worker_event queue in
  require_iterations queue (Fiber.Ivar.read first) 1;
  require_iterations
    queue
    (let* () = checkpoint () in
     assert (Fiber.Ivar.peek second = Some ());
     checkpoint ())
    2;
  (* Parallel checkpoint readers share a single yield, and are all resumed. *)
  let worker = send_worker_event queue in
  require_iterations
    queue
    (let* () = Fiber.fork_and_join_unit checkpoint checkpoint in
     assert (Fiber.Ivar.peek worker = Some ());
     checkpoint ())
    2;
  require_iterations queue (checkpoint ()) 0;
  (* A checkpoint still joins the shared yield after the last real event is
     consumed. It must not bypass readers that are already waiting. *)
  let worker = send_worker_event queue in
  let pending = checkpoint () in
  require_iterations queue (Fiber.Ivar.read worker) 1;
  require_iterations queue (checkpoint ()) 1;
  require_iterations queue pending 0;
  let worker = send_worker_event queue in
  let pending = checkpoint () in
  Event.Queue.send_shutdown queue Requested;
  (match Event.Queue.next queue with
   | Shutdown Requested -> ()
   | _ -> Code_error.raise "shutdown must precede the pending yield" []);
  require_iterations
    queue
    (let* () = pending in
     assert (Fiber.Ivar.peek worker = Some ());
     checkpoint ())
    2;
  if Sys.win32
  then (
    let pid = Pid.of_int_exn 1 in
    let ivar = Fiber.Ivar.create () in
    let job = { Event.pid; is_process_group_leader = false; ivar } in
    let info =
      { Proc.Process_info.pid
      ; status = WEXITED 0
      ; end_time = Time.now ()
      ; resource_usage = None
      }
    in
    Event.Queue.send_job_completed queue job info;
    require_iterations queue (Fiber.Ivar.read ivar >>| ignore) 1;
    require_iterations queue (checkpoint ()) 0)
  else (
    Event.Queue.send_job_completed_ready queue;
    (match Event.Queue.next queue with
     | Job_complete_ready -> ()
     | _ -> Code_error.raise "expected a completed job" []);
    require_iterations queue (checkpoint ()) 0)
;;
