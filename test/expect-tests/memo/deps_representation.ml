open! Stdune
open! Memo.O
open Test_helpers.Make ()

(* These tests assert the *shape* of a memoized computation's recorded dependencies - the
   series-parallel Seq/Par/Singleton/Empty structure - by driving each shape through the
   public Memo combinators and reading it back with [Memo.For_tests.get_deps_structured]. *)

let read = Memo.Node.read

let print_deps label m =
  let cell = Memo.lazy_node ~name:"top" (fun () -> m) in
  let () = run (read cell) in
  match Memo.For_tests.get_deps_structured cell with
  | None -> printfn "%s: <none>" label
  | Some dyn -> printfn "%s: %s" label (Dyn.to_string dyn)
;;

let%expect_test "dependency structure of Memo combinators" =
  let a = Memo.lazy_node ~name:"a" (fun () -> Memo.return ())
  and b = Memo.lazy_node ~name:"b" (fun () -> Memo.return ())
  and c = Memo.lazy_node ~name:"c" (fun () -> Memo.return ()) in
  (* No dependencies. *)
  print_deps "empty" (Memo.return ());
  [%expect {| empty: Empty |}];
  (* A single dependency is a [Singleton], whether discovered by map or bind. *)
  print_deps "map" (Memo.map (read a) ~f:(fun () -> ()));
  [%expect {| map: Singleton (Some "a", ()) |}];
  print_deps "bind" (Memo.bind (read a) ~f:(fun () -> Memo.return ()));
  [%expect {| bind: Singleton (Some "a", ()) |}];
  (* Sequential dependencies form a [Seq] in chronological order. *)
  print_deps
    "seq"
    (let* () = read a in
     let* () = read b in
     read c);
  [%expect
    {|
    seq: Seq
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  (* A parallel combinator forms a [Par]. *)
  print_deps "par" (Memo.parallel_map [ a; b; c ] ~f:read |> Memo.map ~f:ignore);
  [%expect
    {|
    par: Par
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  (* Nested sequential and parallel sections are flattened without changing their order. *)
  let parallel ms = Memo.map (Memo.all_concurrently ms) ~f:ignore in
  print_deps
    "flatten_nested_seq"
    (let* () = read a in
     parallel
       [ (let* () = read b in
          read c)
       ; Memo.return ()
       ]);
  [%expect
    {|
    flatten_nested_seq: Seq
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  print_deps "flatten_nested_par" (parallel [ parallel [ read a; read b ]; read c ]);
  [%expect
    {|
    flatten_nested_par: Par
      [ Singleton (Some "a", ())
      ; Singleton (Some "b", ())
      ; Singleton (Some "c", ())
      ]
    |}];
  (* Empty parallel and sequential sections collapse away, even when nested. *)
  let e = Memo.return () in
  let par m = Memo.map (Memo.all_concurrently [ m; e ]) ~f:ignore in
  let seq m = Memo.map m ~f:(fun () -> ()) in
  print_deps "nested_empty" (seq (par (seq (par e))));
  [%expect {| nested_empty: Empty |}]
;;
