module Glob = Dune_rpc.Private.Glob

let printf = Printf.printf

let%expect_test "escaped metacharacters remain literal" =
  let literal = "*?[ab]{c,d}\\name" in
  let glob = Glob.of_string (Glob.escape literal) in
  printf "matches literal: %b\n" (Glob.test glob literal);
  printf "literal representation: %b\n" (Glob.as_literal glob = Some literal);
  printf "rejects other text: %b\n" (not (Glob.test glob "cname"));
  [%expect
    {|
    matches literal: true
    literal representation: true
    rejects other text: true
    |}]
;;

let test glob s ~expect =
  let res = Glob.test glob s in
  let status = if res = expect then "pass" else "fail" in
  printf "[%s] %S matches %S == %b" status (Glob.to_string glob) s res
;;

let%expect_test _ =
  let glob = Glob.of_string "test" in
  printf "%S" (Glob.to_string glob);
  [%expect {| "test" |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "te*" in
  test glob "test" ~expect:true;
  [%expect {| [pass] "te*" matches "test" == true |}];
  test glob "t" ~expect:false;
  [%expect {| [pass] "te*" matches "t" == false |}];
  test glob "te" ~expect:true;
  [%expect {| [pass] "te*" matches "te" == true |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "*st" in
  test glob "test" ~expect:true;
  [%expect {| [pass] "*st" matches "test" == true |}];
  test glob "t" ~expect:false;
  [%expect {| [pass] "*st" matches "t" == false |}];
  (* This is surprising, but documented *)
  test glob "st" ~expect:false;
  [%expect {| [pass] "*st" matches "st" == false |}];
  test glob ".st" ~expect:false;
  [%expect {| [pass] "*st" matches ".st" == false |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "foo.{ml,mli}" in
  test glob "foo.ml" ~expect:true;
  [%expect {| [pass] "foo.{ml,mli}" matches "foo.ml" == true |}];
  test glob "foo.mli" ~expect:true;
  [%expect {| [pass] "foo.{ml,mli}" matches "foo.mli" == true |}];
  test glob "foo." ~expect:false;
  [%expect {| [pass] "foo.{ml,mli}" matches "foo." == false |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "foo**" in
  test glob "foo.ml" ~expect:false;
  [%expect {| [pass] "foo**" matches "foo.ml" == false |}];
  test glob "fooml" ~expect:true;
  [%expect {| [pass] "foo**" matches "fooml" == true |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "**" in
  test glob "foo/bar" ~expect:true;
  [%expect {| [pass] "**" matches "foo/bar" == true |}];
  test glob "" ~expect:true;
  [%expect {| [pass] "**" matches "" == true |}];
  test glob "foo.bar" ~expect:true;
  [%expect {| [pass] "**" matches "foo.bar" == true |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "*" in
  test glob ".foo" ~expect:false;
  [%expect {| [pass] "*" matches ".foo" == false |}];
  test glob "foo.ml" ~expect:true;
  [%expect {| [pass] "*" matches "foo.ml" == true |}];
  test glob "foo/" ~expect:false;
  [%expect {| [pass] "*" matches "foo/" == false |}]
;;

let%expect_test _ =
  let glob = Glob.of_string "[!._]*" in
  test glob ".foo" ~expect:false;
  [%expect {| [pass] "[!._]*" matches ".foo" == false |}];
  test glob "foo.ml" ~expect:true;
  [%expect {| [pass] "[!._]*" matches "foo.ml" == true |}];
  test glob "a" ~expect:true;
  [%expect {| [pass] "[!._]*" matches "a" == true |}]
;;
