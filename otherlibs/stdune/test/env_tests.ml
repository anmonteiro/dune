open Stdune

let test_override env ~var ~value =
  let expected = Env.add env ~var ~value in
  let actual =
    Env.to_unix_with_override env ~var ~value |> Array.of_list |> Env.of_unix
  in
  if not (Env.equal expected actual)
  then
    Code_error.raise
      "Env.to_unix_with_override returned the wrong environment"
      [ "expected", Env.to_dyn expected; "actual", Env.to_dyn actual ]
;;

let%test_unit "to_unix_with_override" =
  let env = Env.of_unix [| "A=one"; "B=two"; "C=three" |] in
  test_override env ~var:"A" ~value:"one";
  test_override env ~var:"B" ~value:"changed";
  test_override env ~var:"D" ~value:"new";
  test_override env ~var:"C" ~value:"value=with=equals"
;;
