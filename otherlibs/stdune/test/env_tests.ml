open Stdune

let test_override env ~var ~value =
  let expected = Env.add env ~var ~value in
  let actual_unix = Env.to_unix_with_override env ~var ~value in
  let actual = actual_unix |> Array.of_list |> Env.of_unix in
  if Sys.win32
  then (
    let expected_unix = Env.to_unix expected in
    if not (List.equal String.equal expected_unix actual_unix)
    then
      Code_error.raise
        "Env.to_unix_with_override changed the serialized environment"
        [ "expected", Dyn.list Dyn.string expected_unix
        ; "actual", Dyn.list Dyn.string actual_unix
        ];
    let matching_entries =
      List.filter actual_unix ~f:(fun entry ->
        match String.lsplit2 entry ~on:'=' with
        | None -> false
        | Some (entry_var, _) -> Ordering.is_eq (Env.Var.compare entry_var var))
    in
    if List.length matching_entries <> 1
    then
      Code_error.raise
        "Env.to_unix_with_override returned duplicate variables"
        [ "var", Dyn.string var; "environment", Dyn.list Dyn.string actual_unix ]);
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
  test_override env ~var:"C" ~value:"value=with=equals";
  if Sys.win32
  then (
    let mixed_case = Env.of_unix [| "A=one"; "Temp=first" |] in
    test_override mixed_case ~var:"TEMP" ~value:"changed";
    let duplicate_case = Env.of_unix [| "A=one"; "Temp=first"; "TEMP=second" |] in
    test_override duplicate_case ~var:"temp" ~value:"changed")
;;
