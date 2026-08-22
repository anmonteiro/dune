open Import

let rules program ~dir ~sandbox ~units =
  let open Action_builder.O in
  let action =
    let+ action = Command.run' ?sandbox ~dir:(Path.build dir) program [ Deps units ] in
    { Rule.Anonymous_action.action; loc = Loc.none; dir }
  in
  Build_system.execute_action_stdout action
  |> Memo.map ~f:Ocamlobjinfo.parse
  |> Action_builder.of_memo
;;
