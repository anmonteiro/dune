open Stdune
open Dune_engine

let () = Dune_tests_common.init ()

let%expect_test "directory rule unions preserve emission identity and order" =
  let dir = Path.Build.relative Path.Build.root "default/union" in
  let emission name =
    let rule =
      Rule.make
        ~targets:(Targets.File.create (Path.Build.relative dir name))
        (Action_builder.return Action.Full.empty)
    in
    rule, Rules.find (Rules.of_rules [ rule ]) (Path.build dir)
  in
  let first, first_rules = emission "first" in
  let second, second_rules = emission "second" in
  let third, third_rules = emission "third" in
  let union = Rules.Dir_rules.union in
  let check name rules expected =
    let { Rules.Dir_rules.rules; aliases } = Rules.Dir_rules.consume rules in
    printfn
      "%s: %b"
      name
      (List.equal ( == ) rules expected && Alias.Name.Map.is_empty aliases)
  in
  let all = union (union third_rules first_rules) second_rules in
  check "creation order" all [ first; second; third ];
  check "empty left" (union Rules.Dir_rules.empty all) [ first; second; third ];
  check "empty right" (union all Rules.Dir_rules.empty) [ first; second; third ];
  check "same map" (union all all) [ first; second; third ];
  check "shared singleton" (union all second_rules) [ first; second; third ];
  check "non-singleton right" (union second_rules all) [ first; second; third ];
  let another_emission = Rules.find (Rules.of_rules [ first ]) (Path.build dir) in
  check
    "distinct emissions of one rule"
    (union first_rules another_emission)
    [ first; first ];
  [%expect
    {|
    creation order: true
    empty left: true
    empty right: true
    same map: true
    shared singleton: true
    non-singleton right: true
    distinct emissions of one rule: true
    |}]
;;

let%expect_test "prefixed rule groups preserve nesting and alias identity" =
  let run memo =
    Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension")
  in
  let dir = Path.Build.relative Path.Build.root "default/prefix" in
  let calls = ref [] in
  let record name =
    Memo.of_thunk (fun () ->
      calls := name :: !calls;
      Memo.return ())
  in
  let prefix name = Action_builder.record_success (record name) in
  let rule name =
    Rule.make
      ~targets:(Targets.File.create (Path.Build.relative dir name))
      (Action_builder.of_memo
         (let open Memo.O in
          let+ () = record name in
          Action.Full.empty))
  in
  let alias = Alias.make Alias.Name.default ~dir in
  let original =
    run
      (Rules.collect_unit (fun () ->
         let open Memo.O in
         let* () = Rules.Produce.rule (rule "first") in
         let* () = Rules.Produce.Alias.add_deps alias (prefix "alias") in
         Rules.Produce.rule (rule "second")))
  in
  let with_prefix name rules =
    run
      (Rules.collect_unit (fun () ->
         Rules.prefix_rules (prefix name) ~f:(fun () -> Rules.produce rules)))
  in
  let nested = with_prefix "outer" (with_prefix "inner" original) in
  let check name collection =
    let { Rules.Dir_rules.rules; aliases } =
      Rules.find collection (Path.build dir) |> Rules.Dir_rules.consume
    in
    let run_action action =
      calls := [];
      let _, _ = run (Action_builder.evaluate_and_collect_facts action) in
      String.concat ~sep:" " (List.rev !calls)
    in
    let rule_calls = List.map rules ~f:(fun rule -> run_action rule.Rule.action) in
    let alias_calls =
      Alias.Name.Map.find_exn aliases Alias.Name.default
      |> Rules.Dir_rules.Alias_spec.to_list
      |> List.map ~f:(fun (_, item) ->
        match item with
        | Rules.Dir_rules.Alias_spec.Deps action -> run_action action
        | Action _ -> failwith "unexpected alias action")
    in
    printfn
      "%s: %s; aliases: %s"
      name
      (String.concat ~sep:", " rule_calls)
      (String.concat ~sep:", " alias_calls)
  in
  check "nested" nested;
  check "same group replay" (Rules.union nested nested);
  check "different prefixes" (Rules.union nested (with_prefix "other" original));
  check "unprefixed replay" (Rules.union nested original);
  [%expect
    {|
    nested: outer inner first, outer inner second; aliases: alias
    same group replay: outer inner first, outer inner second; aliases: alias
    different prefixes: outer inner first, outer inner second, other first, other second; aliases: alias
    unprefixed replay: first, second, outer inner first, outer inner second; aliases: alias
    |}]
;;
