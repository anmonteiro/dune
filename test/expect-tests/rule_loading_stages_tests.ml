open Stdune
open Dune_engine

let () = Dune_tests_common.init ()

let run memo =
  try Fiber.run (Memo.run memo) ~iter:(fun () -> failwith "unexpected suspension") with
  | Memo.Error.E error -> raise (Memo.Error.get error)
;;

let path = Path.Build.of_string

let rule targets =
  Rule.make ~targets (Action_builder.return (Action.Full.make Action.empty))
;;

let file_rule files = rule (Targets.Files.create (Path.Build.Set.of_list files))

let rules_in ~dir rules =
  let { Rules.Dir_rules.rules; _ } =
    Rules.find rules (Path.build dir) |> Rules.Dir_rules.consume
  in
  rules
;;

let print_files label ~dir rules =
  let files =
    rules_in ~dir rules
    |> List.concat_map ~f:(fun rule -> Filename.Set.to_list rule.Rule.targets.files)
    |> List.map ~f:Filename.to_string
    |> List.sort ~compare:String.compare
  in
  printfn "%s: %s" label (String.concat files ~sep:", ")
;;

let expect_code_error label f =
  try
    f ();
    printfn "%s: no error" label
  with
  | Code_error.E _ -> printfn "%s: code error" label
;;

let%expect_test "target masks distinguish kinds and directory boundaries" =
  let dir = path "default/masks" in
  let child = Path.Build.relative dir "child" in
  let file = Path.Build.relative dir "x.ml" in
  let nested = Path.Build.relative child "x.ml" in
  let alias = Alias.make (Alias.Name.of_string "x.ml") ~dir in
  let files = Target_mask.files [ file ] in
  printfn
    "file/directory overlap: %b"
    (Target_mask.intersects files (Target_mask.directories [ file ]));
  printfn
    "file/alias overlap: %b"
    (Target_mask.intersects files (Target_mask.aliases [ alias ]));
  printfn
    "path covers file and directory: %b"
    (Target_mask.mem_file (Target_mask.path file) file
     && Target_mask.mem_directory (Target_mask.path file) file);
  let direct = Target_mask.files_in_directory dir in
  printfn "direct file: %b" (Target_mask.mem_file direct file);
  printfn "nested file: %b" (Target_mask.mem_file direct nested);
  let subtree = Target_mask.subtree dir in
  printfn "subtree nested file: %b" (Target_mask.mem_file subtree nested);
  printfn "subtree directory: %b" (Target_mask.mem_directory subtree child);
  printfn "subtree alias: %b" (Target_mask.mem_alias subtree alias);
  let extensions =
    Target_mask.file_extensions
      ~dir
      (Filename.Extension.Set.singleton Filename.Extension.ml)
  in
  printfn
    "matching extension: %b"
    (Target_mask.mem_file (Target_mask.inter files extensions) file);
  printfn "extension in child: %b" (Target_mask.mem_file extensions nested);
  printfn
    "disjoint intersection: %b"
    (Target_mask.is_empty (Target_mask.inter extensions (Target_mask.files [ nested ])));
  [%expect
    {|
    file/directory overlap: false
    file/alias overlap: false
    path covers file and directory: true
    direct file: true
    nested file: false
    subtree nested file: true
    subtree directory: true
    subtree alias: true
    matching extension: true
    extension in child: false
    disjoint intersection: true
    |}]
;;

let%expect_test "filename predicates preserve precise literal intersections" =
  let dir = path "default/predicates" in
  let matching pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern
    |> Predicate_lang.Glob.of_glob
    |> Target_mask.files_matching ~dir
  in
  let seed = matching "seed" in
  let modules = matching "*.modules" in
  let overlaps label a b =
    printfn "%s: %b / %b" label (Target_mask.intersects a b) (Target_mask.intersects b a)
  in
  overlaps "seed/glob" seed modules;
  overlaps "distinct exact names" seed (matching "other");
  overlaps "matching name/glob" (matching "selected.modules") modules;
  overlaps
    "literal set/glob"
    (Target_mask.files_matching ~dir (Predicate_lang.Glob.of_string_list [ "seed" ]))
    modules;
  printfn
    "disjoint intersections: %b / %b"
    (Target_mask.is_empty (Target_mask.inter seed modules))
    (Target_mask.is_empty (Target_mask.inter modules seed));
  List.iter
    [ "seed"; "selected.modules"; "selected.modules.backup"; "child/x.modules" ]
    ~f:(fun name ->
      printfn "%s: %b" name (Target_mask.mem_file modules (Path.Build.relative dir name)));
  [%expect
    {|
    seed/glob: false / false
    distinct exact names: false / false
    matching name/glob: true / true
    literal set/glob: false / false
    disjoint intersections: true / true
    seed: false
    selected.modules: true
    selected.modules.backup: false
    child/x.modules: false
    |}]
;;

let%expect_test "subtree extension masks preserve suffix and directory boundaries" =
  let dir = path "default/extensions" in
  let child = Path.Build.relative dir "child" in
  let extensions =
    Filename.Extension.Set.singleton (Filename.Extension.of_string_exn ".v.d")
  in
  let mask = Target_mask.file_extensions_in_subtree ~dir extensions in
  List.iter
    [ "default/extensions/x.v.d"
    ; "default/extensions/child/x.v.d"
    ; "default/extensions/child/x.d"
    ; "default/extensions/child/x.v.d.extra"
    ; "default/extensions-other/x.v.d"
    ; "default/x.v.d"
    ]
    ~f:(fun name -> printfn "%s: %b" name (Target_mask.mem_file mask (path name)));
  printfn
    "directory target: %b"
    (Target_mask.mem_directory mask (Path.Build.relative child "x.v.d"));
  printfn
    "alias: %b"
    (Target_mask.mem_alias mask (Alias.make (Alias.Name.of_string "x.v.d") ~dir:child));
  let direct =
    Target_mask.file_extensions
      ~dir:child
      (Filename.Extension.Set.singleton Filename.Extension.d)
  in
  let intersection = Target_mask.inter mask direct in
  let reverse = Target_mask.inter direct mask in
  List.iter [ "child/x.v.d"; "child/x.d"; "x.v.d" ] ~f:(fun name ->
    let file = Path.Build.relative dir name in
    printfn
      "intersection %s: %b / %b"
      name
      (Target_mask.mem_file intersection file)
      (Target_mask.mem_file reverse file));
  printfn
    "disjoint suffix: %b"
    (Target_mask.intersects
       mask
       (Target_mask.file_extensions_in_subtree
          ~dir
          (Filename.Extension.Set.singleton Filename.Extension.ml)));
  printfn
    "disjoint subtree: %b"
    (Target_mask.intersects
       mask
       (Target_mask.file_extensions_in_subtree
          ~dir:(path "default/extensions-other")
          extensions));
  [%expect
    {|
    default/extensions/x.v.d: true
    default/extensions/child/x.v.d: true
    default/extensions/child/x.d: false
    default/extensions/child/x.v.d.extra: false
    default/extensions-other/x.v.d: false
    default/x.v.d: false
    directory target: false
    alias: false
    intersection child/x.v.d: true / true
    intersection child/x.d: false / false
    intersection x.v.d: false / false
    disjoint suffix: false
    disjoint subtree: false
    |}]
;;

let%expect_test "filename predicates exclude disjoint output suffixes" =
  let dir = path "default/glob-suffixes" in
  let outputs =
    Target_mask.file_extensions_in_subtree
      ~dir
      (Filename.Extension.Set.singleton (Filename.Extension.of_string_exn ".vo"))
  in
  let matches predicate =
    let inputs =
      Target_mask.files_matching ~dir:(Path.Build.relative dir "inputs") predicate
    in
    Target_mask.intersects inputs outputs, Target_mask.intersects outputs inputs
  in
  let glob pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern |> Predicate_lang.Glob.of_glob
  in
  List.iter [ "*.v"; "*.vo"; "{foo,bar}.v"; "*.v[o]"; "*\\?.v"; "*" ] ~f:(fun pattern ->
    let forward, reverse = matches (glob pattern) in
    printfn "%s: %b / %b" pattern forward reverse);
  let print label predicate =
    let forward, reverse = matches predicate in
    printfn "%s: %b / %b" label forward reverse
  in
  print "union" (Predicate_lang.or_ [ glob "*.v"; glob "*.ml" ]);
  print "intersection" (Predicate_lang.and_ [ glob "*"; glob "*.v" ]);
  print "complement" (Predicate_lang.not (glob "*.v"));
  [%expect
    {|
    *.v: false / false
    *.vo: true / true
    {foo,bar}.v: false / false
    *.v[o]: true / true
    *\?.v: false / false
    *: true / true
    union: false / false
    intersection: false / false
    complement: true / true
    |}]
;;

let%expect_test "glob intersections preserve dot-prefixed extension matches" =
  let dir = path "default/hidden-extensions" in
  let extensions =
    Target_mask.file_extensions
      ~dir
      (Filename.Extension.Set.singleton (Filename.Extension.of_string_exn ".vo"))
  in
  List.iter [ "**"; ".*"; "*.vo" ] ~f:(fun pattern ->
    let glob =
      Dune_lang.Glob.of_string_exn Loc.none pattern
      |> Predicate_lang.Glob.of_glob
      |> Target_mask.files_matching ~dir
    in
    let forward = Target_mask.inter extensions glob in
    let reverse = Target_mask.inter glob extensions in
    List.iter [ ".vo"; ".hidden.vo"; "x.vo"; "x.vo.bak" ] ~f:(fun name ->
      let file = Path.Build.relative dir name in
      printfn
        "%s / %s: %b / %b"
        pattern
        name
        (Target_mask.mem_file forward file)
        (Target_mask.mem_file reverse file)));
  [%expect
    {|
    ** / .vo: true / true
    ** / .hidden.vo: true / true
    ** / x.vo: true / true
    ** / x.vo.bak: false / false
    .* / .vo: true / true
    .* / .hidden.vo: true / true
    .* / x.vo: false / false
    .* / x.vo.bak: false / false
    *.vo / .vo: false / false
    *.vo / .hidden.vo: false / false
    *.vo / x.vo: true / true
    *.vo / x.vo.bak: false / false
    |}]
;;

let%expect_test "alias directory projection ignores file-only regions" =
  let dir = path "default/alias-directories" in
  let child = Path.Build.relative dir "generated/deep" in
  let exact =
    Target_mask.aliases [ Alias.make (Alias.Name.of_string "check") ~dir:child ]
  in
  let print label mask =
    let directories = Target_mask.alias_directories mask ~dir in
    match Dir_set.toplevel_subdirs directories with
    | Infinite -> printfn "%s: infinite" label
    | Finite names ->
      let names =
        Filename.Set.to_list names
        |> List.map ~f:Filename.to_string
        |> String.concat ~sep:", "
      in
      printfn "%s: %s" label names
  in
  print "exact alias" exact;
  print "aliases in directory" (Target_mask.aliases_in_directory child);
  let files =
    Target_mask.file_extensions_in_subtree
      ~dir:(Path.Build.relative dir "files")
      (Filename.Extension.Set.singleton Filename.Extension.ml)
  in
  print "alias and file-only subtree" (Target_mask.union exact files);
  print "alias subtree" (Target_mask.subtree dir);
  [%expect
    {|
    exact alias: generated
    aliases in directory: generated
    alias and file-only subtree: generated
    alias subtree: infinite
    |}]
;;

let%expect_test "subtrees use alias directories, not alias names" =
  let dir = path "default/alias-boundaries" in
  let child = Path.Build.relative dir "child" in
  let parent_alias = Alias.make (Alias.Name.of_string "child") ~dir in
  let child_alias = Alias.make (Alias.Name.of_string "check") ~dir:child in
  let subtree = Target_mask.subtree child in
  let parent = Target_mask.aliases [ parent_alias ] in
  printfn "parent alias membership: %b" (Target_mask.mem_alias subtree parent_alias);
  printfn
    "parent alias intersection: %b / %b"
    (Target_mask.intersects subtree parent)
    (Target_mask.intersects parent subtree);
  printfn
    "parent alias ownership: %b"
    (Target_mask.alias_directories subtree ~dir |> Dir_set.here);
  printfn "child alias membership: %b" (Target_mask.mem_alias subtree child_alias);
  printfn
    "child alias ownership: %b"
    (Target_mask.alias_directories subtree ~dir:child |> Dir_set.here);
  [%expect
    {|
    parent alias membership: false
    parent alias intersection: false / false
    parent alias ownership: false
    child alias membership: true
    child alias ownership: true
    |}]
;;

let%expect_test "directory cleanup only considers contained outputs" =
  let dir = path "default/cleanup-boundaries" in
  let child = Path.Build.relative dir "child" in
  let alias name ~dir = Alias.make (Alias.Name.of_string name) ~dir in
  List.iter
    [ "parent aliases", Target_mask.aliases_in_directory dir
    ; "parent alias with same name", Target_mask.aliases [ alias "child" ~dir ]
    ; "file at root", Target_mask.files [ child ]
    ; "descendant file", Target_mask.files [ Path.Build.relative child "out" ]
    ; "exact directory", Target_mask.directories [ child ]
    ; "child alias", Target_mask.aliases [ alias "check" ~dir:child ]
    ; "sibling file", Target_mask.files [ Path.Build.relative dir "sibling/out" ]
    ]
    ~f:(fun (label, mask) ->
      printfn "%s: %b" label (Target_mask.intersects_directory mask child));
  [%expect
    {|
    parent aliases: false
    parent alias with same name: false
    file at root: false
    descendant file: true
    exact directory: true
    child alias: true
    sibling file: false
    |}]
;;

let%expect_test "action masks reuse target inference without expanding variables" =
  let module A = Dune_rules.For_tests.Action_unexpanded in
  let module S = Dune_lang.String_with_vars in
  let dir = path "default/action-targets" in
  let text = S.make_text Loc.none in
  let variable = S.make_pform Loc.none (Var (User_var "output")) in
  let write target = A.Write_file (target, Normal, text "contents") in
  let dynamic = write variable in
  let optional_diff =
    A.Diff
      { Action_types.Diff.file1 = text "expected"
      ; file2 = text "out"
      ; optional = true
      ; mode = Text
      ; directory_diffs = true
      }
  in
  List.iter
    [ "literal", write (text "out")
    ; "dynamic", dynamic
    ; "chdir", A.Chdir (text "child", write (text "../out"))
    ; "dynamic chdir", A.Chdir (variable, write (text "out"))
    ; "escaping target", write (text "../../../out")
    ; "escaping chdir", A.Chdir (text "../../../outside", write (text "out"))
    ; "no-infer", A.No_infer dynamic
    ; "run", A.run (S.make_pform Loc.none (Var Test)) []
    ; "line directive", A.Copy_and_add_line_directive (text "input", text "out")
    ; "format", A.Format_dune_file (text "input", text "out")
    ; "consumed", A.Progn [ write (text "out"); optional_diff ]
    ]
    ~f:(fun (label, action) ->
      let mask = A.rule_targets ~dir ~targets:Infer action in
      printfn
        "%s (out / other): %b / %b"
        label
        (Target_mask.mem_file mask (Path.Build.relative dir "out"))
        (Target_mask.mem_file mask (Path.Build.relative dir "other")));
  let dynamic = A.rule_targets ~dir ~targets:Infer dynamic in
  printfn
    "dynamic descendant: %b"
    (Target_mask.mem_file dynamic (Path.Build.relative dir "child/out"));
  printfn
    "dynamic directory: %b"
    (Target_mask.mem_directory dynamic (Path.Build.relative dir "out"));
  [%expect
    {|
    literal (out / other): true / false
    dynamic (out / other): true / true
    chdir (out / other): true / false
    dynamic chdir (out / other): true / true
    escaping target (out / other): true / true
    escaping chdir (out / other): true / true
    no-infer (out / other): false / false
    run (out / other): false / false
    line directive (out / other): true / false
    format (out / other): true / false
    consumed (out / other): false / false
    dynamic descendant: false
    dynamic directory: false
    |}]
;;

let%expect_test "action target variables preserve singleton declaration precision" =
  let module A = Dune_rules.For_tests.Action_unexpanded in
  let module S = Dune_lang.String_with_vars in
  let dir = path "default/declared-action-targets" in
  let text = S.make_text Loc.none in
  let target = S.make_pform Loc.none (Var Target) in
  let targets = S.make_pform Loc.none (Var Targets) in
  let declaration multiplicity names : _ Dune_lang.Targets_spec.t =
    Static
      { targets =
          List.map names ~f:(fun name -> text name, Dune_lang.Targets_spec.Kind.File)
      ; multiplicity
      }
  in
  let single = declaration One [ "out" ] in
  let multiple = declaration Multiple [ "out" ] in
  let write target = A.Write_file (target, Normal, text "contents") in
  List.iter
    [ "target", single, write target
    ; "targets", multiple, write targets
    ; "escaping target", declaration One [ "../../../out" ], write target
    ; "escaping targets", declaration Multiple [ "../../../out" ], write targets
    ; "chdir target", single, A.Chdir (text "child", write target)
    ; ( "dynamic chdir target"
      , single
      , A.Chdir (S.make_pform Loc.none (Var (User_var "dir")), write target) )
    ]
    ~f:(fun (label, targets, action) ->
      let mask = A.rule_targets ~dir ~targets action in
      printfn
        "%s (out / other): %b / %b"
        label
        (Target_mask.mem_file mask (Path.Build.relative dir "out"))
        (Target_mask.mem_file mask (Path.Build.relative dir "other")));
  let mask =
    A.rule_targets
      ~dir
      ~targets:(declaration Multiple [ "out"; "other" ])
      (write (S.make_pform ~quoted:true Loc.none (Var Targets)))
  in
  printfn
    "quoted multi-target joined filename: %b"
    (Target_mask.mem_file mask (Path.Build.relative dir "out other"));
  [%expect
    {|
    target (out / other): true / false
    targets (out / other): true / false
    escaping target (out / other): true / true
    escaping targets (out / other): true / true
    chdir target (out / other): true / false
    dynamic chdir target (out / other): true / false
    quoted multi-target joined filename: true
    |}]
;;

let%expect_test "nested stages are pulled selectively and shared" =
  let dir = path "default/nested" in
  let x = Path.Build.relative dir "x.ml" in
  let y = Path.Build.relative dir "y.ml" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow (Target_mask.subtree dir) (fun () ->
           printfn "force outer";
           let* () =
             Rules.narrow
               (Target_mask.files [ x; y ])
               (fun () ->
                  printfn "force inner";
                  let* () =
                    Rules.narrow (Target_mask.files [ x ]) (fun () ->
                      printfn "force x";
                      Rules.Produce.rule (file_rule [ x ]))
                  in
                  Rules.narrow (Target_mask.files [ y ]) (fun () ->
                    printfn "force y";
                    Rules.Produce.rule (file_rule [ y ])))
           in
           Rules.narrow (Target_mask.aliases [ alias ]) (fun () ->
             printfn "force alias";
             Rules.Produce.Alias.add_deps alias (Action_builder.return ()))))
     in
     printfn "collected";
     let* first = Rules.load tree (Target_mask.files [ x ]) in
     print_files "first" ~dir first;
     let* repeated = Rules.load tree (Target_mask.files [ x ]) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir repeated));
     let* second = Rules.load tree (Target_mask.files [ y ]) in
     print_files "second" ~dir second;
     let+ aliases = Rules.load tree (Target_mask.aliases [ alias ]) in
     let { Rules.Dir_rules.aliases; _ } =
       Rules.find aliases (Path.build dir) |> Rules.Dir_rules.consume
     in
     printfn "aliases: %d" (Alias.Name.Map.cardinal aliases));
  [%expect
    {|
    collected
    force outer
    force inner
    force x
    first: x.ml
    same rules: true
    force y
    second: y.ml
    force alias
    aliases: 1
    |}]
;;

let%expect_test "child masks are restricted by their parent" =
  let dir = path "default/owned" in
  let inside = Path.Build.relative dir "x" in
  let outside = path "default/other/x" in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow Target_mask.all (fun () ->
           let* () =
             Rules.narrow (Target_mask.files [ inside ]) (fun () ->
               printfn "force inside";
               Rules.Produce.rule (file_rule [ inside ]))
           in
           Rules.narrow (Target_mask.files [ outside ]) (fun () ->
             printfn "force outside";
             Rules.Produce.rule (file_rule [ outside ]))))
     in
     let tree = Rules.restrict tree (Target_mask.subtree dir) in
     printfn "outside declared: %b" (Target_mask.mem_file (Rules.targets tree) outside);
     let* outside_rules = Rules.load tree (Target_mask.files [ outside ]) in
     printfn "outside rules: %d" (List.length (rules_in ~dir outside_rules));
     let+ inside_rules = Rules.load tree (Target_mask.files [ inside ]) in
     print_files "inside" ~dir inside_rules);
  [%expect
    {|
    outside declared: false
    outside rules: 0
    force inside
    inside: x
    |}]
;;

let%expect_test "repeated restrictions preserve matching and selective loading" =
  let dir = path "default/repeated-restrictions" in
  let x = Path.Build.relative dir "x.ml" in
  let y = Path.Build.relative dir "y.ml" in
  let subtree = Target_mask.subtree dir in
  let duplicated = Target_mask.union subtree subtree in
  let matching pattern =
    Dune_lang.Glob.of_string_exn Loc.none pattern
    |> Predicate_lang.Glob.of_glob
    |> Target_mask.files_matching ~dir
  in
  let ml = matching "*.ml" in
  let starts_with_x = matching "x*" in
  let restrictions = [ duplicated; ml; starts_with_x; starts_with_x; ml; duplicated ] in
  let once = Target_mask.inter ml starts_with_x in
  let repeated = List.fold_left restrictions ~init:duplicated ~f:Target_mask.inter in
  List.iter [ "x.ml"; "y.ml"; "x.mli"; "child/x.ml" ] ~f:(fun name ->
    let file = Path.Build.relative dir name in
    printfn
      "%s: %b / %b"
      name
      (Target_mask.mem_file once file)
      (Target_mask.mem_file repeated file));
  printfn "directory: %b" (Target_mask.mem_directory repeated x);
  printfn
    "alias: %b"
    (Target_mask.mem_alias repeated (Alias.make (Alias.Name.of_string "x.ml") ~dir));
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         Rules.narrow duplicated (fun () ->
           Rules.narrow duplicated (fun () ->
             let* () =
               Rules.narrow (Target_mask.files [ x ]) (fun () ->
                 printfn "force x";
                 Rules.Produce.rule (file_rule [ x ]))
             in
             Rules.narrow (Target_mask.files [ y ]) (fun () ->
               printfn "force y";
               Rules.Produce.rule (file_rule [ y ])))))
     in
     let tree = List.fold_left restrictions ~init:tree ~f:Rules.restrict in
     let* first = Rules.load tree (Target_mask.files [ x ]) in
     print_files "first" ~dir first;
     let* second = Rules.load tree (Target_mask.files [ x ]) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir second));
     let+ excluded = Rules.load tree (Target_mask.files [ y ]) in
     printfn "excluded rules: %d" (List.length (rules_in ~dir excluded)));
  [%expect
    {|
    x.ml: true / true
    y.ml: false / false
    x.mli: false / false
    child/x.ml: false / false
    directory: false
    alias: false
    force x
    first: x.ml
    same rules: true
    excluded rules: 0
    |}]
;;

let%expect_test "rules and aliases cannot escape a narrowed mask" =
  let dir = path "default/validated" in
  let inside = Path.Build.relative dir "x" in
  let outside = path "default/other/x" in
  let alias = Alias.make (Alias.Name.of_string "check") ~dir in
  let reject label produce =
    expect_code_error label (fun () ->
      run
        (let open Memo.O in
         let* tree =
           Rules.collect_unit (fun () -> Rules.narrow Target_mask.all produce)
         in
         let tree = Rules.restrict tree (Target_mask.files [ inside ]) in
         let+ (_ : Rules.t) = Rules.load tree (Target_mask.files [ inside ]) in
         ()))
  in
  reject "file" (fun () -> Rules.Produce.rule (file_rule [ outside ]));
  reject "directory" (fun () ->
    let targets =
      Targets.create ~files:Path.Build.Set.empty ~dirs:(Path.Build.Set.singleton inside)
    in
    Rules.Produce.rule (rule targets));
  reject "alias" (fun () -> Rules.Produce.Alias.add_deps alias (Action_builder.return ()));
  [%expect
    {|
    file: code error
    directory: code error
    alias: code error
    |}]
;;

let%expect_test "multi-target closure pulls every overlapping producer" =
  let dir = path "default/multi" in
  let data = Path.Build.relative dir "data" in
  let shared = Path.Build.relative dir "shared" in
  let forced = ref [] in
  let produce label rule =
    forced := label :: !forced;
    Rules.Produce.rule rule
  in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let* () =
           Rules.narrow
             (Target_mask.files [ data; shared ])
             (fun () -> produce "data" (file_rule [ data; shared ]))
         in
         let* () =
           Rules.narrow (Target_mask.files [ shared ]) (fun () ->
             produce "file" (file_rule [ shared ]))
         in
         Rules.narrow (Target_mask.directories [ shared ]) (fun () ->
           let targets =
             Targets.create
               ~files:Path.Build.Set.empty
               ~dirs:(Path.Build.Set.singleton shared)
           in
           produce "directory" (rule targets)))
     in
     let+ loaded = Rules.load tree (Target_mask.files [ data ]) in
     printfn
       "forced: %s"
       (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
     printfn "rules: %d" (List.length (rules_in ~dir loaded));
     print_files "files" ~dir loaded;
     printfn
       "directory targets: %d"
       (Path.Build.Map.cardinal (Rules.directory_targets loaded)));
  [%expect
    {|
    forced: data, directory, file
    rules: 3
    files: data, shared, shared
    directory targets: 1
    |}]
;;

let%expect_test "directory materialization does not pull same-name file producers" =
  let dir = path "default/materialize" in
  let target = Path.Build.relative dir "generated" in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let* () =
           Rules.narrow (Target_mask.directories [ target ]) (fun () ->
             printfn "force directory";
             let targets =
               Targets.create
                 ~files:Path.Build.Set.empty
                 ~dirs:(Path.Build.Set.singleton target)
             in
             Rules.Produce.rule (rule targets))
         in
         Rules.narrow (Target_mask.files [ target ]) (fun () ->
           printfn "force file";
           Rules.Produce.rule (file_rule [ target ])))
     in
     printfn "materialize";
     let* loaded = Rules.load_directory_with_pending tree target in
     printfn "rules: %d" (List.length (rules_in ~dir loaded.selected));
     printfn "file pending: %b" (Target_mask.mem_file loaded.pending target);
     printfn "normal lookup";
     let+ loaded = Rules.load tree (Target_mask.path target) in
     printfn "rules: %d" (List.length (rules_in ~dir loaded)));
  [%expect
    {|
    materialize
    force directory
    rules: 1
    file pending: true
    normal lookup
    force file
    rules: 2
    |}]
;;

let%expect_test "directory materialization closes over mixed rule file outputs" =
  let dir = path "default/materialize-mixed" in
  let target = Path.Build.relative dir "generated" in
  let shared = Path.Build.relative dir "shared" in
  let forced = ref [] in
  let produce label rule =
    forced := label :: !forced;
    Rules.Produce.rule rule
  in
  run
    (let open Memo.O in
     let* tree =
       Rules.collect_unit (fun () ->
         let* () =
           let mask =
             Target_mask.union
               (Target_mask.directories [ target ])
               (Target_mask.files [ shared ])
           in
           Rules.narrow mask (fun () ->
             let targets =
               Targets.create
                 ~files:(Path.Build.Set.singleton shared)
                 ~dirs:(Path.Build.Set.singleton target)
             in
             produce "mixed" (rule targets))
         in
         let* () =
           Rules.narrow (Target_mask.files [ target ]) (fun () ->
             produce "same-name file" (file_rule [ target ]))
         in
         let* () =
           Rules.narrow (Target_mask.files [ shared ]) (fun () ->
             produce "shared file" (file_rule [ shared ]))
         in
         Rules.narrow (Target_mask.directories [ shared ]) (fun () ->
           let targets =
             Targets.create
               ~files:Path.Build.Set.empty
               ~dirs:(Path.Build.Set.singleton shared)
           in
           produce "shared directory" (rule targets)))
     in
     let+ loaded = Rules.load_directory_with_pending tree target in
     printfn
       "forced: %s"
       (List.sort !forced ~compare:String.compare |> String.concat ~sep:", ");
     printfn "rules: %d" (List.length (rules_in ~dir loaded.selected));
     printfn "same-name file pending: %b" (Target_mask.mem_file loaded.pending target));
  [%expect
    {|
    forced: mixed, shared directory, shared file
    rules: 3
    same-name file pending: true
    |}]
;;

let%expect_test "forcing a deferred result shares its rule production" =
  let dir = path "default/deferred" in
  let target = Path.Build.relative dir "result" in
  run
    (let open Memo.O in
     let* value, tree =
       Rules.collect (fun () ->
         Rules.defer (Target_mask.files [ target ]) (fun () ->
           printfn "force producer";
           let+ () = Rules.Produce.rule (file_rule [ target ]) in
           42))
     in
     let* value = Memo.Lazy.force value in
     printfn "value: %d" value;
     let* first = Rules.load tree (Target_mask.files [ target ]) in
     print_files "loaded" ~dir first;
     let+ repeated = Rules.load tree (Target_mask.files [ target ]) in
     printfn
       "same rules: %b"
       (List.equal Rule.equal (rules_in ~dir first) (rules_in ~dir repeated)));
  [%expect
    {|
    force producer
    value: 42
    loaded: result
    same rules: true
    |}]
;;
