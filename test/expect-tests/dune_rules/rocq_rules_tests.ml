open Stdune
open Dune_engine
open Dune_rules.Rocq

let () = Dune_tests_common.init ()

let print_extraction_targets ~rocq_lang_version =
  let dir = Path.Build.relative Path.Build.root "default/rocq-aliases" in
  let sub = Path.Build.relative dir "sub" in
  let files = Filename.Array.Set.of_list [ Filename.of_string_exn "foo.v" ] in
  List.iter
    [ "flat", [ dir, files ]
    ; "generated", []
    ; "qualified", [ dir, Filename.Array.Set.empty; sub, files ]
    ]
    ~f:(fun (label, source_files) ->
      let mask =
        Rocq_rules.extraction_rule_targets
          ~dir
          ~source_files
          { target_fnames = []
          ; prelude = Loc.none, Rocq_module.Name.make "foo"
          ; buildable =
              { flags = Dune_lang.Ordered_set_lang.Unexpanded.standard
              ; rocq_lang_version
              ; mode = Some VoOnly
              ; use_corelib = false
              ; plugins = []
              ; theories = []
              ; loc = Loc.none
              }
          }
      in
      let file name = Target_mask.mem_file mask (Path.Build.relative dir name) in
      let alias dir name =
        Target_mask.mem_alias mask (Alias.make (Alias.Name.of_string name) ~dir)
      in
      printfn "%s:" label;
      List.iter
        [ "root", dir; "sub", sub ]
        ~f:(fun (label, dir) ->
          printfn
            "  %s aliases: runtest=%b foo.diff=%b"
            label
            (alias dir "runtest")
            (alias dir "foo.diff"));
      printfn
        "  files: foo.vo=%b foo.output=%b sub/foo.vo=%b sub/foo.output=%b"
        (file "foo.vo")
        (file "foo.output")
        (file "sub/foo.vo")
        (file "sub/foo.output");
      printfn
        "  unrelated: root-file=%b child-file=%b diff-file=%b directory=%b"
        (file "unrelated.txt")
        (file "sub/unrelated.txt")
        (file "foo.diff")
        (Target_mask.mem_directory mask sub))
;;

let%expect_test "Rocq 0.12 expected-output alias masks" =
  print_extraction_targets ~rocq_lang_version:(0, 12);
  [%expect
    {|
    flat:
      root aliases: runtest=true foo.diff=true
      sub aliases: runtest=false foo.diff=false
      files: foo.vo=true foo.output=true sub/foo.vo=false sub/foo.output=false
      unrelated: root-file=false child-file=false diff-file=false directory=false
    generated:
      root aliases: runtest=true foo.diff=true
      sub aliases: runtest=false foo.diff=false
      files: foo.vo=true foo.output=true sub/foo.vo=false sub/foo.output=false
      unrelated: root-file=false child-file=false diff-file=false directory=false
    qualified:
      root aliases: runtest=true foo.diff=true
      sub aliases: runtest=true foo.diff=true
      files: foo.vo=true foo.output=true sub/foo.vo=true sub/foo.output=true
      unrelated: root-file=false child-file=false diff-file=false directory=false
    |}]
;;

let%expect_test "Rocq 0.11 has no expected-output aliases" =
  print_extraction_targets ~rocq_lang_version:(0, 11);
  [%expect
    {|
    flat:
      root aliases: runtest=true foo.diff=false
      sub aliases: runtest=false foo.diff=false
      files: foo.vo=true foo.output=false sub/foo.vo=false sub/foo.output=false
      unrelated: root-file=false child-file=false diff-file=false directory=false
    generated:
      root aliases: runtest=true foo.diff=false
      sub aliases: runtest=false foo.diff=false
      files: foo.vo=true foo.output=false sub/foo.vo=false sub/foo.output=false
      unrelated: root-file=false child-file=false diff-file=false directory=false
    qualified:
      root aliases: runtest=true foo.diff=false
      sub aliases: runtest=false foo.diff=false
      files: foo.vo=true foo.output=false sub/foo.vo=true sub/foo.output=false
      unrelated: root-file=false child-file=false diff-file=false directory=false
    |}]
;;

let print_theory_targets ~rocq_lang_version =
  let dir = Path.Build.relative Path.Build.root "default/rocq-aliases" in
  let sub = Path.Build.relative dir "sub" in
  let unrelated = Path.Build.relative dir "unrelated" in
  let project =
    Dune_lang.Dune_project.anonymous
      ~dir:Path.Source.root
      Dune_lang.Package_info.empty
      Dune_lang.Package.Name.Map.empty
  in
  let targets ~source_files =
    Rocq_rules.theory_rule_targets
      ~dir
      ~source_files
      { name = Loc.none, Rocq_lib_name.append Rocq_lib_name.empty "test"
      ; package = None
      ; project
      ; synopsis = None
      ; modules = Dune_lang.Ordered_set_lang.standard
      ; modules_flags = None
      ; boot = false
      ; generate_project_file = Loc.none, false
      ; enabled_if = Dune_lang.Blang.true_
      ; buildable =
          { flags = Dune_lang.Ordered_set_lang.Unexpanded.standard
          ; rocq_lang_version
          ; mode = Some VoOnly
          ; use_corelib = false
          ; plugins = []
          ; theories = []
          ; loc = Loc.none
          }
      ; rocqdep_flags = Dune_lang.Ordered_set_lang.Unexpanded.standard
      ; rocqdoc_flags = Dune_lang.Ordered_set_lang.Unexpanded.standard
      ; rocqdoc_header = None
      ; rocqdoc_footer = None
      }
  in
  List.iter
    [ "empty", []
    ; "qualified", [ dir, Filename.Array.Set.empty; sub, Filename.Array.Set.empty ]
    ]
    ~f:(fun (label, source_files) ->
      let mask = targets ~source_files in
      printfn "%s:" label;
      List.iter
        [ "root", dir; "sub", sub; "unrelated", unrelated ]
        ~f:(fun (label, dir) ->
          let alias name =
            Target_mask.mem_alias mask (Alias.make (Alias.Name.of_string name) ~dir)
          in
          let file name = Target_mask.mem_file mask (Path.Build.relative dir name) in
          printfn
            "  %s aliases: runtest=%b unknown.diff=%b"
            label
            (alias "runtest")
            (alias "unknown.diff");
          printfn
            "  %s inputs: unknown.v=%b unknown.expected=%b unrelated.txt=%b"
            label
            (file "unknown.v")
            (file "unknown.expected")
            (file "unrelated.txt")))
;;

let%expect_test "Rocq 0.12 unknown-module alias masks" =
  print_theory_targets ~rocq_lang_version:(0, 12);
  [%expect
    {|
    empty:
      root aliases: runtest=true unknown.diff=true
      root inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      sub aliases: runtest=false unknown.diff=false
      sub inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      unrelated aliases: runtest=false unknown.diff=false
      unrelated inputs: unknown.v=false unknown.expected=false unrelated.txt=false
    qualified:
      root aliases: runtest=true unknown.diff=true
      root inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      sub aliases: runtest=true unknown.diff=true
      sub inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      unrelated aliases: runtest=false unknown.diff=false
      unrelated inputs: unknown.v=false unknown.expected=false unrelated.txt=false
    |}]
;;

let%expect_test "Rocq 0.11 unknown modules have no expected-output aliases" =
  print_theory_targets ~rocq_lang_version:(0, 11);
  [%expect
    {|
    empty:
      root aliases: runtest=true unknown.diff=false
      root inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      sub aliases: runtest=false unknown.diff=false
      sub inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      unrelated aliases: runtest=false unknown.diff=false
      unrelated inputs: unknown.v=false unknown.expected=false unrelated.txt=false
    qualified:
      root aliases: runtest=true unknown.diff=false
      root inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      sub aliases: runtest=false unknown.diff=false
      sub inputs: unknown.v=false unknown.expected=false unrelated.txt=false
      unrelated aliases: runtest=false unknown.diff=false
      unrelated inputs: unknown.v=false unknown.expected=false unrelated.txt=false
    |}]
;;
