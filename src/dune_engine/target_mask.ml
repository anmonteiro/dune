open Import

type basename =
  | Any
  | Name of Filename.t
  | Extensions of Filename.Extension.Set.t
  | Matching of Predicate_lang.Glob.t list

type region =
  | Subtree of Path.Build.t * basename
  | In_directory of Path.Build.t * basename

let compare_basename a b =
  match a, b with
  | Any, Any -> Ordering.Eq
  | Any, _ -> Ordering.Lt
  | _, Any -> Ordering.Gt
  | Name a, Name b -> Filename.compare a b
  | Name _, _ -> Ordering.Lt
  | _, Name _ -> Ordering.Gt
  | Extensions a, Extensions b -> Filename.Extension.Set.compare a b
  | Extensions _, _ -> Ordering.Lt
  | _, Extensions _ -> Ordering.Gt
  | Matching a, Matching b -> List.compare a b ~compare:Predicate_lang.Glob.compare
;;

let compare_region a b =
  match a, b with
  | Subtree (a, names_a), Subtree (b, names_b)
  | In_directory (a, names_a), In_directory (b, names_b) ->
    let open Ordering.O in
    let= () = Path.Build.compare a b in
    compare_basename names_a names_b
  | Subtree _, In_directory _ -> Ordering.Lt
  | In_directory _, Subtree _ -> Ordering.Gt
;;

type t =
  { files : region list
  ; directories : region list
  ; aliases : region list
  }

let empty = { files = []; directories = []; aliases = [] }

let subtree dir =
  let regions = [ Subtree (dir, Any) ] in
  { files = regions; directories = regions; aliases = regions }
;;

let all = subtree Path.Build.root

let union a b =
  { files = a.files @ b.files
  ; directories = a.directories @ b.directories
  ; aliases = a.aliases @ b.aliases
  }
;;

let basename_mem selector name =
  match selector with
  | Any -> true
  | Name other -> Filename.equal name other
  | Extensions extensions ->
    Filename.Extension.Set.exists extensions ~f:(fun extension ->
      Filename.check_suffix
        (Filename.to_string name)
        (Filename.Extension.to_string extension))
  | Matching predicates ->
    List.for_all predicates ~f:(fun predicate ->
      Predicate_lang.Glob.test
        predicate
        ~standard:Predicate_lang.false_
        (Filename.to_string name))
;;

let extension_predicate extensions =
  let glob = Filename.Extension.Set.to_list extensions |> Glob.matching_extensions in
  (* A leading [**] preserves suffix matches for dot-prefixed filenames when
     the predicate language round-trips the glob through its representation. *)
  Glob.of_string ("*" ^ Glob.to_string glob) |> Predicate_lang.Glob.of_glob
;;

let matching predicates =
  Matching (List.sort_uniq predicates ~compare:Predicate_lang.Glob.compare)
;;

let basename_inter a b =
  match a, b with
  | Any, x | x, Any -> Some x
  | Name name, other | other, Name name ->
    Option.some_if (basename_mem other name) (Name name)
  | Extensions a, Extensions b ->
    let extensions =
      Filename.Extension.Set.to_list a
      |> List.concat_map ~f:(fun a ->
        Filename.Extension.Set.to_list b
        |> List.filter_map ~f:(fun b ->
          let a_text = Filename.Extension.to_string a in
          let b_text = Filename.Extension.to_string b in
          if Filename.check_suffix a_text b_text
          then Some a
          else if Filename.check_suffix b_text a_text
          then Some b
          else None))
      |> Filename.Extension.Set.of_list
    in
    Option.some_if
      (not (Filename.Extension.Set.is_empty extensions))
      (Extensions extensions)
  | Matching a, Matching b -> Some (matching (a @ b))
  | Matching predicates, Extensions extensions
  | Extensions extensions, Matching predicates ->
    let extensions =
      Filename.Extension.Set.filter extensions ~f:(fun extension ->
        List.for_all predicates ~f:(fun predicate ->
          Predicate_lang.Glob.may_match_suffix
            predicate
            (Filename.Extension.to_string extension)))
    in
    if Filename.Extension.Set.is_empty extensions
    then None
    else Some (matching (extension_predicate extensions :: predicates))
;;

let region_inter ~include_root a b =
  match a, b with
  | Subtree (a, names_a), Subtree (b, names_b) ->
    let root =
      if Path.Build.is_descendant a ~of_:b
      then Some a
      else if Path.Build.is_descendant b ~of_:a
      then Some b
      else None
    in
    Option.bind root ~f:(fun root ->
      Option.map (basename_inter names_a names_b) ~f:(fun names -> Subtree (root, names)))
  | In_directory (a, names_a), In_directory (b, names_b) ->
    if not (Path.Build.equal a b)
    then None
    else
      Option.map (basename_inter names_a names_b) ~f:(fun names ->
        In_directory (a, names))
  | Subtree (root, subtree_names), In_directory (dir, names)
  | In_directory (dir, names), Subtree (root, subtree_names) ->
    if Path.Build.is_descendant dir ~of_:root
    then
      Option.map (basename_inter subtree_names names) ~f:(fun names ->
        In_directory (dir, names))
    else (
      match Path.Build.parent root with
      | Some parent when include_root && Path.Build.equal parent dir ->
        let name = Path.Build.basename root in
        Option.some_if
          (basename_mem names name && basename_mem subtree_names name)
          (In_directory (dir, Name name))
      | None | Some _ -> None)
;;

let inter_regions ~include_root a b =
  (* Repeated restrictions must not multiply equivalent regions through each
     Cartesian intersection. *)
  List.concat_map a ~f:(fun a -> List.filter_map b ~f:(region_inter ~include_root a))
  |> List.sort_uniq ~compare:compare_region
;;

let inter a b =
  { files = inter_regions ~include_root:true a.files b.files
  ; directories = inter_regions ~include_root:true a.directories b.directories
  ; aliases = inter_regions ~include_root:false a.aliases b.aliases
  }
;;

let is_empty { files; directories; aliases } =
  List.is_empty files && List.is_empty directories && List.is_empty aliases
;;

let intersects_regions ~include_root a b =
  List.exists a ~f:(fun a ->
    List.exists b ~f:(fun b -> Option.is_some (region_inter ~include_root a b)))
;;

let intersects a b =
  intersects_regions ~include_root:true a.files b.files
  || intersects_regions ~include_root:true a.directories b.directories
  || intersects_regions ~include_root:false a.aliases b.aliases
;;

let intersects_directory t dir =
  let below = [ Subtree (dir, Any) ] in
  intersects_regions ~include_root:false t.files below
  || intersects_regions ~include_root:true t.directories below
  || intersects_regions ~include_root:false t.aliases below
;;

let mem ~include_root regions path =
  let location =
    Option.map (Path.Build.parent path) ~f:(fun parent ->
      parent, Path.Build.basename path)
  in
  List.exists regions ~f:(function
    | Subtree (root, names) ->
      Path.Build.is_descendant path ~of_:root
      && (include_root || not (Path.Build.equal root path))
      &&
        (match location with
        | None ->
          (match names with
           | Any -> true
           | Name _ | Extensions _ | Matching _ -> false)
        | Some (_, basename) -> basename_mem names basename)
    | In_directory (dir, names) ->
      (match location with
       | None -> false
       | Some (parent, basename) ->
         Path.Build.equal dir parent && basename_mem names basename))
;;

let mem_file t path = mem ~include_root:true t.files path
let mem_directory t path = mem ~include_root:true t.directories path

let alias_path alias =
  Path.Build.relative (Alias.dir alias) (Alias.Name.to_string (Alias.name alias))
;;

let mem_alias t alias = mem ~include_root:false t.aliases (alias_path alias)

let exact_paths paths =
  List.map paths ~f:(fun path ->
    In_directory (Path.Build.parent_exn path, Name (Path.Build.basename path)))
;;

let files paths = { empty with files = exact_paths paths }
let directories paths = { empty with directories = exact_paths paths }

let aliases aliases =
  { empty with aliases = List.map aliases ~f:alias_path |> exact_paths }
;;

let files_in_directory dir = { empty with files = [ In_directory (dir, Any) ] }

let directories_in_directory dir =
  { empty with directories = [ In_directory (dir, Any) ] }
;;

let aliases_in_directory dir = { empty with aliases = [ In_directory (dir, Any) ] }

let file_extensions ~dir extensions =
  if Filename.Extension.Set.is_empty extensions
  then empty
  else { empty with files = [ In_directory (dir, Extensions extensions) ] }
;;

let file_extensions_in_subtree ~dir extensions =
  if Filename.Extension.Set.is_empty extensions
  then empty
  else { empty with files = [ Subtree (dir, Extensions extensions) ] }
;;

let files_matching ~dir predicate =
  match Predicate_lang.Glob.finite_elements predicate with
  | Some names ->
    let files =
      String.Set.to_list names
      |> List.filter_map ~f:(fun name ->
        Option.map (Filename.of_string name) ~f:(fun name ->
          In_directory (dir, Name name)))
    in
    { empty with files }
  | None -> { empty with files = [ In_directory (dir, Matching [ predicate ]) ] }
;;

let path path = union (files [ path ]) (directories [ path ])

let paths_matching ~dir predicate =
  let mask = files_matching ~dir predicate in
  { mask with directories = mask.files }
;;

let alias_directories t ~dir =
  let singleton path =
    match Path.Local_gen.descendant path ~of_:dir with
    | None -> Dir_set.empty
    | Some relative -> Dir_set.singleton relative
  in
  List.fold_left t.aliases ~init:Dir_set.empty ~f:(fun directories region ->
    let more =
      match region with
      | In_directory (path, _) -> singleton path
      | Subtree (root, _) ->
        if Path.Build.is_descendant dir ~of_:root
        then Dir_set.universal
        else (
          match Path.Local_gen.descendant root ~of_:dir with
          | None -> Dir_set.empty
          | Some relative -> Dir_set.subtree relative)
    in
    Dir_set.union directories more)
;;
