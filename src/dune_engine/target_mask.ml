open Import

type names =
  { values : Filename.Set.t
  ; first : Filename.t
  ; last : Filename.t
    (* This upper bound lets intersections traverse the smaller input without
     counting every element again after each union. *)
  ; weight : int
  }

type basename =
  | Any
  | Name of Filename.t
  | Names of names
  | Extensions of Filename.Extension.Set.t
  | Matching of Predicate_lang.Glob.t list

type locations =
  | Empty
  | One of Path.Build.t * basename list
  | Many of
      { by_dir : basename list Path.Build.Map.t
      ; count : int
      }

type regions =
  { direct : locations
  ; recursive : locations
  }

type t =
  { files : regions
  ; directories : regions
  ; aliases : regions
  }

let empty_locations = Empty
let empty_regions = { direct = empty_locations; recursive = empty_locations }

let empty =
  { files = empty_regions; directories = empty_regions; aliases = empty_regions }
;;

let singleton_location dir names = One (dir, names)

let names values =
  match Filename.Set.min_elt values with
  | None -> None
  | Some first ->
    let last = Filename.Set.max_elt values |> Option.value_exn in
    if Filename.equal first last
    then Some (Name first)
    else Some (Names { values; first; last; weight = Filename.Set.cardinal values })
;;

let name value = Name value

let regions_empty { direct; recursive } =
  match direct, recursive with
  | Empty, Empty -> true
  | _ -> false
;;

let is_empty { files; directories; aliases } =
  regions_empty files && regions_empty directories && regions_empty aliases
;;

let is_file_only { directories; aliases; _ } =
  regions_empty directories && regions_empty aliases
;;

let single_path { files; directories; aliases } =
  match files, directories with
  | ( { direct = One (dir, [ Name name ]); recursive = Empty }
    , { direct = One (other_dir, [ Name other_name ]); recursive = Empty } )
    when regions_empty aliases
         && Path.Build.equal dir other_dir
         && Filename.equal name other_name -> Some (dir, name)
  | _ -> None
;;

let exact_file_names { files; _ } =
  match files.recursive with
  | One _ | Many _ -> None
  | Empty ->
    let add dir selectors acc =
      (* [add_basename] coalesces all exact selectors at each location. *)
      match acc, selectors with
      | Some acc, [] -> Some (Path.Build.Map.set acc dir Filename.Set.empty)
      | Some acc, [ Name name ] ->
        Some (Path.Build.Map.set acc dir (Filename.Set.singleton name))
      | Some acc, [ Names { values; _ } ] -> Some (Path.Build.Map.set acc dir values)
      | _ -> None
    in
    (match files.direct with
     | Empty -> Some Path.Build.Map.empty
     | One (dir, selectors) -> add dir selectors (Some Path.Build.Map.empty)
     | Many { by_dir; _ } ->
       Path.Build.Map.foldi by_dir ~init:(Some Path.Build.Map.empty) ~f:add)
;;

let file_name_bounds { files; _ } ~dir =
  let rec exact = function
    | [] -> true
    | (Name _ | Names _) :: rest -> exact rest
    | (Any | Extensions _ | Matching _) :: _ -> false
  in
  let rec bounds = function
    | [] -> `Empty
    | Name name :: rest -> add_bounds name name rest
    | Names { first; last; _ } :: rest -> add_bounds first last rest
    | (Any | Extensions _ | Matching _) :: _ -> `Non_exact
  and add_bounds first last rest =
    match bounds rest with
    | `Non_exact -> `Non_exact
    | `Empty -> `Bounds (first, last)
    | `Bounds (other_first, other_last) ->
      `Bounds
        ( Ordering.min Filename.compare first other_first
        , Ordering.max Filename.compare last other_last )
  in
  match files.recursive with
  | One _ | Many _ -> `Non_exact
  | Empty ->
    (match files.direct with
     | Empty -> `Empty
     | One (other_dir, selectors) ->
       if Path.Build.equal dir other_dir
       then bounds selectors
       else if exact selectors
       then `Empty
       else `Non_exact
     | Many { by_dir; _ } ->
       if Path.Build.Map.for_all by_dir ~f:exact
       then (
         match Path.Build.Map.find by_dir dir with
         | None -> `Empty
         | Some selectors -> bounds selectors)
       else `Non_exact)
;;

let disjoint_name_ranges a b =
  Filename.compare a.last b.first = Lt || Filename.compare b.last a.first = Lt
;;

let basename_mem selector name =
  match selector with
  | Any -> true
  | Name other -> Filename.equal name other
  | Names { values; first; last; _ } ->
    Filename.compare name first <> Lt
    && Filename.compare name last <> Gt
    && Filename.Set.mem values name
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

let rec basenames_mem names name =
  match names with
  | [] -> false
  | first :: rest -> basename_mem first name || basenames_mem rest name
;;

let rec add_basename names selectors =
  match names, selectors with
  | Any, _ | _, Any :: _ -> [ Any ]
  | _, [] -> [ names ]
  | Name a, Name b :: rest ->
    if Filename.equal a b
    then selectors
    else
      Names
        { values = Filename.Set.of_list [ a; b ]
        ; first = Ordering.min Filename.compare a b
        ; last = Ordering.max Filename.compare a b
        ; weight = 2
        }
      :: rest
  | Name a, Names b :: rest ->
    let values = Filename.Set.add b.values a in
    if values == b.values
    then selectors
    else
      Names
        { values
        ; first = Ordering.min Filename.compare a b.first
        ; last = Ordering.max Filename.compare a b.last
        ; weight = b.weight + 1
        }
      :: rest
  | Names a, Name b :: rest ->
    let values = Filename.Set.add a.values b in
    let names =
      if values == a.values
      then a
      else
        { values
        ; first = Ordering.min Filename.compare a.first b
        ; last = Ordering.max Filename.compare a.last b
        ; weight = a.weight + 1
        }
    in
    Names names :: rest
  | Names a, Names b :: rest ->
    Names
      { values = Filename.Set.union a.values b.values
      ; first = Ordering.min Filename.compare a.first b.first
      ; last = Ordering.max Filename.compare a.last b.last
      ; weight = a.weight + b.weight
      }
    :: rest
  | Extensions a, Extensions b :: rest ->
    Extensions (Filename.Extension.Set.union a b) :: rest
  | Matching a, Matching b :: _ when List.equal Predicate_lang.Glob.equal a b -> selectors
  | _, first :: rest -> first :: add_basename names rest
;;

let union_basenames a b =
  if a == b
  then a
  else List.fold_left a ~init:b ~f:(fun acc names -> add_basename names acc)
;;

let union_locations a b =
  if a == b
  then a
  else (
    match a, b with
    | a, Empty -> a
    | Empty, b -> b
    | One (dir_a, names_a), One (dir_b, names_b) ->
      if Path.Build.equal dir_a dir_b
      then (
        let names = union_basenames names_a names_b in
        if names == names_a
        then a
        else if names == names_b
        then b
        else singleton_location dir_a names)
      else
        Many
          { by_dir =
              Path.Build.Map.set (Path.Build.Map.singleton dir_a names_a) dir_b names_b
          ; count = 2
          }
    | One (dir, names), Many { by_dir; count } ->
      let count = ref count in
      let by_dir =
        Path.Build.Map.update by_dir dir ~f:(function
          | None ->
            incr count;
            Some names
          | Some other -> Some (union_basenames names other))
      in
      Many { by_dir; count = !count }
    | Many { by_dir; count }, One (dir, names) ->
      let count = ref count in
      let by_dir =
        Path.Build.Map.update by_dir dir ~f:(function
          | None ->
            incr count;
            Some names
          | Some other -> Some (union_basenames other names))
      in
      Many { by_dir; count = !count }
    | Many a, Many b ->
      let count = ref (a.count + b.count) in
      let by_dir =
        Path.Build.Map.union a.by_dir b.by_dir ~f:(fun _ a b ->
          decr count;
          Some (union_basenames a b))
      in
      Many { by_dir; count = !count })
;;

let subtree dir =
  let regions = { empty_regions with recursive = singleton_location dir [ Any ] } in
  { files = regions; directories = regions; aliases = regions }
;;

let all = subtree Path.Build.root

let extension_predicate extensions =
  let glob = Filename.Extension.Set.to_list extensions |> Glob.matching_extensions in
  (* A leading [**] preserves suffix matches for dot-prefixed filenames when
     the predicate language round-trips the glob through its representation. *)
  Glob.of_string ("*" ^ Glob.to_string glob) |> Predicate_lang.Glob.of_glob
;;

let matching predicates =
  Matching (List.sort_uniq predicates ~compare:Predicate_lang.Glob.compare)
;;

let smaller_names a b = if a.weight <= b.weight then a, b else b, a

let basename_inter a b =
  match a, b with
  | Any, x | x, Any -> Some x
  | Name name, other | other, Name name ->
    Option.some_if (basename_mem other name) (Name name)
  | Names a, Names b ->
    if disjoint_name_ranges a b
    then None
    else (
      let a, b = smaller_names a b in
      Filename.Set.filter a.values ~f:(Filename.Set.mem b.values) |> names)
  | Names { values; _ }, other | other, Names { values; _ } ->
    Filename.Set.filter values ~f:(basename_mem other) |> names
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

let basename_intersects a b =
  match a, b with
  | Any, _ | _, Any -> true
  | Name name, other | other, Name name -> basename_mem other name
  | Names a, Names b ->
    (not (disjoint_name_ranges a b))
    &&
    if a.weight <= b.weight
    then not (Filename.Set.are_disjoint a.values b.values)
    else not (Filename.Set.are_disjoint b.values a.values)
  | Names { values; _ }, other | other, Names { values; _ } ->
    Filename.Set.exists values ~f:(basename_mem other)
  | Extensions a, Extensions b ->
    Filename.Extension.Set.exists a ~f:(fun a ->
      Filename.Extension.Set.exists b ~f:(fun b ->
        let a = Filename.Extension.to_string a in
        let b = Filename.Extension.to_string b in
        Filename.check_suffix a b || Filename.check_suffix b a))
  | Matching _, Matching _ -> true
  | Matching predicates, Extensions extensions
  | Extensions extensions, Matching predicates ->
    Filename.Extension.Set.exists extensions ~f:(fun extension ->
      List.for_all predicates ~f:(fun predicate ->
        Predicate_lang.Glob.may_match_suffix
          predicate
          (Filename.Extension.to_string extension)))
;;

let rec basename_intersects_any a = function
  | [] -> false
  | b :: rest -> basename_intersects a b || basename_intersects_any a rest
;;

let rec basenames_intersects a b =
  match a with
  | [] -> false
  | a :: rest -> basename_intersects_any a b || basenames_intersects rest b
;;

let inter_basenames a b =
  if a == b
  then a
  else
    List.fold_left a ~init:[] ~f:(fun acc a ->
      List.fold_left b ~init:acc ~f:(fun acc b ->
        match basename_inter a b with
        | None -> acc
        | Some names -> add_basename names acc))
;;

let add_location locations dir names =
  match names with
  | [] -> locations
  | _ :: _ ->
    (match locations with
     | Empty -> singleton_location dir names
     | One (previous_dir, previous) ->
       if Path.Build.equal previous_dir dir
       then (
         let names = union_basenames names previous in
         if names == previous then locations else singleton_location dir names)
       else
         Many
           { by_dir =
               Path.Build.Map.set
                 (Path.Build.Map.singleton previous_dir previous)
                 dir
                 names
           ; count = 2
           }
     | Many { by_dir; count } ->
       let count = ref count in
       let by_dir =
         Path.Build.Map.update by_dir dir ~f:(function
           | None ->
             incr count;
             Some names
           | Some previous -> Some (union_basenames names previous))
       in
       Many { by_dir; count = !count })
;;

let inter_locations_many a b =
  Path.Build.Map.foldi a ~init:empty_locations ~f:(fun dir names acc ->
    match Path.Build.Map.find b dir with
    | None -> acc
    | Some other -> add_location acc dir (inter_basenames names other))
;;

let inter_locations a b =
  if a == b
  then a
  else (
    match a, b with
    | Empty, _ | _, Empty -> empty_locations
    | One (dir_a, names_a), One (dir_b, names_b) ->
      if Path.Build.equal dir_a dir_b
      then add_location empty_locations dir_a (inter_basenames names_a names_b)
      else empty_locations
    | One (dir, names), Many { by_dir; _ } | Many { by_dir; _ }, One (dir, names) ->
      (match Path.Build.Map.find by_dir dir with
       | None -> empty_locations
       | Some other -> add_location empty_locations dir (inter_basenames names other))
    | Many a, Many b ->
      if a.count <= b.count
      then inter_locations_many a.by_dir b.by_dir
      else inter_locations_many b.by_dir a.by_dir)
;;

let nested_root a b =
  if Path.Build.is_descendant a ~of_:b
  then Some a
  else if Path.Build.is_descendant b ~of_:a
  then Some b
  else None
;;

let inter_recursive_location a names_a b names_b acc =
  match nested_root a b with
  | None -> acc
  | Some root -> add_location acc root (inter_basenames names_a names_b)
;;

let inter_recursive a b =
  match a, b with
  | Empty, _ | _, Empty -> empty_locations
  | One (a, names_a), One (b, names_b) ->
    inter_recursive_location a names_a b names_b empty_locations
  | One (a, names_a), Many { by_dir; _ } ->
    Path.Build.Map.foldi
      by_dir
      ~init:empty_locations
      ~f:(inter_recursive_location a names_a)
  | Many { by_dir; _ }, One (b, names_b) ->
    Path.Build.Map.foldi by_dir ~init:empty_locations ~f:(fun a names_a acc ->
      inter_recursive_location a names_a b names_b acc)
  | Many a, Many b ->
    Path.Build.Map.foldi a.by_dir ~init:empty_locations ~f:(fun a names_a acc ->
      Path.Build.Map.foldi b.by_dir ~init:acc ~f:(inter_recursive_location a names_a))
;;

let direct_contains_root ~include_root ~dir ~root =
  include_root
  && Path.Build.is_descendant root ~of_:dir
  && (not (Path.Build.equal root dir))
  &&
  (* Strict descent puts [start] inside the canonical path. The build root
     itself has no prefix before its children's names. *)
  let start =
    if Path.Build.is_root dir then 0 else String.length (Path.Local_gen.to_string dir) + 1
  in
  String.index_from_unchecked (Path.Local_gen.to_string root) start '/' = -1
;;

let inter_recursive_direct_location ~include_root root names_a dir names_b acc =
  if Path.Build.is_descendant dir ~of_:root
  then add_location acc dir (inter_basenames names_a names_b)
  else if direct_contains_root ~include_root ~dir ~root
  then (
    let basename = Path.Build.basename root in
    if basenames_mem names_a basename && basenames_mem names_b basename
    then add_location acc dir [ name basename ]
    else acc)
  else acc
;;

let inter_recursive_direct ~include_root recursive direct =
  match recursive, direct with
  | Empty, _ | _, Empty -> empty_locations
  | One (root, names_a), One (dir, names_b) ->
    inter_recursive_direct_location ~include_root root names_a dir names_b empty_locations
  | One (root, names_a), Many { by_dir; _ } ->
    Path.Build.Map.foldi
      by_dir
      ~init:empty_locations
      ~f:(inter_recursive_direct_location ~include_root root names_a)
  | Many { by_dir; _ }, One (dir, names_b) ->
    Path.Build.Map.foldi by_dir ~init:empty_locations ~f:(fun root names_a acc ->
      inter_recursive_direct_location ~include_root root names_a dir names_b acc)
  | Many recursive, Many direct ->
    Path.Build.Map.foldi
      recursive.by_dir
      ~init:empty_locations
      ~f:(fun root names_a acc ->
        Path.Build.Map.foldi
          direct.by_dir
          ~init:acc
          ~f:(inter_recursive_direct_location ~include_root root names_a))
;;

let direct_inside_subtree ~include_root ~root dir names =
  Path.Build.is_descendant dir ~of_:root
  ||
  match names with
  | [ Name name ] ->
    direct_contains_root ~include_root ~dir ~root
    && Filename.equal name (Path.Build.basename root)
  | _ ->
    (* Outside the subtree, only its root can be included. Exact names are
       merged by [add_basename], and [Names] has at least two distinct names. *)
    false
;;

let inside_subtree ~include_root regions root =
  (match regions.direct with
   | Empty -> true
   | One (dir, names) -> direct_inside_subtree ~include_root ~root dir names
   | Many { by_dir; _ } ->
     Path.Build.Map.for_alli by_dir ~f:(direct_inside_subtree ~include_root ~root))
  &&
  match regions.recursive with
  | Empty -> true
  | One (dir, _) -> Path.Build.is_descendant dir ~of_:root
  | Many { by_dir; _ } ->
    Path.Build.Map.for_alli by_dir ~f:(fun dir _ ->
      Path.Build.is_descendant dir ~of_:root)
;;

let subtree_contains ~include_root parent child =
  match parent.recursive with
  | One (root, [ Any ]) -> inside_subtree ~include_root child root
  | Empty | One _ -> false
  | Many { by_dir; _ } ->
    let direct dir names =
      Path.Build.Map.existsi by_dir ~f:(fun root -> function
        | [ Any ] -> direct_inside_subtree ~include_root ~root dir names
        | _ -> false)
    in
    let recursive dir =
      Path.Build.Map.existsi by_dir ~f:(fun root -> function
        | [ Any ] -> Path.Build.is_descendant dir ~of_:root
        | _ -> false)
    in
    (match child.direct with
     | Empty -> true
     | One (dir, names) -> direct dir names
     | Many { by_dir; _ } -> Path.Build.Map.for_alli by_dir ~f:direct)
    &&
      (match child.recursive with
      | Empty -> true
      | One (dir, _) -> recursive dir
      | Many { by_dir; _ } ->
        Path.Build.Map.for_alli by_dir ~f:(fun dir _ -> recursive dir))
;;

let union_regions ~include_root a b =
  if a == b || regions_empty b
  then a
  else if regions_empty a
  then b
  else if subtree_contains ~include_root a b
  then a
  else if subtree_contains ~include_root b a
  then b
  else (
    let direct = union_locations a.direct b.direct in
    let recursive = union_locations a.recursive b.recursive in
    if direct == a.direct && recursive == a.recursive
    then a
    else if direct == b.direct && recursive == b.recursive
    then b
    else { direct; recursive })
;;

let union a b =
  if a == b || is_empty b
  then a
  else if is_empty a
  then b
  else (
    let files = union_regions ~include_root:true a.files b.files in
    let directories =
      if a.files == a.directories && b.files == b.directories
      then files
      else union_regions ~include_root:true a.directories b.directories
    in
    let aliases = union_regions ~include_root:false a.aliases b.aliases in
    if files == a.files && directories == a.directories && aliases == a.aliases
    then a
    else if files == b.files && directories == b.directories && aliases == b.aliases
    then b
    else { files; directories; aliases })
;;

let inter_regions ~include_root a b =
  if a == b || regions_empty a
  then a
  else if regions_empty b
  then b
  else if subtree_contains ~include_root a b
  then b
  else if subtree_contains ~include_root b a
  then a
  else
    { direct =
        union_locations
          (inter_locations a.direct b.direct)
          (union_locations
             (inter_recursive_direct ~include_root a.recursive b.direct)
             (inter_recursive_direct ~include_root b.recursive a.direct))
    ; recursive = inter_recursive a.recursive b.recursive
    }
;;

let inter a b =
  if a == b || is_empty a
  then a
  else if is_empty b
  then b
  else (
    let files = inter_regions ~include_root:true a.files b.files in
    let directories =
      if a.files == a.directories && b.files == b.directories
      then files
      else inter_regions ~include_root:true a.directories b.directories
    in
    let aliases = inter_regions ~include_root:false a.aliases b.aliases in
    if files == a.files && directories == a.directories && aliases == a.aliases
    then a
    else if files == b.files && directories == b.directories && aliases == b.aliases
    then b
    else { files; directories; aliases })
;;

let intersects_locations_many a b =
  Path.Build.Map.existsi a ~f:(fun dir names ->
    match Path.Build.Map.find b dir with
    | None -> false
    | Some other -> basenames_intersects names other)
;;

let intersects_locations a b =
  match a, b with
  | Empty, _ | _, Empty -> false
  | One (dir_a, names_a), One (dir_b, names_b) ->
    Path.Build.equal dir_a dir_b && basenames_intersects names_a names_b
  | One (dir, names), Many { by_dir; _ } | Many { by_dir; _ }, One (dir, names) ->
    (match Path.Build.Map.find by_dir dir with
     | None -> false
     | Some other -> basenames_intersects names other)
  | Many a, Many b ->
    if a.count <= b.count
    then intersects_locations_many a.by_dir b.by_dir
    else intersects_locations_many b.by_dir a.by_dir
;;

let recursive_intersects a names_a b names_b =
  (Path.Build.is_descendant a ~of_:b || Path.Build.is_descendant b ~of_:a)
  && basenames_intersects names_a names_b
;;

let intersects_recursive a b =
  match a, b with
  | Empty, _ | _, Empty -> false
  | One (a, names_a), One (b, names_b) -> recursive_intersects a names_a b names_b
  | One (a, names_a), Many { by_dir; _ } ->
    Path.Build.Map.existsi by_dir ~f:(recursive_intersects a names_a)
  | Many { by_dir; _ }, One (b, names_b) ->
    Path.Build.Map.existsi by_dir ~f:(recursive_intersects b names_b)
  | Many a, Many b ->
    Path.Build.Map.existsi a.by_dir ~f:(fun a names_a ->
      Path.Build.Map.existsi b.by_dir ~f:(recursive_intersects a names_a))
;;

let recursive_intersects_direct ~include_root root names_a dir names_b =
  if Path.Build.is_descendant dir ~of_:root
  then basenames_intersects names_a names_b
  else if direct_contains_root ~include_root ~dir ~root
  then (
    let name = Path.Build.basename root in
    basenames_mem names_a name && basenames_mem names_b name)
  else false
;;

let intersects_recursive_direct ~include_root recursive direct =
  match recursive, direct with
  | Empty, _ | _, Empty -> false
  | One (root, names_a), One (dir, names_b) ->
    recursive_intersects_direct ~include_root root names_a dir names_b
  | One (root, names_a), Many { by_dir; _ } ->
    Path.Build.Map.existsi
      by_dir
      ~f:(recursive_intersects_direct ~include_root root names_a)
  | Many { by_dir; _ }, One (dir, names_b) ->
    Path.Build.Map.existsi by_dir ~f:(fun root names_a ->
      recursive_intersects_direct ~include_root root names_a dir names_b)
  | Many recursive, Many direct ->
    Path.Build.Map.existsi recursive.by_dir ~f:(fun root names_a ->
      Path.Build.Map.existsi
        direct.by_dir
        ~f:(recursive_intersects_direct ~include_root root names_a))
;;

let intersects_regions ~include_root a b =
  (not (regions_empty a || regions_empty b))
  && (intersects_locations a.direct b.direct
      || intersects_recursive a.recursive b.recursive
      || intersects_recursive_direct ~include_root a.recursive b.direct
      || intersects_recursive_direct ~include_root b.recursive a.direct)
;;

let intersects a b =
  intersects_regions ~include_root:true a.files b.files
  || ((a.files != a.directories || b.files != b.directories)
      && intersects_regions ~include_root:true a.directories b.directories)
  || intersects_regions ~include_root:false a.aliases b.aliases
;;

let intersects_directory t dir =
  let below = { empty_regions with recursive = singleton_location dir [ Any ] } in
  intersects_regions ~include_root:false t.files below
  || intersects_regions ~include_root:true t.directories below
  || intersects_regions ~include_root:false t.aliases below
;;

let mem_direct locations dir name =
  match locations with
  | Empty -> false
  | One (root, names) -> Path.Build.equal dir root && basenames_mem names name
  | Many { by_dir; _ } ->
    (match Path.Build.Map.find by_dir dir with
     | None -> false
     | Some names -> basenames_mem names name)
;;

let rec mem_ancestor by_dir dir name =
  match Path.Build.Map.find by_dir dir with
  | Some names when basenames_mem names name -> true
  | None | Some _ ->
    (match Path.Build.parent dir with
     | None -> false
     | Some parent -> mem_ancestor by_dir parent name)
;;

let mem_recursive locations dir name =
  match locations with
  | Empty -> false
  | One (root, names) ->
    Path.Build.is_descendant dir ~of_:root && basenames_mem names name
  | Many { by_dir; _ } -> mem_ancestor by_dir dir name
;;

let mem ~include_root regions path =
  if regions_empty regions
  then false
  else (
    match Path.Build.parent path with
    | None ->
      include_root
      &&
        (match regions.recursive with
        | Empty -> false
        | One (root, names) ->
          Path.Build.equal root path
          && List.exists names ~f:(function
            | Any -> true
            | _ -> false)
        | Many { by_dir; _ } ->
          (match Path.Build.Map.find by_dir path with
           | None -> false
           | Some names ->
             List.exists names ~f:(function
               | Any -> true
               | _ -> false)))
    | Some parent ->
      let name = Path.Build.basename path in
      mem_direct regions.direct parent name
      || mem_recursive regions.recursive (if include_root then path else parent) name)
;;

let mem_file t path = mem ~include_root:true t.files path
let mem_directory t path = mem ~include_root:true t.directories path

let mem_recursive_name recursive ~dir name =
  match recursive with
  | Empty -> false
  | One (root, names) ->
    basenames_mem names name
    && (Path.Build.is_descendant dir ~of_:root
        || (Path.Build.is_descendant root ~of_:dir
            && Path.Build.equal root (Path.Build.relative_fname dir name)))
  | Many { by_dir; _ } ->
    mem_ancestor by_dir dir name
    ||
      (match Path.Build.Map.find by_dir (Path.Build.relative_fname dir name) with
      | None -> false
      | Some names -> basenames_mem names name)
;;

let mem_name { direct; recursive } ~dir name =
  mem_direct direct dir name || mem_recursive_name recursive ~dir name
;;

let mem_file_name t ~dir name = mem_name t.files ~dir name
let mem_directory_name t ~dir name = mem_name t.directories ~dir name

let mem_path { files; directories; _ } ~dir name =
  mem_direct files.direct dir name
  || mem_direct directories.direct dir name
  || mem_recursive_name files.recursive ~dir name
  || mem_recursive_name directories.recursive ~dir name
;;

let alias_path alias =
  Path.Build.relative (Alias.dir alias) (Alias.Name.to_string (Alias.name alias))
;;

let mem_alias t alias =
  if regions_empty t.aliases
  then false
  else (
    match Filename.of_string (Alias.Name.to_string (Alias.name alias)) with
    | None -> mem ~include_root:false t.aliases (alias_path alias)
    | Some name ->
      let dir = Alias.dir alias in
      mem_direct t.aliases.direct dir name || mem_recursive t.aliases.recursive dir name)
;;

let exact_names dir values =
  match names values with
  | None -> empty_regions
  | Some names -> { empty_regions with direct = singleton_location dir [ names ] }
;;

let files_named ~dir names = { empty with files = exact_names dir names }

let of_targets { Targets.Validated.root; files; dirs } =
  { empty with files = exact_names root files; directories = exact_names root dirs }
;;

let paths ~dir names =
  let regions = exact_names dir names in
  if regions_empty regions
  then empty
  else { empty with files = regions; directories = regions }
;;

let exact_paths paths =
  let direct =
    List.fold_left paths ~init:empty_locations ~f:(fun acc path ->
      add_location acc (Path.Build.parent_exn path) [ name (Path.Build.basename path) ])
  in
  { empty_regions with direct }
;;

let files paths = { empty with files = exact_paths paths }
let directories paths = { empty with directories = exact_paths paths }

let aliases aliases =
  { empty with aliases = List.map aliases ~f:alias_path |> exact_paths }
;;

let files_in_directory dir =
  { empty with files = { empty_regions with direct = singleton_location dir [ Any ] } }
;;

let directories_in_directory dir =
  { empty with
    directories = { empty_regions with direct = singleton_location dir [ Any ] }
  }
;;

let aliases_in_directory dir =
  { empty with aliases = { empty_regions with direct = singleton_location dir [ Any ] } }
;;

let file_extensions ~dir extensions =
  if Filename.Extension.Set.is_empty extensions
  then empty
  else
    { empty with
      files =
        { empty_regions with direct = singleton_location dir [ Extensions extensions ] }
    }
;;

let file_extensions_in_subtree ~dir extensions =
  if Filename.Extension.Set.is_empty extensions
  then empty
  else
    { empty with
      files =
        { empty_regions with
          recursive = singleton_location dir [ Extensions extensions ]
        }
    }
;;

let files_matching ~dir predicate =
  let selectors =
    match Predicate_lang.Glob.finite_elements predicate with
    | Some values ->
      let values =
        String.Set.to_list values
        |> List.filter_map ~f:Filename.of_string
        |> Filename.Set.of_list
      in
      (match names values with
       | None -> []
       | Some names -> [ names ])
    | None -> [ Matching [ predicate ] ]
  in
  let direct = add_location empty_locations dir selectors in
  { empty with files = { empty_regions with direct } }
;;

let path path =
  let regions = exact_paths [ path ] in
  { empty with files = regions; directories = regions }
;;

let paths_matching ~dir predicate =
  let mask = files_matching ~dir predicate in
  { mask with directories = mask.files }
;;

let alias_directories t ~dir =
  if regions_empty t.aliases
  then Dir_set.empty
  else (
    let add_direct path _ acc =
      match Path.Local_gen.descendant path ~of_:dir with
      | None -> acc
      | Some relative -> Dir_set.union acc (Dir_set.singleton relative)
    in
    let direct =
      match t.aliases.direct with
      | Empty -> Dir_set.empty
      | One (path, names) -> add_direct path names Dir_set.empty
      | Many { by_dir; _ } ->
        Path.Build.Map.foldi by_dir ~init:Dir_set.empty ~f:add_direct
    in
    let add_recursive root _ acc =
      if Path.Build.is_descendant dir ~of_:root
      then Dir_set.universal
      else (
        match Path.Local_gen.descendant root ~of_:dir with
        | None -> acc
        | Some relative -> Dir_set.union acc (Dir_set.subtree relative))
    in
    match t.aliases.recursive with
    | Empty -> direct
    | One (root, names) -> add_recursive root names direct
    | Many { by_dir; _ } -> Path.Build.Map.foldi by_dir ~init:direct ~f:add_recursive)
;;
