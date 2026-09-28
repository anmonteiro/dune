open Import

val possible_basenames
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> source_extension:Filename.Extension.t
  -> modules:Ordered_set_lang.Unexpanded.t
  -> string list

val source_files
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> for_:Parser_generators.for_
  -> Path.Build.t list

val rule_targets
  :  dir:Path.Build.t
  -> source_files:(Path.Build.t * Filename.Array.Set.t) list
  -> for_:Parser_generators.for_
  -> Target_mask.t

val gen_rules
  :  Super_context.t
  -> dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> for_:Parser_generators.for_
  -> unit Memo.t
