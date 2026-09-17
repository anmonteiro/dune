open Import

(** A set of possible file targets, directory targets, and aliases. *)
type t

val empty : t
val all : t
val union : t -> t -> t
val inter : t -> t -> t
val is_empty : t -> bool

(** Conservative for intersections between general glob predicates. *)
val intersects : t -> t -> bool

(** Whether a directory may contain targets or aliases, or be a directory
    target itself. A file target or parent alias with the same name does not
    require keeping this directory. *)
val intersects_directory : t -> Path.Build.t -> bool

(** Include all target kinds in this subtree. *)
val subtree : Path.Build.t -> t

val files : Path.Build.t list -> t
val directories : Path.Build.t list -> t
val aliases : Alias.t list -> t
val files_in_directory : Path.Build.t -> t
val directories_in_directory : Path.Build.t -> t
val aliases_in_directory : Path.Build.t -> t
val file_extensions : dir:Path.Build.t -> Filename.Extension.Set.t -> t
val file_extensions_in_subtree : dir:Path.Build.t -> Filename.Extension.Set.t -> t
val files_matching : dir:Path.Build.t -> Predicate_lang.Glob.t -> t

(** A lookup of a path must consider both file and directory producers. *)
val path : Path.Build.t -> t

val paths_matching : dir:Path.Build.t -> Predicate_lang.Glob.t -> t
val mem_file : t -> Path.Build.t -> bool
val mem_directory : t -> Path.Build.t -> bool
val mem_alias : t -> Alias.t -> bool

(** Possible alias-containing directories below [dir], without considering
    file-only or directory-only producers. *)
val alias_directories : t -> dir:Path.Build.t -> Path.Unspecified.w Dir_set.t
