(** A recursively staged collection of rules. *)

open Import

(** Represent a set of rules producing files in a given directory *)
module Dir_rules : sig
  type t

  val empty : t
  val union : t -> t -> t

  module Alias_spec : sig
    type item =
      | Deps of unit Action_builder.t
      | (* Execute an action. You can think of [action t] as a convenient way of
           declaring an anonymous build rule and depending on its outcome. While
           this action does not produce any value observable by the rest of the
           build rules, the action can fail. So its outcome is success or
           failure. This mechanism is commonly used for attaching tests to an
           alias.

           Note that any dependency declared in [t] is treated as a dependency
           of the action returned by [t], rather than anything that depends on
           the alias containing the action.

           When passing [--force] to Dune, these are exactly the actions that
           will be re-executed. *)
        Action of Rule.Anonymous_action.t

    type t = { expansions : (Loc.t * item) Appendable_list.t } [@@unboxed]
  end

  (** A ready to process view of the rules of a directory *)
  type ready =
    { rules : Rule.t list
    ; aliases : Alias_spec.t Alias.Name.Map.t
    }

  val consume : t -> ready
  val is_empty : t -> bool
  val to_dyn : t -> Dyn.t
end

(** Direct rules and suspended producers, potentially spanning many directories. *)
type t

(** Directly available rules; this does not force any suspensions. *)
val to_map : t -> Dir_rules.t Path.Build.Map.t

module Produce : sig
  (** Add a rule to the system. This function must be called from the
      [gen_rules] callback or one of its suspensions. A rule's targets must
      share a directory and lie within every enclosing ownership mask.
      Rules emitted for descendants are inherited automatically. *)
  val rule : Rule.t -> unit Memo.t

  module Alias : sig
    type t = Alias.t

    (** [add_deps alias ?loc deps] arrange things so that all the dependencies
        registered by [deps] are considered as a part of alias expansion of
        [alias]. *)
    val add_deps : t -> ?loc:Stdune.Loc.t -> unit Action_builder.t -> unit Memo.t

    (** [add_action aliases ~loc action] arrange things so that [action]
        is executed as part of the build of aliases [aliases]. *)
    val add_action : t list -> loc:Loc.t -> Action.Full.t Action_builder.t -> unit Memo.t
  end
end

val implicit_output : t Memo.Implicit_output.t
val empty : t
val to_dyn : t -> Dyn.t
val union : t -> t -> t
val of_dir_rules : dir:Path.Build.t -> Dir_rules.t -> t
val of_rules : Rule.t list -> t
val produce : t -> unit Memo.t
val collect : (unit -> 'a Memo.t) -> ('a * t) Memo.t
val collect_unit : (unit -> unit Memo.t) -> t Memo.t

(** Suspend rule production under a narrower target mask. Suspensions may
    produce more suspensions, and are shared by all target lookups. *)
val narrow : Target_mask.t -> (unit -> unit Memo.t) -> unit Memo.t

(** Like [narrow], while separately exposing the memoized producer's result.
    Forcing the result does not emit its rules a second time. *)
val defer : Target_mask.t -> (unit -> 'a Memo.t) -> 'a Memo.Lazy.t Memo.t

module Deferred : sig
  type 'a t

  (** The result, with its prerequisite enforced before every evaluation. *)
  val result : 'a t -> 'a Memo.Lazy.t
end

(** Like [defer], with an ordered prerequisite that does not determine ownership.
    The producer must track all semantic inputs independently of [prepare],
    which may only materialize those inputs and must not emit rules. Cleanup
    can then prove unchanged ownership without validating that materialization.
    Failed or in-flight preparation still prevents reuse. *)
val defer_after
  :  Target_mask.t
  -> prepare:unit Memo.t
  -> (unit -> 'a Memo.t)
  -> 'a Deferred.t Memo.t

(** Produce further rules from a deferred result. Its original preparation is
    forced first, without making materialization a semantic input of the new
    producer. Failure checks retain the original producer as well. *)
val narrow_after : Target_mask.t -> 'a Deferred.t -> ('a -> unit Memo.t) -> unit Memo.t

(** Validate and restrict a rule tree to its producer's ownership mask. *)
val restrict : t -> Target_mask.t -> t

(** Restrict direct and suspended rule production to [dir] and its descendants.
    A target equal to [dir] belongs to its parent and is not allowed. *)
val restrict_to_directory : t -> dir:Path.Build.t -> t

(** Prove that previously evaluated producers still have their cached outputs.
    This does not evaluate producers, including those never requested. A failed
    proof only means that cleanup must take a fresh inventory. *)
val unchanged_since : t -> since:Memo.Run.t -> bool

(** Recursively pull matching suspensions. Multi-target rules expand the
    request to all their outputs before returning a flat collection. *)
val load : t -> Target_mask.t -> t Memo.t

module Producer_id : Id.S

type refinement =
  { id : Producer_id.t
  ; mask : Target_mask.t
  }

module Pending : sig
  (** Ownership summaries of unforced producers, shared with the rule index. *)
  type t

  (** Conservatively compare shared ownership summaries and exclusions. *)
  val same_components : t -> t -> bool

  val of_mask : Target_mask.t -> t
  val mem_file : t -> Path.Build.t -> bool
  val mem_directory : t -> Path.Build.t -> bool
  val mem_path : t -> dir:Path.Build.t -> Filename.t -> bool
  val intersects_directory : t -> Path.Build.t -> bool
  val alias_directories : t -> dir:Path.Build.t -> Path.Unspecified.w Dir_set.t
end

module Revealed : sig
  type rules := t

  (** Direct rules exposed by followed producers, without merging their trees. *)
  type t

  (** Conservatively compare the ordered physical direct chunks. *)
  val same_components : t -> t -> bool

  val of_rules : rules -> t
  val directories : t -> Path.Build.Set.t
  val target_names : t -> dir:Path.Build.t -> Filename.Set.t * Filename.Set.t
  val directory_targets : t -> Loc.t Path.Build.Map.t
  val find : t -> dir:Path.Build.t -> Dir_rules.t
end

type loaded =
  { selected : t
  ; revealed : Revealed.t
  ; pending : Pending.t
  ; refinements : refinement list
    (** Followed suspensions, including their enclosing producers, without
          duplicate IDs. IDs are shared by repeated requests of the same tree. *)
  }

(** Also expose directly revealed rules and the masks of unforced suspensions,
    for conservative artifact cleanup. *)
val load_with_pending : t -> Target_mask.t -> loaded Memo.t

(** Ordinary path lookup, including both file and directory ownership and the
    complete multi-target closure. *)
val load_path_with_pending : t -> Path.Build.t -> loaded Memo.t

(** Materialize a declared directory while discovering file rules. Directory
    outputs do not pull file producers or descendants; file outputs of a mixed
    rule still close over both kinds. Ordinary requests validate all conflicts. *)
val load_directory_with_pending : t -> Path.Build.t -> loaded Memo.t

(** Direct and suspended outputs, without evaluating suspensions. *)
val targets : t -> Target_mask.t

(** returns [Dir_rules.empty] for non-build paths *)
val find : t -> Path.t -> Dir_rules.t

(** [prefix_rules prefix ~f] adds [prefix] to all the rules generated by [f] *)
val prefix_rules : unit Action_builder.t -> f:(unit -> 'a Memo.t) -> 'a Memo.t

(** [directory_targets t] returns all the directory targets generated by [t].
    The locations are of the rules that introduce these targets *)
val directory_targets : t -> Loc.t Path.Build.Map.t

(** Names of directly available file and directory targets in [dir]. *)
val target_names : t -> dir:Path.Build.t -> Filename.Set.t * Filename.Set.t
