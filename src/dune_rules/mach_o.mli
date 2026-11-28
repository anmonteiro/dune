(** Classify linked Mach-O images for signing after artifact substitution.

    Reads thin and universal headers, including byte-swapped and 64-bit fat
    headers. The file-type predicate follows sigtool's [requiresSignature].
    This does not inspect or verify an existing code signature. *)

open Stdune

(** Non-Mach-O files and relocatable objects return [false]. Invalid recognized
    headers raise a user error; I/O errors are propagated. *)
val requires_signature : Path.t -> bool
