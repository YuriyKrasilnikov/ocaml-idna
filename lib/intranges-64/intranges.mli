(** Binary search on sorted, non-overlapping integer ranges.
    64-bit packed representation: one int per range. *)

type t = int array

val contains : int -> t -> bool
