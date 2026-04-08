(** Binary search on sorted, non-overlapping integer ranges.
    32-bit compatible: parallel start/end arrays. *)

type t = { starts : int array; ends : int array }

val contains : int -> t -> bool
