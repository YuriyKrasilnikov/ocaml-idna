(** 64-bit packed ranges: each int = start lsl 32 lor end_exclusive. *)

type t = int array

let contains cp ranges =
  let len = Array.length ranges in
  if len = 0 then false
  else
    let lo = ref 0 in
    let hi = ref (len - 1) in
    while !lo <= !hi do
      let mid = !lo + (!hi - !lo) / 2 in
      let mid_start = ranges.(mid) lsr 32 in
      if mid_start <= cp then lo := mid + 1
      else hi := mid - 1
    done;
    if !hi >= 0 then
      let start = ranges.(!hi) lsr 32 in
      let end_ = ranges.(!hi) land 0xFFFFFFFF in
      start <= cp && cp < end_
    else false
