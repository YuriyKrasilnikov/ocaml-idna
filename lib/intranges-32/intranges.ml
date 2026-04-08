(** 32-bit compatible ranges: parallel starts/ends arrays. *)

type t = { starts : int array; ends : int array }

let contains cp { starts; ends } =
  let len = Array.length starts in
  if len = 0 then false
  else
    let lo = ref 0 in
    let hi = ref (len - 1) in
    while !lo <= !hi do
      let mid = !lo + (!hi - !lo) / 2 in
      if starts.(mid) <= cp then lo := mid + 1
      else hi := mid - 1
    done;
    if !hi >= 0 then
      starts.(!hi) <= cp && cp < ends.(!hi)
    else false
