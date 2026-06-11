(** RFC 5893 bidirectional text checks at label and domain level. *)

open Shared

let bidi_class cp =
  match Idna_tables.Props.bidi_class (Idna_tables.Props.get cp) with
  | 1 -> `R
  | 2 -> `L
  | 3 -> `AL
  | 4 -> `AN
  | 5 -> `EN
  | 6 -> `ES
  | 7 -> `CS
  | 8 -> `ET
  | 9 -> `ON
  | 10 -> `BN
  | 11 -> `NSM
  | _ -> `Other

let label_has_rtl cps =
  List.exists
    (fun cp ->
      let c = bidi_class cp in
      c = `R || c = `AL || c = `AN)
    cps

let rtl_allowed = function
  | `R | `AL | `AN | `EN | `ES | `CS | `ET | `ON | `BN | `NSM -> true
  | _ -> false

let ltr_allowed = function
  | `L | `EN | `ES | `CS | `ET | `ON | `BN | `NSM -> true
  | _ -> false

let check_bidi_label cps =
  match cps with
  | [] -> Ok ()
  | first :: _ ->
      let first_class = bidi_class first in
      let rtl = first_class = `R || first_class = `AL in
      let ltr = first_class = `L in
      if not (rtl || ltr) then Error "bidi: first char must be R, AL, or L"
      else
        let allowed = if rtl then rtl_allowed else ltr_allowed in
        let allowed_ok = ref true in
        let has_en = ref false in
        let has_an = ref false in
        let last_non_nsm = ref first_class in
        List.iter
          (fun cp ->
            let c = bidi_class cp in
            if not (allowed c) then allowed_ok := false;
            if c = `EN then has_en := true;
            if c = `AN then has_an := true;
            if c <> `NSM then last_non_nsm := c)
          cps;
        if not !allowed_ok then
          if rtl then Error "bidi: RTL label contains invalid bidi class"
          else Error "bidi: LTR label contains invalid bidi class"
        else if rtl then
          let last = !last_non_nsm in
          if not (last = `R || last = `AL || last = `EN || last = `AN) then
            Error "bidi: RTL label must end with R, AL, EN, or AN"
          else if !has_en && !has_an then
            Error "bidi: RTL label has both EN and AN"
          else Ok ()
        else
          let last = !last_non_nsm in
          if not (last = `L || last = `EN) then
            Error "bidi: LTR label must end with L or EN"
          else Ok ()

let labels_have_rtl labels =
  List.exists
    (function Root -> false | Label label -> label_has_rtl label.cps)
    labels
