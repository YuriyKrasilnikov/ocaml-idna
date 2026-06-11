(** CONTEXTO rule contextual validation (RFC 5892 Appendix A). *)

let props_of_cp = Idna_tables.Props.get
let has_script_greek cp = Idna_tables.Props.has_script_greek (props_of_cp cp)
let has_script_hebrew cp = Idna_tables.Props.has_script_hebrew (props_of_cp cp)

let has_katakana_middle_dot_script cp =
  let props = props_of_cp cp in
  Idna_tables.Props.has_script_hiragana props
  || Idna_tables.Props.has_script_katakana props
  || Idna_tables.Props.has_script_han props

type summary = {
  cps : int array;
  has_katakana_middle_dot_script : bool;
  has_arabic_indic_digit : bool;
  has_extended_arabic_indic_digit : bool;
}

let make_summary cps =
  let has_required_katakana_dot_script = ref false in
  let has_arabic_indic_digit = ref false in
  let has_extended_arabic_indic_digit = ref false in
  let rec scan = function
    | [] -> ()
    | cp :: rest ->
        if cp <> 0x30FB && has_katakana_middle_dot_script cp then
          has_required_katakana_dot_script := true;
        if 0x0660 <= cp && cp <= 0x0669 then has_arabic_indic_digit := true;
        if 0x06F0 <= cp && cp <= 0x06F9 then
          has_extended_arabic_indic_digit := true;
        scan rest
  in
  scan cps;
  {
    cps = Array.of_list cps;
    has_katakana_middle_dot_script = !has_required_katakana_dot_script;
    has_arabic_indic_digit = !has_arabic_indic_digit;
    has_extended_arabic_indic_digit = !has_extended_arabic_indic_digit;
  }

let previous summary pos =
  if pos <= 0 || pos > Array.length summary.cps then None
  else Some summary.cps.(pos - 1)

let next summary pos =
  let next_pos = pos + 1 in
  if next_pos < 0 || next_pos >= Array.length summary.cps then None
  else Some summary.cps.(next_pos)

let valid_contexto_summary summary pos cp =
  if cp = 0x00B7 then
    match (previous summary pos, next summary pos) with
    | Some 0x6C, Some 0x6C -> true
    | _ -> false
  else if cp = 0x0375 then
    match next summary pos with
    | Some next_cp -> has_script_greek next_cp
    | None -> false
  else if cp = 0x05F3 || cp = 0x05F4 then
    match previous summary pos with
    | Some previous_cp -> has_script_hebrew previous_cp
    | None -> false
  else if cp = 0x30FB then summary.has_katakana_middle_dot_script
  else if 0x0660 <= cp && cp <= 0x0669 then
    not summary.has_extended_arabic_indic_digit
  else if 0x06F0 <= cp && cp <= 0x06F9 then not summary.has_arabic_indic_digit
  else false
