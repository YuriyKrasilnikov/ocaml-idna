(** IDNA2008 (RFC 5890/5892) hostname validation. *)

(** Decode UTF-8 string to codepoint list. *)
let utf8_to_cps s =
  let len = String.length s in
  let cps = ref [] in
  let i = ref 0 in
  while !i < len do
    let b = Char.code s.[!i] in
    let cp, size =
      if b land 0x80 = 0 then (b, 1)
      else if b land 0xE0 = 0xC0 then
        (((b land 0x1F) lsl 6) lor (Char.code s.[!i+1] land 0x3F), 2)
      else if b land 0xF0 = 0xE0 then
        (((b land 0x0F) lsl 12) lor ((Char.code s.[!i+1] land 0x3F) lsl 6)
         lor (Char.code s.[!i+2] land 0x3F), 3)
      else
        (((b land 0x07) lsl 18) lor ((Char.code s.[!i+1] land 0x3F) lsl 12)
         lor ((Char.code s.[!i+2] land 0x3F) lsl 6)
         lor (Char.code s.[!i+3] land 0x3F), 4)
    in
    cps := cp :: !cps;
    i := !i + size
  done;
  List.rev !cps

(** Check hyphen rules: no -- at positions 3-4, no leading/trailing hyphen. *)
let check_hyphen_ok cps =
  let len = List.length cps in
  if len = 0 then Error "empty label"
  else if List.hd cps = 0x2D then Error "label starts with hyphen"
  else if List.nth cps (len - 1) = 0x2D then Error "label ends with hyphen"
  else if len >= 4 && List.nth cps 2 = 0x2D && List.nth cps 3 = 0x2D then
    Error "label has -- at positions 3-4"
  else Ok ()

(** Check first codepoint is not a combining mark (category M). *)
let check_initial_combiner cps =
  match cps with
  | [] -> Ok ()
  | cp :: _ ->
    if Intranges.contains cp Idna_tables.general_category_m then
      Error "label begins with combining mark"
    else Ok ()

(** Check NFC Quick Check — reject if any codepoint has NFC_QC=No. *)
let check_nfc_qc cps =
  if List.exists (fun cp -> Intranges.contains cp Idna_tables.nfc_qc_no) cps then
    Error "label not in NFC"
  else Ok ()

(** Check CONTEXTO rule for a codepoint at given position. *)
let valid_contexto cps pos cp =
  let len = List.length cps in
  if cp = 0x00B7 then (* MIDDLE DOT: must be between two 'l' *)
    pos > 0 && pos < len - 1
    && List.nth cps (pos - 1) = 0x6C
    && List.nth cps (pos + 1) = 0x6C
  else if cp = 0x0375 then (* GREEK LOWER NUMERAL SIGN: next must be Greek *)
    pos < len - 1
    && Intranges.contains (List.nth cps (pos + 1)) Idna_tables.script_greek
  else if cp = 0x05F3 || cp = 0x05F4 then (* HEBREW GERESH/GERSHAYIM *)
    pos > 0
    && Intranges.contains (List.nth cps (pos - 1)) Idna_tables.script_hebrew
  else if cp = 0x30FB then (* KATAKANA MIDDLE DOT: needs Han/Hiragana/Katakana *)
    List.exists (fun c ->
      c <> 0x30FB
      && (Intranges.contains c Idna_tables.script_hiragana
          || Intranges.contains c Idna_tables.script_katakana
          || Intranges.contains c Idna_tables.script_han)
    ) cps
  else if 0x0660 <= cp && cp <= 0x0669 then (* ARABIC-INDIC DIGITS *)
    not (List.exists (fun c -> 0x06F0 <= c && c <= 0x06F9) cps)
  else if 0x06F0 <= cp && cp <= 0x06F9 then (* EXTENDED ARABIC-INDIC *)
    not (List.exists (fun c -> 0x0660 <= c && c <= 0x0669) cps)
  else false

(** Check each codepoint is PVALID, or valid CONTEXTJ/CONTEXTO. *)
let check_codepoints cps =
  let rec check i = function
    | [] -> Ok ()
    | cp :: rest ->
      if Intranges.contains cp Idna_tables.codepoint_pvalid then check (i+1) rest
      else if Intranges.contains cp Idna_tables.codepoint_contextj then check (i+1) rest
      else if Intranges.contains cp Idna_tables.codepoint_contexto then
        if valid_contexto cps i cp then check (i+1) rest
        else Error (Printf.sprintf "CONTEXTO U+%04X not valid in context" cp)
      else Error (Printf.sprintf "codepoint U+%04X not allowed" cp)
  in
  check 0 cps

(** Find joining type for a codepoint. *)
let joining_type cp =
  (* Binary search in sorted (cp, type) array *)
  let arr = Idna_tables.joining_types in
  let len = Array.length arr in
  let lo = ref 0 in
  let hi = ref (len - 1) in
  while !lo <= !hi do
    let mid = !lo + (!hi - !lo) / 2 in
    let (mcp, _) = arr.(mid) in
    if mcp < cp then lo := mid + 1
    else if mcp > cp then hi := mid - 1
    else (lo := mid; hi := mid - 1)
  done;
  if !lo < len then
    let (mcp, jt) = arr.(!lo) in
    if mcp = cp then Some jt else None
  else None

(** Check CONTEXTJ rules (RFC 5892 Appendix A). *)
let check_contextj cps =
  let arr = Array.of_list cps in
  let len = Array.length arr in
  let rec check i =
    if i >= len then Ok ()
    else
      let cp = arr.(i) in
      if not (Intranges.contains cp Idna_tables.codepoint_contextj) then check (i + 1)
      else if cp = 0x200C || cp = 0x200D then begin
        (* Zero Width (Non-)Joiner: ok if preceded by virama (combining class 9) *)
        if i > 0 && Intranges.contains arr.(i-1) Idna_tables.virama then check (i + 1)
        else if cp = 0x200C then
          (* Or: specific joining type context for ZWJ *)
          let has_left = ref false in
          let j = ref (i - 1) in
          while !j >= 0 && not !has_left do
            match joining_type arr.(!j) with
            | Some 4 (* T *) -> decr j
            | Some (2 | 1) (* L or D *) -> has_left := true
            | _ -> j := -1
          done;
          if !has_left then
            let has_right = ref false in
            let j = ref (i + 1) in
            while !j < len && not !has_right do
              match joining_type arr.(!j) with
              | Some 4 -> incr j
              | Some (3 | 1) (* R or D *) -> has_right := true
              | _ -> j := len
            done;
            if !has_right then check (i + 1)
            else Error "CONTEXTJ: ZWJ without valid context"
          else Error "CONTEXTJ: ZWJ without valid context"
        else Error "CONTEXTJ: ZWNJ without virama"
      end else check (i + 1)
  in
  check 0

(** Check bidi rules (RFC 5893). *)
let check_bidi cps =
  let is_bidi cls cp = Intranges.contains cp cls in
  let has_rtl = List.exists (fun cp ->
    is_bidi Idna_tables.bidi_r cp
    || is_bidi Idna_tables.bidi_al cp
    || is_bidi Idna_tables.bidi_an cp
  ) cps in
  if not has_rtl then Ok ()
  else
    match cps with
    | [] -> Ok ()
    | first :: _ ->
      let last = List.nth cps (List.length cps - 1) in
      (* Rule 1: first must be R, AL, or L *)
      let rtl = is_bidi Idna_tables.bidi_r first || is_bidi Idna_tables.bidi_al first in
      let ltr = is_bidi Idna_tables.bidi_l first in
      if not (rtl || ltr) then Error "bidi: first char must be R, AL, or L"
      else if rtl then begin
        (* RTL label: last must be R, AL, EN, AN (ignoring NSM) *)
        let effective_last =
          let rec find_non_nsm = function
            | [] -> last
            | [x] -> x
            | x :: rest ->
              if is_bidi Idna_tables.bidi_nsm x then find_non_nsm rest
              else x
          in
          find_non_nsm (List.rev cps)
        in
        if not (is_bidi Idna_tables.bidi_r effective_last
                || is_bidi Idna_tables.bidi_al effective_last
                || is_bidi Idna_tables.bidi_en effective_last
                || is_bidi Idna_tables.bidi_an effective_last) then
          Error "bidi: RTL label must end with R, AL, EN, or AN"
        else Ok ()
      end else Ok ()

(** Validate a single Unicode label (list of codepoints). *)
let check_unicode_label cps =
  let ( >>= ) r f = match r with Ok () -> f () | Error _ as e -> e in
  check_nfc_qc cps >>= fun () ->
  check_hyphen_ok cps >>= fun () ->
  check_initial_combiner cps >>= fun () ->
  check_codepoints cps >>= fun () ->
  check_contextj cps >>= fun () ->
  check_bidi cps


let check_label label =
  let len = String.length label in
  if len = 0 then Error "empty label"
  else if len > 63 then Error "label too long"
  else if len >= 4 && String.sub label 0 4 = "xn--" then begin
    (* A-label: decode punycode, validate unicode label *)
    match Punycode.decode (String.sub label 4 (len - 4)) with
    | Error e -> Error (Printf.sprintf "invalid punycode: %s" e)
    | Ok cps -> check_unicode_label cps
  end else if String.to_seq label |> Seq.for_all (fun c -> Char.code c < 0x80) then begin
    (* Pure ASCII label — lowercase and validate *)
    let lower = String.lowercase_ascii label in
    let cps = List.init (String.length lower) (fun i -> Char.code lower.[i]) in
    check_hyphen_ok cps |> function
    | Error _ as e -> e
    | Ok () -> check_codepoints cps
  end else begin
    (* U-label: decode UTF-8, validate *)
    let cps = utf8_to_cps label in
    check_unicode_label cps
  end

let is_valid_hostname s =
  let len = String.length s in
  if len = 0 || len > 253 then false
  else if s.[len - 1] = '.' then false
  else if s.[0] = '.' then false
  else
    let labels = String.split_on_char '.' s in
    labels <> [] && List.for_all (fun label ->
      match check_label label with
      | Ok () -> true
      | Error _ -> false
    ) labels

module Punycode = Punycode
