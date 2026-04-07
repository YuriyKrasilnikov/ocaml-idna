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

(** Check if (starter, combining) is a canonical composition pair. *)
let nfc_composes starter combining =
  let key = (starter lsl 21) lor combining in
  let arr = Idna_tables.nfc_compositions in
  let len = Array.length arr in
  let lo = ref 0 in
  let hi = ref (len - 1) in
  while !lo <= !hi do
    let mid = !lo + (!hi - !lo) / 2 in
    if arr.(mid) < key then lo := mid + 1
    else if arr.(mid) > key then hi := mid - 1
    else (lo := mid; hi := mid - 1)
  done;
  !lo < len && arr.(!lo) = key

(** Check NFC Quick Check — reject if any codepoint has NFC_QC=No,
    or if NFC_QC=Maybe and preceding starter composes with it. *)
let check_nfc_qc cps =
  if List.exists (fun cp -> Intranges.contains cp Idna_tables.nfc_qc_no) cps then
    Error "label not in NFC"
  else
    let rec check_maybe prev = function
      | [] -> Ok ()
      | cp :: rest ->
        if Intranges.contains cp Idna_tables.nfc_qc_maybe then
          match prev with
          | Some p when nfc_composes p cp -> Error "label not in NFC"
          | _ -> check_maybe (Some cp) rest
        else
          check_maybe (Some cp) rest
    in
    check_maybe None cps

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

(** Bidi character class detection. *)
let bidi_class cp =
  let is cls = Intranges.contains cp cls in
  if is Idna_tables.bidi_r then `R
  else if is Idna_tables.bidi_l then `L
  else if is Idna_tables.bidi_al then `AL
  else if is Idna_tables.bidi_an then `AN
  else if is Idna_tables.bidi_en then `EN
  else if is Idna_tables.bidi_es then `ES
  else if is Idna_tables.bidi_cs then `CS
  else if is Idna_tables.bidi_et then `ET
  else if is Idna_tables.bidi_on then `ON
  else if is Idna_tables.bidi_bn then `BN
  else if is Idna_tables.bidi_nsm then `NSM
  else `Other

(** Find effective last character class (skip trailing NSM). *)
let effective_last_class cps =
  let rec find = function
    | [] -> `Other
    | [x] -> bidi_class x
    | x :: rest ->
      if bidi_class x = `NSM then find rest
      else bidi_class x
  in
  find (List.rev cps)

(** Does a label contain RTL content? *)
let label_has_rtl cps =
  List.exists (fun cp ->
    let c = bidi_class cp in c = `R || c = `AL || c = `AN
  ) cps

(** Check bidi rules 1-6 on a label (always enforced, for bidi domains). *)
let check_bidi_label cps =
  match cps with
  | [] -> Ok ()
  | first :: _ ->
    let first_class = bidi_class first in
    let rtl = first_class = `R || first_class = `AL in
    let ltr = first_class = `L in
    (* Rule 1: first must be R, AL, or L *)
    if not (rtl || ltr) then Error "bidi: first char must be R, AL, or L"
    else if rtl then begin
      (* RTL label (rules 2, 3, 4) *)
      let rule2_ok = List.for_all (fun cp ->
        match bidi_class cp with
        | `R | `AL | `AN | `EN | `ES | `CS | `ET | `ON | `BN | `NSM -> true
        | _ -> false
      ) cps in
      if not rule2_ok then Error "bidi: RTL label contains invalid bidi class"
      else
        let last = effective_last_class cps in
        if not (last = `R || last = `AL || last = `EN || last = `AN) then
          Error "bidi: RTL label must end with R, AL, EN, or AN"
        else
          let has_en = List.exists (fun cp -> bidi_class cp = `EN) cps in
          let has_an = List.exists (fun cp -> bidi_class cp = `AN) cps in
          if has_en && has_an then
            Error "bidi: RTL label has both EN and AN"
          else Ok ()
    end else begin
      (* LTR label (rules 5, 6) *)
      let rule5_ok = List.for_all (fun cp ->
        match bidi_class cp with
        | `L | `EN | `ES | `CS | `ET | `ON | `BN | `NSM -> true
        | _ -> false
      ) cps in
      if not rule5_ok then Error "bidi: LTR label contains invalid bidi class"
      else
        let last = effective_last_class cps in
        if not (last = `L || last = `EN) then
          Error "bidi: LTR label must end with L or EN"
        else Ok ()
    end

(** Check bidi for a single label (only if label itself has RTL). *)
let check_bidi cps =
  if not (label_has_rtl cps) then Ok ()
  else check_bidi_label cps

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
  else
    let is_xn = len >= 4
      && String.sub (String.lowercase_ascii (String.sub label 0 4)) 0 4 = "xn--" in
    if is_xn then begin
      (* A-label: decode punycode, validate decoded codepoints *)
      if label.[len - 1] = '-' then Error "A-label ends with hyphen"
      else
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

(** Extract codepoints from a label (for domain-level bidi check). *)
let label_to_cps label =
  let len = String.length label in
  if len >= 4 && String.sub (String.lowercase_ascii (String.sub label 0 4)) 0 4 = "xn--" then
    match Punycode.decode (String.sub label 4 (len - 4)) with
    | Ok cps -> Some cps
    | Error _ -> None
  else if String.to_seq label |> Seq.for_all (fun c -> Char.code c < 0x80) then
    let lower = String.lowercase_ascii label in
    Some (List.init (String.length lower) (fun i -> Char.code lower.[i]))
  else
    Some (utf8_to_cps label)

let is_valid_hostname s =
  let len = String.length s in
  if len = 0 || len > 253 then false
  else if s.[len - 1] = '.' then false
  else if s.[0] = '.' then false
  else
    let labels = String.split_on_char '.' s in
    if labels = [] then false
    else
      (* First pass: validate each label + DNS length constraint *)
      let all_ok = List.for_all (fun label ->
        let llen = String.length label in
        if llen = 0 then false
        else if String.to_seq label |> Seq.for_all (fun c -> Char.code c < 0x80) then
          (* ASCII/A-label: 63-byte limit directly *)
          llen <= 63 &&
          (match check_label label with Ok () -> true | Error _ -> false)
        else
          (* U-label: validate, then check A-label encoding length *)
          match check_label label with
          | Error _ -> false
          | Ok () ->
            let cps = utf8_to_cps label in
            match Punycode.encode cps with
            | Error _ -> false
            | Ok encoded -> String.length encoded + 4 <= 63  (* +4 for "xn--" *)
      ) labels in
      if not all_ok then false
      else
        (* Second pass: domain-level bidi (RFC 5893) *)
        (* If any label has RTL content, ALL labels must satisfy bidi rules *)
        let all_cps = List.filter_map label_to_cps labels in
        let domain_has_rtl = List.exists label_has_rtl all_cps in
        if not domain_has_rtl then true
        else
          List.for_all (fun cps ->
            match check_bidi_label cps with
            | Ok () -> true
            | Error _ -> false
          ) all_cps

let is_valid_hostname_bidi labels =
  let all_cps = List.filter_map label_to_cps labels in
  let domain_has_rtl = List.exists label_has_rtl all_cps in
  if not domain_has_rtl then true
  else
    List.for_all (fun cps ->
      match check_bidi_label cps with
      | Ok () -> true
      | Error _ -> false
    ) all_cps

module Punycode = Punycode
