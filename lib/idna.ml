(** IDNA2008 (RFC 5890/5892) hostname validation. *)

(* ── NFC normalization (UAX #15) ── *)

(** Hangul constants. *)
let s_base = 0xAC00
let l_base = 0x1100
let v_base = 0x1161
let t_base = 0x11A7
let l_count = 19
let v_count = 21
let t_count = 28
let n_count = v_count * t_count  (* 588 *)
let s_count = l_count * n_count  (* 11172 *)

(** Lookup canonical combining class. *)
let ccc cp =
  let arr = Idna_tables.canon_ccc in
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
    let (mcp, c) = arr.(!lo) in
    if mcp = cp then c else 0
  else 0

(** Lookup canonical decomposition. Returns None if no decomposition. *)
let canon_decomp_lookup cp =
  let arr = Idna_tables.canon_decomp in
  let len = Array.length arr in
  let lo = ref 0 in
  let hi = ref (len - 1) in
  while !lo <= !hi do
    let mid = !lo + (!hi - !lo) / 2 in
    let (mcp, _, _) = arr.(mid) in
    if mcp < cp then lo := mid + 1
    else if mcp > cp then hi := mid - 1
    else (lo := mid; hi := mid - 1)
  done;
  if !lo < len then
    let (mcp, d1, d2) = arr.(!lo) in
    if mcp = cp then
      if d2 = 0 then Some [d1] else Some [d1; d2]
    else None
  else None

(** Hangul syllable decomposition. *)
let hangul_decomp cp =
  if cp >= s_base && cp < s_base + s_count then
    let s_index = cp - s_base in
    let l = l_base + s_index / n_count in
    let v = v_base + (s_index mod n_count) / t_count in
    let t = t_base + s_index mod t_count in
    if t = t_base then Some [l; v] else Some [l; v; t]
  else None

(** Full canonical decomposition (recursive). *)
let decompose cps =
  let result = ref [] in
  let rec decomp cp =
    match hangul_decomp cp with
    | Some parts -> List.iter decomp parts
    | None ->
      match canon_decomp_lookup cp with
      | Some parts -> List.iter decomp parts
      | None -> result := cp :: !result
  in
  List.iter decomp cps;
  List.rev !result

(** Canonical ordering: sort consecutive combining marks by CCC. *)
let canonical_order cps =
  let arr = Array.of_list cps in
  let len = Array.length arr in
  (* Bubble sort on consecutive combining marks *)
  let changed = ref true in
  while !changed do
    changed := false;
    for i = 0 to len - 2 do
      let cc_a = ccc arr.(i) in
      let cc_b = ccc arr.(i + 1) in
      if cc_a > 0 && cc_b > 0 && cc_a > cc_b then begin
        let tmp = arr.(i) in
        arr.(i) <- arr.(i + 1);
        arr.(i + 1) <- tmp;
        changed := true
      end
    done
  done;
  Array.to_list arr

(** Lookup composition: (starter, combining) → composite or None. *)
let compose_lookup starter combining =
  (* Hangul L + V → LV *)
  if starter >= l_base && starter < l_base + l_count
     && combining >= v_base && combining < v_base + v_count then
    let l_index = starter - l_base in
    let v_index = combining - v_base in
    Some (s_base + (l_index * v_count + v_index) * t_count)
  (* Hangul LV + T → LVT *)
  else if starter >= s_base && starter < s_base + s_count
          && (starter - s_base) mod t_count = 0
          && combining > t_base && combining < t_base + t_count then
    Some (starter + combining - t_base)
  else
    Idna_tables.nfc_compose starter combining

(** Canonical composition: scan left-to-right, compose where possible. *)
let compose cps =
  let len = List.length cps in
  if len = 0 then []
  else
    let arr = Array.of_list cps in
    (* -1 marks composed-away slots *)
    let starter_pos = ref 0 in
    let last_cc = ref 0 in
    for i = 1 to len - 1 do
      let cp = arr.(i) in
      let cp_cc = ccc cp in
      let blocked = !last_cc <> 0 && !last_cc >= cp_cc in
      if not blocked then begin
        match compose_lookup arr.(!starter_pos) cp with
        | Some composite ->
          arr.(!starter_pos) <- composite;
          arr.(i) <- -1;
          (* Don't update last_cc — composed char is gone *)
        | None ->
          if cp_cc = 0 then begin
            starter_pos := i;
            last_cc := 0
          end else
            last_cc := cp_cc
      end else begin
        if cp_cc = 0 then begin
          starter_pos := i;
          last_cc := 0
        end else
          last_cc := cp_cc
      end
    done;
    Array.to_list arr |> List.filter (fun cp -> cp >= 0)

(** NFC normalization: decompose → canonical order → compose. *)
let nfc cps =
  cps |> decompose |> canonical_order |> compose

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
  compose_lookup starter combining <> None

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

(* ── UTS #46 Processing ── *)

(** Lookup UTS #46 mapping for a codepoint.
    Returns: `Map cps | `Ignored | `Valid | `Deviation | `Disallowed *)
let uts46_status cp =
  (* Deviation characters: ß, ς, ZWJ, ZWNJ *)
  if cp = 0x00DF || cp = 0x03C2 || cp = 0x200C || cp = 0x200D then `Deviation
  else if Intranges.contains cp Idna_tables.uts46_ignored then `Ignored
  else
    (* Binary search in mapping index *)
    let arr = Idna_tables.uts46_map_index in
    let len = Array.length arr in
    let lo = ref 0 in
    let hi = ref (len - 1) in
    let found = ref (-1) in
    while !lo <= !hi do
      let mid = !lo + (!hi - !lo) / 2 in
      let (mcp, _, _) = arr.(mid) in
      if mcp < cp then lo := mid + 1
      else if mcp > cp then hi := mid - 1
      else (found := mid; lo := !hi + 1)
    done;
    if !found >= 0 then
      let (_, offset, length) = arr.(!found) in
      let cps = List.init length (fun i -> Idna_tables.uts46_map_data.(offset + i)) in
      `Map cps
    else if Intranges.contains cp Idna_tables.uts46_valid then `Valid
    else `Disallowed

(** UTS #46 Step 1: Map codepoints (Nontransitional). *)
let uts46_map cps =
  List.concat_map (fun cp ->
    match uts46_status cp with
    | `Map mapped -> mapped
    | `Ignored -> []
    | `Valid | `Deviation -> [cp]
    | `Disallowed -> [cp]  (* kept, will be caught in validation *)
  ) cps

(** UTS #46 Step 3: Break on dots (U+002E, U+3002, U+FF0E, U+FF61). *)
let split_on_dots cps =
  let rec split acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | cp :: rest ->
      if cp = 0x002E || cp = 0x3002 || cp = 0xFF0E || cp = 0xFF61 then
        split (List.rev current :: acc) [] rest
      else
        split acc (cp :: current) rest
  in
  split [] [] cps

(** Encode codepoints to UTF-8 string. *)
let cps_to_utf8 cps =
  let buf = Buffer.create 64 in
  List.iter (fun cp ->
    if cp < 0x80 then Buffer.add_char buf (Char.chr cp)
    else if cp < 0x800 then begin
      Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
    end else if cp < 0x10000 then begin
      Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
    end else begin
      Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
    end
  ) cps;
  Buffer.contents buf

(** UTS #46 Section 4.1 Validity Criteria (Nontransitional).
    Returns Ok () or Error with description. *)
let check_label_uts46 cps =
  let ( >>= ) r f = match r with Ok () -> f () | Error _ as e -> e in
  (* 1. NFC *)
  let nfc_cps = nfc cps in
  (if nfc_cps <> cps then Error "not NFC" else Ok ()) >>= fun () ->
  (* 2+3. Hyphens *)
  check_hyphen_ok cps >>= fun () ->
  (* 6. Initial combining mark *)
  check_initial_combiner cps >>= fun () ->
  (* 7. Each code point valid or deviation (Nontransitional) + STD3 *)
  (let rec check = function
    | [] -> Ok ()
    | cp :: rest ->
      (* UseSTD3ASCIIRules: ASCII must be letter, digit, or hyphen *)
      if cp < 0x80 && not ((cp >= 0x61 && cp <= 0x7A) || (cp >= 0x30 && cp <= 0x39)
                            || cp = 0x2D) then
        Error (Printf.sprintf "disallowed ASCII U+%04X (STD3)" cp)
      else
        match uts46_status cp with
        | `Valid | `Deviation -> check rest
        | `Map _ -> Error (Printf.sprintf "mapped U+%04X in label" cp)
        | `Ignored -> Error (Printf.sprintf "ignored U+%04X in label" cp)
        | `Disallowed -> Error (Printf.sprintf "disallowed U+%04X in label" cp)
  in check cps) >>= fun () ->
  (* 8. CONTEXTJ *)
  check_contextj cps >>= fun () ->
  (* 9. Bidi *)
  check_bidi cps

(** UTS #46 Processing: map → NFC → split → validate.
    Returns list of (label_cps, label_utf8) or Error. *)
let uts46_process domain =
  let input_cps = utf8_to_cps domain in
  (* Step 1: Map *)
  let mapped = uts46_map input_cps in
  (* Step 2: NFC *)
  let normalized = nfc mapped in
  (* Step 3: Break on dots *)
  let label_cps_list = split_on_dots normalized in
  (* Step 4: Convert/Validate each label *)
  let rec process_labels acc = function
    | [] -> Ok (List.rev acc)
    | label_cps :: rest ->
      if label_cps = [] then
        (* Empty label — allowed only as trailing (root dot), error otherwise *)
        if rest = [] then Ok (List.rev (("", []) :: acc))
        else Error "empty label"
      else
        let label_utf8 = cps_to_utf8 label_cps in
        let is_xn =
          List.length label_cps >= 4
          && List.nth label_cps 0 = 0x78 (* x *)
          && List.nth label_cps 1 = 0x6E (* n *)
          && List.nth label_cps 2 = 0x2D (* - *)
          && List.nth label_cps 3 = 0x2D (* - *)
        in
        if is_xn then begin
          (* A-label: check trailing hyphen, decode punycode, validate decoded *)
          let llen = String.length label_utf8 in
          if llen > 0 && label_utf8.[llen - 1] = '-' then
            Error "A-label ends with hyphen"
          else
          let encoded = String.sub label_utf8 4 (String.length label_utf8 - 4) in
          match Punycode.decode encoded with
          | Error e -> Error (Printf.sprintf "punycode: %s" e)
          | Ok decoded_cps ->
            match check_label_uts46 decoded_cps with
            | Error e -> Error e
            | Ok () ->
              let u_label = cps_to_utf8 decoded_cps in
              process_labels ((u_label, decoded_cps) :: acc) rest
        end else
          match check_label_uts46 label_cps with
          | Error e -> Error e
          | Ok () ->
            process_labels ((label_utf8, label_cps) :: acc) rest
  in
  process_labels [] label_cps_list

(** Domain-level bidi check for UTS #46 processed labels. *)
let check_domain_bidi labels_cps =
  let all_cps = List.filter (fun cps -> cps <> []) labels_cps in
  let domain_has_rtl = List.exists label_has_rtl all_cps in
  if not domain_has_rtl then Ok ()
  else
    let rec check = function
      | [] -> Ok ()
      | cps :: rest ->
        match check_bidi_label cps with
        | Error e -> Error e
        | Ok () -> check rest
    in
    check all_cps

let to_unicode domain =
  if String.length domain = 0 then Error "empty input"
  else
  match uts46_process domain with
  | Error e -> Error e
  | Ok labels ->
    (* Domain-level bidi *)
    let all_cps = List.map snd labels in
    match check_domain_bidi all_cps with
    | Error e -> Error e
    | Ok () ->
      let parts = List.map fst labels in
      Ok (String.concat "." parts)

let to_ascii domain =
  if String.length domain = 0 then Error "empty input"
  else
  match uts46_process domain with
  | Error e -> Error e
  | Ok labels ->
    let all_cps = List.map snd labels in
    match check_domain_bidi all_cps with
    | Error e -> Error e
    | Ok () ->
      let encode_label (utf8, cps) =
        let is_ascii = List.for_all (fun cp -> cp < 0x80) cps in
        if is_ascii || cps = [] then Ok utf8
        else
          match Punycode.encode cps with
          | Error e -> Error e
          | Ok encoded -> Ok ("xn--" ^ encoded)
      in
      let rec encode_all acc = function
        | [] -> Ok (List.rev acc)
        | label :: rest ->
          match encode_label label with
          | Error e -> Error e
          | Ok a -> encode_all (a :: acc) rest
      in
      match encode_all [] labels with
      | Error e -> Error e
      | Ok parts ->
        (* A4_2: reject trailing empty label *)
        if parts <> [] && List.nth parts (List.length parts - 1) = "" then
          Error "trailing dot (empty label)"
        else
          let result = String.concat "." parts in
          let rlen = String.length result in
          if rlen > 253 then Error "domain too long"
          else if List.exists (fun p -> String.length p > 63) parts then
            Error "label too long"
          else Ok result

module Punycode = Punycode
