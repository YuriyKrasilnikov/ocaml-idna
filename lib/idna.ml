(** Internationalized domain name processing. *)

type registration_hostname_flags = {
  verify_dns_length : bool;
}

let default_registration_hostname_flags = {
  verify_dns_length = true;
}

type lookup_flags = {
  check_bidi : bool;
}

let default_lookup_flags = {
  check_bidi = true;
}

type uts46_flags = {
  check_hyphens : bool;
  check_bidi : bool;
  check_joiners : bool;
  use_std3_ascii_rules : bool;
  verify_dns_length : bool;
  ignore_invalid_punycode : bool;
}

let default_uts46_flags = {
  check_hyphens = true;
  check_bidi = true;
  check_joiners = true;
  use_std3_ascii_rules = true;
  verify_dns_length = true;
  ignore_invalid_punycode = false;
}

type uts46_result = {
  value : string;
  errored : bool;
}

type validated_label = {
  unicode : string;
  cps : int list;
}

type domain_label =
  | Root
  | Label of validated_label

(* ── NFC normalization (UAX #15) ── *)

let s_base = 0xAC00
let l_base = 0x1100
let v_base = 0x1161
let t_base = 0x11A7
let l_count = 19
let v_count = 21
let t_count = 28
let n_count = v_count * t_count
let s_count = l_count * n_count

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

let hangul_decomp cp =
  if cp >= s_base && cp < s_base + s_count then
    let s_index = cp - s_base in
    let l = l_base + s_index / n_count in
    let v = v_base + (s_index mod n_count) / t_count in
    let t = t_base + s_index mod t_count in
    if t = t_base then Some [l; v] else Some [l; v; t]
  else None

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

let canonical_order cps =
  let arr = Array.of_list cps in
  let len = Array.length arr in
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

let compose_lookup starter combining =
  if starter >= l_base && starter < l_base + l_count
     && combining >= v_base && combining < v_base + v_count then
    let l_index = starter - l_base in
    let v_index = combining - v_base in
    Some (s_base + (l_index * v_count + v_index) * t_count)
  else if starter >= s_base && starter < s_base + s_count
          && (starter - s_base) mod t_count = 0
          && combining > t_base && combining < t_base + t_count then
    Some (starter + combining - t_base)
  else
    Idna_tables.nfc_compose starter combining

let compose cps =
  let len = List.length cps in
  if len = 0 then []
  else
    let arr = Array.of_list cps in
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
          arr.(i) <- -1
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

let nfc cps =
  cps |> decompose |> canonical_order |> compose

(* ── Shared helpers ── *)

let string_is_ascii s =
  String.to_seq s |> Seq.for_all (fun c -> Char.code c < 0x80)

let lowercase_ascii_bytes s =
  String.init (String.length s) (fun i -> Char.lowercase_ascii s.[i])

let has_xn_prefix s =
  let len = String.length s in
  len >= 4 && String.lowercase_ascii (String.sub s 0 4) = "xn--"

let cps_to_utf8 = Utf8.of_cps

let cps_are_ascii cps =
  List.for_all (fun cp -> cp < 0x80) cps

let ace_of_cps cps =
  if cps_are_ascii cps then Ok (cps_to_utf8 cps)
  else
    match Punycode.encode cps with
    | Error e -> Error e
    | Ok encoded -> Ok ("xn--" ^ encoded)

let check_hyphen_ok cps =
  let len = List.length cps in
  if len = 0 then Error "empty label"
  else if List.hd cps = 0x2D then Error "label starts with hyphen"
  else if List.nth cps (len - 1) = 0x2D then Error "label ends with hyphen"
  else if len >= 4 && List.nth cps 2 = 0x2D && List.nth cps 3 = 0x2D then
    Error "label has -- at positions 3-4"
  else Ok ()

let check_initial_combiner cps =
  match cps with
  | [] -> Ok ()
  | cp :: _ ->
    if Intranges.contains cp Idna_tables.general_category_m then
      Error "label begins with combining mark"
    else Ok ()

let check_nfc_qc cps =
  if nfc cps = cps then Ok ()
  else Error "label not in NFC"

let valid_contexto cps pos cp =
  let len = List.length cps in
  if cp = 0x00B7 then
    pos > 0 && pos < len - 1
    && List.nth cps (pos - 1) = 0x6C
    && List.nth cps (pos + 1) = 0x6C
  else if cp = 0x0375 then
    pos < len - 1
    && Intranges.contains (List.nth cps (pos + 1)) Idna_tables.script_greek
  else if cp = 0x05F3 || cp = 0x05F4 then
    pos > 0
    && Intranges.contains (List.nth cps (pos - 1)) Idna_tables.script_hebrew
  else if cp = 0x30FB then
    List.exists (fun c ->
      c <> 0x30FB
      && (Intranges.contains c Idna_tables.script_hiragana
          || Intranges.contains c Idna_tables.script_katakana
          || Intranges.contains c Idna_tables.script_han)
    ) cps
  else if 0x0660 <= cp && cp <= 0x0669 then
    not (List.exists (fun c -> 0x06F0 <= c && c <= 0x06F9) cps)
  else if 0x06F0 <= cp && cp <= 0x06F9 then
    not (List.exists (fun c -> 0x0660 <= c && c <= 0x0669) cps)
  else false

let check_codepoints_registration cps =
  let rec check i = function
    | [] -> Ok ()
    | cp :: rest ->
      if Intranges.contains cp Idna_tables.codepoint_pvalid then check (i + 1) rest
      else if Intranges.contains cp Idna_tables.codepoint_contextj then check (i + 1) rest
      else if Intranges.contains cp Idna_tables.codepoint_contexto then
        if valid_contexto cps i cp then check (i + 1) rest
        else Error (Printf.sprintf "CONTEXTO U+%04X not valid in context" cp)
      else Error (Printf.sprintf "codepoint U+%04X not allowed" cp)
  in
  check 0 cps

let check_codepoints_lookup cps =
  let rec check = function
    | [] -> Ok ()
    | cp :: rest ->
      if Intranges.contains cp Idna_tables.codepoint_pvalid
         || Intranges.contains cp Idna_tables.codepoint_contextj
         || Intranges.contains cp Idna_tables.codepoint_contexto
      then check rest
      else Error (Printf.sprintf "codepoint U+%04X not allowed" cp)
  in
  check cps

let joining_type cp =
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

let check_contextj cps =
  let arr = Array.of_list cps in
  let len = Array.length arr in
  let rec check i =
    if i >= len then Ok ()
    else
      let cp = arr.(i) in
      if not (Intranges.contains cp Idna_tables.codepoint_contextj) then check (i + 1)
      else if cp = 0x200C || cp = 0x200D then begin
        if i > 0 && Intranges.contains arr.(i - 1) Idna_tables.virama then check (i + 1)
        else if cp = 0x200C then
          let has_left = ref false in
          let j = ref (i - 1) in
          while !j >= 0 && not !has_left do
            match joining_type arr.(!j) with
            | Some 4 -> decr j
            | Some (2 | 1) -> has_left := true
            | _ -> j := -1
          done;
          if !has_left then
            let has_right = ref false in
            let j = ref (i + 1) in
            while !j < len && not !has_right do
              match joining_type arr.(!j) with
              | Some 4 -> incr j
              | Some (3 | 1) -> has_right := true
              | _ -> j := len
            done;
            if !has_right then check (i + 1)
            else Error "CONTEXTJ: ZWNJ without valid context"
          else Error "CONTEXTJ: ZWNJ without valid context"
        else Error "CONTEXTJ: ZWJ without virama"
      end else check (i + 1)
  in
  check 0

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

let effective_last_class cps =
  let rec find = function
    | [] -> `Other
    | [x] -> bidi_class x
    | x :: rest ->
      if bidi_class x = `NSM then find rest
      else bidi_class x
  in
  find (List.rev cps)

let label_has_rtl cps =
  List.exists (fun cp ->
    let c = bidi_class cp in
    c = `R || c = `AL || c = `AN
  ) cps

let check_bidi_label cps =
  match cps with
  | [] -> Ok ()
  | first :: _ ->
    let first_class = bidi_class first in
    let rtl = first_class = `R || first_class = `AL in
    let ltr = first_class = `L in
    if not (rtl || ltr) then Error "bidi: first char must be R, AL, or L"
    else if rtl then begin
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

let check_bidi_if_label_has_rtl cps =
  if label_has_rtl cps then check_bidi_label cps else Ok ()

let check_unicode_label_registration cps =
  let ( >>= ) r f = match r with Ok () -> f () | Error _ as e -> e in
  check_nfc_qc cps >>= fun () ->
  check_hyphen_ok cps >>= fun () ->
  check_initial_combiner cps >>= fun () ->
  check_codepoints_registration cps >>= fun () ->
  check_contextj cps >>= fun () ->
  check_bidi_if_label_has_rtl cps

let check_unicode_label_lookup cps =
  let ( >>= ) r f = match r with Ok () -> f () | Error _ as e -> e in
  check_nfc_qc cps >>= fun () ->
  check_hyphen_ok cps >>= fun () ->
  check_initial_combiner cps >>= fun () ->
  check_codepoints_lookup cps >>= fun () ->
  check_contextj cps

let validate_ascii_label label =
  let lower = String.lowercase_ascii label in
  let cps = List.init (String.length lower) (fun i -> Char.code lower.[i]) in
  match check_hyphen_ok cps with
  | Error _ as err -> err
  | Ok () ->
    (match check_codepoints_registration cps with
     | Ok () -> Ok { unicode = lower; cps }
     | Error e -> Error e)

let validate_utf8_label ~validate_unicode label =
  match Utf8.to_cps label with
  | Error msg -> Error msg
  | Ok cps ->
    (match validate_unicode cps with
     | Ok () -> Ok { unicode = cps_to_utf8 cps; cps }
     | Error e -> Error e)

let validate_apparent_alabel ~allow_mixed_case ~validate_unicode label =
  let len = String.length label in
  let lower = String.lowercase_ascii label in
  if not (string_is_ascii label) then Error "A-label must be ASCII"
  else if lower.[len - 1] = '-' then Error "A-label ends with hyphen"
  else
    match Punycode.decode (String.sub lower 4 (len - 4)) with
    | Error e -> Error (Printf.sprintf "invalid punycode: %s" e)
    | Ok cps ->
      if cps_are_ascii cps then
        Error "A-label decodes to ASCII-only label"
      else
        let ( >>= ) r f = match r with Ok () -> f () | Error _ as err -> err in
        validate_unicode cps >>= fun () ->
        match Punycode.encode cps with
        | Error e -> Error (Printf.sprintf "invalid punycode: %s" e)
        | Ok encoded ->
          let canonical = "xn--" ^ encoded in
          if canonical <> lower then
            Error "A-label not in canonical form"
          else if (not allow_mixed_case) && canonical <> label then
            Error "A-label must be lowercase canonical form"
          else
            Ok { unicode = cps_to_utf8 cps; cps }

let validate_registration_label label =
  if String.length label = 0 then Error "empty label"
  else if has_xn_prefix label then
    validate_apparent_alabel
      ~allow_mixed_case:false
      ~validate_unicode:check_unicode_label_registration
      label
  else if string_is_ascii label then
    validate_ascii_label label
  else
    validate_utf8_label ~validate_unicode:check_unicode_label_registration label

let validate_lookup_label label =
  if String.length label = 0 then Error "empty label"
  else if has_xn_prefix label then
    validate_apparent_alabel
      ~allow_mixed_case:true
      ~validate_unicode:check_unicode_label_lookup
      label
  else if string_is_ascii label then
    validate_ascii_label label
  else
    validate_utf8_label ~validate_unicode:check_unicode_label_lookup label

let labels_have_rtl labels =
  List.exists (function
    | Root -> false
    | Label label -> label_has_rtl label.cps
  ) labels

let check_domain_bidi_all labels =
  if not (labels_have_rtl labels) then Ok ()
  else
    let rec check = function
      | [] -> Ok ()
      | Root :: rest -> check rest
      | Label label :: rest ->
        (match check_bidi_label label.cps with
         | Ok () -> check rest
         | Error _ as err -> err)
    in
    check labels

let domain_ascii_labels labels =
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | Root :: rest -> collect ("" :: acc) rest
    | Label label :: rest ->
      match ace_of_cps label.cps with
      | Error e -> Error e
      | Ok ace -> collect (ace :: acc) rest
  in
  collect [] labels

let verify_dns_length labels =
  match domain_ascii_labels labels with
  | Error e -> Error e
  | Ok parts ->
    let has_root =
      match List.rev labels with
      | Root :: _ -> true
      | _ -> false
    in
    let parts_no_root =
      if has_root then List.filter (fun s -> s <> "") parts else parts
    in
    if parts_no_root = [] then Error "empty domain"
    else if List.exists (fun part -> String.length part = 0 || String.length part > 63) parts_no_root then
      Error "label too long"
    else
      let total =
        List.fold_left (fun acc part -> acc + String.length part) 0 parts_no_root
        + max 0 (List.length parts_no_root - 1)
      in
      if total > 253 then Error "domain too long"
      else Ok ()

let serialize_unicode labels =
  String.concat "."
    (List.map (function Root -> "" | Label label -> label.unicode) labels)

let serialize_ascii labels =
  match domain_ascii_labels labels with
  | Error e -> Error e
  | Ok parts -> Ok (String.concat "." parts)

let process_domain
    ~validate_label
    ~allow_trailing_root
    ~check_domain_bidi
    ~verify_dns_length_flag
    domain
  =
  let ( >>= ) r f = match r with Ok x -> f x | Error _ as err -> err in
  if String.length domain = 0 then Error "empty input"
  else
    let raw_labels = String.split_on_char '.' domain in
    let last_index = List.length raw_labels - 1 in
    let rec collect i acc = function
      | [] -> Ok (List.rev acc)
      | label :: rest ->
        if label = "" then
          if allow_trailing_root && i = last_index then
            collect (i + 1) (Root :: acc) rest
          else
            Error "empty label"
        else
          validate_label label >>= fun validated ->
          collect (i + 1) (Label validated :: acc) rest
    in
    collect 0 [] raw_labels >>= fun labels ->
    (match check_domain_bidi labels with
     | Ok () -> Ok labels
     | Error e -> Error e) >>= fun labels ->
    (if verify_dns_length_flag then verify_dns_length labels else Ok ()) >>= fun () ->
    Ok labels

(* ── UTS #46 Processing ── *)

let uts46_status cp =
  if Intranges.contains cp Idna_tables.uts46_deviation then `Deviation
  else if Intranges.contains cp Idna_tables.uts46_ignored then `Ignored
  else
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

let uts46_map cps =
  List.concat_map (fun cp ->
    match uts46_status cp with
    | `Map mapped -> mapped
    | `Ignored -> []
    | `Valid | `Deviation -> [cp]
    | `Disallowed -> [cp]
  ) cps

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

let check_label_uts46 ~flags cps =
  let ( >>= ) r f = match r with Ok () -> f () | Error _ as e -> e in
  let nfc_cps = nfc cps in
  (if nfc_cps <> cps then Error "not NFC" else Ok ()) >>= fun () ->
  (if flags.check_hyphens then check_hyphen_ok cps else Ok ()) >>= fun () ->
  (if not flags.check_hyphens then
     match cps with
     | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ -> Error "label starts with xn--"
     | _ -> Ok ()
   else Ok ()) >>= fun () ->
  check_initial_combiner cps >>= fun () ->
  (let rec check = function
    | [] -> Ok ()
    | cp :: rest ->
      let std3_ok =
        not flags.use_std3_ascii_rules
        || cp >= 0x80
        || (cp >= 0x61 && cp <= 0x7A)
        || (cp >= 0x30 && cp <= 0x39)
        || cp = 0x2D
      in
      if not std3_ok then
        Error (Printf.sprintf "disallowed ASCII U+%04X (STD3)" cp)
      else
        match uts46_status cp with
        | `Valid | `Deviation -> check rest
        | `Map _ -> Error (Printf.sprintf "mapped U+%04X in label" cp)
        | `Ignored -> Error (Printf.sprintf "ignored U+%04X in label" cp)
        | `Disallowed -> Error (Printf.sprintf "disallowed U+%04X in label" cp)
  in
   check cps) >>= fun () ->
  (if flags.check_joiners then check_contextj cps else Ok ()) >>= fun () ->
  (if flags.check_bidi then check_bidi_if_label_has_rtl cps else Ok ())

let uts46_process_acc ~flags domain =
  let errored = ref false in
  let mark () = errored := true in
  let labels =
    match Utf8.to_cps domain with
    | Error _ ->
      mark ();
      String.split_on_char '.' (lowercase_ascii_bytes domain)
      |> List.map (fun label ->
        if string_is_ascii label then
          (label, List.init (String.length label) (fun i -> Char.code label.[i]))
        else
          (label, [])
      )
    | Ok input_cps ->
      let mapped = uts46_map input_cps in
      let normalized = nfc mapped in
      let label_cps_list = split_on_dots normalized in
      List.map (fun label_cps ->
        let label_utf8 = cps_to_utf8 label_cps in
        if label_cps = [] then (label_utf8, label_cps)
        else
          let is_xn =
            List.length label_cps >= 4
            && List.nth label_cps 0 = 0x78
            && List.nth label_cps 1 = 0x6E
            && List.nth label_cps 2 = 0x2D
            && List.nth label_cps 3 = 0x2D
          in
          if is_xn then begin
            if not flags.check_hyphens then mark ();
            if not (cps_are_ascii label_cps) then begin
              mark ();
              (label_utf8, label_cps)
            end else
              let encoded = String.sub label_utf8 4 (String.length label_utf8 - 4) in
              if encoded = "" then begin
                mark ();
                ("", [])
              end else
                match Punycode.decode encoded with
                | Error _ ->
                  if flags.ignore_invalid_punycode then (label_utf8, label_cps)
                  else begin mark (); (label_utf8, label_cps) end
                | Ok decoded_cps ->
                  if decoded_cps = [] then begin
                    mark ();
                    (label_utf8, label_cps)
                  end else
                    let u_label = cps_to_utf8 decoded_cps in
                    if cps_are_ascii decoded_cps then mark ();
                    (match check_label_uts46 ~flags decoded_cps with
                     | Error _ -> mark ()
                     | Ok () -> ());
                    (u_label, decoded_cps)
          end else begin
            (match check_label_uts46 ~flags label_cps with
             | Error _ -> mark ()
             | Ok () -> ());
            (label_utf8, label_cps)
          end
      ) label_cps_list
  in
  let n = List.length labels in
  List.iteri (fun i (_, cps) ->
    if cps = [] && i <> n - 1 then mark ()
  ) labels;
  if flags.check_bidi then begin
    let all_cps = List.filter (fun cps -> cps <> []) (List.map snd labels) in
    if List.exists label_has_rtl all_cps then
      List.iter (fun cps ->
        match check_bidi_label cps with
        | Error _ -> mark ()
        | Ok () -> ()
      ) all_cps
  end;
  (labels, !errored)

let uts46_to_unicode ?(flags = default_uts46_flags) domain =
  if String.length domain = 0 then
    { value = ""; errored = true }
  else
    let labels, errored = uts46_process_acc ~flags domain in
    let parts = List.map fst labels in
    { value = String.concat "." parts; errored }

let uts46_to_ascii ?(flags = default_uts46_flags) domain =
  if String.length domain = 0 then Error "empty input"
  else
    let labels, errored = uts46_process_acc ~flags domain in
    if errored then Error "uts46 processing error"
    else
      let encode_label (utf8, cps) =
        if cps_are_ascii cps || cps = [] then Ok utf8
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
        if flags.verify_dns_length
           && parts <> [] && List.nth parts (List.length parts - 1) = "" then
          Error "trailing dot (empty label)"
        else
          let result = String.concat "." parts in
          let rlen = String.length result in
          if flags.verify_dns_length && rlen > 253 then
            Error "domain too long"
          else if flags.verify_dns_length
               && List.exists (fun p -> String.length p > 63) parts then
            Error "label too long"
          else Ok result

(* ── Public modules ── *)

module Registration = struct
  type hostname_flags = registration_hostname_flags = {
    verify_dns_length : bool;
  }

  let default_hostname_flags = default_registration_hostname_flags

  let check_label label =
    match validate_registration_label label with
    | Ok _ -> Ok ()
    | Error _ as err -> err

  let to_unicode ?(flags = default_hostname_flags) domain =
    match process_domain
            ~validate_label:validate_registration_label
            ~allow_trailing_root:false
            ~check_domain_bidi:(fun _ -> Ok ())
            ~verify_dns_length_flag:flags.verify_dns_length
            domain
    with
    | Ok labels -> Ok (serialize_unicode labels)
    | Error _ as err -> err

  let to_ascii ?(flags = default_hostname_flags) domain =
    match process_domain
            ~validate_label:validate_registration_label
            ~allow_trailing_root:false
            ~check_domain_bidi:(fun _ -> Ok ())
            ~verify_dns_length_flag:flags.verify_dns_length
            domain
    with
    | Ok labels -> serialize_ascii labels
    | Error _ as err -> err

  let is_valid_hostname ?(flags = default_hostname_flags) domain =
    match to_ascii ~flags domain with
    | Ok _ -> true
    | Error _ -> false
end

module Lookup = struct
  type flags = lookup_flags = {
    check_bidi : bool;
  }

  let default_flags = default_lookup_flags

  let to_unicode ?(flags = default_flags) domain =
    let domain_bidi labels =
      if flags.check_bidi then check_domain_bidi_all labels else Ok ()
    in
    match process_domain
            ~validate_label:validate_lookup_label
            ~allow_trailing_root:true
            ~check_domain_bidi:domain_bidi
            ~verify_dns_length_flag:false
            domain
    with
    | Ok labels -> Ok (serialize_unicode labels)
    | Error _ as err -> err

  let to_ascii ?(flags = default_flags) domain =
    let domain_bidi labels =
      if flags.check_bidi then check_domain_bidi_all labels else Ok ()
    in
    match process_domain
            ~validate_label:validate_lookup_label
            ~allow_trailing_root:true
            ~check_domain_bidi:domain_bidi
            ~verify_dns_length_flag:false
            domain
    with
    | Ok labels -> serialize_ascii labels
    | Error _ as err -> err
end

module Uts46 = struct
  type flags = uts46_flags = {
    check_hyphens : bool;
    check_bidi : bool;
    check_joiners : bool;
    use_std3_ascii_rules : bool;
    verify_dns_length : bool;
    ignore_invalid_punycode : bool;
  }

  let default_flags = default_uts46_flags

  type result = uts46_result = {
    value : string;
    errored : bool;
  }

  let to_unicode = uts46_to_unicode
  let to_ascii = uts46_to_ascii
end

module Diagnostics = struct
  type policy = [ `Registration | `Lookup | `Uts46 ]
  type operation = [ `Check_label | `To_unicode | `To_ascii | `Is_valid_hostname ]

  type severity = Error | Warning | Info

  type stage =
    | Input
    | Utf8_decode
    | Mapping
    | Normalization
    | Label_split
    | Label_classification
    | A_label
    | Codepoint
    | Context
    | Bidi
    | Dns_length
    | Serialization

  type code =
    | Empty_input
    | Empty_label
    | Label_ascii_nr_ldh
    | Label_u_label
    | Label_a_label
    | Ascii_lowercased
    | Label_not_nfc
    | Hyphen_start
    | Hyphen_end
    | Hyphen_3_4
    | Reserved_xn_prefix
    | Initial_combiner
    | Codepoint_disallowed
    | Contextj_failed
    | Contexto_failed
    | Bidi_failed
    | A_label_non_ascii
    | A_label_trailing_hyphen
    | A_label_invalid_punycode
    | A_label_decodes_to_ascii
    | A_label_not_canonical
    | A_label_not_lowercase_canonical
    | Dns_label_too_long
    | Dns_domain_too_long
    | Trailing_root_present
    | Trailing_root_rejected
    | Uts46_mapped
    | Uts46_ignored
    | Uts46_deviation
    | Uts46_disallowed
    | Std3_disallowed
    | Ignore_invalid_punycode_applied
    | Idna2008_nv8
    | Idna2008_xv8
    | Invalid_utf8
    | Serialization_failed

  let string_of_code = function
    | Empty_input -> "empty_input"
    | Empty_label -> "empty_label"
    | Label_ascii_nr_ldh -> "label_ascii_nr_ldh"
    | Label_u_label -> "label_u_label"
    | Label_a_label -> "label_a_label"
    | Ascii_lowercased -> "ascii_lowercased"
    | Label_not_nfc -> "label_not_nfc"
    | Hyphen_start -> "hyphen_start"
    | Hyphen_end -> "hyphen_end"
    | Hyphen_3_4 -> "hyphen_3_4"
    | Reserved_xn_prefix -> "reserved_xn_prefix"
    | Initial_combiner -> "initial_combiner"
    | Codepoint_disallowed -> "codepoint_disallowed"
    | Contextj_failed -> "contextj_failed"
    | Contexto_failed -> "contexto_failed"
    | Bidi_failed -> "bidi_failed"
    | A_label_non_ascii -> "a_label_non_ascii"
    | A_label_trailing_hyphen -> "a_label_trailing_hyphen"
    | A_label_invalid_punycode -> "a_label_invalid_punycode"
    | A_label_decodes_to_ascii -> "a_label_decodes_to_ascii"
    | A_label_not_canonical -> "a_label_not_canonical"
    | A_label_not_lowercase_canonical -> "a_label_not_lowercase_canonical"
    | Dns_label_too_long -> "dns_label_too_long"
    | Dns_domain_too_long -> "dns_domain_too_long"
    | Trailing_root_present -> "trailing_root_present"
    | Trailing_root_rejected -> "trailing_root_rejected"
    | Uts46_mapped -> "uts46_mapped"
    | Uts46_ignored -> "uts46_ignored"
    | Uts46_deviation -> "uts46_deviation"
    | Uts46_disallowed -> "uts46_disallowed"
    | Std3_disallowed -> "std3_disallowed"
    | Ignore_invalid_punycode_applied -> "ignore_invalid_punycode_applied"
    | Idna2008_nv8 -> "idna2008_nv8"
    | Idna2008_xv8 -> "idna2008_xv8"
    | Invalid_utf8 -> "invalid_utf8"
    | Serialization_failed -> "serialization_failed"

  type event = {
    severity : severity;
    stage : stage;
    code : code;
    label_index : int option;
    cp_index : int option;
    cp : int option;
    detail : string option;
  }

  type report = {
    policy : policy;
    operation : operation;
    input : string;
    output : string option;
    accepted : bool;
    events : event list;
  }

  type builder = {
    policy : policy;
    operation : operation;
    input : string;
    mutable output : string option;
    mutable accepted : bool;
    mutable events_rev : event list;
  }

  let make_builder policy operation input =
    { policy; operation; input; output = None; accepted = true; events_rev = [] }

  let add_event ?label_index ?cp_index ?cp ?detail builder severity stage code =
    (match severity with Error -> builder.accepted <- false | Warning | Info -> ());
    builder.events_rev <- {
      severity;
      stage;
      code;
      label_index;
      cp_index;
      cp;
      detail;
    } :: builder.events_rev

  let finish builder =
    {
      policy = builder.policy;
      operation = builder.operation;
      input = builder.input;
      output = builder.output;
      accepted = builder.accepted;
      events = List.rev builder.events_rev;
    }

  let set_output builder output =
    builder.output <- Some output

  let emit_idna2008_provenance builder ~stage ~label_index cps =
    List.iteri (fun cp_index cp ->
      if Intranges.contains cp Idna_tables.uts46_nv8 then
        add_event ~label_index ~cp_index ~cp builder Warning stage Idna2008_nv8;
      if Intranges.contains cp Idna_tables.uts46_xv8 then
        add_event ~label_index ~cp_index ~cp builder Warning stage Idna2008_xv8
    ) cps

  let emit_label_classification builder ~label_index label =
    if has_xn_prefix label then
      add_event ~label_index builder Info Label_classification Label_a_label
    else if string_is_ascii label then
      add_event ~label_index builder Info Label_classification Label_ascii_nr_ldh
    else
      add_event ~label_index builder Info Label_classification Label_u_label

  let emit_ascii_lowercased_if_needed builder ~stage ~label_index original lowered =
    if original <> lowered then
      add_event ~label_index builder Info stage Ascii_lowercased

  let diagnose_hyphen builder ~label_index cps =
    let len = List.length cps in
    if len = 0 then begin
      add_event ~label_index builder Error Label_split Empty_label;
      false
    end else if List.hd cps = 0x2D then begin
      add_event ~label_index builder Error Codepoint Hyphen_start ~cp:0x2D;
      false
    end else if List.nth cps (len - 1) = 0x2D then begin
      add_event ~label_index builder Error Codepoint Hyphen_end ~cp:0x2D;
      false
    end else if len >= 4 && List.nth cps 2 = 0x2D && List.nth cps 3 = 0x2D then begin
      add_event ~label_index builder Error Codepoint Hyphen_3_4 ~cp:0x2D;
      false
    end else
      true

  let diagnose_nfc builder ~label_index cps =
    if nfc cps = cps then true
    else begin
      add_event ~label_index builder Error Normalization Label_not_nfc;
      false
    end

  let diagnose_initial_combiner builder ~label_index cps =
    match cps with
    | [] -> true
    | cp :: _ ->
      if Intranges.contains cp Idna_tables.general_category_m then begin
        add_event ~label_index ~cp_index:0 ~cp builder Error Codepoint Initial_combiner;
        false
      end else
        true

  let diagnose_codepoints_registration builder ~label_index cps =
    emit_idna2008_provenance builder ~stage:Codepoint ~label_index cps;
    let rec check i = function
      | [] -> true
      | cp :: rest ->
        if Intranges.contains cp Idna_tables.codepoint_pvalid then
          check (i + 1) rest
        else if Intranges.contains cp Idna_tables.codepoint_contextj then
          check (i + 1) rest
        else if Intranges.contains cp Idna_tables.codepoint_contexto then
          if valid_contexto cps i cp then
            check (i + 1) rest
          else begin
            add_event ~label_index ~cp_index:i ~cp builder Error Context Contexto_failed;
            false
          end
        else begin
          add_event ~label_index ~cp_index:i ~cp builder Error Codepoint Codepoint_disallowed;
          false
        end
    in
    check 0 cps

  let diagnose_codepoints_lookup builder ~label_index cps =
    emit_idna2008_provenance builder ~stage:Codepoint ~label_index cps;
    let rec check i = function
      | [] -> true
      | cp :: rest ->
        if Intranges.contains cp Idna_tables.codepoint_pvalid
           || Intranges.contains cp Idna_tables.codepoint_contextj
           || Intranges.contains cp Idna_tables.codepoint_contexto
        then
          check (i + 1) rest
        else begin
          add_event ~label_index ~cp_index:i ~cp builder Error Codepoint Codepoint_disallowed;
          false
        end
    in
    check 0 cps

  let diagnose_contextj builder ~label_index cps =
    let arr = Array.of_list cps in
    let len = Array.length arr in
    let rec check i =
      if i >= len then true
      else
        let cp = arr.(i) in
        if not (Intranges.contains cp Idna_tables.codepoint_contextj) then
          check (i + 1)
        else if i > 0 && Intranges.contains arr.(i - 1) Idna_tables.virama then
          check (i + 1)
        else if cp = 0x200C then begin
          let has_left = ref false in
          let j = ref (i - 1) in
          while !j >= 0 && not !has_left do
            match joining_type arr.(!j) with
            | Some 4 -> decr j
            | Some (2 | 1) -> has_left := true
            | _ -> j := -1
          done;
          if !has_left then begin
            let has_right = ref false in
            let j = ref (i + 1) in
            while !j < len && not !has_right do
              match joining_type arr.(!j) with
              | Some 4 -> incr j
              | Some (3 | 1) -> has_right := true
              | _ -> j := len
            done;
            if !has_right then
              check (i + 1)
            else begin
              add_event ~label_index ~cp_index:i ~cp builder Error Context Contextj_failed;
              false
            end
          end else begin
            add_event ~label_index ~cp_index:i ~cp builder Error Context Contextj_failed;
            false
          end
        end else begin
          add_event ~label_index ~cp_index:i ~cp builder Error Context Contextj_failed;
          false
        end
    in
    check 0

  let diagnose_bidi_label builder ~label_index cps =
    match check_bidi_label cps with
    | Ok () -> true
    | Error msg ->
      add_event ~label_index ~detail:msg builder Error Bidi Bidi_failed;
      false

  let diagnose_unicode_registration builder ~label_index cps =
    diagnose_nfc builder ~label_index cps
    && diagnose_hyphen builder ~label_index cps
    && diagnose_initial_combiner builder ~label_index cps
    && diagnose_codepoints_registration builder ~label_index cps
    && diagnose_contextj builder ~label_index cps
    && (if label_has_rtl cps then diagnose_bidi_label builder ~label_index cps else true)

  let diagnose_unicode_lookup builder ~label_index cps =
    diagnose_nfc builder ~label_index cps
    && diagnose_hyphen builder ~label_index cps
    && diagnose_initial_combiner builder ~label_index cps
    && diagnose_codepoints_lookup builder ~label_index cps
    && diagnose_contextj builder ~label_index cps

  let diagnose_ascii_label builder ~label_index label =
    let lower = String.lowercase_ascii label in
    emit_label_classification builder ~label_index label;
    emit_ascii_lowercased_if_needed builder ~stage:Label_classification ~label_index label lower;
    let cps = List.init (String.length lower) (fun i -> Char.code lower.[i]) in
    if diagnose_hyphen builder ~label_index cps
       && diagnose_codepoints_registration builder ~label_index cps
    then
      Some { unicode = lower; cps }
    else
      None

  let diagnose_utf8_label builder ~label_index ~validate_unicode label =
    emit_label_classification builder ~label_index label;
    match Utf8.to_cps label with
    | Error msg ->
      add_event ~label_index ~detail:msg builder Error Utf8_decode Invalid_utf8;
      None
    | Ok cps ->
      if validate_unicode builder ~label_index cps then
        Some { unicode = cps_to_utf8 cps; cps }
      else
        None

  let diagnose_apparent_alabel
      builder ~label_index ~allow_mixed_case ~validate_unicode label
    =
    let len = String.length label in
    let lower = String.lowercase_ascii label in
    emit_label_classification builder ~label_index label;
    if allow_mixed_case then
      emit_ascii_lowercased_if_needed builder ~stage:A_label ~label_index label lower;
    if not (string_is_ascii label) then begin
      add_event ~label_index builder Error A_label A_label_non_ascii;
      None
    end else if lower.[len - 1] = '-' then begin
      add_event ~label_index ~cp:0x2D builder Error A_label A_label_trailing_hyphen;
      None
    end else
      match Punycode.decode (String.sub lower 4 (len - 4)) with
      | Error msg ->
        add_event ~label_index ~detail:msg builder Error A_label A_label_invalid_punycode;
        None
      | Ok cps ->
        if cps_are_ascii cps then begin
          add_event ~label_index builder Error A_label A_label_decodes_to_ascii;
          None
        end else if not (validate_unicode builder ~label_index cps) then
          None
        else
          match Punycode.encode cps with
          | Error msg ->
            add_event ~label_index ~detail:msg builder Error A_label A_label_invalid_punycode;
            None
          | Ok encoded ->
            let canonical = "xn--" ^ encoded in
            if canonical <> lower then begin
              add_event ~label_index builder Error A_label A_label_not_canonical;
              None
            end else if (not allow_mixed_case) && canonical <> label then begin
              add_event ~label_index builder Error A_label A_label_not_lowercase_canonical;
              None
            end else
              Some { unicode = cps_to_utf8 cps; cps }

  let diagnose_registration_label builder ~label_index label =
    if String.length label = 0 then begin
      add_event ~label_index builder Error Input Empty_label;
      None
    end else if has_xn_prefix label then
      diagnose_apparent_alabel
        builder
        ~label_index
        ~allow_mixed_case:false
        ~validate_unicode:diagnose_unicode_registration
        label
    else if string_is_ascii label then
      diagnose_ascii_label builder ~label_index label
    else
      diagnose_utf8_label builder ~label_index ~validate_unicode:diagnose_unicode_registration label

  let diagnose_lookup_label builder ~label_index label =
    if String.length label = 0 then begin
      add_event ~label_index builder Error Input Empty_label;
      None
    end else if has_xn_prefix label then
      diagnose_apparent_alabel
        builder
        ~label_index
        ~allow_mixed_case:true
        ~validate_unicode:diagnose_unicode_lookup
        label
    else if string_is_ascii label then
      diagnose_ascii_label builder ~label_index label
    else
      diagnose_utf8_label builder ~label_index ~validate_unicode:diagnose_unicode_lookup label

  let diagnose_domain_bidi builder labels =
    if not (labels_have_rtl labels) then
      true
    else
      let rec check idx = function
        | [] -> true
        | Root :: rest -> check (idx + 1) rest
        | Label label :: rest ->
          if diagnose_bidi_label builder ~label_index:idx label.cps then
            check (idx + 1) rest
          else
            false
      in
      check 0 labels

  let diagnose_dns_length builder labels =
    match domain_ascii_labels labels with
    | Error msg ->
      add_event ~detail:msg builder Error Serialization Serialization_failed;
      false
    | Ok parts ->
      let has_root =
        match List.rev labels with
        | Root :: _ -> true
        | _ -> false
      in
      let parts_no_root =
        if has_root then List.filter (fun s -> s <> "") parts else parts
      in
      if parts_no_root = [] then begin
        add_event builder Error Dns_length Empty_input;
        false
      end else
        let offending_label =
          let rec find idx = function
            | [] -> None
            | Root :: rest -> find (idx + 1) rest
            | Label label :: rest ->
              (match ace_of_cps label.cps with
               | Ok ace when String.length ace = 0 || String.length ace > 63 -> Some idx
               | Ok _ -> find (idx + 1) rest
               | Error _ -> Some idx)
          in
          find 0 labels
        in
        match offending_label with
        | Some idx ->
          add_event ~label_index:idx builder Error Dns_length Dns_label_too_long;
          false
        | None ->
          let total =
            List.fold_left (fun acc part -> acc + String.length part) 0 parts_no_root
            + max 0 (List.length parts_no_root - 1)
          in
          if total > 253 then begin
            add_event builder Error Dns_length Dns_domain_too_long;
            false
          end else
            true

  let serialize_ascii_diag builder labels =
    match domain_ascii_labels labels with
    | Error msg ->
      add_event ~detail:msg builder Error Serialization Serialization_failed;
      None
    | Ok parts -> Some (String.concat "." parts)

  let serialize_unicode_diag labels =
    serialize_unicode labels

  let diagnose_process_domain
      builder
      ~validate_label
      ~allow_trailing_root
      ~check_domain_bidi
      ~verify_dns_length_flag
      domain
    =
    if String.length domain = 0 then begin
      add_event builder Error Input Empty_input;
      None
    end else
      let raw_labels = String.split_on_char '.' domain in
      let last_index = List.length raw_labels - 1 in
      let rec collect idx acc = function
        | [] -> Some (List.rev acc)
        | label :: rest ->
          if label = "" then
            if allow_trailing_root && idx = last_index then begin
              add_event ~label_index:idx builder Info Label_split Trailing_root_present;
              collect (idx + 1) (Root :: acc) rest
            end else begin
              let code = if idx = last_index then Trailing_root_rejected else Empty_label in
              add_event ~label_index:idx builder Error Label_split code;
              None
            end
          else
            match validate_label builder ~label_index:idx label with
            | Some validated -> collect (idx + 1) (Label validated :: acc) rest
            | None -> None
      in
      match collect 0 [] raw_labels with
      | None -> None
      | Some labels ->
        if check_domain_bidi builder labels
           && ((not verify_dns_length_flag) || diagnose_dns_length builder labels)
        then Some labels
        else None

  type uts46_label = {
    idx : int;
    utf8 : string;
    cps : int list;
  }

  let emit_uts46_mapping builder cp =
    if Intranges.contains cp Idna_tables.uts46_nv8 then
      add_event ~cp builder Warning Mapping Idna2008_nv8;
    if Intranges.contains cp Idna_tables.uts46_xv8 then
      add_event ~cp builder Warning Mapping Idna2008_xv8;
    match uts46_status cp with
    | `Map _ ->
      add_event ~cp builder Info Mapping Uts46_mapped
    | `Ignored ->
      add_event ~cp builder Info Mapping Uts46_ignored
    | `Deviation ->
      add_event ~cp builder Warning Mapping Uts46_deviation
    | `Valid | `Disallowed -> ()

  let uts46_map_diag builder cps =
    List.concat_map (fun cp ->
      emit_uts46_mapping builder cp;
      match uts46_status cp with
      | `Map mapped -> mapped
      | `Ignored -> []
      | `Valid | `Deviation | `Disallowed -> [cp]
    ) cps

  let diagnose_uts46_label builder ~flags ~label_index cps =
    emit_idna2008_provenance builder ~stage:Codepoint ~label_index cps;
    diagnose_nfc builder ~label_index cps
    &&
    (if flags.check_hyphens then
       diagnose_hyphen builder ~label_index cps
     else
       match cps with
       | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ ->
         add_event ~label_index builder Error A_label Reserved_xn_prefix;
         false
       | _ -> true)
    && diagnose_initial_combiner builder ~label_index cps
    &&
    (let rec check i = function
      | [] -> true
      | cp :: rest ->
        let std3_ok =
          not flags.use_std3_ascii_rules
          || cp >= 0x80
          || (cp >= 0x61 && cp <= 0x7A)
          || (cp >= 0x30 && cp <= 0x39)
          || cp = 0x2D
        in
        if not std3_ok then begin
          add_event ~label_index ~cp_index:i ~cp builder Error Codepoint Std3_disallowed;
          false
        end else
          match uts46_status cp with
          | `Valid | `Deviation -> check (i + 1) rest
          | `Map _ ->
            add_event ~label_index ~cp_index:i ~cp builder Error Codepoint Uts46_mapped;
            false
          | `Ignored ->
            add_event ~label_index ~cp_index:i ~cp builder Error Codepoint Uts46_ignored;
            false
          | `Disallowed ->
            add_event ~label_index ~cp_index:i ~cp builder Error Codepoint Uts46_disallowed;
            false
    in
     check 0 cps)
    && (if flags.check_joiners then diagnose_contextj builder ~label_index cps else true)
    && (if flags.check_bidi then
          if label_has_rtl cps then diagnose_bidi_label builder ~label_index cps else true
        else true)

  let diagnose_uts46_process builder ~flags domain =
    let labels =
      match Utf8.to_cps domain with
      | Error _ ->
        add_event builder Error Utf8_decode Invalid_utf8;
        let lowered = lowercase_ascii_bytes domain in
        String.split_on_char '.' lowered
        |> List.mapi (fun idx label ->
          if label = "" then
            add_event ~label_index:idx builder Info Label_split Trailing_root_present;
          if label <> "" then emit_label_classification builder ~label_index:idx label;
          if string_is_ascii label then
            { idx; utf8 = label; cps = List.init (String.length label) (fun i -> Char.code label.[i]) }
          else
            { idx; utf8 = label; cps = [] })
      | Ok input_cps ->
        let mapped = uts46_map_diag builder input_cps in
        let normalized = nfc mapped in
        let label_cps_list = split_on_dots normalized in
        List.mapi (fun idx label_cps ->
          if label_cps = [] then begin
            add_event ~label_index:idx builder Info Label_split Trailing_root_present;
            { idx; utf8 = ""; cps = [] }
          end else
            let label_utf8 = cps_to_utf8 label_cps in
            let is_xn =
              List.length label_cps >= 4
              && List.nth label_cps 0 = 0x78
              && List.nth label_cps 1 = 0x6E
              && List.nth label_cps 2 = 0x2D
              && List.nth label_cps 3 = 0x2D
            in
            emit_label_classification builder ~label_index:idx label_utf8;
            if is_xn then begin
              if not flags.check_hyphens then
                add_event ~label_index:idx builder Error A_label Reserved_xn_prefix;
              if not (cps_are_ascii label_cps) then begin
                add_event ~label_index:idx builder Error A_label A_label_non_ascii;
                { idx; utf8 = label_utf8; cps = label_cps }
              end else
                let encoded = String.sub label_utf8 4 (String.length label_utf8 - 4) in
                if encoded = "" then begin
                  add_event ~label_index:idx builder Error A_label A_label_invalid_punycode;
                  { idx; utf8 = ""; cps = [] }
                end else
                  match Punycode.decode encoded with
                  | Error msg ->
                    if flags.ignore_invalid_punycode then begin
                      add_event ~label_index:idx ~detail:msg builder Warning A_label Ignore_invalid_punycode_applied;
                      { idx; utf8 = label_utf8; cps = label_cps }
                    end else begin
                      add_event ~label_index:idx ~detail:msg builder Error A_label A_label_invalid_punycode;
                      { idx; utf8 = label_utf8; cps = label_cps }
                    end
                  | Ok decoded_cps ->
                    let u_label = cps_to_utf8 decoded_cps in
                    if decoded_cps = [] then
                      add_event ~label_index:idx builder Error A_label A_label_invalid_punycode;
                    if cps_are_ascii decoded_cps then
                      add_event ~label_index:idx builder Error A_label A_label_decodes_to_ascii;
                    ignore (diagnose_uts46_label builder ~flags ~label_index:idx decoded_cps);
                    { idx; utf8 = u_label; cps = decoded_cps }
            end else begin
              ignore (diagnose_uts46_label builder ~flags ~label_index:idx label_cps);
              { idx; utf8 = label_utf8; cps = label_cps }
            end
        ) label_cps_list
    in
    let n = List.length labels in
    List.iter (fun label ->
      if label.cps = [] && label.idx <> n - 1 then
        add_event ~label_index:label.idx builder Error Label_split Empty_label
    ) labels;
    if flags.check_bidi then begin
      let all_cps = List.filter (fun cps -> cps <> []) (List.map (fun label -> label.cps) labels) in
      if List.exists label_has_rtl all_cps then
        List.iter (fun label ->
          if label.cps <> [] then
            ignore (diagnose_bidi_label builder ~label_index:label.idx label.cps)
        ) labels
    end;
    labels

  module Registration = struct
    let check_label label =
      let builder = make_builder `Registration `Check_label label in
      ignore (diagnose_registration_label builder ~label_index:0 label);
      finish builder

    let to_unicode ?(flags = default_registration_hostname_flags) domain =
      let builder = make_builder `Registration `To_unicode domain in
      match diagnose_process_domain
              builder
              ~validate_label:diagnose_registration_label
              ~allow_trailing_root:false
              ~check_domain_bidi:(fun _ _ -> true)
              ~verify_dns_length_flag:flags.verify_dns_length
              domain
      with
      | Some labels ->
        set_output builder (serialize_unicode_diag labels);
        finish builder
      | None ->
        finish builder

    let to_ascii ?(flags = default_registration_hostname_flags) domain =
      let builder = make_builder `Registration `To_ascii domain in
      match diagnose_process_domain
              builder
              ~validate_label:diagnose_registration_label
              ~allow_trailing_root:false
              ~check_domain_bidi:(fun _ _ -> true)
              ~verify_dns_length_flag:flags.verify_dns_length
              domain
      with
      | Some labels ->
        (match serialize_ascii_diag builder labels with
         | Some output -> set_output builder output
         | None -> ());
        finish builder
      | None ->
        finish builder

    let is_valid_hostname ?(flags = default_registration_hostname_flags) domain =
      let builder = make_builder `Registration `Is_valid_hostname domain in
      match diagnose_process_domain
              builder
              ~validate_label:diagnose_registration_label
              ~allow_trailing_root:false
              ~check_domain_bidi:(fun _ _ -> true)
              ~verify_dns_length_flag:flags.verify_dns_length
              domain
      with
      | Some labels ->
        (match serialize_ascii_diag builder labels with
         | Some output -> set_output builder output
         | None -> ());
        finish builder
      | None ->
        finish builder
  end

  module Lookup = struct
    let to_unicode ?(flags = default_lookup_flags) domain =
      let builder = make_builder `Lookup `To_unicode domain in
      let check_domain_bidi builder labels =
        if flags.check_bidi then diagnose_domain_bidi builder labels else true
      in
      match diagnose_process_domain
              builder
              ~validate_label:diagnose_lookup_label
              ~allow_trailing_root:true
              ~check_domain_bidi
              ~verify_dns_length_flag:false
              domain
      with
      | Some labels ->
        set_output builder (serialize_unicode_diag labels);
        finish builder
      | None ->
        finish builder

    let to_ascii ?(flags = default_lookup_flags) domain =
      let builder = make_builder `Lookup `To_ascii domain in
      let check_domain_bidi builder labels =
        if flags.check_bidi then diagnose_domain_bidi builder labels else true
      in
      match diagnose_process_domain
              builder
              ~validate_label:diagnose_lookup_label
              ~allow_trailing_root:true
              ~check_domain_bidi
              ~verify_dns_length_flag:false
              domain
      with
      | Some labels ->
        (match serialize_ascii_diag builder labels with
         | Some output -> set_output builder output
         | None -> ());
        finish builder
      | None ->
        finish builder
  end

  module Uts46 = struct
    let to_unicode ?(flags = default_uts46_flags) domain =
      let builder = make_builder `Uts46 `To_unicode domain in
      if String.length domain = 0 then begin
        add_event builder Error Input Empty_input;
        set_output builder "";
        finish builder
      end else
        let labels = diagnose_uts46_process builder ~flags domain in
        let output = String.concat "." (List.map (fun label -> label.utf8) labels) in
        set_output builder output;
        finish builder

    let to_ascii ?(flags = default_uts46_flags) domain =
      let builder = make_builder `Uts46 `To_ascii domain in
      if String.length domain = 0 then begin
        add_event builder Error Input Empty_input;
        finish builder
      end else
        let labels = diagnose_uts46_process builder ~flags domain in
        if not builder.accepted then
          finish builder
        else
          let parts = List.map (fun label ->
            if cps_are_ascii label.cps || label.cps = [] then Ok (label.idx, label.utf8)
            else
              match Punycode.encode label.cps with
              | Error msg -> Error (label.idx, msg)
              | Ok encoded -> Ok (label.idx, "xn--" ^ encoded)
          ) labels in
          let rec collect acc = function
            | [] -> Some (List.rev acc)
            | Ok part :: rest -> collect (part :: acc) rest
            | Error (idx, msg) :: _ ->
              add_event ~label_index:idx ~detail:msg builder Error Serialization Serialization_failed;
              None
          in
          match collect [] parts with
          | None -> finish builder
          | Some parts ->
            if flags.verify_dns_length
               && parts <> [] && snd (List.hd (List.rev parts)) = ""
            then begin
              let idx = fst (List.hd (List.rev parts)) in
              add_event ~label_index:idx builder Error Dns_length Trailing_root_rejected;
              finish builder
            end else
              let strings = List.map snd parts in
              let result = String.concat "." strings in
              let too_long =
                List.find_opt (fun (_, part) -> String.length part > 63) parts
              in
              (match too_long with
               | Some (idx, _) ->
                 if flags.verify_dns_length then
                   add_event ~label_index:idx builder Error Dns_length Dns_label_too_long
               | None -> ());
              if flags.verify_dns_length && String.length result > 253 then
                add_event builder Error Dns_length Dns_domain_too_long;
              if builder.accepted then set_output builder result;
              finish builder
  end
end

module Punycode = Punycode
module Utf8 = Utf8
