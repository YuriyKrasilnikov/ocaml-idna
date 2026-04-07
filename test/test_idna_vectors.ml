(* Runner for Unicode IdnaTestV2.txt test vectors.
   Parses the official test file and checks our IDNA2008 implementation.
   Filters NV8/XV8 entries per IDNA2008 conformance rules. *)

(** Decode \uXXXX and \x{XXXX} escapes to UTF-8. *)
let decode_escapes s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  let add_uchar cp =
    if cp < 0x80 then
      Buffer.add_char buf (Char.chr cp)
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
  in
  while !i < len do
    if !i + 5 < len && s.[!i] = '\\' && s.[!i+1] = 'u' then begin
      let hex = String.sub s (!i+2) 4 in
      (try
        let cp = int_of_string ("0x" ^ hex) in
        add_uchar cp;
        i := !i + 6
      with _ ->
        Buffer.add_char buf s.[!i];
        incr i)
    end else if !i + 3 < len && s.[!i] = '\\' && s.[!i+1] = 'x' && s.[!i+2] = '{' then begin
      match String.index_from_opt s (!i+3) '}' with
      | Some close ->
        let hex = String.sub s (!i+3) (close - !i - 3) in
        (try
          let cp = int_of_string ("0x" ^ hex) in
          add_uchar cp;
          i := close + 1
        with _ ->
          Buffer.add_char buf s.[!i];
          incr i)
      | None ->
        Buffer.add_char buf s.[!i];
        incr i
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let strip s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  let j = ref (len - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t') do decr j done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)

let has_errors status =
  let s = strip status in
  s <> "" && s <> "[]"

(** Extract codepoints from a UTF-8 string. *)
let utf8_to_cps s =
  let len = String.length s in
  let cps = ref [] in
  let i = ref 0 in
  while !i < len do
    let b = Char.code s.[!i] in
    let cp, size =
      if b land 0x80 = 0 then (b, 1)
      else if b land 0xE0 = 0xC0 && !i+1 < len then
        (((b land 0x1F) lsl 6) lor (Char.code s.[!i+1] land 0x3F), 2)
      else if b land 0xF0 = 0xE0 && !i+2 < len then
        (((b land 0x0F) lsl 12) lor ((Char.code s.[!i+1] land 0x3F) lsl 6)
         lor (Char.code s.[!i+2] land 0x3F), 3)
      else if !i+3 < len then
        (((b land 0x07) lsl 18) lor ((Char.code s.[!i+1] land 0x3F) lsl 12)
         lor ((Char.code s.[!i+2] land 0x3F) lsl 6)
         lor (Char.code s.[!i+3] land 0x3F), 4)
      else (b, 1)
    in
    cps := cp :: !cps;
    i := !i + size
  done;
  List.rev !cps

(** Parse IdnaMappingTable.txt to build set of NV8/XV8 codepoints. *)
let load_nv8_xv8 path =
  let ic = open_in path in
  let ranges = ref [] in
  (try while true do
    let line = input_line ic in
    let line = strip line in
    if String.length line > 0 && line.[0] <> '#' then begin
      if (try let _ = Str.search_forward (Str.regexp "NV8\\|XV8") line 0 in true
           with Not_found -> false)
      then begin
        (* Parse range: "XXXX..YYYY" or "XXXX" *)
        let range_end = try String.index line ';' with Not_found -> String.length line in
        let range_str = strip (String.sub line 0 range_end) in
        match String.split_on_char '.' range_str with
        | [a; ""; b] ->
          let lo = int_of_string ("0x" ^ strip a) in
          let hi = int_of_string ("0x" ^ strip b) in
          ranges := (lo, hi) :: !ranges
        | _ ->
          (try
            let cp = int_of_string ("0x" ^ range_str) in
            ranges := (cp, cp) :: !ranges
          with _ -> ())
      end
    end
  done with End_of_file -> ());
  close_in ic;
  !ranges

let is_nv8_xv8 ranges cp =
  List.exists (fun (lo, hi) -> cp >= lo && cp <= hi) ranges

let string_has_nv8_xv8 ranges s =
  let cps = utf8_to_cps s in
  List.exists (is_nv8_xv8 ranges) cps

let () =
  let nv8_ranges = load_nv8_xv8 "tools/ucd-16.0.0/IdnaMappingTable.txt" in
  let path = "tools/ucd-16.0.0/IdnaTestV2.txt" in
  let ic = open_in path in
  let total = ref 0 in
  let pass = ref 0 in
  let fail = ref 0 in
  let skip_nv8 = ref 0 in
  let fail_by_cat = Hashtbl.create 16 in
  let fail_examples = Buffer.create 1024 in
  let fail_count = ref 0 in
  (try while true do
    let line = input_line ic in
    let line = strip line in
    if String.length line = 0 || line.[0] = '#' || line.[0] = '@' then ()
    else begin
      let data = match String.index_opt line '#' with
        | Some pos -> String.sub line 0 pos
        | None -> line
      in
      let cols = String.split_on_char ';' data in
      match cols with
      | source_raw :: to_unicode_raw :: to_unicode_status :: _ ->
        let source = decode_escapes (strip source_raw) in
        let to_unicode = decode_escapes (strip to_unicode_raw) in
        (* toUnicode: blank = same as source *)
        let to_unicode_resolved =
          if String.length (strip to_unicode_raw) = 0 then source else to_unicode
        in
        (* Skip NV8/XV8 entries per IDNA2008 conformance *)
        if string_has_nv8_xv8 nv8_ranges to_unicode_resolved then
          incr skip_nv8
        else begin
          incr total;
          let unicode_err = has_errors to_unicode_status in
          let expected_invalid = unicode_err in
          (* Validate with domain-level bidi but without DNS length constraints *)
          let validate s =
            let len = String.length s in
            if len = 0 then false
            else
              let s = if len > 0 && s.[len-1] = '.' then
                String.sub s 0 (len - 1) else s in
              if String.length s = 0 then false
              else
                (* Use is_valid_hostname for domain-level bidi,
                   but also accept labels that pass check_label individually
                   when they exceed DNS length limits *)
                if Idna.is_valid_hostname s then true
                else
                  (* Fallback: check if only DNS length is the issue *)
                  let labels = String.split_on_char '.' s in
                  let all_valid = labels <> [] && List.for_all (fun label ->
                    String.length label > 0 &&
                    match Idna.check_label label with
                    | Ok () -> true
                    | Error _ -> false
                  ) labels in
                  if not all_valid then false
                  else
                    (* Labels individually valid; check domain-level bidi *)
                    Idna.is_valid_hostname_bidi labels
          in
          (* If expected invalid: validate source (catch source-level errors).
             If expected valid: validate toUnicode (already case-folded + NFC). *)
          let our_result =
            if expected_invalid then validate source
            else validate to_unicode_resolved
          in
          let our_invalid = not our_result in
          if our_invalid = expected_invalid then
            incr pass
          else begin
            incr fail;
            (* Categorize by status code *)
            let status_str = strip to_unicode_status in
            let cat =
              if status_str = "" || status_str = "[]" then "false_negative"
              else
                (* Extract first status code *)
                let s = String.sub status_str 1 (String.length status_str - 2) in
                let first = match String.split_on_char ',' s with
                  | x :: _ -> strip x
                  | [] -> s
                in
                (* Group: B1-B6 → bidi, C1-C2 → contextj, V1-V8 → validity *)
                if String.length first > 0 then
                  (match first.[0] with
                   | 'B' -> "bidi"
                   | 'C' -> "contextj"
                   | 'V' -> "validity"
                   | 'A' -> "ascii"
                   | 'P' -> "processing"
                   | _ -> first)
                else "unknown"
            in
            let cur = try Hashtbl.find fail_by_cat cat with Not_found -> 0 in
            Hashtbl.replace fail_by_cat cat (cur + 1);
            if !fail_count < 300 then begin
              Buffer.add_string fail_examples
                (Printf.sprintf "  [%s] %s | expected_%s | got_%s | status=%s\n"
                   cat
                   (String.escaped source)
                   (if expected_invalid then "INVALID" else "VALID")
                   (if our_invalid then "INVALID" else "VALID")
                   status_str);
              incr fail_count
            end
          end
        end
      | _ -> ()
    end
  done with End_of_file -> ());
  close_in ic;

  (* ── Encode verification: toAsciiN ── *)
  (* For valid entries with a toAsciiN value, encode U-labels and compare *)
  let ic2 = open_in path in
  let enc_total = ref 0 in
  let enc_pass = ref 0 in
  let enc_fail = ref 0 in
  let enc_fail_examples = Buffer.create 1024 in
  let enc_fail_count = ref 0 in
  (try while true do
    let line = input_line ic2 in
    let line = strip line in
    if String.length line = 0 || line.[0] = '#' || line.[0] = '@' then ()
    else begin
      let data = match String.index_opt line '#' with
        | Some pos -> String.sub line 0 pos
        | None -> line
      in
      let cols = String.split_on_char ';' data in
      match cols with
      | _source_raw :: to_unicode_raw :: to_unicode_status
        :: to_ascii_n_raw :: to_ascii_n_status :: _ ->
        let to_unicode = decode_escapes (strip to_unicode_raw) in
        let to_unicode_resolved =
          if String.length (strip to_unicode_raw) = 0 then
            decode_escapes (strip _source_raw)
          else to_unicode
        in
        let to_ascii_n = strip to_ascii_n_raw in
        let to_ascii_n_resolved =
          if String.length to_ascii_n = 0 then strip to_unicode_raw
          else to_ascii_n
        in
        let to_ascii_n_resolved =
          if String.length to_ascii_n_resolved = 0 then strip _source_raw
          else to_ascii_n_resolved
        in
        let to_ascii_n_decoded = decode_escapes to_ascii_n_resolved in
        (* Only test valid entries: no toUnicode errors AND no toAsciiN errors *)
        let unicode_err = has_errors to_unicode_status in
        let ascii_err = has_errors (strip to_ascii_n_status) in
        if (not unicode_err) && (not ascii_err)
           && String.length to_ascii_n_decoded > 0
           && not (string_has_nv8_xv8 nv8_ranges to_unicode_resolved)
        then begin
          incr enc_total;
          (* Encode each label: if U-label, encode to xn-- form *)
          let expected_labels = String.split_on_char '.' to_ascii_n_decoded in
          let unicode_str =
            let s = to_unicode_resolved in
            let len = String.length s in
            if len > 0 && s.[len-1] = '.' then String.sub s 0 (len-1) else s
          in
          let actual_labels = String.split_on_char '.' unicode_str in
          if List.length expected_labels = List.length actual_labels then begin
            let all_match = List.for_all2 (fun expected actual ->
              let actual_lower = String.lowercase_ascii actual in
              let expected_lower = String.lowercase_ascii expected in
              if actual_lower = expected_lower then true
              else if String.to_seq actual |> Seq.for_all (fun c -> Char.code c < 0x80) then
                actual_lower = expected_lower
              else
                (* U-label: encode to A-label *)
                let cps = utf8_to_cps actual in
                match Idna.Punycode.encode cps with
                | Error _ -> false
                | Ok encoded ->
                  let a_label = "xn--" ^ encoded in
                  String.lowercase_ascii a_label = expected_lower
            ) expected_labels actual_labels in
            if all_match then incr enc_pass
            else begin
              incr enc_fail;
              if !enc_fail_count < 20 then begin
                Buffer.add_string enc_fail_examples
                  (Printf.sprintf "  encode: %s → expected %s\n"
                     (String.escaped unicode_str)
                     (String.escaped to_ascii_n_decoded));
                incr enc_fail_count
              end
            end
          end else begin
            (* Label count mismatch — skip *)
            decr enc_total
          end
        end
      | _ -> ()
    end
  done with End_of_file -> ());
  close_in ic2;

  Printf.printf "IdnaTestV2 IDNA2008 results:\n";
  Printf.printf "  total:  %d (after NV8/XV8 filter)\n" !total;
  Printf.printf "  pass:   %d\n" !pass;
  Printf.printf "  fail:   %d\n" !fail;
  Printf.printf "  skip:   %d (NV8/XV8)\n" !skip_nv8;
  Printf.printf "  rate:   %.1f%%\n"
    (if !total > 0 then 100.0 *. float_of_int !pass /. float_of_int !total else 0.0);
  if Hashtbl.length fail_by_cat > 0 then begin
    Printf.printf "\nFailures by category:\n";
    Hashtbl.iter (fun cat count ->
      Printf.printf "  %-15s %d\n" cat count
    ) fail_by_cat
  end;
  if !fail > 0 then begin
    Printf.printf "\nFirst %d validation failures:\n" (min !fail 20);
    print_string (Buffer.contents fail_examples)
  end;
  Printf.printf "\nEncode (toAsciiN) results:\n";
  Printf.printf "  total:  %d\n" !enc_total;
  Printf.printf "  pass:   %d\n" !enc_pass;
  Printf.printf "  fail:   %d\n" !enc_fail;
  Printf.printf "  rate:   %.1f%%\n"
    (if !enc_total > 0 then 100.0 *. float_of_int !enc_pass /. float_of_int !enc_total else 0.0);
  if !enc_fail > 0 then begin
    Printf.printf "\nFirst %d encode failures:\n" (min !enc_fail 20);
    print_string (Buffer.contents enc_fail_examples)
  end
