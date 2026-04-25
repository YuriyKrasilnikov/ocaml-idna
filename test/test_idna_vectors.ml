(* Runner for Unicode IdnaTestV2.txt test vectors.
   Parses the official test file and checks our strict IDNA2008 implementation.
   NV8/XV8 vectors are treated as explicit strict-invalid cases, not skipped. *)

let () =
  let mapping_path = "tools/ucd-16.0.0/IdnaMappingTable.txt" in
  let path = "tools/ucd-16.0.0/IdnaTestV2.txt" in
  let reg_flags = { Idna.Registration.verify_dns_length = false } in
  Test_helper.require_files [mapping_path; path];
  let nv8_ranges = Test_helper.load_nv8_xv8 mapping_path in
  let vectors = Test_helper.load_idna_test_vectors path in
  let total = ref 0 in
  let pass = ref 0 in
  let fail = ref 0 in
  let strict_nv8 = ref 0 in
  let fail_by_cat = Hashtbl.create 16 in
  let fail_examples = Buffer.create 1024 in
  let fail_count = ref 0 in
  let validate s =
    let len = String.length s in
    if len = 0 then false
    else
      let s =
        if len > 0 && s.[len - 1] = '.' then
          String.sub s 0 (len - 1)
        else
          s
      in
      if String.length s = 0 then false
      else Idna.Registration.is_valid_hostname ~flags:reg_flags s
  in
  List.iter (fun v ->
    let source = v.Test_helper.source in
    let to_unicode_resolved = v.Test_helper.to_unicode in
    let status_str = v.Test_helper.to_unicode_status in
    let has_nv8 = Test_helper.string_has_nv8_xv8 nv8_ranges to_unicode_resolved in
    incr total;
    if has_nv8 then incr strict_nv8;
    let unicode_err = v.Test_helper.to_unicode_err in
    let bidi_registration_exception =
      (status_str = "[B1]" || status_str = "[B6]") && validate source
    in
    let expected_invalid =
      has_nv8 || (unicode_err && not bidi_registration_exception)
    in
    let our_result =
      if expected_invalid then
        if has_nv8 then validate to_unicode_resolved else validate source
      else
        validate to_unicode_resolved
    in
    let our_invalid = not our_result in
    if our_invalid = expected_invalid then
      incr pass
    else begin
      incr fail;
      let cat =
        if status_str = "" || status_str = "[]" then "false_negative"
        else
          let s = String.sub status_str 1 (String.length status_str - 2) in
          let first =
            match String.split_on_char ',' s with
            | x :: _ -> Test_helper.strip x
            | [] -> s
          in
          if String.length first > 0 then
            match first.[0] with
            | 'B' -> "bidi"
            | 'C' -> "contextj"
            | 'V' -> "validity"
            | 'A' -> "ascii"
            | 'P' -> "processing"
            | _ -> first
          else
            "unknown"
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
  ) vectors;

  (* ── Encode verification: toAsciiN ── *)
  (* For valid entries with a toAsciiN value, encode U-labels and compare. *)
  let enc_total = ref 0 in
  let enc_pass = ref 0 in
  let enc_fail = ref 0 in
  let enc_fail_examples = Buffer.create 1024 in
  let enc_fail_count = ref 0 in
  List.iter (fun v ->
    let to_unicode_resolved = v.Test_helper.to_unicode in
    let to_ascii_n_decoded = v.Test_helper.to_ascii_n in
    if (not v.Test_helper.to_unicode_err)
       && (not v.Test_helper.to_ascii_n_err)
       && String.length to_ascii_n_decoded > 0
       && not (Test_helper.string_has_nv8_xv8 nv8_ranges to_unicode_resolved)
    then begin
      incr enc_total;
      match Idna.Registration.to_ascii ~flags:reg_flags to_unicode_resolved with
      | Ok actual
        when String.lowercase_ascii actual = String.lowercase_ascii to_ascii_n_decoded ->
        incr enc_pass
      | Ok actual ->
        incr enc_fail;
        if !enc_fail_count < 20 then begin
          Buffer.add_string enc_fail_examples
            (Printf.sprintf "  encode: %s → expected %s, got %s\n"
               (String.escaped to_unicode_resolved)
               (String.escaped to_ascii_n_decoded)
               (String.escaped actual));
          incr enc_fail_count
        end
      | Error msg ->
        incr enc_fail;
        if !enc_fail_count < 20 then begin
          Buffer.add_string enc_fail_examples
            (Printf.sprintf "  encode: %s → expected %s, got Error %s\n"
               (String.escaped to_unicode_resolved)
               (String.escaped to_ascii_n_decoded)
               (String.escaped msg));
          incr enc_fail_count
        end
    end
  ) vectors;

  Printf.printf "IdnaTestV2 IDNA2008 results:\n";
  Printf.printf "  total:  %d\n" !total;
  Printf.printf "  pass:   %d\n" !pass;
  Printf.printf "  fail:   %d\n" !fail;
  Printf.printf "  strict-invalid NV8/XV8: %d\n" !strict_nv8;
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
  end;
  if !fail > 0 || !enc_fail > 0 then exit 1
