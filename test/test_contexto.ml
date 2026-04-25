(** Systematic tests for CONTEXTO rules (RFC 5892 Appendix A.3-A.9).

    For each rule, one positive case satisfying the Lookup formula and
    negative cases each violating one condition of the formula. *)

let check_ok name utf8 =
  match Idna.Registration.check_label utf8 with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "%s: expected Ok, got Error %S" name e)

let check_err name utf8 =
  match Idna.Registration.check_label utf8 with
  | Error _ -> ()
  | Ok () -> Alcotest.fail (Printf.sprintf "%s: expected Error, got Ok" name)

let utf8_of_cps cps =
  let buf = Buffer.create 16 in
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

(* ── A.3 MIDDLE DOT (U+00B7) ─────────────────────────────────────
   Lookup: Before(cp) = U+006C AND After(cp) = U+006C *)

let middle_dot = 0x00B7
let l = 0x6C    (* 'l' *)
let a = 0x61    (* 'a' *)

let a3_positive () =
  check_ok "A.3 l·l" (utf8_of_cps [l; middle_dot; l])

let a3_before_not_l () =
  check_err "A.3 before not l" (utf8_of_cps [a; middle_dot; l])

let a3_after_not_l () =
  check_err "A.3 after not l" (utf8_of_cps [l; middle_dot; a])

let a3_both_not_l () =
  check_err "A.3 both not l" (utf8_of_cps [a; middle_dot; a])

let a3_no_before_first_pos () =
  check_err "A.3 at first position" (utf8_of_cps [middle_dot; l])

let a3_no_after_last_pos () =
  check_err "A.3 at last position" (utf8_of_cps [l; middle_dot])

let a3_alone () =
  check_err "A.3 alone" (utf8_of_cps [middle_dot])

(* ── A.4 GREEK LOWER NUMERAL SIGN (U+0375) ────────────────────────
   Lookup: Script(After(cp)) = Greek *)

let keraia = 0x0375
let alpha = 0x03B1     (* Greek α *)
let aleph = 0x05D0     (* Hebrew א — non-Greek *)

let a4_positive () =
  check_ok "A.4 keraia+Greek" (utf8_of_cps [keraia; alpha])

let a4_after_non_greek_latin () =
  check_err "A.4 keraia+Latin" (utf8_of_cps [keraia; a])

let a4_after_non_greek_hebrew () =
  check_err "A.4 keraia+Hebrew" (utf8_of_cps [keraia; aleph])

let a4_no_after_last_pos () =
  check_err "A.4 keraia at end" (utf8_of_cps [alpha; keraia])

let a4_alone () =
  check_err "A.4 keraia alone" (utf8_of_cps [keraia])

(* ── A.5 HEBREW PUNCTUATION GERESH (U+05F3) ───────────────────────
   Lookup: Script(Before(cp)) = Hebrew *)

let geresh = 0x05F3

let a5_positive () =
  check_ok "A.5 Hebrew+geresh" (utf8_of_cps [aleph; geresh])

let a5_before_non_hebrew_latin () =
  check_err "A.5 Latin+geresh" (utf8_of_cps [a; geresh])

let a5_before_non_hebrew_greek () =
  check_err "A.5 Greek+geresh" (utf8_of_cps [alpha; geresh])

let a5_no_before_first_pos () =
  check_err "A.5 geresh at start" (utf8_of_cps [geresh; aleph])

let a5_alone () =
  check_err "A.5 geresh alone" (utf8_of_cps [geresh])

(* ── A.6 HEBREW PUNCTUATION GERSHAYIM (U+05F4) ────────────────────
   Lookup: Script(Before(cp)) = Hebrew *)

let gershayim = 0x05F4

let a6_positive () =
  check_ok "A.6 Hebrew+gershayim" (utf8_of_cps [aleph; gershayim])

let a6_before_non_hebrew_latin () =
  check_err "A.6 Latin+gershayim" (utf8_of_cps [a; gershayim])

let a6_before_non_hebrew_greek () =
  check_err "A.6 Greek+gershayim" (utf8_of_cps [alpha; gershayim])

let a6_no_before_first_pos () =
  check_err "A.6 gershayim at start" (utf8_of_cps [gershayim; aleph])

let a6_alone () =
  check_err "A.6 gershayim alone" (utf8_of_cps [gershayim])

(* ── A.7 KATAKANA MIDDLE DOT (U+30FB) ─────────────────────────────
   Lookup: exists cp in label with Script(cp) in {Hiragana, Katakana, Han} *)

let katakana_dot = 0x30FB
let hiragana_a = 0x3042     (* あ *)
let katakana_a = 0x30A2     (* ア *)
let han_one = 0x4E00        (* 一 *)

let a7_positive_hiragana () =
  check_ok "A.7 dot+Hiragana" (utf8_of_cps [katakana_dot; hiragana_a])

let a7_positive_katakana () =
  check_ok "A.7 dot+Katakana" (utf8_of_cps [katakana_dot; katakana_a])

let a7_positive_han () =
  check_ok "A.7 dot+Han" (utf8_of_cps [katakana_dot; han_one])

let a7_positive_script_before () =
  check_ok "A.7 Hiragana+dot+Latin" (utf8_of_cps [hiragana_a; katakana_dot; a])

let a7_positive_script_far () =
  check_ok "A.7 dot with Han at end"
    (utf8_of_cps [a; katakana_dot; a; han_one])

let a7_no_required_script_all_latin () =
  check_err "A.7 all Latin" (utf8_of_cps [a; katakana_dot; a])

let a7_no_required_script_greek () =
  check_err "A.7 dot with Greek only"
    (utf8_of_cps [alpha; katakana_dot; alpha])

let a7_alone () =
  check_err "A.7 dot alone" (utf8_of_cps [katakana_dot])

(* ── A.8 ARABIC-INDIC DIGITS (U+0660..U+0669) ─────────────────────
   Lookup: no cp in label is in U+06F0..U+06F9.
   Labels prefixed with Arabic letter (U+0627) to satisfy bidi start. *)

let arabic_alef = 0x0627    (* ا *)
let arabic_indic_range = [0x0660; 0x0661; 0x0662; 0x0663; 0x0664;
                          0x0665; 0x0666; 0x0667; 0x0668; 0x0669]
let extended_arabic_range = [0x06F0; 0x06F1; 0x06F2; 0x06F3; 0x06F4;
                             0x06F5; 0x06F6; 0x06F7; 0x06F8; 0x06F9]

let a8_positive_each_digit () =
  List.iter (fun cp ->
    check_ok (Printf.sprintf "A.8 alef+U+%04X" cp)
      (utf8_of_cps [arabic_alef; cp])
  ) arabic_indic_range

let a8_positive_all_together () =
  check_ok "A.8 alef+all arabic-indic"
    (utf8_of_cps (arabic_alef :: arabic_indic_range))

let a8_negative_mixed_with_extended () =
  List.iter (fun ext ->
    check_err (Printf.sprintf "A.8 alef+0660+U+%04X" ext)
      (utf8_of_cps [arabic_alef; 0x0660; ext])
  ) extended_arabic_range

(* ── A.9 EXTENDED ARABIC-INDIC DIGITS (U+06F0..U+06F9) ───────────
   Lookup: no cp in label is in U+0660..U+0669. *)

let a9_positive_each_digit () =
  List.iter (fun cp ->
    check_ok (Printf.sprintf "A.9 alef+U+%04X" cp)
      (utf8_of_cps [arabic_alef; cp])
  ) extended_arabic_range

let a9_positive_all_together () =
  check_ok "A.9 alef+all extended"
    (utf8_of_cps (arabic_alef :: extended_arabic_range))

let a9_negative_mixed_with_arabic () =
  List.iter (fun ar ->
    check_err (Printf.sprintf "A.9 alef+06F0+U+%04X" ar)
      (utf8_of_cps [arabic_alef; 0x06F0; ar])
  ) arabic_indic_range

let () =
  Alcotest.run "contexto" [
    "A.3 MIDDLE DOT", [
      Alcotest.test_case "positive l·l"          `Quick a3_positive;
      Alcotest.test_case "before not l"          `Quick a3_before_not_l;
      Alcotest.test_case "after not l"           `Quick a3_after_not_l;
      Alcotest.test_case "both not l"            `Quick a3_both_not_l;
      Alcotest.test_case "no before (first pos)" `Quick a3_no_before_first_pos;
      Alcotest.test_case "no after (last pos)"   `Quick a3_no_after_last_pos;
      Alcotest.test_case "alone"                 `Quick a3_alone;
    ];
    "A.4 GREEK KERAIA", [
      Alcotest.test_case "positive keraia+Greek" `Quick a4_positive;
      Alcotest.test_case "after non-Greek Latin" `Quick a4_after_non_greek_latin;
      Alcotest.test_case "after non-Greek Hebrew" `Quick a4_after_non_greek_hebrew;
      Alcotest.test_case "no after (last pos)"   `Quick a4_no_after_last_pos;
      Alcotest.test_case "alone"                 `Quick a4_alone;
    ];
    "A.5 HEBREW GERESH", [
      Alcotest.test_case "positive Hebrew+geresh" `Quick a5_positive;
      Alcotest.test_case "before non-Hebrew Latin" `Quick a5_before_non_hebrew_latin;
      Alcotest.test_case "before non-Hebrew Greek" `Quick a5_before_non_hebrew_greek;
      Alcotest.test_case "no before (first pos)" `Quick a5_no_before_first_pos;
      Alcotest.test_case "alone"                 `Quick a5_alone;
    ];
    "A.6 HEBREW GERSHAYIM", [
      Alcotest.test_case "positive Hebrew+gershayim" `Quick a6_positive;
      Alcotest.test_case "before non-Hebrew Latin" `Quick a6_before_non_hebrew_latin;
      Alcotest.test_case "before non-Hebrew Greek" `Quick a6_before_non_hebrew_greek;
      Alcotest.test_case "no before (first pos)" `Quick a6_no_before_first_pos;
      Alcotest.test_case "alone"                 `Quick a6_alone;
    ];
    "A.7 KATAKANA MIDDLE DOT", [
      Alcotest.test_case "positive with Hiragana" `Quick a7_positive_hiragana;
      Alcotest.test_case "positive with Katakana" `Quick a7_positive_katakana;
      Alcotest.test_case "positive with Han"     `Quick a7_positive_han;
      Alcotest.test_case "positive script before dot" `Quick a7_positive_script_before;
      Alcotest.test_case "positive script far from dot" `Quick a7_positive_script_far;
      Alcotest.test_case "no required script (Latin)" `Quick a7_no_required_script_all_latin;
      Alcotest.test_case "no required script (Greek)" `Quick a7_no_required_script_greek;
      Alcotest.test_case "alone"                 `Quick a7_alone;
    ];
    "A.8 ARABIC-INDIC DIGITS", [
      Alcotest.test_case "each digit after alef"  `Quick a8_positive_each_digit;
      Alcotest.test_case "all digits together"    `Quick a8_positive_all_together;
      Alcotest.test_case "mixed with each Extended digit" `Quick a8_negative_mixed_with_extended;
    ];
    "A.9 EXTENDED ARABIC-INDIC DIGITS", [
      Alcotest.test_case "each digit after alef"  `Quick a9_positive_each_digit;
      Alcotest.test_case "all digits together"    `Quick a9_positive_all_together;
      Alcotest.test_case "mixed with each Arabic-Indic digit" `Quick a9_negative_mixed_with_arabic;
    ];
  ]
