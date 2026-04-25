(* Behavioral tests for IDNA2008 hostname validation. *)

let check_ok label =
  match Idna.Registration.check_label label with
  | Ok () -> ()
  | Error e -> Alcotest.fail (Printf.sprintf "check_label %S: %s" label e)

let check_err label =
  match Idna.Registration.check_label label with
  | Ok () -> Alcotest.fail (Printf.sprintf "check_label %S: expected error" label)
  | Error _ -> ()

let check_err_msg label expected_prefix =
  match Idna.Registration.check_label label with
  | Ok () -> Alcotest.fail (Printf.sprintf "check_label %S: expected error" label)
  | Error msg ->
    if not (String.length msg >= String.length expected_prefix
            && String.sub msg 0 (String.length expected_prefix) = expected_prefix) then
      Alcotest.fail (Printf.sprintf "check_label %S: expected %S prefix, got %S"
                       label expected_prefix msg)

let valid s =
  Alcotest.(check bool)
    (Printf.sprintf "valid %S" s) true (Idna.Registration.is_valid_hostname s)
let invalid s =
  Alcotest.(check bool)
    (Printf.sprintf "invalid %S" s) false (Idna.Registration.is_valid_hostname s)

(* ══════════════════════════════════════════════════════════════ *)
(* 1. Happy path: check_label                                    *)
(* ══════════════════════════════════════════════════════════════ *)

(* ASCII labels *)
let test_ascii_labels () =
  check_ok "example";
  check_ok "a";
  check_ok "test-label";
  check_ok "123"

(* ASCII case insensitivity *)
let test_ascii_case () =
  check_ok "Example";
  check_ok "EXAMPLE"

(* xn-- labels (A-label) *)
let test_xn_valid () =
  check_ok "xn--maana-pta"   (* mañana *)

let test_xn_uppercase_rejected () =
  check_err_msg "XN--MAANA-PTA" "A-label must be lowercase canonical form"

(* U-labels (non-ASCII UTF-8) *)
let test_ulabel () =
  check_ok "\xc3\xb1"   (* ñ, U+00F1, PVALID, bidi_l *)

(* 4-byte UTF-8 codepoint: U+20000 CJK Extension B *)
let test_4byte_utf8 () =
  check_ok "\xf0\xa0\x80\x80"

(* ══════════════════════════════════════════════════════════════ *)
(* 2. Negative: check_label length errors                        *)
(* ══════════════════════════════════════════════════════════════ *)

let test_empty_label () =
  check_err_msg "" "empty label"

(* check_label does not enforce DNS length — is_valid_hostname does *)
let test_label_too_long () =
  Alcotest.(check bool) "64-char rejected by hostname"
    false (Idna.Registration.is_valid_hostname (String.make 64 'a'))

(* ══════════════════════════════════════════════════════════════ *)
(* 3. Hyphen rules                                               *)
(* ══════════════════════════════════════════════════════════════ *)

let test_hyphen_start () =
  check_err_msg "-abc" "label starts with hyphen"

let test_hyphen_end () =
  check_err_msg "abc-" "label ends with hyphen"

let test_hyphen_34 () =
  check_err_msg "ab--cd" "label has -- at positions 3-4"

let test_hyphen_internal_ok () =
  check_ok "a-b"

(* ══════════════════════════════════════════════════════════════ *)
(* 4. Initial combining mark                                     *)
(* ══════════════════════════════════════════════════════════════ *)

(* U-label starting with U+0300 COMBINING GRAVE ACCENT *)
let test_initial_combiner_ulabel () =
  check_err_msg "\xcc\x80" "label begins with combining mark"

(* A-label: xn--ced decodes to codepoint caught by NFC or combiner check *)
let test_initial_combiner_alabel () =
  check_err "xn--ced"

(* ══════════════════════════════════════════════════════════════ *)
(* 5. NFC Quick Check                                            *)
(* ══════════════════════════════════════════════════════════════ *)

(* U+0344 COMBINING GREEK DIALYTIKA TONOS is NFC_QC=No *)
(* Label: "a" + U+0344 → U-label path *)
let test_nfc_qc_no () =
  check_err_msg "a\xcd\x84" "label not in NFC"

(* ══════════════════════════════════════════════════════════════ *)
(* 6. Disallowed codepoints                                      *)
(* ══════════════════════════════════════════════════════════════ *)

(* U+00AD SOFT HYPHEN is DISALLOWED *)
let test_disallowed () =
  check_err_msg "a\xc2\xad" "codepoint U+00AD not allowed"

(* U+0041 'A' uppercase is DISALLOWED in U-label *)
(* Actually, the U-label path doesn't lowercase, so 'A' might fail *)
(* Let me test with something clearly DISALLOWED *)
(* U+2028 LINE SEPARATOR = \xe2\x80\xa8 *)
let test_disallowed_separator () =
  check_err "a\xe2\x80\xa8"

(* ══════════════════════════════════════════════════════════════ *)
(* 7. CONTEXTO: MIDDLE DOT (U+00B7)                              *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: between two 'l' *)
let test_middle_dot_valid () =
  check_ok "l\xc2\xb7l"

(* Invalid: not between 'l' *)
let test_middle_dot_wrong_neighbors () =
  check_err "a\xc2\xb7a"

(* Invalid: at start of label *)
let test_middle_dot_at_start () =
  check_err "\xc2\xb7l"

(* Invalid: at end of label *)
let test_middle_dot_at_end () =
  check_err "l\xc2\xb7"

(* ══════════════════════════════════════════════════════════════ *)
(* 8. CONTEXTO: GREEK LOWER NUMERAL SIGN (U+0375)                *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: U+03B1 + U+0375 + U+03B1 (α͵α) — next is Greek *)
let test_greek_numeral_valid () =
  check_ok "\xce\xb1\xcd\xb5\xce\xb1"

(* Invalid: at end of label — no next char *)
let test_greek_numeral_at_end () =
  check_err "\xce\xb1\xcd\xb5"

(* ══════════════════════════════════════════════════════════════ *)
(* 9. CONTEXTO: HEBREW GERESH (U+05F3) / GERSHAYIM (U+05F4)     *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: Hebrew ALEF + GERESH *)
let test_hebrew_geresh_valid () =
  check_ok "\xd7\x90\xd7\xb3"

(* Valid: Hebrew ALEF + GERSHAYIM *)
let test_hebrew_gershayim_valid () =
  check_ok "\xd7\x90\xd7\xb4"

(* Invalid: GERESH at start — no preceding Hebrew *)
let test_hebrew_geresh_at_start () =
  check_err "\xd7\xb3\xd7\x90"

(* ══════════════════════════════════════════════════════════════ *)
(* 10. CONTEXTO: KATAKANA MIDDLE DOT (U+30FB)                    *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: with Katakana A (U+30A2) *)
let test_katakana_dot_with_katakana () =
  check_ok "\xe3\x83\xbb\xe3\x82\xa2"

(* Valid: with Hiragana A (U+3042) *)
let test_katakana_dot_with_hiragana () =
  check_ok "\xe3\x83\xbb\xe3\x81\x82"

(* Valid: with CJK Han (U+4E00) *)
let test_katakana_dot_with_han () =
  check_ok "\xe3\x83\xbb\xe4\xb8\x80"

(* Invalid: alone *)
let test_katakana_dot_alone () =
  check_err "xn--vek"

(* ══════════════════════════════════════════════════════════════ *)
(* 11. CONTEXTO: ARABIC-INDIC DIGITS (U+0660-0x0669)             *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: Arabic BA + Arabic-Indic 0 + Arabic BA *)
(* U+0628 = \xd8\xa8, U+0660 = \xd9\xa0 *)
let test_arabic_indic_valid () =
  check_ok "\xd8\xa8\xd9\xa0\xd8\xa8"

(* Invalid: mixed with Extended Arabic-Indic *)
(* U+06F0 = \xdb\xb0 *)
let test_arabic_indic_mixed () =
  check_err "\xd8\xa8\xd9\xa0\xdb\xb0\xd8\xa8"

(* ══════════════════════════════════════════════════════════════ *)
(* 12. CONTEXTO: EXTENDED ARABIC-INDIC DIGITS (U+06F0-0x06F9)    *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: Arabic BA + Extended Arabic-Indic 0 + Arabic BA *)
let test_ext_arabic_valid () =
  check_ok "\xd8\xa8\xdb\xb0\xd8\xa8"

(* Invalid: mixed with Arabic-Indic *)
let test_ext_arabic_mixed () =
  check_err "\xd8\xa8\xdb\xb0\xd9\xa0\xd8\xa8"

(* ══════════════════════════════════════════════════════════════ *)
(* 13. CONTEXTJ: ZWJ (U+200D) and ZWNJ (U+200C)                 *)
(* ══════════════════════════════════════════════════════════════ *)

(* Valid: ZWNJ after virama *)
(* Devanagari KA + virama + ZWNJ + KA *)
(* U+0915=\xe0\xa4\x95, U+094D=\xe0\xa5\x8d, U+200C=\xe2\x80\x8c *)
let test_zwnj_after_virama () =
  check_ok "\xe0\xa4\x95\xe0\xa5\x8d\xe2\x80\x8c\xe0\xa4\x95"

(* Valid: ZWJ after virama *)
(* Devanagari KA + virama + ZWJ + KA *)
let test_zwj_after_virama () =
  check_ok "\xe0\xa4\x95\xe0\xa5\x8d\xe2\x80\x8d\xe0\xa4\x95"

(* Valid: ZWNJ with joining type context *)
(* Arabic BA (D) + ZWNJ + Arabic ALEF (R) *)
(* U+0628=\xd8\xa8, U+200C=\xe2\x80\x8c, U+0627=\xd8\xa7 *)
let test_zwnj_joining_context () =
  check_ok "\xd8\xa8\xe2\x80\x8c\xd8\xa7"

(* Valid: ZWNJ with T (transparent) joining types in between *)
(* Arabic BA (D) + U+0300 (T) + ZWNJ + U+0300 (T) + Arabic ALEF (R) *)
let test_zwnj_joining_with_transparent () =
  check_ok "\xd8\xa8\xcc\x80\xe2\x80\x8c\xcc\x80\xd8\xa7"

(* Invalid: ZWJ without virama *)
let test_zwj_no_virama () =
  check_err "a\xe2\x80\x8da"

(* Invalid: ZWNJ without virama and without joining context *)
let test_zwnj_no_context () =
  check_err "a\xe2\x80\x8ca"

(* Invalid: ZWNJ at start — no left context *)
let test_zwnj_at_start () =
  check_err "\xe2\x80\x8c\xd8\xa8"

(* Invalid: ZWNJ with left but no right *)
let test_zwnj_no_right () =
  check_err "\xd8\xa8\xe2\x80\x8c"

(* ══════════════════════════════════════════════════════════════ *)
(* 14. Bidi rules (RFC 5893)                                     *)
(* ══════════════════════════════════════════════════════════════ *)

(* No RTL — trivially ok (covered by ASCII/Latin tests) *)

(* RTL label with valid ending: Hebrew ALEF + Hebrew BET *)
(* U+05D0=\xd7\x90, U+05D1=\xd7\x91, both bidi_r *)
let test_bidi_rtl_valid () =
  check_ok "\xd7\x90\xd7\x91"

(* RTL label with L character — rule 2: only R,AL,AN,EN,ES,CS,ET,ON,BN,NSM *)
(* Hebrew ALEF + 'a' — 'a' is bidi_l, not allowed in RTL *)
let test_bidi_rtl_invalid_end () =
  check_err_msg "\xd7\x90a" "bidi: RTL label contains invalid"

(* RTL label with NSM at end — look through NSM to find real last *)
(* Hebrew ALEF + Hebrew BET + U+05B0 (SHEVA, bidi_nsm) *)
(* Real last after skipping NSM = BET (bidi_r) → ok *)
let test_bidi_rtl_nsm_end () =
  check_ok "\xd7\x90\xd7\x91\xd6\xb0"

(* First char not R/AL/L when label has RTL *)
(* '1' (bidi_en) + Hebrew ALEF → first is not R/AL/L *)
let test_bidi_first_not_ral () =
  check_err_msg "1\xd7\x90" "bidi: first char must be"

(* LTR label with RTL char — rule 5: only L,EN,ES,CS,ET,ON,BN,NSM *)
let test_bidi_ltr_with_rtl () =
  check_err "a\xd7\x90"

(* Single RTL character *)
let test_bidi_single_rtl () =
  check_ok "\xd7\x90"

(* ══════════════════════════════════════════════════════════════ *)
(* 15. check_label: xn-- with invalid punycode                   *)
(* ══════════════════════════════════════════════════════════════ *)

let test_xn_invalid_punycode () =
  check_err_msg "xn--!!!" "invalid punycode"

(* xn-- that decodes to DISALLOWED codepoint *)
let test_xn_disallowed () =
  check_err "xn--chb89f"

(* Non-canonical A-label: punycode decodes, but round-trip produces xn--nde *)
let test_xn_noncanonical () =
  check_err_msg "xn---nde" "A-label not in canonical form"

(* ══════════════════════════════════════════════════════════════ *)
(* 16. Boundary                                                  *)
(* ══════════════════════════════════════════════════════════════ *)

let test_max_label () =
  check_ok (String.make 63 'a')

let test_single_char () =
  check_ok "a"

(* ══════════════════════════════════════════════════════════════ *)
(* 17. is_valid_hostname                                         *)
(* ══════════════════════════════════════════════════════════════ *)

let test_hostname_valid () =
  valid "example.com";
  valid "a.b.c";
  valid "a";
  valid "xn--maana-pta.com"

let test_hostname_empty () = invalid ""

let test_hostname_too_long () =
  invalid (String.make 254 'a')

let test_hostname_max () =
  (* 253 chars: 63+1+63+1+63+1+61 *)
  let h = String.concat "." [
    String.make 63 'a'; String.make 63 'b';
    String.make 63 'c'; String.make 61 'd'
  ] in
  Alcotest.(check int) "len=253" 253 (String.length h);
  valid h

let test_hostname_trailing_dot () = invalid "example."
let test_hostname_leading_dot () = invalid ".example"
let test_hostname_double_dot () = invalid "example..com"
let test_hostname_invalid_label () = invalid "-bad.com"

let test_hostname_single_label () = valid "localhost"

let test_hostname_xn () =
  valid "xn--maana-pta.com";
  invalid "xn--X.com"

let test_hostname_bidi_digit_after_rtl () =
  valid "\xd7\x90\xd7\x91.1com"

let () =
  Alcotest.run "idna" [
    "check_label:happy", [
      Alcotest.test_case "ascii labels" `Quick test_ascii_labels;
      Alcotest.test_case "ascii case" `Quick test_ascii_case;
      Alcotest.test_case "xn-- valid" `Quick test_xn_valid;
      Alcotest.test_case "xn-- uppercase rejected" `Quick test_xn_uppercase_rejected;
      Alcotest.test_case "u-label" `Quick test_ulabel;
      Alcotest.test_case "4-byte utf-8" `Quick test_4byte_utf8;
    ];
    "check_label:length", [
      Alcotest.test_case "empty" `Quick test_empty_label;
      Alcotest.test_case "too long" `Quick test_label_too_long;
    ];
    "check_label:hyphen", [
      Alcotest.test_case "start" `Quick test_hyphen_start;
      Alcotest.test_case "end" `Quick test_hyphen_end;
      Alcotest.test_case "positions 3-4" `Quick test_hyphen_34;
      Alcotest.test_case "internal ok" `Quick test_hyphen_internal_ok;
    ];
    "check_label:combiner", [
      Alcotest.test_case "u-label" `Quick test_initial_combiner_ulabel;
      Alcotest.test_case "a-label" `Quick test_initial_combiner_alabel;
    ];
    "check_label:nfc", [
      Alcotest.test_case "nfc_qc_no" `Quick test_nfc_qc_no;
    ];
    "check_label:disallowed", [
      Alcotest.test_case "soft hyphen" `Quick test_disallowed;
      Alcotest.test_case "line separator" `Quick test_disallowed_separator;
    ];
    "contexto:middle_dot", [
      Alcotest.test_case "valid l·l" `Quick test_middle_dot_valid;
      Alcotest.test_case "wrong neighbors" `Quick test_middle_dot_wrong_neighbors;
      Alcotest.test_case "at start" `Quick test_middle_dot_at_start;
      Alcotest.test_case "at end" `Quick test_middle_dot_at_end;
    ];
    "contexto:greek", [
      Alcotest.test_case "valid" `Quick test_greek_numeral_valid;
      Alcotest.test_case "at end" `Quick test_greek_numeral_at_end;
    ];
    "contexto:hebrew", [
      Alcotest.test_case "geresh valid" `Quick test_hebrew_geresh_valid;
      Alcotest.test_case "gershayim valid" `Quick test_hebrew_gershayim_valid;
      Alcotest.test_case "geresh at start" `Quick test_hebrew_geresh_at_start;
    ];
    "contexto:katakana", [
      Alcotest.test_case "with katakana" `Quick test_katakana_dot_with_katakana;
      Alcotest.test_case "with hiragana" `Quick test_katakana_dot_with_hiragana;
      Alcotest.test_case "with han" `Quick test_katakana_dot_with_han;
      Alcotest.test_case "alone" `Quick test_katakana_dot_alone;
    ];
    "contexto:arabic", [
      Alcotest.test_case "indic valid" `Quick test_arabic_indic_valid;
      Alcotest.test_case "indic mixed" `Quick test_arabic_indic_mixed;
      Alcotest.test_case "extended valid" `Quick test_ext_arabic_valid;
      Alcotest.test_case "extended mixed" `Quick test_ext_arabic_mixed;
    ];
    "contextj", [
      Alcotest.test_case "zwnj virama" `Quick test_zwnj_after_virama;
      Alcotest.test_case "zwj virama" `Quick test_zwj_after_virama;
      Alcotest.test_case "zwnj joining" `Quick test_zwnj_joining_context;
      Alcotest.test_case "zwnj joining+T" `Quick test_zwnj_joining_with_transparent;
      Alcotest.test_case "zwj no virama" `Quick test_zwj_no_virama;
      Alcotest.test_case "zwnj no context" `Quick test_zwnj_no_context;
      Alcotest.test_case "zwnj at start" `Quick test_zwnj_at_start;
      Alcotest.test_case "zwnj no right" `Quick test_zwnj_no_right;
    ];
    "bidi", [
      Alcotest.test_case "rtl valid" `Quick test_bidi_rtl_valid;
      Alcotest.test_case "rtl invalid end" `Quick test_bidi_rtl_invalid_end;
      Alcotest.test_case "rtl nsm end" `Quick test_bidi_rtl_nsm_end;
      Alcotest.test_case "first not r/al/l" `Quick test_bidi_first_not_ral;
      Alcotest.test_case "ltr with rtl" `Quick test_bidi_ltr_with_rtl;
      Alcotest.test_case "single rtl" `Quick test_bidi_single_rtl;
    ];
    "xn--", [
      Alcotest.test_case "invalid punycode" `Quick test_xn_invalid_punycode;
      Alcotest.test_case "disallowed after decode" `Quick test_xn_disallowed;
      Alcotest.test_case "non-canonical round-trip" `Quick test_xn_noncanonical;
    ];
    "boundary", [
      Alcotest.test_case "max label 63" `Quick test_max_label;
      Alcotest.test_case "single char" `Quick test_single_char;
    ];
    "is_valid_hostname", [
      Alcotest.test_case "valid" `Quick test_hostname_valid;
      Alcotest.test_case "empty" `Quick test_hostname_empty;
      Alcotest.test_case "too long" `Quick test_hostname_too_long;
      Alcotest.test_case "max length" `Quick test_hostname_max;
      Alcotest.test_case "trailing dot" `Quick test_hostname_trailing_dot;
      Alcotest.test_case "leading dot" `Quick test_hostname_leading_dot;
      Alcotest.test_case "double dot" `Quick test_hostname_double_dot;
      Alcotest.test_case "invalid label" `Quick test_hostname_invalid_label;
      Alcotest.test_case "single label" `Quick test_hostname_single_label;
      Alcotest.test_case "xn-- labels" `Quick test_hostname_xn;
      Alcotest.test_case "bidi digit after rtl" `Quick test_hostname_bidi_digit_after_rtl;
    ];
  ]
