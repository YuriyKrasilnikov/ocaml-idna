(* Behavioral tests for IDNA2008 hostname validation. *)

let valid s =
  Alcotest.(check bool) s true (Idna.is_valid_hostname s)
let invalid s =
  Alcotest.(check bool) s false (Idna.is_valid_hostname s)

(* ══════════════════════════════════════════════════════════════ *)
(* 1. Happy path: valid hostnames                                *)
(* ══════════════════════════════════════════════════════════════ *)

let test_simple_ascii () =
  valid "example.com";
  valid "foo.bar.baz";
  valid "a"

let test_xn_label () =
  valid "xn--maana-pta.com"   (* mañana.com *)

(* ══════════════════════════════════════════════════════════════ *)
(* 2. Negative: invalid hostnames                                *)
(* ══════════════════════════════════════════════════════════════ *)

let test_empty () = invalid ""
let test_single_dot () = invalid "."
let test_trailing_dot () = invalid "example."
let test_leading_dot () = invalid ".example"
let test_double_dot () = invalid "example..com"
let test_hyphen_start () = invalid "-example.com"
let test_hyphen_end () = invalid "example-.com"
let test_underscore () = invalid "host_name"

(* ══════════════════════════════════════════════════════════════ *)
(* 3. Boundary                                                   *)
(* ══════════════════════════════════════════════════════════════ *)

let test_max_label_63 () =
  valid (String.make 63 'a')
let test_label_too_long () =
  invalid (String.make 64 'a')

(* ══════════════════════════════════════════════════════════════ *)
(* 4. IDNA2008: -- in positions 3-4 forbidden                    *)
(* ══════════════════════════════════════════════════════════════ *)

let test_double_hyphen_34 () =
  invalid "ab--cd.com"

(* ══════════════════════════════════════════════════════════════ *)
(* 5. Punycode: invalid xn-- labels                              *)
(* ══════════════════════════════════════════════════════════════ *)

let test_invalid_punycode () =
  invalid "xn--X"

(* ══════════════════════════════════════════════════════════════ *)
(* 6. IDNA2008: label must not start with combining mark         *)
(* ══════════════════════════════════════════════════════════════ *)

let test_initial_combiner () =
  (* U+0300 = Combining Grave Accent, xn--ced = U+0300 alone *)
  invalid "xn--ced"

let test_contexto_katakana_dot_alone () =
  (* U+30FB = Katakana Middle Dot alone — needs Han/Hiragana/Katakana context *)
  invalid "xn--vek"

(* ══════════════════════════════════════════════════════════════ *)
(* 7. IDNA2008: DISALLOWED codepoints                            *)
(* ══════════════════════════════════════════════════════════════ *)

let test_disallowed_hangul_tone () =
  (* U+302E = Hangul Single Dot Tone Mark, xn--chb89f contains it *)
  invalid "xn--chb89f"

(* ══════════════════════════════════════════════════════════════ *)
(* Runner                                                        *)
(* ══════════════════════════════════════════════════════════════ *)

let () =
  Alcotest.run "idna" [
    "valid", [
      Alcotest.test_case "ascii" `Quick test_simple_ascii;
      Alcotest.test_case "xn-- label" `Quick test_xn_label;
    ];
    "invalid", [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "single dot" `Quick test_single_dot;
      Alcotest.test_case "trailing dot" `Quick test_trailing_dot;
      Alcotest.test_case "leading dot" `Quick test_leading_dot;
      Alcotest.test_case "double dot" `Quick test_double_dot;
      Alcotest.test_case "hyphen start" `Quick test_hyphen_start;
      Alcotest.test_case "hyphen end" `Quick test_hyphen_end;
      Alcotest.test_case "underscore" `Quick test_underscore;
    ];
    "boundary", [
      Alcotest.test_case "63 chars" `Quick test_max_label_63;
      Alcotest.test_case "64 chars" `Quick test_label_too_long;
    ];
    "idna2008", [
      Alcotest.test_case "-- at 3-4" `Quick test_double_hyphen_34;
      Alcotest.test_case "invalid punycode" `Quick test_invalid_punycode;
      Alcotest.test_case "initial combiner" `Quick test_initial_combiner;
      Alcotest.test_case "CONTEXTO katakana dot" `Quick test_contexto_katakana_dot_alone;
      Alcotest.test_case "disallowed codepoint" `Quick test_disallowed_hangul_tone;
    ];
  ]
