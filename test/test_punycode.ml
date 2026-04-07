(* Behavioral tests for Punycode (RFC 3492). *)

let check_ok s expected_cps =
  match Idna.Punycode.decode s with
  | Ok cps -> Alcotest.(check (list int)) s expected_cps cps
  | Error e -> Alcotest.fail (Printf.sprintf "decode %S: %s" s e)

let check_err s =
  match Idna.Punycode.decode s with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error for %S" s)

(* ══════════════════════════════════════════════════════════════ *)
(* 1. RFC 3492 Section 7.1 test vectors                          *)
(* ══════════════════════════════════════════════════════════════ *)

let test_rfc_arabic () =
  check_ok "egbpdaj6bu4bxfgehfvwxn"
    [0x0644;0x064A;0x0647;0x0645;0x0627;0x0628;0x062A;0x0643;
     0x0644;0x0645;0x0648;0x0634;0x0639;0x0631;0x0628;0x064A;0x061F]

let test_rfc_chinese () =
  check_ok "ihqwcrb4cv8a8dqg056pqjye"
    [0x4ED6;0x4EEC;0x4E3A;0x4EC0;0x4E48;0x4E0D;0x8BF4;0x4E2D;0x6587]

let test_rfc_czech () =
  check_ok "Proprostnemluvesky-uyb24dma41a"
    [0x50;0x72;0x6F;0x10D;0x70;0x72;0x6F;0x73;0x74;0x11B;0x6E;0x65;
     0x6D;0x6C;0x75;0x76;0xED;0x10D;0x65;0x73;0x6B;0x79]

let test_rfc_japanese () =
  check_ok "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"
    [0x306A;0x305C;0x307F;0x3093;0x306A;0x65E5;0x672C;0x8A9E;
     0x3092;0x8A71;0x3057;0x3066;0x304F;0x308C;0x306A;0x3044;0x306E;0x304B]

(* mañana *)
let test_manana () =
  check_ok "maana-pta"
    [0x6D;0x61;0xF1;0x61;0x6E;0x61]

(* ══════════════════════════════════════════════════════════════ *)
(* 2. Negative: invalid Punycode                                 *)
(* ══════════════════════════════════════════════════════════════ *)

let test_empty () = check_err ""

(* Invalid digit character *)
let test_invalid_char () = check_err "!"

(* Truncated: inner loop runs out of digits *)
let test_truncated () = check_err "b"

(* ══════════════════════════════════════════════════════════════ *)
(* 3. Boundary                                                   *)
(* ══════════════════════════════════════════════════════════════ *)

(* Basic-only: trailing '-' separator, encoded part is empty *)
let test_basic_only () =
  check_ok "abc-" [0x61; 0x62; 0x63]

(* No separator: entire input is encoded part, decodes to U+0080 *)
let test_no_separator () =
  check_ok "a" [0x80]

(* Single non-basic codepoint *)
let test_single_nonbasic () =
  check_ok "tda" [0xFC]  (* ü *)

(* ══════════════════════════════════════════════════════════════ *)
(* 4. decode_digit: all three character ranges                   *)
(* ══════════════════════════════════════════════════════════════ *)

(* Digits 0-9 map to values 26-35 *)
let test_digit_09 () =
  (* "0" as digit = value 26. k=36, t=1, 26>=1 → need more → truncated → error *)
  check_err "0";
  (* RFC arabic vector uses only a-z; let's verify digits work in a real decode *)
  (* "0a" → digit('0')=26, k=36, t=1, 26>=1 → w=35, k=72;
     digit('a')=0, k=72, t=1(bias-adjusted), 0<1 → i=26*1+0*35=26
     n=128+26=154=0x9A, insert at pos 0 *)
  check_ok "0a" [0x9A]

let test_uppercase () =
  check_ok "MAANA-PTA"
    [0x4D;0x41;0xF1;0x41;0x4E;0x41]

(* ══════════════════════════════════════════════════════════════ *)
(* 5. Idempotency                                                *)
(* ══════════════════════════════════════════════════════════════ *)

let test_idempotent () =
  let input = "maana-pta" in
  let r1 = Idna.Punycode.decode input in
  let r2 = Idna.Punycode.decode input in
  match r1, r2 with
  | Ok a, Ok b -> Alcotest.(check (list int)) "same" a b
  | _ -> Alcotest.fail "both should succeed"

let () =
  Alcotest.run "punycode" [
    "rfc3492", [
      Alcotest.test_case "arabic" `Quick test_rfc_arabic;
      Alcotest.test_case "chinese" `Quick test_rfc_chinese;
      Alcotest.test_case "czech" `Quick test_rfc_czech;
      Alcotest.test_case "japanese" `Quick test_rfc_japanese;
      Alcotest.test_case "mañana" `Quick test_manana;
    ];
    "negative", [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "invalid char" `Quick test_invalid_char;
      Alcotest.test_case "truncated" `Quick test_truncated;
    ];
    "boundary", [
      Alcotest.test_case "basic only" `Quick test_basic_only;
      Alcotest.test_case "no separator" `Quick test_no_separator;
      Alcotest.test_case "single nonbasic" `Quick test_single_nonbasic;
    ];
    "decode_digit", [
      Alcotest.test_case "digits 0-9" `Quick test_digit_09;
      Alcotest.test_case "uppercase A-Z" `Quick test_uppercase;
    ];
    "idempotency", [
      Alcotest.test_case "same twice" `Quick test_idempotent;
    ];
  ]
