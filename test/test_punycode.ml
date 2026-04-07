(* Behavioral tests for Punycode (RFC 3492). *)

(* Helper: int list to UTF-8 string *)
let to_utf8 cps =
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

let decode_ok s expected_cps =
  match Idna.Punycode.decode s with
  | Ok cps -> Alcotest.(check string) s (to_utf8 expected_cps) (to_utf8 cps)
  | Error e -> Alcotest.fail (Printf.sprintf "decode %s: %s" s e)

let decode_err s =
  match Idna.Punycode.decode s with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error for %s" s)

(* ══════════════════════════════════════════════════════════════ *)
(* 1. Happy path: RFC 3492 Section 7.1 test vectors             *)
(* ══════════════════════════════════════════════════════════════ *)

let test_rfc_arabic () =
  decode_ok "egbpdaj6bu4bxfgehfvwxn"
    [0x0644;0x064A;0x0647;0x0645;0x0627;0x0628;0x062A;0x0643;
     0x0644;0x0645;0x0648;0x0634;0x0639;0x0631;0x0628;0x064A;0x061F]

let test_rfc_chinese_simplified () =
  decode_ok "ihqwcrb4cv8a8dqg056pqjye"
    [0x4ED6;0x4EEC;0x4E3A;0x4EC0;0x4E48;0x4E0D;0x8BF4;0x4E2D;0x6587]

let test_rfc_czech () =
  decode_ok "Proprostnemluvesky-uyb24dma41a"
    [0x50;0x72;0x6F;0x10D;0x70;0x72;0x6F;0x73;0x74;0x11B;0x6E;0x65;
     0x6D;0x6C;0x75;0x76;0xED;0x10D;0x65;0x73;0x6B;0x79]

let test_rfc_japanese () =
  decode_ok "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"
    [0x306A;0x305C;0x307F;0x3093;0x306A;0x65E5;0x672C;0x8A9E;
     0x3092;0x8A71;0x3057;0x3066;0x304F;0x308C;0x306A;0x3044;0x306E;0x304B]

(* ══════════════════════════════════════════════════════════════ *)
(* 2. Negative: invalid punycode                                 *)
(* ══════════════════════════════════════════════════════════════ *)

let test_invalid_empty () = decode_err ""

(* ══════════════════════════════════════════════════════════════ *)
(* 3. Boundary: basic-only label (no encoded part)               *)
(* ══════════════════════════════════════════════════════════════ *)

let test_basic_only () =
  decode_ok "abc-" [0x61; 0x62; 0x63]

(* ══════════════════════════════════════════════════════════════ *)
(* 4. Real domain: mañana → maana-pta                            *)
(* ══════════════════════════════════════════════════════════════ *)

let test_manana () =
  decode_ok "maana-pta"
    [0x6D;0x61;0xF1;0x61;0x6E;0x61]

(* ══════════════════════════════════════════════════════════════ *)
(* 5. Idempotency: decode same input twice → same result         *)
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
      Alcotest.test_case "chinese" `Quick test_rfc_chinese_simplified;
      Alcotest.test_case "czech" `Quick test_rfc_czech;
      Alcotest.test_case "japanese" `Quick test_rfc_japanese;
    ];
    "negative", [
      Alcotest.test_case "empty" `Quick test_invalid_empty;
    ];
    "boundary", [
      Alcotest.test_case "basic only" `Quick test_basic_only;
    ];
    "real", [
      Alcotest.test_case "mañana" `Quick test_manana;
    ];
    "idempotency", [
      Alcotest.test_case "same twice" `Quick test_idempotent;
    ];
  ]
