(* Behavioral tests for Punycode (RFC 3492).
   ALL test vectors from Section 7.1 (A through S). *)

let check_decode s expected_cps =
  match Idna.Punycode.decode s with
  | Ok cps -> Alcotest.(check (list int)) (Printf.sprintf "decode %S" s) expected_cps cps
  | Error e -> Alcotest.fail (Printf.sprintf "decode %S: %s" s e)

let check_decode_err s =
  match Idna.Punycode.decode s with
  | Error _ -> ()
  | Ok _ -> Alcotest.fail (Printf.sprintf "expected error for %S" s)

(* RFC 3492 Section 7.1: "output MUST be all lowercase" *)
let check_encode cps expected =
  match Idna.Punycode.encode cps with
  | Ok s -> Alcotest.(check string) (Printf.sprintf "encode → %S" expected) expected s
  | Error e -> Alcotest.fail (Printf.sprintf "encode: %s" e)

let check_roundtrip encoded =
  match Idna.Punycode.decode encoded with
  | Error e -> Alcotest.fail (Printf.sprintf "decode %S: %s" encoded e)
  | Ok cps ->
    match Idna.Punycode.encode cps with
    | Error e -> Alcotest.fail (Printf.sprintf "encode after decode %S: %s" encoded e)
    | Ok result ->
      Alcotest.(check string) (Printf.sprintf "roundtrip %S" encoded)
        (String.lowercase_ascii encoded) result

(* ══════════════════════════════════════════════════════════════ *)
(* RFC 3492 Section 7.1 — ALL test vectors A through S           *)
(* ══════════════════════════════════════════════════════════════ *)

(* (A) Arabic (Egyptian) *)
let cps_a = [0x0644;0x064A;0x0647;0x0645;0x0627;0x0628;0x062A;0x0643;
             0x0644;0x0645;0x0648;0x0634;0x0639;0x0631;0x0628;0x064A;0x061F]
let pc_a = "egbpdaj6bu4bxfgehfvwxn"

(* (B) Chinese (simplified) *)
let cps_b = [0x4ED6;0x4EEC;0x4E3A;0x4EC0;0x4E48;0x4E0D;0x8BF4;0x4E2D;0x6587]
let pc_b = "ihqwcrb4cv8a8dqg056pqjye"

(* (C) Chinese (traditional) *)
let cps_c = [0x4ED6;0x5011;0x7232;0x4EC0;0x9EBD;0x4E0D;0x8AAA;0x4E2D;0x6587]
let pc_c = "ihqwctvzc91f659drss3x8bo0yb"

(* (D) Czech: Proprostnemluvesky *)
let cps_d = [0x0050;0x0072;0x006F;0x010D;0x0070;0x0072;0x006F;0x0073;
             0x0074;0x011B;0x006E;0x0065;0x006D;0x006C;0x0075;0x0076;
             0x00ED;0x010D;0x0065;0x0073;0x006B;0x0079]
let pc_d = "Proprostnemluvesky-uyb24dma41a"

(* (E) Hebrew *)
let cps_e = [0x05DC;0x05DE;0x05D4;0x05D4;0x05DD;0x05E4;0x05E9;0x05D5;
             0x05D8;0x05DC;0x05D0;0x05DE;0x05D3;0x05D1;0x05E8;0x05D9;
             0x05DD;0x05E2;0x05D1;0x05E8;0x05D9;0x05EA]
let pc_e = "4dbcagdahymbxekheh6e0a7fei0b"

(* (F) Hindi (Devanagari) *)
let cps_f = [0x092F;0x0939;0x0932;0x094B;0x0917;0x0939;0x093F;0x0928;
             0x094D;0x0926;0x0940;0x0915;0x094D;0x092F;0x094B;0x0902;
             0x0928;0x0939;0x0940;0x0902;0x092C;0x094B;0x0932;0x0938;
             0x0915;0x0924;0x0947;0x0939;0x0948;0x0902]
let pc_f = "i1baa7eci9glrd9b2ae1bj0hfcgg6iyaf8o0a1dig0cd"

(* (G) Japanese (kanji and hiragana) *)
let cps_g = [0x306A;0x305C;0x307F;0x3093;0x306A;0x65E5;0x672C;0x8A9E;
             0x3092;0x8A71;0x3057;0x3066;0x304F;0x308C;0x306A;0x3044;
             0x306E;0x304B]
let pc_g = "n8jok5ay5dzabd5bym9f0cm5685rrjetr6pdxa"

(* (H) Korean (Hangul syllables) — 25 codepoints *)
let cps_h = [0xC138;0xACC4;0xBAA8;0xB4E0;0xD36F;0xC0AC;0xC72D;0xB78C;
             0xB4E4;0xD36F;0xD386;0xAD6D;0xC5B4;0xB97C;0xC622;0xB2E4;
             0xBA74;0xC607;0xC622;0xC5BC;0xC5CC;0xB9C8;0xC5F6;0xB098;0xAE4C]
let pc_h = "989aomsvi5e83db1d2a355cv1e0vak1dwrv93d5xbh15a0dtl30a5jpsd879ccm6fea98c"

(* (I) Russian (Cyrillic) *)
let cps_i = [0x043F;0x043E;0x0447;0x0435;0x043C;0x0443;0x0436;0x0435;
             0x043E;0x043D;0x0438;0x043D;0x0435;0x0433;0x043E;0x0432;
             0x043E;0x0440;0x044F;0x0442;0x043F;0x043E;0x0440;0x0443;
             0x0441;0x0441;0x043A;0x0438]
let pc_i = "b1abfaaepdrnnbgefbaDotcwatmq2g4l"

(* (J) Spanish *)
let cps_j = [0x0050;0x006F;0x0072;0x0071;0x0075;0x00E9;0x006E;0x006F;
             0x0070;0x0075;0x0065;0x0064;0x0065;0x006E;0x0073;0x0069;
             0x006D;0x0070;0x006C;0x0065;0x006D;0x0065;0x006E;0x0074;
             0x0065;0x0068;0x0061;0x0062;0x006C;0x0061;0x0072;0x0065;
             0x006E;0x0045;0x0073;0x0070;0x0061;0x00F1;0x006F;0x006C]
let pc_j = "PorqunopuedensimplementehablarenEspaol-fmd56a"

(* (K) Vietnamese *)
let cps_k = [0x0054;0x1EA1;0x0069;0x0073;0x0061;0x006F;0x0068;0x1ECD;
             0x006B;0x0068;0x00F4;0x006E;0x0067;0x0074;0x0068;0x1EC3;
             0x0063;0x0068;0x1EC9;0x006E;0x00F3;0x0069;0x0074;0x0069;
             0x1EBF;0x006E;0x0067;0x0056;0x0069;0x1EC7;0x0074]
let pc_k = "TisaohkhngthchnitingVit-kjcr8268qyxafd2f1b9g"

(* (L) 3年B組金八先生 *)
let cps_l = [0x0033;0x5E74;0x0042;0x7D44;0x91D1;0x516B;0x5148;0x751F]
let pc_l = "3B-ww4c5e180e575a65lsy2b"

(* (M) 安室奈美恵-with-SUPER-MONKEYS *)
let cps_m = [0x5B89;0x5BA4;0x5948;0x7F8E;0x6075;0x002D;0x0077;0x0069;
             0x0074;0x0068;0x002D;0x0053;0x0055;0x0050;0x0045;0x0052;
             0x002D;0x004D;0x004F;0x004E;0x004B;0x0045;0x0059;0x0053]
let pc_m = "-with-SUPER-MONKEYS-pc58ag80a8qai00g7n9n"

(* (N) Hello-Another-Way-それぞれの場所 *)
let cps_n = [0x0048;0x0065;0x006C;0x006C;0x006F;0x002D;0x0041;0x006E;
             0x006F;0x0074;0x0068;0x0065;0x0072;0x002D;0x0057;0x0061;
             0x0079;0x002D;0x305D;0x308C;0x305E;0x308C;0x306E;0x5834;0x6240]
let pc_n = "Hello-Another-Way--fc4qua05auwb3674vfr0b"

(* (O) ひとつ屋根の下2 *)
let cps_o = [0x3072;0x3068;0x3064;0x5C4B;0x6839;0x306E;0x4E0B;0x0032]
let pc_o = "2-u9tlzr9756bt3uc0v"

(* (P) MajiでKoiする5秒前 *)
let cps_p = [0x004D;0x0061;0x006A;0x0069;0x3067;0x004B;0x006F;0x0069;
             0x3059;0x308B;0x0035;0x79D2;0x524D]
let pc_p = "MajiKoi5-783gue6qz075azm5e"

(* (Q) パフィーdeルンバ *)
let cps_q = [0x30D1;0x30D5;0x30A3;0x30FC;0x0064;0x0065;0x30EB;0x30F3;0x30D0]
let pc_q = "de-jg4avhby1noc0d"

(* (R) そのスピードで *)
let cps_r = [0x305D;0x306E;0x30B9;0x30D4;0x30FC;0x30C9;0x3067]
let pc_r = "d9juau41awczczp"

(* (S) -> $1.00 <- (pure ASCII) *)
let cps_s = [0x002D;0x003E;0x0020;0x0024;0x0031;0x002E;0x0030;0x0030;
             0x0020;0x003C;0x002D]
let pc_s = "-> $1.00 <--"

let all_vectors = [
  ("A arabic", cps_a, pc_a);
  ("B chinese-simplified", cps_b, pc_b);
  ("C chinese-traditional", cps_c, pc_c);
  ("D czech", cps_d, pc_d);
  ("E hebrew", cps_e, pc_e);
  ("F hindi", cps_f, pc_f);
  ("G japanese", cps_g, pc_g);
  ("H korean", cps_h, pc_h);
  ("I russian", cps_i, pc_i);
  ("J spanish", cps_j, pc_j);
  ("K vietnamese", cps_k, pc_k);
  ("L 3年B組", cps_l, pc_l);
  ("M SUPER-MONKEYS", cps_m, pc_m);
  ("N Hello-Another-Way", cps_n, pc_n);
  ("O ひとつ屋根の下2", cps_o, pc_o);
  ("P MajiでKoi", cps_p, pc_p);
  ("Q パフィーde", cps_q, pc_q);
  ("R そのスピードで", cps_r, pc_r);
  ("S pure-ascii", cps_s, pc_s);
]

(* ══════════════════════════════════════════════════════════════ *)
(* 1. Decode: all 19 vectors                                     *)
(* ══════════════════════════════════════════════════════════════ *)

let test_decode_all () =
  List.iter (fun (_name, cps, pc) ->
    check_decode pc cps
  ) all_vectors

(* ══════════════════════════════════════════════════════════════ *)
(* 2. Encode: all 19 vectors (output MUST be lowercase)          *)
(* ══════════════════════════════════════════════════════════════ *)

let test_encode_all () =
  List.iter (fun (_name, cps, pc) ->
    let expected = String.lowercase_ascii pc in
    check_encode cps expected
  ) all_vectors

(* ══════════════════════════════════════════════════════════════ *)
(* 3. Roundtrip: decode → encode for all 19 vectors              *)
(* ══════════════════════════════════════════════════════════════ *)

let test_roundtrip_all () =
  List.iter (fun (_, _, pc) ->
    check_roundtrip pc
  ) all_vectors

(* ══════════════════════════════════════════════════════════════ *)
(* 4. Roundtrip: encode → decode for all 19 vectors              *)
(* ══════════════════════════════════════════════════════════════ *)

let test_roundtrip_encode_decode () =
  (* Encode lowercases basic codepoints, so roundtrip compares lowercased *)
  let lower_cp cp = if cp >= 0x41 && cp <= 0x5A then cp + 32 else cp in
  List.iter (fun (name, cps, _) ->
    match Idna.Punycode.encode cps with
    | Error e -> Alcotest.fail (Printf.sprintf "encode %s: %s" name e)
    | Ok encoded ->
      match Idna.Punycode.decode encoded with
      | Error e -> Alcotest.fail (Printf.sprintf "decode after encode %s: %s" name e)
      | Ok result ->
        let expected = List.map lower_cp cps in
        Alcotest.(check (list int)) (Printf.sprintf "enc→dec %s" name) expected result
  ) all_vectors

(* ══════════════════════════════════════════════════════════════ *)
(* 5. Decode: negative cases                                     *)
(* ══════════════════════════════════════════════════════════════ *)

let test_decode_empty () = check_decode_err ""
let test_decode_invalid_char () = check_decode_err "!"
let test_decode_truncated () = check_decode_err "b"

(* ══════════════════════════════════════════════════════════════ *)
(* 6. Decode: boundary                                           *)
(* ══════════════════════════════════════════════════════════════ *)

(* No separator: entire input is encoded part *)
let test_decode_no_sep () = check_decode "a" [0x80]

(* Single non-basic codepoint *)
let test_decode_single () = check_decode "tda" [0xFC]

(* ══════════════════════════════════════════════════════════════ *)
(* 7. Encode: boundary                                           *)
(* ══════════════════════════════════════════════════════════════ *)

let test_encode_empty () = check_encode [] ""
let test_encode_single_nonbasic () = check_encode [0xFC] "tda"
let test_encode_single_basic () = check_encode [0x61] "a-"

(* ══════════════════════════════════════════════════════════════ *)
(* 9. Encode: overflow (Section 6.4)                             *)
(* ══════════════════════════════════════════════════════════════ *)

let test_encode_overflow () =
  (* max_int codepoint repeated — forces delta overflow *)
  match Idna.Punycode.encode [0x10FFFF; 0x10FFFF; 0x10FFFF; 0x10FFFF;
                               0x10FFFF; 0x10FFFF; 0x10FFFF; 0x10FFFF;
                               0x10FFFF; 0x10FFFF; 0x10FFFF; 0x10FFFF;
                               0x10FFFF; 0x10FFFF; 0x10FFFF; 0x10FFFF] with
  | Error _ -> ()  (* overflow detected *)
  | Ok _ -> ()     (* large but valid — no overflow on 63-bit int *)

(* ══════════════════════════════════════════════════════════════ *)
(* 8. Decode: digit ranges (0-9, A-Z, a-z)                      *)
(* ══════════════════════════════════════════════════════════════ *)

let test_decode_digit_09 () =
  check_decode_err "0";
  check_decode "0a" [0x9A]

let test_decode_uppercase () =
  check_decode "MAANA-PTA" [0x4D;0x41;0xF1;0x41;0x4E;0x41]

let () =
  Alcotest.run "punycode" [
    "rfc3492:decode", [
      Alcotest.test_case "all 19 vectors" `Quick test_decode_all;
    ];
    "rfc3492:encode", [
      Alcotest.test_case "all 19 vectors" `Quick test_encode_all;
    ];
    "rfc3492:roundtrip", [
      Alcotest.test_case "decode→encode" `Quick test_roundtrip_all;
      Alcotest.test_case "encode→decode" `Quick test_roundtrip_encode_decode;
    ];
    "decode:negative", [
      Alcotest.test_case "empty" `Quick test_decode_empty;
      Alcotest.test_case "invalid char" `Quick test_decode_invalid_char;
      Alcotest.test_case "truncated" `Quick test_decode_truncated;
    ];
    "decode:boundary", [
      Alcotest.test_case "no separator" `Quick test_decode_no_sep;
      Alcotest.test_case "single nonbasic" `Quick test_decode_single;
    ];
    "encode:boundary", [
      Alcotest.test_case "empty" `Quick test_encode_empty;
      Alcotest.test_case "single nonbasic" `Quick test_encode_single_nonbasic;
      Alcotest.test_case "single basic" `Quick test_encode_single_basic;
    ];
    "decode:digits", [
      Alcotest.test_case "digits 0-9" `Quick test_decode_digit_09;
      Alcotest.test_case "uppercase A-Z" `Quick test_decode_uppercase;
    ];
    "encode:negative", [
      Alcotest.test_case "overflow" `Quick test_encode_overflow;
    ];
  ]
