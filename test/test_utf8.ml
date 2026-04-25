(** Unit tests for Utf8 module. Tests edge cases and error paths. *)

let check_ok name input expected =
  match Idna.Utf8.to_cps input with
  | Ok cps when cps = expected -> Alcotest.(check pass) name () ()
  | Ok cps ->
    Alcotest.fail
      (Printf.sprintf "%s: got %s, expected %s" name
         (String.concat "," (List.map string_of_int cps))
         (String.concat "," (List.map string_of_int expected)))
  | Error e -> Alcotest.fail (Printf.sprintf "%s: Error %s" name e)

let check_err name input =
  match Idna.Utf8.to_cps input with
  | Error _ -> ()
  | Ok cps ->
    Alcotest.fail
      (Printf.sprintf "%s: expected Error, got Ok %s"
         name (String.concat "," (List.map string_of_int cps)))

(* ── Valid sequences ─────────────────────────────────────────── *)

let test_empty () = check_ok "empty" "" []
let test_ascii () = check_ok "ascii" "abc" [0x61; 0x62; 0x63]
let test_2byte () = check_ok "U+00E9 é" "\xC3\xA9" [0xE9]
let test_3byte () = check_ok "U+4E00 一" "\xE4\xB8\x80" [0x4E00]
let test_4byte () = check_ok "U+1F600 😀" "\xF0\x9F\x98\x80" [0x1F600]
let test_mixed () =
  check_ok "mixed" "a\xC3\xA9\xE4\xB8\x80\xF0\x9F\x98\x80"
    [0x61; 0xE9; 0x4E00; 0x1F600]

(* ── Truncated sequences ─────────────────────────────────────── *)

let test_truncated_2byte () = check_err "truncated 2-byte" "\xC3"
let test_truncated_3byte_1 () = check_err "truncated 3-byte missing 2" "\xE4"
let test_truncated_3byte_2 () = check_err "truncated 3-byte missing 1" "\xE4\xB8"
let test_truncated_4byte_1 () = check_err "truncated 4-byte missing 3" "\xF0"
let test_truncated_4byte_2 () = check_err "truncated 4-byte missing 2" "\xF0\x9F"
let test_truncated_4byte_3 () = check_err "truncated 4-byte missing 1" "\xF0\x9F\x98"

(* ── Invalid lead bytes ──────────────────────────────────────── *)

let test_lead_byte_80 () = check_err "lead byte 0x80" "\x80"
let test_lead_byte_BF () = check_err "lead byte 0xBF" "\xBF"
let test_lead_byte_F5 () = check_err "lead byte 0xF5" "\xF5\x80\x80\x80"
let test_lead_byte_FF () = check_err "lead byte 0xFF" "\xFF"

(* ── Overlong encodings ──────────────────────────────────────── *)

(* U+0000 encoded as 2 bytes (should be 1) *)
let test_overlong_null_2byte () = check_err "overlong U+0000 as 2-byte" "\xC0\x80"
(* U+0041 'A' encoded as 2 bytes (should be 1) *)
let test_overlong_A_2byte () = check_err "overlong 'A' as 2-byte" "\xC1\x81"
(* U+007F encoded as 3 bytes *)
let test_overlong_7F_3byte () = check_err "overlong U+007F as 3-byte" "\xE0\x81\xBF"
(* U+0800 boundary — 3 bytes valid, 2 bytes would be overlong but impossible *)
let test_boundary_0800 () =
  check_ok "U+0800 as 3-byte" "\xE0\xA0\x80" [0x0800]

(* ── Surrogate codepoints ────────────────────────────────────── *)

(* U+D800 encoded in UTF-8 as \xED\xA0\x80 — must reject *)
let test_surrogate_d800 () = check_err "surrogate U+D800" "\xED\xA0\x80"
let test_surrogate_dfff () = check_err "surrogate U+DFFF" "\xED\xBF\xBF"

(* ── Out of range ────────────────────────────────────────────── *)

(* U+110000 — beyond Unicode *)
let test_above_max () = check_err "U+110000" "\xF4\x90\x80\x80"
(* Lead byte 0xF4 with continuation that would give > U+10FFFF *)
let test_above_max_2 () = check_err "U+1FFFFF" "\xF7\xBF\xBF\xBF"

(* ── Invalid continuation bytes ──────────────────────────────── *)

(* 0xC3 followed by non-continuation (0xC0 is lead byte) *)
let test_invalid_continuation () = check_err "invalid cont after C3" "\xC3\xC0"
let test_invalid_continuation_3byte () = check_err "invalid cont 3-byte" "\xE4\x00\x80"

(* ── Boundary values ─────────────────────────────────────────── *)

let test_max_valid () = check_ok "U+10FFFF" "\xF4\x8F\xBF\xBF" [0x10FFFF]
let test_boundary_7f () = check_ok "U+007F" "\x7F" [0x7F]
let test_boundary_80 () = check_ok "U+0080" "\xC2\x80" [0x80]
let test_boundary_ffff () = check_ok "U+FFFF" "\xEF\xBF\xBF" [0xFFFF]
let test_boundary_10000 () = check_ok "U+10000" "\xF0\x90\x80\x80" [0x10000]

(* ── Round-trip: of_cps then to_cps ──────────────────────────── *)

let test_roundtrip () =
  let cps = [0x61; 0xE9; 0x4E00; 0x1F600] in
  let s = Idna.Utf8.of_cps cps in
  match Idna.Utf8.to_cps s with
  | Ok cps' when cps' = cps -> ()
  | Ok cps' ->
    Alcotest.fail
      (Printf.sprintf "roundtrip: %s"
         (String.concat "," (List.map string_of_int cps')))
  | Error e -> Alcotest.fail e

let () =
  Alcotest.run "utf8" [
    "valid", [
      Alcotest.test_case "empty" `Quick test_empty;
      Alcotest.test_case "ascii" `Quick test_ascii;
      Alcotest.test_case "2-byte" `Quick test_2byte;
      Alcotest.test_case "3-byte" `Quick test_3byte;
      Alcotest.test_case "4-byte" `Quick test_4byte;
      Alcotest.test_case "mixed" `Quick test_mixed;
    ];
    "truncated", [
      Alcotest.test_case "2-byte" `Quick test_truncated_2byte;
      Alcotest.test_case "3-byte 1" `Quick test_truncated_3byte_1;
      Alcotest.test_case "3-byte 2" `Quick test_truncated_3byte_2;
      Alcotest.test_case "4-byte 1" `Quick test_truncated_4byte_1;
      Alcotest.test_case "4-byte 2" `Quick test_truncated_4byte_2;
      Alcotest.test_case "4-byte 3" `Quick test_truncated_4byte_3;
    ];
    "invalid lead", [
      Alcotest.test_case "0x80" `Quick test_lead_byte_80;
      Alcotest.test_case "0xBF" `Quick test_lead_byte_BF;
      Alcotest.test_case "0xF5" `Quick test_lead_byte_F5;
      Alcotest.test_case "0xFF" `Quick test_lead_byte_FF;
    ];
    "overlong", [
      Alcotest.test_case "U+0000 as 2-byte" `Quick test_overlong_null_2byte;
      Alcotest.test_case "A as 2-byte" `Quick test_overlong_A_2byte;
      Alcotest.test_case "U+007F as 3-byte" `Quick test_overlong_7F_3byte;
      Alcotest.test_case "U+0800 boundary valid" `Quick test_boundary_0800;
    ];
    "surrogate", [
      Alcotest.test_case "U+D800" `Quick test_surrogate_d800;
      Alcotest.test_case "U+DFFF" `Quick test_surrogate_dfff;
    ];
    "out of range", [
      Alcotest.test_case "U+110000" `Quick test_above_max;
      Alcotest.test_case "U+1FFFFF" `Quick test_above_max_2;
    ];
    "invalid continuation", [
      Alcotest.test_case "after C3" `Quick test_invalid_continuation;
      Alcotest.test_case "3-byte" `Quick test_invalid_continuation_3byte;
    ];
    "boundary", [
      Alcotest.test_case "U+10FFFF" `Quick test_max_valid;
      Alcotest.test_case "U+007F" `Quick test_boundary_7f;
      Alcotest.test_case "U+0080" `Quick test_boundary_80;
      Alcotest.test_case "U+FFFF" `Quick test_boundary_ffff;
      Alcotest.test_case "U+10000" `Quick test_boundary_10000;
    ];
    "roundtrip", [
      Alcotest.test_case "mixed codepoints" `Quick test_roundtrip;
    ];
  ]
