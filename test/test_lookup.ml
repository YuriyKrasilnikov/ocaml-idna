(* Behavioral tests for strict IDNA2008 lookup processing. *)

let check_unicode_ok ?(flags = Idna.Lookup.default_flags) input expected =
  match Idna.Lookup.to_unicode ~flags input with
  | Ok actual -> Alcotest.(check string) input expected actual
  | Error msg ->
    Alcotest.fail
      (Printf.sprintf "Lookup.to_unicode %S: expected Ok %S, got Error %S"
         input expected msg)

let check_ascii_ok ?(flags = Idna.Lookup.default_flags) input expected =
  match Idna.Lookup.to_ascii ~flags input with
  | Ok actual -> Alcotest.(check string) input expected actual
  | Error msg ->
    Alcotest.fail
      (Printf.sprintf "Lookup.to_ascii %S: expected Ok %S, got Error %S"
         input expected msg)

let check_ascii_err ?(flags = Idna.Lookup.default_flags) input =
  match Idna.Lookup.to_ascii ~flags input with
  | Error _ -> ()
  | Ok actual ->
    Alcotest.fail
      (Printf.sprintf "Lookup.to_ascii %S: expected Error, got Ok %S"
         input actual)

let test_apparent_alabel_lowercased () =
  check_ascii_ok "XN--MAANA-PTA" "xn--maana-pta";
  check_unicode_ok "XN--MAANA-PTA" "mañana"

let test_ascii_nr_ldh_lowercased () =
  check_ascii_ok "Example.COM" "example.com";
  check_unicode_ok "Example.COM" "example.com"

let test_contexto_relaxed_for_lookup () =
  (* Rule exists for U+00B7, so lookup may accept without enforcing it. *)
  check_unicode_ok "a\xc2\xb7a" "a\xc2\xb7a";
  match Idna.Registration.check_label "a\xc2\xb7a" with
  | Error _ -> ()
  | Ok () -> Alcotest.fail "Registration.check_label unexpectedly accepted a·a"

let test_disallowed_rejected_for_lookup () =
  check_ascii_err "a\xe2\x80\xa8"

let test_contextj_enforced_for_lookup () =
  check_ascii_err "a\xe2\x80\x8ca"

let test_bidi_default_rejects_digit_label_after_rtl () =
  check_ascii_err "\xd7\x90\xd7\x91.1com"

let test_bidi_can_be_disabled () =
  let flags = { Idna.Lookup.check_bidi = false } in
  match Idna.Lookup.to_ascii ~flags "\xd7\x90\xd7\x91.1com" with
  | Ok _ -> ()
  | Error msg ->
    Alcotest.fail
      (Printf.sprintf "Lookup.to_ascii with bidi disabled: got Error %S" msg)

let test_dns_length_not_prechecked_for_long_label () =
  let label = String.make 64 'a' in
  check_ascii_ok label label;
  check_unicode_ok label label

let test_dns_length_not_prechecked_for_long_domain () =
  let domain = String.concat "." [
    String.make 63 'a';
    String.make 63 'b';
    String.make 63 'c';
    String.make 63 'd';
  ] in
  check_ascii_ok domain domain;
  check_unicode_ok domain domain

let () =
  Alcotest.run "lookup" [
    "lookup", [
      Alcotest.test_case "apparent a-label lowercased" `Quick test_apparent_alabel_lowercased;
      Alcotest.test_case "ascii nr-ldh lowercased" `Quick test_ascii_nr_ldh_lowercased;
      Alcotest.test_case "disallowed rejected" `Quick test_disallowed_rejected_for_lookup;
      Alcotest.test_case "contextj enforced" `Quick test_contextj_enforced_for_lookup;
      Alcotest.test_case "contexto relaxed" `Quick test_contexto_relaxed_for_lookup;
      Alcotest.test_case "bidi default rejects digit label after rtl" `Quick test_bidi_default_rejects_digit_label_after_rtl;
      Alcotest.test_case "bidi can be disabled" `Quick test_bidi_can_be_disabled;
      Alcotest.test_case "dns length not prechecked for long label" `Quick test_dns_length_not_prechecked_for_long_label;
      Alcotest.test_case "dns length not prechecked for long domain" `Quick test_dns_length_not_prechecked_for_long_domain;
    ];
  ]
