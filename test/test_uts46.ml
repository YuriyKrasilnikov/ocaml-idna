(** UTS #46 conformance against IdnaTestV2.txt.

    Format per entry:
      source ; toUnicode ; toUnicodeStatus ; toAsciiN ; toAsciiNStatus ; toAsciiT ; toAsciiTStatus

    Blank toUnicode = same as source.
    Blank toAsciiN = same as toUnicode.
    Blank status = no errors. Explicit [] = no errors.
    Literal "" = empty string. *)

let vectors_path = "tools/ucd-16.0.0/IdnaTestV2.txt"

let vectors =
  Test_helper.require_files [vectors_path];
  Test_helper.load_idna_test_vectors vectors_path

let label_of src =
  let n = String.length src in
  if n <= 40 then String.escaped src
  else String.escaped (String.sub src 0 40) ^ "…"

let check_to_unicode_case ?(flags = Idna.Uts46.default_flags) source expected_value expected_error =
  let r = Idna.Uts46.to_unicode ~flags source in
  Alcotest.(check bool)
    (Printf.sprintf "toUnicode(%s) errored" (label_of source))
    expected_error r.errored;
  Alcotest.(check string)
    (Printf.sprintf "toUnicode(%s) value" (label_of source))
    expected_value r.value

let check_to_ascii_ok ?(flags = Idna.Uts46.default_flags) source expected_value =
  match Idna.Uts46.to_ascii ~flags source with
  | Ok value ->
    Alcotest.(check string)
      (Printf.sprintf "toAscii(%s) value" (label_of source))
      expected_value value
  | Error msg ->
    Alcotest.fail (Printf.sprintf "toAscii(%s): expected Ok, got Error %S" (label_of source) msg)

let check_to_ascii_error ?(flags = Idna.Uts46.default_flags) source =
  match Idna.Uts46.to_ascii ~flags source with
  | Ok value ->
    Alcotest.fail (Printf.sprintf "toAscii(%s): expected Error, got Ok %S" (label_of source) value)
  | Error _ -> ()

let test_regression_invalid_utf8_ascii_lowercase () =
  check_to_unicode_case "A\xed\xa4\x80Z" "a\xed\xa4\x80z" true

let test_regression_xn_empty () =
  check_to_unicode_case "xn--" "" true

let test_regression_xn_ascii_hyphen () =
  check_to_unicode_case "xn--ASCII-" "ascii" true

let test_regression_xn_unicode_hyphen_domain () =
  check_to_unicode_case "xn--unicode-.org" "unicode.org" true

let test_regression_root_label_verify_disabled () =
  let flags = { Idna.Uts46.default_flags with verify_dns_length = false } in
  check_to_unicode_case ~flags "example." "example." false;
  check_to_ascii_ok ~flags "example." "example."

let test_regression_root_label_verify_enabled () =
  check_to_unicode_case "example." "example." false;
  check_to_ascii_error "example."

let test_regression_reserved_xn_when_check_hyphens_disabled () =
  let flags = { Idna.Uts46.default_flags with check_hyphens = false } in
  check_to_ascii_error ~flags "xn--maana-pta"

let test_to_unicode () =
  List.iter (fun v ->
    let source = v.Test_helper.source in
    let expected_err = v.Test_helper.to_unicode_err in
    let expected_value = v.Test_helper.to_unicode in
    let r = Idna.Uts46.to_unicode source in
    Alcotest.(check bool)
      (Printf.sprintf "toUnicode(%s) errored" (label_of source))
      expected_err r.errored;
    Alcotest.(check string)
      (Printf.sprintf "toUnicode(%s) value" (label_of source))
      expected_value r.value
  ) vectors

let test_to_ascii_n () =
  List.iter (fun v ->
    let source = v.Test_helper.source in
    let expected_err = v.Test_helper.to_ascii_n_err in
    let expected_value = v.Test_helper.to_ascii_n in
    let r = Idna.Uts46.to_ascii source in
    match r with
    | Ok s ->
      Alcotest.(check bool)
        (Printf.sprintf "toAsciiN(%s) expected no error" (label_of source))
        false expected_err;
      Alcotest.(check string)
        (Printf.sprintf "toAsciiN(%s) value" (label_of source))
        expected_value s
    | Error _ ->
      Alcotest.(check bool)
        (Printf.sprintf "toAsciiN(%s) expected error" (label_of source))
        true expected_err
  ) vectors

let () =
  Alcotest.run "uts46" [
    "regressions", [
      Alcotest.test_case "invalid utf8 lowercases ascii" `Quick test_regression_invalid_utf8_ascii_lowercase;
      Alcotest.test_case "xn-- to empty" `Quick test_regression_xn_empty;
      Alcotest.test_case "xn-- ascii hyphen" `Quick test_regression_xn_ascii_hyphen;
      Alcotest.test_case "xn-- unicode hyphen domain" `Quick test_regression_xn_unicode_hyphen_domain;
      Alcotest.test_case "root label verify disabled" `Quick test_regression_root_label_verify_disabled;
      Alcotest.test_case "root label verify enabled" `Quick test_regression_root_label_verify_enabled;
      Alcotest.test_case "reserved xn when hyphen checks disabled" `Quick test_regression_reserved_xn_when_check_hyphens_disabled;
    ];
    "toUnicode", [ Alcotest.test_case "all vectors" `Quick test_to_unicode ];
    "toAsciiN",  [ Alcotest.test_case "all vectors" `Quick test_to_ascii_n ];
  ]
