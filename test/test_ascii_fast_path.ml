let result_of_report report =
  if report.Idna.Diagnostics.accepted then
    match report.output with
    | Some output -> Ok output
    | None -> Error "no output"
  else Error "diagnostics rejected"

let check_result name expected actual =
  Alcotest.(check (result string string)) name expected actual

let check_result_shape name report actual =
  match (report.Idna.Diagnostics.accepted, actual) with
  | true, Ok output ->
      Alcotest.(check string)
        name
        (Option.value report.output ~default:"")
        output
  | true, Error msg ->
      Alcotest.fail
        (Printf.sprintf "%s: expected Ok from diagnostics, got Error %S" name
           msg)
  | false, Error _ -> ()
  | false, Ok output ->
      Alcotest.fail
        (Printf.sprintf "%s: expected Error from diagnostics, got Ok %S" name
           output)

let check_bool name expected actual = Alcotest.(check bool) name expected actual

let check_uts46_unicode name expected actual =
  Alcotest.(check string)
    (name ^ " value") expected.Idna.Uts46.value actual.Idna.Uts46.value;
  Alcotest.(check bool) (name ^ " errored") expected.errored actual.errored

let uts46_unicode_of_report report =
  {
    Idna.Uts46.value = Option.value report.Idna.Diagnostics.output ~default:"";
    errored = not report.accepted;
  }

let fast_candidate_domains =
  [
    "Example.COM";
    "foo-bar.example";
    "MAIL01.Example";
    "a.b.example";
    "STATIC-ASSETS.example";
  ]

let test_default_public_entry_points_match_diagnostics () =
  List.iter
    (fun domain ->
      check_result
        ("uts46.to_ascii " ^ domain)
        (result_of_report (Idna.Diagnostics.Uts46.to_ascii domain))
        (Idna.Uts46.to_ascii domain);
      check_uts46_unicode
        ("uts46.to_unicode " ^ domain)
        (uts46_unicode_of_report (Idna.Diagnostics.Uts46.to_unicode domain))
        (Idna.Uts46.to_unicode domain);
      check_result
        ("lookup.to_ascii " ^ domain)
        (result_of_report (Idna.Diagnostics.Lookup.to_ascii domain))
        (Idna.Lookup.to_ascii domain);
      check_result
        ("lookup.to_unicode " ^ domain)
        (result_of_report (Idna.Diagnostics.Lookup.to_unicode domain))
        (Idna.Lookup.to_unicode domain);
      check_result
        ("registration.to_ascii " ^ domain)
        (result_of_report (Idna.Diagnostics.Registration.to_ascii domain))
        (Idna.Registration.to_ascii domain);
      check_result
        ("registration.to_unicode " ^ domain)
        (result_of_report (Idna.Diagnostics.Registration.to_unicode domain))
        (Idna.Registration.to_unicode domain);
      check_bool
        ("registration.is_valid_hostname " ^ domain)
        (Idna.Diagnostics.Registration.is_valid_hostname domain).accepted
        (Idna.Registration.is_valid_hostname domain))
    fast_candidate_domains

let test_registration_check_label_matches_diagnostics () =
  List.iter
    (fun label ->
      let expected =
        if (Idna.Diagnostics.Registration.check_label label).accepted then Ok ()
        else Error "diagnostics rejected"
      in
      Alcotest.(check (result unit string))
        ("registration.check_label " ^ label)
        expected
        (match Idna.Registration.check_label label with
        | Ok () -> Ok ()
        | Error _ -> Error "diagnostics rejected"))
    [ "Example"; "STATIC-ASSETS"; String.make 64 'a' ]

let test_uts46_hyphen_flag_is_preserved () =
  let relaxed = { Idna.Uts46.default_flags with check_hyphens = false } in
  List.iter
    (fun domain ->
      check_result
        ("uts46.to_ascii relaxed " ^ domain)
        (result_of_report
           (Idna.Diagnostics.Uts46.to_ascii ~flags:relaxed domain))
        (Idna.Uts46.to_ascii ~flags:relaxed domain);
      check_uts46_unicode
        ("uts46.to_unicode relaxed " ^ domain)
        (uts46_unicode_of_report
           (Idna.Diagnostics.Uts46.to_unicode ~flags:relaxed domain))
        (Idna.Uts46.to_unicode ~flags:relaxed domain))
    [ "-Bad.example"; "bad-.example"; "ab--cd.example" ];
  List.iter
    (fun domain ->
      match Idna.Uts46.to_ascii domain with
      | Error _ -> ()
      | Ok output ->
          Alcotest.fail
            (Printf.sprintf "default UTS46 unexpectedly accepted %S as %S"
               domain output))
    [ "-Bad.example"; "bad-.example"; "ab--cd.example" ]

let test_dns_length_flags_are_preserved () =
  let long_label = String.make 64 'a' in
  let relaxed_registration = { Idna.Registration.verify_dns_length = false } in
  let relaxed_uts46 =
    { Idna.Uts46.default_flags with verify_dns_length = false }
  in
  check_result "registration.to_ascii relaxed long label"
    (result_of_report
       (Idna.Diagnostics.Registration.to_ascii ~flags:relaxed_registration
          long_label))
    (Idna.Registration.to_ascii ~flags:relaxed_registration long_label);
  check_result "uts46.to_ascii relaxed long label"
    (result_of_report
       (Idna.Diagnostics.Uts46.to_ascii ~flags:relaxed_uts46 long_label))
    (Idna.Uts46.to_ascii ~flags:relaxed_uts46 long_label);
  check_bool "registration.is_valid_hostname default long label" false
    (Idna.Registration.is_valid_hostname long_label);
  check_bool "lookup.to_ascii accepts long ascii label"
    (Idna.Diagnostics.Lookup.to_ascii long_label).accepted
    (Result.is_ok (Idna.Lookup.to_ascii long_label))

let test_slow_path_edges_remain_publicly_identical () =
  let cases =
    [
      "example.";
      ".example";
      "bad..example";
      "xn--maana-pta.example";
      "under_score.example";
    ]
  in
  List.iter
    (fun domain ->
      check_result_shape
        ("uts46.to_ascii slow " ^ domain)
        (Idna.Diagnostics.Uts46.to_ascii domain)
        (Idna.Uts46.to_ascii domain);
      check_uts46_unicode
        ("uts46.to_unicode slow " ^ domain)
        (uts46_unicode_of_report (Idna.Diagnostics.Uts46.to_unicode domain))
        (Idna.Uts46.to_unicode domain))
    cases

let test_uts46_alabel_to_unicode_matches_diagnostics () =
  let default_cases =
    [
      "xn--maana-pta.example";
      "XN--MAANA-PTA.EXAMPLE";
      "ok.xn--bcher-kva.example";
      "xn--caf-dma.example";
      "xn--mgbh0fb.example";
      "xn--";
      "xn--!!!";
      "xn--ASCII-";
      "xn--unicode-.org";
      "xn---nde";
    ]
  in
  List.iter
    (fun domain ->
      check_uts46_unicode
        ("uts46.to_unicode alabel " ^ domain)
        (uts46_unicode_of_report (Idna.Diagnostics.Uts46.to_unicode domain))
        (Idna.Uts46.to_unicode domain))
    default_cases;
  let ignore_invalid =
    { Idna.Uts46.default_flags with ignore_invalid_punycode = true }
  in
  check_uts46_unicode "uts46.to_unicode alabel ignore invalid punycode"
    (uts46_unicode_of_report
       (Idna.Diagnostics.Uts46.to_unicode ~flags:ignore_invalid "xn--!!!"))
    (Idna.Uts46.to_unicode ~flags:ignore_invalid "xn--!!!");
  let relaxed_hyphen =
    { Idna.Uts46.default_flags with check_hyphens = false }
  in
  check_uts46_unicode "uts46.to_unicode alabel relaxed hyphen"
    (uts46_unicode_of_report
       (Idna.Diagnostics.Uts46.to_unicode ~flags:relaxed_hyphen "xn--maana-pta"))
    (Idna.Uts46.to_unicode ~flags:relaxed_hyphen "xn--maana-pta")

let test_uts46_unicode_to_ascii_matches_diagnostics () =
  let default_cases =
    [
      "ma\195\177ana.example";
      "b\195\188cher.example";
      "caf\195\169.example";
      "fa\195\159.de";
      "B\195\156CHER.Example";
      "\227\128\130example";
      "example\227\128\130";
      "bad\227\128\130\227\128\130example";
      "\xd7\x90\xd7\x91.1com";
      "a\xe2\x80\x8c.example";
      "xn--maana-pta.example";
    ]
  in
  List.iter
    (fun domain ->
      check_result_shape
        ("uts46.to_ascii unicode " ^ domain)
        (Idna.Diagnostics.Uts46.to_ascii domain)
        (Idna.Uts46.to_ascii domain))
    default_cases;
  let relaxed = { Idna.Uts46.default_flags with verify_dns_length = false } in
  check_result_shape "uts46.to_ascii unicode relaxed dns"
    (Idna.Diagnostics.Uts46.to_ascii ~flags:relaxed "example.")
    (Idna.Uts46.to_ascii ~flags:relaxed "example.")

let () =
  Alcotest.run "ascii_fast_path"
    [
      ( "parity",
        [
          Alcotest.test_case "default public entry points" `Quick
            test_default_public_entry_points_match_diagnostics;
          Alcotest.test_case "registration check_label" `Quick
            test_registration_check_label_matches_diagnostics;
          Alcotest.test_case "uts46 hyphen flag" `Quick
            test_uts46_hyphen_flag_is_preserved;
          Alcotest.test_case "dns length flags" `Quick
            test_dns_length_flags_are_preserved;
          Alcotest.test_case "slow path edges" `Quick
            test_slow_path_edges_remain_publicly_identical;
          Alcotest.test_case "uts46 a-label to_unicode" `Quick
            test_uts46_alabel_to_unicode_matches_diagnostics;
          Alcotest.test_case "uts46 unicode to_ascii" `Quick
            test_uts46_unicode_to_ascii_matches_diagnostics;
        ] );
    ]
