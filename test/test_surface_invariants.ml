type uts46_flags = Idna.Uts46.flags

let pp_result = function
  | Ok s -> Printf.sprintf "Ok %S" s
  | Error e -> Printf.sprintf "Error %S" e

let is_ascii_lowercase s =
  let ok = ref true in
  for i = 0 to String.length s - 1 do
    let c = Char.code s.[i] in
    if c >= 0x80 || (c >= 0x41 && c <= 0x5A) then ok := false
  done;
  !ok

let starts_with ~prefix s =
  let prefix_len = String.length prefix in
  String.length s >= prefix_len && String.sub s 0 prefix_len = prefix

let contains_xn_label s =
  String.split_on_char '.' s
  |> List.exists (fun label -> starts_with ~prefix:"xn--" label)

let has_error report =
  List.exists
    (fun event -> event.Idna.Diagnostics.severity = Idna.Diagnostics.Error)
    report.Idna.Diagnostics.events

let check_report_consistency name report =
  Alcotest.(check bool)
    (name ^ " accepted matches error presence")
    (not (has_error report))
    report.Idna.Diagnostics.accepted

let check_ascii_result_shape name = function
  | Ok output ->
      Alcotest.(check bool)
        (name ^ " lowercase ASCII")
        true
        (is_ascii_lowercase output)
  | Error _ -> ()

let check_report_result name public report =
  let public_ok = match public with Ok _ -> true | Error _ -> false in
  let public_output =
    match public with Ok output -> Some output | Error _ -> None
  in
  check_report_consistency name report;
  Alcotest.(check bool)
    (name ^ " accepted parity")
    public_ok report.Idna.Diagnostics.accepted;
  Alcotest.(check (option string))
    (name ^ " output parity") public_output report.Idna.Diagnostics.output

let check_registration ?flags name input =
  let label_public =
    match Idna.Registration.check_label input with
    | Ok () -> Ok ""
    | Error e -> Error e
  in
  let label_report = Idna.Diagnostics.Registration.check_label input in
  check_report_consistency (name ^ " registration.check_label") label_report;
  Alcotest.(check bool)
    (name ^ " registration.check_label accepted parity")
    (Result.is_ok label_public)
    label_report.accepted;
  Alcotest.(check (option string))
    (name ^ " registration.check_label output")
    None label_report.output;

  let ascii_public = Idna.Registration.to_ascii ?flags input in
  check_ascii_result_shape (name ^ " registration.to_ascii") ascii_public;
  check_report_result
    (name ^ " registration.to_ascii")
    ascii_public
    (Idna.Diagnostics.Registration.to_ascii ?flags input);

  let unicode_public = Idna.Registration.to_unicode ?flags input in
  check_report_result
    (name ^ " registration.to_unicode")
    unicode_public
    (Idna.Diagnostics.Registration.to_unicode ?flags input);

  let hostname_public = Idna.Registration.is_valid_hostname ?flags input in
  let hostname_report =
    Idna.Diagnostics.Registration.is_valid_hostname ?flags input
  in
  check_report_consistency
    (name ^ " registration.is_valid_hostname")
    hostname_report;
  Alcotest.(check bool)
    (name ^ " registration.is_valid_hostname accepted parity")
    hostname_public hostname_report.accepted

let check_lookup ?flags name input =
  let ascii_public = Idna.Lookup.to_ascii ?flags input in
  check_ascii_result_shape (name ^ " lookup.to_ascii") ascii_public;
  check_report_result
    (name ^ " lookup.to_ascii")
    ascii_public
    (Idna.Diagnostics.Lookup.to_ascii ?flags input);

  let unicode_public = Idna.Lookup.to_unicode ?flags input in
  check_report_result
    (name ^ " lookup.to_unicode")
    unicode_public
    (Idna.Diagnostics.Lookup.to_unicode ?flags input)

let check_uts46 ?(flags = Idna.Uts46.default_flags) name input =
  let ascii_public = Idna.Uts46.to_ascii ~flags input in
  check_ascii_result_shape (name ^ " uts46.to_ascii") ascii_public;
  check_report_result (name ^ " uts46.to_ascii") ascii_public
    (Idna.Diagnostics.Uts46.to_ascii ~flags input);

  let unicode_public = Idna.Uts46.to_unicode ~flags input in
  let unicode_report = Idna.Diagnostics.Uts46.to_unicode ~flags input in
  check_report_consistency (name ^ " uts46.to_unicode") unicode_report;
  Alcotest.(check bool)
    (name ^ " uts46.to_unicode accepted parity")
    (not unicode_public.errored)
    unicode_report.accepted;
  Alcotest.(check (option string))
    (name ^ " uts46.to_unicode output parity")
    (Some unicode_public.value) unicode_report.output

let check_same_to_ascii_idempotent
    ?(expected_failure = fun ~output:_ ~again:_ -> false) name run input =
  match run input with
  | Error _ -> ()
  | Ok output -> (
      match run output with
      | Ok output' when output' = output -> ()
      | again ->
          if not (expected_failure ~output ~again) then
            Alcotest.fail
              (Printf.sprintf
                 "%s: to_ascii idempotence failed: first Ok %S, second %s" name
                 output (pp_result again)))

let check_to_unicode_idempotent name run input =
  match run input with
  | Error _ -> ()
  | Ok output -> (
      match run output with
      | Ok output' when output' = output -> ()
      | again ->
          Alcotest.fail
            (Printf.sprintf
               "%s: to_unicode idempotence failed: first Ok %S, second %s" name
               output (pp_result again)))

let check_uts46_to_unicode_idempotent name flags input =
  let first = Idna.Uts46.to_unicode ~flags input in
  if not first.errored then begin
    let second = Idna.Uts46.to_unicode ~flags first.value in
    if second.errored || second.value <> first.value then
      Alcotest.fail
        (Printf.sprintf
           "%s: UTS46 to_unicode idempotence failed: first=%S \
            second={value=%S; errored=%b}"
           name first.value second.value second.errored)
  end

let expected_uts46_to_ascii_caveat ~(flags : uts46_flags) ~output ~again =
  (not flags.check_hyphens) && contains_xn_label output
  && match again with Error _ -> true | Ok _ -> false

let check_value_invariants ?reg_flags ?lookup_flags
    ?(uts46_flags = Idna.Uts46.default_flags) name input =
  check_same_to_ascii_idempotent
    (name ^ " registration.to_ascii")
    (Idna.Registration.to_ascii ?flags:reg_flags)
    input;
  check_to_unicode_idempotent
    (name ^ " registration.to_unicode")
    (Idna.Registration.to_unicode ?flags:reg_flags)
    input;
  check_same_to_ascii_idempotent
    (name ^ " lookup.to_ascii")
    (Idna.Lookup.to_ascii ?flags:lookup_flags)
    input;
  check_to_unicode_idempotent
    (name ^ " lookup.to_unicode")
    (Idna.Lookup.to_unicode ?flags:lookup_flags)
    input;
  check_same_to_ascii_idempotent (name ^ " uts46.to_ascii")
    ~expected_failure:(expected_uts46_to_ascii_caveat ~flags:uts46_flags)
    (Idna.Uts46.to_ascii ~flags:uts46_flags)
    input;
  check_uts46_to_unicode_idempotent
    (name ^ " uts46.to_unicode")
    uts46_flags input

let cases =
  [
    ("ascii", "example");
    ("ascii-domain", "Example.COM");
    ("dot-u3002", "example\xe3\x80\x82com");
    ("dot-ff0e", "example\xef\xbc\x8ecom");
    ("dot-ff61", "example\xef\xbd\xa1com");
    ("alabel-upper", "XN--MAANA-PTA");
    ("xn-empty", "xn--");
    ("xn-invalid", "xn--!!!");
    ("xn-ascii-decoding", "xn--ASCII-");
    ("xn-oddity", "xn---nde");
    ("xn-non-ascii", "xn--\xc3\xb1");
    ("contexto-middle-dot", "a\xc2\xb7a");
    ("contextj-zwnj", "a\xe2\x80\x8ca");
    ("std3-underscore", "foo_bar");
    ("ignored-soft-hyphen", "a\xc2\xadb");
    ("deviation-sharp-s", "Fa\xc3\x9f");
    ("deviation-final-sigma", "\xce\xb2\xcf\x8c\xce\xbb\xce\xbf\xcf\x82");
    ("arabic-indic-valid", "\xd8\xa8\xd9\xa0\xd8\xa8");
    ("arabic-indic-mixed", "\xd8\xa8\xd9\xa0\xdb\xb0\xd8\xa8");
    ("ext-arabic-valid", "\xd8\xa8\xdb\xb0\xd8\xa8");
    ("ext-arabic-mixed", "\xd8\xa8\xdb\xb0\xd9\xa0\xd8\xa8");
    ("rtl-digit", "\xd7\x90\xd7\x91.1com");
    ("trailing-root", "example.");
    ("empty-interior", "a..b");
    ("leading-empty", ".example");
    ("long-label", String.make 64 'a');
    ("invalid-utf8", "A\xed\xa4\x80Z");
  ]

let uts46_flag_sets =
  [
    ("default", Idna.Uts46.default_flags);
    ("no-dns", { Idna.Uts46.default_flags with verify_dns_length = false });
    ("ignore", { Idna.Uts46.default_flags with ignore_invalid_punycode = true });
    ("no-hy", { Idna.Uts46.default_flags with check_hyphens = false });
    ("no-join", { Idna.Uts46.default_flags with check_joiners = false });
    ("no-std3", { Idna.Uts46.default_flags with use_std3_ascii_rules = false });
    ( "no-hy-ignore",
      {
        Idna.Uts46.default_flags with
        check_hyphens = false;
        ignore_invalid_punycode = true;
      } );
    ("no-bidi", { Idna.Uts46.default_flags with check_bidi = false });
    ( "no-bidi-dns",
      {
        Idna.Uts46.default_flags with
        check_bidi = false;
        verify_dns_length = false;
      } );
    ( "no-join-std3",
      {
        Idna.Uts46.default_flags with
        check_joiners = false;
        use_std3_ascii_rules = false;
      } );
  ]

let test_public_diagnostics_parity () =
  List.iter
    (fun (name, input) ->
      check_registration name input;
      check_registration
        ~flags:{ Idna.Registration.verify_dns_length = false }
        ("reg-no-dns/" ^ name) input;
      check_lookup name input;
      check_lookup
        ~flags:{ Idna.Lookup.check_bidi = false }
        ("lookup-no-bidi/" ^ name) input;
      List.iter
        (fun (flag_name, flags) ->
          check_uts46 ~flags ("uts46-" ^ flag_name ^ "/" ^ name) input)
        uts46_flag_sets)
    cases

let test_value_invariants () =
  List.iter
    (fun (name, input) ->
      check_value_invariants name input;
      check_value_invariants
        ~reg_flags:{ Idna.Registration.verify_dns_length = false }
        ("reg-no-dns/" ^ name) input;
      check_value_invariants
        ~lookup_flags:{ Idna.Lookup.check_bidi = false }
        ("lookup-no-bidi/" ^ name) input;
      List.iter
        (fun (flag_name, flags) ->
          check_value_invariants ~uts46_flags:flags
            ("uts46-" ^ flag_name ^ "/" ^ name)
            input)
        uts46_flag_sets)
    cases

let () =
  Alcotest.run "surface_invariants"
    [
      ( "cross-surface",
        [
          Alcotest.test_case "public/diagnostics parity" `Quick
            test_public_diagnostics_parity;
        ] );
      ( "values",
        [
          Alcotest.test_case "idempotence and output shape" `Quick
            test_value_invariants;
        ] );
    ]
