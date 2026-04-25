let has_event ?severity ?stage ?label_index ?cp report code =
  List.exists (fun event ->
    event.Idna.Diagnostics.code = code
    && (match severity with None -> true | Some x -> event.severity = x)
    && (match stage with None -> true | Some x -> event.stage = x)
    && (match label_index with None -> true | Some x -> event.label_index = Some x)
    && (match cp with None -> true | Some x -> event.cp = Some x)
  ) report.Idna.Diagnostics.events

let require_event ?severity ?stage ?label_index ?cp report code =
  if not (has_event ?severity ?stage ?label_index ?cp report code) then
    Alcotest.fail ("missing event: " ^ Idna.Diagnostics.string_of_code code)

let forbid_event ?severity ?stage ?label_index ?cp report code =
  if has_event ?severity ?stage ?label_index ?cp report code then
    Alcotest.fail ("unexpected event: " ^ Idna.Diagnostics.string_of_code code)

let string_of_stage = function
  | Idna.Diagnostics.Input -> "input"
  | Idna.Diagnostics.Utf8_decode -> "utf8_decode"
  | Idna.Diagnostics.Mapping -> "mapping"
  | Idna.Diagnostics.Normalization -> "normalization"
  | Idna.Diagnostics.Label_split -> "label_split"
  | Idna.Diagnostics.Label_classification -> "label_classification"
  | Idna.Diagnostics.A_label -> "a_label"
  | Idna.Diagnostics.Codepoint -> "codepoint"
  | Idna.Diagnostics.Context -> "context"
  | Idna.Diagnostics.Bidi -> "bidi"
  | Idna.Diagnostics.Dns_length -> "dns_length"
  | Idna.Diagnostics.Serialization -> "serialization"

let allowed_stages = function
  | Idna.Diagnostics.Empty_input ->
    [Idna.Diagnostics.Input; Idna.Diagnostics.Dns_length]
  | Idna.Diagnostics.Empty_label ->
    [Idna.Diagnostics.Input; Idna.Diagnostics.Label_split]
  | Idna.Diagnostics.Label_ascii_nr_ldh
  | Idna.Diagnostics.Label_u_label
  | Idna.Diagnostics.Label_a_label ->
    [Idna.Diagnostics.Label_classification]
  | Idna.Diagnostics.Ascii_lowercased ->
    [Idna.Diagnostics.Label_classification; Idna.Diagnostics.A_label]
  | Idna.Diagnostics.Label_not_nfc ->
    [Idna.Diagnostics.Normalization]
  | Idna.Diagnostics.Hyphen_start
  | Idna.Diagnostics.Hyphen_end
  | Idna.Diagnostics.Hyphen_3_4
  | Idna.Diagnostics.Initial_combiner
  | Idna.Diagnostics.Codepoint_disallowed
  | Idna.Diagnostics.Uts46_disallowed
  | Idna.Diagnostics.Std3_disallowed ->
    [Idna.Diagnostics.Codepoint]
  | Idna.Diagnostics.Reserved_xn_prefix
  | Idna.Diagnostics.A_label_non_ascii
  | Idna.Diagnostics.A_label_trailing_hyphen
  | Idna.Diagnostics.A_label_invalid_punycode
  | Idna.Diagnostics.A_label_decodes_to_ascii
  | Idna.Diagnostics.A_label_not_canonical
  | Idna.Diagnostics.A_label_not_lowercase_canonical
  | Idna.Diagnostics.Ignore_invalid_punycode_applied ->
    [Idna.Diagnostics.A_label]
  | Idna.Diagnostics.Contextj_failed
  | Idna.Diagnostics.Contexto_failed ->
    [Idna.Diagnostics.Context]
  | Idna.Diagnostics.Bidi_failed ->
    [Idna.Diagnostics.Bidi]
  | Idna.Diagnostics.Dns_label_too_long
  | Idna.Diagnostics.Dns_domain_too_long ->
    [Idna.Diagnostics.Dns_length]
  | Idna.Diagnostics.Trailing_root_present ->
    [Idna.Diagnostics.Label_split]
  | Idna.Diagnostics.Trailing_root_rejected ->
    [Idna.Diagnostics.Label_split; Idna.Diagnostics.Dns_length]
  | Idna.Diagnostics.Uts46_mapped
  | Idna.Diagnostics.Uts46_ignored ->
    [Idna.Diagnostics.Mapping; Idna.Diagnostics.Codepoint]
  | Idna.Diagnostics.Uts46_deviation ->
    [Idna.Diagnostics.Mapping]
  | Idna.Diagnostics.Idna2008_nv8
  | Idna.Diagnostics.Idna2008_xv8 ->
    [Idna.Diagnostics.Mapping; Idna.Diagnostics.Codepoint]
  | Idna.Diagnostics.Invalid_utf8 ->
    [Idna.Diagnostics.Utf8_decode]
  | Idna.Diagnostics.Serialization_failed ->
    [Idna.Diagnostics.Serialization]

let require_allowed_stage event =
  let allowed = allowed_stages event.Idna.Diagnostics.code in
  if not (List.mem event.stage allowed) then
    Alcotest.fail
      (Printf.sprintf
         "forbidden stage for %s: got %s"
         (Idna.Diagnostics.string_of_code event.code)
         (string_of_stage event.stage))

let require_report_stage_policy report =
  List.iter require_allowed_stage report.Idna.Diagnostics.events

let rec index_of_code code idx = function
  | [] -> None
  | event :: rest ->
    if event.Idna.Diagnostics.code = code then Some idx else index_of_code code (idx + 1) rest

let require_before report left right =
  let left_idx = index_of_code left 0 report.Idna.Diagnostics.events in
  let right_idx = index_of_code right 0 report.Idna.Diagnostics.events in
  match left_idx, right_idx with
  | Some l, Some r when l < r -> ()
  | _ ->
    Alcotest.fail
      ("expected event order: "
       ^ Idna.Diagnostics.string_of_code left
       ^ " before "
       ^ Idna.Diagnostics.string_of_code right)

let has_error report =
  List.exists (fun event -> event.Idna.Diagnostics.severity = Idna.Diagnostics.Error)
    report.Idna.Diagnostics.events

let a_label_of_cps cps =
  match Idna.Punycode.encode cps with
  | Ok encoded -> "xn--" ^ encoded
  | Error msg -> Alcotest.fail ("failed to construct test a-label: " ^ msg)

let check_registration_rejects_with_nv8 () =
  let report = Idna.Diagnostics.Registration.check_label "/" in
  Alcotest.(check bool) "accepted" false report.accepted;
  Alcotest.(check (option string)) "output" None report.output;
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x002F
    report
    Idna.Diagnostics.Idna2008_nv8;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x002F
    report
    Idna.Diagnostics.Codepoint_disallowed
  ;
  require_before report
    Idna.Diagnostics.Idna2008_nv8
    Idna.Diagnostics.Codepoint_disallowed;
  forbid_event
    ~stage:Idna.Diagnostics.Mapping
    report
    Idna.Diagnostics.Idna2008_nv8

let check_registration_empty_label () =
  let report = Idna.Diagnostics.Registration.check_label "" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Input
    report
    Idna.Diagnostics.Empty_label;
  forbid_event
    ~stage:Idna.Diagnostics.Label_split
    report
    Idna.Diagnostics.Empty_label

let check_registration_empty_input () =
  let report = Idna.Diagnostics.Registration.to_ascii "" in
  Alcotest.(check bool) "accepted" false report.accepted;
  Alcotest.(check (option string)) "output" None report.output;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Input
    report
    Idna.Diagnostics.Empty_input;
  forbid_event
    ~stage:Idna.Diagnostics.Dns_length
    report
    Idna.Diagnostics.Empty_input

let check_registration_ascii_label_classification () =
  let report = Idna.Diagnostics.Registration.check_label "Example" in
  Alcotest.(check bool) "accepted" true report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_ascii_nr_ldh;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Ascii_lowercased;
  forbid_event
    ~stage:Idna.Diagnostics.A_label
    report
    Idna.Diagnostics.Ascii_lowercased

let check_registration_ulabel_classification () =
  let report = Idna.Diagnostics.Registration.check_label "\xc3\xb1" in
  Alcotest.(check bool) "accepted" true report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_u_label;
  forbid_event
    ~stage:Idna.Diagnostics.A_label
    report
    Idna.Diagnostics.Label_u_label

let check_registration_nfc_failure () =
  let report = Idna.Diagnostics.Registration.check_label "a\xcd\x84" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_u_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Normalization
    report
    Idna.Diagnostics.Label_not_nfc;
  require_before report
    Idna.Diagnostics.Label_u_label
    Idna.Diagnostics.Label_not_nfc

let check_registration_hyphen_failures () =
  let start_report = Idna.Diagnostics.Registration.check_label "-abc" in
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2D
    start_report
    Idna.Diagnostics.Hyphen_start;

  let end_report = Idna.Diagnostics.Registration.check_label "abc-" in
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2D
    end_report
    Idna.Diagnostics.Hyphen_end;

  let mid_report = Idna.Diagnostics.Registration.check_label "ab--cd" in
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2D
    mid_report
    Idna.Diagnostics.Hyphen_3_4

let check_registration_initial_combiner () =
  let report = Idna.Diagnostics.Registration.check_label "\xcc\x80" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x0300
    report
    Idna.Diagnostics.Initial_combiner

let check_registration_context_failures () =
  let contexto_report = Idna.Diagnostics.Registration.check_label "a\xc2\xb7a" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    contexto_report
    Idna.Diagnostics.Label_u_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Context
    ~cp:0x00B7
    contexto_report
    Idna.Diagnostics.Contexto_failed;
  require_before contexto_report
    Idna.Diagnostics.Label_u_label
    Idna.Diagnostics.Contexto_failed;
  forbid_event
    ~stage:Idna.Diagnostics.Context
    contexto_report
    Idna.Diagnostics.Label_u_label;

  let contextj_report = Idna.Diagnostics.Registration.check_label "a\xe2\x80\x8ca" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    contextj_report
    Idna.Diagnostics.Label_u_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Context
    ~cp:0x200C
    contextj_report
    Idna.Diagnostics.Contextj_failed;
  require_before contextj_report
    Idna.Diagnostics.Label_u_label
    Idna.Diagnostics.Contextj_failed

let check_registration_bidi_failure () =
  let report = Idna.Diagnostics.Registration.check_label "\xd7\x90a" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_u_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Bidi
    report
    Idna.Diagnostics.Bidi_failed;
  require_before report
    Idna.Diagnostics.Label_u_label
    Idna.Diagnostics.Bidi_failed

let check_registration_rejects_with_xv8 () =
  let report = Idna.Diagnostics.Registration.check_label "\xe1\xa7\x9a" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x19DA
    report
    Idna.Diagnostics.Idna2008_xv8;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x19DA
    report
    Idna.Diagnostics.Codepoint_disallowed;
  require_before report
    Idna.Diagnostics.Idna2008_xv8
    Idna.Diagnostics.Codepoint_disallowed;
  forbid_event
    ~stage:Idna.Diagnostics.Mapping
    report
    Idna.Diagnostics.Idna2008_xv8

let check_registration_uppercase_alabel () =
  let report = Idna.Diagnostics.Registration.check_label "XN--MAANA-PTA" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    report
    Idna.Diagnostics.A_label_not_lowercase_canonical
  ;
  require_before report
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_not_lowercase_canonical;
  forbid_event
    ~stage:Idna.Diagnostics.A_label
    report
    Idna.Diagnostics.Label_a_label

let check_registration_alabel_failures () =
  let non_ascii = Idna.Diagnostics.Registration.check_label "xn--\xc3\xb1" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    non_ascii
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    non_ascii
    Idna.Diagnostics.A_label_non_ascii;
  require_before non_ascii
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_non_ascii;

  let trailing = Idna.Diagnostics.Registration.check_label "xn--ascii-" in
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    ~cp:0x2D
    trailing
    Idna.Diagnostics.A_label_trailing_hyphen;

  let invalid = Idna.Diagnostics.Registration.check_label "xn--!!!" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    invalid
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    invalid
    Idna.Diagnostics.A_label_invalid_punycode;
  require_before invalid
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_invalid_punycode;

  let noncanonical = Idna.Diagnostics.Registration.check_label "xn---nde" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    noncanonical
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    noncanonical
    Idna.Diagnostics.A_label_not_canonical;
  require_before noncanonical
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_not_canonical

let check_registration_dns_failures () =
  let long_label = Idna.Diagnostics.Registration.to_ascii (String.make 64 'a') in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    long_label
    Idna.Diagnostics.Label_ascii_nr_ldh;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Dns_length
    long_label
    Idna.Diagnostics.Dns_label_too_long;
  require_before long_label
    Idna.Diagnostics.Label_ascii_nr_ldh
    Idna.Diagnostics.Dns_label_too_long;

  let long_domain =
    String.concat "." [
      String.make 63 'a';
      String.make 63 'b';
      String.make 63 'c';
      String.make 63 'd';
    ]
  in
  let long_domain_report = Idna.Diagnostics.Registration.to_ascii long_domain in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    ~label_index:0
    long_domain_report
    Idna.Diagnostics.Label_ascii_nr_ldh;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Dns_length
    long_domain_report
    Idna.Diagnostics.Dns_domain_too_long;
  require_before long_domain_report
    Idna.Diagnostics.Label_ascii_nr_ldh
    Idna.Diagnostics.Dns_domain_too_long

let check_registration_empty_domain_label_split () =
  let report = Idna.Diagnostics.Registration.to_ascii "a..b" in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Label_split
    ~label_index:1
    report
    Idna.Diagnostics.Empty_label;
  forbid_event
    ~stage:Idna.Diagnostics.Input
    report
    Idna.Diagnostics.Empty_label

let check_lookup_uppercase_alabel () =
  let report = Idna.Diagnostics.Lookup.to_ascii "XN--MAANA-PTA" in
  Alcotest.(check bool) "accepted" true report.accepted;
  Alcotest.(check (option string)) "output"
    (Some "xn--maana-pta") report.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.A_label
    report
    Idna.Diagnostics.Ascii_lowercased
  ;
  require_before report
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.Ascii_lowercased;
  forbid_event
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Ascii_lowercased

let check_lookup_ascii_label_classification () =
  let report = Idna.Diagnostics.Lookup.to_ascii "Example.COM" in
  Alcotest.(check bool) "accepted" true report.accepted;
  Alcotest.(check (option string)) "output" (Some "example.com") report.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    ~label_index:0
    report
    Idna.Diagnostics.Label_ascii_nr_ldh;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    ~label_index:0
    report
    Idna.Diagnostics.Ascii_lowercased;
  forbid_event
    ~stage:Idna.Diagnostics.A_label
    ~label_index:0
    report
    Idna.Diagnostics.Ascii_lowercased

let check_lookup_bidi_failure () =
  let input = "\xd7\x90\xd7\x91.1com" in
  let report = Idna.Diagnostics.Lookup.to_ascii input in
  Alcotest.(check bool) "accepted" false report.accepted;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Bidi
    ~label_index:1
    report
    Idna.Diagnostics.Bidi_failed

let check_lookup_trailing_root_present () =
  let report = Idna.Diagnostics.Lookup.to_ascii "example." in
  Alcotest.(check bool) "accepted" true report.accepted;
  Alcotest.(check (option string)) "output" (Some "example.") report.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_split
    ~label_index:1
    report
    Idna.Diagnostics.Trailing_root_present;
  forbid_event report Idna.Diagnostics.Trailing_root_rejected;
  forbid_event
    ~stage:Idna.Diagnostics.Dns_length
    report
    Idna.Diagnostics.Trailing_root_present

let check_lookup_dns_length_not_prechecked () =
  let long_label = String.make 64 'a' in
  let long_label_report = Idna.Diagnostics.Lookup.to_ascii long_label in
  Alcotest.(check bool) "accepted long label" true long_label_report.accepted;
  Alcotest.(check (option string)) "output long label" (Some long_label) long_label_report.output;
  forbid_event long_label_report Idna.Diagnostics.Dns_label_too_long;
  forbid_event long_label_report Idna.Diagnostics.Dns_domain_too_long;

  let long_domain = String.concat "." [
    String.make 63 'a';
    String.make 63 'b';
    String.make 63 'c';
    String.make 63 'd';
  ] in
  let long_domain_report = Idna.Diagnostics.Lookup.to_ascii long_domain in
  Alcotest.(check bool) "accepted long domain" true long_domain_report.accepted;
  Alcotest.(check (option string)) "output long domain" (Some long_domain) long_domain_report.output;
  forbid_event long_domain_report Idna.Diagnostics.Dns_label_too_long;
  forbid_event long_domain_report Idna.Diagnostics.Dns_domain_too_long

let check_uts46_mapping_and_deviation () =
  let report = Idna.Diagnostics.Uts46.to_ascii "Fa\xc3\x9f.de" in
  Alcotest.(check bool) "accepted" true report.accepted;
  Alcotest.(check (option string)) "output"
    (Some "xn--fa-hia.de") report.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x0046
    report
    Idna.Diagnostics.Uts46_mapped;
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x00DF
    report
    Idna.Diagnostics.Uts46_deviation
  ;
  require_before report
    Idna.Diagnostics.Uts46_mapped
    Idna.Diagnostics.Uts46_deviation;
  forbid_event
    ~stage:Idna.Diagnostics.Codepoint
    report
    Idna.Diagnostics.Uts46_mapped;
  forbid_event
    ~stage:Idna.Diagnostics.Codepoint
    report
    Idna.Diagnostics.Uts46_deviation

let check_uts46_mapping_variants () =
  let ignored = Idna.Diagnostics.Uts46.to_unicode "a\xc2\xadb" in
  Alcotest.(check bool) "accepted" true ignored.accepted;
  Alcotest.(check (option string)) "output" (Some "ab") ignored.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x00AD
    ignored
    Idna.Diagnostics.Uts46_ignored;
  forbid_event
    ~stage:Idna.Diagnostics.Codepoint
    ignored
    Idna.Diagnostics.Uts46_ignored;

  let disallowed = Idna.Diagnostics.Uts46.to_ascii "a\xe2\x80\xa8" in
  Alcotest.(check bool) "accepted" false disallowed.accepted;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2028
    disallowed
    Idna.Diagnostics.Uts46_disallowed;

  let ignored_then_disallowed = Idna.Diagnostics.Uts46.to_ascii "\xc2\xad\xe2\x80\xa8" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x00AD
    ignored_then_disallowed
    Idna.Diagnostics.Uts46_ignored;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2028
    ignored_then_disallowed
    Idna.Diagnostics.Uts46_disallowed;
  require_before ignored_then_disallowed
    Idna.Diagnostics.Uts46_ignored
    Idna.Diagnostics.Uts46_disallowed;

  let mapped_nv8_then_disallowed = Idna.Diagnostics.Uts46.to_ascii "\xc2\xa1\xe2\x80\xa8" in
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x00A1
    mapped_nv8_then_disallowed
    Idna.Diagnostics.Idna2008_nv8;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2028
    mapped_nv8_then_disallowed
    Idna.Diagnostics.Uts46_disallowed;
  require_before mapped_nv8_then_disallowed
    Idna.Diagnostics.Idna2008_nv8
    Idna.Diagnostics.Uts46_disallowed;

  let mapped_xv8_then_disallowed = Idna.Diagnostics.Uts46.to_ascii "\xe1\xa7\x9a\xe2\x80\xa8" in
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x19DA
    mapped_xv8_then_disallowed
    Idna.Diagnostics.Idna2008_xv8;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2028
    mapped_xv8_then_disallowed
    Idna.Diagnostics.Uts46_disallowed;
  require_before mapped_xv8_then_disallowed
    Idna.Diagnostics.Idna2008_xv8
    Idna.Diagnostics.Uts46_disallowed;

  let deviation_then_disallowed = Idna.Diagnostics.Uts46.to_ascii "\xc3\x9f\xe2\x80\xa8" in
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x00DF
    deviation_then_disallowed
    Idna.Diagnostics.Uts46_deviation;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2028
    deviation_then_disallowed
    Idna.Diagnostics.Uts46_disallowed;
  require_before deviation_then_disallowed
    Idna.Diagnostics.Uts46_deviation
    Idna.Diagnostics.Uts46_disallowed;
  forbid_event
    ~stage:Idna.Diagnostics.Codepoint
    deviation_then_disallowed
    Idna.Diagnostics.Uts46_deviation;

  let mapped_then_disallowed = Idna.Diagnostics.Uts46.to_ascii "A\xe2\x80\xa8" in
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Mapping
    ~cp:0x0041
    mapped_then_disallowed
    Idna.Diagnostics.Uts46_mapped;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x2028
    mapped_then_disallowed
    Idna.Diagnostics.Uts46_disallowed;
  require_before mapped_then_disallowed
    Idna.Diagnostics.Uts46_mapped
    Idna.Diagnostics.Uts46_disallowed;

  let mapped_from_alabel =
    Idna.Diagnostics.Uts46.to_unicode (a_label_of_cps [ 0x00C4 ])
  in
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x00C4
    mapped_from_alabel
    Idna.Diagnostics.Uts46_mapped;
  forbid_event
    ~stage:Idna.Diagnostics.Mapping
    mapped_from_alabel
    Idna.Diagnostics.Uts46_mapped;

  let ignored_from_alabel =
    Idna.Diagnostics.Uts46.to_unicode (a_label_of_cps [ 0x00AD ])
  in
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x00AD
    ignored_from_alabel
    Idna.Diagnostics.Uts46_ignored;
  forbid_event
    ~stage:Idna.Diagnostics.Mapping
    ignored_from_alabel
    Idna.Diagnostics.Uts46_ignored;

  let std3 = Idna.Diagnostics.Uts46.to_ascii "foo_bar" in
  Alcotest.(check bool) "accepted" false std3.accepted;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Codepoint
    ~cp:0x005F
    std3
    Idna.Diagnostics.Std3_disallowed

let check_uts46_alabel_failures () =
  let decodes_ascii = Idna.Diagnostics.Uts46.to_unicode "xn--ASCII-" in
  Alcotest.(check bool) "accepted" false decodes_ascii.accepted;
  Alcotest.(check (option string)) "output" (Some "ascii") decodes_ascii.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    decodes_ascii
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    decodes_ascii
    Idna.Diagnostics.A_label_decodes_to_ascii;
  require_before decodes_ascii
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_decodes_to_ascii;

  let ignored_invalid =
    let flags = { Idna.Uts46.default_flags with ignore_invalid_punycode = true } in
    Idna.Diagnostics.Uts46.to_unicode ~flags "xn--!!!"
  in
  Alcotest.(check bool) "accepted" true ignored_invalid.accepted;
  Alcotest.(check (option string)) "output" (Some "xn--!!!") ignored_invalid.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    ignored_invalid
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Warning
    ~stage:Idna.Diagnostics.A_label
    ignored_invalid
    Idna.Diagnostics.Ignore_invalid_punycode_applied;
  require_before ignored_invalid
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.Ignore_invalid_punycode_applied;
  forbid_event
    ~stage:Idna.Diagnostics.Codepoint
    ignored_invalid
    Idna.Diagnostics.Ignore_invalid_punycode_applied

let check_uts46_reserved_xn_prefix () =
  let flags = { Idna.Uts46.default_flags with check_hyphens = false } in
  let report = Idna.Diagnostics.Uts46.to_unicode ~flags "xn--maana-pta" in
  Alcotest.(check bool) "accepted" false report.accepted;
  Alcotest.(check (option string)) "output" (Some "mañana") report.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_a_label;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.A_label
    report
    Idna.Diagnostics.Reserved_xn_prefix;
  require_before report
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.Reserved_xn_prefix;
  forbid_event
    ~stage:Idna.Diagnostics.Codepoint
    report
    Idna.Diagnostics.Reserved_xn_prefix

let check_uts46_invalid_utf8 () =
  let input = "A\xed\xa4\x80Z" in
  let report = Idna.Diagnostics.Uts46.to_unicode input in
  Alcotest.(check bool) "accepted" false report.accepted;
  Alcotest.(check (option string)) "output"
    (Some "a\xed\xa4\x80z") report.output;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Utf8_decode
    report
    Idna.Diagnostics.Invalid_utf8

let check_uts46_empty_input_and_root_rejection () =
  let empty = Idna.Diagnostics.Uts46.to_unicode "" in
  Alcotest.(check bool) "accepted" false empty.accepted;
  Alcotest.(check (option string)) "output" (Some "") empty.output;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Input
    empty
    Idna.Diagnostics.Empty_input;
  forbid_event
    ~stage:Idna.Diagnostics.Dns_length
    empty
    Idna.Diagnostics.Empty_input;

  let rooted = Idna.Diagnostics.Uts46.to_ascii "example." in
  Alcotest.(check bool) "accepted" false rooted.accepted;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_split
    ~label_index:1
    rooted
    Idna.Diagnostics.Trailing_root_present;
  require_event
    ~severity:Idna.Diagnostics.Error
    ~stage:Idna.Diagnostics.Dns_length
    ~label_index:1
    rooted
    Idna.Diagnostics.Trailing_root_rejected;
  require_before rooted
    Idna.Diagnostics.Trailing_root_present
    Idna.Diagnostics.Trailing_root_rejected;
  forbid_event
    ~stage:Idna.Diagnostics.Label_split
    rooted
    Idna.Diagnostics.Trailing_root_rejected

let check_uts46_long_label_verify_disabled () =
  let flags = { Idna.Uts46.default_flags with verify_dns_length = false } in
  let input = String.make 64 'a' in
  let report = Idna.Diagnostics.Uts46.to_ascii ~flags input in
  Alcotest.(check bool) "accepted" true report.accepted;
  Alcotest.(check (option string)) "output" (Some input) report.output;
  require_event
    ~severity:Idna.Diagnostics.Info
    ~stage:Idna.Diagnostics.Label_classification
    report
    Idna.Diagnostics.Label_ascii_nr_ldh;
  forbid_event
    ~stage:Idna.Diagnostics.Dns_length
    report
    Idna.Diagnostics.Dns_label_too_long

let check_report_metadata () =
  let reg = Idna.Diagnostics.Registration.check_label "/" in
  Alcotest.(check bool) "registration policy"
    true (reg.policy = `Registration);
  Alcotest.(check bool) "registration operation"
    true (reg.operation = `Check_label);

  let lookup = Idna.Diagnostics.Lookup.to_ascii "XN--MAANA-PTA" in
  Alcotest.(check bool) "lookup policy"
    true (lookup.policy = `Lookup);
  Alcotest.(check bool) "lookup operation"
    true (lookup.operation = `To_ascii);

  let uts46 = Idna.Diagnostics.Uts46.to_unicode "Fa\xc3\x9f.de" in
  Alcotest.(check bool) "uts46 policy"
    true (uts46.policy = `Uts46);
  Alcotest.(check bool) "uts46 operation"
    true (uts46.operation = `To_unicode)

let check_accepted_matches_error_presence () =
  let reports = [
    Idna.Diagnostics.Registration.check_label "/";
    Idna.Diagnostics.Registration.check_label "example";
    Idna.Diagnostics.Lookup.to_ascii "XN--MAANA-PTA";
    Idna.Diagnostics.Lookup.to_ascii "\xd7\x90\xd7\x91.1com";
    Idna.Diagnostics.Uts46.to_ascii "Fa\xc3\x9f.de";
    Idna.Diagnostics.Uts46.to_unicode "A\xed\xa4\x80Z";
  ] in
  List.iter (fun report ->
    Alcotest.(check bool) "accepted matches error presence"
      (not (has_error report))
      report.accepted
  ) reports

let check_stage_policy_matrix () =
  let reports = [
    Idna.Diagnostics.Registration.check_label "/";
    Idna.Diagnostics.Registration.check_label "";
    Idna.Diagnostics.Registration.check_label "Example";
    Idna.Diagnostics.Registration.check_label "\xc3\xb1";
    Idna.Diagnostics.Registration.check_label "a\xcd\x84";
    Idna.Diagnostics.Registration.check_label "a\xc2\xb7a";
    Idna.Diagnostics.Registration.check_label "-abc";
    Idna.Diagnostics.Registration.check_label "XN--MAANA-PTA";
    Idna.Diagnostics.Registration.check_label "xn--!!!";
    Idna.Diagnostics.Registration.check_label "xn---nde";
    Idna.Diagnostics.Registration.to_ascii (String.make 64 'a');
    Idna.Diagnostics.Registration.to_ascii "a..b";
    Idna.Diagnostics.Lookup.to_ascii "XN--MAANA-PTA";
    Idna.Diagnostics.Lookup.to_ascii "Example.COM";
    Idna.Diagnostics.Lookup.to_ascii "\xd7\x90\xd7\x91.1com";
    Idna.Diagnostics.Lookup.to_ascii "example.";
    Idna.Diagnostics.Uts46.to_ascii "Fa\xc3\x9f.de";
    Idna.Diagnostics.Uts46.to_unicode "a\xc2\xadb";
    Idna.Diagnostics.Uts46.to_ascii "A\xe2\x80\xa8";
    Idna.Diagnostics.Uts46.to_unicode (a_label_of_cps [ 0x00C4 ]);
    Idna.Diagnostics.Uts46.to_unicode (a_label_of_cps [ 0x00AD ]);
    Idna.Diagnostics.Uts46.to_unicode "xn--ASCII-";
    Idna.Diagnostics.Uts46.to_unicode
      ~flags:{ Idna.Uts46.default_flags with ignore_invalid_punycode = true }
      "xn--!!!";
    Idna.Diagnostics.Uts46.to_unicode "A\xed\xa4\x80Z";
    Idna.Diagnostics.Uts46.to_unicode "";
    Idna.Diagnostics.Uts46.to_ascii "example.";
    Idna.Diagnostics.Uts46.to_unicode
      ~flags:{ Idna.Uts46.default_flags with check_hyphens = false }
      "xn--maana-pta";
  ] in
  List.iter require_report_stage_policy reports

let check_ordering_policy () =
  let registration_dns = Idna.Diagnostics.Registration.to_ascii (String.make 64 'a') in
  require_before registration_dns
    Idna.Diagnostics.Label_ascii_nr_ldh
    Idna.Diagnostics.Dns_label_too_long;

  let registration_nfc = Idna.Diagnostics.Registration.check_label "a\xcd\x84" in
  require_before registration_nfc
    Idna.Diagnostics.Label_u_label
    Idna.Diagnostics.Label_not_nfc;

  let lookup_alabel = Idna.Diagnostics.Lookup.to_ascii "XN--MAANA-PTA" in
  require_before lookup_alabel
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.Ascii_lowercased;

  let registration_invalid_alabel = Idna.Diagnostics.Registration.check_label "xn--!!!" in
  require_before registration_invalid_alabel
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_invalid_punycode;

  let registration_xv8 = Idna.Diagnostics.Registration.check_label "\xe1\xa7\x9a" in
  require_before registration_xv8
    Idna.Diagnostics.Idna2008_xv8
    Idna.Diagnostics.Codepoint_disallowed;

  let uts46_rejected = Idna.Diagnostics.Uts46.to_ascii "A\xe2\x80\xa8" in
  require_before uts46_rejected
    Idna.Diagnostics.Uts46_mapped
    Idna.Diagnostics.Uts46_disallowed;

  let uts46_nv8 = Idna.Diagnostics.Uts46.to_ascii "\xc2\xa1\xe2\x80\xa8" in
  require_before uts46_nv8
    Idna.Diagnostics.Idna2008_nv8
    Idna.Diagnostics.Uts46_disallowed;

  let uts46_ascii_decoding = Idna.Diagnostics.Uts46.to_unicode "xn--ASCII-" in
  require_before uts46_ascii_decoding
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.A_label_decodes_to_ascii;

  let uts46_reserved = Idna.Diagnostics.Uts46.to_unicode
      ~flags:{ Idna.Uts46.default_flags with check_hyphens = false }
      "xn--maana-pta"
  in
  require_before uts46_reserved
    Idna.Diagnostics.Label_a_label
    Idna.Diagnostics.Reserved_xn_prefix;

  let rooted = Idna.Diagnostics.Uts46.to_ascii "example." in
  require_before rooted
    Idna.Diagnostics.Trailing_root_present
    Idna.Diagnostics.Trailing_root_rejected

let check_parity_with_public_api () =
  let reg_report = Idna.Diagnostics.Registration.to_ascii "example.com" in
  let reg_public = Idna.Registration.to_ascii "example.com" in
  Alcotest.(check bool) "registration parity"
    (match reg_public with Ok _ -> true | Error _ -> false)
    reg_report.accepted;

  let hostname_report =
    Idna.Diagnostics.Registration.is_valid_hostname "Example.COM"
  in
  let hostname_public = Idna.Registration.is_valid_hostname "Example.COM" in
  Alcotest.(check bool) "registration hostname parity"
    hostname_public
    hostname_report.accepted;
  Alcotest.(check (option string)) "registration hostname output"
    (Some "example.com")
    hostname_report.output;

  let lookup_report = Idna.Diagnostics.Lookup.to_unicode "XN--MAANA-PTA" in
  let lookup_public = Idna.Lookup.to_unicode "XN--MAANA-PTA" in
  Alcotest.(check bool) "lookup parity"
    (match lookup_public with Ok _ -> true | Error _ -> false)
    lookup_report.accepted;

  let uts46_report = Idna.Diagnostics.Uts46.to_unicode "Fa\xc3\x9f.de" in
  let uts46_public = Idna.Uts46.to_unicode "Fa\xc3\x9f.de" in
  Alcotest.(check bool) "uts46 parity" (not uts46_public.errored) uts46_report.accepted

let check_registration_hostname_report_mirror () =
  let valid = Idna.Diagnostics.Registration.is_valid_hostname "Example.COM" in
  Alcotest.(check bool) "registration hostname policy"
    true (valid.policy = `Registration);
  Alcotest.(check bool) "registration hostname operation"
    true (valid.operation = `Is_valid_hostname);
  Alcotest.(check bool) "registration hostname accepted"
    true valid.accepted;
  Alcotest.(check (option string)) "registration hostname output"
    (Some "example.com") valid.output;

  let invalid = Idna.Diagnostics.Registration.is_valid_hostname "example." in
  Alcotest.(check bool) "registration invalid hostname accepted"
    false invalid.accepted;
  Alcotest.(check (option string)) "registration invalid hostname output"
    None invalid.output;
  require_event
    ~severity:Idna.Diagnostics.Error
    invalid
    Idna.Diagnostics.Trailing_root_rejected

let () =
  Alcotest.run "diagnostics" [
    "registration", [
      Alcotest.test_case "empty label" `Quick check_registration_empty_label;
      Alcotest.test_case "empty input" `Quick check_registration_empty_input;
      Alcotest.test_case "ascii label classification" `Quick check_registration_ascii_label_classification;
      Alcotest.test_case "u-label classification" `Quick check_registration_ulabel_classification;
      Alcotest.test_case "nfc failure" `Quick check_registration_nfc_failure;
      Alcotest.test_case "hyphen failures" `Quick check_registration_hyphen_failures;
      Alcotest.test_case "initial combiner" `Quick check_registration_initial_combiner;
      Alcotest.test_case "context failures" `Quick check_registration_context_failures;
      Alcotest.test_case "bidi failure" `Quick check_registration_bidi_failure;
      Alcotest.test_case "nv8 provenance + disallowed" `Quick check_registration_rejects_with_nv8;
      Alcotest.test_case "xv8 provenance + disallowed" `Quick check_registration_rejects_with_xv8;
      Alcotest.test_case "uppercase a-label rejected" `Quick check_registration_uppercase_alabel;
      Alcotest.test_case "a-label failures" `Quick check_registration_alabel_failures;
      Alcotest.test_case "dns failures" `Quick check_registration_dns_failures;
      Alcotest.test_case "empty domain label split" `Quick check_registration_empty_domain_label_split;
    ];
    "lookup", [
      Alcotest.test_case "uppercase a-label lowercased" `Quick check_lookup_uppercase_alabel;
      Alcotest.test_case "ascii label classification" `Quick check_lookup_ascii_label_classification;
      Alcotest.test_case "bidi failure" `Quick check_lookup_bidi_failure;
      Alcotest.test_case "trailing root present" `Quick check_lookup_trailing_root_present;
      Alcotest.test_case "dns length not prechecked" `Quick check_lookup_dns_length_not_prechecked;
    ];
    "uts46", [
      Alcotest.test_case "mapping and deviation" `Quick check_uts46_mapping_and_deviation;
      Alcotest.test_case "mapping variants" `Quick check_uts46_mapping_variants;
      Alcotest.test_case "a-label failures" `Quick check_uts46_alabel_failures;
      Alcotest.test_case "reserved xn prefix" `Quick check_uts46_reserved_xn_prefix;
      Alcotest.test_case "invalid utf8" `Quick check_uts46_invalid_utf8;
      Alcotest.test_case "empty input and root rejection" `Quick check_uts46_empty_input_and_root_rejection;
      Alcotest.test_case "long label verify disabled" `Quick check_uts46_long_label_verify_disabled;
    ];
    "parity", [
      Alcotest.test_case "public api parity" `Quick check_parity_with_public_api;
    ];
    "contract", [
      Alcotest.test_case "metadata fields" `Quick check_report_metadata;
      Alcotest.test_case "registration hostname report mirror" `Quick check_registration_hostname_report_mirror;
      Alcotest.test_case "accepted matches error presence" `Quick check_accepted_matches_error_presence;
      Alcotest.test_case "stage policy matrix" `Quick check_stage_policy_matrix;
      Alcotest.test_case "ordering policy" `Quick check_ordering_policy;
    ];
  ]
