(** Types, builder, and shared rule primitives for the diagnostic engine. *)

open Shared
module Rules = Idna2008_rules

type policy = [ `Registration | `Lookup | `Uts46 ]
type operation = [ `Check_label | `To_unicode | `To_ascii | `Is_valid_hostname ]
type severity = Error | Warning | Info

type stage =
  | Input
  | Utf8_decode
  | Mapping
  | Normalization
  | Label_split
  | Label_classification
  | A_label
  | Codepoint
  | Context
  | Bidi
  | Dns_length
  | Serialization

type code =
  | Empty_input
  | Empty_label
  | Label_ascii_nr_ldh
  | Label_u_label
  | Label_a_label
  | Ascii_lowercased
  | Label_not_nfc
  | Hyphen_start
  | Hyphen_end
  | Hyphen_3_4
  | Reserved_xn_prefix
  | Initial_combiner
  | Codepoint_disallowed
  | Contextj_failed
  | Contexto_failed
  | Bidi_failed
  | A_label_non_ascii
  | A_label_trailing_hyphen
  | A_label_invalid_punycode
  | A_label_decodes_to_ascii
  | A_label_not_canonical
  | A_label_not_lowercase_canonical
  | Dns_label_too_long
  | Dns_domain_too_long
  | Trailing_root_present
  | Trailing_root_rejected
  | Uts46_mapped
  | Uts46_ignored
  | Uts46_deviation
  | Uts46_disallowed
  | Std3_disallowed
  | Ignore_invalid_punycode_applied
  | Idna2008_nv8
  | Idna2008_xv8
  | Invalid_utf8
  | Serialization_failed

let string_of_code = function
  | Empty_input -> "empty_input"
  | Empty_label -> "empty_label"
  | Label_ascii_nr_ldh -> "label_ascii_nr_ldh"
  | Label_u_label -> "label_u_label"
  | Label_a_label -> "label_a_label"
  | Ascii_lowercased -> "ascii_lowercased"
  | Label_not_nfc -> "label_not_nfc"
  | Hyphen_start -> "hyphen_start"
  | Hyphen_end -> "hyphen_end"
  | Hyphen_3_4 -> "hyphen_3_4"
  | Reserved_xn_prefix -> "reserved_xn_prefix"
  | Initial_combiner -> "initial_combiner"
  | Codepoint_disallowed -> "codepoint_disallowed"
  | Contextj_failed -> "contextj_failed"
  | Contexto_failed -> "contexto_failed"
  | Bidi_failed -> "bidi_failed"
  | A_label_non_ascii -> "a_label_non_ascii"
  | A_label_trailing_hyphen -> "a_label_trailing_hyphen"
  | A_label_invalid_punycode -> "a_label_invalid_punycode"
  | A_label_decodes_to_ascii -> "a_label_decodes_to_ascii"
  | A_label_not_canonical -> "a_label_not_canonical"
  | A_label_not_lowercase_canonical -> "a_label_not_lowercase_canonical"
  | Dns_label_too_long -> "dns_label_too_long"
  | Dns_domain_too_long -> "dns_domain_too_long"
  | Trailing_root_present -> "trailing_root_present"
  | Trailing_root_rejected -> "trailing_root_rejected"
  | Uts46_mapped -> "uts46_mapped"
  | Uts46_ignored -> "uts46_ignored"
  | Uts46_deviation -> "uts46_deviation"
  | Uts46_disallowed -> "uts46_disallowed"
  | Std3_disallowed -> "std3_disallowed"
  | Ignore_invalid_punycode_applied -> "ignore_invalid_punycode_applied"
  | Idna2008_nv8 -> "idna2008_nv8"
  | Idna2008_xv8 -> "idna2008_xv8"
  | Invalid_utf8 -> "invalid_utf8"
  | Serialization_failed -> "serialization_failed"

type event = {
  severity : severity;
  stage : stage;
  code : code;
  label_index : int option;
  cp_index : int option;
  cp : int option;
  detail : string option;
}

type report = {
  policy : policy;
  operation : operation;
  input : string;
  output : string option;
  accepted : bool;
  events : event list;
}

type builder = {
  policy : policy;
  operation : operation;
  input : string;
  mutable output : string option;
  mutable accepted : bool;
  mutable events_rev : event list;
}

let make_builder policy operation input =
  { policy; operation; input; output = None; accepted = true; events_rev = [] }

let add_event ?label_index ?cp_index ?cp ?detail builder severity stage code =
  (match severity with
  | Error -> builder.accepted <- false
  | Warning | Info -> ());
  builder.events_rev <-
    { severity; stage; code; label_index; cp_index; cp; detail }
    :: builder.events_rev

let finish builder =
  {
    policy = builder.policy;
    operation = builder.operation;
    input = builder.input;
    output = builder.output;
    accepted = builder.accepted;
    events = List.rev builder.events_rev;
  }

let set_output builder output = builder.output <- Some output

let domain_ascii_labels labels =
  let rec collect acc = function
    | [] -> Ok (List.rev acc)
    | Root :: rest -> collect ("" :: acc) rest
    | Label label :: rest -> (
        match ace_of_cps label.cps with
        | Error e -> Error e
        | Ok ace -> collect (ace :: acc) rest)
  in
  collect [] labels

let error_message_for_code code cp =
  match code with
  | Empty_input -> "empty input"
  | Empty_label -> "empty label"
  | Label_not_nfc -> "label not in NFC"
  | Hyphen_start -> "label starts with hyphen"
  | Hyphen_end -> "label ends with hyphen"
  | Hyphen_3_4 -> "label has -- at positions 3-4"
  | Initial_combiner -> "label begins with combining mark"
  | Codepoint_disallowed -> (
      match cp with
      | Some c -> Printf.sprintf "codepoint U+%04X not allowed" c
      | None -> "codepoint not allowed")
  | Contexto_failed -> (
      match cp with
      | Some c -> Printf.sprintf "CONTEXTO U+%04X not valid in context" c
      | None -> "CONTEXTO not valid in context")
  | Contextj_failed -> "CONTEXTJ failure"
  | Bidi_failed -> "bidi check failed"
  | A_label_non_ascii -> "A-label must be ASCII"
  | A_label_trailing_hyphen -> "A-label ends with hyphen"
  | A_label_invalid_punycode -> "invalid punycode"
  | A_label_decodes_to_ascii -> "A-label decodes to ASCII-only label"
  | A_label_not_canonical -> "A-label not in canonical form"
  | A_label_not_lowercase_canonical ->
      "A-label must be lowercase canonical form"
  | Reserved_xn_prefix -> "label starts with xn--"
  | Std3_disallowed -> (
      match cp with
      | Some c -> Printf.sprintf "disallowed ASCII U+%04X (STD3)" c
      | None -> "disallowed ASCII (STD3)")
  | Uts46_mapped -> (
      match cp with
      | Some c -> Printf.sprintf "mapped U+%04X in label" c
      | None -> "mapped codepoint in label")
  | Uts46_ignored -> (
      match cp with
      | Some c -> Printf.sprintf "ignored U+%04X in label" c
      | None -> "ignored codepoint in label")
  | Uts46_disallowed -> (
      match cp with
      | Some c -> Printf.sprintf "disallowed U+%04X in label" c
      | None -> "disallowed codepoint in label")
  | Invalid_utf8 -> "invalid UTF-8"
  | Dns_label_too_long -> "label too long"
  | Dns_domain_too_long -> "domain too long"
  | Trailing_root_rejected -> "trailing dot (empty label)"
  | Serialization_failed -> "serialization failure"
  | Label_ascii_nr_ldh | Label_u_label | Label_a_label | Ascii_lowercased
  | Trailing_root_present | Uts46_deviation | Ignore_invalid_punycode_applied
  | Idna2008_nv8 | Idna2008_xv8 ->
      "validation failed"

let first_error_message_from_events events =
  let rec find = function
    | [] -> "validation failed"
    | (e : event) :: rest ->
        if e.severity = Error then
          match e.detail with
          | Some d -> (
              match e.code with
              | A_label_invalid_punycode -> "invalid punycode: " ^ d
              | _ -> d)
          | None -> error_message_for_code e.code e.cp
        else find rest
  in
  find events

let props_of_cp = Idna_tables.Props.get

let emit_idna2008_provenance builder ~stage ~label_index cps =
  List.iteri
    (fun cp_index cp ->
      let props = props_of_cp cp in
      if Idna_tables.Props.is_uts46_nv8 props then
        add_event ~label_index ~cp_index ~cp builder Warning stage Idna2008_nv8;
      if Idna_tables.Props.is_uts46_xv8 props then
        add_event ~label_index ~cp_index ~cp builder Warning stage Idna2008_xv8)
    cps

let emit_label_classification builder ~label_index label =
  if has_xn_prefix label then
    add_event ~label_index builder Info Label_classification Label_a_label
  else if string_is_ascii label then
    add_event ~label_index builder Info Label_classification Label_ascii_nr_ldh
  else add_event ~label_index builder Info Label_classification Label_u_label

let emit_ascii_lowercased_if_needed builder ~stage ~label_index original lowered
    =
  if original <> lowered then
    add_event ~label_index builder Info stage Ascii_lowercased

let diagnose_hyphen builder ~label_index cps =
  match Rules.check_hyphen cps with
  | Ok () -> true
  | Error Rules.Empty_label ->
      add_event ~label_index builder Error Label_split Empty_label;
      false
  | Error Rules.Hyphen_start ->
      add_event ~label_index builder Error Codepoint Hyphen_start ~cp:0x2D;
      false
  | Error Rules.Hyphen_end ->
      add_event ~label_index builder Error Codepoint Hyphen_end ~cp:0x2D;
      false
  | Error Rules.Hyphen_3_4 ->
      add_event ~label_index builder Error Codepoint Hyphen_3_4 ~cp:0x2D;
      false

let diagnose_nfc builder ~label_index cps =
  if Rules.is_nfc cps then true
  else begin
    add_event ~label_index builder Error Normalization Label_not_nfc;
    false
  end

let diagnose_initial_combiner builder ~label_index cps =
  match Rules.initial_combiner cps with
  | None -> true
  | Some cp ->
      add_event ~label_index ~cp_index:0 ~cp builder Error Codepoint
        Initial_combiner;
      false

let diagnose_codepoints ~check_contexto builder ~label_index cps =
  emit_idna2008_provenance builder ~stage:Codepoint ~label_index cps;
  match Rules.check_codepoints ~check_contexto cps with
  | Ok () -> true
  | Error (Rules.Contexto_failed { index; cp }) ->
      add_event ~label_index ~cp_index:index ~cp builder Error Context
        Contexto_failed;
      false
  | Error (Rules.Codepoint_disallowed { index; cp }) ->
      add_event ~label_index ~cp_index:index ~cp builder Error Codepoint
        Codepoint_disallowed;
      false

let diagnose_contextj builder ~label_index cps =
  match Rules.check_contextj cps with
  | Ok () -> true
  | Error { index; cp; detail } ->
      add_event ~label_index ~cp_index:index ~cp ~detail builder Error Context
        Contextj_failed;
      false

let diagnose_bidi_label builder ~label_index cps =
  match Bidi.check_bidi_label cps with
  | Ok () -> true
  | Error msg ->
      add_event ~label_index ~detail:msg builder Error Bidi Bidi_failed;
      false

let diagnose_unicode ~check_contexto ~check_bidi builder ~label_index cps =
  diagnose_nfc builder ~label_index cps
  && diagnose_hyphen builder ~label_index cps
  && diagnose_initial_combiner builder ~label_index cps
  && diagnose_codepoints ~check_contexto builder ~label_index cps
  && diagnose_contextj builder ~label_index cps
  &&
  if check_bidi && Bidi.label_has_rtl cps then
    diagnose_bidi_label builder ~label_index cps
  else true

let diagnose_codepoints_registration builder ~label_index cps =
  diagnose_codepoints ~check_contexto:true builder ~label_index cps

let diagnose_codepoints_lookup builder ~label_index cps =
  diagnose_codepoints ~check_contexto:false builder ~label_index cps

let diagnose_unicode_registration builder ~label_index cps =
  diagnose_unicode ~check_contexto:true ~check_bidi:true builder ~label_index
    cps

let diagnose_unicode_lookup builder ~label_index cps =
  diagnose_unicode ~check_contexto:false ~check_bidi:false builder ~label_index
    cps
