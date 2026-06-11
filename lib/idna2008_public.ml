(** No-diagnostics IDNA2008 registration and lookup public paths. *)

open Shared
module Rules = Idna2008_rules

let error_message ?detail code cp =
  match detail with
  | Some d -> d
  | None -> (
      match code with
      | `Empty_input -> "empty input"
      | `Empty_label -> "empty label"
      | `Label_not_nfc -> "label not in NFC"
      | `Hyphen_start -> "label starts with hyphen"
      | `Hyphen_end -> "label ends with hyphen"
      | `Hyphen_3_4 -> "label has -- at positions 3-4"
      | `Reserved_xn_prefix -> "label starts with xn--"
      | `Initial_combiner -> "label begins with combining mark"
      | `Codepoint_disallowed -> (
          match cp with
          | Some c -> Printf.sprintf "codepoint U+%04X not allowed" c
          | None -> "codepoint not allowed")
      | `Contexto_failed -> (
          match cp with
          | Some c -> Printf.sprintf "CONTEXTO U+%04X not valid in context" c
          | None -> "CONTEXTO not valid in context")
      | `Contextj_failed -> "CONTEXTJ failure"
      | `Bidi_failed -> "bidi check failed"
      | `A_label_non_ascii -> "A-label must be ASCII"
      | `A_label_trailing_hyphen -> "A-label ends with hyphen"
      | `A_label_invalid_punycode -> "invalid punycode"
      | `A_label_decodes_to_ascii -> "A-label decodes to ASCII-only label"
      | `A_label_not_canonical -> "A-label not in canonical form"
      | `A_label_not_lowercase_canonical ->
          "A-label must be lowercase canonical form"
      | `Dns_label_too_long -> "label too long"
      | `Dns_domain_too_long -> "domain too long"
      | `Trailing_root_rejected -> "trailing dot (empty label)"
      | `Invalid_utf8 -> "invalid UTF-8"
      | `Serialization_failed -> "serialization failure")

let error ?detail code = Error (error_message ?detail code None)
let error_cp ?detail code cp = Error (error_message ?detail code (Some cp))

let validate_hyphen cps =
  match Rules.check_hyphen cps with
  | Ok () -> Ok ()
  | Error Rules.Empty_label -> error `Empty_label
  | Error Rules.Hyphen_start -> error_cp `Hyphen_start 0x2D
  | Error Rules.Hyphen_end -> error_cp `Hyphen_end 0x2D
  | Error Rules.Hyphen_3_4 -> error_cp `Hyphen_3_4 0x2D

let validate_nfc cps = if Rules.is_nfc cps then Ok () else error `Label_not_nfc

let validate_initial_combiner = function
  | cps -> (
      match Rules.initial_combiner cps with
      | None -> Ok ()
      | Some cp -> error_cp `Initial_combiner cp)

let validate_codepoints ~check_contexto cps =
  match Rules.check_codepoints ~check_contexto cps with
  | Ok () -> Ok ()
  | Error (Rules.Codepoint_disallowed { cp; _ }) ->
      error_cp `Codepoint_disallowed cp
  | Error (Rules.Contexto_failed { cp; _ }) -> error_cp `Contexto_failed cp

let validate_contextj cps =
  match Rules.check_contextj cps with
  | Ok () -> Ok ()
  | Error { detail; _ } -> error ~detail `Contextj_failed

let validate_bidi_label cps =
  match Bidi.check_bidi_label cps with
  | Ok () -> Ok ()
  | Error msg -> error ~detail:msg `Bidi_failed

let validate_unicode ~check_contexto ~check_bidi cps =
  validate_nfc cps >>= fun () ->
  validate_hyphen cps >>= fun () ->
  validate_initial_combiner cps >>= fun () ->
  validate_codepoints ~check_contexto cps >>= fun () ->
  validate_contextj cps >>= fun () ->
  if check_bidi && Bidi.label_has_rtl cps then validate_bidi_label cps
  else Ok ()

let validate_unicode_registration =
  validate_unicode ~check_contexto:true ~check_bidi:true

let validate_unicode_lookup =
  validate_unicode ~check_contexto:false ~check_bidi:false

let validate_ascii_label label =
  let lower = String.lowercase_ascii label in
  let cps = List.init (String.length lower) (fun i -> Char.code lower.[i]) in
  validate_hyphen cps >>= fun () ->
  validate_codepoints ~check_contexto:true cps >>= fun () ->
  Ok { unicode = lower; cps }

let validate_utf8_label ~validate_unicode label =
  match Utf8.to_cps label with
  | Error msg -> error ~detail:msg `Invalid_utf8
  | Ok cps ->
      validate_unicode cps >>= fun () -> Ok { unicode = cps_to_utf8 cps; cps }

let validate_apparent_alabel ~allow_mixed_case ~validate_unicode label =
  let len = String.length label in
  let lower = String.lowercase_ascii label in
  if not (string_is_ascii label) then error `A_label_non_ascii
  else if lower.[len - 1] = '-' then error_cp `A_label_trailing_hyphen 0x2D
  else
    match Punycode.decode (String.sub lower 4 (len - 4)) with
    | Error msg ->
        error ~detail:("invalid punycode: " ^ msg) `A_label_invalid_punycode
    | Ok cps -> (
        if cps_are_ascii cps then error `A_label_decodes_to_ascii
        else
          validate_unicode cps >>= fun () ->
          match Punycode.encode cps with
          | Error msg ->
              error
                ~detail:("invalid punycode: " ^ msg)
                `A_label_invalid_punycode
          | Ok encoded ->
              let canonical = "xn--" ^ encoded in
              if canonical <> lower then error `A_label_not_canonical
              else if (not allow_mixed_case) && canonical <> label then
                error `A_label_not_lowercase_canonical
              else Ok { unicode = cps_to_utf8 cps; cps })

let registration_label label =
  if String.length label = 0 then error `Empty_label
  else if has_xn_prefix label then
    validate_apparent_alabel ~allow_mixed_case:false
      ~validate_unicode:validate_unicode_registration label
  else if string_is_ascii label then validate_ascii_label label
  else validate_utf8_label ~validate_unicode:validate_unicode_registration label

let lookup_label label =
  if String.length label = 0 then error `Empty_label
  else if has_xn_prefix label then
    validate_apparent_alabel ~allow_mixed_case:true
      ~validate_unicode:validate_unicode_lookup label
  else if string_is_ascii label then validate_ascii_label label
  else validate_utf8_label ~validate_unicode:validate_unicode_lookup label

let process_domain ~validate_label ~allow_trailing_root domain =
  if String.length domain = 0 then error `Empty_input
  else
    let raw_labels = String.split_on_char '.' domain in
    let last_index = List.length raw_labels - 1 in
    let rec collect idx acc = function
      | [] -> Ok (List.rev acc)
      | label :: rest ->
          if label = "" then
            if allow_trailing_root && idx = last_index then
              collect (idx + 1) (Root :: acc) rest
            else if idx = last_index then error `Trailing_root_rejected
            else error `Empty_label
          else
            validate_label label >>= fun validated ->
            collect (idx + 1) (Label validated :: acc) rest
    in
    collect 0 [] raw_labels

let serialize_unicode labels =
  String.concat "."
    (List.map (function Root -> "" | Label label -> label.unicode) labels)

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

let serialize_ascii labels =
  domain_ascii_labels labels >>= fun parts -> Ok (String.concat "." parts)

let validate_domain_bidi labels =
  if not (Bidi.labels_have_rtl labels) then Ok ()
  else
    let rec check = function
      | [] -> Ok ()
      | Root :: rest -> check rest
      | Label label :: rest ->
          validate_bidi_label label.cps >>= fun () -> check rest
    in
    check labels

let validate_dns_length labels =
  domain_ascii_labels labels >>= fun parts ->
  let has_root = match List.rev labels with Root :: _ -> true | _ -> false in
  let parts_no_root =
    if has_root then List.filter (( <> ) "") parts else parts
  in
  if parts_no_root = [] then error `Empty_input
  else
    let rec find_offending = function
      | [] -> false
      | Root :: rest -> find_offending rest
      | Label label :: rest -> (
          match ace_of_cps label.cps with
          | Ok ace ->
              String.length ace = 0
              || String.length ace > 63
              || find_offending rest
          | Error _ -> true)
    in
    if find_offending labels then error `Dns_label_too_long
    else
      let total =
        List.fold_left
          (fun acc part -> acc + String.length part)
          0 parts_no_root
        + max 0 (List.length parts_no_root - 1)
      in
      if total > 253 then error `Dns_domain_too_long else Ok ()

module Registration = struct
  let check_label label = registration_label label >>= fun _ -> Ok ()

  let to_unicode ?(flags = default_registration_hostname_flags) domain =
    process_domain ~validate_label:registration_label ~allow_trailing_root:false
      domain
    >>= fun labels ->
    (if flags.verify_dns_length then validate_dns_length labels else Ok ())
    >>= fun () -> Ok (serialize_unicode labels)

  let to_ascii ?(flags = default_registration_hostname_flags) domain =
    process_domain ~validate_label:registration_label ~allow_trailing_root:false
      domain
    >>= fun labels ->
    (if flags.verify_dns_length then validate_dns_length labels else Ok ())
    >>= fun () -> serialize_ascii labels

  let is_valid_hostname ?(flags = default_registration_hostname_flags) domain =
    match to_ascii ~flags domain with Ok _ -> true | Error _ -> false
end

module Lookup = struct
  let to_unicode ?(flags = default_lookup_flags) domain =
    process_domain ~validate_label:lookup_label ~allow_trailing_root:true domain
    >>= fun labels ->
    (if flags.check_bidi then validate_domain_bidi labels else Ok ())
    >>= fun () -> Ok (serialize_unicode labels)

  let to_ascii ?(flags = default_lookup_flags) domain =
    process_domain ~validate_label:lookup_label ~allow_trailing_root:true domain
    >>= fun labels ->
    (if flags.check_bidi then validate_domain_bidi labels else Ok ())
    >>= fun () -> serialize_ascii labels
end
