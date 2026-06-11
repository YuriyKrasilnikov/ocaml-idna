(** Public diagnostic surface: label/domain adapters and per-policy modules. *)

open Shared
include Diagnostics_core

let diagnose_ascii_label builder ~label_index label =
  let lower = String.lowercase_ascii label in
  emit_label_classification builder ~label_index label;
  emit_ascii_lowercased_if_needed builder ~stage:Label_classification
    ~label_index label lower;
  let cps = List.init (String.length lower) (fun i -> Char.code lower.[i]) in
  if
    diagnose_hyphen builder ~label_index cps
    && diagnose_codepoints_registration builder ~label_index cps
  then Some { unicode = lower; cps }
  else None

let diagnose_utf8_label builder ~label_index ~validate_unicode label =
  emit_label_classification builder ~label_index label;
  match Utf8.to_cps label with
  | Error msg ->
      add_event ~label_index ~detail:msg builder Error Utf8_decode Invalid_utf8;
      None
  | Ok cps ->
      if validate_unicode builder ~label_index cps then
        Some { unicode = cps_to_utf8 cps; cps }
      else None

let diagnose_apparent_alabel builder ~label_index ~allow_mixed_case
    ~validate_unicode label =
  let len = String.length label in
  let lower = String.lowercase_ascii label in
  emit_label_classification builder ~label_index label;
  if allow_mixed_case then
    emit_ascii_lowercased_if_needed builder ~stage:A_label ~label_index label
      lower;
  if not (string_is_ascii label) then begin
    add_event ~label_index builder Error A_label A_label_non_ascii;
    None
  end
  else if lower.[len - 1] = '-' then begin
    add_event ~label_index ~cp:0x2D builder Error A_label
      A_label_trailing_hyphen;
    None
  end
  else
    match Punycode.decode (String.sub lower 4 (len - 4)) with
    | Error msg ->
        add_event ~label_index ~detail:msg builder Error A_label
          A_label_invalid_punycode;
        None
    | Ok cps -> (
        if cps_are_ascii cps then begin
          add_event ~label_index builder Error A_label A_label_decodes_to_ascii;
          None
        end
        else if not (validate_unicode builder ~label_index cps) then None
        else
          match Punycode.encode cps with
          | Error msg ->
              add_event ~label_index ~detail:msg builder Error A_label
                A_label_invalid_punycode;
              None
          | Ok encoded ->
              let canonical = "xn--" ^ encoded in
              if canonical <> lower then begin
                add_event ~label_index builder Error A_label
                  A_label_not_canonical;
                None
              end
              else if (not allow_mixed_case) && canonical <> label then begin
                add_event ~label_index builder Error A_label
                  A_label_not_lowercase_canonical;
                None
              end
              else Some { unicode = cps_to_utf8 cps; cps })

let diagnose_registration_label builder ~label_index label =
  if String.length label = 0 then begin
    add_event ~label_index builder Error Input Empty_label;
    None
  end
  else if has_xn_prefix label then
    diagnose_apparent_alabel builder ~label_index ~allow_mixed_case:false
      ~validate_unicode:diagnose_unicode_registration label
  else if string_is_ascii label then
    diagnose_ascii_label builder ~label_index label
  else
    diagnose_utf8_label builder ~label_index
      ~validate_unicode:diagnose_unicode_registration label

let diagnose_lookup_label builder ~label_index label =
  if String.length label = 0 then begin
    add_event ~label_index builder Error Input Empty_label;
    None
  end
  else if has_xn_prefix label then
    diagnose_apparent_alabel builder ~label_index ~allow_mixed_case:true
      ~validate_unicode:diagnose_unicode_lookup label
  else if string_is_ascii label then
    diagnose_ascii_label builder ~label_index label
  else
    diagnose_utf8_label builder ~label_index
      ~validate_unicode:diagnose_unicode_lookup label

let diagnose_domain_bidi builder labels =
  if not (Bidi.labels_have_rtl labels) then true
  else
    let rec check idx = function
      | [] -> true
      | Root :: rest -> check (idx + 1) rest
      | Label label :: rest ->
          if diagnose_bidi_label builder ~label_index:idx label.cps then
            check (idx + 1) rest
          else false
    in
    check 0 labels

let diagnose_dns_length builder labels =
  match domain_ascii_labels labels with
  | Error msg ->
      add_event ~detail:msg builder Error Serialization Serialization_failed;
      false
  | Ok parts -> (
      let has_root =
        match List.rev labels with Root :: _ -> true | _ -> false
      in
      let parts_no_root =
        if has_root then List.filter (fun s -> s <> "") parts else parts
      in
      if parts_no_root = [] then begin
        add_event builder Error Dns_length Empty_input;
        false
      end
      else
        let offending_label =
          let rec find idx = function
            | [] -> None
            | Root :: rest -> find (idx + 1) rest
            | Label label :: rest -> (
                match ace_of_cps label.cps with
                | Ok ace when String.length ace = 0 || String.length ace > 63 ->
                    Some idx
                | Ok _ -> find (idx + 1) rest
                | Error _ -> Some idx)
          in
          find 0 labels
        in
        match offending_label with
        | Some idx ->
            add_event ~label_index:idx builder Error Dns_length
              Dns_label_too_long;
            false
        | None ->
            let total =
              List.fold_left
                (fun acc part -> acc + String.length part)
                0 parts_no_root
              + max 0 (List.length parts_no_root - 1)
            in
            if total > 253 then begin
              add_event builder Error Dns_length Dns_domain_too_long;
              false
            end
            else true)

let serialize_ascii_diag builder labels =
  match domain_ascii_labels labels with
  | Error msg ->
      add_event ~detail:msg builder Error Serialization Serialization_failed;
      None
  | Ok parts -> Some (String.concat "." parts)

let serialize_unicode_diag labels =
  String.concat "."
    (List.map (function Root -> "" | Label label -> label.unicode) labels)

let diagnose_process_domain builder ~validate_label ~allow_trailing_root
    ~check_domain_bidi ~verify_dns_length_flag domain =
  if String.length domain = 0 then begin
    add_event builder Error Input Empty_input;
    None
  end
  else
    let raw_labels = String.split_on_char '.' domain in
    let last_index = List.length raw_labels - 1 in
    let rec collect idx acc = function
      | [] -> Some (List.rev acc)
      | label :: rest -> (
          if label = "" then
            if allow_trailing_root && idx = last_index then begin
              add_event ~label_index:idx builder Info Label_split
                Trailing_root_present;
              collect (idx + 1) (Root :: acc) rest
            end
            else begin
              let code =
                if idx = last_index then Trailing_root_rejected else Empty_label
              in
              add_event ~label_index:idx builder Error Label_split code;
              None
            end
          else
            match validate_label builder ~label_index:idx label with
            | Some validated -> collect (idx + 1) (Label validated :: acc) rest
            | None -> None)
    in
    match collect 0 [] raw_labels with
    | None -> None
    | Some labels ->
        if
          check_domain_bidi builder labels
          && ((not verify_dns_length_flag) || diagnose_dns_length builder labels)
        then Some labels
        else None

module Registration = struct
  let check_label label =
    let builder = make_builder `Registration `Check_label label in
    ignore (diagnose_registration_label builder ~label_index:0 label);
    finish builder

  let to_unicode ?(flags = default_registration_hostname_flags) domain =
    let builder = make_builder `Registration `To_unicode domain in
    match
      diagnose_process_domain builder
        ~validate_label:diagnose_registration_label ~allow_trailing_root:false
        ~check_domain_bidi:(fun _ _ -> true)
        ~verify_dns_length_flag:flags.verify_dns_length domain
    with
    | Some labels ->
        set_output builder (serialize_unicode_diag labels);
        finish builder
    | None -> finish builder

  let to_ascii ?(flags = default_registration_hostname_flags) domain =
    let builder = make_builder `Registration `To_ascii domain in
    match
      diagnose_process_domain builder
        ~validate_label:diagnose_registration_label ~allow_trailing_root:false
        ~check_domain_bidi:(fun _ _ -> true)
        ~verify_dns_length_flag:flags.verify_dns_length domain
    with
    | Some labels ->
        (match serialize_ascii_diag builder labels with
        | Some output -> set_output builder output
        | None -> ());
        finish builder
    | None -> finish builder

  let is_valid_hostname ?(flags = default_registration_hostname_flags) domain =
    let builder = make_builder `Registration `Is_valid_hostname domain in
    match
      diagnose_process_domain builder
        ~validate_label:diagnose_registration_label ~allow_trailing_root:false
        ~check_domain_bidi:(fun _ _ -> true)
        ~verify_dns_length_flag:flags.verify_dns_length domain
    with
    | Some labels ->
        (match serialize_ascii_diag builder labels with
        | Some output -> set_output builder output
        | None -> ());
        finish builder
    | None -> finish builder
end

module Lookup = struct
  let to_unicode ?(flags = default_lookup_flags) domain =
    let builder = make_builder `Lookup `To_unicode domain in
    let check_domain_bidi builder labels =
      if flags.check_bidi then diagnose_domain_bidi builder labels else true
    in
    match
      diagnose_process_domain builder ~validate_label:diagnose_lookup_label
        ~allow_trailing_root:true ~check_domain_bidi
        ~verify_dns_length_flag:false domain
    with
    | Some labels ->
        set_output builder (serialize_unicode_diag labels);
        finish builder
    | None -> finish builder

  let to_ascii ?(flags = default_lookup_flags) domain =
    let builder = make_builder `Lookup `To_ascii domain in
    let check_domain_bidi builder labels =
      if flags.check_bidi then diagnose_domain_bidi builder labels else true
    in
    match
      diagnose_process_domain builder ~validate_label:diagnose_lookup_label
        ~allow_trailing_root:true ~check_domain_bidi
        ~verify_dns_length_flag:false domain
    with
    | Some labels ->
        (match serialize_ascii_diag builder labels with
        | Some output -> set_output builder output
        | None -> ());
        finish builder
    | None -> finish builder
end

module Uts46 = struct
  let to_unicode ?(flags = default_uts46_flags) domain =
    let builder = make_builder `Uts46 `To_unicode domain in
    if String.length domain = 0 then begin
      add_event builder Error Input Empty_input;
      set_output builder "";
      finish builder
    end
    else
      let labels =
        Diagnostics_uts46.diagnose_uts46_process builder ~flags domain
      in
      let output =
        String.concat "."
          (List.map (fun (l : Diagnostics_uts46.uts46_label) -> l.utf8) labels)
      in
      set_output builder output;
      finish builder

  let to_ascii ?(flags = default_uts46_flags) domain =
    let builder = make_builder `Uts46 `To_ascii domain in
    if String.length domain = 0 then begin
      add_event builder Error Input Empty_input;
      finish builder
    end
    else
      let labels =
        Diagnostics_uts46.diagnose_uts46_process builder ~flags domain
      in
      if not builder.accepted then finish builder
      else
        let parts =
          List.map
            (fun (l : Diagnostics_uts46.uts46_label) ->
              if cps_are_ascii l.cps || l.cps = [] then Ok (l.idx, l.utf8)
              else
                match Punycode.encode l.cps with
                | Error msg -> Error (l.idx, msg)
                | Ok encoded -> Ok (l.idx, "xn--" ^ encoded))
            labels
        in
        let rec collect acc = function
          | [] -> Some (List.rev acc)
          | Ok part :: rest -> collect (part :: acc) rest
          | Error (idx, msg) :: _ ->
              add_event ~label_index:idx ~detail:msg builder Error Serialization
                Serialization_failed;
              None
        in
        match collect [] parts with
        | None -> finish builder
        | Some parts ->
            if
              flags.verify_dns_length && parts <> []
              && snd (List.hd (List.rev parts)) = ""
            then begin
              let idx = fst (List.hd (List.rev parts)) in
              add_event ~label_index:idx builder Error Dns_length
                Trailing_root_rejected;
              finish builder
            end
            else
              let strings = List.map snd parts in
              let result = String.concat "." strings in
              let too_long =
                List.find_opt (fun (_, part) -> String.length part > 63) parts
              in
              (match too_long with
              | Some (idx, _) ->
                  if flags.verify_dns_length then
                    add_event ~label_index:idx builder Error Dns_length
                      Dns_label_too_long
              | None -> ());
              if flags.verify_dns_length && String.length result > 253 then
                add_event builder Error Dns_length Dns_domain_too_long;
              if builder.accepted then set_output builder result;
              finish builder
end
