(** UTS#46-specific diagnostic rules and orchestration. *)

open Shared
open Diagnostics_core

type uts46_label = { idx : int; utf8 : string; cps : int list }

let emit_uts46_mapping builder cp props status =
  if Idna_tables.Props.is_uts46_nv8 props then
    add_event ~cp builder Warning Mapping Idna2008_nv8;
  if Idna_tables.Props.is_uts46_xv8 props then
    add_event ~cp builder Warning Mapping Idna2008_xv8;
  match status with
  | `Map _ -> add_event ~cp builder Info Mapping Uts46_mapped
  | `Ignored -> add_event ~cp builder Info Mapping Uts46_ignored
  | `Deviation -> add_event ~cp builder Warning Mapping Uts46_deviation
  | `Valid | `Disallowed -> ()

let rec append_map_span_rev offset stop acc =
  if offset = stop then acc
  else
    append_map_span_rev (offset + 1) stop
      (Idna_tables.uts46_map_data.(offset) :: acc)

let append_map_payload_rev payload acc =
  match payload with
  | Map_one cp -> cp :: acc
  | Map_span (offset, length) ->
      append_map_span_rev offset (offset + length) acc

let uts46_map_diag builder cps =
  let acc =
    List.fold_left
      (fun acc cp ->
        let props = Idna_tables.Props.get cp in
        let status = uts46_status_of_props cp props in
        emit_uts46_mapping builder cp props status;
        match status with
        | `Map payload -> append_map_payload_rev payload acc
        | `Ignored -> acc
        | `Valid | `Deviation | `Disallowed -> cp :: acc)
      [] cps
  in
  List.rev acc

let diagnose_uts46_label builder ~flags ~label_index cps =
  emit_idna2008_provenance builder ~stage:Codepoint ~label_index cps;
  diagnose_nfc builder ~label_index cps
  && (if flags.check_hyphens then diagnose_hyphen builder ~label_index cps
      else
        match cps with
        | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ ->
            add_event ~label_index builder Error A_label Reserved_xn_prefix;
            false
        | _ -> true)
  && diagnose_initial_combiner builder ~label_index cps
  && (let rec check i = function
        | [] -> true
        | cp :: rest -> (
            let std3_ok =
              (not flags.use_std3_ascii_rules)
              || cp >= 0x80
              || (cp >= 0x61 && cp <= 0x7A)
              || (cp >= 0x30 && cp <= 0x39)
              || cp = 0x2D
            in
            if not std3_ok then begin
              add_event ~label_index ~cp_index:i ~cp builder Error Codepoint
                Std3_disallowed;
              false
            end
            else
              let props = Idna_tables.Props.get cp in
              match uts46_status_of_props cp props with
              | `Valid | `Deviation -> check (i + 1) rest
              | `Map _ ->
                  add_event ~label_index ~cp_index:i ~cp builder Error Codepoint
                    Uts46_mapped;
                  false
              | `Ignored ->
                  add_event ~label_index ~cp_index:i ~cp builder Error Codepoint
                    Uts46_ignored;
                  false
              | `Disallowed ->
                  add_event ~label_index ~cp_index:i ~cp builder Error Codepoint
                    Uts46_disallowed;
                  false)
      in
      check 0 cps)
  && (if flags.check_joiners then diagnose_contextj builder ~label_index cps
      else true)
  &&
  if flags.check_bidi then
    if Bidi.label_has_rtl cps then diagnose_bidi_label builder ~label_index cps
    else true
  else true

let diagnose_uts46_process builder ~flags domain =
  let labels =
    match Utf8.to_cps domain with
    | Error _ ->
        add_event builder Error Utf8_decode Invalid_utf8;
        let lowered = lowercase_ascii_bytes domain in
        String.split_on_char '.' lowered
        |> List.mapi (fun idx label ->
            if label = "" then
              add_event ~label_index:idx builder Info Label_split
                Trailing_root_present;
            if label <> "" then
              emit_label_classification builder ~label_index:idx label;
            if string_is_ascii label then
              {
                idx;
                utf8 = label;
                cps =
                  List.init (String.length label) (fun i -> Char.code label.[i]);
              }
            else { idx; utf8 = label; cps = [] })
    | Ok input_cps ->
        let mapped = uts46_map_diag builder input_cps in
        let normalized = Nfc.nfc mapped in
        let label_cps_list = split_on_dots normalized in
        List.mapi
          (fun idx label_cps ->
            if label_cps = [] then begin
              add_event ~label_index:idx builder Info Label_split
                Trailing_root_present;
              { idx; utf8 = ""; cps = [] }
            end
            else
              let label_utf8 = cps_to_utf8 label_cps in
              let is_xn =
                match label_cps with
                | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ -> true
                | _ -> false
              in
              emit_label_classification builder ~label_index:idx label_utf8;
              if is_xn then begin
                if not flags.check_hyphens then
                  add_event ~label_index:idx builder Error A_label
                    Reserved_xn_prefix;
                if not (cps_are_ascii label_cps) then begin
                  add_event ~label_index:idx builder Error A_label
                    A_label_non_ascii;
                  { idx; utf8 = label_utf8; cps = label_cps }
                end
                else
                  let encoded =
                    String.sub label_utf8 4 (String.length label_utf8 - 4)
                  in
                  if encoded = "" then begin
                    add_event ~label_index:idx builder Error A_label
                      A_label_invalid_punycode;
                    { idx; utf8 = ""; cps = [] }
                  end
                  else
                    match Punycode.decode encoded with
                    | Error msg ->
                        if flags.ignore_invalid_punycode then begin
                          add_event ~label_index:idx ~detail:msg builder Warning
                            A_label Ignore_invalid_punycode_applied;
                          { idx; utf8 = label_utf8; cps = label_cps }
                        end
                        else begin
                          add_event ~label_index:idx ~detail:msg builder Error
                            A_label A_label_invalid_punycode;
                          { idx; utf8 = label_utf8; cps = label_cps }
                        end
                    | Ok decoded_cps ->
                        if decoded_cps = [] then begin
                          add_event ~label_index:idx builder Error A_label
                            A_label_invalid_punycode;
                          { idx; utf8 = label_utf8; cps = label_cps }
                        end
                        else
                          let u_label = cps_to_utf8 decoded_cps in
                          if cps_are_ascii decoded_cps then
                            add_event ~label_index:idx builder Error A_label
                              A_label_decodes_to_ascii;
                          ignore
                            (diagnose_uts46_label builder ~flags
                               ~label_index:idx decoded_cps);
                          { idx; utf8 = u_label; cps = decoded_cps }
              end
              else begin
                ignore
                  (diagnose_uts46_label builder ~flags ~label_index:idx
                     label_cps);
                { idx; utf8 = label_utf8; cps = label_cps }
              end)
          label_cps_list
  in
  let n = List.length labels in
  List.iter
    (fun label ->
      if label.cps = [] && label.idx <> n - 1 then
        add_event ~label_index:label.idx builder Error Label_split Empty_label)
    labels;
  if flags.check_bidi then begin
    let all_cps =
      List.filter
        (fun cps -> cps <> [])
        (List.map (fun label -> label.cps) labels)
    in
    if List.exists Bidi.label_has_rtl all_cps then
      List.iter
        (fun label ->
          if label.cps <> [] then
            ignore
              (diagnose_bidi_label builder ~label_index:label.idx label.cps))
        labels
  end;
  labels
