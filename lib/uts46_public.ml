(** No-diagnostics UTS #46 public path.

    This mirrors [Diagnostics.Uts46] acceptance/output semantics for the plain
    public API without allocating diagnostics events. Diagnostics remains the
    explainability oracle. *)

open Shared

type label = { idx : int; utf8 : string; cps : int list }
type process_result = { labels : label list; errored : bool }

let set_error errored = errored := true

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

let uts46_map cps =
  let acc =
    List.fold_left
      (fun acc cp ->
        let props = Idna_tables.Props.get cp in
        match uts46_status_of_props cp props with
        | `Map payload -> append_map_payload_rev payload acc
        | `Ignored -> acc
        | `Valid | `Deviation | `Disallowed -> cp :: acc)
      [] cps
  in
  List.rev acc

let uts46_map_utf8 domain =
  let len = String.length domain in
  let rec loop i acc =
    if i >= len then Ok (List.rev acc)
    else
      let d = String.get_utf_8_uchar domain i in
      if Uchar.utf_decode_is_valid d then
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        let props = Idna_tables.Props.get cp in
        let acc =
          match uts46_status_of_props cp props with
          | `Map payload -> append_map_payload_rev payload acc
          | `Ignored -> acc
          | `Valid | `Deviation | `Disallowed -> cp :: acc
        in
        loop (i + Uchar.utf_decode_length d) acc
      else Error "invalid UTF-8"
  in
  loop 0 []

let validate_status_cp ~flags errored index cp =
  let props = Idna_tables.Props.get cp in
  if index = 0 && Idna_tables.Props.is_mark props then set_error errored;
  let std3_ok =
    (not flags.use_std3_ascii_rules)
    || cp >= 0x80
    || (cp >= 0x61 && cp <= 0x7A)
    || (cp >= 0x30 && cp <= 0x39)
    || cp = 0x2D
  in
  if not std3_ok then set_error errored
  else
    match uts46_status_of_props cp props with
    | `Valid | `Deviation -> ()
    | `Map _ | `Ignored | `Disallowed -> set_error errored

let rec validate_contextj_loop errored summary i = function
  | [] -> ()
  | cp :: rest ->
      if Contextj.is_contextj_cp cp && not (Contextj.valid_at summary i cp) then
        set_error errored;
      validate_contextj_loop errored summary (i + 1) rest

let validate_contextj errored cps =
  if Contextj.has_contextj cps then
    validate_contextj_loop errored (Contextj.make_summary cps) 0 cps

let rec validate_label_codepoints ~flags errored index = function
  | [] -> ()
  | [ cp ] ->
      validate_status_cp ~flags errored index cp;
      if flags.check_hyphens && cp = 0x2D then set_error errored
  | cp :: rest ->
      validate_status_cp ~flags errored index cp;
      validate_label_codepoints ~flags errored (index + 1) rest

let validate_label_core ~flags errored cps =
  if cps = [] then set_error errored
  else if flags.check_hyphens then
    (* Public mode only needs [errored=true]. Diagnostics keeps full event order. *)
    begin match cps with
    | 0x2D :: _ -> set_error errored
    | _ :: _ :: 0x2D :: 0x2D :: _ -> set_error errored
    | _ ->
        ();
        validate_label_codepoints ~flags errored 0 cps
    end
  else begin
    (match cps with
    | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ -> set_error errored
    | _ -> ());
    validate_label_codepoints ~flags errored 0 cps
  end

let validate_label_after_nfc ~flags errored cps =
  validate_label_core ~flags errored cps;
  if flags.check_joiners then validate_contextj errored cps

let validate_label ~flags errored cps =
  if (not (Nfc.is_nfc_qc cps)) && Nfc.nfc cps <> cps then set_error errored;
  validate_label_after_nfc ~flags errored cps

let ascii_label_of_bytes idx label =
  {
    idx;
    utf8 = label;
    cps = List.init (String.length label) (fun i -> Char.code label.[i]);
  }

let invalid_utf8_labels domain =
  let lowered = lowercase_ascii_bytes domain in
  String.split_on_char '.' lowered
  |> List.mapi (fun idx label ->
      if string_is_ascii label then ascii_label_of_bytes idx label
      else { idx; utf8 = label; cps = [] })

let decode_alabel ~flags errored idx label_utf8 label_cps encoded =
  if not flags.check_hyphens then set_error errored;
  if not (cps_are_ascii label_cps) then begin
    set_error errored;
    { idx; utf8 = label_utf8; cps = label_cps }
  end
  else if encoded = "" then begin
    set_error errored;
    { idx; utf8 = ""; cps = [] }
  end
  else
    match Punycode.decode encoded with
    | Error _ ->
        if not flags.ignore_invalid_punycode then set_error errored;
        { idx; utf8 = label_utf8; cps = label_cps }
    | Ok decoded_cps ->
        if decoded_cps = [] then begin
          set_error errored;
          { idx; utf8 = label_utf8; cps = label_cps }
        end
        else begin
          let u_label = cps_to_utf8 decoded_cps in
          if cps_are_ascii decoded_cps then set_error errored;
          validate_label ~flags errored decoded_cps;
          { idx; utf8 = u_label; cps = decoded_cps }
        end

let process_label ~flags errored idx label_cps =
  if label_cps = [] then { idx; utf8 = ""; cps = [] }
  else
    let label_utf8 = cps_to_utf8 label_cps in
    match label_cps with
    | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ ->
        let encoded = String.sub label_utf8 4 (String.length label_utf8 - 4) in
        decode_alabel ~flags errored idx label_utf8 label_cps encoded
    | _ ->
        validate_label_after_nfc ~flags errored label_cps;
        { idx; utf8 = label_utf8; cps = label_cps }

let process_label_for_ascii ~flags errored idx label_cps =
  if label_cps = [] then { idx; utf8 = ""; cps = [] }
  else
    match label_cps with
    | 0x78 :: 0x6E :: 0x2D :: 0x2D :: _ ->
        process_label ~flags errored idx label_cps
    | _ ->
        validate_label_after_nfc ~flags errored label_cps;
        if cps_are_ascii label_cps then
          { idx; utf8 = cps_to_utf8 label_cps; cps = label_cps }
        else { idx; utf8 = ""; cps = label_cps }

let validate_empty_labels errored labels =
  let last_idx = List.length labels - 1 in
  List.iter
    (fun label ->
      if label.cps = [] && label.idx <> last_idx then set_error errored)
    labels

let rec labels_have_rtl = function
  | [] -> false
  | label :: rest ->
      (label.cps <> [] && Bidi.label_has_rtl label.cps) || labels_have_rtl rest

let rec validate_bidi_labels errored = function
  | [] -> ()
  | label :: rest ->
      if label.cps <> [] then
        begin match Bidi.check_bidi_label label.cps with
        | Ok () -> ()
        | Error _ -> set_error errored
        end;
      validate_bidi_labels errored rest

let validate_domain_bidi ~flags errored labels =
  if flags.check_bidi then
    if labels_have_rtl labels then validate_bidi_labels errored labels

let ascii_lower_code c = if c >= 0x41 && c <= 0x5A then c + 0x20 else c

let ascii_is_ldh c =
  (c >= 0x61 && c <= 0x7A) || (c >= 0x30 && c <= 0x39) || c = 0x2D

let ascii_label_has_xn_prefix_bytes s start len =
  len >= 4
  && ascii_lower_code (Char.code s.[start]) = 0x78
  && ascii_lower_code (Char.code s.[start + 1]) = 0x6E
  && s.[start + 2] = '-'
  && s.[start + 3] = '-'

let ascii_lower_substring s start len =
  let bytes = Bytes.create len in
  for i = 0 to len - 1 do
    Bytes.unsafe_set bytes i
      (Char.unsafe_chr (ascii_lower_code (Char.code s.[start + i])))
  done;
  Bytes.unsafe_to_string bytes

let buffer_add_lower_ascii_span b s start stop =
  for i = start to stop - 1 do
    Buffer.add_char b (Char.unsafe_chr (ascii_lower_code (Char.code s.[i])))
  done

let validate_ascii_ldh_label_span ~flags errored domain start stop =
  if flags.check_hyphens then
    let len = stop - start in
    if domain.[start] = '-' then set_error errored
    else if len >= 4 && domain.[start + 2] = '-' && domain.[start + 3] = '-'
    then set_error errored
    else if domain.[stop - 1] = '-' then set_error errored

exception Ascii_alabel_fast_fallback

let to_unicode_ascii_alabel_fast ~flags domain =
  let len = String.length domain in
  let output = Buffer.create len in
  let errored = ref false in
  let saw_rtl = ref false in
  let bidi_labels_rev = ref [] in
  let ascii_label_cps start stop =
    let rec loop i acc =
      if i < start then acc
      else loop (i - 1) (ascii_lower_code (Char.code domain.[i]) :: acc)
    in
    loop (stop - 1) []
  in
  let record_bidi_label cps =
    if flags.check_bidi then begin
      if Bidi.label_has_rtl cps then saw_rtl := true;
      bidi_labels_rev := cps :: !bidi_labels_rev
    end
  in
  let append_ascii_label start stop =
    validate_ascii_ldh_label_span ~flags errored domain start stop;
    buffer_add_lower_ascii_span output domain start stop;
    record_bidi_label (ascii_label_cps start stop)
  in
  let append_alabel start stop =
    let label_len = stop - start in
    let encoded_len = label_len - 4 in
    let encoded = ascii_lower_substring domain (start + 4) encoded_len in
    if not flags.check_hyphens then set_error errored;
    if encoded = "" then set_error errored
    else
      match Punycode.decode encoded with
      | Error _ ->
          if not flags.ignore_invalid_punycode then set_error errored;
          Buffer.add_string output "xn--";
          Buffer.add_string output encoded;
          record_bidi_label (ascii_label_cps start stop)
      | Ok [] ->
          set_error errored;
          Buffer.add_string output "xn--";
          Buffer.add_string output encoded;
          record_bidi_label (ascii_label_cps start stop)
      | Ok decoded_cps ->
          if cps_are_ascii decoded_cps then set_error errored;
          validate_label ~flags errored decoded_cps;
          record_bidi_label decoded_cps;
          Buffer.add_string output (cps_to_utf8 decoded_cps)
  in
  let validate_bidi () =
    if flags.check_bidi && !saw_rtl then
      List.iter
        (fun cps ->
          if cps <> [] then
            match Bidi.check_bidi_label cps with
            | Ok () -> ()
            | Error _ -> set_error errored)
        (List.rev !bidi_labels_rev)
  in
  let rec loop idx start i =
    if i = len || domain.[i] = '.' then begin
      let label_len = i - start in
      if label_len = 0 then raise Ascii_alabel_fast_fallback;
      if idx > 0 then Buffer.add_char output '.';
      if ascii_label_has_xn_prefix_bytes domain start label_len then
        append_alabel start i
      else append_ascii_label start i;
      if i < len then loop (idx + 1) (i + 1) (i + 1)
    end
    else loop idx start (i + 1)
  in
  try
    loop 0 0 0;
    validate_bidi ();
    Some { value = Buffer.contents output; errored = !errored }
  with Ascii_alabel_fast_fallback -> None

let ascii_to_ascii_fast_flags_supported flags =
  flags.check_hyphens && flags.check_bidi && flags.check_joiners
  && flags.use_std3_ascii_rules && flags.verify_dns_length
  && not flags.ignore_invalid_punycode

let ascii_label_cps domain start stop =
  let rec loop i acc =
    if i < start then acc
    else loop (i - 1) (ascii_lower_code (Char.code domain.[i]) :: acc)
  in
  loop (stop - 1) []

let ascii_span_ldh domain start stop =
  let ok = ref true in
  for i = start to stop - 1 do
    if not (ascii_is_ldh (ascii_lower_code (Char.code domain.[i]))) then
      ok := false
  done;
  !ok

let append_canonical_alabel output cps =
  match Punycode.encode cps with
  | Error e -> Error e
  | Ok encoded ->
      Buffer.add_string output "xn--";
      Buffer.add_string output encoded;
      Ok (4 + String.length encoded)

let string_is_ascii_bytes s =
  let len = String.length s in
  let rec loop i = i = len || (Char.code s.[i] < 0x80 && loop (i + 1)) in
  loop 0

let to_ascii_ascii_alabel_fast_ascii ~flags domain =
  if not (ascii_to_ascii_fast_flags_supported flags) then None
  else
    let len = String.length domain in
    let output = Buffer.create len in
    let errored = ref false in
    let saw_rtl = ref false in
    let bidi_labels_rev = ref [] in
    let label_count = ref 0 in
    let last_part_empty = ref false in
    let any_part_too_long = ref false in
    let encode_error = ref None in
    let record_label cps =
      if flags.check_bidi then begin
        if Bidi.label_has_rtl cps then saw_rtl := true;
        bidi_labels_rev := cps :: !bidi_labels_rev
      end
    in
    let append_part_len part_len =
      if part_len > 63 then any_part_too_long := true;
      last_part_empty := false
    in
    let finish_label ~is_final start stop =
      if !label_count > 0 then Buffer.add_char output '.';
      incr label_count;
      let label_len = stop - start in
      if label_len = 0 then begin
        if is_final then last_part_empty := true else set_error errored;
        record_label []
      end
      else if !errored then begin
        buffer_add_lower_ascii_span output domain start stop;
        append_part_len label_len
      end
      else if ascii_label_has_xn_prefix_bytes domain start label_len then begin
        let encoded =
          ascii_lower_substring domain (start + 4) (label_len - 4)
        in
        if encoded = "" then begin
          set_error errored;
          Buffer.add_string output "xn--";
          record_label (ascii_label_cps domain start stop);
          append_part_len label_len
        end
        else
          match Punycode.decode encoded with
          | Error _ ->
              set_error errored;
              buffer_add_lower_ascii_span output domain start stop;
              record_label (ascii_label_cps domain start stop);
              append_part_len label_len
          | Ok [] ->
              set_error errored;
              buffer_add_lower_ascii_span output domain start stop;
              append_part_len label_len
          | Ok decoded_cps ->
              if cps_are_ascii decoded_cps then set_error errored;
              validate_label ~flags errored decoded_cps;
              if
                (not !errored) && flags.check_bidi
                && Bidi.label_has_rtl decoded_cps
              then
                begin match Bidi.check_bidi_label decoded_cps with
                | Ok () -> ()
                | Error _ -> set_error errored
                end;
              if !errored then begin
                buffer_add_lower_ascii_span output domain start stop;
                append_part_len label_len
              end
              else begin
                record_label decoded_cps;
                match append_canonical_alabel output decoded_cps with
                | Ok part_len -> append_part_len part_len
                | Error e ->
                    encode_error := Some e;
                    append_part_len 0
              end
      end
      else begin
        if not (ascii_span_ldh domain start stop) then set_error errored;
        validate_ascii_ldh_label_span ~flags errored domain start stop;
        buffer_add_lower_ascii_span output domain start stop;
        record_label (ascii_label_cps domain start stop);
        append_part_len label_len
      end
    in
    let validate_bidi () =
      if flags.check_bidi && !saw_rtl then
        List.iter
          (fun cps ->
            if cps <> [] then
              match Bidi.check_bidi_label cps with
              | Ok () -> ()
              | Error _ -> set_error errored)
          (List.rev !bidi_labels_rev)
    in
    let rec loop start i =
      if i = len then begin
        finish_label ~is_final:true start i;
        `Done
      end
      else
        let c = Char.code domain.[i] in
        if c >= 0x80 then `Fallback
        else if c = 0x2E then begin
          finish_label ~is_final:false start i;
          loop (i + 1) (i + 1)
        end
        else loop start (i + 1)
    in
    if len = 0 then Some (Error "validation failed")
    else
      match loop 0 0 with
      | `Fallback -> None
      | `Done -> (
          if not !errored then validate_bidi ();
          match !encode_error with
          | Some e -> Some (Error e)
          | None ->
              if !errored then Some (Error "validation failed")
              else if !last_part_empty then
                Some (Error "trailing dot (empty label)")
              else if !any_part_too_long then Some (Error "label too long")
              else
                let value = Buffer.contents output in
                if String.length value > 253 then Some (Error "domain too long")
                else Some (Ok value))

let to_ascii_ascii_alabel_fast ~flags domain =
  if not (string_is_ascii_bytes domain) then None
  else to_ascii_ascii_alabel_fast_ascii ~flags domain

let process ~flags domain =
  let errored = ref false in
  if String.length domain = 0 then set_error errored;
  let labels =
    match uts46_map_utf8 domain with
    | Error _ ->
        set_error errored;
        invalid_utf8_labels domain
    | Ok mapped ->
        let normalized = Nfc.nfc mapped in
        split_on_dots normalized |> List.mapi (process_label ~flags errored)
  in
  validate_empty_labels errored labels;
  validate_domain_bidi ~flags errored labels;
  { labels; errored = !errored }

let process_for_ascii ~flags domain =
  let errored = ref false in
  if String.length domain = 0 then set_error errored;
  let labels =
    match uts46_map_utf8 domain with
    | Error _ ->
        set_error errored;
        invalid_utf8_labels domain
    | Ok mapped ->
        let normalized = Nfc.nfc mapped in
        split_on_dots normalized
        |> List.mapi (process_label_for_ascii ~flags errored)
  in
  validate_empty_labels errored labels;
  validate_domain_bidi ~flags errored labels;
  { labels; errored = !errored }

let unicode_of_labels labels =
  String.concat "." (List.map (fun label -> label.utf8) labels)

let to_unicode ~flags domain =
  let processed = process ~flags domain in
  { value = unicode_of_labels processed.labels; errored = processed.errored }

let ascii_part label =
  if cps_are_ascii label.cps || label.cps = [] then Ok label.utf8
  else
    match Punycode.encode label.cps with
    | Error e -> Error e
    | Ok encoded -> Ok ("xn--" ^ encoded)

type ascii_parts = {
  parts_rev : string list;
  last_part : string option;
  has_long_label : bool;
}

let collect_ascii_parts ~verify_dns_length labels =
  let rec loop acc = function
    | [] -> Ok acc
    | label :: rest -> (
        match ascii_part label with
        | Error e -> Error e
        | Ok part ->
            let acc =
              {
                parts_rev = part :: acc.parts_rev;
                last_part = Some part;
                has_long_label =
                  acc.has_long_label
                  || (verify_dns_length && String.length part > 63);
              }
            in
            loop acc rest)
  in
  loop { parts_rev = []; last_part = None; has_long_label = false } labels

let to_ascii ~flags domain =
  let processed = process_for_ascii ~flags domain in
  if processed.errored then Error "validation failed"
  else
    match
      collect_ascii_parts ~verify_dns_length:flags.verify_dns_length
        processed.labels
    with
    | Error e -> Error e
    | Ok parts ->
        if
          flags.verify_dns_length
          && match parts.last_part with Some "" -> true | _ -> false
        then Error "trailing dot (empty label)"
        else if parts.has_long_label then Error "label too long"
        else
          let parts = List.rev parts.parts_rev in
          let output = String.concat "." parts in
          let output_len = String.length output in
          if flags.verify_dns_length && output_len > 253 then
            Error "domain too long"
          else Ok output
