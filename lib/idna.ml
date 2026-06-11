(** Internationalized domain name processing. *)

open Shared

let nfc = Nfc.nfc

exception Ascii_slow_path

let ascii_lower_code c = if c >= 0x41 && c <= 0x5A then c + 0x20 else c

let ascii_is_ldh c =
  (c >= 0x61 && c <= 0x7A) || (c >= 0x30 && c <= 0x39) || c = 0x2D

let ascii_label_has_xn_prefix domain start len =
  len >= 4
  && ascii_lower_code (Char.code domain.[start]) = 0x78
  && ascii_lower_code (Char.code domain.[start + 1]) = 0x6E
  && domain.[start + 2] = '-'
  && domain.[start + 3] = '-'

let ascii_label_has_hyphen_3_4 domain start len =
  len >= 4 && domain.[start + 2] = '-' && domain.[start + 3] = '-'

type uts46_ascii_class =
  | Uts46_non_ascii
  | Uts46_ascii_plain of bool
  | Uts46_ascii_alabel_candidate
  | Uts46_ascii_other

let ascii_seen_xn = 1
let ascii_seen_upper = 2
let ascii_seen_non_ldh = 4
let ascii_seen_empty_label = 8
let ascii_seen_hyphen_error = 16
let ascii_seen_length_error = 32
let ascii_has bit flags = flags land bit <> 0

let classify_uts46_ascii_label ~check_hyphens ~verify_dns_length domain start
    stop flags =
  let len = stop - start in
  if len = 0 then flags lor ascii_seen_empty_label
  else
    let flags =
      if verify_dns_length && len > 63 then flags lor ascii_seen_length_error
      else flags
    in
    let flags =
      if ascii_label_has_xn_prefix domain start len then flags lor ascii_seen_xn
      else flags
    in
    if check_hyphens then
      let flags =
        if domain.[start] = '-' then flags lor ascii_seen_hyphen_error
        else flags
      in
      let flags =
        if domain.[stop - 1] = '-' then flags lor ascii_seen_hyphen_error
        else flags
      in
      if ascii_label_has_hyphen_3_4 domain start len then
        flags lor ascii_seen_hyphen_error
      else flags
    else flags

let classify_uts46_ascii ~check_hyphens ~verify_dns_length domain =
  let n = String.length domain in
  if n = 0 then Uts46_ascii_other
  else
    let initial_flags =
      if verify_dns_length && n > 253 then ascii_seen_length_error else 0
    in
    let rec loop flags start i =
      if i = n then
        `Ascii
          (classify_uts46_ascii_label ~check_hyphens ~verify_dns_length domain
             start n flags)
      else
        let c = Char.code domain.[i] in
        if c >= 0x80 then `Non_ascii
        else
          let lc = ascii_lower_code c in
          let flags = if lc <> c then flags lor ascii_seen_upper else flags in
          if lc = 0x2E then
            let flags =
              classify_uts46_ascii_label ~check_hyphens ~verify_dns_length
                domain start i flags
            in
            loop flags (i + 1) (i + 1)
          else
            let flags =
              if ascii_is_ldh lc then flags else flags lor ascii_seen_non_ldh
            in
            loop flags start (i + 1)
    in
    match loop initial_flags 0 0 with
    | `Non_ascii -> Uts46_non_ascii
    | `Ascii flags ->
        if
          (not (ascii_has ascii_seen_xn flags))
          && (not (ascii_has ascii_seen_non_ldh flags))
          && (not (ascii_has ascii_seen_empty_label flags))
          && (not (ascii_has ascii_seen_hyphen_error flags))
          && not (ascii_has ascii_seen_length_error flags)
        then Uts46_ascii_plain (ascii_has ascii_seen_upper flags)
        else if
          ascii_has ascii_seen_xn flags
          && (not (ascii_has ascii_seen_non_ldh flags))
          && not (ascii_has ascii_seen_empty_label flags)
        then Uts46_ascii_alabel_candidate
        else Uts46_ascii_other

let ascii_check_label_end domain ~start ~stop ~check_hyphens ~verify_dns_length
    =
  let len = stop - start in
  if len = 0 then raise Ascii_slow_path;
  if ascii_label_has_xn_prefix domain start len then raise Ascii_slow_path;
  if verify_dns_length && len > 63 then raise Ascii_slow_path;
  if check_hyphens then begin
    if domain.[start] = '-' then raise Ascii_slow_path;
    if domain.[stop - 1] = '-' then raise Ascii_slow_path;
    if ascii_label_has_hyphen_3_4 domain start len then raise Ascii_slow_path
  end

let ascii_lowercase_copy domain =
  let n = String.length domain in
  let buf = Bytes.create n in
  for i = 0 to n - 1 do
    let lc = ascii_lower_code (Char.code domain.[i]) in
    Bytes.set buf i (Char.chr lc)
  done;
  Bytes.unsafe_to_string buf

let rec ascii_scan domain n allow_dots check_hyphens verify_dns_length i
    label_start prev_dot saw_upper =
  if i = n then begin
    ascii_check_label_end domain ~start:label_start ~stop:n ~check_hyphens
      ~verify_dns_length;
    saw_upper
  end
  else
    let c = Char.code domain.[i] in
    if c >= 0x80 then raise Ascii_slow_path
    else
      let lc = ascii_lower_code c in
      let saw_upper = saw_upper || lc <> c in
      if lc = 0x2E then begin
        if (not allow_dots) || prev_dot then raise Ascii_slow_path;
        ascii_check_label_end domain ~start:label_start ~stop:i ~check_hyphens
          ~verify_dns_length;
        ascii_scan domain n allow_dots check_hyphens verify_dns_length (i + 1)
          (i + 1) true saw_upper
      end
      else begin
        if not (ascii_is_ldh lc) then raise Ascii_slow_path;
        ascii_scan domain n allow_dots check_hyphens verify_dns_length (i + 1)
          label_start false saw_upper
      end

let ascii_fast ~allow_dots ~check_hyphens ~verify_dns_length domain =
  let n = String.length domain in
  if n = 0 then `Slow
  else if allow_dots && (domain.[0] = '.' || domain.[n - 1] = '.') then `Slow
  else if (not allow_dots) && String.contains domain '.' then `Slow
  else if allow_dots && verify_dns_length && n > 253 then `Slow
  else
    try
      if
        ascii_scan domain n allow_dots check_hyphens verify_dns_length 0 0 false
          false
      then `Ok (ascii_lowercase_copy domain)
      else `Ok domain
    with Ascii_slow_path -> `Slow

module Registration = struct
  type hostname_flags = registration_hostname_flags = {
    verify_dns_length : bool;
  }

  let default_hostname_flags = default_registration_hostname_flags

  let check_label label =
    match
      ascii_fast ~allow_dots:false ~check_hyphens:true ~verify_dns_length:false
        label
    with
    | `Ok _ -> Ok ()
    | `Slow -> Idna2008_public.Registration.check_label label

  let to_unicode ?(flags = default_hostname_flags) domain =
    match
      ascii_fast ~allow_dots:true ~check_hyphens:true
        ~verify_dns_length:flags.verify_dns_length domain
    with
    | `Ok s -> Ok s
    | `Slow -> Idna2008_public.Registration.to_unicode ~flags domain

  let to_ascii ?(flags = default_hostname_flags) domain =
    match
      ascii_fast ~allow_dots:true ~check_hyphens:true
        ~verify_dns_length:flags.verify_dns_length domain
    with
    | `Ok s -> Ok s
    | `Slow -> Idna2008_public.Registration.to_ascii ~flags domain

  let is_valid_hostname ?(flags = default_hostname_flags) domain =
    match
      ascii_fast ~allow_dots:true ~check_hyphens:true
        ~verify_dns_length:flags.verify_dns_length domain
    with
    | `Ok _ -> true
    | `Slow -> Idna2008_public.Registration.is_valid_hostname ~flags domain
end

module Lookup = struct
  type flags = lookup_flags = { check_bidi : bool }

  let default_flags = default_lookup_flags

  let to_unicode ?(flags = default_flags) domain =
    match
      ascii_fast ~allow_dots:true ~check_hyphens:true ~verify_dns_length:false
        domain
    with
    | `Ok s -> Ok s
    | `Slow -> Idna2008_public.Lookup.to_unicode ~flags domain

  let to_ascii ?(flags = default_flags) domain =
    match
      ascii_fast ~allow_dots:true ~check_hyphens:true ~verify_dns_length:false
        domain
    with
    | `Ok s -> Ok s
    | `Slow -> Idna2008_public.Lookup.to_ascii ~flags domain
end

module Uts46 = struct
  type flags = uts46_flags = {
    check_hyphens : bool;
    check_bidi : bool;
    check_joiners : bool;
    use_std3_ascii_rules : bool;
    verify_dns_length : bool;
    ignore_invalid_punycode : bool;
  }

  let default_flags = default_uts46_flags

  type result = uts46_result = { value : string; errored : bool }

  let to_unicode ?(flags = default_flags) domain =
    match
      classify_uts46_ascii ~check_hyphens:flags.check_hyphens
        ~verify_dns_length:false domain
    with
    | Uts46_ascii_plain saw_upper ->
        {
          value = (if saw_upper then ascii_lowercase_copy domain else domain);
          errored = false;
        }
    | Uts46_ascii_alabel_candidate -> (
        match Uts46_public.to_unicode_ascii_alabel_fast ~flags domain with
        | Some result -> result
        | None -> Uts46_public.to_unicode ~flags domain)
    | Uts46_non_ascii | Uts46_ascii_other ->
        Uts46_public.to_unicode ~flags domain

  let to_ascii ?(flags = default_flags) domain =
    match
      classify_uts46_ascii ~check_hyphens:flags.check_hyphens
        ~verify_dns_length:flags.verify_dns_length domain
    with
    | Uts46_ascii_plain saw_upper ->
        Ok (if saw_upper then ascii_lowercase_copy domain else domain)
    | Uts46_ascii_alabel_candidate | Uts46_ascii_other -> (
        match Uts46_public.to_ascii_ascii_alabel_fast_ascii ~flags domain with
        | Some result -> result
        | None -> Uts46_public.to_ascii ~flags domain)
    | Uts46_non_ascii -> Uts46_public.to_ascii ~flags domain
end

module Diagnostics = Diagnostics
module Punycode = Punycode
module Utf8 = Utf8
