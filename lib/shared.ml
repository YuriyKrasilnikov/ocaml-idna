(** Shared types, helpers, and binary search functors used across the [Idna]
    internal modules. *)

type registration_hostname_flags = { verify_dns_length : bool }

let default_registration_hostname_flags = { verify_dns_length = true }

type lookup_flags = { check_bidi : bool }

let default_lookup_flags = { check_bidi = true }

type uts46_flags = {
  check_hyphens : bool;
  check_bidi : bool;
  check_joiners : bool;
  use_std3_ascii_rules : bool;
  verify_dns_length : bool;
  ignore_invalid_punycode : bool;
}

let default_uts46_flags =
  {
    check_hyphens = true;
    check_bidi = true;
    check_joiners = true;
    use_std3_ascii_rules = true;
    verify_dns_length = true;
    ignore_invalid_punycode = false;
  }

type uts46_result = { value : string; errored : bool }
type validated_label = { unicode : string; cps : int list }
type domain_label = Root | Label of validated_label
type uts46_map_payload = Map_span of int * int | Map_one of int

module Make_bsearch (K : sig
  type t

  val key : t -> int
end) =
struct
  let[@inline always] first arr cp =
    let len = Array.length arr in
    let lo = ref 0 in
    let hi = ref (len - 1) in
    while !lo <= !hi do
      let mid = !lo + ((!hi - !lo) / 2) in
      if K.key arr.(mid) < cp then lo := mid + 1 else hi := mid - 1
    done;
    if !lo < len && K.key arr.(!lo) = cp then !lo else -1
end

module Bsearch_pair = Make_bsearch (struct
  type t = int * int

  let[@inline always] key (k, _) = k
end)

module Bsearch_triple = Make_bsearch (struct
  type t = int * int * int

  let[@inline always] key (k, _, _) = k
end)

let ( >>= ) r f = match r with Ok x -> f x | Error _ as e -> e

let string_is_ascii s =
  let len = String.length s in
  let rec loop i = i = len || (Char.code s.[i] < 0x80 && loop (i + 1)) in
  loop 0

let lowercase_ascii_bytes s =
  String.init (String.length s) (fun i -> Char.lowercase_ascii s.[i])

let ascii_lower_code c = if c >= 0x41 && c <= 0x5A then c + 0x20 else c

let has_xn_prefix s =
  let len = String.length s in
  len >= 4
  && ascii_lower_code (Char.code s.[0]) = 0x78
  && ascii_lower_code (Char.code s.[1]) = 0x6E
  && s.[2] = '-'
  && s.[3] = '-'

let cps_to_utf8 = Utf8.of_cps

let rec cps_are_ascii = function
  | [] -> true
  | cp :: rest -> cp < 0x80 && cps_are_ascii rest

let ace_of_cps cps =
  if cps_are_ascii cps then Ok (cps_to_utf8 cps)
  else
    match Punycode.encode cps with
    | Error e -> Error e
    | Ok encoded -> Ok ("xn--" ^ encoded)

let joining_type cp =
  match Idna_tables.Props.joining_type (Idna_tables.Props.get cp) with
  | 0 -> None
  | n -> Some (n - 1)

let uts46_map_payload cp =
  let meta = Idna_tables.uts46_map_meta cp in
  if meta = 0 then invalid_arg "missing UTS46 mapped payload"
  else
    let offset = (meta lsr 5) - 1 in
    let length = meta land 0x1F in
    Map_span (offset, length)

let uts46_status_of_props cp props =
  match Idna_tables.Props.uts46_status props with
  | 1 -> `Valid
  | 2 -> `Ignored
  | 3 -> `Deviation
  | 4 -> `Map (uts46_map_payload cp)
  | _ -> `Disallowed

let uts46_status cp = uts46_status_of_props cp (Idna_tables.Props.get cp)

let split_on_dots cps =
  let rec split acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | cp :: rest ->
        if cp = 0x002E || cp = 0x3002 || cp = 0xFF0E || cp = 0xFF61 then
          split (List.rev current :: acc) [] rest
        else split acc (cp :: current) rest
  in
  split [] [] cps
