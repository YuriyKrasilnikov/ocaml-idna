(** Internationalized domain name processing.

    The library exposes three semantic layers:

    - [Registration]: strict IDNA2008 registration validation.
    - [Lookup]: strict IDNA2008 lookup preparation and conversion.
    - [Uts46]: Unicode IDNA Compatibility Processing (UTS #46, Nontransitional).

    Shared Unicode and codec primitives are also exposed. *)

val nfc : int list -> int list
(** Normalize a codepoint sequence to Unicode NFC (Normalization Form C).
    Applies canonical decomposition, canonical ordering of combining marks, and
    canonical composition including Hangul syllables. *)

(** Strict IDNA2008 registration validation. *)
module Registration : sig
  type hostname_flags = { verify_dns_length : bool }
  (** Hostname validation flags. *)

  val default_hostname_flags : hostname_flags
  (** Registration validation with DNS length checks enabled. *)

  val check_label : string -> (unit, string) result
  (** Validate a single registration label. Accepts canonical A-labels, NR-LDH
      labels, and U-labels. *)

  val to_unicode : ?flags:hostname_flags -> string -> (string, string) result
  (** Convert a registration-valid domain name to canonical Unicode form.
      A-labels are decoded; ASCII labels are lowercased. *)

  val to_ascii : ?flags:hostname_flags -> string -> (string, string) result
  (** Convert a registration-valid domain name to canonical ACE form. *)

  val is_valid_hostname : ?flags:hostname_flags -> string -> bool
  (** Check whether a domain name is valid for registration. *)
end

(** Strict IDNA2008 lookup processing. *)
module Lookup : sig
  type flags = { check_bidi : bool }
  (** Lookup flags. Bidi checking is recommended by RFC 5891 and enabled by
      default, but may be disabled in special circumstances. *)

  val default_flags : flags
  (** Lookup with bidi checking enabled. *)

  val to_unicode : ?flags:flags -> string -> (string, string) result
  (** Convert a domain name to canonical Unicode form for lookup. Apparent
      A-labels are lowercased before decoding and validation. *)

  val to_ascii : ?flags:flags -> string -> (string, string) Stdlib.result
  (** Convert a domain name to ACE form for lookup. *)
end

(** UTS #46 Nontransitional processing. *)
module Uts46 : sig
  type flags = {
    check_hyphens : bool;
    check_bidi : bool;
    check_joiners : bool;
    use_std3_ascii_rules : bool;
    verify_dns_length : bool;
    ignore_invalid_punycode : bool;
  }
  (** UTS #46 processing flags.

      [check_hyphens = false] disables the ordinary hyphen-position checks, but
      labels beginning with [xn--] are still rejected in that mode. This is not
      a pure relaxation: a successful [to_ascii] output containing an A-label
      may be rejected if processed again with [check_hyphens = false]. *)

  val default_flags : flags
  (** Nontransitional processing with all checks enabled. *)

  type result = { value : string; errored : bool }
  (** Unicode-form conversion result: the converted value is always produced;
      [errored] signals whether validation errors occurred. *)

  val to_ascii : ?flags:flags -> string -> (string, string) Stdlib.result
  (** Convert a domain name to its ASCII-compatible form (UTS #46
      Nontransitional). Maps, normalizes to NFC, splits on dots, validates, and
      Punycode-encodes non-ASCII labels.

      The error strings returned by this surface are coarse. For branch-specific
      explanation use [Diagnostics.Uts46.to_ascii]. *)

  val to_unicode : ?flags:flags -> string -> result
  (** Convert a domain name to its Unicode form (UTS #46 Nontransitional). Maps,
      normalizes to NFC, splits on dots, validates, and Punycode-decodes [xn--]
      labels. Always returns a converted value and an error flag. *)
end

(** Structured explainability and provenance for IDNA processing.

    Diagnostics mirrors the public semantic layers:
    - [Diagnostics.Registration] for strict IDNA2008 registration validation
    - [Diagnostics.Lookup] for strict lookup preparation/conversion
    - [Diagnostics.Uts46] for UTS #46 Nontransitional processing

    The diagnostics API is additive: it explains the outcome of the
    corresponding public operation, but it does not change the underlying
    acceptance or conversion semantics. Public operations do not construct
    diagnostics reports; use this module only when structured explanation is
    needed. *)
module Diagnostics : sig
  type policy = [ `Registration | `Lookup | `Uts46 ]

  type operation =
    [ `Check_label | `To_unicode | `To_ascii | `Is_valid_hostname ]

  (** Severity of a diagnostics event.
      - [Error]: contributes to rejection; any [Error] implies
        [accepted = false]
      - [Warning]: semantically notable or provenance-only fact that does not by
        itself reject the input
      - [Info]: neutral trace or classification event *)
  type severity = Error | Warning | Info

  (** Pipeline stage that emitted the event. Events are emitted in deterministic
      pipeline order. *)
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
        (** Defensive serialization failure. This is not a normative IDNA/UTS
            #46 condition. *)
    | Serialization_failed

  val string_of_code : code -> string
  (** Stable symbolic name for a diagnostics code. Intended for assertions,
      logs, and machine-readable comparisons. *)

  type event = {
    severity : severity;
    stage : stage;
    code : code;
    label_index : int option;
    cp_index : int option;
    cp : int option;
    detail : string option;
  }
  (** A single diagnostics fact. [label_index], [cp_index], and [cp] are present
      when the event is tied to a specific label or codepoint. [detail] is
      explanatory text only and is not a stability contract. *)

  type report = {
    policy : policy;
    operation : operation;
    input : string;
    output : string option;
    accepted : bool;
    events : event list;
  }
  (** A structured explanation of an operation.

      [accepted] matches the outcome of the corresponding non-diagnostics API:
      accepted reports correspond to [Ok _], [true], or [errored = false]
      depending on the mirrored operation.

      [events] are emitted in deterministic pipeline order.

      [output] mirrors the transformed value when the operation produces one. In
      particular, [Diagnostics.Uts46.to_unicode] may return [accepted = false]
      together with a converted [output], matching [Uts46.to_unicode]. *)

  module Registration : sig
    val check_label : string -> report
    (** Mirror of [Registration.check_label]. *)

    val to_unicode : ?flags:Registration.hostname_flags -> string -> report
    (** Mirror of [Registration.to_unicode]. *)

    val to_ascii : ?flags:Registration.hostname_flags -> string -> report
    (** Mirror of [Registration.to_ascii]. *)

    val is_valid_hostname :
      ?flags:Registration.hostname_flags -> string -> report
    (** Mirror of [Registration.is_valid_hostname]. *)
  end

  module Lookup : sig
    val to_unicode : ?flags:Lookup.flags -> string -> report
    (** Mirror of [Lookup.to_unicode]. *)

    val to_ascii : ?flags:Lookup.flags -> string -> report
    (** Mirror of [Lookup.to_ascii]. *)
  end

  module Uts46 : sig
    val to_unicode : ?flags:Uts46.flags -> string -> report
    (** Mirror of [Uts46.to_unicode]. *)

    val to_ascii : ?flags:Uts46.flags -> string -> report
    (** Mirror of [Uts46.to_ascii]. *)
  end
end

(** Punycode encoding and decoding (RFC 3492). *)
module Punycode : sig
  val decode : string -> (int list, string) result
  (** Decode a Punycode string to a list of Unicode codepoints. *)

  val encode : int list -> (string, string) result
  (** Encode a list of Unicode codepoints to a Punycode string. Output is
      lowercase. *)
end

(** UTF-8 codec (RFC 3629). *)
module Utf8 : sig
  val to_cps : string -> (int list, string) result
  (** Decode UTF-8 string to list of codepoints. Rejects truncated, overlong,
      surrogate, and out-of-range sequences. *)

  val of_cps : int list -> string
  (** Encode list of codepoints to UTF-8 string. Each codepoint must be a
      Unicode scalar value; invalid inputs raise [Invalid_argument]. *)
end
