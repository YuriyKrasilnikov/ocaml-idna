(** IDNA2008 (RFC 5890/5892) hostname validation.

    Validates internationalized domain name labels per IDNA2008.
    Handles both A-labels (xn-- Punycode) and U-labels (Unicode). *)

(** Check if a hostname is valid per IDNA2008.
    Splits on dots, validates each label (ASCII or xn-- Punycode). *)
val is_valid_hostname : string -> bool

(** Check a single label (without dots).
    Decodes xn-- if present, then validates the Unicode label. *)
val check_label : string -> (unit, string) result

(** Check domain-level bidi rules across labels (RFC 5893).
    Takes pre-split labels (strings). Returns true if bidi is ok. *)
val is_valid_hostname_bidi : string list -> bool

(** Punycode (RFC 3492) codec. *)
module Punycode : sig
  val decode : string -> (int list, string) result
  val encode : int list -> (string, string) result
end
