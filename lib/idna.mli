(** Internationalized domain name validation, Punycode encoding/decoding,
    and Unicode NFC normalization.

    Validates hostnames and labels per IDNA2008 (RFC 5890/5892).
    Handles A-labels (xn-- Punycode), U-labels (Unicode), and ASCII labels. *)

(** Check if a hostname is valid.
    Splits on dots, validates each label, enforces DNS length limits
    and domain-level bidirectional text rules. *)
val is_valid_hostname : string -> bool

(** Validate a single label (without dots).
    Decodes xn-- prefix if present. Checks codepoint validity, hyphen rules,
    NFC form, initial combining mark, bidirectional text, and CONTEXTJ/CONTEXTO rules. *)
val check_label : string -> (unit, string) result

(** Check bidirectional text rules across all labels in a domain.
    If any label contains right-to-left characters, all labels must
    satisfy bidirectional text constraints. *)
val is_valid_hostname_bidi : string list -> bool

(** Normalize a codepoint sequence to Unicode NFC (Normalization Form C).
    Applies canonical decomposition, canonical ordering of combining marks,
    and canonical composition including Hangul syllables. *)
val nfc : int list -> int list

(** Punycode encoding and decoding (RFC 3492). *)
module Punycode : sig
  (** Decode a Punycode string to a list of Unicode codepoints. *)
  val decode : string -> (int list, string) result

  (** Encode a list of Unicode codepoints to a Punycode string.
      Output is lowercase. *)
  val encode : int list -> (string, string) result
end
