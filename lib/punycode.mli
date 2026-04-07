(** Punycode (RFC 3492) decoder.

    Decodes Punycode-encoded strings to Unicode codepoint lists. *)

val decode : string -> (int list, string) result
