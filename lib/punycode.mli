(** Punycode (RFC 3492) codec.

    Encodes and decodes Punycode strings (Unicode codepoint lists). *)

val decode : string -> (int list, string) result

val encode : int list -> (string, string) result
