(** UTF-8 codec per RFC 3629. *)

val to_cps : string -> (int list, string) result
(** Decode UTF-8 string to list of codepoints.
    Rejects truncated, overlong, surrogate, and out-of-range sequences. *)

val of_cps : int list -> string
(** Encode list of codepoints to UTF-8 string.
    Each codepoint must be in valid Unicode range [0..0x10FFFF] minus surrogates. *)
