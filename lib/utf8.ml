(** UTF-8 encoding and decoding.

    Strict decoder per RFC 3629:
    - Rejects truncated sequences, invalid lead bytes, invalid continuations.
    - Rejects overlong encodings.
    - Rejects surrogate codepoints (U+D800..U+DFFF).
    - Rejects codepoints beyond U+10FFFF.

    Uses OCaml stdlib [String.get_utf_8_uchar] which implements the above. *)

(** Decode UTF-8 string to list of codepoints. Returns [Error msg] on first
    malformed sequence, with byte position. *)
let to_cps (s : string) : (int list, string) result =
  let len = String.length s in
  let rec loop i acc =
    if i >= len then Ok (List.rev acc)
    else
      let d = String.get_utf_8_uchar s i in
      if Uchar.utf_decode_is_valid d then
        let cp = Uchar.to_int (Uchar.utf_decode_uchar d) in
        loop (i + Uchar.utf_decode_length d) (cp :: acc)
      else Error (Printf.sprintf "invalid UTF-8 at byte %d" i)
  in
  loop 0 []

(** Encode list of codepoints to UTF-8 string. Raises [Invalid_argument] if any
    codepoint is not a Unicode scalar value. *)
let of_cps (cps : int list) : string =
  let buf = Buffer.create 64 in
  List.iter
    (fun cp ->
      if cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then
        invalid_arg "Idna.Utf8.of_cps: non-scalar codepoint";
      Buffer.add_utf_8_uchar buf (Uchar.of_int cp))
    cps;
  Buffer.contents buf
