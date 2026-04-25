val require_files : string list -> unit

val strip : string -> string
val unquote_field : string -> string
val decode_escapes : string -> string
val has_errors : string -> bool

val utf8_to_cps : string -> int list

type cp_range = int * int

val load_nv8_xv8 : string -> cp_range list
val load_uts46_status_ranges : string -> string -> cp_range list
val string_has_nv8_xv8 : cp_range list -> string -> bool

type idna_test_vector = {
  source : string;
  to_unicode : string;
  to_unicode_status : string;
  to_unicode_err : bool;
  to_ascii_n : string;
  to_ascii_n_status : string;
  to_ascii_n_err : bool;
}

val load_idna_test_vectors : string -> idna_test_vector list
