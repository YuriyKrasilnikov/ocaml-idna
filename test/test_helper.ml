let require_files paths =
  List.iter (fun path ->
    if not (Sys.file_exists path) then begin
      Printf.eprintf
        "UCD file missing: %s\n\
         Run ./tools/download_ucd.sh 16.0.0 to fetch it (see README).\n"
        path;
      exit 77
    end
  ) paths

let strip s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  let j = ref (len - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t') do decr j done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)

let unquote_field s =
  if s = "\"\"" then "" else s

let decode_escapes s =
  let buf = Buffer.create (String.length s) in
  let len = String.length s in
  let i = ref 0 in
  let add_uchar cp =
    if cp < 0x80 then
      Buffer.add_char buf (Char.chr cp)
    else if cp < 0x800 then begin
      Buffer.add_char buf (Char.chr (0xC0 lor (cp lsr 6)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
    end else if cp < 0x10000 then begin
      Buffer.add_char buf (Char.chr (0xE0 lor (cp lsr 12)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
    end else begin
      Buffer.add_char buf (Char.chr (0xF0 lor (cp lsr 18)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
      Buffer.add_char buf (Char.chr (0x80 lor (cp land 0x3F)))
    end
  in
  while !i < len do
    if !i + 5 < len && s.[!i] = '\\' && s.[!i + 1] = 'u' then begin
      let hex = String.sub s (!i + 2) 4 in
      (try
         let cp = int_of_string ("0x" ^ hex) in
         add_uchar cp;
         i := !i + 6
       with _ ->
         Buffer.add_char buf s.[!i];
         incr i)
    end else if !i + 3 < len && s.[!i] = '\\' && s.[!i + 1] = 'x' && s.[!i + 2] = '{' then begin
      match String.index_from_opt s (!i + 3) '}' with
      | Some close ->
        let hex = String.sub s (!i + 3) (close - !i - 3) in
        (try
           let cp = int_of_string ("0x" ^ hex) in
           add_uchar cp;
           i := close + 1
         with _ ->
           Buffer.add_char buf s.[!i];
           incr i)
      | None ->
        Buffer.add_char buf s.[!i];
        incr i
    end else begin
      Buffer.add_char buf s.[!i];
      incr i
    end
  done;
  Buffer.contents buf

let has_errors status =
  let s = strip status in
  s <> "" && s <> "[]"

let utf8_to_cps s =
  let len = String.length s in
  let cps = ref [] in
  let i = ref 0 in
  while !i < len do
    let b = Char.code s.[!i] in
    let cp, size =
      if b land 0x80 = 0 then
        (b, 1)
      else if b land 0xE0 = 0xC0 && !i + 1 < len then
        (((b land 0x1F) lsl 6) lor (Char.code s.[!i + 1] land 0x3F), 2)
      else if b land 0xF0 = 0xE0 && !i + 2 < len then
        (((b land 0x0F) lsl 12)
         lor ((Char.code s.[!i + 1] land 0x3F) lsl 6)
         lor (Char.code s.[!i + 2] land 0x3F), 3)
      else if !i + 3 < len then
        (((b land 0x07) lsl 18)
         lor ((Char.code s.[!i + 1] land 0x3F) lsl 12)
         lor ((Char.code s.[!i + 2] land 0x3F) lsl 6)
         lor (Char.code s.[!i + 3] land 0x3F), 4)
      else
        (b, 1)
    in
    cps := cp :: !cps;
    i := !i + size
  done;
  List.rev !cps

type cp_range = int * int

let load_nv8_xv8 path =
  let ic = open_in path in
  let ranges = ref [] in
  let has_nv8_xv8_field fields =
    List.exists (fun field ->
      let field = strip field in
      field = "NV8" || field = "XV8"
    ) fields
  in
  (try
     while true do
       let line = input_line ic |> strip in
       if String.length line > 0 && line.[0] <> '#' then begin
         let data =
           match String.index_opt line '#' with
           | Some pos -> String.sub line 0 pos
           | None -> line
         in
         let fields = List.map strip (String.split_on_char ';' data) in
         if has_nv8_xv8_field fields then
           match fields with
           | range_str :: _ ->
             let range_str = strip range_str in
             (match String.split_on_char '.' range_str with
              | [a; ""; b] ->
                let lo = int_of_string ("0x" ^ strip a) in
                let hi = int_of_string ("0x" ^ strip b) in
                ranges := (lo, hi) :: !ranges
              | _ ->
                (try
                   let cp = int_of_string ("0x" ^ range_str) in
                   ranges := (cp, cp) :: !ranges
                 with _ -> ()))
           | [] -> ()
       end
     done
   with End_of_file -> ());
  close_in ic;
  !ranges

let load_uts46_status_ranges path wanted_status =
  let ic = open_in path in
  let ranges = ref [] in
  (try
     while true do
       let line = input_line ic |> strip in
       if String.length line > 0 && line.[0] <> '#' then begin
         let data =
           match String.index_opt line '#' with
           | Some pos -> String.sub line 0 pos
           | None -> line
         in
         let fields = List.map strip (String.split_on_char ';' data) in
         match fields with
         | range_str :: status :: _ when status = wanted_status ->
           let range_str = strip range_str in
           (match String.split_on_char '.' range_str with
            | [a; ""; b] ->
              let lo = int_of_string ("0x" ^ strip a) in
              let hi = int_of_string ("0x" ^ strip b) in
              ranges := (lo, hi) :: !ranges
            | _ ->
              (try
                 let cp = int_of_string ("0x" ^ range_str) in
                 ranges := (cp, cp) :: !ranges
               with _ -> ()))
         | _ -> ()
       end
     done
   with End_of_file -> ());
  close_in ic;
  !ranges

let string_has_nv8_xv8 ranges s =
  let cps = utf8_to_cps s in
  List.exists (fun cp ->
    List.exists (fun (lo, hi) -> cp >= lo && cp <= hi) ranges
  ) cps

type idna_test_vector = {
  source : string;
  to_unicode : string;
  to_unicode_status : string;
  to_unicode_err : bool;
  to_ascii_n : string;
  to_ascii_n_status : string;
  to_ascii_n_err : bool;
}

let load_idna_test_vectors path =
  let ic = open_in path in
  let vectors = ref [] in
  (try
     while true do
       let line = input_line ic |> strip in
       if String.length line = 0 || line.[0] = '#' || line.[0] = '@' then ()
       else begin
         let data =
           match String.index_opt line '#' with
           | Some pos -> String.sub line 0 pos
           | None -> line
         in
         match String.split_on_char ';' data with
         | source_raw :: to_unicode_raw :: to_unicode_status_raw
           :: to_ascii_n_raw :: to_ascii_n_status_raw :: _ ->
           let source = decode_escapes (unquote_field (strip source_raw)) in
           let to_unicode_str = strip to_unicode_raw in
           let to_unicode =
             if String.length to_unicode_str = 0 then source
             else decode_escapes (unquote_field to_unicode_str)
           in
           let to_unicode_status = strip to_unicode_status_raw in
           let to_unicode_err = has_errors to_unicode_status in
           let to_ascii_n_str = strip to_ascii_n_raw in
           let to_ascii_n =
             if String.length to_ascii_n_str = 0 then to_unicode
             else decode_escapes (unquote_field to_ascii_n_str)
           in
           let to_ascii_n_status = strip to_ascii_n_status_raw in
           let to_ascii_n_err =
             if String.length to_ascii_n_status = 0 then to_unicode_err
             else has_errors to_ascii_n_status
           in
           vectors := {
             source;
             to_unicode;
             to_unicode_status;
             to_unicode_err;
             to_ascii_n;
             to_ascii_n_status;
             to_ascii_n_err;
           } :: !vectors
         | _ -> ()
       end
     done
   with End_of_file -> ());
  close_in ic;
  List.rev !vectors
