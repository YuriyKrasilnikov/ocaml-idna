(* UTS #46 conformance tests from IdnaTestV2.txt.

   Tests toUnicode and toAsciiN (Nontransitional) for all entries.
   Conformance: Section 8 of UTS #46.

   Format per entry:
     source ; toUnicode ; toUnicodeStatus ; toAsciiN ; toAsciiNStatus ; toAsciiT ; toAsciiTStatus

   Blank toUnicode = same as source.
   Blank toAsciiN = same as toUnicode.
   Blank status = no errors.
   Explicit [] = no errors. *)

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
    if !i + 5 < len && s.[!i] = '\\' && s.[!i+1] = 'u' then begin
      let hex = String.sub s (!i+2) 4 in
      (try
        let cp = int_of_string ("0x" ^ hex) in
        add_uchar cp;
        i := !i + 6
      with _ ->
        Buffer.add_char buf s.[!i];
        incr i)
    end else if !i + 3 < len && s.[!i] = '\\' && s.[!i+1] = 'x' && s.[!i+2] = '{' then begin
      match String.index_from_opt s (!i+3) '}' with
      | Some close ->
        let hex = String.sub s (!i+3) (close - !i - 3) in
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

let strip s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && (s.[!i] = ' ' || s.[!i] = '\t') do incr i done;
  let j = ref (len - 1) in
  while !j >= !i && (s.[!j] = ' ' || s.[!j] = '\t') do decr j done;
  if !i > !j then "" else String.sub s !i (!j - !i + 1)

let has_errors status =
  let s = strip status in
  s <> "" && s <> "[]"

let () =
  let path = "tools/ucd-16.0.0/IdnaTestV2.txt" in
  let ic = open_in path in
  (* toUnicode stats *)
  let u_total = ref 0 in
  let u_pass = ref 0 in
  let u_fail = ref 0 in
  let u_fail_examples = Buffer.create 1024 in
  let u_fail_count = ref 0 in
  (* toAsciiN stats *)
  let a_total = ref 0 in
  let a_pass = ref 0 in
  let a_fail = ref 0 in
  let a_fail_examples = Buffer.create 1024 in
  let a_fail_count = ref 0 in
  (try while true do
    let line = input_line ic in
    let line = strip line in
    if String.length line = 0 || line.[0] = '#' || line.[0] = '@' then ()
    else begin
      let data = match String.index_opt line '#' with
        | Some pos -> String.sub line 0 pos
        | None -> line
      in
      let cols = String.split_on_char ';' data in
      match cols with
      | source_raw :: to_unicode_raw :: to_unicode_status_raw
        :: to_ascii_n_raw :: to_ascii_n_status_raw :: _ ->
        let source = decode_escapes (strip source_raw) in
        let to_unicode_str = strip to_unicode_raw in
        let to_unicode_expected =
          if String.length to_unicode_str = 0 then source
          else decode_escapes to_unicode_str
        in
        let to_unicode_err = has_errors to_unicode_status_raw in

        let to_ascii_n_str = strip to_ascii_n_raw in
        let to_ascii_n_expected =
          if String.length to_ascii_n_str = 0 then to_unicode_expected
          else decode_escapes to_ascii_n_str
        in
        let to_ascii_n_status_str = strip to_ascii_n_status_raw in
        let to_ascii_n_err =
          if String.length to_ascii_n_status_str = 0 then to_unicode_err
          else has_errors to_ascii_n_status_str
        in

        (* ── Test toUnicode ── *)
        incr u_total;
        let u_result = Idna.to_unicode source in
        let u_ok = match u_result with
          | Ok result -> not to_unicode_err && result = to_unicode_expected
          | Error _ -> to_unicode_err
        in
        if u_ok then incr u_pass
        else begin
          incr u_fail;
          if !u_fail_count < 20 then begin
            let got = match u_result with
              | Ok s -> Printf.sprintf "Ok %S" (String.escaped s)
              | Error e -> Printf.sprintf "Error %S" e
            in
            Buffer.add_string u_fail_examples
              (Printf.sprintf "  toUnicode(%s) = %s, expected %s%s\n"
                 (String.escaped (String.sub source 0 (min 40 (String.length source))))
                 got
                 (if to_unicode_err then "ERROR" else String.escaped (String.sub to_unicode_expected 0 (min 40 (String.length to_unicode_expected))))
                 (strip to_unicode_status_raw));
            incr u_fail_count
          end
        end;

        (* ── Test toAsciiN ── *)
        incr a_total;
        let a_result = Idna.to_ascii source in
        let a_ok = match a_result with
          | Ok result -> not to_ascii_n_err && result = to_ascii_n_expected
          | Error _ -> to_ascii_n_err
        in
        if a_ok then incr a_pass
        else begin
          incr a_fail;
          if !a_fail_count < 20 then begin
            let got = match a_result with
              | Ok s -> Printf.sprintf "Ok %S" (String.escaped s)
              | Error e -> Printf.sprintf "Error %S" e
            in
            Buffer.add_string a_fail_examples
              (Printf.sprintf "  toAsciiN(%s) = %s, expected %s%s\n"
                 (String.escaped (String.sub source 0 (min 40 (String.length source))))
                 got
                 (if to_ascii_n_err then "ERROR" else String.escaped (String.sub to_ascii_n_expected 0 (min 40 (String.length to_ascii_n_expected))))
                 (strip to_ascii_n_status_str));
            incr a_fail_count
          end
        end

      | _ -> ()
    end
  done with End_of_file -> ());
  close_in ic;
  Printf.printf "UTS #46 IdnaTestV2 results (Nontransitional):\n\n";
  Printf.printf "toUnicode:\n";
  Printf.printf "  total:  %d\n" !u_total;
  Printf.printf "  pass:   %d\n" !u_pass;
  Printf.printf "  fail:   %d\n" !u_fail;
  Printf.printf "  rate:   %.1f%%\n"
    (if !u_total > 0 then 100.0 *. float_of_int !u_pass /. float_of_int !u_total else 0.0);
  if !u_fail > 0 then begin
    Printf.printf "\n  First %d toUnicode failures:\n" (min !u_fail 20);
    print_string (Buffer.contents u_fail_examples)
  end;
  Printf.printf "\ntoAsciiN:\n";
  Printf.printf "  total:  %d\n" !a_total;
  Printf.printf "  pass:   %d\n" !a_pass;
  Printf.printf "  fail:   %d\n" !a_fail;
  Printf.printf "  rate:   %.1f%%\n"
    (if !a_total > 0 then 100.0 *. float_of_int !a_pass /. float_of_int !a_total else 0.0);
  if !a_fail > 0 then begin
    Printf.printf "\n  First %d toAsciiN failures:\n" (min !a_fail 20);
    print_string (Buffer.contents a_fail_examples)
  end
