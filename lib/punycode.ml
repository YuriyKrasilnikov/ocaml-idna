(** Punycode (RFC 3492) codec. *)

let base = 36
let tmin = 1
let tmax = 26
let skew = 38
let damp = 700
let initial_bias = 72
let initial_n = 0x80

let decode_digit c =
  match c with
  | '0'..'9' -> Char.code c - 22
  | 'A'..'Z' -> Char.code c - 65
  | 'a'..'z' -> Char.code c - 97
  | _ -> -1

let adapt delta num_points first_time =
  let delta = ref (if first_time then delta / damp else delta asr 1) in
  delta := !delta + !delta / num_points;
  let k = ref 0 in
  while !delta > ((base - tmin) * tmax) / 2 do
    delta := !delta / (base - tmin);
    k := !k + base
  done;
  !k + (base - tmin + 1) * !delta / (!delta + skew)

let decode input =
  if String.length input = 0 then Error "empty input"
  else
  let basic, encoded =
    match String.rindex_opt input '-' with
    | Some pos ->
      (String.sub input 0 pos,
       String.sub input (pos + 1) (String.length input - pos - 1))
    | None -> ("", input)
  in
  let output = Array.make (String.length input * 2 + 10) 0 in
  let out_len = ref 0 in
  (* Copy basic codepoints — must all be <= 0x7F per RFC 3492 §5 *)
  let basic_ok = ref true in
  String.iter (fun c ->
    let code = Char.code c in
    if code > 0x7F then basic_ok := false
    else begin
      output.(!out_len) <- code;
      incr out_len
    end
  ) basic;
  if not !basic_ok then
    Error "non-basic byte in basic segment"
  else
  let n = ref initial_n in
  let i = ref 0 in
  let bias = ref initial_bias in
  let ic = ref 0 in
  let len = String.length encoded in
  try
    while !ic < len do
      let oldi = !i in
      let w = ref 1 in
      let k = ref base in
      let cont = ref true in
      while !cont do
        if !ic >= len then raise Exit;
        let digit = decode_digit encoded.[!ic] in
        incr ic;
        if digit < 0 || digit >= base then raise Exit;
        i := !i + digit * !w;
        let t =
          if !k <= !bias + tmin then tmin
          else if !k >= !bias + tmax then tmax
          else !k - !bias
        in
        if digit < t then cont := false
        else begin
          w := !w * (base - t);
          k := !k + base
        end
      done;
      let out = !out_len + 1 in
      bias := adapt (!i - oldi) out (oldi = 0);
      n := !n + !i / out;
      if !n > 0x10FFFF then raise Exit;
      i := !i mod out;
      (* Insert n at position i *)
      let pos = !i in
      Array.blit output pos output (pos + 1) (!out_len - pos);
      output.(pos) <- !n;
      out_len := out;
      i := !i + 1
    done;
    Ok (Array.to_list (Array.sub output 0 !out_len))
  with Exit -> Error "invalid punycode"

let encode_digit d =
  if d < 26 then Char.chr (d + 97)      (* a-z *)
  else Char.chr (d - 26 + 48)           (* 0-9 *)

let encode input =
  try
  let buf = Buffer.create 64 in
  let n = ref initial_n in
  let delta = ref 0 in
  let bias = ref initial_bias in
  (* Copy basic codepoints (lowercased per Section 7.1) *)
  let basic_count = ref 0 in
  List.iter (fun cp ->
    if cp < 0x80 then begin
      let c = Char.chr cp in
      Buffer.add_char buf (Char.lowercase_ascii c);
      incr basic_count
    end
  ) input;
  if !basic_count > 0 then Buffer.add_char buf '-';
  let h = ref !basic_count in
  let len = List.length input in
  while !h < len do
    (* Find minimum codepoint >= n *)
    let m = ref max_int in
    List.iter (fun cp -> if cp >= !n && cp < !m then m := cp) input;
    (* Increase delta for skipped codepoints — overflow check (Section 6.4) *)
    let step = !m - !n in
    if step > (max_int - !delta) / (!h + 1) then
      raise Exit;
    delta := !delta + step * (!h + 1);
    n := !m;
    List.iter (fun cp ->
      if cp < !n then incr delta
      else if cp = !n then begin
        (* Encode delta as variable-length integer *)
        let q = ref !delta in
        let k = ref base in
        let cont = ref true in
        while !cont do
          let t =
            if !k <= !bias + tmin then tmin
            else if !k >= !bias + tmax then tmax
            else !k - !bias
          in
          if !q < t then begin
            Buffer.add_char buf (encode_digit !q);
            cont := false
          end else begin
            Buffer.add_char buf (encode_digit (t + (!q - t) mod (base - t)));
            q := (!q - t) / (base - t);
            k := !k + base
          end
        done;
        bias := adapt !delta (!h + 1) (!h = !basic_count);
        delta := 0;
        incr h
      end
    ) input;
    incr delta;
    incr n
  done;
  Ok (Buffer.contents buf)
  with Exit -> Error "overflow"
