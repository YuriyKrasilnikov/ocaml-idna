(** Punycode (RFC 3492) codec. *)

let base = 36
let tmin = 1
let tmax = 26
let skew = 38
let damp = 700
let initial_bias = 72
let initial_n = 0x80
let is_surrogate cp = cp >= 0xD800 && cp <= 0xDFFF
let is_scalar cp = cp >= 0 && cp <= 0x10FFFF && not (is_surrogate cp)

let checked_add_nonnegative a b =
  if a < 0 || b < 0 || a > max_int - b then raise Exit else a + b

let checked_mul_nonnegative a b =
  if a < 0 || b < 0 || (b <> 0 && a > max_int / b) then raise Exit else a * b

let checked_int_of_int64 n =
  if n > Int64.of_int max_int || n < Int64.zero then raise Exit
  else Int64.to_int n

let adapt_term delta =
  let numerator =
    Int64.mul (Int64.of_int (base - tmin + 1)) (Int64.of_int delta)
  in
  let denominator = Int64.add (Int64.of_int delta) (Int64.of_int skew) in
  checked_int_of_int64 (Int64.div numerator denominator)

let decode_digit c =
  match c with
  | '0' .. '9' -> Char.code c - 22
  | 'A' .. 'Z' -> Char.code c - 65
  | 'a' .. 'z' -> Char.code c - 97
  | _ -> -1

let adapt delta num_points first_time =
  let delta = ref (if first_time then delta / damp else delta asr 1) in
  delta := checked_add_nonnegative !delta (!delta / num_points);
  let k = ref 0 in
  while !delta > (base - tmin) * tmax / 2 do
    delta := !delta / (base - tmin);
    k := checked_add_nonnegative !k base
  done;
  checked_add_nonnegative !k (adapt_term !delta)

let decode input =
  if String.length input = 0 then Error "empty input"
  else
    let basic, encoded =
      match String.rindex_opt input '-' with
      | Some pos ->
          ( String.sub input 0 pos,
            String.sub input (pos + 1) (String.length input - pos - 1) )
      | None -> ("", input)
    in
    let output = Array.make ((String.length input * 2) + 10) 0 in
    let out_len = ref 0 in
    (* Copy basic codepoints — must all be <= 0x7F per RFC 3492 §5 *)
    let basic_ok = ref true in
    String.iter
      (fun c ->
        let code = Char.code c in
        if code > 0x7F then basic_ok := false
        else begin
          output.(!out_len) <- code;
          incr out_len
        end)
      basic;
    if not !basic_ok then Error "non-basic byte in basic segment"
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
            i := checked_add_nonnegative !i (checked_mul_nonnegative digit !w);
            let t =
              if !k <= !bias + tmin then tmin
              else if !k >= !bias + tmax then tmax
              else !k - !bias
            in
            if digit < t then cont := false
            else begin
              w := checked_mul_nonnegative !w (base - t);
              k := checked_add_nonnegative !k base
            end
          done;
          let out = !out_len + 1 in
          if !i < oldi then raise Exit;
          bias := adapt (!i - oldi) out (oldi = 0);
          n := checked_add_nonnegative !n (!i / out);
          if not (is_scalar !n) then raise Exit;
          i := !i mod out;
          (* Insert n at position i *)
          let pos = !i in
          Array.blit output pos output (pos + 1) (!out_len - pos);
          output.(pos) <- !n;
          out_len := out;
          i := checked_add_nonnegative !i 1
        done;
        Ok (Array.to_list (Array.sub output 0 !out_len))
      with Exit -> Error "invalid punycode"

let encode_digit d =
  if d < 26 then Char.chr (d + 97) (* a-z *) else Char.chr (d - 26 + 48)
(* 0-9 *)

let encode input =
  if List.exists (fun cp -> not (is_scalar cp)) input then
    Error "invalid codepoint"
  else
    try
      let buf = Buffer.create 64 in
      let n = ref initial_n in
      let delta = ref 0 in
      let bias = ref initial_bias in
      (* Copy basic codepoints (lowercased per Section 7.1) *)
      let basic_count = ref 0 in
      List.iter
        (fun cp ->
          if cp < 0x80 then begin
            let c = Char.chr cp in
            Buffer.add_char buf (Char.lowercase_ascii c);
            incr basic_count
          end)
        input;
      if !basic_count > 0 then Buffer.add_char buf '-';
      let h = ref !basic_count in
      let len = List.length input in
      while !h < len do
        (* Find minimum codepoint >= n *)
        let m = ref max_int in
        List.iter (fun cp -> if cp >= !n && cp < !m then m := cp) input;
        (* Increase delta for skipped codepoints — overflow check (Section 6.4) *)
        let step = !m - !n in
        if step > (max_int - !delta) / (!h + 1) then raise Exit;
        delta := checked_add_nonnegative !delta (step * (!h + 1));
        n := !m;
        List.iter
          (fun cp ->
            if cp < !n then delta := checked_add_nonnegative !delta 1
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
                end
                else begin
                  Buffer.add_char buf
                    (encode_digit (t + ((!q - t) mod (base - t))));
                  q := (!q - t) / (base - t);
                  k := !k + base
                end
              done;
              bias := adapt !delta (!h + 1) (!h = !basic_count);
              delta := 0;
              incr h
            end)
          input;
        delta := checked_add_nonnegative !delta 1;
        n := checked_add_nonnegative !n 1
      done;
      Ok (Buffer.contents buf)
    with Exit -> Error "overflow"
