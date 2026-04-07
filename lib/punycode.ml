(** Punycode (RFC 3492) decoder. *)

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
  (* Copy basic codepoints *)
  String.iter (fun c ->
    output.(!out_len) <- Char.code c;
    incr out_len
  ) basic;
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
