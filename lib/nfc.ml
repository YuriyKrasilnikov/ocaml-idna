(** Unicode Normalization Form C (UAX #15). *)

let s_base = 0xAC00
let l_base = 0x1100
let v_base = 0x1161
let t_base = 0x11A7
let l_count = 19
let v_count = 21
let t_count = 28
let n_count = v_count * t_count
let s_count = l_count * n_count
let ccc cp = Idna_tables.Props.ccc (Idna_tables.Props.get cp)

let rec is_nfc_qc_loop last_ccc = function
  | [] -> true
  | cp :: rest ->
      let props = Idna_tables.Props.get cp in
      if Idna_tables.Props.is_nfc_qc_non_yes props then false
      else
        let c = Idna_tables.Props.ccc props in
        if c <> 0 && c < last_ccc then false
        else is_nfc_qc_loop (if c = 0 then 0 else c) rest

let is_nfc_qc cps = is_nfc_qc_loop 0 cps

let canon_decomp_lookup cp =
  let props = Idna_tables.Props.get cp in
  if not (Idna_tables.Props.has_canon_decomp props) then None
  else
    let i = Shared.Bsearch_triple.first Idna_tables.canon_decomp cp in
    if i < 0 then
      invalid_arg
        (Printf.sprintf "missing canonical decomposition payload for U+%04X" cp)
    else
      let _, d1, d2 = Idna_tables.canon_decomp.(i) in
      if d2 = 0 then Some [ d1 ] else Some [ d1; d2 ]

let hangul_decomp cp =
  if cp >= s_base && cp < s_base + s_count then
    let s_index = cp - s_base in
    let l = l_base + (s_index / n_count) in
    let v = v_base + (s_index mod n_count / t_count) in
    let t = t_base + (s_index mod t_count) in
    if t = t_base then Some [ l; v ] else Some [ l; v; t ]
  else None

let small_canonical_run_limit = 32

let insertion_sort_nonstarter_run arr first last =
  for i = first + 1 to last - 1 do
    let cp = arr.(i) in
    let cp_cc = ccc cp in
    let j = ref (i - 1) in
    while !j >= first && ccc arr.(!j) > cp_cc do
      arr.(!j + 1) <- arr.(!j);
      decr j
    done;
    arr.(!j + 1) <- cp
  done

(* Canonical ordering is a stable sort by CCC inside each non-starter run;
   starters (CCC = 0) are hard boundaries and are never moved. *)
let stable_sort_nonstarter_run arr first last =
  let len = last - first in
  if len > 1 then
    if len <= small_canonical_run_limit then
      insertion_sort_nonstarter_run arr first last
    else begin
      let run = Array.sub arr first len in
      Array.stable_sort (fun a b -> compare (ccc a) (ccc b)) run;
      Array.blit run 0 arr first len
    end

let compose_lookup starter combining =
  if
    starter >= l_base
    && starter < l_base + l_count
    && combining >= v_base
    && combining < v_base + v_count
  then
    let l_index = starter - l_base in
    let v_index = combining - v_base in
    Some (s_base + (((l_index * v_count) + v_index) * t_count))
  else if
    starter >= s_base
    && starter < s_base + s_count
    && (starter - s_base) mod t_count = 0
    && combining > t_base
    && combining < t_base + t_count
  then Some (starter + combining - t_base)
  else Idna_tables.nfc_compose starter combining

type normalize_buffer = { mutable data : int array; mutable len : int }

let create_buffer capacity = { data = Array.make (max 8 capacity) 0; len = 0 }

let ensure_capacity b needed =
  if needed > Array.length b.data then begin
    let next = ref (Array.length b.data * 2) in
    while !next < needed do
      next := !next * 2
    done;
    let data = Array.make !next 0 in
    Array.blit b.data 0 data 0 b.len;
    b.data <- data
  end

let append b cp =
  ensure_capacity b (b.len + 1);
  b.data.(b.len) <- cp;
  b.len <- b.len + 1

let decompose_into cps out =
  let rec decomp cp =
    match hangul_decomp cp with
    | Some parts -> List.iter decomp parts
    | None -> (
        match canon_decomp_lookup cp with
        | Some parts -> List.iter decomp parts
        | None -> append out cp)
  in
  List.iter decomp cps

let canonical_order_array arr len =
  let run_start = ref 0 in
  for i = 0 to len - 1 do
    if ccc arr.(i) = 0 then begin
      stable_sort_nonstarter_run arr !run_start i;
      run_start := i + 1
    end
  done;
  stable_sort_nonstarter_run arr !run_start len

let compact_composed_array arr len =
  let write = ref 0 in
  for read = 0 to len - 1 do
    if arr.(read) >= 0 then begin
      arr.(!write) <- arr.(read);
      incr write
    end
  done;
  !write

let compose_array_in_place arr len =
  if len = 0 then 0
  else begin
    let starter_pos = ref 0 in
    let last_cc = ref 0 in
    for i = 1 to len - 1 do
      let cp = arr.(i) in
      let cp_cc = ccc cp in
      let blocked = !last_cc <> 0 && !last_cc >= cp_cc in
      if not blocked then
        begin match compose_lookup arr.(!starter_pos) cp with
        | Some composite ->
            arr.(!starter_pos) <- composite;
            arr.(i) <- -1
        | None ->
            if cp_cc = 0 then begin
              starter_pos := i;
              last_cc := 0
            end
            else last_cc := cp_cc
        end
      else if cp_cc = 0 then begin
        starter_pos := i;
        last_cc := 0
      end
      else last_cc := cp_cc
    done;
    compact_composed_array arr len
  end

let array_prefix_to_list arr len =
  let rec loop i acc = if i < 0 then acc else loop (i - 1) (arr.(i) :: acc) in
  loop (len - 1) []

let normalize_slow cps =
  let out = create_buffer 16 in
  decompose_into cps out;
  canonical_order_array out.data out.len;
  let len = compose_array_in_place out.data out.len in
  array_prefix_to_list out.data len

let nfc cps = if is_nfc_qc cps then cps else normalize_slow cps
