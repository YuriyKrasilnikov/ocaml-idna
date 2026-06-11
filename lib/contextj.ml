(** CONTEXTJ rule contextual validation (RFC 5892 Appendix A.1-A.2). *)

let props_of_cp = Idna_tables.Props.get
let idna_class_contextj = 2
let canonical_combining_class_virama = 9
let joining_type_dual_joining = 1
let joining_type_left_joining = 2
let joining_type_right_joining = 3
let joining_type_transparent = 4

let props_is_contextj props =
  Idna_tables.Props.idna_class props = idna_class_contextj

let props_is_virama props =
  Idna_tables.Props.ccc props = canonical_combining_class_virama

let joining_type_of_props props =
  match Idna_tables.Props.joining_type props with 0 -> None | n -> Some (n - 1)

let is_contextj_cp cp = props_is_contextj (props_of_cp cp)

let rec has_contextj = function
  | [] -> false
  | cp :: rest -> is_contextj_cp cp || has_contextj rest

let update_left_context current cp =
  match joining_type_of_props (props_of_cp cp) with
  | Some jt when jt = joining_type_transparent -> current
  | Some jt when jt = joining_type_left_joining -> true
  | Some jt when jt = joining_type_dual_joining -> true
  | _ -> false

let update_right_context current cp =
  match joining_type_of_props (props_of_cp cp) with
  | Some jt when jt = joining_type_transparent -> current
  | Some jt when jt = joining_type_right_joining -> true
  | Some jt when jt = joining_type_dual_joining -> true
  | _ -> false

type summary = {
  cps : int array;
  prev_virama : bool array;
  left_context : bool array;
  right_context : bool array;
}

let make_summary cps =
  let cps = Array.of_list cps in
  let len = Array.length cps in
  let prev_virama = Array.make len false in
  let left_context = Array.make len false in
  let right_context = Array.make len false in
  let left = ref false in
  for i = 0 to len - 1 do
    left_context.(i) <- !left;
    if i > 0 then prev_virama.(i) <- props_is_virama (props_of_cp cps.(i - 1));
    left := update_left_context !left cps.(i)
  done;
  let right = ref false in
  for i = len - 1 downto 0 do
    right_context.(i) <- !right;
    right := update_right_context !right cps.(i)
  done;
  { cps; prev_virama; left_context; right_context }

let valid_at summary pos cp =
  pos >= 0
  && pos < Array.length summary.cps
  && (summary.prev_virama.(pos)
     || cp = 0x200C && summary.left_context.(pos)
        && summary.right_context.(pos))

let error_detail cp =
  if cp = 0x200C then "CONTEXTJ: ZWNJ without valid context"
  else "CONTEXTJ: ZWJ without virama"
