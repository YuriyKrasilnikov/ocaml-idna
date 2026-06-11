let emit_set name table =
  for cp = 0 to 0x10FFFF do
    if Intranges.contains cp table then Printf.printf "set\t%s\t%06X\n" name cp
  done

let emit_uts46_map () =
  for cp = 0 to 0x10FFFF do
    let meta = Idna_tables.uts46_map_meta cp in
    if meta <> 0 then (
      let offset = (meta lsr 5) - 1 in
      let length = meta land 0x1F in
      Printf.printf "map\tuts46_mapped\t%06X" cp;
      for i = 0 to length - 1 do
        Printf.printf "\t%06X" Idna_tables.uts46_map_data.(offset + i)
      done;
      print_newline ())
  done

let emit_triple name (cp, a, b) =
  Printf.printf "%s\t%06X\t%06X\t%06X\n" name cp a b

let emit_pair name (cp, value) = Printf.printf "%s\t%06X\t%d\n" name cp value
let emit_props_set name cp = Printf.printf "props_set\t%s\t%06X\n" name cp

let emit_props_pair name cp value =
  Printf.printf "props_pair\t%s\t%06X\t%d\n" name cp value

let emit_props () =
  for cp = 0 to 0x10FFFF do
    let props = Idna_tables.Props.get cp in
    if props <> 0 then Printf.printf "props\t%06X\t%08X\n" cp props;
    begin match Idna_tables.Props.uts46_status props with
    | 1 ->
        emit_props_set "uts46_valid" cp;
        emit_props_pair "uts46_status" cp 1
    | 2 ->
        emit_props_set "uts46_ignored" cp;
        emit_props_pair "uts46_status" cp 2
    | 3 ->
        emit_props_set "uts46_deviation" cp;
        emit_props_pair "uts46_status" cp 3
    | 4 ->
        emit_props_set "uts46_mapped" cp;
        emit_props_pair "uts46_status" cp 4
    | _ -> ()
    end;
    begin match Idna_tables.Props.idna_class props with
    | 1 ->
        emit_props_set "codepoint_pvalid" cp;
        emit_props_pair "idna_class" cp 1
    | 2 ->
        emit_props_set "codepoint_contextj" cp;
        emit_props_pair "idna_class" cp 2
    | 3 ->
        emit_props_set "codepoint_contexto" cp;
        emit_props_pair "idna_class" cp 3
    | _ -> ()
    end;
    begin match Idna_tables.Props.bidi_class props with
    | 1 ->
        emit_props_set "bidi_r" cp;
        emit_props_pair "bidi_class" cp 1
    | 2 ->
        emit_props_set "bidi_l" cp;
        emit_props_pair "bidi_class" cp 2
    | 3 ->
        emit_props_set "bidi_al" cp;
        emit_props_pair "bidi_class" cp 3
    | 4 ->
        emit_props_set "bidi_an" cp;
        emit_props_pair "bidi_class" cp 4
    | 5 ->
        emit_props_set "bidi_en" cp;
        emit_props_pair "bidi_class" cp 5
    | 6 ->
        emit_props_set "bidi_es" cp;
        emit_props_pair "bidi_class" cp 6
    | 7 ->
        emit_props_set "bidi_cs" cp;
        emit_props_pair "bidi_class" cp 7
    | 8 ->
        emit_props_set "bidi_et" cp;
        emit_props_pair "bidi_class" cp 8
    | 9 ->
        emit_props_set "bidi_on" cp;
        emit_props_pair "bidi_class" cp 9
    | 10 ->
        emit_props_set "bidi_bn" cp;
        emit_props_pair "bidi_class" cp 10
    | 11 ->
        emit_props_set "bidi_nsm" cp;
        emit_props_pair "bidi_class" cp 11
    | _ -> ()
    end;
    if Idna_tables.Props.has_script_greek props then
      emit_props_set "script_greek" cp;
    if Idna_tables.Props.has_script_hebrew props then
      emit_props_set "script_hebrew" cp;
    if Idna_tables.Props.has_script_han props then
      emit_props_set "script_han" cp;
    if Idna_tables.Props.has_script_hiragana props then
      emit_props_set "script_hiragana" cp;
    if Idna_tables.Props.has_script_katakana props then
      emit_props_set "script_katakana" cp;
    if Idna_tables.Props.is_mark props then
      emit_props_set "general_category_m" cp;
    if Idna_tables.Props.is_nfc_qc_non_yes props then
      emit_props_set "nfc_qc_non_yes" cp;
    if Idna_tables.Props.is_uts46_nv8 props then emit_props_set "uts46_nv8" cp;
    if Idna_tables.Props.is_uts46_xv8 props then emit_props_set "uts46_xv8" cp;
    if Idna_tables.Props.has_canon_decomp props then
      emit_props_set "canon_decomp_present" cp;
    let ccc = Idna_tables.Props.ccc props in
    if ccc <> 0 then emit_props_pair "canon_ccc" cp ccc;
    let joining_type = Idna_tables.Props.joining_type props in
    if joining_type <> 0 then emit_props_pair "joining_type" cp joining_type
  done

let () =
  Printf.printf "meta\tword_size\t%d\n" Sys.word_size;
  emit_props ();
  emit_set "codepoint_pvalid" Idna_tables.codepoint_pvalid;
  emit_set "codepoint_contextj" Idna_tables.codepoint_contextj;
  emit_set "codepoint_contexto" Idna_tables.codepoint_contexto;
  emit_set "uts46_ignored" Idna_tables.uts46_ignored;
  emit_set "uts46_valid" Idna_tables.uts46_valid;
  emit_set "uts46_deviation" Idna_tables.uts46_deviation;
  emit_set "uts46_nv8" Idna_tables.uts46_nv8;
  emit_set "uts46_xv8" Idna_tables.uts46_xv8;
  emit_set "nfc_qc_non_yes" Idna_tables.nfc_qc_non_yes;
  emit_uts46_map ();
  Array.iter (emit_triple "canon_decomp") Idna_tables.canon_decomp;
  Array.iter (emit_pair "canon_ccc") Idna_tables.canon_ccc;
  Idna_tables.iter_nfc_compositions (fun starter combining composite ->
      Printf.printf "nfc_comp\t%06X\t%06X\t%06X\n" starter combining composite)
