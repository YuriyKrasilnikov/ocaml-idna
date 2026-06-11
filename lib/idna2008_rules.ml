(** Shared IDNA2008 rule predicates used by public and diagnostic paths. *)

type hyphen_error = Empty_label | Hyphen_start | Hyphen_end | Hyphen_3_4

type codepoint_error =
  | Codepoint_disallowed of { index : int; cp : int }
  | Contexto_failed of { index : int; cp : int }

type contextj_error = { index : int; cp : int; detail : string }

let rec label_ends_with_hyphen = function
  | [] -> false
  | [ 0x2D ] -> true
  | _ :: rest -> label_ends_with_hyphen rest

let check_hyphen = function
  | [] -> Error Empty_label
  | 0x2D :: _ -> Error Hyphen_start
  | cps when label_ends_with_hyphen cps -> Error Hyphen_end
  | _ :: _ :: 0x2D :: 0x2D :: _ -> Error Hyphen_3_4
  | _ -> Ok ()

let is_nfc cps = Nfc.is_nfc_qc cps || Nfc.nfc cps = cps

let initial_combiner = function
  | [] -> None
  | cp :: _ ->
      let props = Idna_tables.Props.get cp in
      if Idna_tables.Props.is_mark props then Some cp else None

let check_codepoints ~check_contexto cps =
  let contexto_summary = ref None in
  let get_contexto_summary () =
    match !contexto_summary with
    | Some summary -> summary
    | None ->
        let summary = Contexto.make_summary cps in
        contexto_summary := Some summary;
        summary
  in
  let rec check index = function
    | [] -> Ok ()
    | cp :: rest -> (
        let props = Idna_tables.Props.get cp in
        match Idna_tables.Props.idna_class props with
        | 1 | 2 -> check (index + 1) rest
        | 3 ->
            if
              (not check_contexto)
              || Contexto.valid_contexto_summary (get_contexto_summary ()) index
                   cp
            then check (index + 1) rest
            else Error (Contexto_failed { index; cp })
        | _ -> Error (Codepoint_disallowed { index; cp }))
  in
  check 0 cps

let check_contextj cps =
  let rec check summary index = function
    | [] -> Ok ()
    | cp :: rest ->
        if not (Contextj.is_contextj_cp cp) then check summary (index + 1) rest
        else if Contextj.valid_at summary index cp then
          check summary (index + 1) rest
        else Error { index; cp; detail = Contextj.error_detail cp }
  in
  if Contextj.has_contextj cps then check (Contextj.make_summary cps) 0 cps
  else Ok ()
