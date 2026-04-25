(** NFC conformance against Unicode NormalizationTest.txt (UAX #15).

    For each test row c1;c2;c3;c4;c5:
      toNFC(c1) = toNFC(c2) = toNFC(c3) = c2
      toNFC(c4) = toNFC(c5) = c4 *)

let parse_cps s =
  let s = String.trim s in
  if String.length s = 0 then []
  else
    String.split_on_char ' ' s
    |> List.map (fun hex -> int_of_string ("0x" ^ hex))

let cps_pp = Alcotest.(list int)

type row = {
  line : int;
  part : string;
  c1 : int list;
  c2 : int list;
  c3 : int list;
  c4 : int list;
  c5 : int list;
}

let load_rows path =
  let ic = open_in path in
  let rows = ref [] in
  let line_num = ref 0 in
  let part = ref "" in
  (try while true do
    let line = input_line ic in
    incr line_num;
    let line = String.trim line in
    if String.length line > 0 && line.[0] = '@' then
      part := line
    else if String.length line > 0 && line.[0] <> '#' then begin
      let data = match String.index_opt line '#' with
        | Some pos -> String.sub line 0 pos
        | None -> line
      in
      let cols = String.split_on_char ';' data in
      match cols with
      | c1s :: c2s :: c3s :: c4s :: c5s :: _ ->
        rows := {
          line = !line_num;
          part = !part;
          c1 = parse_cps c1s;
          c2 = parse_cps c2s;
          c3 = parse_cps c3s;
          c4 = parse_cps c4s;
          c5 = parse_cps c5s;
        } :: !rows
      | _ -> ()
    end
  done with End_of_file -> ());
  close_in ic;
  List.rev !rows

let rows_path = "tools/ucd-16.0.0/NormalizationTest.txt"

let rows =
  if not (Sys.file_exists rows_path) then begin
    Printf.eprintf
      "UCD file missing: %s\n\
       Run ./tools/download_ucd.sh 16.0.0 to fetch it (see README).\n"
      rows_path;
    exit 77
  end;
  load_rows rows_path

let test_nfc () =
  List.iter (fun r ->
    let msg desc = Printf.sprintf "line %d %s: %s" r.line r.part desc in
    Alcotest.check cps_pp (msg "nfc(c1)=c2") r.c2 (Idna.nfc r.c1);
    Alcotest.check cps_pp (msg "nfc(c2)=c2") r.c2 (Idna.nfc r.c2);
    Alcotest.check cps_pp (msg "nfc(c3)=c2") r.c2 (Idna.nfc r.c3);
    Alcotest.check cps_pp (msg "nfc(c4)=c4") r.c4 (Idna.nfc r.c4);
    Alcotest.check cps_pp (msg "nfc(c5)=c4") r.c4 (Idna.nfc r.c5)
  ) rows

let () =
  Alcotest.run "nfc" [
    "NormalizationTest", [
      Alcotest.test_case "all rows" `Quick test_nfc
    ]
  ]
