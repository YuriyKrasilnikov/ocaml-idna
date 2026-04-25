let mapping_path =
  let rel = "tools/ucd-16.0.0/IdnaMappingTable.txt" in
  match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Filename.concat root rel
  | None -> rel

let source_deviation_ranges =
  Test_helper.require_files [mapping_path];
  Test_helper.load_uts46_status_ranges mapping_path "deviation"

let source_contains cp =
  List.exists (fun (lo, hi) -> lo <= cp && cp <= hi) source_deviation_ranges

let test_generated_deviation_exact_match () =
  let mismatches = ref [] in
  for cp = 0 to 0x10FFFF do
    let source = source_contains cp in
    let generated = Intranges.contains cp Idna_tables.uts46_deviation in
    if source <> generated && List.length !mismatches < 8 then
      mismatches :=
        Printf.sprintf "U+%04X source=%b generated=%b" cp source generated
        :: !mismatches
  done;
  match List.rev !mismatches with
  | [] -> ()
  | xs ->
    Alcotest.fail
      ("source/generated deviation mismatch: " ^ String.concat "; " xs)

let () =
  Alcotest.run "generated-vectors" [
    "uts46 deviation", [
      Alcotest.test_case "source/generated exact match" `Quick test_generated_deviation_exact_match;
    ];
  ]
