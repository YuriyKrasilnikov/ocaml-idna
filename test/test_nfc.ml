(* NFC conformance tests from Unicode NormalizationTest.txt.

   UAX #15 conformance for NFC:
     c2 == toNFC(c1) == toNFC(c2) == toNFC(c3)
     c4 == toNFC(c4) == toNFC(c5)

   5 NFC assertions per test line, ~19965 lines = ~100000 checks. *)

let parse_cps s =
  let s = String.trim s in
  if String.length s = 0 then []
  else
    String.split_on_char ' ' s
    |> List.map (fun hex -> int_of_string ("0x" ^ hex))

let cps_to_string cps =
  String.concat " " (List.map (Printf.sprintf "%04X") cps)

let () =
  let path = "tools/ucd-16.0.0/NormalizationTest.txt" in
  let ic = open_in path in
  let total = ref 0 in
  let pass = ref 0 in
  let fail = ref 0 in
  let fail_examples = Buffer.create 1024 in
  let fail_count = ref 0 in
  let line_num = ref 0 in
  let part = ref "" in
  (try while true do
    let line = input_line ic in
    incr line_num;
    let line = String.trim line in
    if String.length line > 0 && line.[0] = '@' then
      part := line
    else if String.length line > 0 && line.[0] <> '#' then begin
      (* Strip trailing comment *)
      let data = match String.index_opt line '#' with
        | Some pos -> String.sub line 0 pos
        | None -> line
      in
      let cols = String.split_on_char ';' data in
      match cols with
      | c1s :: c2s :: c3s :: c4s :: c5s :: _ ->
        let c1 = parse_cps c1s in
        let c2 = parse_cps c2s in
        let c3 = parse_cps c3s in
        let c4 = parse_cps c4s in
        let c5 = parse_cps c5s in
        let check desc input expected =
          incr total;
          let result = Idna.nfc input in
          if result = expected then
            incr pass
          else begin
            incr fail;
            if !fail_count < 20 then begin
              Buffer.add_string fail_examples
                (Printf.sprintf "  line %d %s: %s: nfc(%s) = %s, expected %s\n"
                   !line_num !part desc
                   (cps_to_string input) (cps_to_string result) (cps_to_string expected));
              incr fail_count
            end
          end
        in
        (* c2 == toNFC(c1) *)
        check "toNFC(c1)==c2" c1 c2;
        (* c2 == toNFC(c2) *)
        check "toNFC(c2)==c2" c2 c2;
        (* c2 == toNFC(c3) *)
        check "toNFC(c3)==c2" c3 c2;
        (* c4 == toNFC(c4) *)
        check "toNFC(c4)==c4" c4 c4;
        (* c4 == toNFC(c5) *)
        check "toNFC(c5)==c4" c5 c4
      | _ -> ()
    end
  done with End_of_file -> ());
  close_in ic;
  Printf.printf "NormalizationTest NFC results:\n";
  Printf.printf "  total:  %d assertions\n" !total;
  Printf.printf "  pass:   %d\n" !pass;
  Printf.printf "  fail:   %d\n" !fail;
  Printf.printf "  rate:   %.1f%%\n"
    (if !total > 0 then 100.0 *. float_of_int !pass /. float_of_int !total else 0.0);
  if !fail > 0 then begin
    Printf.printf "\nFirst %d failures:\n" (min !fail 20);
    print_string (Buffer.contents fail_examples)
  end
