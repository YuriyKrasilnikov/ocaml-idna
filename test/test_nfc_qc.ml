let cps = Alcotest.(list int)

let check_nfc name input expected =
  Alcotest.check cps name expected (Idna.nfc input)

let check_registration_error name label expected =
  match Idna.Registration.check_label label with
  | Ok () -> Alcotest.fail (name ^ ": unexpectedly accepted")
  | Error msg -> Alcotest.(check string) name expected msg

let test_qc_yes_identity () =
  check_nfc "ascii identity"
    [ 0x0065; 0x0078; 0x0061; 0x006D; 0x0070; 0x006C; 0x0065 ]
    [ 0x0065; 0x0078; 0x0061; 0x006D; 0x0070; 0x006C; 0x0065 ];
  check_nfc "precomposed e acute identity" [ 0x00E9 ] [ 0x00E9 ]

let test_qc_non_yes_falls_back_to_full_nfc () =
  check_nfc "angstrom sign NFC_QC=N" [ 0x212B ] [ 0x00C5 ];
  check_nfc "e + combining acute NFC_QC=M" [ 0x0065; 0x0301 ] [ 0x00E9 ];
  check_registration_error "registration still rejects non-NFC label"
    "e\204\129" "label not in NFC"

let test_ccc_order_is_part_of_qc () =
  let wrong_order = [ 0x0041; 0x0300; 0x0323 ] in
  let canonical_order = [ 0x0041; 0x0323; 0x0300 ] in
  let normalized_wrong = Idna.nfc wrong_order in
  Alcotest.(check bool)
    "wrong CCC order must not be accepted unchanged" true
    (normalized_wrong <> wrong_order);
  Alcotest.check cps "wrong and ordered normalize identically"
    (Idna.nfc canonical_order) normalized_wrong

let decreasing_ccc_marks =
  [
    (240, 0x0345);
    (234, 0x0361);
    (233, 0x0362);
    (232, 0x0358);
    (230, 0x1ACE);
    (228, 0x18A9);
    (222, 0x1939);
    (220, 0x1ACA);
    (216, 0x0F39);
    (202, 0x0328);
    (132, 0x0F74);
    (130, 0x0F80);
    (129, 0x0F71);
    (122, 0x0ECB);
    (118, 0x0EB9);
    (107, 0x0E4B);
    (103, 0x0E39);
    (91, 0x0C56);
    (84, 0x0C55);
    (36, 0x0711);
    (35, 0x0670);
    (34, 0x0652);
    (33, 0x0651);
    (32, 0x0650);
    (31, 0x064F);
    (30, 0x064E);
    (29, 0x08F2);
    (28, 0x08F1);
    (27, 0x08F0);
    (25, 0x05C2);
    (24, 0x05C1);
    (23, 0x05BF);
    (22, 0x05BD);
    (21, 0x05BC);
    (20, 0x05BB);
    (19, 0x05BA);
    (18, 0x05C7);
    (17, 0x05B7);
    (16, 0x05B6);
    (15, 0x05B5);
    (14, 0x05B4);
    (13, 0x05B3);
    (12, 0x05B2);
    (11, 0x05B1);
    (10, 0x05B0);
    (9, 0x1A60);
    (7, 0x1037);
    (1, 0x0338);
  ]

let test_long_decreasing_ccc_run_is_stably_ordered () =
  let pairs = List.concat (List.init 4 (fun _ -> decreasing_ccc_marks)) in
  let input = List.map snd pairs in
  let expected =
    List.stable_sort (fun (cc_a, _) (cc_b, _) -> compare cc_a cc_b) pairs
    |> List.map snd
  in
  check_nfc "long decreasing CCC run" input expected

let test_uts46_still_normalizes_before_validation () =
  let actual = Idna.Uts46.to_unicode "e\204\129.example" in
  Alcotest.(check string)
    "uts46 normalized value" "\195\169.example" actual.value;
  Alcotest.(check bool) "uts46 normalized without error" false actual.errored

let () =
  Alcotest.run "nfc_qc"
    [
      ( "quick_check",
        [
          Alcotest.test_case "qc yes identity" `Quick test_qc_yes_identity;
          Alcotest.test_case "qc non-yes fallback" `Quick
            test_qc_non_yes_falls_back_to_full_nfc;
          Alcotest.test_case "ccc order" `Quick test_ccc_order_is_part_of_qc;
          Alcotest.test_case "long decreasing ccc run" `Quick
            test_long_decreasing_ccc_run_is_stably_ordered;
          Alcotest.test_case "uts46 normalization" `Quick
            test_uts46_still_normalizes_before_validation;
        ] );
    ]
