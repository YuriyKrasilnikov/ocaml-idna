let check_ascii_ok ?(flags = Idna.Uts46.default_flags) input expected =
  match Idna.Uts46.to_ascii ~flags input with
  | Ok actual -> Alcotest.(check string) input expected actual
  | Error msg ->
    Alcotest.fail
      (Printf.sprintf "Uts46.to_ascii %S: expected Ok %S, got Error %S"
         input expected msg)

let test_deviation_behavior_sharp_s () =
  check_ascii_ok "Fa\xc3\x9f.de" "xn--fa-hia.de"

let test_deviation_behavior_final_sigma () =
  check_ascii_ok "\xce\xb2\xcf\x8c\xce\xbb\xce\xbf\xcf\x82" "xn--nxasmm1c"

let test_deviation_behavior_zwnj_preserved () =
  check_ascii_ok "a\xe0\xa5\x8d\xe2\x80\x8cb" "xn--ab-fsf604u"

let test_deviation_behavior_zwj_preserved () =
  check_ascii_ok "a\xe0\xa5\x8d\xe2\x80\x8db" "xn--ab-fsf014u"

let () =
  Alcotest.run "generated" [
    "uts46 deviation", [
      Alcotest.test_case "sharp s preserved" `Quick test_deviation_behavior_sharp_s;
      Alcotest.test_case "final sigma preserved" `Quick test_deviation_behavior_final_sigma;
      Alcotest.test_case "zwnj preserved" `Quick test_deviation_behavior_zwnj_preserved;
      Alcotest.test_case "zwj preserved" `Quick test_deviation_behavior_zwj_preserved;
    ];
  ]
