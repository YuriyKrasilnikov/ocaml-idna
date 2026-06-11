(** Shared benchmark workloads.

    The benchmark deliberately keeps conformance-heavy and traffic-shaped
    workloads separate, then reports an explicit weighted score. *)

type workload = { name : string; weight : int; ops : int; run : unit -> unit }

let ignore_result x = ignore (Sys.opaque_identity x)

let run_conformance vectors =
  List.iter
    (fun v ->
      ignore_result (Idna.Uts46.to_unicode v.Test_helper.source);
      ignore_result (Idna.Uts46.to_ascii v.Test_helper.source))
    vectors

let conformance vectors =
  {
    name = "conformance_idnatestv2";
    weight = 50;
    ops = List.length vectors * 2;
    run = (fun () -> run_conformance vectors);
  }

type traffic_case = {
  case_name : string;
  case_weight : int;
  inputs : string array;
  run_input : string -> unit;
}

let ascii_domains =
  [|
    "example.com";
    "www.example.com";
    "api.service.example";
    "static-assets.example";
    "mail01.example";
    "foo-bar.example";
    "a.b.example";
    "internal-api.test";
  |]

let ascii_labels =
  [| "example"; "www"; "api"; "static-assets"; "mail01"; "foo-bar" |]

let unicode_domains =
  [|
    "ma\195\177ana.example";
    "b\195\188cher.example";
    "caf\195\169.example";
    "fa\195\159.example";
  |]

let alabel_domains =
  [| "xn--maana-pta.example"; "xn--bcher-kva.example"; "xn--caf-dma.example" |]

let invalid_domains =
  [|
    "bad..example";
    "-bad.example";
    "bad-.example";
    "xn--.example";
    String.make 64 'a' ^ ".example";
  |]

let traffic_cases =
  [
    {
      case_name = "uts46_to_ascii_ascii";
      case_weight = 20;
      inputs = ascii_domains;
      run_input = (fun s -> ignore_result (Idna.Uts46.to_ascii s));
    };
    {
      case_name = "uts46_to_unicode_ascii";
      case_weight = 15;
      inputs = ascii_domains;
      run_input = (fun s -> ignore_result (Idna.Uts46.to_unicode s));
    };
    {
      case_name = "lookup_to_ascii_ascii";
      case_weight = 15;
      inputs = ascii_domains;
      run_input = (fun s -> ignore_result (Idna.Lookup.to_ascii s));
    };
    {
      case_name = "lookup_to_unicode_ascii";
      case_weight = 10;
      inputs = ascii_domains;
      run_input = (fun s -> ignore_result (Idna.Lookup.to_unicode s));
    };
    {
      case_name = "registration_is_valid_ascii";
      case_weight = 10;
      inputs = ascii_domains;
      run_input =
        (fun s -> ignore_result (Idna.Registration.is_valid_hostname s));
    };
    {
      case_name = "registration_check_label_ascii";
      case_weight = 5;
      inputs = ascii_labels;
      run_input = (fun s -> ignore_result (Idna.Registration.check_label s));
    };
    {
      case_name = "uts46_to_ascii_unicode";
      case_weight = 10;
      inputs = unicode_domains;
      run_input = (fun s -> ignore_result (Idna.Uts46.to_ascii s));
    };
    {
      case_name = "lookup_to_ascii_unicode";
      case_weight = 5;
      inputs = unicode_domains;
      run_input = (fun s -> ignore_result (Idna.Lookup.to_ascii s));
    };
    {
      case_name = "uts46_to_unicode_alabel";
      case_weight = 5;
      inputs = alabel_domains;
      run_input = (fun s -> ignore_result (Idna.Uts46.to_unicode s));
    };
    {
      case_name = "uts46_to_ascii_invalid";
      case_weight = 5;
      inputs = invalid_domains;
      run_input = (fun s -> ignore_result (Idna.Uts46.to_ascii s));
    };
  ]

let traffic_total_weight =
  List.fold_left (fun acc case -> acc + case.case_weight) 0 traffic_cases

let traffic_cycles = 128
let traffic_ops = traffic_cycles * traffic_total_weight

let run_traffic_case cycle case =
  let len = Array.length case.inputs in
  for i = 0 to case.case_weight - 1 do
    let input = case.inputs.((cycle + i) mod len) in
    case.run_input input
  done

let run_traffic_mix () =
  for cycle = 0 to traffic_cycles - 1 do
    List.iter (run_traffic_case cycle) traffic_cases
  done

let traffic_mix =
  {
    name = "traffic_mix_weighted";
    weight = 50;
    ops = traffic_ops;
    run = run_traffic_mix;
  }

let all vectors = [ conformance vectors; traffic_mix ]

let iter_traffic_cases f =
  List.iter (fun case -> f case.case_name case.case_weight) traffic_cases
