(** UTS#46 throughput gate with explicit workload weights.

    The gate keeps the conformance-heavy IdnaTestV2 corpus and a traffic-shaped
    public API mix separate, then reports a weighted per-op score. This prevents
    a conformance corpus from hiding real-world ASCII fast-path regressions. *)

let vectors_path = "tools/ucd-16.0.0/IdnaTestV2.txt"
let runs = 5

type backend_config = {
  backend_label : string;
  measurement_repetitions : int;
  conformance_max_us : float;
  traffic_max_us : float;
  weighted_max_us : float;
}

let backend_config =
  match Sys.backend_type with
  | Sys.Native ->
      {
        backend_label = "native";
        measurement_repetitions = 4;
        conformance_max_us = 10.0;
        traffic_max_us = 2.8;
        weighted_max_us = 5.0;
      }
  | Sys.Bytecode ->
      {
        backend_label = "bytecode";
        measurement_repetitions = 1;
        conformance_max_us = 220.0;
        traffic_max_us = 60.0;
        weighted_max_us = 135.0;
      }
  | Sys.Other name ->
      {
        backend_label = name;
        measurement_repetitions = 1;
        conformance_max_us = 5_000.0;
        traffic_max_us = 5_000.0;
        weighted_max_us = 5_000.0;
      }

type result = {
  workload : Bench_workloads.workload;
  ops : int;
  times : float array;
  median : float;
  per_op_us : float;
}

let median times =
  let sorted = Array.copy times in
  Array.sort compare sorted;
  sorted.(Array.length sorted / 2)

let measure (workload : Bench_workloads.workload) =
  let times = Array.make runs 0.0 in
  let ops = workload.ops * backend_config.measurement_repetitions in
  for i = 0 to runs - 1 do
    Gc.compact ();
    let t0 = Unix.gettimeofday () in
    for _ = 1 to backend_config.measurement_repetitions do
      workload.Bench_workloads.run ()
    done;
    let t1 = Unix.gettimeofday () in
    times.(i) <- t1 -. t0
  done;
  let median = median times in
  let per_op_us = median *. 1_000_000.0 /. float_of_int ops in
  { workload; ops; times; median; per_op_us }

let runs_str times =
  Array.to_list times |> List.map (Printf.sprintf "%.3fs") |> String.concat " "

let print_traffic_weights () =
  Printf.printf "bench_uts46_traffic_weights total=%d cycles=%d"
    Bench_workloads.traffic_total_weight Bench_workloads.traffic_cycles;
  Bench_workloads.iter_traffic_cases (fun name weight ->
      Printf.printf " %s=%d" name weight);
  print_newline ()

let print_result result =
  let w = result.workload in
  Printf.printf
    "bench_uts46_suite name=%s weight=%d ops=%d runs=%d repetitions=%d \
     backend=%s times=[%s] median=%.3fs per_op=%.2fµs\n"
    w.name w.weight result.ops runs backend_config.measurement_repetitions
    backend_config.backend_label (runs_str result.times) result.median
    result.per_op_us

let weighted_per_op_us results =
  let total_weight =
    List.fold_left
      (fun acc result -> acc + result.workload.Bench_workloads.weight)
      0 results
  in
  let weighted_sum =
    List.fold_left
      (fun acc result ->
        acc
        +. result.per_op_us
           *. float_of_int result.workload.Bench_workloads.weight)
      0.0 results
  in
  weighted_sum /. float_of_int total_weight

let print_gate name ~actual ~threshold =
  let margin = actual -. threshold in
  let pass = actual <= threshold in
  Printf.printf
    "bench_uts46_gate name=%s backend=%s actual=%.2fµs threshold=%.2fµs \
     margin=%+.2fµs result=%s\n"
    name backend_config.backend_label actual threshold margin
    (if pass then "PASS" else "FAIL");
  pass

let utf8_of_cps cps = Idna.Utf8.of_cps cps

let repeat n xs =
  let rec loop acc n =
    if n = 0 then acc else loop (List.rev_append xs acc) (n - 1)
  in
  List.rev (loop [] n)

let adversarial_contexto_label n =
  utf8_of_cps (repeat n [ 0x006C; 0x00B7 ] @ [ 0x006C ])

let adversarial_contextj_label n =
  utf8_of_cps (repeat n [ 0x0628; 0x200C ] @ [ 0x0628 ])

let measure_adversarial_label ?(iterations = 16) name label =
  let times = Array.make runs 0.0 in
  for i = 0 to runs - 1 do
    Gc.compact ();
    let t0 = Unix.gettimeofday () in
    for _ = 1 to iterations do
      match Idna.Registration.check_label label with
      | Ok () -> ()
      | Error e -> failwith (name ^ " unexpectedly failed: " ^ e)
    done;
    let t1 = Unix.gettimeofday () in
    times.(i) <- t1 -. t0
  done;
  let median = median times in
  let per_op_us = median *. 1_000_000.0 /. float_of_int iterations in
  Printf.printf
    "bench_uts46_adversarial name=%s bytes=%d runs=%d iterations=%d backend=%s \
     times=[%s] median=%.3fs per_op=%.2fµs\n"
    name (String.length label) runs iterations backend_config.backend_label
    (runs_str times) median per_op_us;
  per_op_us

let adversarial_scaling_sizes = [ 256; 512; 1024 ]
let adversarial_scaling_ratio_max = 3.25
let adversarial_iterations n = max 8 (131072 / n)

let measure_adversarial_scaling name make_label =
  let measurements =
    List.map
      (fun n ->
        let label = make_label n in
        let per_op_us =
          measure_adversarial_label ~iterations:(adversarial_iterations n)
            (Printf.sprintf "%s_n%d" name n)
            label
        in
        (n, per_op_us))
      adversarial_scaling_sizes
  in
  let rec check pass = function
    | (n1, t1) :: ((n2, t2) :: _ as rest) ->
        let ratio = t2 /. t1 in
        let pair_pass = ratio <= adversarial_scaling_ratio_max in
        Printf.printf
          "bench_uts46_adversarial_scaling name=%s from_n=%d to_n=%d \
           per_op_ratio=%.2f threshold=%.2f result=%s\n"
          name n1 n2 ratio adversarial_scaling_ratio_max
          (if pair_pass then "PASS" else "FAIL");
        check (pass && pair_pass) rest
    | _ -> pass
  in
  check true measurements

let () =
  Test_helper.require_files [ vectors_path ];
  let vectors = Test_helper.load_idna_test_vectors vectors_path in
  print_traffic_weights ();
  let workloads = Bench_workloads.all vectors in
  let results = List.map measure workloads in
  List.iter print_result results;
  let conformance, traffic =
    match results with
    | [ conformance; traffic ] -> (conformance, traffic)
    | _ -> failwith "Bench_workloads.all must return conformance then traffic"
  in
  let weighted = weighted_per_op_us results in
  let conformance_pass =
    print_gate conformance.workload.name ~actual:conformance.per_op_us
      ~threshold:backend_config.conformance_max_us
  in
  let traffic_pass =
    print_gate traffic.workload.name ~actual:traffic.per_op_us
      ~threshold:backend_config.traffic_max_us
  in
  let weighted_pass =
    print_gate "weighted" ~actual:weighted
      ~threshold:backend_config.weighted_max_us
  in
  let pass = conformance_pass && traffic_pass && weighted_pass in
  Printf.printf
    "bench_uts46_weighted workloads=%d backend=%s weighted_per_op=%.2fµs \
     threshold_per_op=%.2fµs weighted_margin=%+.2fµs result=%s\n"
    (List.length results) backend_config.backend_label weighted
    backend_config.weighted_max_us
    (weighted -. backend_config.weighted_max_us)
    (if pass then "PASS" else "FAIL");
  let contexto_scaling_pass =
    measure_adversarial_scaling "contexto_many_middle_dots"
      adversarial_contexto_label
  in
  let contextj_scaling_pass =
    measure_adversarial_scaling "contextj_many_joiners"
      adversarial_contextj_label
  in
  if not (pass && contexto_scaling_pass && contextj_scaling_pass) then exit 1
