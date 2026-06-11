# Performance

This document describes the repository performance checks for `ocaml-idna`.
The numbers here are benchmark evidence for this repository workload, not a
portable latency guarantee.

## Local UTS #46 benchmark

Run the local benchmark gate with:

```sh
dune build @test/bench --force
```

The gate runs the package-safe test suite first and then reports:

- a conformance-heavy `IdnaTestV2.txt` workload
- a traffic-shaped public API mix
- a 50/50 weighted score
- adversarial CONTEXTO and CONTEXTJ scaling checks, reported separately from
  the weighted score but still enforced as part of the benchmark gate

Current throughput thresholds are intentionally loose regression thresholds:

| Backend | Conformance max | Traffic max | Weighted max |
|---|---:|---:|---:|
| native | 10.0 us/op | 2.8 us/op | 5.0 us/op |
| bytecode | 220.0 us/op | 60.0 us/op | 135.0 us/op |

The adversarial scaling checks run crafted CONTEXTO and CONTEXTJ labels at
multiple sizes. Each doubling must stay below the configured growth-ratio
threshold. This is a behavioral complexity guard: it checks that contextual
validation does not drift back toward repeated whole-label scans.

Use the numbers printed by the benchmark run being evaluated. Do not treat
older local measurements as current release evidence after code, compiler,
runner, or workload changes. For performance-sensitive changes, record native
and bytecode/32-bit output from the same change set.

These measurements require the local Unicode data files under
`tools/ucd-16.0.0/`.

## Scope Of Performance Claims

This gate is intentionally not an optimization-decision benchmark. It should
not be used to claim small speedups, rank candidate implementations, or compare
against other libraries. Those claims need a separate benchmark protocol with
an explicit workload, repeated measurements, order-bias control, noise
analysis, and native plus bytecode results from the same change set.

Do not use any performance result unless correctness and diagnostics parity
gates have already passed. A change that is faster but alters public output,
diagnostics acceptance, diagnostics output shape, or diagnostics event order is
invalid before performance is considered.
