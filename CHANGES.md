# Changes

## Unreleased

- Improved UTS #46, IDNA2008, Punycode, and NFC evidence documentation.
  The repository now documents the public conformance scope, requirement trace,
  diagnostics policy, the performance regression gate, contribution workflow,
  and security reporting expectations.

- Added the scoped local conformance gate:

  ```sh
  dune build @test/conformance --force
  ```

  This alias runs the local evidence suite: package-safe unit/regression tests,
  Unicode/UTS #46 corpus runners, generated-vector checks, and generated table
  exactness checks. The gate requires local Unicode data files and is separate
  from package `dune runtest`.

- Added a local UTS #46 benchmark regression and adversarial scaling gate.
  It runs the package-safe tests before reporting conformance-heavy,
  traffic-shaped, weighted, and adversarial scaling rows. These numbers are
  regression evidence for the evaluated change set, not portable latency
  guarantees and not speedup proof.

- Improved generated Unicode table support. The generator emits packed
  codepoint property data for UTS #46 status, IDNA class, Bidi class, canonical
  combining class, joining type, CONTEXTO script bits, mark category, NFC Quick
  Check, NV8/XV8 provenance, and canonical decomposition presence. The compiled
  generated tables are checked back against Unicode 16.0.0 and UTS #46 source
  data for both 32-bit and 64-bit table backends.

- Refined UTS #46 public processing without changing public API shape or
  diagnostics contracts. The public no-diagnostics path now has dedicated ASCII
  and A-label handling, single-pass UTF-8 decoding with UTS #46 mapping,
  reduced redundant label validation, and a more direct common-path data flow.
  Treat performance impact as benchmark-gate evidence for the evaluated change
  set, not as a release-note guarantee.

- Improved contextual validation internals without changing public semantics.
  CONTEXTO validation now builds one per-label summary for neighbor, script,
  and Arabic-digit facts instead of repeatedly walking the label. CONTEXTJ
  validation uses a shared internal helper for the public UTS #46 path and the
  diagnostics path.

- Improved NFC internals while preserving Unicode NormalizationTest behavior.
  NFC Quick Check avoids unnecessary full normalization when the sequence is
  already NFC, canonical ordering handles long non-starter runs robustly, and
  fallback normalization uses an internal growable array buffer with in-place
  composition/compaction before returning the public list result.

- Improved the UTS #46 `to_ascii` slow path by collecting encoded parts,
  trailing-root state, and per-label length state in one pass.

- Added explicit regeneration commands for both generated table backends:

  ```sh
  make generate
  make evidence
  ```

  Table regeneration requires Python with Unicode 16.0.0 data.

No public runtime API change is recorded in this entry.
