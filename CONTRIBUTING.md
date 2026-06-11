# Contributing

Contributions should preserve the separation between normative runtime
behavior, library-defined diagnostics, and performance evidence.

## Test discipline

Product tests must check system properties or observable behavior, not the
source shape used to implement that behavior.

Allowed test classes:

- mathematical and invariant tests, such as idempotence, parity,
  representation equivalence, ordering constraints, and complexity bounds;
- behavioral contract tests, such as a concrete IDNA input producing a concrete
  Unicode result, ASCII result, error, or diagnostics report shape;
- oracle or differential tests, such as official Unicode/RFC corpora,
  generated-table exactness checks, and public-path versus diagnostics-path
  parity;
- performance and scaling tests with explicit thresholds or growth bounds.

Do not add product tests that only grep implementation text, private planning
terms, source wording, coverage of a scan, or development-process discipline. A
test that fails after a behavior-preserving refactor is usually the wrong test
unless it enforces an explicitly documented public API or architecture boundary.

## Local checks

Run the package-safe test suite:

```sh
dune build @runtest --force
```

Run the local evidence gate after downloading Unicode data:

```sh
./tools/download_ucd.sh 16.0.0
dune build @test/evidence --force
```

Run the scoped conformance gate:

```sh
dune build @test/conformance --force
```

This gate is the release-facing alias for the local evidence suite. Diagnostics
parity checks may be used for public API equivalence, but diagnostics must not
be treated as the normative RFC/TR oracle.

Run the local performance regression gate:

```sh
dune build @test/bench --force
```

## Unicode tables

Generated runtime tables are committed package artifacts. Regenerate them only
when intentionally updating table generation or Unicode data:

```sh
./tools/download_ucd.sh 16.0.0
make generate
make evidence
```

The generator requires Python with `unicodedata.unidata_version == "16.0.0"`.

## Documentation expectations

When changing runtime behavior, update the relevant evidence documents:

- `CONFORMANCE_DOSSIER.md` for normative baseline, requirement trace, evidence
  tiers, and documented exclusions

Avoid unscoped claims such as "100% spec conformant". The preferred release
wording is the scoped claim from `CONFORMANCE_DOSSIER.md`: release-verified as
100% compliant with the documented public conformance scope for Unicode 16.0.0,
with library-defined behavior and exclusions documented separately.

## Performance Changes

Performance changes should keep `dune build @test/bench --force` passing. The
local benchmark is a regression and adversarial scaling gate. A speedup or
architecture-selection claim requires a separate benchmark protocol with an
explicit workload, repeated measurements, order-bias control, noise analysis,
and native plus bytecode results from the same change set.
