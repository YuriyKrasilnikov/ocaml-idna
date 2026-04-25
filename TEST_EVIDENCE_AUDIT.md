# Test Evidence Audit

This document is the final evidence pass over the repository test story.

It complements:

- `SPEC_CONFORMANCE.md`
- `NORMATIVE_HIERARCHY.md`
- `NORMATIVE_TEST_MATRIX.md`
- `DIAGNOSTICS_TEST_MATRIX.md`
- `DEVIATION_REGISTER.md`

Its purpose is not to restate every row from the matrices, but to answer the
audit question:

**what is proved by tests, what is contract-only, and what is intentionally
excluded?**

## Status vocabulary

This audit uses the same effective status language already present in the
repository:

- **normative covered**
- **contract-only**
- **partially covered**
- **gap**
- **library-defined**

## Test Runner Tiers

This document cites both default tests and manual/development evidence
executables.

The default `dune runtest` suite is the set of tests declared in the `(tests
...)` stanza:

- `test_punycode`
- `test_idna`
- `test_lookup`
- `test_utf8`
- `test_contexto`
- `test_diagnostics`
- `test_generated`

The following executables are evidence runners, but they are not part of the
default `dune runtest` suite unless invoked explicitly:

- `test_idna_vectors`
- `test_nfc`
- `test_uts46`
- `test_generated_vectors`

## 1. Normative-covered runtime surfaces

The following areas have direct or strong audited normative evidence.

## Registration

Normative-covered:

- valid NR-LDH acceptance
- canonical A-label acceptance
- uppercase/non-lowercase canonical A-label rejection
- fake/non-canonical A-label rejection
- invalid punycode A-label rejection
- NFC enforcement
- hyphen restrictions
- initial combining mark rejection
- RFC 5892 `DISALLOWED`
- `CONTEXTJ`
- `CONTEXTO`
- bidi on labels that require bidi

Library-defined but covered:

- hostname DNS length enforcement
- empty input / empty interior label hostname surface

Evidence basis:

- `test_idna`
- `test_contexto`
- `test_diagnostics`

## Lookup

Normative-covered:

- apparent A-label detection is case-insensitive
- apparent A-label lowercasing before decode
- `DISALLOWED` rejection
- `CONTEXTJ` enforcement
- relaxed `CONTEXTO` treatment under the audited lookup model
- no DNS pre-reject before DNS lookup

Library-defined but covered:

- uppercase NR-LDH normalization
- bidi flag surface
- trailing-root allowance

Evidence basis:

- `test_lookup`
- `test_diagnostics`

## UTS46

Normative-covered:

- full ToUnicode conformance against `IdnaTestV2.txt`
- full ToASCII Nontransitional conformance against `IdnaTestV2.txt`
- mapping before later validation
- invalid UTF-8 converted-value behavior
- malformed `xn--` regressions
- `CheckHyphens=false` reserved `xn--`
- `UseSTD3ASCIIRules`
- `IgnoreInvalidPunycode`
- trailing-root behavior under `verify_dns_length`

Contract-only but covered:

- diagnostics visibility of `Uts46_mapped` / `Uts46_ignored` / `Uts46_deviation`

Evidence basis:

- `test_uts46`
- `test_diagnostics`
- `test_generated`
- `test_generated_vectors`

## Generated data boundary

Normative/architectural-covered:

- RFC 5892-derived classes are honored
- `NV8/XV8` strict-invalid behavior is exercised
- `deviation` comes from generated tables

Evidence basis:

- `test_idna_vectors`
- `test_contexto`
- `test_generated`
- `test_generated_vectors`
- `test_diagnostics`

## Low-level primitives

Normative-covered:

- RFC 3492 punycode encode vectors
- RFC 3492 punycode decode vectors
- punycode round-trip behavior
- punycode negative/boundary behavior
- Unicode normalization conformance

Evidence basis:

- `test_punycode`
- `test_nfc`

## 2. Diagnostics contract evidence

Diagnostics is not a normative RFC/TR API. Its status is therefore tracked as
contract evidence, not normative evidence.

Currently covered:

- `report.accepted` parity with public API
- `accepted` / `Error` invariant
- representative `code -> allowed stage(s)` checks
- representative forbidden `code -> stage` checks for both overloaded and
  single-stage public codes with stable witnesses
- representative partial-order rules, including classification-before-failure
  and provenance-before-rejection paths
- metadata fields (`policy`, `operation`, `output`)
- all public `Diagnostics.code` except `Serialization_failed` in the default
  suite

Evidence basis:

- `test_diagnostics`

Classification:

- **contract-only**, not normative-covered

## 3. Partial coverage by design

There is one meaningful `partially covered` area:

- deterministic event order for key diagnostics paths

- representative `require_before` assertions exist
- the repository deliberately does **not** freeze total order for full event
  lists

Interpretation:

- this is not a runtime conformance weakness
- it is the expected shape of a partial-order diagnostics contract

## 4. Intentional exclusions

The intentional exclusions are:

1. `Serialization_failed`
   - public diagnostics code
   - defensive, non-normative path
   - intentionally left outside default-suite witness coverage

2. `event.detail`
   - documented as non-contractual
   - intentionally not asserted in tests

These are **policy-backed intentional exclusions**, not forgotten holes.

## 5. No proved runtime mismatch in audited core paths

This audit does **not** identify a proved runtime mismatch in:

- `Registration`
- `Lookup`
- `Uts46`
- generated facts
- `Punycode`
- `nfc`

## 6. Final evidence summary

Repository status:

- runtime normative core: **covered**
- generated-data boundary: **covered**
- low-level primitives: **covered**
- diagnostics layer: **contract-only, mostly covered**
- declared exclusions: **intentional diagnostics-policy exclusions**
