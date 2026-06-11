# Diagnostics Policy

This document defines the public contract for `Idna.Diagnostics`.

Diagnostics is a library-defined explainability surface over the normative
runtime behavior. Diagnostics codes and stages are public API, but they are not
themselves RFC/UTS vocabulary and must not be treated as a separate conformance
oracle.

## Contract Classes

Diagnostics codes belong to one of these classes:

- **Spec-grounded reason**: reject-causing and grounded in RFC 3492,
  RFC 5890-5893, or UTS #46 semantics.
- **Library-defined reason**: reject-causing and observable, but primarily a
  library API decision rather than a direct specification term.
- **Provenance**: explanatory metadata about notable code points or processing
  decisions; does not reject by itself.
- **Trace**: classification or transformation trace; does not reject by itself.
- **Defensive**: non-normative runtime failure path exposed by the public
  diagnostics API.

## Stability Contract

The following are stable public diagnostics fields:

- `report.policy`
- `report.operation`
- `report.accepted`
- `report.output` shape and semantic meaning
- `event.severity`
- `event.stage`
- `event.code`
- `event.label_index`
- `event.cp_index`
- `event.cp`
- the `code -> allowed stage(s)` matrix below
- the relative ordering rules documented below

The following are explicitly not stable:

- `event.detail`
- exact full-list event order outside the ordering rules below
- incidental informational trace presence or adjacency

## Stage Matrix

For every code below, the listed stage set is exhaustive. Any other stage is a
bug.

| Code | Class | Allowed stage(s) | Notes |
|---|---|---|---|
| `Empty_input` | Library-defined reason | `Input`, `Dns_length` | Empty entrypoint input and empty post-root DNS payload share one code. |
| `Empty_label` | Spec-grounded reason | `Input`, `Label_split` | Empty standalone label and empty split label share one code. |
| `Label_ascii_nr_ldh` | Trace | `Label_classification` | Classification only. |
| `Label_u_label` | Trace | `Label_classification` | Classification only. |
| `Label_a_label` | Trace | `Label_classification` | Classification only. |
| `Ascii_lowercased` | Trace | `Label_classification`, `A_label` | ASCII label path and apparent A-label path share one trace code. |
| `Label_not_nfc` | Spec-grounded reason | `Normalization` | Shared by registration, lookup, and UTS46 label validation. |
| `Hyphen_start` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 when hyphen checks are enabled. |
| `Hyphen_end` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 when hyphen checks are enabled. |
| `Hyphen_3_4` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 when hyphen checks are enabled. |
| `Reserved_xn_prefix` | Spec-grounded reason | `A_label` | UTS #46 reserved prefix rejection. |
| `Initial_combiner` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 label validation. |
| `Codepoint_disallowed` | Spec-grounded reason | `Codepoint` | Strict IDNA2008 path. |
| `Contextj_failed` | Spec-grounded reason | `Context` | Shared by registration, lookup, and UTS46 when joiner checks are enabled. |
| `Contexto_failed` | Spec-grounded reason | `Context` | Registration only. |
| `Bidi_failed` | Spec-grounded reason | `Bidi` | Shared by registration, lookup, and UTS46 when bidi checks are enabled. |
| `A_label_non_ascii` | Spec-grounded reason | `A_label` | Apparent A-label must stay ASCII. |
| `A_label_trailing_hyphen` | Spec-grounded reason | `A_label` | Registration/lookup A-label branch. |
| `A_label_invalid_punycode` | Spec-grounded reason | `A_label` | Includes empty payload and decode failures. |
| `A_label_decodes_to_ascii` | Spec-grounded reason | `A_label` | Fake A-label case. |
| `A_label_not_canonical` | Spec-grounded reason | `A_label` | Round-trip mismatch after decode/re-encode. |
| `A_label_not_lowercase_canonical` | Spec-grounded reason | `A_label` | Registration-only stricter lowercase canonicality. |
| `Dns_label_too_long` | Spec-grounded reason | `Dns_length` | Registration and UTS46 only. |
| `Dns_domain_too_long` | Spec-grounded reason | `Dns_length` | Registration and UTS46 only. |
| `Trailing_root_present` | Trace | `Label_split` | Informational only. |
| `Trailing_root_rejected` | Spec-grounded reason | `Label_split`, `Dns_length` | Generic split rejection and UTS46 DNS-length rejection share one code. |
| `Uts46_mapped` | Trace | `Mapping`, `Codepoint` | Informational during mapping; rejecting if a mapped code point survives validation. |
| `Uts46_ignored` | Trace | `Mapping`, `Codepoint` | Informational during mapping; rejecting if an ignored code point survives validation. |
| `Uts46_deviation` | Provenance | `Mapping` | Generated-table-backed provenance for the nontransitional deviation set. |
| `Uts46_disallowed` | Spec-grounded reason | `Codepoint` | UTS #46 validity-criteria rejection. |
| `Std3_disallowed` | Spec-grounded reason | `Codepoint` | UTS #46 with `use_std3_ascii_rules = true`. |
| `Ignore_invalid_punycode_applied` | Provenance | `A_label` | UTS #46 fallback when `ignore_invalid_punycode = true`. |
| `Idna2008_nv8` | Provenance | `Codepoint`, `Mapping` | Strict provenance and UTS #46 mapping provenance share one public code. |
| `Idna2008_xv8` | Provenance | `Codepoint`, `Mapping` | Strict provenance and UTS #46 mapping provenance share one public code. |
| `Invalid_utf8` | Library-defined reason | `Utf8_decode` | Public APIs accept raw OCaml strings, so byte decoding failure is observable. |
| `Serialization_failed` | Defensive | `Serialization` | Defensive runtime failure path. |

## Severity Policy

- `Error` contributes to rejection; `report.accepted = false` must imply at
  least one `Error`.
- `Warning` is semantically notable or provenance-oriented and does not reject
  by itself.
- `Info` is neutral trace/classification data and does not reject by itself.

## Ordering Contract

The public ordering contract is a partial order, not a total order. The
implementation emits deterministic pipeline-order events, but only the causal
relations below are stable.

Stable relative ordering rules:

- Label classification, when emitted, precedes downstream failures for the same
  label. Examples: `Label_a_label` before `A_label_*`, `Label_u_label` before
  NFC/codepoint/context/bidi failures.
- `Ascii_lowercased` precedes `A_label_not_lowercase_canonical` when both occur
  in the same report.
- UTS #46 mapping/provenance events precede later validity rejection on the same
  processing path. Examples: `Uts46_mapped`, `Uts46_ignored`,
  `Uts46_deviation`, `Idna2008_nv8`, and `Idna2008_xv8` before the later
  codepoint-stage rejection they explain.
- Label/domain validation events that were reached earlier precede
  `Serialization_failed`.
- `Trailing_root_present` precedes `Trailing_root_rejected` when both occur in
  the same report.

Non-contractual ordering:

- exact adjacency of unrelated informational trace
- exact order of multiple provenance warnings in the same stage
- full ordering of all `Info` events on successful paths
- position or text of `event.detail`

Forbidden regressions:

- failure event before its causal trace/classification event
- mapping-stage provenance after a later codepoint-stage UTS #46 rejection on
  the same path
- `Serialization_failed` as the first meaningful event after earlier
  classification or validation was reached

## Test Coverage Policy

`test/test_diagnostics.ml` is the default-suite contract test for diagnostics.
It covers:

- all public `Diagnostics.code` values except `Serialization_failed`
- diagnostics/public API parity for accepted/rejected outcomes
- `report.accepted` matching the presence or absence of `Error` events
- metadata fields (`policy`, `operation`, `output`)
- the `code -> allowed stage(s)` matrix on representative reports
- representative forbidden stage combinations
- the partial-order rules above through relative-order assertions

`Serialization_failed` is intentionally excluded from default-suite witness
coverage because it is a defensive runtime failure path, not a normative
IDNA/UTS #46 condition.

Diagnostics tests should not assert:

- `event.detail`
- total equality of the full event list
- incidental ordering of unrelated informational trace
