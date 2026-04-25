# Diagnostics Policy

This document turns the diagnostics audit into an explicit public
contract policy for `Idna.Diagnostics`.

It is narrower than the broader spec-conformance documents:

- [SPEC_CONFORMANCE.md](SPEC_CONFORMANCE.md)
- [DEVIATION_REGISTER.md](DEVIATION_REGISTER.md)
- [NORMATIVE_TEST_MATRIX.md](NORMATIVE_TEST_MATRIX.md)
- [TEST_EVIDENCE_AUDIT.md](TEST_EVIDENCE_AUDIT.md)

Its purpose is to define:

1. which diagnostics codes belong to which contract class
2. which stage or stages are allowed for each public code
3. which parts of diagnostics are stable, and which are not

## Contract classes

Diagnostics codes belong to one of these classes:

- **Spec-grounded reason**
  - reject-causing
  - directly grounded in RFC 3492 / RFC 5890-5893 / TR46 semantics
- **Library-defined reason**
  - reject-causing
  - observable and public, but primarily a library contract choice rather than
    a direct spec term
- **Provenance**
  - explanatory metadata about notable code points or processing decisions
  - does not reject by itself
- **Trace**
  - classification or transformation trace
  - does not reject by itself
- **Defensive**
  - non-normative runtime failure path exposed by the public diagnostics API

## Stability policy

The following are part of the public diagnostics contract:

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

The following are explicitly **not** a stability contract:

- `event.detail`
- the exact full event list order outside the ordering rules in
  [DIAGNOSTICS_ORDERING.md](DIAGNOSTICS_ORDERING.md)
- the presence or exact position of incidental informational trace beyond the
  guarantees in [DIAGNOSTICS_ORDERING.md](DIAGNOSTICS_ORDERING.md)

## Stage policy

Policy decision for the public surface:

- keep current public `Diagnostics.code` names unchanged
- allow a **small stable set of stages** for the few overloaded public codes
- do **not** split public codes in this round

This keeps the public surface stable while still making stage semantics explicit
and testable.

For every code below, the listed stage set is exhaustive. Any other stage is a
bug.

| Code | Class | Allowed stage(s) | Notes |
|---|---|---|---|
| `Empty_input` | Library-defined reason | `Input`, `Dns_length` | Empty entrypoint input and empty post-root DNS payload share one code in this contract. |
| `Empty_label` | Spec-grounded reason | `Input`, `Label_split` | Empty standalone label and empty split label share one code in this contract. |
| `Label_ascii_nr_ldh` | Trace | `Label_classification` | Classification only. |
| `Label_u_label` | Trace | `Label_classification` | Classification only. |
| `Label_a_label` | Trace | `Label_classification` | Classification only. |
| `Ascii_lowercased` | Trace | `Label_classification`, `A_label` | ASCII label path and apparent A-label path share one trace code in this contract. |
| `Label_not_nfc` | Spec-grounded reason | `Normalization` | Shared by registration, lookup, and UTS46 label validation. |
| `Hyphen_start` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 when hyphen checks are enabled. |
| `Hyphen_end` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 when hyphen checks are enabled. |
| `Hyphen_3_4` | Spec-grounded reason | `Codepoint` | Shared by registration, lookup, and UTS46 when hyphen checks are enabled. |
| `Reserved_xn_prefix` | Spec-grounded reason | `A_label` | TR46-specific reserved prefix rejection. |
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
| `Trailing_root_rejected` | Spec-grounded reason | `Label_split`, `Dns_length` | Generic split rejection and UTS46 DNS-length rejection share one code in this contract. |
| `Uts46_mapped` | Trace | `Mapping`, `Codepoint` | Informational during mapping; rejecting if a mapped code point survives validation. |
| `Uts46_ignored` | Trace | `Mapping`, `Codepoint` | Informational during mapping; rejecting if an ignored code point survives validation. |
| `Uts46_deviation` | Provenance | `Mapping` | Generated-table-backed provenance for the nontransitional deviation set. |
| `Uts46_disallowed` | Spec-grounded reason | `Codepoint` | UTS46 validity-criteria rejection. |
| `Std3_disallowed` | Spec-grounded reason | `Codepoint` | UTS46 with `use_std3_ascii_rules = true`. |
| `Ignore_invalid_punycode_applied` | Provenance | `A_label` | UTS46 fallback when `ignore_invalid_punycode = true`. |
| `Idna2008_nv8` | Provenance | `Codepoint`, `Mapping` | Strict provenance and UTS46 mapping provenance share one public code in this contract. |
| `Idna2008_xv8` | Provenance | `Codepoint`, `Mapping` | Strict provenance and UTS46 mapping provenance share one public code in this contract. |
| `Invalid_utf8` | Library-defined reason | `Utf8_decode` | Public API accepts raw OCaml strings, so byte decoding failure is observable. |
| `Serialization_failed` | Defensive | `Serialization` | Defensive runtime failure path. |

## Severity policy

- `Error`
  - contributes to rejection
  - `report.accepted = false` must imply at least one `Error`
- `Warning`
  - semantically notable, provenance-oriented, or fallback-oriented
  - does not reject by itself
- `Info`
  - neutral trace/classification fact
  - does not reject by itself
