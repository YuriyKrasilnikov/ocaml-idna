# Diagnostics Test Matrix

This document maps the public diagnostics contract to the current test suite in
`test/test_diagnostics.ml`.

It is intentionally diagnostics-specific and complements:

- [DIAGNOSTICS_POLICY.md](DIAGNOSTICS_POLICY.md)
- [DIAGNOSTICS_ORDERING.md](DIAGNOSTICS_ORDERING.md)
- [TEST_EVIDENCE_AUDIT.md](TEST_EVIDENCE_AUDIT.md)

## Current status

- All public `Diagnostics.code` except `Serialization_failed` are covered by the
  default test suite.
- `Serialization_failed` remains part of the public contract, but it is
  intentionally left outside default-suite coverage because it is a defensive,
  non-normative runtime path.
- Contract-level parity with the non-diagnostics public API is covered.
- The `code -> allowed stage(s)` policy is checked against a representative
  report matrix, including additional A-label and context witnesses.
- Representative forbidden `code -> stage` combinations are checked for the
  public codes that already have stable witnesses, including both overloaded
  multi-stage codes and single-stage codes with clean regression points.
- The partial-order contract is checked through representative ordering
  assertions, including mapping-provenance-before-rejection paths, not full-list
  total-order snapshots.

## Test mapping

| Test case | Primary coverage | Stage / ordering implications |
|---|---|---|
| `empty label` | `Empty_label` | Covers `Input` stage variant. |
| `empty input` | `Empty_input` | Covers `Input` stage variant. |
| `ascii label classification` (registration) | `Label_ascii_nr_ldh` | Covers classification trace. |
| `u-label classification` | `Label_u_label` | Covers classification trace and representative forbidden-stage regression for a single-stage code. |
| `nfc failure` | `Label_not_nfc` | Covers `Normalization`. |
| `hyphen failures` | `Hyphen_start`, `Hyphen_end`, `Hyphen_3_4` | Covers `Codepoint`. |
| `initial combiner` | `Initial_combiner` | Covers `Codepoint`. |
| `context failures` | `Contextj_failed`, `Contexto_failed` | Covers `Context` and representative `Label_u_label -> Context*` ordering. |
| `bidi failure` (registration) | `Bidi_failed` | Covers registration-side `Bidi` and representative `Label_u_label -> Bidi_failed` ordering. |
| `nv8 provenance + disallowed` | `Idna2008_nv8`, `Codepoint_disallowed` | Covers provenance + strict rejection and representative ordering. |
| `xv8 provenance + disallowed` | `Idna2008_xv8`, `Codepoint_disallowed` | Covers provenance + strict rejection. |
| `uppercase a-label rejected` | `Label_a_label`, `Ascii_lowercased`, `A_label_not_lowercase_canonical` | Covers `A_label`-path lowercasing and representative ordering. |
| `a-label failures` (registration) | `A_label_non_ascii`, `A_label_trailing_hyphen`, `A_label_invalid_punycode`, `A_label_decodes_to_ascii`, `A_label_not_canonical` | Covers `A_label` and representative `Label_a_label -> A_label_*` ordering. |
| `dns failures` | `Dns_label_too_long`, `Dns_domain_too_long` | Covers `Dns_length` and representative `Label_ascii_nr_ldh -> Dns_*` ordering. |
| `empty domain label split` | `Empty_label` | Covers `Label_split` stage variant. |
| `uppercase a-label lowercased` (lookup) | `Label_a_label`, `Ascii_lowercased` | Covers lookup-side lowercasing trace. |
| `ascii label classification` (lookup) | `Label_ascii_nr_ldh` | Covers lookup classification trace. |
| `bidi failure` (lookup) | `Bidi_failed` | Covers lookup-side `Bidi`. |
| `trailing root present` | `Trailing_root_present` | Covers root trace and representative forbidden-stage regression for a single-stage code. |
| `dns length not prechecked` | absence of `Dns_label_too_long` / `Dns_domain_too_long` on lookup | Negative contract check for lookup path. |
| `mapping and deviation` | `Uts46_mapped`, `Uts46_deviation` | Covers `Mapping`, representative ordering, and the absence of invalid `Codepoint`-stage `Uts46_deviation` on that witness. |
| `mapping variants` | `Uts46_mapped`, `Uts46_ignored`, `Uts46_deviation`, `Uts46_disallowed`, `Std3_disallowed`, `Idna2008_nv8`, `Idna2008_xv8` | Covers `Mapping` and `Codepoint` variants of UTS46 path, including provenance/ignored/deviation before later rejection. |
| `a-label failures` (UTS46) | `A_label_non_ascii`, `A_label_invalid_punycode`, `A_label_decodes_to_ascii`, `Ignore_invalid_punycode_applied` | Covers UTS46 A-label behavior, representative classification-before-failure ordering, and single-stage provenance regression checks. |
| `reserved xn prefix` | `Reserved_xn_prefix` | Covers TR46 reserved prefix rejection, representative A-label-path ordering, and single-stage regression checks. |
| `invalid utf8` | `Invalid_utf8` | Covers `Utf8_decode`. |
| `empty input and root rejection` | `Empty_input`, `Trailing_root_rejected` | Covers `Dns_length` variant of `Empty_input` and `Trailing_root_rejected`. |
| `public api parity` | `report.accepted` parity | Cross-checks diagnostics with non-diagnostics public API. |
| `metadata fields` | `policy`, `operation`, `output` metadata | Contract-level metadata coverage. |
| `accepted matches error presence` | `accepted` / `Error` invariant | Contract-level invariant. |
| `stage policy matrix` | `code -> allowed stage(s)` | Cross-checks every event in representative reports against `DIAGNOSTICS_POLICY.md`. |
| `representative forbidden stages` | negative `code -> stage` contract | Ensures public codes do not leak into forbidden stage variants on their existing witnesses. |
| `ordering policy` | partial-order contract | Cross-checks representative causal order rules from `DIAGNOSTICS_ORDERING.md`. |

## Declared exclusions

This matrix records one deliberate exclusion from default-suite witness
coverage:

- `Serialization_failed`

## What tests should not do

Diagnostics tests should not:

- match `event.detail`
- require total ordering of the full `events` list
- infer stability from incidental ordering of unrelated informational trace
