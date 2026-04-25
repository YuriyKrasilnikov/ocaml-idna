# Normative Test Matrix

This matrix is the stable bridge between:

- the governing-source hierarchy in [`NORMATIVE_HIERARCHY.md`](NORMATIVE_HIERARCHY.md)
- the normative baseline in [`SPEC_CONFORMANCE.md`](SPEC_CONFORMANCE.md)
- the deviation register in [`DEVIATION_REGISTER.md`](DEVIATION_REGISTER.md)
- the final evidence pass in [`TEST_EVIDENCE_AUDIT.md`](TEST_EVIDENCE_AUDIT.md)

Unlike the audit document, this file is intentionally **not** line-oriented.
It is requirement-centered and test-centered.

Status values:

- **covered**: repository tests directly exercise the requirement
- **partially covered**: requirement is exercised, but not yet with a clean
  one-row normative test story
- **gap**: no satisfactory normative test currently identified
- **intentional exclusion**: deliberately outside default coverage under a
  documented policy
- **contract-only**: repository tests exist, but they prove API/diagnostics shape
  rather than normative behavior

Rows may cite either default `dune runtest` tests or manual/development
executables. Runner-tier details are recorded in `TEST_EVIDENCE_AUDIT.md`.

## Registration

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| Accept valid NR-LDH label | RFC 5890 / RFC 5891 | `test_idna`: `ascii labels`, `ascii case` | covered |
| Accept valid canonical A-label | RFC 5890 / RFC 5891 / RFC 3492 | `test_idna`: `xn-- valid` | covered |
| Reject uppercase / non-lowercase canonical A-label | RFC 5891 registration A-label handling | `test_idna`: `xn-- uppercase rejected`; `test_diagnostics`: `uppercase a-label rejected` | covered |
| Reject fake / non-canonical A-label | RFC 5890 / RFC 5891 | `test_idna`: `non-canonical round-trip`; `test_diagnostics`: `a-label failures` | covered |
| Reject invalid punycode A-label | RFC 3492 / RFC 5891 | `test_idna`: `invalid punycode`; `test_diagnostics`: `a-label failures` | covered |
| NFC required for U-label | RFC 5891 | `test_idna`: `nfc_qc_no`; `test_diagnostics`: `nfc failure` | covered |
| Reject leading combining mark | RFC 5891 | `test_idna`: `u-label`, `a-label` under `combiner`; `test_diagnostics`: `initial combiner` | covered |
| Hyphen restrictions | RFC 5891 | `test_idna`: `start`, `end`, `positions 3-4` | covered |
| RFC 5892 DISALLOWED rejection | RFC 5892 | `test_idna`: `soft hyphen`, `line separator`; `test_diagnostics`: `nv8 provenance + disallowed`, `xv8 provenance + disallowed` | covered |
| CONTEXTO validation in registration | RFC 5892 / Appendix rules | `test_idna`: middle dot / Greek / Hebrew / Katakana / Arabic digit cases; `test_contexto` suite | covered |
| CONTEXTJ validation in registration | RFC 5892 / RFC 5891 | `test_idna`: `contextj` group | covered |
| RFC 5893 bidi on labels that need bidi | RFC 5893 / RFC 5891 | `test_idna`: `bidi` group | covered |
| Hostname DNS length enforcement | library hostname contract | `test_idna`: `too long`, `max length`; `test_diagnostics`: `dns failures` | covered |
| Reject empty input / empty interior label in hostname surface | library hostname contract | `test_idna`: `empty`, `leading dot`, `double dot`; `test_diagnostics`: `empty input`, `empty domain label split` | covered |

## Lookup

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| Apparent A-label detection is case-insensitive | RFC 5891 Section 5.3 | `test_lookup`: `apparent a-label lowercased`; `test_diagnostics`: `uppercase a-label lowercased` | covered |
| Apparent A-label is lowercased before decode | RFC 5891 Section 5.3 | `test_lookup`: `apparent a-label lowercased`; `test_diagnostics`: `uppercase a-label lowercased` | covered |
| Lookup accepts uppercase NR-LDH via lowercase normalization | lookup library policy | `test_lookup`: `ascii nr-ldh lowercased`; `test_diagnostics`: `ascii label classification` | covered |
| Lookup rejects DISALLOWED code points | RFC 5891 Section 5.4 | `test_lookup`: `disallowed rejected` | covered |
| Lookup enforces CONTEXTJ failures | RFC 5891 Section 5.4 | `test_lookup`: `contextj enforced` | covered |
| Lookup may omit full CONTEXTO contextual tests | RFC 5891 Section 5.4 | `test_lookup`: `contexto relaxed` | covered |
| Lookup bidi enforcement enabled by default | RFC 5891 SHOULD-level behavior + library flag policy | `test_lookup`: `bidi default rejects digit label after rtl`; `test_diagnostics`: `bidi failure` | covered |
| Lookup bidi may be disabled | library policy consistent with RFC 5891 allowance | `test_lookup`: `bidi can be disabled` | covered |
| Lookup trailing root accepted | library policy | `test_diagnostics`: `trailing root present` | covered |
| Lookup does not pre-reject on DNS length before DNS lookup | RFC 5891 Section 5.4 / 5.6 | `test_lookup`: `dns length not prechecked for long label`, `dns length not prechecked for long domain`; `test_diagnostics`: `dns length not prechecked` | covered |

## UTS #46

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| Full ToUnicode conformance against `IdnaTestV2.txt` | TR46 | `test_uts46`: `toUnicode all vectors` | covered |
| Full ToASCII Nontransitional conformance against `IdnaTestV2.txt` | TR46 | `test_uts46`: `toAsciiN all vectors` | covered |
| Invalid UTF-8 preserves converted value and records error | tested `Uts46.to_unicode` repository behavior under TR46 / vector-backed coverage | `test_uts46`: `invalid utf8 lowercases ascii`; `test_diagnostics`: `invalid utf8` | covered |
| Malformed `xn--` to empty label behavior | tested `Uts46` vector-backed / regression-backed behavior (`IdnaTestV2`) | `test_uts46`: `xn-- to empty` | covered |
| `xn--ASCII-` style ASCII decode rejection | tested `Uts46` vector-backed / regression-backed behavior (`IdnaTestV2`) | `test_uts46`: `xn-- ascii hyphen`; `test_diagnostics`: `a-label failures` | covered |
| `VerifyDnsLength=false` passes trailing root | TR46 | `test_uts46`: `root label verify disabled`; `test_diagnostics`: `empty input and root rejection` partly overlaps | covered |
| `VerifyDnsLength=true` rejects trailing root | TR46 | `test_uts46`: `root label verify enabled`; `test_diagnostics`: `empty input and root rejection` | covered |
| `UseSTD3ASCIIRules` rejects underscore etc. | TR46 | `test_diagnostics`: `mapping variants` (`Std3_disallowed`) | covered |
| `IgnoreInvalidPunycode=true` warning path | TR46 | `test_diagnostics`: `a-label failures` | covered |
| `CheckHyphens=false` reserved `xn--` behavior | TR46 validity criteria | `test_uts46`: `reserved xn when hyphen checks disabled` | covered |
| `Uts46_mapped` / `ignored` / `deviation` are visible in diagnostics | library diagnostics contract over TR46 data | `test_diagnostics`: `mapping and deviation`, `mapping variants` | contract-only |

## Generated facts

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| RFC 5892-derived codepoint classes are honored by strict validation | RFC 5892 | `test_idna`, `test_contexto`, `test_idna_vectors` | covered |
| `NV8/XV8` strict-invalid behavior is exercised | TR46 metadata + strict runtime policy | `test_idna_vectors`; `test_diagnostics`: `nv8 provenance + disallowed`, `xv8 provenance + disallowed` | covered |
| UTS46 deviation comes from generated tables | architectural requirement | auto-tests: `test_generated` deviation behavior representatives for `00DF`, `03C2`, `200C`, `200D`; dev runner: `test_generated_vectors` `source/generated exact match` | covered |

## Punycode

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| RFC 3492 encode vectors | RFC 3492 | `test_punycode`: `all 19 vectors` | covered |
| RFC 3492 decode vectors | RFC 3492 | `test_punycode`: `all 19 vectors` | covered |
| round-trip behavior | RFC 3492 | `test_punycode`: roundtrip groups | covered |
| negative decode cases | RFC 3492 / implementation hardening | `test_punycode`: negative group | covered |

## NFC

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| Unicode normalization conformance | Unicode Normalization Algorithm | `test_nfc`: `all rows` over `NormalizationTest.txt` | covered |

## Diagnostics

| Requirement | Primary source | Current tests | Status |
|---|---|---|---|
| `report.accepted` mirrors public API outcome | library diagnostics contract | `test_diagnostics`: `public api parity`; `accepted matches error presence` | covered |
| deterministic event order for key paths | library diagnostics contract | `test_diagnostics`: `require_before`-based checks | partially covered |
| `Serialization_failed` remains a defensive public code | library diagnostics contract | documented as defensive/non-normative; intentionally no default-suite witness | intentional exclusion |
| `detail` is non-contractual | library diagnostics contract | documented, not matched by tests | intentional exclusion |

## Interpretation

The rows above include both:

- normative behavior status for runtime surfaces
- contract-layer status for `Diagnostics`

The `Diagnostics` rows record declared contract limits and exclusions; they do
not by themselves indicate a normative runtime defect.
