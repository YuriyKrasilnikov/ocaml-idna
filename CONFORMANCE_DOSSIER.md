# Conformance Dossier

This document is the single repository source for the public conformance claim,
governing sources, requirement trace, evidence tiers, and documented exclusions.

## Public Claim

`ocaml-idna` is release-verified as **100% compliant with its documented public
conformance scope** for Unicode 16.0.0.

The documented scope is:

- `Idna.Punycode`: RFC 3492 Punycode encode/decode behavior.
- `Idna.nfc`: Unicode NFC normalization using Unicode 16.0.0 data.
- `Idna.Registration`: strict IDNA2008 registration-oriented validation and
  conversion exposed by this library.
- `Idna.Lookup`: strict IDNA2008 lookup-oriented conversion exposed by this
  library.
- `Idna.Uts46`: UTS #46 Nontransitional processing exposed by this library.

The claim explicitly excludes:

- phishing, confusable, or registry policy beyond IDNA validity;
- portable performance guarantees;
- exact human-readable error string text;
- `Idna.Diagnostics` as a normative RFC/TR API.

Avoid unscoped wording such as "100% spec conformant". The supported shorthand
must include the documented public conformance scope.

## Governing Sources

When sources overlap, repository behavior follows this precedence:

1. RFC 3492 governs Punycode.
2. RFC 5890 governs IDNA terminology and label categories.
3. RFC 5891 governs strict IDNA2008 registration and lookup behavior.
4. RFC 5892 governs derived codepoint validity classes.
5. RFC 5893 governs Bidi conditions.
6. UTS #46 governs compatibility processing only.
7. Unicode Normalization Algorithm plus pinned Unicode 16.0.0 data governs NFC.
8. Pinned Unicode source files under `tools/ucd-16.0.0/` govern regeneration
   inputs; generated runtime tables are the committed package artifact.

Surface split:

- `Registration` and `Lookup` are strict IDNA2008 surfaces, not UTS #46.
- `Uts46` is the UTS #46 compatibility-processing surface.
- `Diagnostics` is a library-defined explainability layer over runtime
  semantics. It must mirror public outcomes but does not need to use RFC/TR
  vocabulary literally.

## Architectural Boundary

Generated facts come from `tools/gen_tables.py` and generated tables, not from
hand-maintained runtime lists:

- RFC 5892-derived codepoint classes;
- UTS #46 status and mapping data;
- UTS #46 `NV8` / `XV8` metadata;
- property support data for normalization, contextual rules, and Bidi handling.

Hand-written semantic orchestration remains runtime code:

- registration vs lookup split;
- A-label / U-label / NR-LDH orchestration;
- domain splitting and recomposition;
- ordering of mapping, normalization, decoding, validation, and encoding;
- DNS length and trailing-root policy;
- invalid Punycode handling;
- Bidi application at label/domain level.

Library-defined contract includes:

- module layout and return shapes;
- diagnostics event names, stages, severities, and report shape;
- exact error strings.

## Evidence Tiers

Package-safe default tests:

```sh
dune runtest
```

Default tests cover representative unit/regression behavior for Punycode,
Registration, Lookup, UTF-8, CONTEXTO, diagnostics, generated table smoke tests,
ASCII fast-path parity, cross-surface public/diagnostics invariants, value
idempotence invariants, and NFC Quick Check witnesses.

Full local evidence gate:

```sh
dune build @test/evidence --force
```

This gate depends on the default test alias and additionally runs local evidence
executables that require Unicode data under `tools/ucd-16.0.0/`:

- `test_idna_vectors`
- `test_nfc`
- `test_uts46`
- `test_generated_vectors`
- `dump_generated_tables` checked by `tools/generated_exact_check.py`

Scoped release conformance gate:

```sh
dune build @test/conformance --force
```

`@test/conformance` is the release-facing alias for the full local evidence
gate. It does not add a second hidden suite; it names the evidence suite used to
support the documented public conformance claim.

Performance regression gate:

```sh
dune build @test/bench --force
```

The benchmark gate is performance evidence only. It is not normative conformance
evidence.

## Requirement Trace

| ID | Requirement | Source | Public surface | Implementation anchor | Evidence | Status |
|---|---|---|---|---|---|---|
| PUNY-1 | Decode RFC Punycode examples | RFC 3492 Section 7.1 | `Idna.Punycode.decode` | `lib/punycode.ml` | `test_punycode`: all decode vectors | covered_by_unit_tests |
| PUNY-2 | Encode RFC Punycode examples | RFC 3492 Section 7.1 | `Idna.Punycode.encode` | `lib/punycode.ml` | `test_punycode`: all encode vectors | covered_by_unit_tests |
| PUNY-3 | Preserve round-trip behavior and lowercase ACE output | RFC 3492 | `Idna.Punycode`, A-label paths | `lib/punycode.ml`, public A-label orchestration | `test_punycode`, `test_idna` | covered_by_unit_tests |
| PUNY-4 | Reject malformed, overflow, non-scalar, and invalid-character inputs | RFC 3492 + Unicode scalar boundary | `Idna.Punycode` | `lib/punycode.ml` | `test_punycode` | covered_by_unit_tests |
| NFC-1 | Canonical decomposition, ordering, and composition | Unicode Normalization Algorithm | `Idna.nfc`, UTS #46 normalization | `lib/nfc.ml`, generated NFC data | `test_nfc`: all `NormalizationTest.txt` rows | covered_by_official_vectors |
| NFC-2 | NFC Quick Check fast path preserves full NFC semantics | Unicode normalization data | `Idna.nfc`, UTS #46 normalization | `lib/nfc.ml` | `test_nfc_qc`, `test_nfc` | covered_by_official_vectors |
| NFC-3 | Hangul algorithmic composition works | Unicode Normalization Algorithm | `Idna.nfc` | `lib/nfc.ml` | `test_nfc` | covered_by_official_vectors |
| IDNA0-1 | A-label, U-label, and NR-LDH labels are separated | RFC 5890 | `Registration`, `Lookup`, diagnostics classification | `lib/idna.ml`, diagnostics core | `test_idna`, `test_lookup`, `test_diagnostics` | covered_by_unit_tests |
| IDNA0-2 | Canonical A-label symmetry is enforced | RFC 5890 / RFC 5891 | `Registration`, `Lookup`, `Uts46` | public A-label orchestration | `test_idna`, `test_lookup`, `test_uts46` | covered_by_unit_tests |
| REG-1 | Registration accepts valid NR-LDH labels | RFC 5891 | `Idna.Registration` | `lib/idna.ml` | `test_idna` | covered_by_unit_tests |
| REG-2 | Registration accepts valid canonical A-labels | RFC 5891 / RFC 3492 | `Idna.Registration` | `lib/idna.ml`, `lib/punycode.ml` | `test_idna`, `test_idna_vectors` | covered_by_generated_vectors |
| REG-3 | Registration rejects uppercase or non-canonical A-labels | RFC 5891 | `Idna.Registration` | `lib/idna.ml` | `test_idna`, `test_diagnostics` | covered_by_unit_tests |
| REG-4 | Registration enforces NFC for U-labels | RFC 5891 | `Idna.Registration` | `lib/idna.ml`, `lib/nfc.ml` | `test_idna`, `test_nfc_qc` | covered_by_unit_tests |
| REG-5 | Registration enforces hyphen restrictions and initial-combiner rejection | RFC 5891 | `Idna.Registration` | `lib/idna.ml`, diagnostics core | `test_idna`, `test_diagnostics` | covered_by_unit_tests |
| REG-6 | Registration enforces RFC 5892 codepoint classes | RFC 5892 | `Idna.Registration` | generated props and runtime validation | `test_idna`, `test_idna_vectors`, generated exactness | covered_by_generated_vectors |
| REG-7 | Registration enforces CONTEXTJ and CONTEXTO rules | RFC 5892 Appendix A | `Idna.Registration` | `lib/contextj.ml`, `lib/contexto.ml`, diagnostics core | `test_contexto`, `test_idna` | covered_by_unit_tests |
| REG-8 | Registration enforces RFC 5893 Bidi label rules | RFC 5893 | `Idna.Registration` | `lib/bidi.ml` | `test_idna` | covered_by_unit_tests |
| LOOK-1 | Lookup lowercases apparent A-labels before decode | RFC 5891 Section 5.3 | `Idna.Lookup` | `lib/idna.ml` | `test_lookup`, `test_diagnostics` | covered_by_unit_tests |
| LOOK-2 | Lookup rejects DISALLOWED and CONTEXTJ failures | RFC 5891 / RFC 5892 | `Idna.Lookup` | `lib/idna.ml`, context helpers | `test_lookup`, `test_diagnostics` | covered_by_unit_tests |
| LOOK-3 | Lookup policy for CONTEXTO relaxation is explicit | RFC 5891 allowance + library policy | `Idna.Lookup` | `lib/idna.ml` | `test_lookup` | library_defined |
| LOOK-4 | Lookup Bidi checking is enabled by default and can be disabled | RFC 5891 guidance + library flag | `Idna.Lookup` | `lib/idna.ml`, `lib/bidi.ml` | `test_lookup` | covered_by_unit_tests |
| LOOK-5 | Lookup does not pre-reject DNS length before DNS lookup | RFC 5891 Sections 5.4 / 5.6 | `Idna.Lookup` | `lib/idna.ml` | `test_lookup` | covered_by_unit_tests |
| UTS46-1 | ToUnicode matches `IdnaTestV2.txt` for Nontransitional processing | UTS #46 | `Idna.Uts46.to_unicode` | `lib/uts46_public.ml` | `test_uts46`: all vectors | covered_by_official_vectors |
| UTS46-2 | ToASCII Nontransitional matches `IdnaTestV2.txt` | UTS #46 | `Idna.Uts46.to_ascii` | `lib/uts46_public.ml` | `test_uts46`: all `toAsciiN` vectors | covered_by_official_vectors |
| UTS46-3 | UTS #46 status and mapping data match Unicode 16.0.0 source data | UTS #46 data files | `Idna.Uts46`, diagnostics | generated runtime tables | `test_generated_vectors`, `tools/generated_exact_check.py` | covered_by_generated_vectors |
| UTS46-4 | `check_hyphens` flag is preserved, including reserved `xn--` behavior | UTS #46 validity criteria | `Idna.Uts46` | `lib/uts46_public.ml` | `test_uts46`, `test_ascii_fast_path` | covered_by_unit_tests |
| UTS46-5 | `check_bidi` flag is preserved | UTS #46 / RFC 5893 | `Idna.Uts46` | `lib/uts46_public.ml`, `lib/bidi.ml` | `test_diagnostics` | covered_by_unit_tests |
| UTS46-6 | `check_joiners` flag is preserved | UTS #46 / RFC 5892 CONTEXTJ | `Idna.Uts46` | `lib/uts46_public.ml`, context helpers | `test_diagnostics` | covered_by_unit_tests |
| UTS46-7 | `use_std3_ascii_rules` flag controls STD3 ASCII rejection | UTS #46 | `Idna.Uts46` | `lib/uts46_public.ml` | `test_diagnostics` | covered_by_unit_tests |
| UTS46-8 | `verify_dns_length` flag controls DNS length and trailing-root validation | UTS #46 | `Idna.Uts46` | `lib/uts46_public.ml` | `test_uts46`, `test_ascii_fast_path` | covered_by_unit_tests |
| UTS46-9 | `ignore_invalid_punycode` flag preserves converted output policy | UTS #46 | `Idna.Uts46` | `lib/uts46_public.ml` | `test_diagnostics` | covered_by_unit_tests |
| FAST-1 | ASCII and A-label fast paths match non-fast-path public behavior | Library optimization invariant | `Registration`, `Lookup`, `Uts46` | public wrappers and diagnostics engine | `test_ascii_fast_path`, `test_surface_invariants` | covered_by_differential |
| DNS-1 | Registration hostname DNS length behavior is documented and tested | DNS hostname constraints + library policy | `Idna.Registration` | `lib/idna.ml`, diagnostics core | `test_idna`, `test_diagnostics` | covered_by_unit_tests |
| DIAG-1 | Diagnostics report shape mirrors runtime outcomes | Library contract | `Idna.Diagnostics` | `lib/diagnostics*.ml` | `test_diagnostics`, `test_surface_invariants` | library_defined |
| DIAG-2 | Diagnostics ordering is a partial-order contract | Library contract | `Idna.Diagnostics.events` | diagnostics emit order | `test_diagnostics`, `DIAGNOSTICS_POLICY.md` | library_defined |
| DIAG-3 | `Serialization_failed` is defensive and outside the normative claim | Library contract | `Idna.Diagnostics` | serialization fallback path | `DIAGNOSTICS_POLICY.md` | intentional_exclusion |
| ERR-1 | Exact human-readable error strings are not normative | Library contract | string-returning APIs | public wrappers | docs and diagnostics contract | library_defined |

## Coverage Summary

Normative-covered runtime surfaces:

- Registration: NR-LDH, canonical A-labels, uppercase/non-canonical/invalid
  Punycode rejection, NFC, hyphen restrictions, initial combiner, RFC 5892
  disallowed codepoints, CONTEXTJ, CONTEXTO, Bidi, hostname DNS/empty-label
  policy.
- Lookup: apparent A-label lowercasing, disallowed rejection, CONTEXTJ,
  documented CONTEXTO relaxation, Bidi default/disabled behavior, trailing-root
  allowance, and no DNS pre-reject.
- UTS #46: full ToUnicode and ToASCII Nontransitional vector coverage against
  `IdnaTestV2.txt`, plus focused flag and malformed-input regressions.
- Generated data: representative tests plus source/generated exactness checks
  for the committed generated table libraries.
- Punycode: RFC vectors, round trips, and negative/scalar-boundary cases.
- NFC: `NormalizationTest.txt` and focused quick-check/composition witnesses.

Diagnostics is contract-covered, not normative-covered. It is tested for public
API parity, accepted/error invariants, representative code-to-stage policy, and
partial ordering. The repository intentionally does not freeze total event-list
order for every possible input.

## Deviations And Exclusions

No confirmed runtime mismatch is currently recorded in the audited core paths:

- `Registration`
- `Lookup`
- `Uts46`
- generated facts
- `Punycode`
- `nfc`

Library-defined surfaces:

- hostname-style helper behavior;
- diagnostics event model;
- exact plain error strings;
- lookup-side CONTEXTO relaxation policy;
- Bidi flag surfaces;
- `Uts46.to_unicode` result record shape.

Intentional diagnostics exclusions:

- `Serialization_failed` is a defensive public diagnostics code, not a
  normative IDNA/UTS #46 condition. It is outside default-suite witness
  coverage by policy.
- `event.detail` is explanatory text only and is not a stability contract.

This dossier is test and audit evidence, not a mechanized formal proof of every
possible input and every normative clause. Stronger language requires stronger
formal evidence than this repository currently claims.
