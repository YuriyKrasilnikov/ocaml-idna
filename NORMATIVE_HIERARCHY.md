# Normative Hierarchy

This document fixes the governing-spec hierarchy for `ocaml-idna`.

It is narrower than [`SPEC_CONFORMANCE.md`](SPEC_CONFORMANCE.md): the purpose
here is not to restate all requirements, but to answer one question cleanly for
each public surface:

**which source governs this behavior first, which sources are secondary, and
which parts are intentionally library-defined?**

This hierarchy is the first gate before any new implementation work.

## Source precedence

When sources overlap, the repository should use the following precedence model.

1. **RFC 3492** governs Punycode encoding/decoding.
2. **RFC 5890** governs IDNA terminology:
   - NR-LDH
   - A-label
   - U-label
3. **RFC 5891** governs strict IDNA2008 protocol behavior for:
   - registration
   - lookup
4. **RFC 5892** governs derived codepoint validity classes.
5. **RFC 5893** governs bidi conditions when bidi handling is applicable.
6. **TR46 / UTS #46** governs compatibility processing only:
   - mapping
   - ignored/deviation/disallowed handling
   - TR46 flags and validity criteria
7. **Unicode Normalization Algorithm + pinned Unicode 16.0.0 data** govern NFC.
8. **Pinned Unicode source files downloaded under `tools/ucd-16.0.0/`** govern
   regeneration inputs such as `IdnaMappingTable.txt`; generated runtime tables
   are the committed package artifact.

Corollary:

- `Registration` and `Lookup` are governed by the RFC 589x family, not by TR46.
- `Uts46` is governed by TR46, not by strict RFC 5891 registration/lookup
  semantics.
- `Diagnostics` is not itself a normative RFC/TR API. It is a library-defined
  explainability layer that must not contradict the governing runtime surface.

## Architectural hierarchy

The repository is expected to respect the following layered split.

### Generated facts

These are governed first by Unicode / IDNA source data and must come from
`tools/gen_tables.py` and generated tables, not from hand-maintained runtime
lists:

- RFC 5892-derived codepoint classes
- TR46 mapping status:
  - `valid`
  - `mapped`
  - `ignored`
  - `deviation`
  - `disallowed`
- TR46 metadata:
  - `NV8`
  - `XV8`
- property support data used by:
  - normalization checks
  - contextual rules
  - bidi handling

### Hand-written semantic orchestration

These are governed by RFC/TR prose and must remain explicit in runtime code:

- registration vs lookup split
- label classification
- A-label / U-label / NR-LDH orchestration
- domain splitting / trailing root policy
- bidi application at label/domain level
- serialization ordering
- hostname-level DNS length policy

### Library-defined contract

These are intentionally outside direct normative text:

- module layout
- result/boolean/report return shapes
- diagnostics event names, stages, and severities
- exact error strings

## Governing-source matrix

| Public surface | Governing sources | Secondary sources | Library-defined remainder |
|---|---|---|---|
| `Idna.Registration.check_label` | RFC 5890, RFC 5891, RFC 5892, RFC 5893 | RFC 3492 for A-label decode/encode symmetry | error strings, diagnostics representation |
| `Idna.Registration.to_ascii` | RFC 5891, RFC 5892, RFC 5893, RFC 3492 | RFC 5890 terminology | result shape, error strings |
| `Idna.Registration.to_unicode` | RFC 5891, RFC 5892, RFC 5893, RFC 3492 | RFC 5890 terminology | result shape, error strings |
| `Idna.Registration.is_valid_hostname` | registration semantics + library hostname contract | RFC 5891 registration semantics | boolean shape, whole-domain policy surface |
| `Idna.Lookup.to_ascii` | RFC 5891, RFC 5892, RFC 5893, RFC 3492 | RFC 5890 terminology | flags, exact strictness surface around bidi |
| `Idna.Lookup.to_unicode` | RFC 5891, RFC 5892, RFC 5893, RFC 3492 | RFC 5890 terminology | flags, result shape |
| `Idna.Uts46.to_ascii` | TR46, RFC 3492 | pinned `IdnaMappingTable.txt`, Unicode normalization data | diagnostics, error strings |
| `Idna.Uts46.to_unicode` | TR46 | pinned `IdnaMappingTable.txt`, Unicode normalization data | record shape `{ value; errored }` |
| `Idna.Punycode.encode` / `decode` | RFC 3492 | none | concrete error strings |
| `Idna.nfc` | Unicode Normalization Algorithm, pinned Unicode 16.0.0 data | none | API exposure shape |
| `Idna.Diagnostics.*` | library-defined contract over runtime semantics | may expose provenance from TR46 source data | the entire event model is library-defined |

## Surface-specific notes

### Registration

`Registration` should be read as the strict IDNA2008 surface. When a TR46 rule
would differ, RFC 5891/5892/5893 wins here.

### Lookup

`Lookup` should be read as strict lookup-oriented IDNA2008 behavior. It is not a
TR46 compatibility layer. Relaxations that remain in this surface must be
traceable to lookup text or to an explicit library policy.

### Uts46

`Uts46` should be read as a separate compatibility-processing surface. Generated
TR46 data and TR46 prose govern it first. It must not silently inherit
registration-only assumptions.

### Diagnostics

`Diagnostics` is downstream of the runtime surfaces above. Its obligations are:

1. mirror the outcome of the corresponding non-diagnostics API
2. preserve the documented `code -> allowed stage(s)` policy
3. preserve the documented partial-order contract

It does **not** need to mirror RFC/TR vocabulary literally.

## Decision gate

Any implementation change should be able to name:

1. the governing source from this hierarchy
2. the current code path implementing it
3. the tests that currently prove it

If one of those three is missing, that work should stay in research/audit phase.
