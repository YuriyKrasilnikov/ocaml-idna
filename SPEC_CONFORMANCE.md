# Spec Conformance Matrix

This document is the normative baseline for `ocaml-idna`.

It sits inside the repository document set:

- [`NORMATIVE_HIERARCHY.md`](NORMATIVE_HIERARCHY.md) fixes governing-source
  precedence by public surface
- this document states the normative baseline and architectural boundary
- [`NORMATIVE_TEST_MATRIX.md`](NORMATIVE_TEST_MATRIX.md) records stable
  requirement-to-test status
- [`DEVIATION_REGISTER.md`](DEVIATION_REGISTER.md) records confirmed deviations
  and library-defined surfaces

Its purpose is to separate:

- **generated facts** derived from Unicode / IDNA data files
- **semantic orchestration** that must follow RFC / TR prose
- **library-defined contract** that is intentionally outside direct normative text

This file defines the normative baseline against which implementation and tests
are judged.

## Sources

Primary normative sources used by this repository:

- RFC 3492: Punycode
- RFC 5890: terminology and label categories
- RFC 5891: IDNA2008 protocol, registration, lookup
- RFC 5892: derived property algorithm
- RFC 5893: bidirectional rule
- Unicode UTS #46 / TR46: compatibility processing
- Unicode Normalization Algorithm + versioned Unicode data files

Versioned Unicode data in this repository is pinned to **Unicode 16.0.0**.

## Architectural Boundary

### Generated facts

These should come from `tools/gen_tables.py` and generated tables, not from
hand-maintained lists in runtime code:

- RFC 5892-derived codepoint classes:
  - `PVALID`
  - `CONTEXTJ`
  - `CONTEXTO`
  - disallowed remainder
- UTS #46 status and mapping data:
  - `valid`
  - `mapped`
  - `ignored`
  - `deviation`
- UTS #46 metadata:
  - `NV8`
  - `XV8`
- Unicode property support data:
  - normalization support
  - joining type
  - bidi class support inputs
  - scripts / blocks as needed for context rules

### Hand-written semantic orchestration

These cannot be honestly generated from tables alone and must be implemented by
runtime code:

- registration vs lookup behavior split
- A-label / U-label / NR-LDH orchestration
- domain splitting and recomposition
- exact ordering of mapping, normalization, decoding, validation, encoding
- DNS length enforcement behavior
- trailing-root behavior
- invalid punycode handling policy
- bidi application at label/domain level

### Library-defined contract

These are not normative RFC/TR concepts:

- public module layout:
  - `Idna.Registration`
  - `Idna.Lookup`
  - `Idna.Uts46`
  - `Idna.Diagnostics`
- error message strings
- diagnostics event names, stages, severities, and report shape
- exact choice of boolean vs result-returning API per operation

## Public API Matrix

## `Idna.Registration`

### `Registration.check_label`

**Normative basis**

- RFC 5890 terminology for:
  - A-label
  - U-label
  - NR-LDH label
- RFC 5891 registration requirements
- RFC 5892 codepoint validity derivation
- RFC 5893 label bidi rule when the label contains RTL codepoints

**Normative requirements**

- Accept canonical A-labels, valid NR-LDH labels, and valid U-labels.
- Enforce A-label symmetry / canonicality.
- Enforce NFC for U-labels.
- Enforce hyphen restrictions.
- Reject initial combining mark.
- Enforce RFC 5892 codepoint validity.
- Enforce `CONTEXTJ` / `CONTEXTO` contextual rules.
- Enforce RFC 5893 bidi criteria on labels that require bidi handling.

**Library-defined / out of scope**

- concrete error strings
- diagnostics event naming
- whether the function returns `unit result` instead of a richer type
- DNS hostname length: this function is label-scoped and does **not** by itself
  imply whole-domain length validation

### `Registration.to_ascii`

**Normative basis**

- same label rules as `Registration.check_label`
- RFC 3492 for ACE/Punycode encoding

**Normative requirements**

- Validate every label under strict registration rules before encoding.
- Lowercase ASCII labels canonically.
- Encode non-ASCII labels as canonical A-labels.
- Reject non-canonical or invalid A-label inputs.

**Library-defined / out of scope**

- exact error string
- whether output is returned as `string result`

### `Registration.to_unicode`

**Normative basis**

- same label rules as `Registration.check_label`
- RFC 3492 for A-label decoding

**Normative requirements**

- Validate every label under strict registration rules.
- Decode canonical A-labels to Unicode.
- Lowercase ASCII labels canonically.
- Reject fake / non-canonical / invalid A-labels.

**Library-defined / out of scope**

- exact output formatting for diagnostics

### `Registration.is_valid_hostname`

**Normative basis**

- same label rules as registration validation
- DNS hostname size constraints as part of the library hostname contract

**Normative requirements**

- Split domain into labels and validate each under registration rules.
- Reject empty input.
- Reject empty interior labels.
- Reject trailing root in registration hostname validation.
- Enforce per-label DNS length.
- Enforce total hostname DNS length when enabled.

**Library-defined / out of scope**

- boolean return shape
- exact hostname flag surface

## `Idna.Lookup`

### `Lookup.to_ascii`

**Normative basis**

- RFC 5891 lookup behavior
- RFC 3492 for Punycode encoding/decoding
- RFC 5892 validity classes
- RFC 5893 bidi guidance where lookup applies bidi checking

**Normative requirements**

- Support apparent A-label input by lowercasing before decode/validation.
- Preserve strict IDNA2008 lookup-oriented behavior rather than UTS #46 mapping.
- Apply contextual rules as required by lookup semantics.

**Library-defined choices**

- `check_bidi` flag surface
- exact strictness of lookup-side bidi enforcement when the flag is enabled
- exact result shape and error strings

### `Lookup.to_unicode`

**Normative basis**

- same as `Lookup.to_ascii`, but Unicode output path

**Normative requirements**

- Lowercase apparent A-labels before decode.
- Decode and validate under lookup semantics.
- Return canonical Unicode form on success.

**Library-defined choices**

- exact output/result shape
- whether specific relaxed behaviors are exposed as flags or fixed policy

## `Idna.Uts46`

### `Uts46.to_ascii`

**Normative basis**

- UTS #46 Nontransitional processing
- TR46 validity criteria
- RFC 3492 for Punycode encoding

**Normative requirements**

- Perform UTS #46 mapping first.
- Normalize to NFC.
- Split on recognized dot variants.
- Validate labels according to TR46 validity criteria and active flags.
- Encode non-ASCII labels as A-labels.
- Respect:
  - `check_hyphens`
  - `check_bidi`
  - `check_joiners`
  - `use_std3_ascii_rules`
  - `verify_dns_length`
  - `ignore_invalid_punycode`

**Library-defined choices**

- exact error strings
- plain `Uts46.to_ascii` error-string granularity
- diagnostics event model

The repository keeps `Uts46.to_ascii` plain errors intentionally coarser than
the structured diagnostics surface. Branch-specific explanation belongs to
`Idna.Diagnostics.Uts46`, not to the plain `Error msg` string alone.

### `Uts46.to_unicode`

**Normative basis**

- UTS #46 Nontransitional processing
- TR46 ToUnicode behavior

**Normative requirements**

- Perform mapping and normalization before validation.
- Decode apparent A-labels per UTS #46 processing.
- Always return a public result that contains a converted value.
- Indicate validation failure separately from that converted value.

**Library-defined choices**

- concrete return record shape `{ value; errored }`
- diagnostics event model

## Shared primitives

### `Idna.Punycode`

**Normative basis**

- RFC 3492

**Normative requirements**

- Encode and decode according to RFC 3492.
- Preserve lowercase canonical output for ACE use.

### `Idna.nfc`

**Normative basis**

- Unicode Normalization Algorithm
- versioned Unicode normalization data files

**Normative requirements**

- canonical decomposition
- canonical ordering of combining marks
- canonical composition
- Hangul algorithmic behavior

## Diagnostics status

`Idna.Diagnostics` is **not** itself a normative API from RFC/TR prose.

It is expected to satisfy two constraints:

1. It must not contradict the normative runtime behavior.
2. It should make the runtime decision explainable in a structured way.

Current intended contract:

- `report.accepted` mirrors the corresponding non-diagnostics API outcome.
- `Error` events correspond to rejecting conditions in runtime behavior.
- `Warning` and `Info` are explanatory/provenance layers.
- `detail` text is non-contractual.

Examples of provenance facts that may appear in diagnostics but are not
themselves primary normative truth:

- `Idna2008_nv8`
- `Idna2008_xv8`
- `Uts46_deviation`

## What this document is for

This file should be used as the baseline for:

1. spec-to-code audit
2. deviation register
3. normative test matrix
4. requirement trace matrix
5. runtime fixes grounded in proven mismatches

It should **not** be used as proof that current implementation is already fully
conformant. That requires a separate deviation audit.
