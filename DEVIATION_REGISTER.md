# Deviation Register

This file records confirmed mismatches, audit status, and library-defined
surfaces that should not be mistaken for normative RFC / TR requirements.

Status values:

- **match**: looks aligned with audited normative text
- **mismatch**: confirmed divergence from the normative baseline
- **open**: needs deeper audit before being classified
- **library-defined**: outside direct normative obligation for this API shape

## Confirmed runtime mismatches

This register records **no confirmed runtime mismatch** in the audited core
paths.

## Unresolved runtime items

This register records **no unresolved runtime audit item** requiring separate
normative classification.

## Library-defined surfaces

### 1. Hostname-style surfaces vs protocol primitives

- Surface:
  - `Idna.Registration.is_valid_hostname`
  - parts of trailing-root and whole-domain behavior in `Registration`
- Status:
  - **library-defined**
- Reason:
  - These are useful API surfaces but not literal named operations from RFC
    5891. They should be judged by coherence with the normative model, not by
    one-to-one textual equivalence.

### 2. Diagnostics contract exhaustiveness

- Surface:
  - `Idna.Diagnostics`
- Status:
  - **library-defined**
- Reason:
  - Diagnostics is intentionally structured explainability. Its codes and stages
    are not normative RFC / TR vocabulary. The requirement is that diagnostics
    not contradict runtime semantics.

## Recently fixed items now believed aligned

These items had previously been identified as real or likely problems and are
now treated as fixed pending deeper regression evidence:

- strict A-label canonicality / lowercase canonical form
- explicit separation of `Registration`, `Lookup`, and `Uts46`
- lookup no longer pre-rejects on DNS length prior to DNS resolution
- UTS #46 deviation status moved to generated tables
- UTS #46 handling of malformed `xn--` labels in tested regression cases
- UTS #46 handling of invalid UTF-8 in tested regression cases
- strict vector handling of `NV8/XV8` as explicit invalid cases instead of
  silent skip

## Connected stable artifacts

- governing-source hierarchy:
  - `NORMATIVE_HIERARCHY.md`
- normative baseline:
  - `SPEC_CONFORMANCE.md`
- normative test coverage:
  - `NORMATIVE_TEST_MATRIX.md`
- final evidence/status pass:
  - `TEST_EVIDENCE_AUDIT.md`

## Interpretation

This register treats diagnostics/documentation refinement as separate from
runtime mismatch tracking. A runtime change belongs here only if it is justified
as a proved `mismatch`.
