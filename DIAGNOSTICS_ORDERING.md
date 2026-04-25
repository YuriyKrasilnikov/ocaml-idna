# Diagnostics Ordering Policy

This document defines the ordering contract for `Idna.Diagnostics.events`.

It complements:

- [DIAGNOSTICS_POLICY.md](DIAGNOSTICS_POLICY.md)
- [DIAGNOSTICS_TEST_MATRIX.md](DIAGNOSTICS_TEST_MATRIX.md)

## Decision

The public ordering contract is **partial order**, not total order.

That means:

- the implementation still emits events in deterministic pipeline order
- only a subset of those ordering relations is part of the public stability
  contract
- exact full-list ordering outside those guarantees is intentionally not frozen

This is the highest-quality tradeoff for the current library:

- strong enough for causal and tooling-relevant guarantees
- less brittle than freezing every informational adjacency

## Ordering rules that are contractual

The following relations are part of the public contract.

### 1. Classification precedes downstream label failures

If a label-classification trace is emitted for a label, it must precede any
downstream failure for that same label.

Examples:

- `Label_a_label` before any `A_label_*` error
- `Label_u_label` before `Label_not_nfc`, `Initial_combiner`,
  `Codepoint_disallowed`, `Contextj_failed`, `Contexto_failed`, `Bidi_failed`
- `Label_ascii_nr_ldh` before downstream DNS-length failure on that label

### 2. Lowercasing trace precedes lowercase-sensitive A-label failure

If `Ascii_lowercased` and `A_label_not_lowercase_canonical` both occur in the
same report, `Ascii_lowercased` must come first.

### 3. Mapping provenance precedes UTS46 validity rejection

If mapping/provenance events are emitted for a UTS46 path and the same report
later rejects the label or domain, mapping-stage events must come first.

Examples:

- `Uts46_mapped` before `Uts46_disallowed`
- `Uts46_ignored` before `Uts46_disallowed`
- `Uts46_deviation` before any later UTS46 codepoint rejection that occurs on
  the same processing path
- `Idna2008_nv8` / `Idna2008_xv8` emitted from mapping must precede later UTS46
  codepoint rejection if both occur

### 4. Label/domain validation precedes serialization failure

If `Serialization_failed` is emitted, it must come after earlier label/domain
classification and validation events for the same operation.

In particular, `Serialization_failed` must not appear before:

- label classification events already emitted on that path
- `Trailing_root_present`
- DNS-length validation events that are reached earlier on the same path

### 5. Root-presence trace precedes root-specific rejection

If `Trailing_root_present` and `Trailing_root_rejected` both occur in the same
report, `Trailing_root_present` must come first.

## Ordering relations that are intentionally non-contractual

The following are explicitly *not* part of the public contract:

- the exact adjacency of informational trace events that do not carry causal
  meaning
- the exact order of multiple provenance warnings on the same code point when
  they are emitted within the same stage
- the full ordering of all `Info` events on a successful path
- the exact position of `event.detail`

## Negative policy

The following are forbidden as contract regressions:

- a failure event appearing before the causal trace/classification event that
  justifies it
- a mapping-stage provenance event appearing after a later codepoint-stage UTS46
  rejection on the same path
- `Serialization_failed` appearing as the first meaningful event on a path that
  has already emitted earlier classification or validation trace

## Test strategy implications

Tests should assert:

- event presence
- allowed stage membership
- required relative order via helper assertions such as `assert_before`

Tests should not assert:

- exact equality of the full `events` list order
- incidental ordering of informational trace outside the rules above
- `event.detail`
