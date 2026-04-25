# ocaml-idna

Internationalized Domain Names for OCaml (IDNA 2008 and UTS #46).

Pure OCaml implementation of IDNA2008 hostname validation, UTS #46 compatibility processing, Punycode encoding/decoding, and Unicode NFC normalization. No C dependencies. Unicode 16.0.0.

## Installation

```
opam install idna
```

## Semantic layers

The library exposes three separate semantic layers:

- `Idna.Registration`: strict IDNA2008 registration validation
- `Idna.Lookup`: strict IDNA2008 lookup preparation/conversion
- `Idna.Uts46`: UTS #46 Nontransitional compatibility processing

This separation is intentional. `Registration`, `Lookup`, and `Uts46` do not
share exactly the same acceptance and conversion rules.

If you are migrating from the earlier top-level API, choose the layer that
matches the operation you need:

- `Idna.check_label` -> `Idna.Registration.check_label`
- `Idna.is_valid_hostname` -> `Idna.Registration.is_valid_hostname`
- `Idna.to_ascii` -> `Idna.Uts46.to_ascii` for compatibility processing
- `Idna.to_unicode` -> `Idna.Uts46.to_unicode` for compatibility processing

The old top-level names are not part of the current public API. `Registration`,
`Lookup`, and `Uts46` intentionally have different result shapes and policy
flags where their governing semantics differ.

For the normative baseline of these layers, see [SPEC_CONFORMANCE.md](SPEC_CONFORMANCE.md).
For governing-source precedence, see [NORMATIVE_HIERARCHY.md](NORMATIVE_HIERARCHY.md).
For runtime/test evidence status, see [TEST_EVIDENCE_AUDIT.md](TEST_EVIDENCE_AUDIT.md).
For open and confirmed deviations, see [DEVIATION_REGISTER.md](DEVIATION_REGISTER.md).
For requirement-to-test coverage, see [NORMATIVE_TEST_MATRIX.md](NORMATIVE_TEST_MATRIX.md).
For diagnostics policy, ordering, and diagnostics-specific test coverage, see
[DIAGNOSTICS_POLICY.md](DIAGNOSTICS_POLICY.md),
[DIAGNOSTICS_ORDERING.md](DIAGNOSTICS_ORDERING.md), and
[DIAGNOSTICS_TEST_MATRIX.md](DIAGNOSTICS_TEST_MATRIX.md).

## Usage

```ocaml
(* Strict registration validation *)
Idna.Registration.is_valid_hostname "example.com"    (* true *)
Idna.Registration.is_valid_hostname "-invalid.com"   (* false *)
Idna.Registration.check_label "xn--maana-pta"        (* Ok () *)
Idna.Registration.check_label "XN--MAANA-PTA"
(* Error "... lowercase canonical ..." *)

(* Strict lookup conversion *)
Idna.Lookup.to_ascii "XN--MAANA-PTA.com"   (* Ok "xn--maana-pta.com" *)
Idna.Lookup.to_unicode "XN--MAANA-PTA.com" (* Ok "mañana.com" *)

(* UTS #46 compatibility processing *)
Idna.Uts46.to_ascii "Königsgäßchen.example"
(* Ok "xn--knigsgchen-b4a3dun.example" *)
Idna.Uts46.to_unicode "xn--maana-pta.com"
(* { value = "mañana.com"; errored = false } *)
(* Uts46.to_unicode always returns { value; errored }.
   Uts46.to_ascii returns Ok value | Error msg instead.
   Those plain Uts46.to_ascii error strings are intentionally coarser than
   Diagnostics.Uts46, which is the structured explanatory surface. *)

(* Structured diagnostics *)
let report = Idna.Diagnostics.Registration.check_label "/" in
report.accepted           (* false *)
List.map Idna.Diagnostics.string_of_code
  (List.map (fun e -> e.Idna.Diagnostics.code) report.events)
(* ["label_ascii_nr_ldh"; "idna2008_nv8"; "codepoint_disallowed"] *)

(* Punycode *)
Idna.Punycode.decode "maana-pta"         (* Ok [0x6D; 0x61; 0xF1; ...] *)
Idna.Punycode.encode [0x6D;0x61;0xF1;0x61;0x6E;0x61]  (* Ok "maana-pta" *)

(* NFC normalization *)
Idna.nfc [0x0065; 0x0301]               (* [0x00E9] — e + acute → é *)
```

## Features

- UTS #46 Nontransitional processing (`Idna.Uts46.to_ascii`, `Idna.Uts46.to_unicode`)
- IDNA2008 hostname and label validation (RFC 5890, 5891, 5892)
- Bidirectional text rules 1-6 (RFC 5893), with layer-specific label/domain enforcement
- Punycode encoding and decoding (RFC 3492)
- Unicode NFC normalization (canonical decomposition, ordering, composition; Hangul)
- Codepoint classification (PVALID, CONTEXTJ, CONTEXTO)
- CONTEXTJ/CONTEXTO contextual rules
- STD3 ASCII rules

## Diagnostics

`Idna.Diagnostics` mirrors the public API and provides structured explainability
and provenance for `Registration`, `Lookup`, and `Uts46`.

Public contract:

- `report.accepted` matches the corresponding public API outcome
- `report.events` are emitted in deterministic pipeline order, but the public
  ordering contract is intentionally only the partial-order subset documented in
  [DIAGNOSTICS_ORDERING.md](DIAGNOSTICS_ORDERING.md)
- `Error` events contribute to rejection
- `Warning` events are semantically notable or provenance-only and do not by
  themselves reject the input
- `Info` events are neutral trace/classification facts
- `event.detail` is explanatory text only and is not a stability contract

For `Uts46`, diagnostics is also the precise explanatory surface: the plain
`Uts46.to_ascii` error strings are intentionally coarser than the corresponding
`Diagnostics.Uts46` reports.

Examples of provenance exposed by diagnostics:

- `Idna2008_nv8`
- `Idna2008_xv8`
- `Uts46_deviation`

`Serialization_failed` is part of the public diagnostics surface too, but it is
not a normative IDNA/UTS #46 condition. It is a defensive runtime failure and
is intentionally left outside default-suite coverage.

## Regenerating Unicode tables

Tables are generated from Unicode 16.0.0 UCD.

Requires Python with `unicodedata.unidata_version == "16.0.0"` (Python 3.14+).
The generator refuses to run on older Python because `unicodedata.normalize`
is used for the RFC 5892 Unstable check and would mix Unicode versions.

```
./tools/download_ucd.sh 16.0.0
uv run --python 3.14 python tools/gen_tables.py --format 64 -o lib/idna-tables-64/idna_tables.ml
uv run --python 3.14 python tools/gen_tables.py --format 32 -o lib/idna-tables-32/idna_tables.ml
```

## License

ISC
