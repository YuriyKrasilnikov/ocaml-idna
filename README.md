# ocaml-idna

Internationalized Domain Names for OCaml (IDNA 2008 and UTS #46).

Pure OCaml implementation of IDNA2008 hostname validation, UTS #46
compatibility processing, Punycode encoding/decoding, and Unicode NFC
normalization. No C dependencies. Unicode 16.0.0.

`ocaml-idna` is release-verified as **100% compliant with its documented public
conformance scope** for Unicode 16.0.0: IDNA2008 Registration and Lookup,
UTS #46 Nontransitional processing, RFC 3492 Punycode, and Unicode NFC.

## Installation

```
opam install idna
```

## Semantic layers

The library exposes three separate semantic layers:

- `Idna.Registration`: strict IDNA2008 registration validation
- `Idna.Lookup`: strict IDNA2008 lookup preparation/conversion
- `Idna.Uts46`: UTS #46 Nontransitional compatibility processing

Each layer has its own acceptance rules, result shape, and policy flags
following its governing specification.

For the normative baseline, governing-source precedence, requirement trace,
evidence tiers, and documented exclusions, see
[CONFORMANCE_DOSSIER.md](CONFORMANCE_DOSSIER.md).
For local benchmark commands and thresholds, see [PERFORMANCE.md](PERFORMANCE.md).
For diagnostics policy, ordering, and diagnostics-specific test coverage, see
[DIAGNOSTICS_POLICY.md](DIAGNOSTICS_POLICY.md).
For contribution and security reporting policy, see [CONTRIBUTING.md](CONTRIBUTING.md)
and [SECURITY.md](SECURITY.md).
For release notes, see [CHANGES.md](CHANGES.md).

The documented public surfaces are covered by a scoped conformance gate:

```sh
dune build @test/conformance --force
```

That gate requires the local Unicode data files and is separate from package
`dune runtest`. It is the release-facing alias for the local evidence suite:
ordinary unit/regression tests, the official Unicode/UTS #46 corpora,
generated-vector checks, and generated table exactness checks.

Bundled official vectors pass in full: UTS #46 `toUnicode` and `toAsciiN` over
all IdnaTestV2 rows, strict IDNA2008 validation `6389/6389`, strict
registration encode subset `386/386`, NormalizationTest PASS, and generated
Unicode/UTS #46 table exactness PASS.
Library-defined diagnostics, error text, performance, registry policy, and
security/confusable policy are outside this conformance claim and documented
separately.

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
   Uts46.to_ascii returns Ok value | Error msg.
   Plain Uts46.to_ascii error strings are coarse;
   Diagnostics.Uts46 provides per-rule explanation. *)

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

## Performance

The repository includes a local UTS #46 performance regression and adversarial
scaling gate. Treat benchmark output as run-specific evidence for the evaluated
change set, not as a portable latency guarantee or speedup proof.

Run the local benchmark gate with:

```sh
dune build @test/bench --force
```

For thresholds and benchmarking caveats, see [PERFORMANCE.md](PERFORMANCE.md).

## Diagnostics

`Idna.Diagnostics` mirrors the public API and provides structured explainability
and provenance for `Registration`, `Lookup`, and `Uts46`.

Public contract:

- `report.accepted` matches the corresponding public API outcome
- `report.events` are emitted in deterministic pipeline order; the public
  ordering contract is the partial-order subset documented in
  [DIAGNOSTICS_POLICY.md](DIAGNOSTICS_POLICY.md)
- `Error` events contribute to rejection
- `Warning` events are semantically notable or provenance-only and do not by
  themselves reject the input
- `Info` events are neutral trace/classification facts
- `event.detail` is explanatory text only and is not a stability contract

For `Uts46`, the diagnostics surface provides per-rule explanation; the plain
`Uts46.to_ascii` error strings are coarse by comparison.

Examples of provenance exposed by diagnostics:

- `Idna2008_nv8`
- `Idna2008_xv8`
- `Uts46_deviation`

`Serialization_failed` is a public diagnostics code for a defensive runtime
failure, not a normative IDNA/UTS #46 condition. It is outside default-suite
coverage.

## Regenerating Unicode tables

Tables are generated from Unicode 16.0.0 UCD.

The public generated-table libraries `idna.tables` and `idna.intranges` are
support data, not standalone IDNA validators. Some raw Unicode property tables
can reflect Unicode property ranges that include non-scalar code points. Public
IDNA entry points validate UTF-8, Punycode scalar values, and IDNA
admissibility before such property data can affect acceptance.

Requires Python with `unicodedata.unidata_version == "16.0.0"` (Python 3.14+).
The generator refuses to run on older Python because `unicodedata.normalize`
is used for the RFC 5892 Unstable check and would mix Unicode versions.

```
./tools/download_ucd.sh 16.0.0
uv run --python 3.14 python tools/gen_tables.py --format 64 -o lib/idna-tables-64/idna_tables.ml
uv run --python 3.14 python tools/gen_tables.py --format 32 -o lib/idna-tables-32/idna_tables.ml
```

The same regeneration workflow is available as:

```
make generate
```

The local evidence gate verifies the regenerated runtime tables against the
Unicode 16.0.0 / UTS #46 source data:

```
make evidence
```

This evidence gate requires the local Unicode data files. The package test
surface is the default `dune runtest` suite.

## License

ISC
