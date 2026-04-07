# ocaml-idna

IDNA2008 ([RFC 5890](https://www.rfc-editor.org/rfc/rfc5890)/[RFC 5892](https://www.rfc-editor.org/rfc/rfc5892)) hostname validation and [Punycode](https://www.rfc-editor.org/rfc/rfc3492) (RFC 3492) decoding for OCaml.

Pure OCaml, no C dependencies. Uses Unicode 16.0.0 character tables.

## Installation

```
opam pin add idna .
```

## Usage

```ocaml
Idna.is_valid_hostname "example.com"          (* true *)
Idna.is_valid_hostname "xn--maana-pta.com"    (* true — mañana.com *)
Idna.is_valid_hostname "-invalid.com"          (* false *)

Idna.check_label "xn--maana-pta"              (* Ok () *)
Idna.check_label "xn--X"                      (* Error "invalid punycode" *)

Idna.Punycode.decode "maana-pta"              (* Ok [0x6D; 0x61; 0xF1; ...] *)
```

## What it validates

- Hyphen rules (no leading/trailing hyphen, no `--` at positions 3-4)
- Label length (1-63 octets, hostname max 253)
- Codepoint validity (PVALID, CONTEXTJ, CONTEXTO per RFC 5892)
- NFC normalization quick check
- Initial combining mark rejection
- Bidi rules (RFC 5893)
- A-label (xn--) Punycode decoding and validation

## Regenerating Unicode tables

Tables are pre-generated from Unicode 16.0.0 UCD. To regenerate:

```
./tools/download_ucd.sh 16.0.0
python3 tools/gen_tables.py --output lib/idna_tables.ml
```

## License

ISC
