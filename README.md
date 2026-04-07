# ocaml-idna

Internationalized domain name validation, [Punycode](https://www.rfc-editor.org/rfc/rfc3492) (RFC 3492) encoding/decoding, and Unicode NFC normalization for OCaml.

Implements IDNA2008 ([RFC 5890](https://www.rfc-editor.org/rfc/rfc5890), [RFC 5891](https://www.rfc-editor.org/rfc/rfc5891), [RFC 5892](https://www.rfc-editor.org/rfc/rfc5892), [RFC 5893](https://www.rfc-editor.org/rfc/rfc5893)). Pure OCaml, no C dependencies. Unicode 16.0.0 character tables.

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
Idna.Punycode.encode [0x6D;0x61;0xF1;0x61;0x6E;0x61]  (* Ok "maana-pta" *)

Idna.nfc [0x0065; 0x0301]                    (* [0x00E9] — e + acute → é *)
```

## Features

- Hostname and label validation (RFC 5891 Sections 4-5)
- Codepoint classification (PVALID, CONTEXTJ, CONTEXTO per RFC 5892)
- Hyphen rules, label and hostname length limits
- NFC normalization (canonical decomposition, ordering, composition; Hangul syllables)
- Bidirectional text rules 1-6 (RFC 5893), including domain-level enforcement
- A-label (xn--) Punycode encoding and decoding (case-insensitive prefix)
- Initial combining mark rejection
- CONTEXTJ/CONTEXTO contextual rules

## Not implemented

- [UTS #46](https://www.unicode.org/reports/tr46/) mapping and compatibility processing

## Regenerating Unicode tables

Tables are generated from Unicode 16.0.0 UCD:

```
./tools/download_ucd.sh 16.0.0
python3 tools/gen_tables.py --output lib/idna_tables.ml
```

## License

ISC
