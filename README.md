# ocaml-idna

Internationalized Domain Names for OCaml (IDNA 2008 and UTS #46).

Pure OCaml implementation of IDNA2008 hostname validation, UTS #46 compatibility processing, Punycode encoding/decoding, and Unicode NFC normalization. No C dependencies. Unicode 16.0.0.

## Installation

```
opam install idna
```

## Usage

```ocaml
(* UTS #46 processing *)
Idna.to_ascii "Königsgäßchen.example"   (* Ok "xn--knigsgchen-b4a3dun.example" *)
Idna.to_unicode "xn--maana-pta.com"     (* Ok "mañana.com" *)

(* IDNA2008 validation *)
Idna.is_valid_hostname "example.com"     (* true *)
Idna.is_valid_hostname "-invalid.com"    (* false *)

Idna.check_label "xn--maana-pta"        (* Ok () *)
Idna.check_label "xn--X"                (* Error "invalid punycode" *)

(* Punycode *)
Idna.Punycode.decode "maana-pta"         (* Ok [0x6D; 0x61; 0xF1; ...] *)
Idna.Punycode.encode [0x6D;0x61;0xF1;0x61;0x6E;0x61]  (* Ok "maana-pta" *)

(* NFC normalization *)
Idna.nfc [0x0065; 0x0301]               (* [0x00E9] — e + acute → é *)
```

## Features

- UTS #46 Nontransitional processing (`to_ascii`, `to_unicode`)
- IDNA2008 hostname and label validation (RFC 5890, 5891, 5892)
- Bidirectional text rules 1-6 (RFC 5893), domain-level enforcement
- Punycode encoding and decoding (RFC 3492)
- Unicode NFC normalization (canonical decomposition, ordering, composition; Hangul)
- Codepoint classification (PVALID, CONTEXTJ, CONTEXTO)
- CONTEXTJ/CONTEXTO contextual rules
- STD3 ASCII rules

## Regenerating Unicode tables

Tables are generated from Unicode 16.0.0 UCD:

```
./tools/download_ucd.sh 16.0.0
python3 tools/gen_tables.py --output lib/idna_tables.ml
```

## License

ISC
