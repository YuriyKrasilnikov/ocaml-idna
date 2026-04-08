(lang dune 3.15)

(name idna)

(generate_opam_files true)

(license ISC)

(authors "Yuriy Krasilnikov")

(maintainers "Yuriy Krasilnikov")

(source (github YuriyKrasilnikov/ocaml-idna))

(homepage "https://github.com/YuriyKrasilnikov/ocaml-idna")

(bug_reports "https://github.com/YuriyKrasilnikov/ocaml-idna/issues")

(package
 (name idna)
 (synopsis "Internationalized Domain Names for OCaml (IDNA 2008 and UTS #46)")
 (description
  "Pure OCaml implementation of IDNA2008 hostname validation, UTS #46
   compatibility processing, Punycode encoding/decoding, and Unicode NFC
   normalization. Uses Unicode 16.0.0 character tables. No C dependencies.")
 (depends
  (ocaml (>= 5.1))
  (alcotest :with-test)))
