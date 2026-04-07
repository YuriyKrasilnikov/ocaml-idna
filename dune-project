(lang dune 3.0)

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
 (synopsis "IDNA2008 and Punycode for OCaml (RFC 5890/5892/3492)")
 (description
  "Pure OCaml implementation of IDNA2008 (RFC 5890/5892) hostname validation
   and Punycode (RFC 3492) encoding/decoding. Uses Unicode 16.0.0 character
   tables with O(log n) codepoint lookup. No external C dependencies.")
 (depends
  (ocaml (>= 5.1))
  (alcotest :with-test)))
