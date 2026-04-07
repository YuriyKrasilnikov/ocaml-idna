#!/usr/bin/env python3
"""Generate OCaml IDNA2008 tables from Unicode Character Database.

Reads UCD files from /tmp/ucd/ (Unicode 16.0.0) and generates
lib/idna_tables.ml with packed integer ranges for binary search.

Based on RFC 5892 derivation algorithm and Python idna library
(https://github.com/kjd/idna) as reference.
"""

import collections
import os
import re
import sys
import unicodedata

UCD_DIR = os.environ.get("UCD_DIR", os.path.join(os.path.dirname(__file__), "ucd-16.0.0"))

# RFC 5892 Section 2.6 — Exceptions
exceptions = {
    0x00DF: "PVALID",   # LATIN SMALL LETTER SHARP S
    0x03C2: "PVALID",   # GREEK SMALL LETTER FINAL SIGMA
    0x06FD: "PVALID",   # ARABIC SIGN SINDHI AMPERSAND
    0x06FE: "PVALID",   # ARABIC SIGN SINDHI POSTPOSITION MEN
    0x0F0B: "PVALID",   # TIBETAN MARK INTERSYLLABIC TSHEG
    0x3007: "PVALID",   # IDEOGRAPHIC NUMBER ZERO
    0x00B7: "CONTEXTO", # MIDDLE DOT
    0x0375: "CONTEXTO", # GREEK LOWER NUMERAL SIGN
    0x05F3: "CONTEXTO", # HEBREW PUNCTUATION GERESH
    0x05F4: "CONTEXTO", # HEBREW PUNCTUATION GERSHAYIM
    0x30FB: "CONTEXTO", # KATAKANA MIDDLE DOT
    0x0660: "CONTEXTO", 0x0661: "CONTEXTO", 0x0662: "CONTEXTO",
    0x0663: "CONTEXTO", 0x0664: "CONTEXTO", 0x0665: "CONTEXTO",
    0x0666: "CONTEXTO", 0x0667: "CONTEXTO", 0x0668: "CONTEXTO",
    0x0669: "CONTEXTO", # ARABIC-INDIC DIGITS
    0x06F0: "CONTEXTO", 0x06F1: "CONTEXTO", 0x06F2: "CONTEXTO",
    0x06F3: "CONTEXTO", 0x06F4: "CONTEXTO", 0x06F5: "CONTEXTO",
    0x06F6: "CONTEXTO", 0x06F7: "CONTEXTO", 0x06F8: "CONTEXTO",
    0x06F9: "CONTEXTO", # EXTENDED ARABIC-INDIC DIGITS
    0x0640: "DISALLOWED", # ARABIC TATWEEL
    0x07FA: "DISALLOWED", # NKO LAJANYALAN
    0x302E: "DISALLOWED", # HANGUL SINGLE DOT TONE MARK
    0x302F: "DISALLOWED", # HANGUL DOUBLE DOT TONE MARK
    0x3031: "DISALLOWED", 0x3032: "DISALLOWED", 0x3033: "DISALLOWED",
    0x3034: "DISALLOWED", 0x3035: "DISALLOWED", # VERTICAL KANA REPEAT MARKS
    0x303B: "DISALLOWED", # VERTICAL IDEOGRAPHIC ITERATION MARK
}

# ── UCD parsing ──

def parse_ranges(filename):
    """Parse UCD file with XXXX..YYYY ; Property format."""
    result = collections.defaultdict(set)
    path = os.path.join(UCD_DIR, filename)
    with open(path) as f:
        for line in f:
            line = line.split("#")[0].strip()
            if not line:
                continue
            m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
            if m:
                start = int(m.group(1), 16)
                end = int(m.group(2), 16) if m.group(2) else start
                prop = m.group(3)
                for i in range(start, end + 1):
                    result[prop].add(i)
    return result

def load_unicode_data():
    """Parse UnicodeData.txt → {codepoint: (name, general_category)}."""
    data = {}
    path = os.path.join(UCD_DIR, "UnicodeData.txt")
    range_begin = None
    with open(path) as f:
        for line in f:
            fields = line.strip().split(";")
            cp = int(fields[0], 16)
            name = fields[1]
            gc = fields[2]
            if name.endswith(", First>"):
                range_begin = cp
            elif name.endswith(", Last>"):
                for i in range(range_begin, cp + 1):
                    data[i] = gc
                range_begin = None
            else:
                data[cp] = gc
    return data

def load_blocks():
    """Parse Blocks.txt → {codepoint: block_name}."""
    blocks = {}
    path = os.path.join(UCD_DIR, "Blocks.txt")
    with open(path) as f:
        for line in f:
            line = line.split("#")[0].strip()
            if not line:
                continue
            m = re.match(r"([0-9A-F]{4,6})\.\.([0-9A-F]{4,6});\s*(.*)", line)
            if m:
                start = int(m.group(1), 16)
                end = int(m.group(2), 16)
                block = m.group(3).strip()
                for i in range(start, end + 1):
                    blocks[i] = block
    return blocks

def load_casefolding():
    """Parse CaseFolding.txt → {codepoint: folded_string}."""
    cf = {}
    path = os.path.join(UCD_DIR, "CaseFolding.txt")
    with open(path) as f:
        for line in f:
            line = line.split("#")[0].strip()
            if not line:
                continue
            m = re.match(r"([0-9A-F]{4,6});\s*([CFTSI]);\s*([0-9A-F ]+)", line)
            if m:
                if m.group(2) in ("C", "F"):
                    cp = int(m.group(1), 16)
                    mapped = "".join(chr(int(x, 16)) for x in m.group(3).strip().split())
                    cf[cp] = mapped
    return cf

# ── RFC 5892 derivation ──

def casefold(s, cf):
    return "".join(cf.get(ord(c), c) for c in s)

def derive_status(cp, ucd_data, props_all, blocks, hst, cf):
    """RFC 5892 Section 2 — derive IDNA2008 status for a codepoint."""
    if cp in exceptions:
        return exceptions[cp]
    # Unassigned
    gc = ucd_data.get(cp)
    is_nonchar = cp in props_all.get("Noncharacter_Code_Point", set())
    if gc is None and not is_nonchar:
        return "UNASSIGNED"
    # LDH
    if cp == 0x2D or 0x30 <= cp <= 0x39 or 0x61 <= cp <= 0x7A:
        return "PVALID"
    # Join_Control
    if cp in props_all.get("Join_Control", set()):
        return "CONTEXTJ"
    # Unstable: NFKC(CaseFold(NFKC(char))) != char
    c = chr(cp)
    try:
        nfkc_cf = unicodedata.normalize("NFKC", casefold(unicodedata.normalize("NFKC", c), cf))
        if c != nfkc_cf:
            return "DISALLOWED"
    except (ValueError, OverflowError):
        pass
    # IgnorableProperties
    for prop in ("Default_Ignorable_Code_Point", "White_Space", "Noncharacter_Code_Point"):
        if cp in props_all.get(prop, set()):
            return "DISALLOWED"
    # IgnorableBlocks
    if blocks.get(cp) in ("Combining Diacritical Marks for Symbols",
                           "Musical Symbols", "Ancient Greek Musical Notation"):
        return "DISALLOWED"
    # OldHangulJamo
    if cp in hst.get("L", set()) or cp in hst.get("V", set()) or cp in hst.get("T", set()):
        return "DISALLOWED"
    # LetterDigits
    if gc in ("Ll", "Lu", "Lo", "Nd", "Lm", "Mn", "Mc"):
        return "PVALID"
    return "DISALLOWED"

# ── Packed range encoding ──

def to_ranges(codepoints):
    """Convert sorted set of ints to list of (start, end) ranges."""
    if not codepoints:
        return []
    sorted_cps = sorted(codepoints)
    ranges = []
    start = sorted_cps[0]
    prev = start
    for cp in sorted_cps[1:]:
        if cp == prev + 1:
            prev = cp
        else:
            ranges.append((start, prev + 1))
            start = cp
            prev = cp
    ranges.append((start, prev + 1))
    return ranges

def encode_range(start, end):
    """Pack (start, end) into single int: start << 32 | end."""
    return (start << 32) | end

def emit_array(name, ranges):
    """Emit OCaml int array from ranges."""
    print(f"let {name} = [|")
    for start, end in ranges:
        packed = encode_range(start, end)
        print(f"  0x{packed:016x}; (* 0x{start:04X}..0x{end-1:04X} *)")
    print("|]")
    print()

def emit_map(name, mapping):
    """Emit OCaml (int * int) array for codepoint→value mapping."""
    print(f"let {name} = [|")
    for cp in sorted(mapping.keys()):
        v = mapping[cp]
        print(f"  (0x{cp:04X}, {v});")
    print("|]")
    print()

# ── Main ──

def main():
    # Load UCD
    ucd_data = load_unicode_data()
    props_pl = parse_ranges("PropList.txt")
    props_dcp = parse_ranges("DerivedCoreProperties.txt")
    blocks = load_blocks()
    cf = load_casefolding()
    hst_data = parse_ranges("HangulSyllableType.txt")
    jt_data = parse_ranges("DerivedJoiningType.txt")
    scripts_data = parse_ranges("Scripts.txt")
    nfc_data = parse_ranges("DerivedNormalizationProps.txt")

    # Merge props
    props_all = collections.defaultdict(set)
    for d in (props_pl, props_dcp):
        for k, v in d.items():
            props_all[k] |= v

    # Find max codepoint
    max_cp = max(ucd_data.keys()) if ucd_data else 0x10FFFF

    # Derive IDNA2008 status for all codepoints
    classes = collections.defaultdict(set)
    for cp in range(0, max_cp + 1):
        status = derive_status(cp, ucd_data, props_all, blocks, hst_data, cf)
        if status in ("PVALID", "CONTEXTJ", "CONTEXTO"):
            classes[status].add(cp)

    # Collect bidi classes from UnicodeData.txt (field 4)
    bidi_classes = collections.defaultdict(set)
    path = os.path.join(UCD_DIR, "UnicodeData.txt")
    range_begin = None
    with open(path) as f:
        for line in f:
            fields = line.strip().split(";")
            cp = int(fields[0], 16)
            bidi = fields[4]
            name = fields[1]
            if name.endswith(", First>"):
                range_begin = (cp, bidi)
            elif name.endswith(", Last>"):
                for i in range(range_begin[0], cp + 1):
                    bidi_classes[range_begin[1]].add(i)
                range_begin = None
            else:
                bidi_classes[bidi].add(cp)

    # Collect combining class = 9 (virama)
    virama = set()
    with open(os.path.join(UCD_DIR, "UnicodeData.txt")) as f:
        for line in f:
            fields = line.strip().split(";")
            cp = int(fields[0], 16)
            ccc = fields[3]
            if ccc == "9":
                virama.add(cp)

    # General category M (combining marks)
    gc_m = set()
    for cp, gc in ucd_data.items():
        if gc.startswith("M"):
            gc_m.add(cp)

    # ── Emit OCaml ──
    print("(* GENERATED by tools/gen_tables.py from Unicode 16.0.0 UCD. DO NOT EDIT. *)")
    print()

    # Codepoint classes
    emit_array("codepoint_pvalid", to_ranges(classes["PVALID"]))
    emit_array("codepoint_contextj", to_ranges(classes["CONTEXTJ"]))
    emit_array("codepoint_contexto", to_ranges(classes["CONTEXTO"]))

    # Scripts (only the 5 needed for CONTEXTO rules)
    for script in ("Greek", "Hebrew", "Han", "Hiragana", "Katakana"):
        emit_array(f"script_{script.lower()}", to_ranges(scripts_data.get(script, set())))

    # Joining types (for CONTEXTJ)
    jt_map = {}
    type_to_int = {"C": 0, "D": 1, "L": 2, "R": 3, "T": 4, "U": 5}
    for jt, cps in jt_data.items():
        if jt in type_to_int:
            for cp in cps:
                jt_map[cp] = type_to_int[jt]
    emit_map("joining_types", jt_map)

    # Bidi classes (for bidi rules)
    for bc in ("R", "AL", "AN", "EN", "ES", "CS", "ET", "ON", "BN", "NSM", "L"):
        emit_array(f"bidi_{bc.lower()}", to_ranges(bidi_classes.get(bc, set())))

    # Combining class = 9 (virama)
    emit_array("virama", to_ranges(virama))

    # General category M (combining marks — for check_initial_combiner)
    emit_array("general_category_m", to_ranges(gc_m))

    # NFC Quick Check
    nfc_no = set()
    nfc_maybe = set()
    for prop, cps in nfc_data.items():
        if "NFC_QC" in prop:
            # The property names in the file are like "NFC_QC"
            # and the values are in a separate field, but parse_ranges
            # combines them. We need to re-parse.
            pass
    # Re-parse NFC_QC specifically
    nfc_no = set()
    nfc_maybe = set()
    with open(os.path.join(UCD_DIR, "DerivedNormalizationProps.txt")) as f:
        for line in f:
            line_clean = line.split("#")[0].strip()
            if not line_clean:
                continue
            m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*NFC_QC\s*;\s*(\S+)", line_clean)
            if m:
                start = int(m.group(1), 16)
                end = int(m.group(2), 16) if m.group(2) else start
                val = m.group(3)
                for i in range(start, end + 1):
                    if val == "N":
                        nfc_no.add(i)
                    elif val == "M":
                        nfc_maybe.add(i)
    emit_array("nfc_qc_no", to_ranges(nfc_no))
    emit_array("nfc_qc_maybe", to_ranges(nfc_maybe))

    # NFC canonical composition pairs (for NFC_QC=Maybe verification)
    # From UnicodeData.txt field 5: canonical decomposition (no <tag>)
    # Composition exclusions: Full_Composition_Exclusion from DerivedNormalizationProps.txt
    composition_exclusions = set()
    with open(os.path.join(UCD_DIR, "DerivedNormalizationProps.txt")) as f:
        for line in f:
            line_clean = line.split("#")[0].strip()
            if not line_clean:
                continue
            m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*Full_Composition_Exclusion", line_clean)
            if m:
                start = int(m.group(1), 16)
                end = int(m.group(2), 16) if m.group(2) else start
                for i in range(start, end + 1):
                    composition_exclusions.add(i)

    # Build composition pairs: (starter, combining) → composite
    composition_pairs = {}  # (starter, combining) → composite
    with open(os.path.join(UCD_DIR, "UnicodeData.txt")) as f:
        for line in f:
            fields = line.strip().split(";")
            cp = int(fields[0], 16)
            decomp = fields[5]
            if not decomp or decomp.startswith("<"):
                continue  # Skip compatibility decompositions
            parts = decomp.split()
            if len(parts) == 2:
                # Canonical 2-char decomposition: composite = parts[0] + parts[1]
                starter = int(parts[0], 16)
                combining = int(parts[1], 16)
                if cp not in composition_exclusions:
                    composition_pairs[(starter, combining)] = cp

    # Emit as sorted array of (packed_key, composite) for binary search
    # Key = starter lsl 21 lor combining (both fit in 21 bits)
    print(f"(* NFC composition pairs: {len(composition_pairs)} entries *)")
    print("let nfc_compositions = [|")
    for (starter, combining) in sorted(composition_pairs.keys()):
        composite = composition_pairs[(starter, combining)]
        key = (starter << 21) | combining
        print(f"  (0x{key:010x}, 0x{composite:04X}); (* U+{starter:04X} + U+{combining:04X} → U+{composite:04X} *)")
    print("|]")
    print()

    # ── NFC normalization tables ──

    # Canonical decomposition mappings: cp → (d1, d2) or cp → (d1, 0) for single
    # Only canonical (no <tag>), excluding Hangul (algorithmic)
    canon_decomp = {}
    with open(os.path.join(UCD_DIR, "UnicodeData.txt")) as f:
        for line in f:
            fields = line.strip().split(";")
            cp = int(fields[0], 16)
            decomp = fields[5]
            if not decomp or decomp.startswith("<"):
                continue
            parts = [int(x, 16) for x in decomp.split()]
            # Skip Hangul syllables (algorithmic)
            if 0xAC00 <= cp <= 0xD7A3:
                continue
            canon_decomp[cp] = parts

    print(f"(* Canonical decomposition: {len(canon_decomp)} entries *)")
    print("let canon_decomp = [|")
    for cp in sorted(canon_decomp.keys()):
        parts = canon_decomp[cp]
        if len(parts) == 1:
            print(f"  (0x{cp:04X}, 0x{parts[0]:04X}, 0); (* U+{cp:04X} → U+{parts[0]:04X} *)")
        elif len(parts) == 2:
            print(f"  (0x{cp:04X}, 0x{parts[0]:04X}, 0x{parts[1]:04X}); (* U+{cp:04X} → U+{parts[0]:04X} U+{parts[1]:04X} *)")
        else:
            # Longer decompositions: store first two, rest handled recursively
            print(f"  (0x{cp:04X}, 0x{parts[0]:04X}, 0x{parts[1]:04X}); (* U+{cp:04X} → {' '.join(f'U+{p:04X}' for p in parts)} (truncated) *)")
    print("|]")
    print()

    # Canonical Combining Class: only non-zero entries
    ccc_map = {}
    with open(os.path.join(UCD_DIR, "UnicodeData.txt")) as f:
        range_begin = None
        for line in f:
            fields = line.strip().split(";")
            cp = int(fields[0], 16)
            ccc = int(fields[3])
            name = fields[1]
            if name.endswith(", First>"):
                range_begin = (cp, ccc)
            elif name.endswith(", Last>"):
                if range_begin and range_begin[1] > 0:
                    for i in range(range_begin[0], cp + 1):
                        ccc_map[i] = range_begin[1]
                range_begin = None
            elif ccc > 0:
                ccc_map[cp] = ccc

    # Emit as sorted (cp, ccc) array for binary search
    print(f"(* Canonical Combining Class: {len(ccc_map)} non-zero entries *)")
    print("let canon_ccc = [|")
    for cp in sorted(ccc_map.keys()):
        print(f"  (0x{cp:04X}, {ccc_map[cp]});")
    print("|]")
    print()

    # Stats
    print(f"(* Stats: PVALID={len(classes['PVALID'])}, CONTEXTJ={len(classes['CONTEXTJ'])}, CONTEXTO={len(classes['CONTEXTO'])} *)")
    print(f"(* Bidi classes: {', '.join(f'{k}={len(v)}' for k,v in sorted(bidi_classes.items()))} *)")
    print(f"(* NFC: QC_No={len(nfc_no)}, QC_Maybe={len(nfc_maybe)}, Decomp={len(canon_decomp)}, CCC={len(ccc_map)}, Compositions={len(composition_pairs)} *)")

if __name__ == "__main__":
    import argparse
    parser = argparse.ArgumentParser(description="Generate IDNA2008 OCaml tables from UCD")
    parser.add_argument("--dry-run", action="store_true",
                        help="Show summary of what would be generated, don't emit OCaml")
    parser.add_argument("--output", "-o", type=str, default=None,
                        help="Write output to file (default: stdout)")
    args = parser.parse_args()

    if args.dry_run:
        import io
        old_stdout = sys.stdout
        sys.stdout = io.StringIO()
        main()
        output = sys.stdout.getvalue()
        sys.stdout = old_stdout
        # Summary
        lines = output.split("\n")
        arrays = [l for l in lines if l.startswith("let ") and "= [|" in l]
        total_entries = output.count(";")
        print(f"Would generate {len(lines)} lines, {len(arrays)} arrays, ~{total_entries} entries")
        print()
        for a in arrays:
            name = a.split("=")[0].strip().replace("let ", "")
            count = sum(1 for l in lines if l.startswith("  ") and
                        lines.index(a) < lines.index(l) < (
                            lines.index(a) + 10000))
            print(f"  {name}")
        print()
        # Print stats lines
        for l in lines:
            if l.startswith("(* Stats") or l.startswith("(* Bidi") or l.startswith("(* NFC"):
                print(l)
    elif args.output:
        with open(args.output, "w") as f:
            old_stdout = sys.stdout
            sys.stdout = f
            main()
            sys.stdout = old_stdout
        # Count what was written
        with open(args.output) as f:
            n = sum(1 for _ in f)
        print(f"Written {n} lines to {args.output}")
    else:
        main()
