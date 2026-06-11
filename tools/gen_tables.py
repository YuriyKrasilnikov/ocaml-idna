#!/usr/bin/env python3
"""Generate OCaml IDNA/NFC/UTS46 tables from Unicode Character Database.

Two output formats:
  --format 64  Packed int ranges (start << 32 | end), Eytzinger packed composition keys
  --format 32  Record ranges {starts; ends}, Eytzinger triple compositions (s, c, comp)

Usage:
  ./tools/download_ucd.sh 16.0.0
  python3 tools/gen_tables.py --format 64 -o lib/idna-tables-64/idna_tables.ml
  python3 tools/gen_tables.py --format 32 -o lib/idna-tables-32/idna_tables.ml
"""

import argparse
import collections
import os
import re
import sys
import unicodedata


# ── UCD Loading ──

class UCData:
    """Load all Unicode Character Database files once."""

    def __init__(self, ucd_dir):
        self.ucd_dir = ucd_dir
        self.general_category = {}   # cp → gc string
        self.bidi_class = {}         # cp → bidi string
        self.combining_class = {}    # cp → int (only non-zero)
        self.decomposition = {}      # cp → [int] (canonical only, no Hangul)
        self.props = collections.defaultdict(set)  # prop_name → {cp}
        self.scripts = collections.defaultdict(set)
        self.blocks = {}             # cp → block_name
        self.case_folding = {}       # cp → folded string
        self.hangul_syllable_types = collections.defaultdict(set)
        self.joining_types = {}      # cp → type_int
        self.idna_mapping = {}       # cp → (status, [mapped_cps], idna2008_status?)
        self.nfc_qc_non_yes = set()  # cp → NFC_QC is No or Maybe

        self._load_unicode_data()
        self._load_props("PropList.txt")
        self._load_props("DerivedCoreProperties.txt")
        self._load_blocks()
        self._load_case_folding()
        self._load_hangul_syllable_types()
        self._load_joining_types()
        self._load_scripts()
        self._load_composition_exclusions()
        self._load_idna_mapping()

    def _path(self, filename):
        return os.path.join(self.ucd_dir, filename)

    def _load_unicode_data(self):
        """Parse UnicodeData.txt — gc, bidi, ccc, decomposition."""
        range_begin = None
        with open(self._path("UnicodeData.txt")) as f:
            for line in f:
                fields = line.strip().split(";")
                cp = int(fields[0], 16)
                name = fields[1]
                gc = fields[2]
                ccc = int(fields[3])
                bidi = fields[4]
                decomp = fields[5]

                if name.endswith(", First>"):
                    range_begin = (cp, gc, bidi, ccc)
                    continue
                elif name.endswith(", Last>"):
                    if range_begin:
                        for i in range(range_begin[0], cp + 1):
                            self.general_category[i] = range_begin[1]
                            self.bidi_class[i] = range_begin[2]
                            if range_begin[3] > 0:
                                self.combining_class[i] = range_begin[3]
                    range_begin = None
                    continue

                self.general_category[cp] = gc
                self.bidi_class[cp] = bidi
                if ccc > 0:
                    self.combining_class[cp] = ccc

                # Canonical decomposition (no <tag>, no Hangul)
                if decomp and not decomp.startswith("<") and not (0xAC00 <= cp <= 0xD7A3):
                    self.decomposition[cp] = [int(x, 16) for x in decomp.split()]

    def _load_props(self, filename):
        with open(self._path(filename)) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
                if m:
                    start = int(m.group(1), 16)
                    end = int(m.group(2), 16) if m.group(2) else start
                    for i in range(start, end + 1):
                        self.props[m.group(3)].add(i)

    def _load_blocks(self):
        with open(self._path("Blocks.txt")) as f:
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
                        self.blocks[i] = block

    def _load_case_folding(self):
        with open(self._path("CaseFolding.txt")) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                m = re.match(r"([0-9A-F]{4,6});\s*([CFTSI]);\s*([0-9A-F ]+)", line)
                if m and m.group(2) in ("C", "F"):
                    cp = int(m.group(1), 16)
                    self.case_folding[cp] = "".join(
                        chr(int(x, 16)) for x in m.group(3).strip().split())

    def _load_hangul_syllable_types(self):
        with open(self._path("HangulSyllableType.txt")) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
                if m:
                    start = int(m.group(1), 16)
                    end = int(m.group(2), 16) if m.group(2) else start
                    for i in range(start, end + 1):
                        self.hangul_syllable_types[m.group(3)].add(i)

    def _load_joining_types(self):
        type_map = {"C": 0, "D": 1, "L": 2, "R": 3, "T": 4, "U": 5}
        with open(self._path("DerivedJoiningType.txt")) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
                if m and m.group(3) in type_map:
                    start = int(m.group(1), 16)
                    end = int(m.group(2), 16) if m.group(2) else start
                    for i in range(start, end + 1):
                        self.joining_types[i] = type_map[m.group(3)]

    def _load_scripts(self):
        with open(self._path("Scripts.txt")) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
                if m:
                    start = int(m.group(1), 16)
                    end = int(m.group(2), 16) if m.group(2) else start
                    for i in range(start, end + 1):
                        self.scripts[m.group(3)].add(i)

    def _load_composition_exclusions(self):
        self.composition_exclusions = set()
        with open(self._path("DerivedNormalizationProps.txt")) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*Full_Composition_Exclusion", line)
                if m:
                    start = int(m.group(1), 16)
                    end = int(m.group(2), 16) if m.group(2) else start
                    for i in range(start, end + 1):
                        self.composition_exclusions.add(i)
                    continue
                m = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*NFC_QC\s*;\s*([NM])\b", line)
                if m:
                    start = int(m.group(1), 16)
                    end = int(m.group(2), 16) if m.group(2) else start
                    for i in range(start, end + 1):
                        self.nfc_qc_non_yes.add(i)

    def _load_idna_mapping(self):
        path = self._path("IdnaMappingTable.txt")
        if not os.path.exists(path):
            return
        with open(path) as f:
            for line in f:
                line = line.split("#")[0].strip()
                if not line:
                    continue
                fields = [field.strip() for field in line.split(";")]
                if len(fields) < 2:
                    continue
                range_str = fields[0]
                if ".." in range_str:
                    lo, hi = range_str.split("..", 1)
                    start = int(lo, 16)
                    end = int(hi, 16)
                else:
                    start = int(range_str, 16)
                    end = start
                status = fields[1]
                mapping_str = fields[2] if len(fields) > 2 else ""
                idna2008_status = fields[3] if len(fields) > 3 and fields[3] else None
                mapped_cps = [int(x, 16) for x in mapping_str.split()] if mapping_str else []
                for cp in range(start, end + 1):
                    self.idna_mapping[cp] = (status, mapped_cps, idna2008_status)


# ── Derivation ──

# RFC 5892 Section 2.6 Exceptions
EXCEPTIONS = {
    0x00DF: "PVALID", 0x03C2: "PVALID", 0x06FD: "PVALID",
    0x06FE: "PVALID", 0x0F0B: "PVALID", 0x3007: "PVALID",
    0x00B7: "CONTEXTO", 0x0375: "CONTEXTO",
    0x05F3: "CONTEXTO", 0x05F4: "CONTEXTO", 0x30FB: "CONTEXTO",
    0x0660: "CONTEXTO", 0x0661: "CONTEXTO", 0x0662: "CONTEXTO",
    0x0663: "CONTEXTO", 0x0664: "CONTEXTO", 0x0665: "CONTEXTO",
    0x0666: "CONTEXTO", 0x0667: "CONTEXTO", 0x0668: "CONTEXTO",
    0x0669: "CONTEXTO",
    0x06F0: "CONTEXTO", 0x06F1: "CONTEXTO", 0x06F2: "CONTEXTO",
    0x06F3: "CONTEXTO", 0x06F4: "CONTEXTO", 0x06F5: "CONTEXTO",
    0x06F6: "CONTEXTO", 0x06F7: "CONTEXTO", 0x06F8: "CONTEXTO",
    0x06F9: "CONTEXTO",
    0x0640: "DISALLOWED", 0x07FA: "DISALLOWED",
    0x302E: "DISALLOWED", 0x302F: "DISALLOWED",
    0x3031: "DISALLOWED", 0x3032: "DISALLOWED", 0x3033: "DISALLOWED",
    0x3034: "DISALLOWED", 0x3035: "DISALLOWED", 0x303B: "DISALLOWED",
}

IGNORABLE_BLOCKS = {
    "Combining Diacritical Marks for Symbols",
    "Musical Symbols",
    "Ancient Greek Musical Notation",
}

# RFC 5892 Section 2.7: code points frozen at their prior derived value
# to preserve backward compatibility across Unicode versions.
# Empty in the original specification; may be populated by future updates.
BACKWARD_COMPATIBLE = {}


# Packed Props bit layout. Keep in sync with the runtime Props accessors and
# the independent checker in tools/generated_exact_check.py.
PROP_B_UTS = 0
PROP_B_IDNA = 3
PROP_B_BIDI = 5
PROP_B_CCC = 9
PROP_B_JOIN = 17
PROP_B_SCRIPT = 20
PROP_B_MARK = 25
PROP_B_NFC_QC = 26
PROP_B_NV8 = 27
PROP_B_XV8 = 28
PROP_B_DECOMP = 29

PROP_BIDI_ORDER = ["R", "L", "AL", "AN", "EN", "ES", "CS", "ET", "ON", "BN", "NSM"]
PROP_BIDI_CODE = {name: i + 1 for i, name in enumerate(PROP_BIDI_ORDER)}


class Derivation:
    """Derive all IDNA/NFC/bidi/UTS46 data from UCData."""

    def __init__(self, data):
        self.data = data
        self._derive_idna_classes()
        self._derive_bidi()
        self._derive_nfc()
        self._derive_uts46()

    def _casefold(self, s):
        return "".join(self.data.case_folding.get(ord(c), c) for c in s)

    def _derive_status(self, cp):
        # RFC 5892 Section 3 — evaluation order is normative.
        if cp in EXCEPTIONS:
            return EXCEPTIONS[cp]
        if cp in BACKWARD_COMPATIBLE:
            return BACKWARD_COMPATIBLE[cp]
        gc = self.data.general_category.get(cp)
        if gc is None and cp not in self.data.props.get("Noncharacter_Code_Point", set()):
            return "UNASSIGNED"
        if cp == 0x2D or 0x30 <= cp <= 0x39 or 0x61 <= cp <= 0x7A:
            return "PVALID"
        if cp in self.data.props.get("Join_Control", set()):
            return "CONTEXTJ"
        c = chr(cp)
        try:
            nfkc_cf = unicodedata.normalize(
                "NFKC", self._casefold(unicodedata.normalize("NFKC", c)))
            if c != nfkc_cf:
                return "DISALLOWED"
        except (ValueError, OverflowError):
            pass
        for prop in ("Default_Ignorable_Code_Point", "White_Space", "Noncharacter_Code_Point"):
            if cp in self.data.props.get(prop, set()):
                return "DISALLOWED"
        if self.data.blocks.get(cp) in IGNORABLE_BLOCKS:
            return "DISALLOWED"
        hst = self.data.hangul_syllable_types
        if cp in hst.get("L", set()) or cp in hst.get("V", set()) or cp in hst.get("T", set()):
            return "DISALLOWED"
        if gc in ("Ll", "Lu", "Lo", "Nd", "Lm", "Mn", "Mc"):
            return "PVALID"
        return "DISALLOWED"

    def _derive_idna_classes(self):
        self.classes = collections.defaultdict(set)
        max_cp = max(self.data.general_category.keys()) if self.data.general_category else 0x10FFFF
        for cp in range(0, max_cp + 1):
            status = self._derive_status(cp)
            if status in ("PVALID", "CONTEXTJ", "CONTEXTO"):
                self.classes[status].add(cp)

    def _derive_bidi(self):
        self.bidi = collections.defaultdict(set)
        for cp, bc in self.data.bidi_class.items():
            self.bidi[bc].add(cp)

    def _derive_nfc(self):
        # Composition pairs: (starter, combining) → composite
        self.composition_pairs = {}
        for cp, parts in self.data.decomposition.items():
            if len(parts) == 2 and cp not in self.data.composition_exclusions:
                self.composition_pairs[(parts[0], parts[1])] = cp

        # Virama (combining class 9)
        self.virama = {cp for cp, ccc in self.data.combining_class.items() if ccc == 9}

        # General category M (combining marks)
        self.gc_m = {cp for cp, gc in self.data.general_category.items() if gc.startswith("M")}

    def _derive_uts46(self):
        self.uts46_mapped = {}
        self.uts46_ignored = set()
        self.uts46_valid = set()
        self.uts46_deviation = set()
        self.uts46_nv8 = set()
        self.uts46_xv8 = set()
        for cp, (status, mapped_cps, idna2008_status) in self.data.idna_mapping.items():
            if status == "mapped":
                self.uts46_mapped[cp] = mapped_cps
            elif status == "ignored":
                self.uts46_ignored.add(cp)
            elif status == "deviation":
                self.uts46_deviation.add(cp)
            elif status == "valid":
                self.uts46_valid.add(cp)
            if idna2008_status == "NV8":
                self.uts46_nv8.add(cp)
            elif idna2008_status == "XV8":
                self.uts46_xv8.add(cp)


# ── Range encoding ──

def to_ranges(codepoints):
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


def build_props(data, derived):
    bidi_by_cp = {}
    for bidi_class, cps in derived.bidi.items():
        if bidi_class in PROP_BIDI_CODE:
            code = PROP_BIDI_CODE[bidi_class]
            for cp in cps:
                bidi_by_cp[cp] = code

    idna_by_cp = {}
    for cp in derived.classes["PVALID"]:
        idna_by_cp[cp] = 1
    for cp in derived.classes["CONTEXTJ"]:
        idna_by_cp[cp] = 2
    for cp in derived.classes["CONTEXTO"]:
        idna_by_cp[cp] = 3

    uts_by_cp = {}
    for cp in derived.uts46_valid:
        uts_by_cp[cp] = 1
    for cp in derived.uts46_ignored:
        uts_by_cp[cp] = 2
    for cp in derived.uts46_deviation:
        uts_by_cp[cp] = 3
    for cp in derived.uts46_mapped:
        uts_by_cp[cp] = 4

    script_by_cp = collections.defaultdict(int)
    for name, bit in [
        ("Greek", 0),
        ("Hebrew", 1),
        ("Han", 2),
        ("Hiragana", 3),
        ("Katakana", 4),
    ]:
        for cp in data.scripts.get(name, set()):
            script_by_cp[cp] |= 1 << bit

    props = []
    for cp in range(0x110000):
        value = 0
        value |= uts_by_cp.get(cp, 0) << PROP_B_UTS
        value |= idna_by_cp.get(cp, 0) << PROP_B_IDNA
        value |= bidi_by_cp.get(cp, 0) << PROP_B_BIDI
        value |= data.combining_class.get(cp, 0) << PROP_B_CCC
        joining = data.joining_types.get(cp)
        if joining is not None:
            value |= (joining + 1) << PROP_B_JOIN
        value |= script_by_cp.get(cp, 0) << PROP_B_SCRIPT
        if cp in derived.gc_m:
            value |= 1 << PROP_B_MARK
        if cp in data.nfc_qc_non_yes:
            value |= 1 << PROP_B_NFC_QC
        if cp in derived.uts46_nv8:
            value |= 1 << PROP_B_NV8
        if cp in derived.uts46_xv8:
            value |= 1 << PROP_B_XV8
        if cp in data.decomposition:
            value |= 1 << PROP_B_DECOMP
        props.append(value)
    return props


def dedup_pages(values, page_size):
    page_ids = {}
    index = []
    data = []
    for offset in range(0, len(values), page_size):
        page = tuple(values[offset:offset + page_size])
        page_id = page_ids.get(page)
        if page_id is None:
            page_id = len(page_ids)
            page_ids[page] = page_id
            data.extend(page)
        index.append(page_id)
    return index, data


def eytzinger_order(items):
    """Return a 1-indexed Eytzinger layout from sorted items.

    The generated lookup keeps the greatest item <= target as a candidate, then
    performs an exact key equality check. Index 0 is a dummy slot.
    """
    ordered = [None] * (len(items) + 1)
    cursor = 0

    def fill(i):
        nonlocal cursor
        if i <= len(items):
            fill(i * 2)
            ordered[i] = items[cursor]
            cursor += 1
            fill((i * 2) + 1)

    fill(1)
    if cursor != len(items):
        raise AssertionError("Eytzinger layout did not consume all items")
    return ordered


# ── Emitters ──

class Emitter:
    """Base emitter with shared methods."""

    def __init__(self, out, props_shape="a1-direct", props_page_size=128, uts46_map_page_size=128):
        self.out = out
        self.props_shape = props_shape
        self.props_page_size = props_page_size
        self.uts46_map_page_size = uts46_map_page_size

    def _print(self, s=""):
        print(s, file=self.out)

    def header(self):
        self._print("(* GENERATED by tools/gen_tables.py from Unicode 16.0.0 UCD. DO NOT EDIT. *)")
        self._print()

    def emit_map(self, name, mapping):
        self._print(f"let {name} = [|")
        for cp in sorted(mapping.keys()):
            self._print(f"  (0x{cp:04X}, {mapping[cp]});")
        self._print("|]")
        self._print()

    def emit_triple_map(self, name, mapping):
        """Emit (cp, v1, v2) array."""
        self._print(f"let {name} = [|")
        for cp in sorted(mapping.keys()):
            v1, v2 = mapping[cp]
            self._print(f"  (0x{cp:04X}, 0x{v1:04X}, 0x{v2:04X});")
        self._print("|]")
        self._print()

    def emit_uts46(self, derived):
        if not derived.uts46_mapped:
            self._print("(* IdnaMappingTable.txt not found *)")
            return

        data = []
        index = {}
        for cp in sorted(derived.uts46_mapped.keys()):
            mapped_cps = derived.uts46_mapped[cp]
            offset = len(data)
            data.extend(mapped_cps)
            index[cp] = (offset, len(mapped_cps))

        page_size = self.uts46_map_page_size
        if page_size <= 0 or page_size & (page_size - 1) != 0:
            raise ValueError(f"UTS46 map page size must be a power of two: {page_size}")
        if 0x110000 % page_size != 0:
            raise ValueError(f"UTS46 map page size must divide Unicode domain: {page_size}")
        dense_meta = [0] * 0x110000
        for cp, (offset, length) in index.items():
            if length >= 32:
                raise ValueError(f"UTS46 mapping length does not fit metadata encoding: {length}")
            dense_meta[cp] = ((offset + 1) << 5) | length
        meta_index, meta_data = dedup_pages(dense_meta, page_size)
        shift = page_size.bit_length() - 1
        mask = page_size - 1

        self._print(f"(* UTS #46 mapping: {len(index)} mapped, "
                     f"{len(derived.uts46_ignored)} ignored, "
                     f"{len(derived.uts46_valid)} valid, "
                     f"{len(derived.uts46_deviation)} deviation, "
                     f"{len(derived.uts46_nv8)} NV8, "
                     f"{len(derived.uts46_xv8)} XV8 *)")
        self._print()
        self._print(f"(* UTS #46 mapped payload metadata: page_size={page_size}, "
                     f"pages={len(meta_index)}, unique_pages={len(meta_data) // page_size}, "
                     f"data_entries={len(meta_data)} *)")
        self._print("let uts46_map_meta_index = [|")
        for i in range(0, len(meta_index), 16):
            chunk = meta_index[i:i + 16]
            self._print("  " + "; ".join(str(v) for v in chunk) + ";")
        self._print("|]")
        self._print()
        self._print("let uts46_map_meta_data = [|")
        for i in range(0, len(meta_data), 12):
            chunk = meta_data[i:i + 12]
            self._print("  " + "; ".join(str(v) for v in chunk) + ";")
        self._print("|]")
        self._print()
        self._print("let uts46_map_meta cp =")
        self._print("  if cp < 0 || cp > 0x10FFFF then 0")
        self._print("  else")
        self._print(f"    let page = cp lsr {shift} in")
        self._print(f"    let off = cp land 0x{mask:X} in")
        self._print(f"    uts46_map_meta_data.((uts46_map_meta_index.(page) lsl {shift}) + off)")
        self._print()
        self._print("let uts46_map_index = [|")
        for cp in sorted(index.keys()):
            offset, length = index[cp]
            self._print(f"  (0x{cp:04X}, {offset}, {length});")
        self._print("|]")
        self._print()
        self._print("let uts46_map_data = [|")
        for i in range(0, len(data), 16):
            chunk = data[i:i + 16]
            self._print("  " + "; ".join(f"0x{cp:04X}" for cp in chunk) + ";")
        self._print("|]")
        self._print()

    def emit_all(self, derived):
        self.header()
        self._emit_ranges_section(derived)
        self._emit_maps_section(derived)
        self._emit_nfc_section(derived)
        self.emit_uts46(derived)
        self._emit_uts46_ranges(derived)
        self.emit_props(derived)
        self._emit_stats(derived)

    def _emit_maps_section(self, derived):
        self.emit_map("joining_types", derived.data.joining_types)

    def _emit_nfc_section(self, derived):
        # Decomposition: cp → (d1, d2) with d2=0 for single
        decomp_map = {}
        for cp, parts in derived.data.decomposition.items():
            if len(parts) == 1:
                decomp_map[cp] = (parts[0], 0)
            elif len(parts) == 2:
                decomp_map[cp] = (parts[0], parts[1])
        self._print(f"(* Canonical decomposition: {len(decomp_map)} entries *)")
        self.emit_triple_map("canon_decomp", decomp_map)

        # Combining class
        self._print(f"(* Canonical Combining Class: {len(derived.data.combining_class)} entries *)")
        self.emit_map("canon_ccc", derived.data.combining_class)

    def _emit_stats(self, derived):
        self._print(f"(* Stats: PVALID={len(derived.classes['PVALID'])}, "
                     f"CONTEXTJ={len(derived.classes['CONTEXTJ'])}, "
                     f"CONTEXTO={len(derived.classes['CONTEXTO'])} *)")
        self._print(f"(* NFC: Decomp={len(derived.data.decomposition)}, "
                     f"CCC={len(derived.data.combining_class)}, "
                     f"Compositions={len(derived.composition_pairs)} *)")

    def emit_props(self, derived):
        if self.props_shape != "a1-direct":
            raise ValueError(f"unsupported props shape: {self.props_shape}")
        page_size = self.props_page_size
        if page_size <= 0 or page_size & (page_size - 1) != 0:
            raise ValueError(f"props page size must be a power of two: {page_size}")
        if 0x110000 % page_size != 0:
            raise ValueError(f"props page size must divide Unicode domain: {page_size}")

        props = build_props(derived.data, derived)
        index, data = dedup_pages(props, page_size)
        shift = page_size.bit_length() - 1
        mask = page_size - 1

        self._print(
            f"(* Dense packed props: shape={self.props_shape}, page_size={page_size}, "
            f"pages={len(index)}, unique_pages={len(data) // page_size}, "
            f"data_entries={len(data)} *)"
        )
        self._print("module Props = struct")
        self._print("  let index = [|")
        for i in range(0, len(index), 16):
            chunk = index[i:i + 16]
            self._print("    " + "; ".join(str(v) for v in chunk) + ";")
        self._print("  |]")
        self._print()
        self._print("  let data = [|")
        for i in range(0, len(data), 8):
            chunk = data[i:i + 8]
            self._print("    " + "; ".join(f"0x{v:08X}" for v in chunk) + ";")
        self._print("  |]")
        self._print()
        self._print("  let get cp =")
        self._print("    if cp < 0 || cp > 0x10FFFF then 0")
        self._print("    else")
        self._print(f"      let page = cp lsr {shift} in")
        self._print(f"      let off = cp land 0x{mask:X} in")
        self._print(f"      data.((index.(page) lsl {shift}) + off)")
        self._print()
        self._print("  let uts46_status props = props land 0x7")
        self._print("  let idna_class props = (props lsr 3) land 0x3")
        self._print("  let bidi_class props = (props lsr 5) land 0xF")
        self._print("  let ccc props = (props lsr 9) land 0xFF")
        self._print("  let joining_type props = (props lsr 17) land 0x7")
        self._print()
        self._print("  let script_bits props = (props lsr 20) land 0x1F")
        self._print("  let has_script_greek props = (script_bits props land 0x01) <> 0")
        self._print("  let has_script_hebrew props = (script_bits props land 0x02) <> 0")
        self._print("  let has_script_han props = (script_bits props land 0x04) <> 0")
        self._print("  let has_script_hiragana props = (script_bits props land 0x08) <> 0")
        self._print("  let has_script_katakana props = (script_bits props land 0x10) <> 0")
        self._print()
        self._print("  let is_mark props = (props land 0x02000000) <> 0")
        self._print("  let is_nfc_qc_non_yes props = (props land 0x04000000) <> 0")
        self._print("  let is_uts46_nv8 props = (props land 0x08000000) <> 0")
        self._print("  let is_uts46_xv8 props = (props land 0x10000000) <> 0")
        self._print("  let has_canon_decomp props = (props land 0x20000000) <> 0")
        self._print("end")
        self._print()


class Emitter64(Emitter):
    """64-bit: packed int ranges, packed composition keys."""

    def emit_ranges(self, name, ranges):
        self._print(f"let {name} = [|")
        for start, end in ranges:
            packed = (start << 32) | end
            self._print(f"  0x{packed:016x}; (* 0x{start:04X}..0x{end - 1:04X} *)")
        self._print("|]")
        self._print()

    def _emit_ranges_section(self, derived):
        for cat in ("PVALID", "CONTEXTJ", "CONTEXTO"):
            self.emit_ranges(f"codepoint_{cat.lower()}", to_ranges(derived.classes[cat]))
        for script in ("Greek", "Hebrew", "Han", "Hiragana", "Katakana"):
            self.emit_ranges(f"script_{script.lower()}",
                             to_ranges(derived.data.scripts.get(script, set())))
        for bc in ("R", "AL", "AN", "EN", "ES", "CS", "ET", "ON", "BN", "NSM", "L"):
            self.emit_ranges(f"bidi_{bc.lower()}", to_ranges(derived.bidi.get(bc, set())))
        self.emit_ranges("virama", to_ranges(derived.virama))
        self.emit_ranges("general_category_m", to_ranges(derived.gc_m))
        self.emit_ranges("nfc_qc_non_yes", to_ranges(derived.data.nfc_qc_non_yes))

    def _emit_nfc_section(self, derived):
        super()._emit_nfc_section(derived)
        # Composition pairs with packed key in 1-indexed Eytzinger order.
        pairs = derived.composition_pairs
        self._print(f"(* NFC composition pairs: {len(pairs)} entries *)")
        self._print("(* Eytzinger layout, index 0 is a dummy slot. *)")
        self._print("let nfc_compositions = [|")
        sorted_items = [
            ((starter << 21) | combining, pairs[(starter, combining)])
            for (starter, combining) in sorted(pairs.keys())
        ]
        for item in eytzinger_order(sorted_items):
            if item is None:
                key, composite = 0, 0
            else:
                key, composite = item
            self._print(f"  (0x{key:010x}, 0x{composite:04X});")
        self._print("|]")
        self._print()
        self._print("let iter_nfc_compositions f =")
        self._print("  for i = 1 to Array.length nfc_compositions - 1 do")
        self._print("    let (key, composite) = nfc_compositions.(i) in")
        self._print("      let starter = key lsr 21 in")
        self._print("      let combining = key land ((1 lsl 21) - 1) in")
        self._print("      f starter combining composite")
        self._print("  done")
        self._print()
        # nfc_compose using Eytzinger predecessor traversal plus exact key check.
        self._print("let nfc_compose starter combining =")
        self._print("  let key = (starter lsl 21) lor combining in")
        self._print("  let len = Array.length nfc_compositions - 1 in")
        self._print("  let i = ref 1 in")
        self._print("  let candidate = ref 0 in")
        self._print("  while !i <= len do")
        self._print("    let (mkey, _) = nfc_compositions.(!i) in")
        self._print("    if mkey <= key then begin")
        self._print("      candidate := !i;")
        self._print("      i := (!i * 2) + 1")
        self._print("    end")
        self._print("    else i := !i * 2")
        self._print("  done;")
        self._print("  if !candidate > 0 then")
        self._print("    let (mkey, composite) = nfc_compositions.(!candidate) in")
        self._print("    if mkey = key then Some composite else None")
        self._print("  else None")
        self._print()

    def _emit_uts46_ranges(self, derived):
        self.emit_ranges("uts46_ignored", to_ranges(derived.uts46_ignored))
        self.emit_ranges("uts46_valid", to_ranges(derived.uts46_valid))
        self.emit_ranges("uts46_deviation", to_ranges(derived.uts46_deviation))
        self.emit_ranges("uts46_nv8", to_ranges(derived.uts46_nv8))
        self.emit_ranges("uts46_xv8", to_ranges(derived.uts46_xv8))


class Emitter32(Emitter):
    """32-bit: record ranges {starts; ends}, triple compositions."""

    def emit_ranges(self, name, ranges):
        self._print(f"let {name} = {{ Intranges.starts = [|")
        for start, end in ranges:
            self._print(f"  0x{start:04X};")
        self._print("|]; Intranges.ends = [|")
        for start, end in ranges:
            self._print(f"  0x{end:04X};")
        self._print("|] }")
        self._print()

    def _emit_ranges_section(self, derived):
        for cat in ("PVALID", "CONTEXTJ", "CONTEXTO"):
            self.emit_ranges(f"codepoint_{cat.lower()}", to_ranges(derived.classes[cat]))
        for script in ("Greek", "Hebrew", "Han", "Hiragana", "Katakana"):
            self.emit_ranges(f"script_{script.lower()}",
                             to_ranges(derived.data.scripts.get(script, set())))
        for bc in ("R", "AL", "AN", "EN", "ES", "CS", "ET", "ON", "BN", "NSM", "L"):
            self.emit_ranges(f"bidi_{bc.lower()}", to_ranges(derived.bidi.get(bc, set())))
        self.emit_ranges("virama", to_ranges(derived.virama))
        self.emit_ranges("general_category_m", to_ranges(derived.gc_m))
        self.emit_ranges("nfc_qc_non_yes", to_ranges(derived.data.nfc_qc_non_yes))

    def _emit_nfc_section(self, derived):
        super()._emit_nfc_section(derived)
        # Composition pairs as triples in 1-indexed Eytzinger order.
        pairs = derived.composition_pairs
        self._print(f"(* NFC composition pairs: {len(pairs)} entries *)")
        self._print("(* Eytzinger layout, index 0 is a dummy slot. *)")
        self._print("let nfc_compositions = [|")
        sorted_items = [
            (starter, combining, pairs[(starter, combining)])
            for (starter, combining) in sorted(pairs.keys())
        ]
        for item in eytzinger_order(sorted_items):
            if item is None:
                starter, combining, composite = 0, 0, 0
            else:
                starter, combining, composite = item
            self._print(f"  (0x{starter:04X}, 0x{combining:04X}, 0x{composite:04X});")
        self._print("|]")
        self._print()
        self._print("let iter_nfc_compositions f =")
        self._print("  for i = 1 to Array.length nfc_compositions - 1 do")
        self._print("    let (starter, combining, composite) = nfc_compositions.(i) in")
        self._print("    f starter combining composite")
        self._print("  done")
        self._print()
        # nfc_compose using Eytzinger predecessor traversal plus exact pair check.
        self._print("let nfc_compose starter combining =")
        self._print("  let len = Array.length nfc_compositions - 1 in")
        self._print("  let i = ref 1 in")
        self._print("  let candidate = ref 0 in")
        self._print("  while !i <= len do")
        self._print("    let (ms, mc, _) = nfc_compositions.(!i) in")
        self._print("    if ms < starter || (ms = starter && mc <= combining) then begin")
        self._print("      candidate := !i;")
        self._print("      i := (!i * 2) + 1")
        self._print("    end")
        self._print("    else i := !i * 2")
        self._print("  done;")
        self._print("  if !candidate > 0 then")
        self._print("    let (ms, mc, composite) = nfc_compositions.(!candidate) in")
        self._print("    if ms = starter && mc = combining then Some composite else None")
        self._print("  else None")
        self._print()

    def _emit_uts46_ranges(self, derived):
        self.emit_ranges("uts46_ignored", to_ranges(derived.uts46_ignored))
        self.emit_ranges("uts46_valid", to_ranges(derived.uts46_valid))
        self.emit_ranges("uts46_deviation", to_ranges(derived.uts46_deviation))
        self.emit_ranges("uts46_nv8", to_ranges(derived.uts46_nv8))
        self.emit_ranges("uts46_xv8", to_ranges(derived.uts46_xv8))


# ── Main ──

def main():
    parser = argparse.ArgumentParser(description="Generate OCaml IDNA/NFC/UTS46 tables")
    parser.add_argument("--format", type=int, default=64, choices=[32, 64],
                        help="64 (packed int) or 32 (parallel arrays)")
    parser.add_argument("--output", "-o", type=str, default=None)
    parser.add_argument("--props-shape", choices=["a1-direct"], default="a1-direct")
    parser.add_argument("--props-page-size", type=int, choices=[64, 128, 256], default=128)
    parser.add_argument("--uts46-map-page-size", type=int, choices=[128, 256], default=128)
    parser.add_argument("--ucd-dir", type=str,
                        default=os.environ.get("UCD_DIR",
                            os.path.join(os.path.dirname(__file__), "ucd-16.0.0")))
    args = parser.parse_args()

    ucd_version = os.path.basename(os.path.normpath(args.ucd_dir)).removeprefix("ucd-")
    py_version = unicodedata.unidata_version
    if ucd_version != py_version:
        sys.exit(
            f"Python unicodedata version {py_version} does not match UCD {ucd_version}. "
            f"Section 2.2 (Unstable) uses unicodedata.normalize; mismatched versions "
            f"produce wrong classification for codepoints added after {py_version}. "
            f"Use a Python whose unicodedata.unidata_version == {ucd_version}."
        )

    data = UCData(args.ucd_dir)
    derived = Derivation(data)

    out = sys.stdout
    if args.output:
        out = open(args.output, "w")

    emitter_args = {
        "props_shape": args.props_shape,
        "props_page_size": args.props_page_size,
        "uts46_map_page_size": args.uts46_map_page_size,
    }
    emitter = Emitter64(out, **emitter_args) if args.format == 64 else Emitter32(out, **emitter_args)
    emitter.emit_all(derived)

    if args.output:
        out.close()
        with open(args.output) as f:
            n = sum(1 for _ in f)
        print(f"Written {n} lines to {args.output}")


if __name__ == "__main__":
    main()
