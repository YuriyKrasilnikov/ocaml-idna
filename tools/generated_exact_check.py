#!/usr/bin/env python3
"""Compiled generated-table exactness checker.

The expected side is independently derived from local Unicode/RFC source data.
The actual side is read from a small OCaml executable that dumps the compiled
`Idna_tables` module for the current switch and word size. This deliberately
does not parse generated OCaml source text.
"""

from __future__ import annotations

import argparse
import re
import subprocess
import sys
from pathlib import Path
from typing import Dict, Iterable, Set, Tuple


ROOT = Path(__file__).resolve().parent.parent
UCD_DIR = ROOT / "tools" / "ucd-16.0.0"
SetMap = Dict[str, Set[int]]
MappedTable = Dict[int, Tuple[int, ...]]
DecompTable = Dict[int, Tuple[int, int]]
CccTable = Dict[int, int]
CompositionTable = Dict[Tuple[int, int], int]
PropsTable = Dict[int, int]
PropsPairMap = Dict[str, Dict[int, int]]
DumpResult = Tuple[
    int,
    SetMap,
    MappedTable,
    DecompTable,
    CccTable,
    CompositionTable,
    PropsTable,
    SetMap,
    PropsPairMap,
]

S_BASE = 0xAC00
L_BASE = 0x1100
V_BASE = 0x1161
T_BASE = 0x11A7
L_COUNT = 19
V_COUNT = 21
T_COUNT = 28
N_COUNT = V_COUNT * T_COUNT
S_COUNT = L_COUNT * N_COUNT

U_MAX = 0x10FFFF

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

RFC5892_EXCEPTIONS: Dict[int, str] = {
    0x00DF: "PVALID",
    0x03C2: "PVALID",
    0x06FD: "PVALID",
    0x06FE: "PVALID",
    0x0F0B: "PVALID",
    0x3007: "PVALID",
    0x00B7: "CONTEXTO",
    0x0375: "CONTEXTO",
    0x05F3: "CONTEXTO",
    0x05F4: "CONTEXTO",
    0x30FB: "CONTEXTO",
    0x0660: "CONTEXTO",
    0x0661: "CONTEXTO",
    0x0662: "CONTEXTO",
    0x0663: "CONTEXTO",
    0x0664: "CONTEXTO",
    0x0665: "CONTEXTO",
    0x0666: "CONTEXTO",
    0x0667: "CONTEXTO",
    0x0668: "CONTEXTO",
    0x0669: "CONTEXTO",
    0x06F0: "CONTEXTO",
    0x06F1: "CONTEXTO",
    0x06F2: "CONTEXTO",
    0x06F3: "CONTEXTO",
    0x06F4: "CONTEXTO",
    0x06F5: "CONTEXTO",
    0x06F6: "CONTEXTO",
    0x06F7: "CONTEXTO",
    0x06F8: "CONTEXTO",
    0x06F9: "CONTEXTO",
    0x0640: "DISALLOWED",
    0x07FA: "DISALLOWED",
    0x302E: "DISALLOWED",
    0x302F: "DISALLOWED",
    0x3031: "DISALLOWED",
    0x3032: "DISALLOWED",
    0x3033: "DISALLOWED",
    0x3034: "DISALLOWED",
    0x3035: "DISALLOWED",
    0x303B: "DISALLOWED",
}

RFC5892_IGNORABLE_BLOCKS = {
    "Combining Diacritical Marks for Symbols",
    "Musical Symbols",
    "Ancient Greek Musical Notation",
}

RFC5892_BACKWARD_COMPATIBLE: Dict[int, str] = {}


def parse_cp_range(field: str) -> range:
    match = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?$", field)
    if not match:
        raise SystemExit(f"bad code point field: {field!r}")
    start = int(match.group(1), 16)
    end = int(match.group(2), 16) if match.group(2) else start
    return range(start, end + 1)


def parse_prop_file(path: Path) -> Dict[str, Set[int]]:
    props: Dict[str, Set[int]] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
        if not match:
            continue
        prop = match.group(3)
        field = match.group(1)
        if match.group(2):
            field += ".." + match.group(2)
        props.setdefault(prop, set()).update(parse_cp_range(field))
    return props


def parse_blocks(path: Path) -> Dict[int, str]:
    blocks: Dict[int, str] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"([0-9A-F]{4,6})\.\.([0-9A-F]{4,6});\s*(.*)", line)
        if not match:
            continue
        block = match.group(3).strip()
        for cp in range(int(match.group(1), 16), int(match.group(2), 16) + 1):
            blocks[cp] = block
    return blocks


def parse_hangul_syllable_types(path: Path) -> Dict[str, Set[int]]:
    out: Dict[str, Set[int]] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
        if not match:
            continue
        typ = match.group(3)
        field = match.group(1) + (".." + match.group(2) if match.group(2) else "")
        out.setdefault(typ, set()).update(parse_cp_range(field))
    return out


def parse_scripts(path: Path) -> Dict[str, Set[int]]:
    out: Dict[str, Set[int]] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
        if not match:
            continue
        script = match.group(3)
        field = match.group(1) + (".." + match.group(2) if match.group(2) else "")
        out.setdefault(script, set()).update(parse_cp_range(field))
    return out


def parse_joining_types(path: Path) -> Dict[int, int]:
    type_map = {"C": 0, "D": 1, "L": 2, "R": 3, "T": 4, "U": 5}
    out: Dict[int, int] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*(\S+)", line)
        if match and match.group(3) in type_map:
            field = match.group(1) + (".." + match.group(2) if match.group(2) else "")
            for cp in parse_cp_range(field):
                out[cp] = type_map[match.group(3)]
    return out


def parse_unicode_data(
    path: Path,
) -> Tuple[
    Dict[int, str],
    Dict[int, str],
    Dict[int, int],
    Dict[int, Tuple[int, ...]],
    Dict[int, Tuple[int, ...]],
]:
    general_category: Dict[int, str] = {}
    bidi_class: Dict[int, str] = {}
    ccc_map: Dict[int, int] = {}
    compat_decomp: Dict[int, Tuple[int, ...]] = {}
    canonical_decomp: Dict[int, Tuple[int, ...]] = {}
    range_begin: Tuple[int, str, str, int] | None = None

    for raw in path.read_text(encoding="utf-8").splitlines():
        fields = raw.split(";")
        cp = int(fields[0], 16)
        name = fields[1]
        category = fields[2]
        ccc = int(fields[3])
        bidi = fields[4]
        decomp = fields[5]

        if name.endswith(", First>"):
            range_begin = (cp, category, bidi, ccc)
            continue
        if name.endswith(", Last>"):
            assert range_begin is not None
            for i in range(range_begin[0], cp + 1):
                general_category[i] = range_begin[1]
                bidi_class[i] = range_begin[2]
                if range_begin[3] > 0:
                    ccc_map[i] = range_begin[3]
            range_begin = None
            continue

        general_category[cp] = category
        bidi_class[cp] = bidi
        if ccc > 0:
            ccc_map[cp] = ccc
        if decomp:
            parts = decomp.split()
            if parts[0].startswith("<"):
                seq = tuple(int(x, 16) for x in parts[1:])
                if seq:
                    compat_decomp[cp] = seq
            else:
                seq = tuple(int(x, 16) for x in parts)
                if seq:
                    compat_decomp[cp] = seq
                    canonical_decomp[cp] = seq

    return general_category, bidi_class, ccc_map, compat_decomp, canonical_decomp


def parse_case_folding(path: Path) -> Dict[int, str]:
    out: Dict[int, str] = {}
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(r"([0-9A-F]{4,6});\s*([CFTSI]);\s*([0-9A-F ]+)", line)
        if match and match.group(2) in ("C", "F"):
            out[int(match.group(1), 16)] = "".join(chr(int(x, 16)) for x in match.group(3).split())
    return out


def parse_full_composition_exclusions(path: Path) -> Set[int]:
    out: Set[int] = set()
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        match = re.match(
            r"([0-9A-F]{4,6})(?:\.\.([0-9A-F]{4,6}))?\s*;\s*Full_Composition_Exclusion\b",
            line,
        )
        if match:
            field = match.group(1) + (".." + match.group(2) if match.group(2) else "")
            out.update(parse_cp_range(field))
    return out


def parse_nfc_qc_non_yes(path: Path) -> Set[int]:
    out: Set[int] = set()
    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        parts = [part.strip() for part in line.split(";")]
        if len(parts) >= 3 and parts[1] == "NFC_QC" and parts[2] in ("N", "M"):
            out.update(parse_cp_range(parts[0]))
    return out


def hangul_decompose(cp: int) -> Tuple[int, ...] | None:
    if S_BASE <= cp < S_BASE + S_COUNT:
        s_index = cp - S_BASE
        l = L_BASE + s_index // N_COUNT
        v = V_BASE + (s_index % N_COUNT) // T_COUNT
        t = T_BASE + s_index % T_COUNT
        if t == T_BASE:
            return (l, v)
        return (l, v, t)
    return None


def hangul_compose(starter: int, combining: int) -> int | None:
    if L_BASE <= starter < L_BASE + L_COUNT and V_BASE <= combining < V_BASE + V_COUNT:
        l_index = starter - L_BASE
        v_index = combining - V_BASE
        return S_BASE + (l_index * V_COUNT + v_index) * T_COUNT
    if (
        S_BASE <= starter < S_BASE + S_COUNT
        and (starter - S_BASE) % T_COUNT == 0
        and T_BASE < combining < T_BASE + T_COUNT
    ):
        return starter + combining - T_BASE
    return None


def decompose_compat(cps: Tuple[int, ...], compat_decomp: Dict[int, Tuple[int, ...]]) -> Tuple[int, ...]:
    out = []

    def visit(cp: int) -> None:
        hangul = hangul_decompose(cp)
        if hangul is not None:
            for x in hangul:
                visit(x)
            return
        seq = compat_decomp.get(cp)
        if seq is not None:
            for x in seq:
                visit(x)
            return
        out.append(cp)

    for cp in cps:
        visit(cp)
    return tuple(out)


def canonical_order(cps: Tuple[int, ...], ccc_map: Dict[int, int]) -> Tuple[int, ...]:
    arr = list(cps)
    changed = True
    while changed:
        changed = False
        for i in range(len(arr) - 1):
            cc_a = ccc_map.get(arr[i], 0)
            cc_b = ccc_map.get(arr[i + 1], 0)
            if cc_a > 0 and cc_b > 0 and cc_a > cc_b:
                arr[i], arr[i + 1] = arr[i + 1], arr[i]
                changed = True
    return tuple(arr)


def build_composition_pairs(
    canonical_decomp: Dict[int, Tuple[int, ...]],
    exclusions: Set[int],
) -> Dict[Tuple[int, int], int]:
    out: Dict[Tuple[int, int], int] = {}
    for cp, seq in canonical_decomp.items():
        if len(seq) == 2 and cp not in exclusions:
            out[(seq[0], seq[1])] = cp
    return out


def compose(
    cps: Tuple[int, ...],
    ccc_map: Dict[int, int],
    composition_pairs: Dict[Tuple[int, int], int],
) -> Tuple[int, ...]:
    if not cps:
        return ()
    arr: list[int | None] = list(cps)
    starter_pos = 0
    last_cc = 0
    for i in range(1, len(arr)):
        cp = arr[i]
        if cp is None:
            continue
        cp_cc = ccc_map.get(cp, 0)
        blocked = last_cc != 0 and last_cc >= cp_cc
        starter = arr[starter_pos]
        assert starter is not None
        if not blocked:
            composite = hangul_compose(starter, cp)
            if composite is None:
                composite = composition_pairs.get((starter, cp))
            if composite is not None:
                arr[starter_pos] = composite
                arr[i] = None
            elif cp_cc == 0:
                starter_pos = i
                last_cc = 0
            else:
                last_cc = cp_cc
        elif cp_cc == 0:
            starter_pos = i
            last_cc = 0
        else:
            last_cc = cp_cc
    return tuple(cp for cp in arr if cp is not None)


def nfkc_string(
    s: str,
    *,
    compat_decomp: Dict[int, Tuple[int, ...]],
    ccc_map: Dict[int, int],
    composition_pairs: Dict[Tuple[int, int], int],
) -> str:
    cps = tuple(ord(c) for c in s)
    cps = decompose_compat(cps, compat_decomp)
    cps = canonical_order(cps, ccc_map)
    cps = compose(cps, ccc_map, composition_pairs)
    return "".join(chr(cp) for cp in cps)


def casefold_string(s: str, case_folding: Dict[int, str]) -> str:
    return "".join(case_folding.get(ord(c), c) for c in s)


def derive_rfc5892_status(
    cp: int,
    *,
    general_category: Dict[int, str],
    noncharacter: Set[int],
    join_control: Set[int],
    default_ignorable: Set[int],
    white_space: Set[int],
    blocks: Dict[int, str],
    hangul_types: Dict[str, Set[int]],
    case_folding: Dict[int, str],
    compat_decomp: Dict[int, Tuple[int, ...]],
    ccc_map: Dict[int, int],
    composition_pairs: Dict[Tuple[int, int], int],
) -> str:
    if cp in RFC5892_EXCEPTIONS:
        return RFC5892_EXCEPTIONS[cp]
    if cp in RFC5892_BACKWARD_COMPATIBLE:
        return RFC5892_BACKWARD_COMPATIBLE[cp]

    gc = general_category.get(cp)
    if gc is None and cp not in noncharacter:
        return "UNASSIGNED"

    if cp == 0x2D or 0x30 <= cp <= 0x39 or 0x61 <= cp <= 0x7A:
        return "PVALID"
    if cp in join_control:
        return "CONTEXTJ"

    ch = chr(cp)
    nfkc = nfkc_string(
        ch,
        compat_decomp=compat_decomp,
        ccc_map=ccc_map,
        composition_pairs=composition_pairs,
    )
    nfkc_cf = nfkc_string(
        casefold_string(nfkc, case_folding),
        compat_decomp=compat_decomp,
        ccc_map=ccc_map,
        composition_pairs=composition_pairs,
    )
    if ch != nfkc_cf:
        return "DISALLOWED"

    if cp in default_ignorable or cp in white_space or cp in noncharacter:
        return "DISALLOWED"
    if blocks.get(cp) in RFC5892_IGNORABLE_BLOCKS:
        return "DISALLOWED"
    if cp in hangul_types.get("L", set()) or cp in hangul_types.get("V", set()) or cp in hangul_types.get("T", set()):
        return "DISALLOWED"
    if gc in ("Ll", "Lu", "Lo", "Nd", "Lm", "Mn", "Mc"):
        return "PVALID"
    return "DISALLOWED"


def derive_rfc5892_sets(
    *,
    general_category: Dict[int, str],
    ccc_map: Dict[int, int],
    compat_decomp: Dict[int, Tuple[int, ...]],
    canonical_decomp: Dict[int, Tuple[int, ...]],
    exclusions: Set[int],
) -> Dict[str, Set[int]]:
    prop_list = parse_prop_file(UCD_DIR / "PropList.txt")
    derived_props = parse_prop_file(UCD_DIR / "DerivedCoreProperties.txt")
    blocks = parse_blocks(UCD_DIR / "Blocks.txt")
    hangul = parse_hangul_syllable_types(UCD_DIR / "HangulSyllableType.txt")
    case_folding = parse_case_folding(UCD_DIR / "CaseFolding.txt")
    composition_pairs = build_composition_pairs(canonical_decomp, exclusions)

    result = {"PVALID": set(), "CONTEXTJ": set(), "CONTEXTO": set()}
    for cp in range(0x110000):
        status = derive_rfc5892_status(
            cp,
            general_category=general_category,
            noncharacter=prop_list.get("Noncharacter_Code_Point", set()),
            join_control=prop_list.get("Join_Control", set()),
            default_ignorable=derived_props.get("Default_Ignorable_Code_Point", set()),
            white_space=prop_list.get("White_Space", set()),
            blocks=blocks,
            hangul_types=hangul,
            case_folding=case_folding,
            compat_decomp=compat_decomp,
            ccc_map=ccc_map,
            composition_pairs=composition_pairs,
        )
        if status in result:
            result[status].add(cp)
    return result


def parse_uts46_source(
    path: Path,
) -> Tuple[Dict[int, Tuple[int, ...]], Set[int], Set[int], Set[int], Set[int], Set[int]]:
    mapped: Dict[int, Tuple[int, ...]] = {}
    ignored: Set[int] = set()
    valid: Set[int] = set()
    deviation: Set[int] = set()
    nv8: Set[int] = set()
    xv8: Set[int] = set()

    for raw in path.read_text(encoding="utf-8").splitlines():
        line = raw.split("#", 1)[0].strip()
        if not line:
            continue
        parts = [part.strip() for part in line.split(";")]
        if len(parts) < 2:
            continue
        cps = parse_cp_range(parts[0])
        status = parts[1]
        mapping_field = parts[2] if len(parts) > 2 else ""
        idna2008_status = parts[3] if len(parts) > 3 else ""

        if status == "mapped":
            seq = tuple(int(x, 16) for x in mapping_field.split())
            for cp in cps:
                mapped[cp] = seq
        elif status == "ignored":
            ignored.update(cps)
        elif status == "valid":
            valid.update(cps)
        elif status == "deviation":
            deviation.update(cps)

        if idna2008_status == "NV8":
            nv8.update(cps)
        elif idna2008_status == "XV8":
            xv8.update(cps)

    return mapped, ignored, valid, deviation, nv8, xv8


def expected_nfc_tables(
    canonical_decomp: Dict[int, Tuple[int, ...]],
    ccc_map: Dict[int, int],
    exclusions: Set[int],
) -> Tuple[Dict[int, Tuple[int, int]], Dict[int, int], Dict[Tuple[int, int], int]]:
    decomp: Dict[int, Tuple[int, int]] = {}
    for cp, seq in canonical_decomp.items():
        if 0xAC00 <= cp <= 0xD7A3:
            continue
        if len(seq) == 1:
            decomp[cp] = (seq[0], 0)
        elif len(seq) == 2:
            decomp[cp] = (seq[0], seq[1])
    return decomp, ccc_map, build_composition_pairs(canonical_decomp, exclusions)


def add_set(out: SetMap, name: str, cps: Iterable[int]) -> None:
    out.setdefault(name, set()).update(cps)


def build_expected_props(
    *,
    general_category: Dict[int, str],
    bidi_class: Dict[int, str],
    ccc_map: Dict[int, int],
    canonical_decomp: Dict[int, Tuple[int, ...]],
    rfc5892: Dict[str, Set[int]],
    scripts: Dict[str, Set[int]],
    joining_types: Dict[int, int],
    nfc_qc_non_yes: Set[int],
    uts46_mapped: MappedTable,
    uts46_ignored: Set[int],
    uts46_valid: Set[int],
    uts46_deviation: Set[int],
    uts46_nv8: Set[int],
    uts46_xv8: Set[int],
) -> Tuple[PropsTable, SetMap, PropsPairMap]:
    idna_by_cp: Dict[int, int] = {}
    for cp in rfc5892["PVALID"]:
        idna_by_cp[cp] = 1
    for cp in rfc5892["CONTEXTJ"]:
        idna_by_cp[cp] = 2
    for cp in rfc5892["CONTEXTO"]:
        idna_by_cp[cp] = 3

    uts_by_cp: Dict[int, int] = {}
    for cp in uts46_valid:
        uts_by_cp[cp] = 1
    for cp in uts46_ignored:
        uts_by_cp[cp] = 2
    for cp in uts46_deviation:
        uts_by_cp[cp] = 3
    for cp in uts46_mapped:
        uts_by_cp[cp] = 4

    script_by_cp: Dict[int, int] = {}
    for script, bit in [
        ("Greek", 0),
        ("Hebrew", 1),
        ("Han", 2),
        ("Hiragana", 3),
        ("Katakana", 4),
    ]:
        for cp in scripts.get(script, set()):
            script_by_cp[cp] = script_by_cp.get(cp, 0) | (1 << bit)

    props: PropsTable = {}
    for cp in range(U_MAX + 1):
        value = 0
        value |= uts_by_cp.get(cp, 0) << PROP_B_UTS
        value |= idna_by_cp.get(cp, 0) << PROP_B_IDNA
        value |= PROP_BIDI_CODE.get(bidi_class.get(cp, ""), 0) << PROP_B_BIDI
        value |= ccc_map.get(cp, 0) << PROP_B_CCC
        joining = joining_types.get(cp)
        if joining is not None:
            value |= (joining + 1) << PROP_B_JOIN
        value |= script_by_cp.get(cp, 0) << PROP_B_SCRIPT
        if general_category.get(cp, "").startswith("M"):
            value |= 1 << PROP_B_MARK
        if cp in nfc_qc_non_yes:
            value |= 1 << PROP_B_NFC_QC
        if cp in uts46_nv8:
            value |= 1 << PROP_B_NV8
        if cp in uts46_xv8:
            value |= 1 << PROP_B_XV8
        if cp in canonical_decomp:
            value |= 1 << PROP_B_DECOMP
        if value:
            props[cp] = value

    sets: SetMap = {}
    add_set(sets, "uts46_valid", uts46_valid)
    add_set(sets, "uts46_ignored", uts46_ignored)
    add_set(sets, "uts46_deviation", uts46_deviation)
    add_set(sets, "uts46_mapped", uts46_mapped.keys())
    add_set(sets, "codepoint_pvalid", rfc5892["PVALID"])
    add_set(sets, "codepoint_contextj", rfc5892["CONTEXTJ"])
    add_set(sets, "codepoint_contexto", rfc5892["CONTEXTO"])
    for bidi_name, code in PROP_BIDI_CODE.items():
        add_set(
            sets,
            "bidi_" + bidi_name.lower(),
            (cp for cp, cls in bidi_class.items() if cls == bidi_name),
        )
    for script in ("Greek", "Hebrew", "Han", "Hiragana", "Katakana"):
        add_set(sets, "script_" + script.lower(), scripts.get(script, set()))
    add_set(sets, "general_category_m", (cp for cp, gc in general_category.items() if gc.startswith("M")))
    add_set(sets, "nfc_qc_non_yes", nfc_qc_non_yes)
    add_set(sets, "uts46_nv8", uts46_nv8)
    add_set(sets, "uts46_xv8", uts46_xv8)
    add_set(sets, "canon_decomp_present", canonical_decomp.keys())

    pairs: PropsPairMap = {
        "uts46_status": uts_by_cp,
        "idna_class": idna_by_cp,
        "bidi_class": {
            cp: PROP_BIDI_CODE[cls]
            for cp, cls in bidi_class.items()
            if cls in PROP_BIDI_CODE
        },
        "canon_ccc": ccc_map,
        "joining_type": {cp: value + 1 for cp, value in joining_types.items()},
    }

    return props, sets, pairs


def run_dump(dump_exe: str) -> DumpResult:
    proc = subprocess.run([dump_exe], check=True, text=True, stdout=subprocess.PIPE)
    word_size = 0
    sets: Dict[str, Set[int]] = {}
    mapped: Dict[int, Tuple[int, ...]] = {}
    decomp: Dict[int, Tuple[int, int]] = {}
    ccc: Dict[int, int] = {}
    comp: Dict[Tuple[int, int], int] = {}
    props: PropsTable = {}
    prop_sets: SetMap = {}
    prop_pairs: PropsPairMap = {}

    for line in proc.stdout.splitlines():
        parts = line.split("\t")
        if not parts:
            continue
        if parts[0] == "meta" and parts[1] == "word_size":
            word_size = int(parts[2])
        elif parts[0] == "set":
            sets.setdefault(parts[1], set()).add(int(parts[2], 16))
        elif parts[0] == "map" and parts[1] == "uts46_mapped":
            mapped[int(parts[2], 16)] = tuple(int(x, 16) for x in parts[3:])
        elif parts[0] == "canon_decomp":
            decomp[int(parts[1], 16)] = (int(parts[2], 16), int(parts[3], 16))
        elif parts[0] == "canon_ccc":
            ccc[int(parts[1], 16)] = int(parts[2])
        elif parts[0] == "nfc_comp":
            comp[(int(parts[1], 16), int(parts[2], 16))] = int(parts[3], 16)
        elif parts[0] == "props":
            props[int(parts[1], 16)] = int(parts[2], 16)
        elif parts[0] == "props_set":
            prop_sets.setdefault(parts[1], set()).add(int(parts[2], 16))
        elif parts[0] == "props_pair":
            prop_pairs.setdefault(parts[1], {})[int(parts[2], 16)] = int(parts[3], 0)
        else:
            raise SystemExit(f"unknown dump line: {line!r}")

    if word_size not in (32, 64):
        raise SystemExit("dump did not include a valid word_size header")
    return word_size, sets, mapped, decomp, ccc, comp, props, prop_sets, prop_pairs


def report_many_sets(label_prefix: str, expected: SetMap, actual: SetMap) -> int:
    failures = 0
    for name in sorted(expected):
        failures += report(f"{label_prefix} {name}", expected[name], actual.get(name, set()))
    extra_names = sorted(set(actual) - set(expected))
    for name in extra_names:
        print(f"{label_prefix} {name}: MISMATCH missing=0 extra={len(actual[name])}")
        print(f"  extra_sample: {sorted(actual[name])[:3]}")
        failures += 1
    return failures


def report_many_pairs(label_prefix: str, expected: PropsPairMap, actual: PropsPairMap) -> int:
    failures = 0
    for name in sorted(expected):
        failures += report(f"{label_prefix} {name}", expected[name], actual.get(name, {}))
    extra_names = sorted(set(actual) - set(expected))
    for name in extra_names:
        print(f"{label_prefix} {name}: MISMATCH missing=0 extra={len(actual[name])}")
        print(f"  extra_sample: {sorted(actual[name].items())[:3]}")
        failures += 1
    return failures


def report(label: str, expected, actual) -> int:
    if expected == actual:
        print(f"{label}: OK ({len(expected)} entries)")
        return 0
    if isinstance(expected, dict):
        missing = sorted(set(expected.items()) - set(actual.items()))
        extra = sorted(set(actual.items()) - set(expected.items()))
    else:
        missing = sorted(expected - actual)
        extra = sorted(actual - expected)
    print(f"{label}: MISMATCH missing={len(missing)} extra={len(extra)}")
    if missing:
        print(f"  missing_sample: {missing[:3]}")
    if extra:
        print(f"  extra_sample: {extra[:3]}")
    return 1


def require_inputs(paths: Iterable[Path]) -> int:
    missing = [path for path in paths if not path.exists()]
    if missing:
        for path in missing:
            print(f"missing generated exactness input: {path}", file=sys.stderr)
        print("Run ./tools/download_ucd.sh 16.0.0 first.", file=sys.stderr)
        return 77
    return 0


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--dump-exe", required=True)
    args = parser.parse_args()

    required = [
        UCD_DIR / "UnicodeData.txt",
        UCD_DIR / "PropList.txt",
        UCD_DIR / "DerivedCoreProperties.txt",
        UCD_DIR / "Blocks.txt",
        UCD_DIR / "HangulSyllableType.txt",
        UCD_DIR / "CaseFolding.txt",
        UCD_DIR / "Scripts.txt",
        UCD_DIR / "DerivedJoiningType.txt",
        UCD_DIR / "DerivedNormalizationProps.txt",
        UCD_DIR / "IdnaMappingTable.txt",
    ]
    missing_status = require_inputs(required)
    if missing_status:
        return missing_status

    (
        word_size,
        actual_sets,
        actual_mapped,
        actual_decomp,
        actual_ccc,
        actual_comp,
        actual_props,
        actual_prop_sets,
        actual_prop_pairs,
    ) = run_dump(args.dump_exe)
    print(f"compiled generated table word size: {word_size}")

    general_category, bidi_class, ccc_map, compat_decomp, canonical_decomp = parse_unicode_data(
        UCD_DIR / "UnicodeData.txt"
    )
    exclusions = parse_full_composition_exclusions(UCD_DIR / "DerivedNormalizationProps.txt")
    rfc5892 = derive_rfc5892_sets(
        general_category=general_category,
        ccc_map=ccc_map,
        compat_decomp=compat_decomp,
        canonical_decomp=canonical_decomp,
        exclusions=exclusions,
    )
    uts46_mapped, uts46_ignored, uts46_valid, uts46_deviation, uts46_nv8, uts46_xv8 = parse_uts46_source(
        UCD_DIR / "IdnaMappingTable.txt"
    )
    nfc_decomp, nfc_ccc, nfc_comp = expected_nfc_tables(canonical_decomp, ccc_map, exclusions)
    nfc_qc_non_yes = parse_nfc_qc_non_yes(UCD_DIR / "DerivedNormalizationProps.txt")
    scripts = parse_scripts(UCD_DIR / "Scripts.txt")
    joining_types = parse_joining_types(UCD_DIR / "DerivedJoiningType.txt")
    expected_props, expected_prop_sets, expected_prop_pairs = build_expected_props(
        general_category=general_category,
        bidi_class=bidi_class,
        ccc_map=ccc_map,
        canonical_decomp=canonical_decomp,
        rfc5892=rfc5892,
        scripts=scripts,
        joining_types=joining_types,
        nfc_qc_non_yes=nfc_qc_non_yes,
        uts46_mapped=uts46_mapped,
        uts46_ignored=uts46_ignored,
        uts46_valid=uts46_valid,
        uts46_deviation=uts46_deviation,
        uts46_nv8=uts46_nv8,
        uts46_xv8=uts46_xv8,
    )

    failures = 0
    failures += report("PVALID vs compiled", rfc5892["PVALID"], actual_sets.get("codepoint_pvalid", set()))
    failures += report("CONTEXTJ vs compiled", rfc5892["CONTEXTJ"], actual_sets.get("codepoint_contextj", set()))
    failures += report("CONTEXTO vs compiled", rfc5892["CONTEXTO"], actual_sets.get("codepoint_contexto", set()))
    failures += report("uts46_mapped vs compiled", uts46_mapped, actual_mapped)
    failures += report("uts46_ignored vs compiled", uts46_ignored, actual_sets.get("uts46_ignored", set()))
    failures += report("uts46_valid vs compiled", uts46_valid, actual_sets.get("uts46_valid", set()))
    failures += report("uts46_deviation vs compiled", uts46_deviation, actual_sets.get("uts46_deviation", set()))
    failures += report("uts46_nv8 vs compiled", uts46_nv8, actual_sets.get("uts46_nv8", set()))
    failures += report("uts46_xv8 vs compiled", uts46_xv8, actual_sets.get("uts46_xv8", set()))
    failures += report("canon_decomp vs compiled", nfc_decomp, actual_decomp)
    failures += report("canon_ccc vs compiled", nfc_ccc, actual_ccc)
    failures += report("nfc_compositions vs compiled", nfc_comp, actual_comp)
    failures += report("nfc_qc_non_yes vs compiled", nfc_qc_non_yes, actual_sets.get("nfc_qc_non_yes", set()))
    failures += report("props raw vs compiled", expected_props, actual_props)
    failures += report_many_sets("props_set vs compiled", expected_prop_sets, actual_prop_sets)
    failures += report_many_pairs("props_pair vs compiled", expected_prop_pairs, actual_prop_pairs)
    failures += report(
        "uts46_mapped props vs metadata",
        actual_prop_sets.get("uts46_mapped", set()),
        set(actual_mapped),
    )
    return 1 if failures else 0


if __name__ == "__main__":
    sys.exit(main())
