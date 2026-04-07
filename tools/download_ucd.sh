#!/bin/sh
# Download Unicode Character Database files needed for IDNA2008 table generation.
# Usage: ./tools/download_ucd.sh [version]
# Default version: 16.0.0

VERSION="${1:-16.0.0}"
BASE_URL="https://www.unicode.org/Public/${VERSION}/ucd"
DIR="tools/ucd-${VERSION}"

mkdir -p "$DIR"

FILES="
UnicodeData.txt
PropList.txt
DerivedCoreProperties.txt
Blocks.txt
CaseFolding.txt
HangulSyllableType.txt
Scripts.txt
DerivedNormalizationProps.txt
extracted/DerivedJoiningType.txt
"

for f in $FILES; do
    out="$DIR/$(basename $f)"
    if [ -f "$out" ]; then
        echo "  exists: $out"
    else
        echo "  downloading: $f -> $out"
        wget -q "${BASE_URL}/${f}" -O "$out" || curl -sL "${BASE_URL}/${f}" -o "$out"
    fi
done

# IDNA-specific files (from /Public/idna/)
IDNA_URL="https://www.unicode.org/Public/idna/${VERSION}"
IDNA_FILES="
IdnaTestV2.txt
IdnaMappingTable.txt
"

for f in $IDNA_FILES; do
    out="$DIR/$(basename $f)"
    if [ -f "$out" ]; then
        echo "  exists: $out"
    else
        echo "  downloading: idna/$f -> $out"
        wget -q "${IDNA_URL}/${f}" -O "$out" || curl -sL "${IDNA_URL}/${f}" -o "$out"
    fi
done

# NormalizationTest.txt (for NFC verification)
NFC_URL="${BASE_URL}/NormalizationTest.txt"
NFC_OUT="$DIR/NormalizationTest.txt"
if [ -f "$NFC_OUT" ]; then
    echo "  exists: $NFC_OUT"
else
    echo "  downloading: NormalizationTest.txt -> $NFC_OUT"
    wget -q "$NFC_URL" -O "$NFC_OUT" || curl -sL "$NFC_URL" -o "$NFC_OUT"
fi

echo "Done. UCD ${VERSION} in ${DIR}/"
echo "Run: UCD_DIR=${DIR} python3 tools/gen_tables.py --dry-run"
