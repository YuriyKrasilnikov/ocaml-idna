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

echo "Done. UCD ${VERSION} in ${DIR}/"
echo "Run: UCD_DIR=${DIR} python3 tools/gen_tables.py --dry-run"
