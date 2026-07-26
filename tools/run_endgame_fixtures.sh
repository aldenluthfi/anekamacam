#!/usr/bin/env bash
#
# Run the decisive end-condition regression fixtures.
#
# Reads tools/endgame_fixtures.txt (see its header for the format), drives the
# release engine over UCI for each case, and asserts the `d` Result line. Exits
# non-zero if any case fails, so it doubles as a CI check.
#
# Usage: tools/run_endgame_fixtures.sh   (build the release binary first)

set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/target/release/anekamacam"
FIXTURES="$ROOT/tools/endgame_fixtures.txt"

if [ ! -x "$BIN" ]; then
    echo "release binary not found: $BIN"
    echo "build it first: cargo build --release"
    exit 2
fi

trim() { sed 's/^[[:space:]]*//;s/[[:space:]]*$//'; }

pass=0
fail=0

while IFS='|' read -r variant fen moves expected description; do
    case "$variant" in
        ''|\#*) continue ;;
    esac

    variant=$(printf '%s' "$variant" | trim)
    fen=$(printf '%s' "$fen" | trim)
    moves=$(printf '%s' "$moves" | trim)
    expected=$(printf '%s' "$expected" | trim)
    description=$(printf '%s' "$description" | trim)

    if [ "$fen" = "startpos" ]; then
        posline="position startpos moves $moves"
    else
        posline="position fen $fen moves $moves"
    fi

    got=$(printf 'uci\nsetoption name UCI_Variant value %s\n%s\nd\nquit\n' \
            "$variant" "$posline" \
          | "$BIN" 2>/dev/null | sed -n 's/^Result: //p' | tail -1)

    if [ "$got" = "$expected" ]; then
        pass=$((pass + 1))
        printf 'PASS  %-11s %-52s [%s]\n' "$variant" "$description" "$got"
    else
        fail=$((fail + 1))
        printf 'FAIL  %-11s %-52s expected [%s] got [%s]\n' \
            "$variant" "$description" "$expected" "$got"
    fi
done < "$FIXTURES"

echo "--------------------------------------------------------------"
echo "$pass passed, $fail failed"
[ "$fail" -eq 0 ]
