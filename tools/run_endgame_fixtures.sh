#!/usr/bin/env bash
#
# Run the decisive end-condition regression fixtures.
#
# Reads tools/endgame_fixtures.txt (see its header for the format), drives the
# release engine over UCI for each case, and asserts the `d` Result line. Exits
# non-zero if any case fails, so it doubles as a CI check.
#
# A case whose expectation reads `score cp` or `score mate` is a search case
# instead: it runs `go depth $GO_DEPTH` and asserts the kind of the last score
# reported. That covers the verdicts search reaches on its own, which the `d`
# oracle cannot see -- a perpetual scored terminal before the game itself has
# ended is the reason this mode exists.
#
# Usage: tools/run_endgame_fixtures.sh   (build the release binary first)
#        GO_DEPTH=8 tools/run_endgame_fixtures.sh

set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/target/release/anekamacam"
FIXTURES="$ROOT/tools/endgame_fixtures.txt"
GO_DEPTH="${GO_DEPTH:-6}"

if [ ! -x "$BIN" ]; then
    echo "release binary not found: $BIN"
    echo "build it first: cargo build --release"
    exit 2
fi

trim() { sed 's/^[[:space:]]*//;s/[[:space:]]*$//'; }

# Drives one case: sets the variant, plays the position, then runs the probe
# the case asked for (`d` for a game-truth case, `go ...` for a search case).
drive() {
    printf 'uci\nsetoption name UCI_Variant value %s\n%s\n%s\nquit\n' \
        "$variant" "$posline" "$1" | "$BIN" 2>/dev/null
}

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

    case "$expected" in
        score\ *)
            got=$(drive "go depth $GO_DEPTH" \
                  | grep -o 'score [a-z]*' | tail -1)
            ;;
        *)
            got=$(drive d | sed -n 's/^Result: //p' | tail -1)
            ;;
    esac

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
