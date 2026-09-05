#!/usr/bin/env bash
#
# Run the decisive end-condition regression fixtures.
#
# Reads tools/endgame_fixtures.txt (see its header for the format), drives the
# release engine over UCI for each game-truth case, and asserts the `d` Result
# line. Exits non-zero if any case fails, so it doubles as a CI check.
#
# A case whose expectation reads `score cp` or `score mate` is a search case
# instead: synchronous `debug-headless search` reaches its full depth before
# returning, then the same UCI score text is asserted. Piping `go` followed by
# `quit` cannot do this: quit stops and joins the active search, so the last
# score may belong to an earlier completed iteration. Search covers verdicts
# the `d` oracle cannot see -- a perpetual scored terminal before the game
# itself has ended is the reason this mode exists.
#
# That depth is `GO_DEPTH`, or the case's own trailing depth field where it
# asks for more. Raising `GO_DEPTH` therefore still deepens every case, while
# one expensive case does not tax the rest.
#
# Adding the value, as in `score mate -2`, asserts the whole score rather than
# its kind. Sign is what separates a perpetual verdict from an ordinary one:
# the side the rule punishes and the side being mated are opposites, so a case
# that must not adjudicate early pins the number.
#
# Usage: tools/run_endgame_fixtures.sh   (build the release binary first)
#        GO_DEPTH=8 tools/run_endgame_fixtures.sh
#        BIN=bin/phaseC-3 tools/run_endgame_fixtures.sh
#
# The BIN override is how a whole set of phase binaries is checked to be
# playing one rule set: run it against each and require the same result on
# every case. Phase binaries built before a rules fix will not agree.

set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="${BIN:-$ROOT/target/release/anekamacam}"
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

# Runs a fixed-depth search synchronously, so EOF cannot interrupt its last
# iteration. Arguments stay split as moves while the FEN remains one value.
drive_search() {
    local -a arguments move_list
    arguments=(debug-headless search "$variant" "$case_depth" 1)

    if [ "$fen" != "startpos" ]; then
        arguments+=(--fen "$fen")
    fi
    arguments+=(--protocol uci)

    if [ -n "$moves" ]; then
        read -r -a move_list <<< "$moves"
        arguments+=(--moves "${move_list[@]}")
    fi

    "$BIN" "${arguments[@]}" 2>/dev/null
}

pass=0
fail=0

while IFS='|' read -r variant fen moves expected description depth; do
    case "$variant" in
        ''|\#*) continue ;;
    esac

    variant=$(printf '%s' "$variant" | trim)
    fen=$(printf '%s' "$fen" | trim)
    moves=$(printf '%s' "$moves" | trim)
    expected=$(printf '%s' "$expected" | trim)
    description=$(printf '%s' "$description" | trim)
    depth=$(printf '%s' "$depth" | trim)

    case_depth=$GO_DEPTH
    if [ -n "$depth" ] && [ "$depth" -gt "$GO_DEPTH" ]; then
        case_depth=$depth
    fi

    if [ "$fen" = "startpos" ]; then
        posline="position startpos moves $moves"
    else
        posline="position fen $fen moves $moves"
    fi

    case "$expected" in
        score\ *)
            got=$(drive_search \
                  | grep -o 'score [a-z]* -\{0,1\}[0-9]\{1,\}' | tail -1)
            case "$expected" in
                score\ *\ *) ;;
                *) got=$(printf '%s' "$got" | cut -d' ' -f1,2) ;;
            esac
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
