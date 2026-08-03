#!/usr/bin/env bash
#
# Check every variant's FEN dictionary against the engine's own FEN writer.
#
# For each config, and for each protocol its dictionary declares, the startpos
# FEN is written out through that dialect and fed straight back in. Three
# things are asserted:
#
#   BREAK  the translated FEN does not parse back, or parses to a different
#          position -- the dictionary and the parser disagree
#   NO-OP  the translated FEN is byte-identical to the internal one, so no
#          rule fired at all and the dialect is getting our private format
#   ok     out-and-back is stable and the dialect form really differs
#
# A variant with no dictionary is reported NO-DICT and skipped: it speaks the
# internal format by construction. NO-OP is a failure for a variant whose
# dictionary declares fen rules, because those rules exist precisely to make
# the two forms differ.
#
# When fairy-stockfish is on PATH it also acts as an outside reader for the
# uci dialect: our FEN is handed to it and its echo compared. That check runs
# only for variants whose starting board it agrees with, so a piece-letter or
# rules difference (makruk's khon, our janggi's setup phase) reads as INFO
# rather than a failure -- those are questions about what to model, not about
# whether the dictionary emits the shape it claims to.
#
# Usage: tools/run_fen_roundtrip.sh   (build the release binary first)

set -u

ROOT="$(cd "$(dirname "$0")/.." && pwd)"
BIN="$ROOT/target/release/anekamacam"

if [ ! -x "$BIN" ]; then
    echo "release binary not found: $BIN"
    echo "build it first: cargo build --release"
    exit 2
fi

pass=0
fail=0
skip=0

internal_fen() {
    "$BIN" debug-headless state "$1" 2>/dev/null | sed -n 's/^FEN: //p'
}

dialect_fen() {
    "$BIN" debug-headless state "$1" --protocol "$2" 2>/dev/null \
        | sed -n 's/^FEN: //p'
}

reparsed_fen() {
    "$BIN" debug-headless state "$1" --fen "$3" --protocol "$2" 2>/dev/null \
        | sed -n 's/^FEN: //p'
}

reference_fen() {
    printf 'setoption name UCI_Variant value %s\nposition %s\nd\nquit\n' \
        "$1" "${2:-startpos}" \
        | fairy-stockfish 2>/dev/null | sed -n 's/^Fen: //p'
}

for config in "$ROOT"/configs/*.conf; do
    variant=$(basename "$config" .conf)
    [ "$variant" = "example" ] && continue

    internal=$(internal_fen "$variant")
    if [ -z "$internal" ]; then
        printf 'FAIL  %-13s %-5s config does not load\n' "$variant" '-'
        fail=$((fail + 1))
        continue
    fi

    dict="$ROOT/res/dicts/$variant.dict"
    if [ ! -f "$dict" ] || ! grep -q 'fen =' "$dict"; then
        printf 'SKIP  %-13s %-5s no fen rules\n' "$variant" '-'
        skip=$((skip + 1))
        continue
    fi

    while read -r protocol; do
        [ -z "$protocol" ] && continue
        grep -q "= $protocol fen =" "$dict" || continue

        out=$(dialect_fen "$variant" "$protocol")
        back=$(reparsed_fen "$variant" "$protocol" "$out")

        if [ "$out" != "$back" ]; then
            printf 'FAIL  %-13s %-5s does not survive a round trip\n' \
                "$variant" "$protocol"
            printf '      out  [%s]\n      back [%s]\n' "$out" "$back"
            fail=$((fail + 1))
        elif [ "$out" = "$internal" ]; then
            printf 'FAIL  %-13s %-5s no rule fired, dialect gets [%s]\n' \
                "$variant" "$protocol" "$out"
            fail=$((fail + 1))
        elif [ "$protocol" = "uci" ] && command -v fairy-stockfish >/dev/null
        then
            reference=$(reference_fen "$variant")
            if [ "${reference%% *}" != "${out%% *}" ]; then
                printf 'INFO  %-13s %-5s %s\n' "$variant" "$protocol" \
                    'reference models a different board, not cross-checked'
                pass=$((pass + 1))
            elif [ "$(reference_fen "$variant" "fen $out")" != "$out" ]; then
                printf 'FAIL  %-13s %-5s reference will not read it back\n' \
                    "$variant" "$protocol"
                printf '      sent [%s]\n      ref  [%s]\n' \
                    "$out" "$(reference_fen "$variant" "fen $out")"
                fail=$((fail + 1))
            else
                printf 'PASS  %-13s %-5s %s\n' "$variant" "$protocol" "$out"
                pass=$((pass + 1))
            fi
        else
            printf 'PASS  %-13s %-5s %s\n' "$variant" "$protocol" "$out"
            pass=$((pass + 1))
        fi
    done < <(sed -n '/= protocols =/,/^$/p' "$dict" | sed '/=/d;/^$/d')
done

echo "--------------------------------------------------------------"
echo "$pass passed, $fail failed, $skip skipped"

[ "$fail" -eq 0 ]
