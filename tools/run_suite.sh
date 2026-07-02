#!/usr/bin/env bash
#
# run_suite.sh -- drive the engine over UCI on a small position suite and check
# each best move against the expected move. Positions live in
# res/bench/probes.txt as "FEN|expected_uci_move|note" lines.
#
# Usage: tools/run_suite.sh [engine] [movetime_ms] [suite_file]
#
# Each position is searched for movetime_ms; the extra sleep keeps stdin open
# so the fixed-time search finishes before EOF (the engine reads stdin
# concurrently with searching).

set -u

engine="${1:-./target/release/anekamacam}"
movetime="${2:-4000}"
suite="${3:-res/bench/probes.txt}"
hold=$(( movetime / 1000 + 2 ))

pass=0
total=0

while IFS='|' read -r fen expected note; do
    [ -z "${fen}" ] && continue
    total=$(( total + 1 ))

    output=$( { printf 'position fen %s\ngo movetime %s\n' "${fen}" \
        "${movetime}"; sleep "${hold}"; } | "${engine}" 2>/dev/null )
    best=$( printf '%s\n' "${output}" \
        | grep -oE 'bestmove [a-h0-9=]+' | head -1 | awk '{ print $2 }' )
    depth=$( printf '%s\n' "${output}" \
        | grep -oE 'depth [0-9]+' | tail -1 | awk '{ print $2 }' )

    if [ "${best}" = "${expected}" ]; then
        pass=$(( pass + 1 ))
        printf 'PASS  d%-3s  %-8s  %s\n' "${depth}" "${best}" "${note}"
    else
        printf 'FAIL  d%-3s  got %-8s want %-8s  %s\n' \
            "${depth}" "${best}" "${expected}" "${note}"
    fi
done < "${suite}"

printf '\n%d/%d passed (movetime %sms)\n' "${pass}" "${total}" "${movetime}"
