#!/usr/bin/env bash
#
# run_ttd.sh -- measure move-stabilization depth and time-to-depth per probe
# position over several runs (search is nondeterministic, so medians matter).
# For each "FEN|expected_uci_move|note" line the script reports the depth at
# which the expected move becomes the principal-variation move and stays
# there (stable depth), nodes and time at that depth, and nodes/time at a
# fixed reference depth for NPS comparison.
#
# Usage: tools/run_ttd.sh [engine] [movetime_ms] [suite] [runs] [refdepth]

set -u

engine="${1:-./target/release/anekamacam}"
movetime="${2:-15000}"
suite="${3:-res/bench/probes.txt}"
runs="${4:-5}"
refdepth="${5:-12}"
hold=$(( movetime / 1000 + 2 ))

median() {
    tr ' ' '\n' | sed '/^$/d' | sort -n | awk '{ v[NR] = $1 } END {
        if (NR == 0) { print "-"; exit }
        if (NR % 2) { print v[(NR + 1) / 2] }
        else { print (v[NR / 2] + v[NR / 2 + 1]) / 2 }
    }'
}

while IFS='|' read -r fen expected note; do
    [ -z "${fen}" ] && continue
    printf '=== %s | want %s\n' "${note}" "${expected}"

    stable_depths=""
    stable_times=""
    stable_nodes=""
    ref_times=""
    ref_nodes=""
    found=0

    for run in $(seq 1 "${runs}"); do
        output=$( { printf 'position fen %s\ngo movetime %s\n' \
            "${fen}" "${movetime}"; sleep "${hold}"; } \
            | "${engine}" 2>/dev/null )

        stats=$( printf '%s\n' "${output}" | awk -v want="${expected}" \
            -v ref="${refdepth}" '
            $1 == "info" {
                depth = ""; nodes = ""; time = ""; mv = ""
                for (i = 2; i <= NF; i++) {
                    if ($i == "depth") depth = $(i + 1)
                    if ($i == "nodes") nodes = $(i + 1)
                    if ($i == "time")  time  = $(i + 1)
                    if ($i == "pv")    mv    = $(i + 1)
                }
                if (depth == "") next
                if (mv == want) {
                    if (stable == "") {
                        stable = depth; snodes = nodes; stime = time
                    }
                } else {
                    stable = ""
                }
                if (depth == ref && rtime == "") {
                    rnodes = nodes; rtime = time
                }
            }
            END {
                if (stable == "") { stable = "-"; stime = "-"; snodes = "-" }
                if (rtime == "") { rtime = "-"; rnodes = "-" }
                print stable, stime, snodes, rtime, rnodes
            }'
        )

        read -r sd st sn rt rn <<< "${stats}"
        printf '  run %s: stable d%-3s time %-8s nodes %-12s' \
            "${run}" "${sd}" "${st}" "${sn}"
        printf '| d%-3s time %-8s nodes %s\n' "${refdepth}" "${rt}" "${rn}"

        if [ "${sd}" != "-" ]; then
            found=$(( found + 1 ))
            stable_depths="${stable_depths} ${sd}"
            stable_times="${stable_times} ${st}"
            stable_nodes="${stable_nodes} ${sn}"
        fi
        if [ "${rt}" != "-" ]; then
            ref_times="${ref_times} ${rt}"
            ref_nodes="${ref_nodes} ${rn}"
        fi
    done

    printf '  median: stable d%s time %s nodes %s (found %s/%s)' \
        "$(printf '%s' "${stable_depths}" | median)" \
        "$(printf '%s' "${stable_times}" | median)" \
        "$(printf '%s' "${stable_nodes}" | median)" \
        "${found}" "${runs}"
    printf ' | d%s time %s nodes %s\n' \
        "${refdepth}" \
        "$(printf '%s' "${ref_times}" | median)" \
        "$(printf '%s' "${ref_nodes}" | median)"
done < "${suite}"
