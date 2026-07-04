#!/usr/bin/env bash
#
# run_probes.sh -- torture-test the UCI time-control and lifecycle paths.
# Each probe drives one pathological `go` over stdin and asserts that a
# single well-formed bestmove arrives within its latency bound (never
# "null", never only after quit). Guards the fixes for zero/negative
# budgets, tiny movetime, and stop-right-after-go.
#
# Usage: tools/run_probes.sh [engine]

set -u

engine="${1:-./target/release/anekamacam}"

now_ms() {
    python3 -c 'import time; print(int(time.time() * 1000))'
}

pass=0
total=0

# probe <name> <max_ms> <commands...>: sends each argument as one line,
# waits, then quits; asserts one bestmove != null within max_ms.
probe() {
    name="$1"
    max_ms="$2"
    shift 2

    total=$(( total + 1 ))
    start_ms=$( now_ms )

    result=$( { for cmd in "$@"; do printf '%s\n' "${cmd}"; done
                sleep 3; printf 'quit\n'; } | "${engine}" 2>/dev/null \
        | while IFS= read -r line; do
            case "${line}" in
                (bestmove*)
                    printf '%s %s\n' "$( now_ms )" "${line}"
                    ;;
            esac
        done )

    count=$( printf '%s\n' "${result}" | grep -c bestmove )
    stamp=$( printf '%s\n' "${result}" | head -1 | awk '{ print $1 }' )
    move=$( printf '%s\n' "${result}" | head -1 | awk '{ print $3 }' )
    elapsed=$(( ${stamp:-0} - start_ms ))

    if [ "${count}" = "1" ] && [ -n "${move}" ] \
    && [ "${move}" != "null" ] && [ "${elapsed}" -le "${max_ms}" ]
    then
        pass=$(( pass + 1 ))
        printf 'PASS  %-18s %5dms  bestmove %s\n' \
            "${name}" "${elapsed}" "${move}"
    else
        printf 'FAIL  %-18s %5dms  bestmoves=%s move=%s\n' \
            "${name}" "${elapsed}" "${count}" "${move:-none}"
    fi
}

probe "clock 40ms"      500 \
    "position startpos" "go wtime 40 btime 40 winc 100 binc 100"
probe "clock negative"  500 \
    "position startpos" "go wtime -100 btime 5000"
probe "clock zero"      500 \
    "position startpos" "go wtime 0 btime 0"
probe "movetime 20ms"   500 \
    "position startpos" "go movetime 20"
probe "stop after go"   500 \
    "position startpos" "go wtime 60000 btime 60000" "stop"
probe "nodes 50000"    2500 \
    "position startpos" "go nodes 50000"
probe "depth 6"        2500 \
    "position startpos" "go depth 6"

printf '\n%d/%d probes passed\n' "${pass}" "${total}"
[ "${pass}" = "${total}" ]
