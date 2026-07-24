#!/usr/bin/env bash
set -euo pipefail

# Strength Iteration 2 round robin over selected phase binaries plus
# configurable Fairy-Stockfish anchors. Defaults to Standard, Shogi, and Grand.
# Engines vary games through per-process Zobrist seeding, so no opening book is
# used.
#
# No draw/resign adjudication: AnekaMacam evaluation units are not centipawns,
# so score thresholds would misfire. Every engine receives an isolated working
# directory. Use a fresh RR directory for every experiment campaign.
#
# Detaches from the shell so it survives SSH logout. The first invocation
# re-execs itself under setsid/nohup, streams output to a log, and returns its
# pid. cutechess-cli reprints a full rank table every rating interval.
#
# Usage:
#   round-robin.sh phaseA-2 phaseJ-2
#   round-robin.sh A-2 J-2
#   round-robin.sh --status
#   round-robin.sh --stop
#
# Env:
#   RR             output directory (default: ~/rr)
#   ROUNDS         round-robin rounds (default: 1024)
#   CONCURRENCY    concurrent games (default: CPU count)
#   VARIANTS       space-separated variants
#   FSF_ELOS       space-separated Fairy-Stockfish anchors
#   ALLOW_EXISTING_RR=1 permits launch into an existing result directory

REPO=$(cd "$(dirname "$0")" && pwd)
RR=${RR:-~/rr}
ROUNDS=${ROUNDS:-1024}
CONCURRENCY=${CONCURRENCY:-$(
	nproc 2>/dev/null || sysctl -n hw.ncpu 2>/dev/null
)}
VARIANTS=${VARIANTS:-"standard shogi grand"}
FSF_ELOS=${FSF_ELOS:-"1700 1800 1900"}

LOG="$RR/round-robin.log"
PIDFILE="$RR/round-robin.pid"

mkdir -p "$RR"

if [[ "${1:-}" == "--status" ]]; then
	if [[ ! -f "$LOG" ]]; then
		echo "no log at $LOG"
		exit 1
	fi
	if [[ -f "$PIDFILE" ]] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
		echo "status: running (pid $(cat "$PIDFILE"))"
	else
		echo "status: not running"
	fi

	awk '
		/^=== variant / {
			cur = $3
			if (!(cur in seen)) { seen[cur] = 1; order[++n] = cur }
			next
		}
		/^Rank +Name/ { inblk = 1; blk[cur] = $0 "\n"; next }
		inblk && /^[[:space:]]*$/ { inblk = 0; next }
		inblk { blk[cur] = blk[cur] $0 "\n" }
		END {
			for (i = 1; i <= n; i++)
				if (order[i] in blk)
					printf "--- %s ---\n%s\n", order[i], blk[order[i]]
		}
	' "$LOG"
	exit 0
fi

if [[ "${1:-}" == "--stop" ]]; then
	if [[ -f "$PIDFILE" ]] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
		kill -- "-$(cat "$PIDFILE")" 2>/dev/null ||
			kill "$(cat "$PIDFILE")"
		echo "stopped pid $(cat "$PIDFILE")"
		rm -f "$PIDFILE"
	else
		echo "nothing running"
	fi
	exit 0
fi

if [[ $# -eq 0 ]]; then
	echo "usage: round-robin.sh <iteration-2-phase> [...]" >&2
	exit 1
fi

CANDIDATES=()
for requested in "$@"; do
	case "$requested" in
	phase*-2) candidate=$requested ;;
	*-2) candidate="phase$requested" ;;
	*)
		echo "ERROR: invalid iteration-2 phase: $requested" >&2
		exit 1
		;;
	esac

	if [[ ! -x "$REPO/bin/$candidate" ]]; then
		echo "ERROR: missing executable bin/$candidate" >&2
		exit 1
	fi
	CANDIDATES+=("$candidate")
done

if [[ "${RR_DETACHED:-}" != "1" ]]; then
	if [[ "${ALLOW_EXISTING_RR:-0}" != "1" ]]; then
		if [[ -f "$LOG" ]] ||
			compgen -G "$RR/rr-*.pgn" >/dev/null; then
			echo "ERROR: RR directory already contains results: $RR" >&2
			echo "use a fresh RR path or set ALLOW_EXISTING_RR=1" >&2
			exit 1
		fi
	fi

	RR_DETACHED=1 setsid nohup "$0" "${CANDIDATES[@]}" \
		>"$LOG" 2>&1 </dev/null &
	pid=$!
	echo "$pid" >"$PIDFILE"
	echo "round-robin detached (pid $pid)"
	echo "  log:       $LOG"
	echo "  live:      tail -f $LOG"
	echo "  standings: $0 --status"
	echo "  stop:      $0 --stop"
	echo "  pgn:       $RR/rr-<variant>.pgn"
	exit 0
fi

for candidate in "${CANDIDATES[@]}"; do
	mkdir -p "$RR/$candidate"
done

read -ra ANCHORS <<<"$FSF_ELOS"

run_rr() {
	local variant=$1
	local engines=()
	local candidate
	local elo

	echo "=== variant $variant ==="

	for candidate in "${CANDIDATES[@]}"; do
		engines+=(
			-engine "name=$candidate" "cmd=$REPO/bin/$candidate"
			"dir=$RR/$candidate" arg=uci
		)
	done

	for elo in "${ANCHORS[@]}"; do
		engines+=(
			-engine "name=fsf-$elo" cmd=fairy-stockfish
			option.UCI_LimitStrength=true "option.UCI_Elo=$elo"
		)
	done

	cutechess-cli \
		"${engines[@]}" \
		-each proto=uci option.Threads=1 option.Hash=64 \
		tc=30+0.3 timemargin=200 \
		-tournament round-robin -rounds "$ROUNDS" -games 2 \
		-recover \
		-concurrency "$CONCURRENCY" -ratinginterval 50 \
		-pgnout "$RR/rr-$variant.pgn" \
		-variant "$variant"
}

read -ra TARGET_VARIANTS <<<"$VARIANTS"
for variant in "${TARGET_VARIANTS[@]}"; do
	run_rr "$variant"
done
