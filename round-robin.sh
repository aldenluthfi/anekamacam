#!/usr/bin/env bash
set -euo pipefail

# Round robin over all stage binaries (stageA..stageV) plus two
# fairy-stockfish anchors, run for fide and shogi. Engines vary games on
# their own (per-process Zobrist seeding), so no opening book is used.
# No draw/resign adjudication: the engine's eval units are not
# centipawns, so score thresholds would misfire. Stages before N load
# params from disk, so each engine gets an isolated dir pre-seeded with
# the params its own code derives; stageN+ use params embedded at build
# time. Games per pair = 2 * ROUNDS.
#
# Detaches from the shell so it survives SSH logout: the first invocation
# re-execs itself under `setsid nohup`, streams all output to a log, and
# returns immediately with the pid. cutechess-cli reprints a full rank
# table every -ratinginterval games, so the log is the live scoreboard.
#
# Usage:
#   round-robin.sh <stage> [stage...]   # launch detached, print log path
#   round-robin.sh --status             # dump the latest rank table
#   round-robin.sh --stop               # kill a running detached run
#
# Env: ROUNDS (256), CONCURRENCY (4), RR (/tmp/rr) output dir.

REPO=$(cd "$(dirname "$0")" && pwd)
RR=${RR:-/tmp/rr}
ROUNDS=${ROUNDS:-256}
CONCURRENCY=${CONCURRENCY:-4}

LOG="$RR/round-robin.log"
PIDFILE="$RR/round-robin.pid"

mkdir -p "$RR"

# --status: print the most recent complete rank table from the log. Each
# table starts with a "Rank Name ..." header; awk keeps only the last
# block so a scrolling log collapses to the current standings.
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
	awk '/^Rank +Name/ {buf=""} {buf=buf $0 "\n"} END {printf "%s", buf}' \
		"$LOG"
	exit 0
fi

# --stop: kill the detached process group started below.
if [[ "${1:-}" == "--stop" ]]; then
	if [[ -f "$PIDFILE" ]] && kill -0 "$(cat "$PIDFILE")" 2>/dev/null; then
		kill -- "-$(cat "$PIDFILE")" 2>/dev/null || kill "$(cat "$PIDFILE")"
		echo "stopped pid $(cat "$PIDFILE")"
		rm -f "$PIDFILE"
	else
		echo "nothing running"
	fi
	exit 0
fi

STAGES=("$@")

if [[ ${#STAGES[@]} -eq 0 ]]; then
	echo "usage: round-robin.sh <stage> [stage...] | --status | --stop" >&2
	exit 1
fi

# First entry: re-exec detached. setsid puts the run in its own session
# (new process group == pid), so --stop can signal the whole tree, and
# nohup + redirected stdin/stdout survive the shell closing.
if [[ "${RR_DETACHED:-}" != "1" ]]; then
	RR_DETACHED=1 setsid nohup "$0" "$@" >"$LOG" 2>&1 </dev/null &
	pid=$!
	echo "$pid" >"$PIDFILE"
	echo "round-robin detached (pid $pid)"
	echo "  log:      $LOG"
	echo "  live:     tail -f $LOG"
	echo "  standings: $0 --status"
	echo "  stop:     $0 --stop"
	echo "  pgn:      $RR/rr-fide.pgn, $RR/rr-shogi.pgn"
	exit 0
fi

for s in "${STAGES[@]}"; do
	mkdir -p "$RR/stage$s"
done

run_rr() {
	local v=$1
	shift
	local engines=()

	for s in "${STAGES[@]}"; do
		engines+=(-engine "name=stage$s" "cmd=$REPO/bin/stage$s"
			"dir=$RR/stage$s" arg=uci "option.UCI_Variant=$v")
	done

	for elo in 1700 1800; do
		engines+=(-engine "name=fsf-$elo" cmd=fairy-stockfish
			"option.UCI_Variant=$v" option.UCI_LimitStrength=true
			"option.UCI_Elo=$elo")
	done

	cutechess-cli \
		"${engines[@]}" \
		-each proto=uci option.Threads=1 tc=10+0.1 timemargin=200 \
		-tournament round-robin -rounds "$ROUNDS" -games 2 \
		-recover \
		-concurrency "$CONCURRENCY" -ratinginterval 50 \
		-pgnout "$RR/rr-$v.pgn" \
		"$@"
}

run_rr fide
run_rr shogi -variant shogi
