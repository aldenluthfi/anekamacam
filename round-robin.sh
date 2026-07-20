#!/usr/bin/env bash
set -euo pipefail

# Round robin over all stage binaries (stageA..stageV) plus three
# fairy-stockfish anchors, run once per variant. Engines vary games on
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
	# Each run_rr prints a "=== variant <ak> (<cc>) ===" marker, and
	# cutechess reprints a "Rank Name ..." table every -ratinginterval
	# games. Keep the latest table seen under each variant marker and
	# print them all, so finished variants stay visible instead of being
	# overwritten by whichever variant is running now.
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
	echo "  pgn:      $RR/rr-<variant>.pgn (one per variant)"
	exit 0
fi

for s in "${STAGES[@]}"; do
	mkdir -p "$RR/stage$s"
done

# run_rr <variant>
#
# The engine's config stems now match the fairy-stockfish/cutechess
# spelling (standard, minishogi, minixiangqi), so one variant name serves
# everyone: cutechess as the referee (-variant) and the stage binaries
# plus the fsf anchors via UCI_Variant.
run_rr() {
	local variant=$1
	local engines=()

	# Marker the --status parser keys on to label each variant's table.
	echo "=== variant $variant ==="

	for s in "${STAGES[@]}"; do
		engines+=(-engine "name=stage$s" "cmd=$REPO/bin/stage$s"
			"dir=$RR/stage$s" arg=uci)
	done

	for elo in 1700 1800 1900; do
		engines+=(-engine "name=fsf-$elo" cmd=fairy-stockfish
			option.UCI_LimitStrength=true "option.UCI_Elo=$elo")
	done

	cutechess-cli \
		"${engines[@]}" \
		-each proto=uci option.Threads=1 option.Hash=64 tc=10+0.1 timemargin=200 \
		-tournament round-robin -rounds "$ROUNDS" -games 2 \
		-recover \
		-concurrency "$CONCURRENCY" -ratinginterval 50 \
		-pgnout "$RR/rr-$variant.pgn" \
		-variant "$variant"
}

# Every variant both engines share. berolina is anekamacam-only (fairy
# has no berolina), so it is dropped: cutechess refs through the fairy
# variant set and could not adjudicate it.
run_rr standard
run_rr grand
run_rr shogi
run_rr minishogi
run_rr xiangqi
run_rr minixiangqi

# fairy ∩ cutechess variants shipped as configs (see configs/). Two
# caveats before trusting their scores:
#   1. Only stages built from a commit that embeds these configs (current
#      HEAD or later) can play them; historical stageA..V forfeit.
#   2. The referee adjudicates with fairy's rules, so a config whose rules
#      do not yet match fairy exactly (the `// TODO verify` moves and the
#      non-8x8 castling geometry) forfeits on the first divergent move.
# Smoke-test with ROUNDS=1 first and trim this list to what checks out.
run_rr amazon
run_rr almost
run_rr newzealand
run_rr hoppelpoppel
run_rr pocketknight
run_rr knightmate
run_rr chigorin
run_rr chancellor
run_rr modern
run_rr janus
run_rr embassy
run_rr gothic
run_rr asean
run_rr ai-wok
run_rr judkins
run_rr euroshogi
