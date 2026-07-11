#!/usr/bin/env bash
set -euo pipefail

# Round robin over all stage binaries (stageA..stageQ) plus two
# fairy-stockfish anchors, run for fide and shogi. Engines vary games on
# their own (per-process Zobrist seeding), so no opening book is used.
# No draw/resign adjudication: the engine's eval units are not
# centipawns, so score thresholds would misfire. Stages before N load
# params from disk, so each engine gets an isolated dir pre-seeded with
# the params its own code derives; stageN+ use params embedded at build
# time. Games per pair = 2 * ROUNDS.

REPO=$(cd "$(dirname "$0")" && pwd)
RR=/tmp/rr
ROUNDS=${ROUNDS:-256}
CONCURRENCY=${CONCURRENCY:-4}

STAGES=(A B C D E F G H I J K L M N O P Q)
PRE_N=(A B C D E F G H I J K L M)

for s in "${STAGES[@]}"; do
	mkdir -p "$RR/stage$s"
done

for s in "${PRE_N[@]}"; do
	(cd "$RR/stage$s" && printf '%s\n' \
		'uci' \
		'setoption name UCI_Variant value fide' \
		'isready' \
		'setoption name UCI_Variant value shogi' \
		'isready' \
		'quit' |
		"$REPO/bin/stage$s" uci >/dev/null)
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
