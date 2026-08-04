#!/usr/bin/env bash
set -euo pipefail

# Effective-branching-factor suite for one or two engine binaries, plus
# fairy-stockfish when it is on PATH.
#
# Reads tools/ebf_positions.txt (see its header for the format), drives every
# case over UCI with an explicit Hash and Threads 1, and parses the
# per-iteration `nodes` of the `info` lines. Reports cumulative nodes at the
# case depth and the geometric-mean iteration ratio nodes[d]/nodes[d-1] taken
# from EBF_FROM upward, which is the intra-engine branching number the search
# stages after F-3 are gated on. Every engine gets the same Hash: a 1 MB
# against 16 MB mismatch inflated an earlier table by up to 2x.
#
# The engine's stdin is held open until `bestmove` arrives. A case fed through
# a pipe that closes sends EOF mid-search, the engine reads it as `quit`, and
# the search is truncated -- that is how a first grand measurement came back
# as depth 2.
#
# Node counts are deterministic for one binary at one Hash, one thread and a
# pinned Zobrist seed, so a case is run once per engine; wall-clock time is not
# measured here and the suite is safe to run alongside other work. The seed
# pin is load-bearing rather than tidy: unseeded, two binaries that search
# identically came back up to 2x apart on the same case, which is larger than
# any gate this suite carries.
#
# Usage:
#   tools/ebf-suite.sh BIN_A [BIN_B]
#
# Env:
#   POSITIONS  case file (default tools/ebf_positions.txt)
#   HASH       Hash MB given to every engine (default 64)
#   EBF_FROM   first depth counted into the ratio (default 6)
#   FSF        fairy-stockfish binary, "" to drop the column
#              (default: fairy-stockfish from PATH)
#   FILTER     run only cases whose variant matches this word
#   SEED       ANEKAMACAM_SEED value for our binaries (default 42)
#   CASE_TIMEOUT  seconds one case may take before it is failed (default 900)

if [[ $# -lt 1 || $# -gt 2 ]]; then
	echo "usage: ebf-suite.sh BIN_A [BIN_B]" >&2
	exit 1
fi

ROOT=$(cd "$(dirname "$0")/.." && pwd)
BIN_A=$1
BIN_B=${2:-}
POSITIONS=${POSITIONS:-"$ROOT/tools/ebf_positions.txt"}
HASH=${HASH:-64}
EBF_FROM=${EBF_FROM:-6}
FILTER=${FILTER:-}
SEED=${SEED:-42}
CASE_TIMEOUT=${CASE_TIMEOUT:-900}

if [[ -z ${FSF+x} ]]; then
	FSF=$(command -v fairy-stockfish || true)
fi

for bin in "$BIN_A" ${BIN_B:+"$BIN_B"}; do
	if [[ ! -x "$bin" ]]; then
		echo "ERROR: not executable: $bin" >&2
		exit 1
	fi
done

# Engines run in a scratch directory so each resolves res/param against its
# own exports or embedded defaults instead of the repository's files, which
# is what makes two phase binaries comparable. Their paths must therefore be
# absolute before the run changes directory.
BIN_A=$(cd "$(dirname "$BIN_A")" && pwd)/$(basename "$BIN_A")
if [[ -n "$BIN_B" ]]; then
	BIN_B=$(cd "$(dirname "$BIN_B")" && pwd)/$(basename "$BIN_B")
fi
if [[ -n "$FSF" ]]; then
	FSF=$(cd "$(dirname "$FSF")" && pwd)/$(basename "$FSF")
fi

if [[ ! -r "$POSITIONS" ]]; then
	echo "ERROR: unreadable case file: $POSITIONS" >&2
	exit 1
fi

TMP=$(mktemp -d "${TMPDIR:-/tmp}/anekamacam-ebf-suite.XXXXXX")
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/logs"

# fairy-stockfish spells three of our variants differently and has no name
# for the rest, so a case it cannot play drops out of its column.
fsf_variant() {
	case "$1" in
	standard) echo "chess" ;;
	crazyhouse | shogi | xiangqi | grand) echo "$1" ;;
	*) echo "" ;;
	esac
}

# Runs one case to its depth and leaves the engine's whole output in $5.
# stdin stays open until `bestmove` lands or CASE_TIMEOUT expires.
drive() {
	local bin=$1 variant=$2 command=$3 depth=$4 out=$5
	local fifo="$TMP/fifo" waited=0 pid

	rm -f "$fifo"
	mkfifo "$fifo"
	: >"$out"

	(cd "$TMP" && ANEKAMACAM_SEED=$SEED "$bin" <"$fifo" >"$out" 2>/dev/null) &
	pid=$!

	exec 3>"$fifo"
	printf 'uci\n' >&3
	printf 'setoption name UCI_Variant value %s\n' "$variant" >&3
	printf 'setoption name Threads value 1\n' >&3
	printf 'setoption name Hash value %s\n' "$HASH" >&3
	printf '%s\n' "$command" >&3
	printf 'go depth %s\n' "$depth" >&3

	while ! grep -q '^bestmove' "$out" 2>/dev/null; do
		if ! kill -0 "$pid" 2>/dev/null; then
			break
		fi
		if ((waited > CASE_TIMEOUT * 20)); then
			kill "$pid" 2>/dev/null || true
			break
		fi
		waited=$((waited + 1))
		sleep 0.05
	done

	printf 'quit\n' >&3
	exec 3>&-
	wait "$pid" 2>/dev/null || true
}

# Cumulative nodes at the case depth and the geometric-mean iteration ratio,
# printed as "nodes ebf". Takes the last info line carrying each depth, so a
# bound line or a re-search never counts as its own iteration.
summarise() {
	local out=$1 depth=$2

	awk -v target="$depth" -v from="$EBF_FROM" '
		{
			depth = 0; nodes = 0
			for (field = 1; field < NF; field++) {
				if ($field == "depth") depth = $(field + 1) + 0
				else if ($field == "nodes") nodes = $(field + 1) + 0
			}
			if (depth > 0 && nodes > 0) seen[depth] = nodes
		}
		END {
			if (!(target in seen)) { print "0 0"; exit }
			for (depth = from; depth <= target; depth++) {
				if (!(depth in seen) || !((depth - 1) in seen)) continue
				if (seen[depth - 1] <= 0) continue
				ratio += log(seen[depth] / seen[depth - 1]); steps++
			}
			printf "%d %.3f\n", seen[target], \
				(steps > 0 ? exp(ratio / steps) : 0)
		}
	' "$out"
}

RESULTS="$TMP/results"
: >"$RESULTS"

printf '# hash %s MB, ebf from depth %s%s\n' "$HASH" "$EBF_FROM" \
	"${FSF:+, fsf $FSF}"

while IFS='|' read -r variant label command depth; do
	case "$variant" in
	'' | \#*) continue ;;
	esac

	variant=$(printf '%s' "$variant" | tr -d '[:space:]')
	label=$(printf '%s' "$label" | tr -d '[:space:]')
	depth=$(printf '%s' "$depth" | tr -d '[:space:]')
	command=$(printf '%s' "$command" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')

	if [[ -n "$FILTER" && "$variant" != "$FILTER" ]]; then
		continue
	fi

	drive "$BIN_A" "$variant" "$command" "$depth" "$TMP/a"
	read -r nodes_a ebf_a <<<"$(summarise "$TMP/a" "$depth")"

	line="case=$variant/$label depth=$depth a_nodes=$nodes_a a_ebf=$ebf_a"

	if [[ -n "$BIN_B" ]]; then
		drive "$BIN_B" "$variant" "$command" "$depth" "$TMP/b"
		read -r nodes_b ebf_b <<<"$(summarise "$TMP/b" "$depth")"
		line="$line b_nodes=$nodes_b b_ebf=$ebf_b"
		if ((nodes_a > 0 && nodes_b > 0)); then
			line="$line b_over_a=$(awk -v b="$nodes_b" -v a="$nodes_a" \
				'BEGIN { printf "%.3f", b / a }')"
		fi
	fi

	fsf_name=$(fsf_variant "$variant")

	if [[ -n "$FSF" && -n "$fsf_name" ]]; then
		drive "$FSF" "$fsf_name" "$command" "$depth" "$TMP/f"
		read -r nodes_f ebf_f <<<"$(summarise "$TMP/f" "$depth")"
		line="$line fsf_nodes=$nodes_f fsf_ebf=$ebf_f"
		if ((nodes_a > 0 && nodes_f > 0)); then
			line="$line a_over_fsf=$(awk -v a="$nodes_a" -v f="$nodes_f" \
				'BEGIN { printf "%.2f", a / f }')"
		fi
	fi

	echo "$line"
	echo "$variant $label $depth $nodes_a $ebf_a" >>"$RESULTS"
done <"$POSITIONS"

echo "--- per-variant geometric means (spread is min..max over cases) ---"

awk '
	{
		variant = $1; depth = $3; nodes = $4; ebf = $5
		key = variant "@" depth
		count[key]++
		nodes_log[key] += log(nodes > 0 ? nodes : 1)
		ebf_log[key] += log(ebf > 0 ? ebf : 1)
		if (!(key in low) || nodes < low[key]) low[key] = nodes
		if (!(key in high) || nodes > high[key]) high[key] = nodes
		if (!(key in ebf_low) || ebf < ebf_low[key]) ebf_low[key] = ebf
		if (!(key in ebf_high) || ebf > ebf_high[key]) ebf_high[key] = ebf
	}
	END {
		for (key in count) {
			printf "%-22s cases %2d  nodes %12.0f (%d..%d)  ebf %5.3f (%.3f..%.3f)\n", \
				key, count[key], exp(nodes_log[key] / count[key]), \
				low[key], high[key], exp(ebf_log[key] / count[key]), \
				ebf_low[key], ebf_high[key]
		}
	}
' "$RESULTS" | sort

awk '
	{
		if ($1 == "crazyhouse" && $2 != "startpos") {
			house_log += log($4); house++; house_depth = $3
		}
		if ($1 == "standard") {
			base_log += log($4); base++; base_depth = $3
		}
	}
	END {
		if (house == 0 || base == 0) exit
		if (house_depth != base_depth) {
			printf "drop ratio skipped: crazyhouse depth %d, standard depth %d\n", \
				house_depth, base_depth
			exit
		}
		printf "crazyhouse-mid / standard nodes at depth %d: %.2fx\n", \
			house_depth, exp(house_log / house) / exp(base_log / base)
	}
' "$RESULTS"
