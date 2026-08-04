#!/usr/bin/env bash
set -euo pipefail

# Evaluation-agreement suite: does this engine agree with a reference engine
# about which side is better, on positions neither was tuned on?
#
# Reads tools/agree_positions.txt (see its header for the format), searches each
# case to a fixed depth in both engines with a matched Hash, and reports per
# variant the median score gap, the number of sign flips (one engine says one
# side is winning, the other says the opposite), and the number of cases where
# the reference sees a lost position and we do not.
#
# The sign count is the gate. Centipawns are not a shared unit across engines,
# and two attempts at a unit multiplier for this engine (1.98, then 0.74) were
# both wrong, so a threshold stated in centipawns of the other engine's scale is
# not measurable. Which side is winning is measurable, and a variant that flips
# sign against a stronger reference is a variant whose evaluation is wrong.
#
# The standard cases are the control: this engine agrees with the reference in
# standard while disagreeing in crazyhouse, and it is that difference, not
# either number alone, that says the defect belongs to the drop rules.
#
# Usage:
#   tools/agree-suite.sh BIN [REFERENCE]
#
# Env:
#   POSITIONS  case file (default tools/agree_positions.txt)
#   HASH       Hash MB given to both engines (default 64)
#   DEPTH      override the per-case depth
#   SEED       ANEKAMACAM_SEED for our binary (default 42)
#   FILTER     run only cases whose variant matches this word

if [[ $# -lt 1 || $# -gt 2 ]]; then
	echo "usage: agree-suite.sh BIN [REFERENCE]" >&2
	exit 1
fi

ROOT=$(cd "$(dirname "$0")/.." && pwd)
BIN=$1
REFERENCE=${2:-$(command -v fairy-stockfish || true)}
POSITIONS=${POSITIONS:-"$ROOT/tools/agree_positions.txt"}
HASH=${HASH:-64}
DEPTH=${DEPTH:-}
SEED=${SEED:-42}
FILTER=${FILTER:-}

if [[ ! -x "$BIN" ]]; then
	echo "ERROR: not executable: $BIN" >&2
	exit 1
fi

if [[ -z "$REFERENCE" || ! -x "$REFERENCE" ]]; then
	echo "ERROR: no reference engine; pass one or install fairy-stockfish" >&2
	exit 1
fi

BIN=$(cd "$(dirname "$BIN")" && pwd)/$(basename "$BIN")
REFERENCE=$(cd "$(dirname "$REFERENCE")" && pwd)/$(basename "$REFERENCE")

TMP=$(mktemp -d "${TMPDIR:-/tmp}/anekamacam-agree-suite.XXXXXX")
trap 'rm -rf "$TMP"' EXIT
mkdir -p "$TMP/logs"

# The reference spells three of our variants differently and has no name for
# the rest, so a case it cannot play drops out.
reference_variant() {
	case "$1" in
	standard) echo "chess" ;;
	crazyhouse | shogi | xiangqi | grand) echo "$1" ;;
	*) echo "" ;;
	esac
}

# Runs one case and prints the last reported score as "kind value". stdin is
# held open until bestmove: a closing pipe reads as quit and truncates.
drive() {
	local bin=$1 variant=$2 command=$3 depth=$4 seeded=$5
	local fifo="$TMP/fifo" out="$TMP/out" waited=0 pid

	rm -f "$fifo"
	mkfifo "$fifo"
	: >"$out"

	if [[ "$seeded" == "yes" ]]; then
		(cd "$TMP" && ANEKAMACAM_SEED=$SEED "$bin" <"$fifo" >"$out" 2>/dev/null) &
	else
		(cd "$TMP" && "$bin" <"$fifo" >"$out" 2>/dev/null) &
	fi
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
		if ((waited > 12000)); then
			kill "$pid" 2>/dev/null || true
			break
		fi
		waited=$((waited + 1))
		sleep 0.05
	done

	printf 'quit\n' >&3
	exec 3>&-
	wait "$pid" 2>/dev/null || true

	awk '/ score /{ line = $0 } END {
		if (line == "") { print "none 0"; exit }
		split(line, parts, " score ")
		split(parts[2], fields, " ")
		print fields[1], fields[2]
	}' "$out"
}

# A mate score becomes a large centipawn number so one comparison covers both.
# The magnitude is meaningless; only its sign and hugeness are used.
as_centipawns() {
	awk -v kind="$1" -v value="$2" 'BEGIN {
		if (kind == "mate") {
			printf "%d", (value > 0 ? 100000 - value * 100 : -100000 - value * 100)
		} else {
			printf "%d", value
		}
	}'
}

RESULTS="$TMP/results"
: >"$RESULTS"

printf '# hash %s MB, reference %s\n' "$HASH" "$REFERENCE"

while IFS='|' read -r variant label command depth; do
	case "$variant" in
	'' | \#*) continue ;;
	esac

	variant=$(printf '%s' "$variant" | tr -d '[:space:]')
	label=$(printf '%s' "$label" | tr -d '[:space:]')
	depth=$(printf '%s' "$depth" | tr -d '[:space:]')
	command=$(printf '%s' "$command" | sed 's/^[[:space:]]*//;s/[[:space:]]*$//')
	depth=${DEPTH:-$depth}

	if [[ -n "$FILTER" && "$variant" != "$FILTER" ]]; then
		continue
	fi

	reference_name=$(reference_variant "$variant")
	if [[ -z "$reference_name" ]]; then
		continue
	fi

	read -r our_kind our_value <<<"$(drive "$BIN" "$variant" "$command" "$depth" yes)"
	read -r ref_kind ref_value <<<"$(
		drive "$REFERENCE" "$reference_name" "$command" "$depth" no)"

	ours=$(as_centipawns "$our_kind" "$our_value")
	reference=$(as_centipawns "$ref_kind" "$ref_value")

	printf 'case=%s/%s depth=%s ours=%s reference=%s gap=%s\n' \
		"$variant" "$label" "$depth" "$ours" "$reference" \
		"$((ours - reference))"
	echo "$variant $ours $reference" >>"$RESULTS"
done <"$POSITIONS"

echo "--- per-variant agreement ---"

awk '
	{
		variant = $1; ours = $2; reference = $3
		count[variant]++
		gaps[variant "," count[variant]] = ours - reference
		if ((ours > 50 && reference < -50) || (ours < -50 && reference > 50)) {
			flips[variant]++
		}
		if (reference < -300 && ours > -300) blind[variant]++
		total[variant] += ours - reference
	}
	END {
		for (variant in count) {
			n = count[variant]
			for (i = 1; i <= n; i++) values[i] = gaps[variant "," i]
			for (i = 1; i <= n; i++) {
				for (j = i + 1; j <= n; j++) {
					if (values[j] < values[i]) {
						swap = values[i]; values[i] = values[j]; values[j] = swap
					}
				}
			}
			median = (n % 2) ? values[(n + 1) / 2] \
				: (values[n / 2] + values[n / 2 + 1]) / 2
			printf "%-12s cases %2d  median gap %+7d  sign flips %2d  reference-sees-lost-we-do-not %2d\n", \
				variant, n, median, flips[variant] + 0, blind[variant] + 0
		}
	}
' "$RESULTS" | sort
