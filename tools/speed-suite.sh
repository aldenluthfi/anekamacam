#!/usr/bin/env bash
set -euo pipefail

# Speed suite over `debug-headless bench` for one or two engine binaries.
# Runs every variant PROCS times per binary with a pinned Zobrist seed,
# interleaving A/B passes so thermal drift cancels, then reports geometric
# mean nodes/time/nps per variant plus B-versus-A deltas.
#
# With a pinned seed and one thread, node counts are identical across passes
# of the same binary; the suite fails loudly when they are not.
#
# Usage:
#   tools/speed-suite.sh BIN_A [BIN_B]
#
# Env:
#   VARIANTS  space-separated variants
#             (default: "standard shogi crazyhouse xiangqi grand")
#   DEPTHS    per-variant "name=depth" overrides, space-separated
#             (defaults: standard=11 shogi=8 crazyhouse=8 xiangqi=9
#              grand=9; unlisted variants use 8)
#   PROCS     interleaved passes per binary per variant (default: 10)
#   LIMIT     bench positions per run (default: 16)
#   SEED      ANEKAMACAM_SEED value (default: 42)

if [[ $# -lt 1 || $# -gt 2 ]]; then
	echo "usage: speed-suite.sh BIN_A [BIN_B]" >&2
	exit 1
fi

BIN_A=$1
BIN_B=${2:-}
VARIANTS=${VARIANTS:-"standard shogi crazyhouse xiangqi grand"}
PROCS=${PROCS:-10}
LIMIT=${LIMIT:-16}
SEED=${SEED:-42}

for bin in "$BIN_A" ${BIN_B:+"$BIN_B"}; do
	if [[ ! -x "$bin" ]]; then
		echo "ERROR: not executable: $bin" >&2
		exit 1
	fi
done

# Bench depth for a variant: DEPTHS override first, then the default table.
depth_for() {
	local variant=$1 entry

	for entry in ${DEPTHS:-}; do
		if [[ "${entry%%=*}" == "$variant" ]]; then
			echo "${entry##*=}"
			return 0
		fi
	done

	case "$variant" in
	standard) echo 11 ;;
	xiangqi | grand) echo 9 ;;
	*) echo 8 ;;
	esac
}

# One bench run; emits only the machine-readable totals line.
bench_totals() {
	local bin=$1 variant=$2 depth=$3

	ANEKAMACAM_SEED=$SEED "$bin" debug-headless bench "$variant" \
		"$depth" --limit "$LIMIT" | awk '/^total /'
}

# Geometric-mean summary of one binary's totals lines.
# Totals fields: total nodes $3 time_ns $5 nps $7.
report() {
	local label=$1 file=$2

	awk -v label="$label" '
		{
			nodes += log($3); time += log($5); nps += log($7); n++
			if (first == "") first = $3
			else if ($3 != first) mismatch = 1
		}
		END {
			if (n == 0) exit 1
			printf "  %-8s nodes %12.0f  time_ms %9.1f  nps %9.0f\n", \
				label, exp(nodes / n), exp(time / n) / 1e6, \
				exp(nps / n)
			if (mismatch) {
				printf "ERROR: node counts differ across %s passes\n", \
					label > "/dev/stderr"
				exit 1
			}
		}
	' "$file"
}

TMP=$(mktemp -d "${TMPDIR:-/tmp}/anekamacam-speed-suite.XXXXXX")
trap 'rm -rf "$TMP"' EXIT

for variant in $VARIANTS; do
	depth=$(depth_for "$variant")
	echo "=== $variant depth $depth limit $LIMIT seed $SEED ==="
	: >"$TMP/a"
	: >"$TMP/b"

	for ((pass = 1; pass <= PROCS; pass++)); do
		bench_totals "$BIN_A" "$variant" "$depth" >>"$TMP/a"
		if [[ -n "$BIN_B" ]]; then
			bench_totals "$BIN_B" "$variant" "$depth" >>"$TMP/b"
		fi
	done

	report A "$TMP/a"
	if [[ -n "$BIN_B" ]]; then
		report B "$TMP/b"
		paste "$TMP/a" "$TMP/b" | awk '
			{
				ta += log($5); tb += log($12)
				pa += log($7); pb += log($14); n++
			}
			END {
				if (n == 0) exit 1
				printf "  %-8s time %+7.2f%%  nps %+7.2f%%\n", \
					"B vs A", \
					(exp(tb / n) / exp(ta / n) - 1) * 100, \
					(exp(pb / n) / exp(pa / n) - 1) * 100
			}
		'
	fi
done
