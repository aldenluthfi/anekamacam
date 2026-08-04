#!/usr/bin/env bash
set -euo pipefail

# Builds selected Strength Iteration 3 phase binaries into bin/.
# Run from anywhere inside the repo.
#
# phaseA-3 is the pinned Stage A-3 branch, created at the harness commit.
# Later phases resolve their own branch names, auto-created from their parent
# phase when the branch does not yet exist. Current configs/ and res/dicts/
# are copied into every build worktree so all phase binaries expose identical
# protocol variants.
#
# Each PHASES row is "name ref parent". ref is the configured base commit-ish
# (normally the phase's own branch name). parent is the phase a missing
# branch is auto-created from ("-" means none). L-3, M-3 and R-3 are
# conditional on measurement and may never exist, so every phase takes a
# PHASE_<LETTER>_PARENT override that re-parents it onto whatever did land.
#
# Ladder order (iteration 3, renumbered 2026-08-04 when F-3 took the front):
# A-3..E-3 speed block, F-3 time management, G-3..K-3 drop block, L-3
# continuation-history density, M-3 grand diagnosis, N-3..P-3 eval block,
# Q-3 simplification, R-3 drop-variant NPS last of all.
#
# Usage:
#   build-stages.sh A-3
#   build-stages.sh B-3 C-3 D-3
#   PHASE_N_PARENT=phaseK-3 build-stages.sh phaseN-3

PHASES=(
	"phaseA-3  phaseA-3  -"
	"phaseB-3  phaseB-3  phaseA-3"
	"phaseC-3  phaseC-3  phaseB-3"
	"phaseD-3  phaseD-3  phaseC-3"
	"phaseE-3  phaseE-3  phaseD-3"
	"phaseF-3  phaseF-3  phaseE-3"
	"phaseG-3  phaseG-3  phaseF-3"
	"phaseH-3  phaseH-3  phaseG-3"
	"phaseI-3  phaseI-3  phaseH-3"
	"phaseJ-3  phaseJ-3  phaseI-3"
	"phaseK-3  phaseK-3  phaseJ-3"
	"phaseL-3  phaseL-3  phaseK-3"
	"phaseM-3  phaseM-3  phaseL-3"
	"phaseN-3  phaseN-3  phaseM-3"
	"phaseO-3  phaseO-3  phaseN-3"
	"phaseP-3  phaseP-3  phaseO-3"
	"phaseQ-3  phaseQ-3  phaseP-3"
	"phaseR-3  phaseR-3  phaseQ-3"
)

if [[ $# -eq 0 ]]; then
	echo "usage: build-stages.sh <phase> [...]" >&2
	exit 1
fi

REQUESTED=()
for requested in "$@"; do
	case "$requested" in
	phase*-3*) REQUESTED+=("$requested") ;;
	*-3*) REQUESTED+=("phase$requested") ;;
	*)
		echo "ERROR: invalid phase: $requested" >&2
		exit 1
		;;
	esac
done

is_requested() {
	local name=$1
	local requested

	for requested in "${REQUESTED[@]}"; do
		if [[ "$name" == "$requested" ]]; then
			return 0
		fi
	done

	return 1
}

# Configured base ref (second column) for a phase name.
phase_ref() {
	local want=$1 name ref parent

	for entry in "${PHASES[@]}"; do
		read -r name ref parent <<<"$entry"
		if [[ "$name" == "$want" ]]; then
			echo "$ref"
			return 0
		fi
	done

	return 1
}

# Parent phase (third column) for a phase name, honoring the phase's own
# PHASE_<LETTER>_PARENT override so a dropped conditional stage is skipped.
phase_parent() {
	local want=$1 name ref parent override

	for entry in "${PHASES[@]}"; do
		read -r name ref parent <<<"$entry"
		if [[ "$name" == "$want" ]]; then
			override="PHASE_${want:5:1}_PARENT"
			echo "${!override:-$parent}"
			return 0
		fi
	done

	return 1
}

# Commit-ish to build or branch from: the phase's own branch if it exists,
# otherwise its configured base ref.
resolve_commit() {
	local phase=$1 ref

	if git rev-parse --verify -q "$phase^{commit}" >/dev/null; then
		echo "$phase"
		return 0
	fi

	ref=$(phase_ref "$phase") || return 1
	echo "$ref"
}

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"
mkdir -p bin

BUILD_ROOT=$(mktemp -d "${TMPDIR:-/tmp}/anekamacam-phase-build.XXXXXX")
WT="$BUILD_ROOT/worktree"
export CARGO_TARGET_DIR="$BUILD_ROOT/target"

cleanup() {
	git worktree remove --force "$WT" 2>/dev/null || true
	rm -rf "$BUILD_ROOT"
}
trap cleanup EXIT

BUILT=()

for entry in "${PHASES[@]}"; do
	read -r name ref parent <<<"$entry"

	if ! is_requested "$name"; then
		continue
	fi

	if [[ "$parent" != "-" ]] \
	&& ! git rev-parse --verify -q "$name^{commit}" >/dev/null; then
		parent_name=$(phase_parent "$name")
		if ! base=$(resolve_commit "$parent_name"); then
			echo "ERROR: cannot resolve parent $parent_name for $name" >&2
			exit 1
		fi
		git branch "$name" "$base"
		echo "created $name from $parent_name ($base)"
	fi

	build_ref=$(resolve_commit "$name")

	if ! git rev-parse --verify -q "$build_ref^{commit}" >/dev/null; then
		echo "ERROR: missing git ref for $name: $build_ref" >&2
		exit 1
	fi

	echo "building $name ($build_ref)"
	git worktree remove --force "$WT" 2>/dev/null || true
	git worktree add --detach "$WT" "$build_ref" >/dev/null

	mkdir -p "$WT/res/dicts" "$WT/configs"
	cp res/dicts/* "$WT/res/dicts/"
	cp configs/* "$WT/configs/"

	(cd "$WT" && cargo build --release)
	cp "$CARGO_TARGET_DIR/release/anekamacam" "bin/$name"
	BUILT+=("bin/$name")
done

if [[ ${#BUILT[@]} -ne ${#REQUESTED[@]} ]]; then
	echo "ERROR: one or more requested phases were not defined" >&2
	exit 1
fi

echo "done:"
ls -l "${BUILT[@]}"

if cksum "${BUILT[@]}" | awk '{print $1}' | sort | uniq -d | grep -q .; then
	echo "ERROR: duplicate phase binaries detected" >&2
	cksum "${BUILT[@]}"
	exit 1
fi

if command -v md5 >/dev/null 2>&1; then
	md5 "${BUILT[@]}"
elif command -v md5sum >/dev/null 2>&1; then
	md5sum "${BUILT[@]}"
else
	cksum "${BUILT[@]}"
fi
