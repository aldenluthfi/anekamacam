#!/usr/bin/env bash
set -euo pipefail

# Builds selected Strength Iteration 2 phase binaries into bin/.
# Run from anywhere inside the repo.
#
# phaseA-2 is exact Stage AC commit 773d04e. phaseB-2 through phaseJ-2
# resolve temporary experiment branch names, auto-created from their parent
# phase when the branch does not yet exist. Current configs/ and res/dicts/ are
# copied into every build worktree so all phase binaries expose identical
# protocol variants.
#
# Each PHASES row is "name ref parent". ref is the configured base commit-ish
# (for phaseA-2, commit 773d04e; for experiment phases, their own branch name).
# parent is the phase a missing branch is auto-created from ("-" means none).
# Phase H-2's parent defaults to phaseF-2; override with PHASE_H_PARENT once the
# F-2 vs G-2 comparison picks a winner.
#
# Usage:
#   build-stages.sh A-2
#   build-stages.sh B-2 C-2 D-2
#   build-stages.sh phaseA-2 phaseJ-2

PHASES=(
	"phaseA-2 773d04e  -"
	"phaseB-2 phaseB-2 phaseA-2"
	"phaseC-2 phaseC-2 phaseB-2"
	"phaseD-2 phaseD-2 phaseB-2"
	"phaseE-2 phaseE-2 phaseB-2"
	"phaseF-2 phaseF-2 phaseA-2"
	"phaseG-2 phaseG-2 phaseF-2"
	"phaseH-2 phaseH-2 phaseF-2"
	"phaseI-2 phaseI-2 phaseA-2"
	"phaseJ-2 phaseJ-2 phaseI-2"
)

if [[ $# -eq 0 ]]; then
	echo "usage: build-stages.sh <phase> [...]" >&2
	exit 1
fi

REQUESTED=()
for requested in "$@"; do
	case "$requested" in
	phase*-2) REQUESTED+=("$requested") ;;
	*-2) REQUESTED+=("phase$requested") ;;
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

# Parent phase (third column) for a phase name, honoring PHASE_H_PARENT.
phase_parent() {
	local want=$1 name ref parent

	for entry in "${PHASES[@]}"; do
		read -r name ref parent <<<"$entry"
		if [[ "$name" == "$want" ]]; then
			if [[ "$want" == "phaseH-2" ]]; then
				echo "${PHASE_H_PARENT:-$parent}"
			else
				echo "$parent"
			fi
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
