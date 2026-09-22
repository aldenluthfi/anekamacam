#!/usr/bin/env bash
set -euo pipefail

# Builds Strength Iteration 4 ladder binaries into bin/.
# Run from anywhere inside the repo.
#
# The ladder is derived, not listed: base-4 is the prerequisite-complete
# baseline that RR #0 anchors, phaseA-4 branches from it, and every later
# letter branches from the one before it. A phase builds from its own branch
# when that branch exists and is auto-created from its parent when it does
# not. Any letter can be re-parented with PHASE_<LETTER>_PARENT, which is how
# a rejected phase is skipped without renaming everything after it.
#
# Current res/config/ and res/dicts/ are copied into every build worktree, so all
# ladder binaries expose identical protocol variants no matter which commit
# they are built from. That makes the working tree part of what a binary is,
# so each build writes a bin/<name>.provenance record naming its commit and
# hashing those resources. Verify a binary with
# `tools/provenance.sh verify bin/<name>`.
#
# Usage:
#   build-stages.sh base-4
#   build-stages.sh A-4
#   build-stages.sh B-4 C-4 D-4
#   PHASE_C_PARENT=phaseA-4 build-stages.sh C-4
#
# Env:
#   LADDER_BASE  commit-ish base-4 is built from (default: current branch)

BASE_NAME="base-4"
LETTERS=({A..Z})

if [[ $# -eq 0 ]]; then
	echo "usage: build-stages.sh <phase> [...]" >&2
	exit 1
fi

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"

LADDER_BASE=${LADDER_BASE:-$(git rev-parse --abbrev-ref HEAD)}

NAMES=("$BASE_NAME")
for letter in "${LETTERS[@]}"; do
	NAMES+=("phase$letter-4")
done

REQUESTED=()
for requested in "$@"; do
	case "$requested" in
	base | base-4) REQUESTED+=("$BASE_NAME") ;;
	phase?-4) REQUESTED+=("$requested") ;;
	?-4) REQUESTED+=("phase$requested") ;;
	*)
		echo "ERROR: invalid phase: $requested" >&2
		exit 1
		;;
	esac
done

for requested in "${REQUESTED[@]}"; do
	if ! printf '%s\n' "${NAMES[@]}" | grep -qx "$requested"; then
		echo "ERROR: not a ladder binary: $requested" >&2
		exit 1
	fi
done

is_requested() {
	local name=$1 requested

	for requested in "${REQUESTED[@]}"; do
		if [[ "$name" == "$requested" ]]; then
			return 0
		fi
	done

	return 1
}

# Configured base ref for a phase: the baseline follows LADDER_BASE, every
# letter owns a branch named after itself.
phase_ref() {
	local want=$1

	if [[ "$want" == "$BASE_NAME" ]]; then
		echo "$LADDER_BASE"
	else
		echo "$want"
	fi
}

# Phase a missing branch is created from: the letter before it, or the
# baseline for the first letter, honoring a PHASE_<LETTER>_PARENT override so
# a dropped phase is skipped.
phase_parent() {
	local want=$1 letter override index

	if [[ "$want" == "$BASE_NAME" ]]; then
		echo "-"
		return 0
	fi

	letter=${want:5:1}
	override="PHASE_${letter}_PARENT"

	if [[ -n "${!override:-}" ]]; then
		echo "${!override}"
		return 0
	fi

	if [[ "$letter" == "A" ]]; then
		echo "$BASE_NAME"
		return 0
	fi

	for index in "${!LETTERS[@]}"; do
		if [[ "${LETTERS[index]}" == "$letter" ]]; then
			echo "phase${LETTERS[index - 1]}-4"
			return 0
		fi
	done

	echo "ERROR: unknown phase letter: $letter" >&2
	return 1
}

# Commit-ish to build or branch from: the phase's own branch if it exists,
# otherwise its configured base ref.
resolve_commit() {
	local phase=$1

	if [[ "$phase" != "$BASE_NAME" ]] &&
		git rev-parse --verify -q "$phase^{commit}" >/dev/null; then
		echo "$phase"
		return 0
	fi

	phase_ref "$phase"
}

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

for name in "${NAMES[@]}"; do
	if ! is_requested "$name"; then
		continue
	fi

	if [[ "$name" != "$BASE_NAME" ]] &&
		! git rev-parse --verify -q "$name^{commit}" >/dev/null; then
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

	mkdir -p "$WT/res/dicts" "$WT/res/config"
	cp res/dicts/* "$WT/res/dicts/"
	cp res/config/* "$WT/res/config/"

	(cd "$WT" && cargo build --release)
	cp "$CARGO_TARGET_DIR/release/anekamacam" "bin/$name"
	tools/provenance.sh record "bin/$name" "$build_ref"
	BUILT+=("bin/$name")
done

echo "done:"
ls -l "${BUILT[@]}"

if cksum "${BUILT[@]}" | awk '{print $1}' | sort | uniq -d | grep -q .; then
	echo "ERROR: duplicate phase binaries detected" >&2
	cksum "${BUILT[@]}"
	exit 1
fi
