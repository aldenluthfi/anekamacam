#!/usr/bin/env bash
set -euo pipefail

# Builds all stage binaries into bin/. Run from anywhere inside the repo.
# The fix column is a comma-separated list of commits cherry-picked (in order)
# onto the stage before building. stageA/stageB predate the pv-walk fix
# (e9dc4f0) and get it cherry-picked, otherwise they crash under SPRT. Every
# stage before the ponder fix (c3e5e37, "emit ponder only when pv head matches
# best move") also gets it, so ponder replies stay correct in the round robin.
# stageR already contains c3e5e37, so it needs no fix.
# We copy updated dicts from res/dicts/ into each stage worktree before build,
# so we do not cherry-pick the fcabab9 commit (which has protocol code conflicts).

SPRT=e9dc4f0
PONDER=c3e5e37
UTIL=1f30a48

STAGES=(
	"stageA a67c0b0 $SPRT,$PONDER"
	"stageB 7692035 $SPRT,$PONDER"
	"stageC e9dc4f0 $PONDER"
	"stageD 5e0683e $PONDER"
	"stageE ad81ddf $PONDER"
	"stageF e91bdd9 $PONDER"
	"stageG 98e917c $PONDER"
	"stageH 0c2808c $PONDER"
	"stageI 4f12942 $PONDER"
	"stageJ 119af96 $PONDER"
	"stageK 44ab827 $PONDER"
	"stageL c485ee3 $PONDER"
	"stageM ed36592 $PONDER"
	"stageN ffe8a53 $PONDER"
	"stageO 96a4913 $PONDER"
	"stageP 9cf2175 $PONDER"
	"stageQ 8758909 $PONDER"
	"stageR f984db9 -"
	"stageS 41bd580 -"
	"stageT bdd2d5c -"
	"stageU 037f1a3 $UTIL"
	"stageV ba65b18 $UTIL"
)

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"
mkdir -p bin

export CARGO_TARGET_DIR="$ROOT/target-stages"

WT="$ROOT/.stage-build-wt"
trap 'git worktree remove --force "$WT" 2>/dev/null || true' EXIT

for entry in "${STAGES[@]}"; do

	read -r name commit pick <<<"$entry"

	if [[ ! "$@" =~ ${name: -1} ]]; then
		continue
	fi

	echo "building $name ($commit)"
	git worktree remove --force "$WT" 2>/dev/null || true
	git worktree add --detach "$WT" "$commit" >/dev/null
	if [ "$pick" != "-" ]; then
		IFS=',' read -ra picks <<<"$pick"
		for p in "${picks[@]}"; do
			if [ "$p" = "$UTIL" ]; then
				git -C "$WT" checkout "$p" -- src/game/util.rs src/main.rs
			else
				git -C "$WT" cherry-pick -n "$p"
			fi
		done
	fi
	mkdir -p "$WT/res/dicts"
	cp res/dicts/* "$WT/res/dicts/"
	(cd "$WT" && cargo build --release)
	cp "$CARGO_TARGET_DIR/release/anekamacam" "bin/$name"
done

echo "done:"
ls -l bin/stage?

if cksum bin/stage? | awk '{print $1}' | sort | uniq -d | grep -q .; then
	echo "ERROR: duplicate stage binaries detected" >&2
	cksum bin/stage?
	exit 1
fi

rm -r "$ROOT/target-stages"
cargo clean
