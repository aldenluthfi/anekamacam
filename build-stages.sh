#!/usr/bin/env bash
set -euo pipefail

# Builds all stage binaries into bin/. Run from anywhere inside the repo.
# The fix column is a comma-separated list of commits cherry-picked (in order)
# onto the stage before building. stageA/stageB predate the pv-walk fix
# (ac510c0) and get it cherry-picked, otherwise they crash under SPRT. Every
# stage before the ponder fix (992bc95, "emit ponder only when pv head matches
# best move") also gets it, so ponder replies stay correct in the round robin.
# stageR already contains 992bc95, so it needs no fix.
# We copy updated dicts from res/dicts/ into each stage worktree before build,
# so we do not cherry-pick the fcabab9 commit (which has protocol code conflicts).

SPRT=ac510c0
PONDER=992bc95
UTIL=1f30a48

STAGES=(
	"stageA 5b8357d $SPRT,$PONDER"
	"stageB 6a5ab5e $SPRT,$PONDER"
	"stageC ac510c0 $PONDER"
	"stageD 21999d9 $PONDER"
	"stageE 96d4360 $PONDER"
	"stageF d8c333f $PONDER"
	"stageG 4d6e0e4 $PONDER"
	"stageH c86dd76 $PONDER"
	"stageI 5bd2ace $PONDER"
	"stageJ 0af3dd6 $PONDER"
	"stageK 9c7177d $PONDER"
	"stageL dce81bd $PONDER"
	"stageM 8ec807f $PONDER"
	"stageN f20be48 $PONDER"
	"stageO 960cd9b $PONDER"
	"stageP da65b17 $PONDER"
	"stageQ 2372ed7 $PONDER"
	"stageR 6fa29fc -"
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
