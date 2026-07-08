#!/usr/bin/env bash
set -euo pipefail

# Builds all stage binaries into bin/. Run from anywhere inside the repo.
# stageA/stageB predate the pv-walk fix (ac510c0) and get it cherry-picked,
# otherwise they crash under SPRT.

FIX=ac510c0

STAGES=(
  "stageA 5b8357d $FIX"
  "stageB 6a5ab5e $FIX"
  "stageC ac510c0 -"
  "stageD 21999d9 -"
  "stageE 96d4360 -"
  "stageF d8c333f -"
  "stageG 4d6e0e4 -"
  "stageH c86dd76 -"
  "stageI 5bd2ace -"
  "stageJ 0af3dd6 -"
)

ROOT=$(git rev-parse --show-toplevel)
cd "$ROOT"
mkdir -p bin

export CARGO_TARGET_DIR="$ROOT/target-stages"

WT="$ROOT/.stage-build-wt"
trap 'git worktree remove --force "$WT" 2>/dev/null || true' EXIT

for entry in "${STAGES[@]}"; do
  read -r name commit pick <<< "$entry"
  echo "building $name ($commit)"
  git worktree remove --force "$WT" 2>/dev/null || true
  git worktree add --detach "$WT" "$commit" >/dev/null
  if [ "$pick" != "-" ]; then
    git -C "$WT" cherry-pick -n "$pick"
  fi
  (cd "$WT" && cargo build --release)
  cp "$CARGO_TARGET_DIR/release/anekamacam" "bin/$name"
done

echo "done:"
ls -l bin/stage?
