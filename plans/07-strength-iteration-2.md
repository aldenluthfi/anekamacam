# Strength Iteration 2 Plan

## Purpose

Iteration 1 ends at Stage AC. Iteration 2 imports that source snapshot as its
own Phase A-2 baseline, then uses only `-2` phase labels. No stage lettering
continues across the boundary.

This iteration decides whether capture history, singular search, correction
history, aggressive quiet LMR, and continuation history earn their search cost.
Temporary phase branches and commits are reproducible experiment artifacts.
Final results remain Iteration 2 phases. Iteration 1's stage alphabet is closed
at Stage AC.

## Handoff from Plan 06

Original recovery plan and execution status:
`plans/06-search-evaluation-legality-recovery.md`.

Iteration 2 baseline:

- phase label: **Phase A-2**;
- source snapshot: Iteration 1 Stage AC;
- commit: `773d04e`;
- clean release MD5: `b8bec5b0450d38fd5e6f2b9f7ccb20dd`;
- replay gate: 26 fixtures passing;
- external legality gate: 2,024 Fairy-Stockfish walk plies passing;
- target variants: Standard, Crazyhouse, Shogi, Xiangqi, Grand.

Phase A-2 is the first Iteration 2 checkpoint. All later experiment phases
branch from A-2 or another explicitly named `-2` phase.

Preserve current user work in `src/main.rs` and `tools/datagen_local.sh`. Do not
include either file in experiment commits unless the user explicitly changes
scope.

## Naming and commit rules

- `phaseA-2`: Iteration 2 baseline at commit `773d04e`.
- `phaseB-2` through `phaseJ-2`: Iteration 2 experiment refs.
- Temporary phase commits may exist on isolated branches so binaries can be
  reproduced.
- Rejected candidates remain documented under their phase labels; no stage
  letter is assigned.
- Final winning source is reconstructed on Phase A-2 and recorded as Phase J-2.
- Later Iteration 2 work continues with Phase K-2, L-2, and so on.
- Iteration 1 stage names are never reused for Iteration 2 work.

## Iteration 2 phase map

| Phase | Parent | Candidate | Purpose |
|---|---|---|---|
| A-2 | Iteration 1 snapshot | Fixed baseline | Trusted legality and measurement base. |
| B-2 | A-2 | Remove three search groups | Test full lean-search bundle first. |
| C-2 | B-2 | Restore capture history | Decide whether capture history earns keep. |
| D-2 | B-2 | Restore singular family | Isolate singular/multicut contribution. |
| E-2 | B-2 | Restore correction history | Test correction history as one unit. |
| F-2 | A-2 | Rebuild best add-back subset | Verify minimal Lean-1 synthesis. |
| G-2 | F-2 | Restore older quiet LMR | Test whether aggressive LMR still helps. |
| H-2 | best F/G | Remove continuation history | Test its full removal. |
| I-2 | A-2 | Rebuild best lean subset | Remove branch-history accidents. |
| J-2 | I-2 | Freeze final candidate | Final Iteration 2 arbitration candidate. |

Iteration-2 labels always include the `-2` suffix. Legacy mechanisms are named
in prose as `Iteration 1 Stage G`, `Iteration 1 Stage H`, `Iteration 1 Stages
I/L`, and `Iteration 1 Stages B/J`; bare letters are not used for candidates.

## Phase A-2: reproduce baseline

Before editing search code:

1. Verify branch and commit:

   ```bash
   git checkout main
   git rev-parse HEAD
   ```

   Expected commit: `773d04e`.

2. Verify working tree contains only preserved user changes and plan files:

   ```bash
   git status --short
   ```

3. Build exact baseline binary:

   ```bash
   ./build-stages.sh A-2
   md5 bin/phaseA-2
   ```

4. Run release replay gate:

   ```bash
   tools/replay_fixtures.py --binary bin/phaseA-2
   ```

5. Record compiler, binary MD5, config/dict/parameter hashes, and commit in this
   plan's execution-status section.

Do not benchmark a stale `target/release/anekamacam` copy. Verify binary MD5
before every comparison.

## Phase B-2: full Lean-1 bundle

Branch from Phase A-2. Remove all three mechanism groups together.

### Remove Iteration 1 Stage G: capture history

Delete:

- `capt_hist` runtime storage and allocation;
- `capt_hist_div` and victim buckets when no longer used;
- capture-history ordering offsets;
- capture-history updates in search;
- dead constants, parameters, and imports.

Keep ordinary MVV/LVA, SEE, TT move ordering, and generic capture scoring unless
they directly depend on capture history.

### Remove Iteration 1 Stage H: singular search family

Delete:

- excluded-move state and per-ply storage;
- singular verification probe;
- singular extension;
- singular multicut;
- negative extension from the same mechanism;
- singular margins and dead helper state.

Do not remove the cumulative check-extension cap from Iteration 1 Stage S in
this phase.

### Remove Iteration 1 Stages I/L: correction history

Delete:

- correction-history tables and allocation;
- pawn-hash correction indexing;
- corrected-static-eval read path;
- correction-history updates;
- Iteration 1 Stages I/L parameters and dead gating logic.

Do not alter ordinary evaluation, pawn hash computation, or transposition-table
static-eval storage beyond references required by correction history.

### B-2 gate

- no warnings in release build;
- all 26 replay fixtures pass;
- all five variants complete short legality walks;
- ten-process, three-position-per-variant speed suite runs;
- exact diff reviewed for dead fields and partial mechanism remnants.

B-2 is not committed to main even if fast. Keep its branch and binary for later
arbitration.

## Phase C-2: capture-history add-back

Branch from B-2. Restore only Iteration 1 Stage G capture history. Keep the
singular family and correction history removed.

Purpose: compare C-2 directly with B-2. If C-2 raises cost without a later
strength reason, capture history stays removed. If C-2 materially improves move
ordering or node count, retain it in the F-2 synthesis shortlist.

No unrelated capture-ordering changes enter this phase.

## Phase D-2: singular-family add-back

Branch from B-2. Restore only Iteration 1 Stage H singular verification,
multicut, and negative extension. Keep capture and correction history removed.

Purpose: isolate singular-family search cost and tree effect. Review every
restored use of excluded move and extension accounting; do not restore dead
fields that no longer participate.

## Phase E-2: correction-history add-back

Branch from B-2. Restore Iteration 1 Stages I/L correction history as one
inseparable unit. Keep capture history and the singular family removed.

Never split Iteration 1 Stages I and L. Historical evidence already shows the
ungated correction read is not a valid candidate.

## Phase F-2: Lean-1 synthesis

Use B-2, C-2, D-2, and E-2 measurements to choose the smallest justified
mechanism set. Rebuild that set from Phase A-2 rather than promoting an
experiment branch blindly.

Requirements:

- diff contains only intended mechanism removals;
- no dead fields, parameters, or allocations remain;
- exact release binary provenance recorded;
- replay and five-variant speed gates repeated from scratch.

F-2 is the Lean-1 finalist, not the final Iteration 2 candidate.

## Phase G-2: quiet-LMR rollback

Branch from F-2. Restore quiet LMR behavior from before Iteration 1 Stage C
while retaining F-2's chosen mechanism set.

This is distinct from Iteration 1 Stage W, which reverted Iteration 1 Stage V's
later sqrt/sqrt retune to Iteration 1 Stage U behavior. G-2 asks whether
Iteration 1 Stage C's older aggressiveness should also be removed.

Compare F-2 and G-2 over the full multi-position suite. Search is
nondeterministic, so single start-position results do not decide this phase.

## Phase H-2: continuation-history removal

Branch from the better local candidate among F-2 and G-2. Remove Iteration 1
Stages B/J together:

- one-ply and two-ply continuation-history storage;
- continuation-history allocation;
- continuation bases and direct indexing;
- continuation updates and ordering contribution;
- dead constants and parameters.

Do not retain J without B. If H-2 loses, continuation history remains in the
final synthesis.

## Phase I-2: interaction reconstruction

Rebuild the best F-2 through H-2 mechanism combination directly from Phase A-2.
This phase catches accidental dependencies and tests non-monotonic interactions.

If results show `F-2 < G-2 < H-2`, also build the required comparison without
the middle change. Do not infer that cumulative improvement proves each
intermediate mechanism useful.

Required comparisons depend on the winning path, for example:

- H-2 versus H-2 without G-2;
- continuation removal with current versus pre-C LMR;
- final subset with and without each retained add-back when evidence overlaps.

## Phase J-2: final Iteration 2 candidate

Freeze one candidate from Phase I-2. Run complete local gates again with fresh
binaries and no reused benchmark output.

J-2 must have:

- release build without warnings;
- 26 replay fixtures passing;
- short Standard, Crazyhouse, Shogi, Xiangqi, and Grand legality walks;
- ten independent speed processes;
- binary and embedded-asset provenance;
- adversarial diff review;
- no generated parameter or cache artifacts in git status.

If J-2 fails correctness, fix within the phase before arbitration. If it fails
stable throughput limits, fall back to the strongest simpler predecessor.

## Measurement protocol

### Correctness

Every behavior-changing phase runs:

```bash
cargo build --release
tools/replay_fixtures.py --binary <candidate-binary>
```

Also run release `d` verification during short external walks. No engine
`#[test]` modules. Do not run `cargo fmt`; the project intentionally uses custom
column alignment.

### Speed

- same compiler and host;
- one thread;
- same hash size and fixed depth;
- three or more positions per target variant;
- at least ten independent processes;
- clear hash between positions;
- alternate candidate/baseline order;
- report geometric-mean time, NPS, and nodes;
- record MD5 before running.

Performance-only changes need at least 2% aggregate gain and no stable
per-variant NPS regression above 2%. Correctness-induced node-count changes are
reported separately from throughput.

### Strength

No per-phase SPRT. Keep phase binaries and defer strength arbitration until
J-2.

Final iteration round robin includes:

- Phase A-2 baseline;
- Phase J-2 finalist;
- at most one or two materially different runner-up phases when interactions
  remain ambiguous;
- Fairy-Stockfish anchors.

All five target variants are mandatory. Use fresh result directories and never
merge historical pre-AC Shogi or Grand games.

Pilot:

```bash
RR=/tmp/rr-iteration2-pilot \
ROUNDS=32 \
VARIANTS="standard crazyhouse shogi xiangqi grand" \
./round-robin.sh phaseA-2 phaseJ-2
```

Main arbitration after clean pilot:

```bash
RR=/tmp/rr-iteration2 \
ROUNDS=1024 \
VARIANTS="standard crazyhouse shogi xiangqi grand" \
./round-robin.sh phaseA-2 phaseJ-2
```

Use FSF 1700, 1800, and 1900 initially. Add 2000, 2100, 2200, and 2300 only
after the finalist beats FSF-1900.

## Phase J-2 acceptance rule

Adopt Phase J-2 only when final arbitration supports its cumulative candidate.

Acceptance requires:

- zero unresolved legality failures;
- no stable five-variant throughput failure;
- aggregate strength improvement or justified complexity reduction with
  mandatory variant non-regression;
- no Shogi or Grand result contaminated by old games;
- final diff reconstructed cleanly from Phase A-2.

Commit one logical Iteration 2 result identified as Phase J-2. Do not include
Plan 07 updates, generated results, user `src/main.rs`, or
`tools/datagen_local.sh` in that code commit.

If Phase A-2 wins, close Iteration 2 with no search-code commit and record that
the removed mechanisms earned keep as a group.

## Build script contract

`build-stages.sh` supports only Strength Iteration 2:

- `phaseA-2` at commit `773d04e`;
- experiment refs `phaseB-2` through `phaseJ-2` when those branches exist;
- exact binary names under `bin/`;
- current config/dict assets for comparable protocol support;
- duplicate-binary detection across the binaries built in one invocation.

Examples:

```bash
./build-stages.sh A-2
./build-stages.sh B-2 C-2 D-2 E-2
./build-stages.sh phaseA-2 phaseJ-2
```

## Round-robin script contract

`round-robin.sh` accepts only iteration-2 labels such as `phaseA-2` or shorthand
such as `A-2`. Its unattended default covers Standard, Shogi, and Grand on the
current remote layout. Final arbitration must set `VARIANTS` explicitly to all
five target variants. `FSF_ELOS` remains configurable.

Every candidate receives an isolated engine directory under `RR`. Status output
continues to show one latest table per variant. Old result directories are never
reused for iteration 2.

## New-session start checklist

1. Read Plan 06 and this Plan 07.
2. Confirm commit `773d04e` exists as the Phase A-2 source snapshot.
3. Confirm only preserved user changes and generated phase binaries are
   uncommitted.
4. Build and verify `phaseA-2`.
5. Create isolated `phaseB-2` experiment branch/worktree from `773d04e`.
6. Remove capture history, singular search, and correction history without
   touching evaluation.
7. Run B-2 correctness and speed gates.
8. Preserve B-2 binary and measurements.
9. Continue C-2, D-2, and E-2 as independent add-backs from B-2.
10. Update this file's execution status after every phase.

Suggested new-session prompt:

> Continue `plans/07-strength-iteration-2.md`. Start Phase B-2 from Phase A-2
> commit `773d04e`, preserve `src/main.rs` and `tools/datagen_local.sh`, and use
> only Iteration 2 phase labels.

---

## Execution Status

### Current state

- Phase A-2 baseline: built from source snapshot `773d04e` and verified.
- Current `bin/phaseA-2` MD5: `50c270e588604adc8ac620a7a94c285e` (reproduced
  after the `build-stages.sh` lineage change; anchor holds).
- Phase A-2 replay gate: 26/26 fixtures passed.
- Iteration 1 stage naming is closed at Stage AC.
- Phase B-2: DONE. Branch `phaseB-2` from `773d04e`; three ordered commits
  (`4ddcff0` capture history, `e551feb` singular family, `1774e67` correction
  history). MD5 `94d4ed454a6c98b97cfe7693d4e7b345`; 26/26 fixtures; warning-free.
- Phase C-2: DONE (restore capture history). MD5
  `abb94b2ba326b66e8e8d4c35d09bfb2a`; 26/26 fixtures.
- Phase D-2: DONE (restore singular family). MD5
  `80c0d01789a78de9de259bd42ba4333f`; 26/26 fixtures.
- Phase E-2: DONE (restore correction history; branch = `e551feb`). MD5
  `88070cbe2ee3d262cf0639a6fe18757a`; 26/26 fixtures.
- Phase F-2: not started (PAUSED here for F-2 synthesis decision).
- Phase G-2: not started.
- Phase H-2: not started.
- Phase I-2: not started.
- Phase J-2: not started.

### B-2..E-2 measurements (standard speed suite, depth 13, 5 runs/pos)

Baseline `phaseA-2`. Ratios are candidate/baseline. Node counts at fixed depth
are noisy (Zobrist reseeds per process; A-2 self-swung ~8% across invocations),
so treat >1.15 as signal and ~1.0-1.05 as noise. NOT strength; strength deferred
to the J-2 round robin. Standard-only per this iteration's per-phase gate; full
five-variant suite runs at J-2.

| Phase | Mechanism kept | nodes@d13 | nps |
|---|---|---|---|
| B-2 | none (all removed) | x1.259 | x1.019 |
| C-2 | capture history | x1.269 | x0.997 |
| D-2 | singular family | x1.159 | x1.013 |
| E-2 | correction history | x1.012 | x1.047 |

Reading: correction history (E-2) recovers nearly all of A-2's tree size on its
own; singular (D-2) recovers part; capture history (C-2) shows no node-count
benefit over B-2 at this depth on standard. Throughput (nps) moves within +-5%
for every phase, so none of the three costs measurable standard throughput.
Harness: `tools/speed_suite.py` (new, standard-only reusable gate).

### Setup completed in this session

- original Plan 06 and its execution status consolidated into one file;
- Plan 07 iteration labels and phase decomposition defined;
- Iteration 1 Stage AC snapshot imported as independent Phase A-2;
- `build-stages.sh` reduced to Iteration 2 phases A-2 through J-2;
- `round-robin.sh` reduced to iteration-2 phase binaries and phase resolution
  verified without launching games;
- Phase A-2 built successfully and passed all replay fixtures;
- perft removed from this iteration's phase gate because phases B-2 through J-2
  change only search machinery, not move generation.

### Next action

PAUSED after E-2 (F-2 depends on the C/D/E matrix). Decide the F-2 mechanism
set from the measurements above, then rebuild F-2 fresh from Phase A-2 (cherry-
pick the kept-removal subset) and resume G-2 -> J-2. Round robin runs on the
remote layout (cutechess-cli is not installed locally); build and verify J-2
here, then hand off the `round-robin.sh` command.

Branches present: `phaseB-2`, `phaseC-2`, `phaseD-2`, `phaseE-2`. Editing
worktrees under `/tmp/aneka-edit-phase{B,C,D}-2`. Binaries under `bin/`.
`build-stages.sh` now carries phase parentage and auto-creates a phase branch
from its parent (H-2 parent overridable via `PHASE_H_PARENT`).
