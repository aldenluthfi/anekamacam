# Search Core Only — Search Purge Experiment

## Context

Plan 14 reduced evaluation to phase-tapered material and piece-square tables.
Search still combined many pruning, reduction, ordering, cache, and time-control
techniques, so its node count and strength were no easier to attribute than the
old evaluator.

Plan 15 creates the matching search baseline. It starts from plan 14 commit
`94b0759` and keeps only iterative deepening, plain negamax alpha-beta,
capture-only quiescence, null-move pruning (NMP), late-move pruning (LMP), two
killer moves per ply, one butterfly history table, SEE move ordering, and the
existing main/quiescence transposition tables.

The branch is `experiment/search-core-only`. Removal began immediately: no
baseline build, benchmark, or test was run before editing. No subagents and no
`cargo fmt` were used.

## Implementation

### 1. Plain search core

`src/game/position/search.rs` now performs one full-window alpha-beta search per
iterative-deepening depth. Every legal child receives the current full
`[-beta, -alpha]` window. Quiescence retains stand pat, capture-only search, and
full evasions while in check.

Removed from the driver and node search:

- aspiration windows;
- principal variation search and every zero-window re-search;
- check extensions;
- mate-distance pruning;
- reverse futility pruning;
- razoring;
- ProbCut;
- futility pruning;
- internal iterative reduction;
- late-move reduction and all reduction tables/adjustments;
- improving-state tracking;
- SEE pruning and delta pruning;
- TT static-eval reuse and bound-refined pruning eval;
- staged capture/quiet move generation;
- best-move stability and score-drop time scaling.

Rule truth remains untouched: eager termination, on-demand repetition and
perpetual outcomes, checkmate/stalemate detection, illegal mating-drop handling,
make/undo verification, and maximum-ply protection all remain.

### 2. NMP and LMP

NMP has no stored or derived per-variant parameters. It requires a non-check,
non-root, non-endgame node above depth 2, at least one live big piece for side to
move, and a static evaluation at or above beta. Reduction is simply
`min(depth, 4 + depth / 4)`. Eval-surplus scaling and deep-endgame verification
were removed together with `nmp_min_material` and `nmp_eval_div`.

LMP has no constant table. After one legal move, a quiet non-promotion, non-drop
move is skipped once `legal_moves >= 3 + depth * depth`. Root, check, capture,
promotion, drop, and mate-score positions remain exempt.

### 3. Killer and history

Two killer moves per ply remain. Continuation history is deleted; ordering and
updates use one butterfly table keyed by piece, origin, and destination.

History gravity, `HIST_BONUS_TABLE`, and `HIST_BONUS_SCALE` are gone. Reward and
malus are literal `depth * depth`, added directly and clamped to half the `i16`
range. Existing update placement remains: reward quiet cutoffs and alpha
improvements, penalize tried non-best quiets.

### 4. SEE move ordering without SearchBufs

SEE remains for capture ordering and both debug commands. It is not used as a
pruning rule. Ordering priority remains table move, winning SEE capture,
killers, butterfly history, then losing SEE capture.

`SearchBufs` is deleted end to end. Alpha-beta and quiescence own small local
move, score, and move-generation vectors. `see!` owns its local LVA and capture
scratch. All buffer construction, arguments, exports, worker plumbing, protocol
plumbing, debug plumbing, and utility plumbing were removed.

`generate_all_quiets_and_drops` is also deleted. Main search always calls
`generate_all_moves_and_drops` once; quiescence calls `generate_all_captures`
unless it must generate every check evasion.

### 5. State and parameter derivation

`StaticState` loses every search field: futility, reverse-futility, SEE, delta,
ProbCut, aspiration, razor, four LMR tables, and both NMP fields. Their defaults,
LMR construction, and derivation are deleted.

Dynamic `State` loses `cont_hist` and `static_eval`. It retains only PV storage,
butterfly history, and killers as search-owned state.

`derive_search_parameters` is deleted. Config loading and tuner export no longer
call it; `derive_parameters` now derives evaluation parameters and refreshes the
material/PST state only.

### 6. Transposition tables

`TTable` and `QTable` remain, including parity/seqlock safety, age-based
replacement, bound/depth cutoffs, mate-score ply normalization, table moves, PV
extension, counters, and shared lazy-SMP ownership.

The main-table static-eval payload, encoders, decoders, probe outputs, and store
arguments are removed. Freed bits remain unused. Table default sizes now call
`with_mb` directly; `T_TABLE_SIZE`, `Q_TABLE_SIZE`, and the three hash-ratio
constants are gone. Hash allocation uses a direct two-thirds/one-third split.

### 7. One-deadline time management

`SearchInfo` now carries one `deadline`. Per-node interrupt polling enforces it;
iterative deepening has no second soft-stop policy.

Protocol time allocation now produces one budget. `movetime` subtracts overhead.
Clock searches divide usable time by `movestogo`, or 20 when absent, add the
increment, and clamp to the remaining clock. Ponder restart stores and reapplies
one budget. Soft/hard deadlines, stability arrays, score-drop scaling, reserve
shares, horizon constants, and hard-budget factors are gone.

### 8. Constant and support sweep

Search-local tables and tuning constants were replaced by direct expressions.
Removed constants include the history bonus table/scale, all pruning and LMR
limits, NMP/LMP tables, time modifiers, repetition scan constant, table-size
constants, and hash-ratio constants. Core representation constants such as
`MAX_DEPTH`, PV stride, bound flags, result codes, and move tags remain.

Debug SEE commands remain in graphical and headless tools. README search
features now describe plain alpha-beta, T/Q tables, SEE ordering, NMP/LMP,
killers, and history.

## Result

Before adding this record:

- `src/`: 16 files, 416 insertions, 1,841 deletions;
- README: 39 insertions, 16 deletions;
- total: 17 files, 455 insertions, 1,857 deletions.

Largest source cuts are `search.rs` (-1,011), `transposition.rs` (-200),
`move_ordering.rs` (-187), `parameters.rs` (-82), `state.rs` (-71), and
`prelude.rs` (-69).

Post-purge release binary:

- size: 5,432,464 bytes;
- MD5: `8ea7df8db711efb47b452b02201c3c05`.

## Verification

All verification occurred after the purge.

1. `cargo check --workspace --all-targets` passed in the user session.
2. Debug and release builds passed.
3. Clippy reports two inherited warnings (`type_complexity` in
   `termination.rs`, `collapsible_if` in `move_parse.rs`) and no new warning.
4. Symbol sweep finds no live `SearchBufs`, staged quiet generator,
   continuation history, static-eval cache, search derivation, removed pruning
   fields, LMR infrastructure, history-gravity/table constants, dynamic time
   modifiers, or removed table-size/hash-ratio constants.
5. Debug depth-3 search passes for all 38 embedded variants, exercising
   `verify_game_state` at every visited node.
6. All 14 shipped perft suites pass in the release binary at depth 4 and limit
   30: standard 120/120, xiangqi 44/44, janggi 28/28, shogi 16/16, sittuyin
   16/16, and 4/4 for the other nine suites.
7. Endgame fixtures pass 38/38. FEN round trips pass 44/44 with 0 failed and
   0 skipped.
8. Headless SEE returns `SEE e4d5: 104` on the README fixture.
9. Release two-thread search returns a legal standard PV and best move.
10. A 50 ms timed headless game and UCI `go movetime 50` both stop and return a
    legal best move; UCI returned `bestmove g1f3 ponder b8c6`.
11. `git diff --check` passes. Changed core source has no non-column-comment
    line over 80 characters.

### Not run

- No pre-purge benchmark was preserved.
- No speed comparison, agreement suite, or strength match was run. Those are
  experiment measurements, not implementation correctness checks.
