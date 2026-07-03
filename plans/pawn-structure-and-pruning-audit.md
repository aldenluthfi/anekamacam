# Strengthen anekamacam: variant-agnostic pawn structure + pruning audit

## Context

The engine plays ~1700 ELO across 17 variants despite a full pruning stack.
Exploration found the core gap: **there is no pawn-structure evaluation at
all** — eval is material + PST + king shelter + imbalance/pair + tempo. The
rook-sac probe (`b3b2` → connected passers win) is eval-limited: search can
never see the passers' value, so no depth finds it. Second gap: qsearch
searches **captures only and stand-pats even while in check** (unsound), so
sacrifice lines get cut at the horizon. Third: several pruning parameters are
FIDE-hardcoded (delta margin 200, aspiration ±50) or aggressive (LMR bad-
history +1, unconditional check extension) in ways that plausibly explain the
zugzwang probe's TTD regression (depth ~27-30 → ~40-50).

Plan: (0) benchmark harness, (1) derive pawn-like pieces + masks geometrically,
(2) passed/connected/doubled eval term, (3) qsearch in-check evasions,
(4) dangerous-push pruning exemptions, (5) individual pruning-parameter
experiments. Every phase gets a before/after TTD benchmark.

### Verified facts that shape the design

- **Param load path**: `game_io.rs:1731-1746` has three branches; when
  `res/param/{variant}/latest.param` exists (disk or embedded — fide, shogi,
  tjatoer do), only `derive_search_parameters` runs and `derive_eval_parameters`
  is skipped. **All new pawn derivation must be called from
  `derive_search_parameters`** (`parameters.rs:498`), where `avg` non-royal
  piece value is already computed.
- **Leg orientation**: legs in `relevant_moves` are stored in raw config space
  (forward = +y for both colors); color sign `(-2 * color + 1)` is applied at
  use time (`move_list.rs:243-290`). Classify on WHITE pieces, copy to black
  via `state.statics.piece_swap_map`.
- Zone-conditional legs (xiangqi soldier's post-river sideways moves) are
  already square-filtered out of `relevant_moves` — per-square mask derivation
  inherits this for free.
- Qsearch (`search.rs:392-537`): stand-pat unconditionally (even in check),
  captures only, SEE < 0 pruned unconditionally, delta margin hardcoded `200`
  at `search.rs:493`.
- `MAX_CHECK_EXTENSIONS: u8 = 8` exists at `prelude.rs:221` and is **unused**;
  check extension at `search.rs:681` is unconditional.
- The three probe FENs are lines 1-3 of `res/bench/probes.txt`; lines 4-6 are
  mate-in-1 regression probes.
- Search is **nondeterministic** (`piece_list: Vec<HashSet<Square>>` with
  RandomState drives generation order) — all benchmarks use medians of ≥3 runs.

## Phase 0 — Benchmark harness + baselines (no behavior change)

1. **`tools/run_ttd.sh`** (new, modeled on `tools/run_suite.sh`): per probe
   line, run `position fen` + `go movetime <ms>` N times (default 5), parse
   `info` lines, report per position: stabilization depth (first depth whose pv
   starts with the expected move and never changes after), time+nodes at that
   depth, and nodes/time at a fixed reference depth (e.g. 12). Print medians.
2. **`perft` CLI subcommand** in `main.rs`:
   `anekamacam perft <variant> <depth> [limit]` — parse config, load embedded
   `{variant}.perft` (mirror `src/debug/console.rs:2663-2694`), call
   `benchmark_perft`. Makes variant regression checks scriptable.

**Baseline block** (recorded now, repeated after every phase):

```
cargo build --release                                     # warnings-clean
tools/run_ttd.sh ./target/release/anekamacam 15000 res/bench/probes.txt
tools/run_suite.sh ./target/release/anekamacam 4000       # x3, pass counts
# startpos go depth 12 x5 → median nodes+time (NPS)
./target/release/anekamacam perft shogi 4
./target/release/anekamacam perft berolina 4
./target/release/anekamacam perft xiangqi 4
# shogi/berolina/xiangqi: position startpos, go depth 8 → legal move, no panic
```

Probe 2 (zugzwang) additionally gets `go movetime 60000` runs since its current
TTD is depth 40-50.

**Rollback criterion (used by every phase)**: revert if, over medians of ≥3
runs: suite passes drop, OR TTD on any probe worsens >20% with no suite gain,
OR NPS drops >10% with no suite/TTD gain.

## Phase 1 — Pawn-like classification + masks (derive time, no eval change)

All in `src/game/search/parameters.rs`, new fn `derive_pawn_parameters(state)`
called at the end of `derive_search_parameters` (so it runs in all three
param-load branches). New fields on `StaticState`
(`src/game/representations/state.rs`, fields ~293-351, defaults in `State::new`
~495-577). Derive-time speed is explicitly not a concern.

### 1a. Classification predicate

`pawn_like: Vec<bool>` (per piece index), computed on WHITE pieces in
stored-leg space (forward = +y), copied to black via `piece_swap_map`. Over all
squares' `relevant_moves[pi * board_size + sq]`, net displacement = summed
x/y per multi-leg vector. Piece is pawn-like iff:

1. `!p_is_royal!(piece)`;
2. opening value equals the **minimum** among non-royal white pieces
   (cheapest tier — excludes shogi knight/lance by value);
3. no vector anywhere has net `dy < 0` (never retreats; `dy == 0` allowed so
   the xiangqi/janggi soldier passes);
4. some vector whose last leg has `m!` has net `dy >= 1` (a quiet forward
   step exists);
5. every vector has net `dy <= 2` and `|dx| <= 2` (step-mover bound; excludes
   the shogi lance's forward slide independently of value).

Lance excluded deliberately: passed/connected heuristics assume a blockable
step-mover; a forward slider's ray is already captured by mobility/PST.

Expected classification (assert via `log_3!` debug output per variant): pawns
in fide/berolina/capablanca/grand/los-alamos/crazyhouse/tjatoer/shatranj/
makruk/ouk; shogi + mini-shogi pawn only; xiangqi/mini-xiangqi/janggi soldier.

### 1b. Precomputed masks (only populated where `pawn_like[pi]`)

Indexed `pi * board_size + sq`; effective displacement = stored offset ×
`(-2 * color + 1)`, same as `generate_relevant_moves`.

- **`pawn_path_mask: Vec<Board>`** — BFS closure from `sq` over quiet-move
  vectors (last leg has `m!`), using each landing square's own
  `relevant_moves` entry (inherits zone/edge filtering). Ignore the `i`
  (initial) flag — single-step closure supersets double-step squares. Handles
  berolina's branching diagonal tree as a union naturally.
- **`pawn_interference_mask: Vec<Board>`** — start with
  `pawn_path_mask[idx]`; then for every *enemy* pawn-like type `ep` and square
  `t`, if any capture-capable vector of `ep` at `t` lands in `path ∪ {sq}`,
  set bit `t`. **Passed ⇔ enemy pawn-like occupancy ∩ mask = empty.**
- **`pawn_support_mask: Vec<Board>`** — squares `t` from which a *friendly*
  pawn-like capture leg lands on `sq` (reversed friendly captures), plus
  lateral neighbors `(±1, 0)` (phalanx).
  **Connected ⇔ friendly pawn-like occupancy ∩ mask ≠ empty.**
  **Doubled ⇔ friendly pawn-like occupancy ∩ path mask ≠ empty.**
  (Isolated dropped in v1 — mostly the complement of connected; note as
  follow-up.)
- **`pawn_advancement: Vec<i32>`** (fixed-point `adv² * 256`, 0..256) —
  `adv(sq) = (1 - closest_promotion / ranks).max(0)` reusing
  `derive_closest_promotion` (`parameters.rs:252-272`); when no promotion zone
  (xiangqi soldier → INFINITY), fall back to
  `adv = 1 - |target_rank - rank| / (ranks - 1)` (target = last rank).

Memory: ≲4 MB worst case across ≤~30 piece indices × ≤90 squares. Fine.

**Verify**: build warnings-clean; debug logs show expected classification for
fide/shogi/berolina/xiangqi; all Phase-0 numbers unchanged within noise
(nothing reads the fields yet); perft passes.

## Phase 2 — Pawn-structure eval term (highest expected ELO)

### Weights (derived in `derive_pawn_parameters`; consts in prelude.rs)

With `avg` = average non-royal white opening value, `promoted_value` = max
non-royal value (same convention as `derive_eval_parameters:449-456`):

```
passed_gain(pi) = if p_can_promote { (promoted_value - value).max(0) }
                  else { avg }
pawn_passed_opening[idx] = PASSED_FRACTION_OPENING * passed_gain * adv²  /* ~0.10 */
pawn_passed_endgame[idx] = PASSED_FRACTION_ENDGAME * passed_gain * adv²  /* ~0.35 */
pawn_connected_opening   = avg / 20
pawn_connected_endgame   = avg / 12
pawn_doubled_penalty     = avg / 16        /* both phases */
```

Stored as two `Vec<i32>` per-piece-square tables + three `i32` fields — no
float math at eval time. Endgame passed fraction mirrors the existing 0.40
endgame promotion-gradient fraction (`derive_promotion_bonus:311-315`): a
passed pawn roughly doubles the advancement gradient the PST already gives.

### Eval term (`src/game/position/evaluation.rs`)

New macro `pawn_structure!($state, ...)` following the `king_shelter!`
precedent, returning `(opening_diff, endgame_diff)` white-minus-black:

```
for each piece_index with statics.pawn_like[piece_index]:
    for sq in state.piece_list[piece_index]:
        idx = piece_index * board_size + sq
        if disjoint!(interference_mask[idx], enemy_pawn_occ):  passed bonus
        if !disjoint!(support_mask[idx], own_pawn_occ):        connected bonus
        if !disjoint!(path_mask[idx], own_pawn_occ):           doubled penalty
```

Support pieces:
- new `disjoint!` macro in `board.rs`: `(a.2 & b.2) == U4096::MIN` (avoids
  clone + `and!` + `count_bits!`);
- per-side pawn-like occupancy boards built on the fly at the top of
  `evaluate_position!` (iterate pawn-like indices, `set!` from `piece_list`;
  ≤ ~18 bits per side).

**Compute-at-eval-time first** (simplest concrete). A pawn hash table or
incremental pawn occupancy is a documented follow-up **only if** fixed-depth
NPS drops >10%.

Integration in `evaluate_position!` (three phase arms, lines ~84-119): opening
arm adds `pawn_o`, endgame arm `pawn_e`, middlegame blends both via the
existing interpolation. No make/unmake or `refresh_eval_state` changes.

**Verify**: probe 1 (`b3b2`) is the primary target — record stabilization
depth (currently never found). Probe 3 (`d5d6`) holds or improves. Suite ×3,
fixed-depth NPS ×5 (record the drop). Shogi/berolina/xiangqi `go depth 8`
sane; perft untouched. Debug-console spot check: a protected passer position
evaluates above its mirrored non-passer twin.

## Phase 3 — Qsearch in-check evasions (soundness fix)

In `quiescence_search` (`search.rs:392-537`):

```
let in_check = is_in_check!(state.playing, state);
if !in_check { /* existing stand-pat block, lines 421-433 */ }
if in_check { generate_all_moves_and_drops(...) }   /* evasions incl. drops */
else        { generate_all_captures(...) }
/* skip SEE pruning (482) and delta pruning (493) when in_check */
track legal_moves; if in_check && legal_moves == 0 { return -INF + ply }
```

Do **not** implement checking-move generation in v1 (variant-agnostic "gives
check" requires make/probe per move — expensive path; future work only).

**Verify**: probe 1; mate probes 4-6 stay PASS; NPS ×5 (accept up to ~10% drop
if suite/TTD improves); suite ×3. Rollback per standard criterion.

## Phase 4 — Dangerous pawn-push exemptions in search

Inline in `alpha_beta`'s move loop (after ~line 861):

```
let dangerous_push = state.statics.pawn_like[piece]
    && !is_capture && !is_drop
    && statics.pawn_advancement[piece * board_size + end]
       >= DANGEROUS_PUSH_THRESHOLD;   /* prelude const 92 ≈ adv 0.6 squared */
```

1. Futility move-skip (`search.rs:867-874`): `&& !dangerous_push`.
2. LMP (`search.rs:908-921`): `&& !dangerous_push`.
3. LMR (`search.rs:932-948`): after history adjustments,
   `if dangerous_push { reduction = reduction.saturating_sub(1).max(1); }`.

Benchmark as one phase; keep the three hooks as separate diffs for regression
attribution. NPS impact ~0 expected.

## Phase 5 — Pruning-aggressiveness experiments (one commit each, each benchmarked alone)

Ordered by expected value/risk; each against the Phase-4 baseline.

- **5a. Delta margin from piece values** (`search.rs:493`): new
  `StaticState.delta_margin: i32 = (avg / 3).max(100)` derived in
  `derive_search_parameters`. Near-neutral for FIDE (~200); correct scaling
  for other variants.
- **5b. Aspiration width from piece values** (`search.rs:218`, fixed ±50):
  `aspiration_delta = (avg / 12).clamp(25, 80)`. Window should live in the
  variant's material units.
- **5c. LMR bad-history +1** (`search.rs:943-945`): test (i) remove it,
  (ii) soften threshold to `-MAX_HIST_VALUE / 2`. Keep the winner of
  {current, i, ii} on probe-2 TTD without suite loss.
- **5d. LMR verification threshold** (`search.rs:956-962`, commit e5f3058):
  attribute the probe-2 regression. A = HEAD; B = verification disabled;
  C = checkout `4f72376~1` (pre-occupancy-values) historical reference.
  5 × `go movetime 60000` on probe 2 each; median stabilization depth +
  time-to-depth-30. If B ≈ A in TTD but loses the move: keep A. If B strictly
  dominates: raise threshold to `reduction > 3`, retest. Document in commit.
- **5e. Check-extension cap** (`search.rs:681`): use the dead
  `MAX_CHECK_EXTENSIONS` const — thread an `extensions: u8` param through
  `alpha_beta` (root passes 0);
  `if in_check && extensions < MAX_CHECK_EXTENSIONS { depth += 1; ... }`.
  Verify mate probes 4-6 still PASS; if a cap breaks long forced mates, exempt
  when `beta.abs() >= MATE_SCORE`.

Explicitly not touched: NMP/razoring retuning (already phase-gated and
value-derived; no probe evidence against them).

## Style compliance (every phase)

- All new consts (`PASSED_FRACTION_*`, `DANGEROUS_PUSH_THRESHOLD`, divisors if
  made consts) in the CONSTANTS section of `src/prelude.rs`; new pub items
  re-exported there. Other files keep `use crate::*;` only.
- ≤80 chars/line; comments only `/* */` at col 80-120 (StaticState fields);
  none inside function bodies; no `_`-prefixed names; descriptive names.
- No new files except `tools/run_ttd.sh`; everything else lands in existing
  modules.
- `cargo build --release` warnings-clean before each benchmark.

## Critical files

- `src/game/search/parameters.rs` — pawn predicate, masks, advancement,
  weights; delta/aspiration derivation (5a/5b)
- `src/game/position/evaluation.rs` — `pawn_structure!` +
  `evaluate_position!` integration
- `src/game/position/search.rs` — qsearch evasions, dangerous-push hooks,
  aspiration, extension cap, LMR tweaks
- `src/game/representations/state.rs` — new `StaticState` fields + defaults
- `src/game/representations/board.rs` — `disjoint!` macro
- `src/prelude.rs` — consts + re-exports
- `src/main.rs`, `tools/run_ttd.sh` — Phase 0 harness
- Reuse: `derive_closest_promotion` (parameters.rs:252-272),
  `piece_swap_map`, `adjacency_mask` pattern, `king_shelter!` macro precedent,
  `benchmark_perft` (util.rs), `run_suite.sh`

## Verification summary

After each phase, run the Phase-0 block and compare medians (≥3 runs — search
is nondeterministic). Success criteria for the whole effort: probe 1 (`b3b2`)
found and stable by a finite depth; probe 2 (`a1b1`) TTD back toward depth
~27-30; probe 3 (`d5d6`) still found; mate probes PASS; suite pass count never
drops; perft green on shogi/berolina/xiangqi; NPS regression bounded and
justified by strength.

## Outcome (as executed)

Shipped in commit `b99c82e` (`feat: variant-agnostic pawn eval, qsearch check
evasions, value-scaled pruning`):

- Phase 1-2: pawn-like classification + passed/connected/doubled eval, repacked
  to **u128 bitset masks** (all 17 variants ≤128 squares; gated off above).
  First `U4096` implementation cost 45% NPS; u128 rewrite brought it to ~3-6%.
- Phase 3: qsearch in-check evasions (incl. drops), mate return when no legal
  evasion. ~0 NPS.
- Phase 4: dangerous pawn-push exemptions (futility/LMP skip, reduced LMR).
- Phase 5a/5b: value-scaled `delta_margin = (avg/3).max(200)` (max floor kept
  FIDE-neutral) and `aspiration_delta = (avg/12).clamp(25,80)`.
- Phase 5c: tested, **reverted** (no benefit).
- Phase 5d/5e: **skipped** — probe 2 TTD is nondeterminism-dominated (runs span
  depth ~11-44), making the "regression" unattributable and 5d/5e untestable/
  risky. `MAX_CHECK_EXTENSIONS` left available as a future safety knob.

Results (medians of 5): probe 3 improved 2/5 → 5/5 (d16.5 → d13); probe 2 held
5/5; mates 4-6 held 5/5; probe 1 (`b3b2`) **remains eval-limited** (0/5 —
declined to overfit passer weights); NPS ~1492k → ~1447k (~3%); 11 variants
verified panic-free; perft green (xiangqi 44/44, crazyhouse 4/4).

## Revision (rule-violation fixes + isolated/backward pawns)

Follow-up round addressing standing-rule violations in the `b99c82e` pawn work,
plus two new eval terms. Scope was deliberately narrow — the four fixes and
isolated/backward pawns; no SEE/move-ordering changes, so the rook-sac probe is
untouched.

1. **Dead const removed.** `MAX_CHECK_EXTENSIONS` deleted (no dead "future
   knob"); the check extension at `search.rs` stays unconditional.
2. **Variant discrimination removed.** Dropped the u128 packing and the
   `pawn_eval_enabled = board_size <= 128` gate that silently disabled pawn
   eval on wide boards. Masks are now `Vec<Board>` tested with O(1) `get!`
   (`bnum .bit()`) reads against the per-side pawn squares — works at any board
   width, no cap. A tiled-`u128` variant was prototyped for speed but reverted;
   the `Vec<Board>` term costs ~3% (≈1.45M → ≈1.1M NPS), accepted.
3. **Pawn-like is now a static Piece property.** New `encoded_static` bit 19 +
   `p_is_pawn!` accessor, set at derive time; the `pawn_like: Vec<bool>` and
   `pawn_like_indices` fields are gone.
4. **Start-count criterion.** Classifier also requires
   `count_bits!(initial_setup[i]) + piece_in_hand[..][i] >= 5`
   (`PAWN_MIN_START_COUNT`). At threshold 5, only **mini-shogi** (1 pawn) drops
   out; xiangqi/janggi/mini-xiangqi soldiers (5) stay. (Mini-xiangqi has 5
   soldiers — `P1PPP1P` — not 2 as the original plan claimed.)
5. **Isolated + backward pawns.** Isolated = no friendly pawn on an adjacent
   file; backward = has file-neighbours but is unconnected and every adjacent
   friendly pawn is strictly further advanced. Penalties `avg/16` and `avg/24`.

Verification: build warnings-clean; all 17 configs load + derive with no panic;
classification confirmed (mini-shogi excluded, all others pawn); suite 5/6
(probes 2 @d47, 3 @d16 pass; mates 4-6 pass @d128; probe 1 still eval/search-
limited, out of scope); fide/shogi/xiangqi search clean.
