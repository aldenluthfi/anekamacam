# Material + PST Only — Evaluation Purge Experiment

## Context

Iteration 3 (plan 13) accumulated positional evaluation terms, derived masks,
state caches, hash support, parameter scalars, and search corrections without
producing a clean strength result. The ladder went in several directions at
once and no single term's contribution is separable from the rest.

This experiment resets the measurement baseline. It strips the evaluator down
to phase-tapered piece values and piece-square tables, and deletes every piece
of infrastructure whose only remaining consumer was a removed term. What
survives is the variant-rule machinery, the tactical search, the
material-scaled search margins, game-phase blending, and the royal tracking
termination needs.

The branch is `experiment/material-pst-only`, cut from a clean `main` HEAD.
The in-flight `zone_drop_pad` work was stashed first
(`stash@{0}: On main: wip: zone-drop-pad before material-pst experiment`)
and is untouched.

## Implementation

### 1. Evaluator and draw scoring

`src/game/position/evaluation.rs` keeps two macros. `evaluate_position!` reads
only the cached opening/endgame material and PST totals, blends them by phase,
and applies the side-to-move sign; it no longer takes `SearchBufs` or a
`PTable`. `terminal_score!` keeps its mate scale and scores draws as literal
zero.

Deleted: `draw_score!`, `king_shelter!`, `pawn_shield!`, `castling_bonus!`,
`king_danger!`, `open_shield!`, `pawn_structure!`, and the imbalance, pair,
and tempo terms folded into the old `evaluate_position!`.

`outcome_score!` in `termination.rs` returns zero for `Outcome::Draw`, and the
repetition cutoffs in `search.rs` return zero directly. Iterative deepening's
score-drop detector now reuses the derived `aspiration_delta` instead of
keeping `draw_bias` alive under another name.

### 2. State and derivation

`StaticState` loses every evaluation scalar (tempo, draw bias, king-safety
bonuses, imbalance weights, pair bonus, mobility, all pawn-structure terms and
passed-pawn scaling) and every lookup table built for them (`adjacency_mask`,
`royal_shield_mask`, `royal_front_mask`, `zone_attack`, `zone_attack_best`,
and the six pawn mask/score tables).

`State` loses `pair_score`, `has_castled`, `pawn_board`, `pawn_hash`, and
`corr_hist`; `Snapshot` loses `pawn_hash`. Pair-score maintenance is gone from
`piece_list_push!` / `piece_list_remove!`, and `populate_adjacency_mask` is
gone from `precompute`.

Kept, because search and termination still consume them: `royal_list`, the
material/PST caches, phase state, and the big/major/minor counts.

`parameters.rs` drops `derive_eval_scalars` entirely along with the pair,
draw-bias, pawn, royal-shield/front, and zone-attack derivations.
`derive_pawn_like` and `derive_pawn_advancement` go too — their last consumer,
dangerous-push pruning, is also removed. `derive_eval_parameters` (piece
values, role flags, phase thresholds, PSTs) and the material-derived search
margins in `derive_search_parameters` are untouched.

`piece.rs` drops encoded-static bit 19 and `p_is_pawn!`.

### 3. Pawn hash, correction history, dangerous push

`hash_pawns`, the incremental pawn-hash updates, `pawn_board_in_or_out!`, the
snapshot save/restore, the FEN recomputation, and the debug verification are
all deleted. `has_castled` make/undo tracking and the pair-score refresh and
verification go with them. Full-position Zobrist hashing is unchanged.

In `search.rs`: correction-history indexing, reads, updates, macros, and doc
prose are gone, so the pruning stages read the plain eval. The dangerous-pawn-
push futility and LMP exception and its advancement lookup are gone.
`SearchBufs` loses `pawn_entry_buf`. TT static-eval reuse and every unrelated
search heuristic are preserved.

### 4. Pawn transposition table

`PTEntry`, `PTable`, and the `pt_index!` / `probe_pt_entry!` / `hash_pt_entry!`
macros are deleted from `transposition.rs`. Ownership, construction,
arguments, cloning, counters, and log output are removed across `search.rs`,
`parallel.rs`, `protocol.rs`, `util.rs`, `headless.rs`, `datagen.rs`,
`graphics.rs`, `tuning.rs`, and `prelude.rs`. `Session`, `ThreadPool`, the
search entry points, the debug tools, and hash resizing now carry TT and QT
only, with the budget split reduced to `HASH_T_PARTS`/`HASH_Q_PARTS`.

### 5. Parameter schema and tuner

The on-disk format is now phase thresholds, material values, role flags, and
opening/endgame PST rows. The 19-token scalar tail, its parsing, its export,
and `PARAM_SCALAR_TAIL` are gone. All 38 `res/param/*/latest.param` files had
exactly 19 trailing tokens removed, with every threshold, value, flag, and PST
token preserved byte-for-byte.

`tuning.rs` no longer touches `PTable`, `SearchBufs`, current theta, or the
evaluator during sample extraction: the bare evaluator *is* the sparse
material/PST dot product, so `Sample` loses its frozen `base` residual and
`model_score` is `f·θ`. Export emits thresholds, flags, material, and PSTs
only, still routed through `parse_tuned_parameters` →
`derive_search_parameters` → `export_tuned_parameters_file`.

### 6. Prelude audit

Removed as zero-consumer: `CORR_HIST_*` (5), `DANGEROUS_PUSH_THRESHOLD`,
`DRAW_BIAS_DIV`, `PAWN_MIN_START_COUNT`, `PARAM_SCALAR_TAIL`, plus the
already-dead `MAX_PIECES` and `MIN_LMP_DEPTH`.

Single-subsystem constants moved to their owning modules rather than staying
globally re-exported: SPRT constants to `sprt.rs`, Adam/Texel constants to
`tuning.rs`, protocol option and time-management constants to `protocol.rs`,
and the occupancy constants to `parameters.rs`. `HASH_*_PARTS` and
`HASH_DEFAULT_MB` stay in the prelude because `T_TABLE_SIZE`/`Q_TABLE_SIZE`
are computed there. No compatibility aliases were kept for purged code.

## Result

`57 files changed, 248 insertions(+), 2739 deletions(-)` — of which `src/` is
`19 files changed, 210 insertions(+), 2701 deletions(-)`. Largest deletions:
`parameters.rs` -961, `evaluation.rs` -695, `transposition.rs` -274,
`search.rs` -210, `state.rs` -173.

## Verification

All performed on this branch; formatting is hand-maintained, so `cargo fmt`
was deliberately not run.

1. **Build** — `cargo check --workspace --all-targets` and
   `cargo build --release` are clean. `cargo clippy --workspace --all-targets`
   reports 3 warnings, identical to the `main` baseline (verified by stashing
   the diff and re-running); two empty `if is_unload {}` blocks left by the
   purge were removed rather than left to warn.

2. **Symbol sweep** — zero live references to `PTable`, `pawn_hash`,
   `pawn_board`, `corr_hist`, `has_castled`, `draw_score`, `draw_bias`,
   `p_is_pawn`, `pawn_advancement`, `dangerous_push`, `zone_attack`,
   `adjacency_mask`, `royal_shield_mask`, `royal_front_mask`,
   `PARAM_SCALAR_TAIL`, and every removed scalar. (`pair_score` survives only
   as an unrelated local in the SPRT pentanomial code.)

3. **Variant load** — `debug-headless state` succeeds for all 38 shipped
   variants, validating both the shortened embedded parameter files and the
   parse and derive paths.

4. **Evaluation** — symmetric start positions score 0 cp in standard,
   crazyhouse, shogi, xiangqi, and grand. Positions differing only in castling
   rights (`KQkq` vs `-`) now evaluate identically, and a white/black mirrored
   pawn-advance position scores 26 cp for both sides, confirming the removed
   safety and structure metadata no longer contributes and the taper stayed
   symmetric.

5. **Termination and FEN** — `tools/run_endgame_fixtures.sh` 38/38 passed;
   `tools/run_fen_roundtrip.sh` 44/44 passed, 0 failed, 0 skipped.

6. **Make/undo** — all 14 shipped perft suites pass at depth 4 (standard
   120/120 at `--limit 30`, xiangqi 44/44, janggi 28/28, shogi 16/16, sittuyin
   16/16, and 4/4 each for berolina, capablanca, crazyhouse, grand,
   los-alamos, makruk, minishogi, minixiangqi, shatranj). The other 24
   variants ship no perft suite, so they were covered instead by a debug build
   — where `verify_game_state` asserts every incremental cache each node —
   running `debug-headless search <variant> 4 1` across all 38 variants with
   zero failures. That is the check that matters here, since the removed pawn
   hash, pawn boards, pair score, and `has_castled` were exactly the
   incrementally maintained state.

### Not yet run

- `tools/speed-suite.sh` on standard, shogi, crazyhouse, xiangqi, and grand
  with a pinned `ANEKAMACAM_SEED` — record nodes, NPS, and binary size as
  experiment data. Search behaviour will differ; that is the point of the
  experiment, not a correctness failure.
- `tools/agree-suite.sh` against Fairy-Stockfish, as measurement only.
- A round robin to price what the removed evaluation was actually worth.
