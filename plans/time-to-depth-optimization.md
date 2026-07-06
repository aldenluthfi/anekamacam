# Time-to-Depth Optimization Plan — AnekaMacam

## Context

Engine has nps parity with Fairy-Stockfish (~1.9M both) but searches ~10x more
nodes per depth (startpos depth 17: 16.4M vs 1.6M). Pure tree-size problem:
move ordering quality + pruning aggressiveness + TT effectiveness. Goal:
maximize time-to-depth by shrinking the tree, then add strength-focused search
features (singular extensions, correction history, ProbCut).

**Verified root causes** (all confirmed by reading code):

1. RFP/razoring/futility gated on `pv_move.is_none()` (TT-move absence) at
   `search.rs:903,918,978` — the three cheapest prunings are disabled at
   exactly the TT-hit nodes that dominate the tree. Should gate on non-PV
   (`beta - alpha == 1`).
2. LMP double-gated by `legal_moves >= (moves_len/2).max(1)` (`search.rs:1105`)
   — overrides the threshold table on any wide movelist.
3. LMR under-applied: gate `legal_moves > (moves_len/5).max(1)`
   (`search.rs:1125`), killers never reduced (`:1126-1127`), conservative
   divisors (5 / 5.5 vs Stockfish-family ~2.25), re-search only when
   `reduction > 2` (`:1156`).
4. History: butterfly-only; malus fires for every non-best quiet at EVERY node
   (`search.rs:1268-1272`) drowning signal; no malus-on-cutoff for tried
   quiets; no continuation history, no capture history.
5. IID (depth>=7, full-window depth-2 pre-search, `search.rs:989-998`) AND IIR
   both run — IID duplicates work IIR compensates for.
6. NMP weak: R = 3 + depth/8, blanket ENDGAME-phase ban, no eval-delta term
   (`search.rs:938-962`).
7. TT: single slot, `hash % size`, no static eval stored, replacement clause
   `old_score <= $score` lets shallow entries evict deep ones
   (`transposition.rs:407`).
8. No bench command (`main.rs:84` documents it, dispatch at `main.rs:93-118`
   lacks the arm); Zobrist RNG seeded from process start time
   (`prelude.rs:313-315`) → cross-process node counts incomparable.

## Constraints (uphold throughout)

- Variant-agnostic: no FIDE assumptions, no variant/dialect tokens. All new
  margins derived from avg piece value in `derive_search_parameters`
  (`parameters.rs:1359-1447`). Tables sized dynamically from
  `piece_count * board_size` with hard byte ceilings (MAX_SQUARES=2048,
  MAX_PIECES=255).
- Code style: 80-col, `/* */` comments at col 81+ only, all imports/consts in
  `prelude.rs`, other files `use crate::*;` only. No unit tests — verify via
  release-build CLI (debug build broken).
- Search nondeterministic across processes — comparisons need fixed seed
  (Stage 0) or statistics.
- Exactly one commit per stage, independently bench-verifiable. No
  Co-Authored-By / AI attribution in commit messages.

## Verification protocol (per user decision)

- **Every stage**: fixed-seed bench node counts across 4 variants (fide,
  crazyhouse, shogi, xiangqi — all have .conf + .perft files). Accept if
  geomean node ratio < 1.0 and no variant regresses > 5%.
- **SPRT additionally** (existing console `sprt` cmd, `src/debug/sprt.rs`,
  alpha=beta=0.05, ~100ms movetime, fide + crazyhouse) for risky stages:
  Stage 4 (LMR retune), Stage 5 (NMP), Stage 8 (singular), Stage 9 items.

## Stage 0 — Measurement harness (prerequisite)

Files: `src/prelude.rs`, `src/main.rs`, `src/game/util.rs`.

1. `prelude.rs` RNG init: read `ANEKAMACAM_SEED` env var; if parses as u64 use
   `StdRng::seed_from_u64(seed)`, else keep time-based. Same-seed binaries
   from different commits then produce identical Zobrist tables → exactly
   comparable node counts.
2. Also replace default hasher (`hashbrown::HashMap::new()` etc. — 41 call
   sites) with `foldhash::fast::FixedState` alias so iteration order is
   deterministic across runs. Requires `foldhash` workspace dep (add to
   `Cargo.toml` + `src/Cargo.toml`).
3. Implement documented `bench` arm in `main.rs` dispatch:
   `anekamacam bench [variant=fide] [depth=12] [positions=16] [seed=0xA11EBEEF]`.
   - Set seed env before `parse_config_file` (first lazy RNG touch).
   - Suite: variant startpos + first N FENs from
     `EMBEDDED_PERFT."{variant}.perft"` (FEN = text before first `,`),
     loaded via `parse_fen` with `dict=None` (FEN dict round-trip corrupts
     board — known issue).
   - Shared tables (TTable/QTable/PTable allocated once, reused across suite)
     to match real game behaviour. Per position: fixed-depth single-threaded
     `search_position`, accumulate nodes.
   - Output: per-position lines + final `bench: <nodes> nodes <nps> nps`.
4. `run_search_bench` helper in `src/game/util.rs`, re-exported via prelude.
5. Sanity: same binary+seed → identical totals; different seed → different.

**Status**: committed (b91caf8). Deterministic, perft 200/200, Stage 1
regression confirmed.

## Stage 1 — Fix over-restrictive pruning gates (biggest cheap win)

File: `src/game/position/search.rs`.

1. RFP (`:902-910`): gate `(pv_move.is_none() || !pv_capture)` →
   `beta - alpha == 1`. Delete now-unused `pv_capture` (`:892-896`).
2. Razoring (`:916-932`): `pv_move.is_none()` → `beta - alpha == 1`; keep
   zero-window `quiescence_search(alpha, alpha+1)` as verification.
   Keep ENDGAME guard.
3. Futility flag (`:976-983`): `pv_move.is_none()` → `beta - alpha == 1`.
4. LMP (`:1105`): delete `legal_moves >= (moves_len/2).max(1)` conjunct;
   LMP_THRESHOLD table alone gates. Move LMP check before `make_move!` so
   pruned tails don't pay legality probe overhead. Remove `opponent_in_check`
   from LMP condition (LMP is already gated `!in_check`); compute
   `opponent_in_check` lazily only inside the LMR branch.
5. Delete IID block (`:989-998`), keep IIR. `MIN_IID_DEPTH` const removed from
   prelude. Update alpha_beta doc header accordingly.

**Status**: committed (fe0aa5b). Results:
- fide d14: 3666073 → 1647341 (-55%)
- crazyhouse d12: 449584 → 258051 (-43%)
- shogi d9: 18650478 → 10680013 (-43%)
- xiangqi d12: 2070689 → 1704927 (-18%)

## Stage 2 — History malus scheme

File: `src/game/position/search.rs`.

Tested and rejected: standard Stockfish-style fail-high-only updates
(bonus cutoff move, malus previously tried quiets) regressed nodes +19-47%
across all variants. The existing all-node malus (bonus on alpha improve,
immediate malus for every tried non-best quiet) is empirically superior for
this engine's wide unstaged movegen.

**Conclusion**: do NOT replace the update sites. Stage 3 (continuation
history) will mirror the existing three update sites instead.

## Stage 3 — Continuation history (1-ply + 2-ply)

Files: `state.rs`, `search.rs`, `move_ordering.rs`, `prelude.rs`.

**Memory-safe indexing** (single concrete scheme):
- Key of (piece, to): `k = piece * board_size + end`; key space
  `ps = piece_count * board_size`.
- `prelude.rs`: `CONT_HIST_MAX_DIM: usize = 1 << 11` (2048).
- `cont_dim = ps.next_power_of_two().min(CONT_HIST_MAX_DIM)`; fold
  `kf = if ps <= cont_dim { k } else { (k ^ (k >> 11)) & (cont_dim - 1) }`.
  Exact for FIDE (ps=768→1024); XOR-fold for monster variants. Collisions
  acceptable in a heuristic table.
- `State.cont_hist: Vec<i16>`, len `2 * cont_dim * cont_dim` (slot 0 = 1-ply,
  slot 1 = 2-ply). FIDE 4MB, worst case 16MB per thread. Allocate in
  `from_statics` when length differs.

Wiring:
1. At node entry derive `prev1`/`prev2` keys from `state.history` snapshots
   (skip null-move snapshots where `move_ply == null_move()`, `usize::MAX` =
   none). Pass folded keys into `score_move!`/`pick_by_score!` (extend macro
   params).
2. Ordering quiet branch: `hist = butterfly + cont1 + cont2`. Re-band to stay
   disjoint: quiet base `1_000_000 + 3*MAX_HIST_VALUE + 1`, killer base
   `1_000_000 + 7*MAX_HIST_VALUE` {+1,+2}. Losing captures below, winning
   captures (4M+) and PV (5M) above. Update score_move! band doc comment.
3. Updates: mirror the three existing update sites (bonus on cutoff quiet,
   bonus on alpha-improve quiet, malus on non-best quiet) into both
   `cont_hist` slots. Store the two cont indices per tried quiet in a
   `quiet_tried_buf: Vec<Vec<[usize; 3]>>` (upgrade from `Vec<Vec<usize>>`).
4. LMR history hook (`:1134-1142`): use combined sum (range ±3·MAX);
   thresholds `> 3*MAX/8` → −1, `< -3*MAX/8` → +1.
5. History-based shallow pruning: before `make_move!`, at non-PV, `!in_check`,
   `depth <= 2`, `legal_moves > 0`, quiet non-drop non-promo
   non-dangerous-push, `cont1 + cont2 < -MAX_HIST_VALUE/2` → skip.

Drops participate naturally (piece/end well-defined). Per-thread by
construction (Lazy SMP clones State).

Expected: −15-30%; crazyhouse benefits most. Bench.

## Stage 4 — LMR aggressiveness retune (after 2-3; SPRT mandatory)

Files: `state.rs:648-675` (tables), `search.rs`, `prelude.rs`.

1. Tables (call site computes `reduction = 1 + table`):
   - quiet: `(ln(d)*ln(m)/2.25 − 0.25).clamp(0, d−1)`
   - quiet-in-check: quiet − 1.0
   - capture/promo/drop: `(ln(d)*ln(m)/3.25 − 0.5).clamp(0, d−1)`
   - capture-in-check: capture − 1.0
2. Gate (`:1125`): → `legal_moves > 1 + 2 * (beta - alpha > 1) as usize`
   (move 2 at non-PV, move 4 at PV).
3. Delete killer-exclusion conjuncts (`:1126-1127`).
4. Add adjustments (keep existing improving/dangerous-push/history): +1 when
   `beta - alpha == 1 && pv_move.is_none()` (expected cut node, poor
   ordering); −1 when `beta - alpha > 1` (PV).
5. Re-search trigger (`:1156`): `reduction > 2` → `reduction > 1` (verify
   every reduced fail-high at depth−1 — makes aggressive tables sound).
6. LMP retune (`prelude.rs:226-229`): not-improving
   `[0,2,3,6,9,14,19,26,33]`, improving `[0,4,7,12,19,28,39,52,67]`.

Expected: −15-35%, widest variance. Bench all 4 variants, iterate divisors
(2.25/3.25) on bench, then SPRT.

## Stage 5 — NMP strengthening with variant-safe verification (SPRT)

Files: `search.rs:938-962`, `parameters.rs`, `prelude.rs`, `state.rs`.

1. `prelude.rs`: `MIN_NMP_DEPTH: usize = 2` (stop borrowing MIN_LMP_DEPTH).
2. Derived param `nmp_eval_div = (avg / 2).max(1)` (StaticState +
   `derive_search_parameters`).
3. Conditions: `depth > MIN_LMP_DEPTH` → `depth >= MIN_NMP_DEPTH`;
   `game_phase != ENDGAME` → `(game_phase != ENDGAME || depth >= 8)`. Keep
   `static_eval >= beta` and material guard (universal zugzwang defense).
4. `reduct = 4 + depth/4 + ((static_eval − beta) / nmp_eval_div).clamp(0,3)`;
   child depth `depth.saturating_sub(reduct)` (0 → qsearch via existing
   terminal path).
5. Verification: if null search `>= beta` AND `game_phase == ENDGAME`, run
   real-position zero-window `alpha_beta(depth.saturating_sub(reduct),
   beta−1, beta, ..., null=false)`; cut only if that also `>= beta`.

Variant-safety: zugzwang variants keep 3 defenses — material guard, shallow
endgame ban (endgame + depth<8 still refuses), deep-endgame verification.
Bench + SPRT (fide + shatranj/makruk if playable, else xiangqi).

## Stage 6 — TT: eval caching, indexing, replacement, clusters

Files: `transposition.rs`, `prelude.rs`, call sites in `search.rs`.

**Part A (low risk, first):**
1. Bit layout: slot[1] bits 0-1 flags, 2-8 depth, 9-40 score, 41-104 sig →
   bits 105-127 free (23 bits). Store static eval as 23-bit two's-complement
   (|eval| < INF = 2M < 4.19M lossless); sentinel `TT_EVAL_NONE = −(1<<22)`
   for in-check. Macros `tt_enc_eval!`/`tt_eval!` (sign-extend).
2. Widen `probe_tt_entry!` to also return eval, depth, flags (Stage 8 needs
   depth/flags). Update both call sites.
3. In alpha_beta: `static_eval` from TT when available (nps win); separate
   `eval_for_pruning` = tt_score when bound brackets static_eval
   (FBETA/FEXACT and tt_score > static_eval, or FALPHA/FEXACT and <). Use
   `eval_for_pruning` in RFP/razor/futility/NMP-delta; keep raw value in
   `state.static_eval[ply]` for improving flag.
4. `hash_tt_entry!` gains `$eval` param (TT_EVAL_NONE when in check).
5. Replacement (`:403-408`): delete `old_score <= $score`; policy = `empty ||
   different || entry.age < age || $flags == FEXACT || $depth + 3 >=
   old_depth`.
6. Power-of-2 sizing + mask: round sizes down to pow-2; `tt_index!` →
   `hash & (size − 1)`; same for qt/pt index macros; pow-2 helper in prelude.

**Part B (clusters of 4):**
- `num_clusters = entries/4` (pow-2); `base = (hash & (nc−1)) * 4`. Probe
  scans 4 for parity match. Store: parity-match slot, else empty, else evict
  min `quality = old_depth − 8*(cur_age − entry.age)`. Per-entry seqlock
  unchanged — concurrency story untouched.

Measure: bench at depth 15-16 (TT pressure) + nps (eval caching ~+5-10% nps);
Part B also multi-thread stability check (threads=4 long search, legal PV).

## Stage 7 — Capture history

Files: `move_ordering.rs`, `search.rs`, `parameters.rs`, `state.rs`.

1. `capt_hist: Vec<i16>` sized `piece_count * board_size * 8`; index
   `piece*bs*8 + end*8 + victim_bucket`. Precompute `piece_value_bucket:
   Vec<u8>` in StaticState: `min(7, 8*value/(max_value+1))`; multi-captures
   bucket summed victim value clamped. Worst case ~8.4MB — safe.
2. Ordering capture branch: add `capt_hist as i32 * capt_hist_scale /
   MAX_HIST_VALUE as i32` to SEE score (derived `capt_hist_scale =
   (avg/3).max(1)` caps contribution below one victim step; band split stays
   on raw SEE sign). Qsearch inherits via score_move!.
3. Updates: track tried legal captures per ply (extend `quiet_tried_buf` or
   add a parallel `capt_tried_buf`); bonus cutoff capture, malus
   previously tried captures; malus tried captures on quiet cutoff too.

Expected: −3-8%. Bench; cheap revert if flat.

## Stage 8 — Singular extensions + multicut + negative extension (SPRT)

Files: `search.rs`, `state.rs`, `prelude.rs`. Requires Stage 6 Part A.

1. `State.excluded: Vec<PseudoMove>` (len MAX_DEPTH, null default, reset in
   `clear_search`). When set at ply: skip TT-cutoff return (keep move for
   ordering), skip NMP/RFP/razor/IIR, skip final TT store, skip the excluded
   move in loop (`m_matches!`).
2. Trigger after TT probe: `depth >= 8 && ply > 0 && pv_move.is_some() &&
   tt_flags != FALPHA && tt_depth >= depth − 3 && tt_score.abs() < MATE_SCORE
   && excluded[ply] is null`.
3. `singular_beta = tt_score − singular_margin * depth` (derived
   `singular_margin = (avg/64).max(1)`); zero-window `alpha_beta(depth/2,
   singular_beta−1, singular_beta)` with excluded set.
   - `score < singular_beta` → extend TT move +1 (that child only).
   - `singular_beta >= beta` → multicut: `return singular_beta`.
   - else if `tt_score >= beta` → negative extension: TT move at depth−2.
4. Don't stack with check extension at same node; MAX_DEPTH backstop guards
   runaway.

Metric note: singular +nodes, multicut −nodes; net ~neutral but stronger.
SPRT decides; if flat, keep multicut/negative-ext, drop the +1.

## Stage 9 — Optional (priority order)

1. **Correction history**: `corr_hist: [Vec<i16>; 2]` per side, 16384
   entries, index `pawn_hash & 16383` (pawn hash exists). Update at node end
   when `!in_check`, non-mate, bound-consistent; gravity with
   `bonus = (best_score − static_eval).clamp(±cap) * depth_weight`. Apply
   `corrected = static_eval + corr/64` where eval_for_pruning used + improving
   flag. Pawnless variants degrade gracefully to side-to-move bias. SPRT.
2. **Aspiration tweak**: `aspiration_delta = (avg/24).clamp(12, 40)`
   (`parameters.rs:1399`). Bench only.
3. **ProbCut**: non-PV, depth>=5, `raised = beta + probcut_margin` (derived
   avg/4); SEE-winning captures: qsearch-verify then zero-window depth−4;
   cut at `>= raised`. Skip if Stage 8 multicut leaves <2% headroom. SPRT.

**Dropped, with rationale**: qsearch quiet checks (no cheap variant-agnostic
gives-check predicate; TT-move-first already implemented in qsearch);
countermoves table (subsumed by 1-ply conthist).

## Stage interaction map

- Stage 1 before 4 and 5 (pruning baselines shift).
- Stages 2-3 before 4 (LMR aggressiveness assumes good ordering; LMR history
  thresholds rescaled in 3).
- Stage 6A feeds Stage 1 prunings (eval_for_pruning) and is prerequisite of
  8 and 9.1.
- After Stage 4: re-run full 4-variant bench matrix once as combined
  regression check.

## Verification summary

- Build: `cargo build --release` only.
- Per stage: `anekamacam bench <variant> <depth>` × {fide, crazyhouse, shogi,
  xiangqi} × seeds {1,2,3}; accept geomean < 1.0, no variant > +5%.
- SPRT (console `sprt` cmd) for Stages 4, 5, 8, 9.1, 9.3.
- End-to-end: repeat Fairy-Stockfish startpos depth-17 comparison
  after Stage 4 and after Stage 8.
- Perft unchanged throughout (no movegen changes) — spot-check
  `perft fide <limit>` after each stage.
