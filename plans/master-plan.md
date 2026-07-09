# Time-to-Depth Optimization — Master Plan (v2)

## Status (2026-07-07)

- Stage A committed `5b8357d` (drop redundant IID sub-search).
- Stage B committed `6a5ab5e` (continuation history 1-ply + 2-ply).
  Measured (6-run means): fide d13 446k→395k (−11.5%), xiangqi d11
  190k→159k (−16.4%).
- Stage C committed `39d6b3b` (LMR retune: gate move 3/5, killer exclusion
  deleted, re-search on `reduction > 1`). Measured: fide d13 →167k (−58%
  vs B), xiangqi →99.6k (−37%). Cumulative startpos ladder: d17 781k
  (fsf 1.62M; pre-plan 7.18M). **Awaiting SPRT verdict** (was blocked on
  the bugfix plan: pv-walk corruption + sprt runner panics; see commit
  `91f5a79` and the fixes that follow it).
- Bugfix committed `ac510c0` (validate pv walk + royal capture
  bookkeeping); `bin/stageB` (6a5ab5e + cherry-picked fix) and `bin/stageC`
  (main) rebuilt and crash-position-verified. Pending from the bugfix plan:
  Fix 3 (sprt runner aborts cleanly on engine death, `src/debug/sprt.rs`)
  and Fix 4 (per-pid logs + panic hook, `src/io/logger.rs`) — apply on
  request as commit `fix: sprt abort on engine death, per-pid logs`.
- Stage C SPRT: inconclusive at budget (538W 532L 930D over 2000 games,
  stageB perspective, elo +1.0) — stageC elo-neutral, accepted (goal was
  node cut without strength loss). Note: runner reports from **engine A =
  first arg**; pass candidate first (`sprt <candidate> <baseline>`), or
  use non-regression bounds `sprt <cand> <base> <mt> 2000 -5 0`.
- Stage D committed `21999d9` (gentler aspiration widening:
  `delta += delta/2`, was doubling). Spike re-measure: post-B+C ladder is
  flat (per-ply <= 1.8, old 3.2×/2.6×/2.4× spikes gone; deep-depth ladder
  noise is ±30%, so 4-variant bench geomean is the signal). Bench geomean
  0.90/0.93 vs C over two batches. Lower initial delta measured
  neutral-to-worse, dropped; optional fail-high depth-reduction item
  deferred. `bin/stageD` saved.
- Stage D SPRT: non-regression bounds `[-5, 0]`, 529W 537L 934D over 2000
  games (stageD perspective, ~−1.4 elo), LLR +0.204, inconclusive at
  budget — no regression, node cut kept. **Accepted.**
- Stage E committed `96d4360` (eval-scaled NMP with endgame verification:
  `MIN_NMP_DEPTH=2`, `reduct = 4 + depth/4 +
  ((static_eval-beta)/nmp_eval_div).clamp(0,3)` capped at `depth`, derived
  `nmp_eval_div=(avg/2).max(1)`; endgame ban replaced with
  `depth >= MIN_NMP_ENDGAME_DEPTH (8)` + zero-window verification search
  without null; material guard kept). Bench geomean **0.758** vs D (fide
  −22%, crazyhouse −21%, shogi −21%, xiangqi −33%). Startpos ladder d20
  1.62M vs D's 3.18M (fsf 6.16M); d17 wobble +43% within deep-ladder
  noise. `bin/stageE` saved.
- Stage E SPRT: non-regression bounds `[-5, 0]`, 302W 248L 450D over 1000
  games (stageE perspective, ~+19 elo), LLR +1.819, inconclusive at
  budget — leaning positive, no regression. **Accepted.**
- Stage F Part A committed `d8c333f` (TT eval caching + pow-2 mask
  indexing): 23-bit static eval in slot[1] bits 105-127 (`-INF` sentinel =
  written in check), `probe_tt_entry!` widened to
  `(valid, score, move, eval, pruning_eval)` with eval returned regardless
  of stored depth; search reuses stored eval over a fresh
  `evaluate_position!` and RFP/razor/NMP/futility read `pruning_eval`
  (stored score sharpens eval when its bound brackets it; improving flag
  keeps raw eval). Tables floored to power-of-two, `%` → mask in
  `tt/qt/pt_index!`. **Replacement clause `old_score <= $score` KEPT** —
  plan called it a bug but deleting it measured +11-15% nodes (stale TT
  moves; theory lost to measurement again, cf. history malus).
  Measurement note: single-position benches proved too noisy for Stage-F
  -sized effects (stageE fide d15 swung 221k-403k across batches); built
  multi-position suite bench (16-32 perft-suite FENs/variant, summed
  nodes+time; inert control build read 0.998-1.003). Suite results vs E:
  4-variant nodes 0.953 / time 0.936; fide 32-pos d11 nodes 0.929; fide
  12-pos d14 nodes 0.916 / time 0.900. No variant regressed. Threads=4
  behavior unchanged (pre-existing depth-4 stop quirk in both E and F).
  `bin/stageF` saved. Part B (4-entry clusters, quality eviction)
  deferred — Part A already delivers the win at lower risk.
- Bug found while building the suite: UCI `position fen` panics on some
  legal FENs (`game_io.rs:2004` ParseIntError PosOverflow — the unanchored
  `inverse_fen` ep-regex overflow again); crazyhouse `position fen` dies
  on nearly every suite FEN. Workaround in suite: prefilter FENs, drive
  crazyhouse via `position startpos moves`. Fix candidate for the pending
  hygiene commit alongside Fixes 3-4.
- Stage G committed `4d6e0e4` (capture history): `capt_hist` sized
  `piece_count * board_size * CAPT_HIST_BUCKETS(8)`, indexed
  `capt_hist_index!` = `piece*B*8 + end*8 + victim_bucket`, bucket =
  `victim_value / capt_hist_div` (derived `capt_hist_div=(avg/4).max(1)`),
  saturated at 7. Ordering: winning band `4M + h + SEE + capt`, losing
  band `1M - h + SEE + capt` (shifts keep bands in their lanes; in-search
  SEE pruning decodes `SEE + capt` — history-adjusted threshold).
  Bonus/malus mirror the quiet scheme: bonus on capture cutoff and
  alpha-raise, all-node malus otherwise. Suite vs F: czh 0.951, shogi
  0.888, xiangqi 0.939; fide sample-split (0.836/1.069/1.046/0.955 —
  mean ≈0.97), geomean ≈0.94. Win-only-band variant measured worse
  (0.977 vs 0.955 in-batch), discarded. `bin/stageG` saved.
- Stage H committed `c86dd76` (singular multicut + negative extension):
  `State.excluded` per ply; trigger at `depth >= MIN_SINGULAR_DEPTH (8)`,
  `ply > 0`, `ply < 2 * info.root_depth` (new SearchInfo field set per ID
  iteration), TT move present, `tt_depth >= depth-3`, bound != FALPHA,
  non-mate; zero-window `alpha_beta(depth/2, sb-1, sb, null=false)` with
  the TT move excluded, `sb = tt_score - depth * singular_margin` (derived
  `(avg/128).max(1)`). Exclusion disables TT cutoff + TT stores + the move
  at that node. On fail-high: multicut return `sb` when `sb >= beta`, else
  negative extension (TT move searched one ply shallower) when
  `tt_score >= beta`. **The +1 singular extension was dropped per the plan
  fallback** — with it, nodes exploded (fide +63-94%, geomean 1.37-1.43,
  even ply-capped); extension chains compound with the uncapped check
  extension. Also: `is_tt_move` m_matches must be gated behind
  `singular_ext != 0` — unguarded it cost measurable NPS. Final form
  node-flat: 4-variant suite nodes 1.010 / time 1.036, fide d16 nodes
  1.028 (xiangqi 0.882). Deep d16-x8 wall-time readings unreliable
  (control stageG-vs-own-copy read time 1.63); node counts are the
  signal. `bin/stageH` saved. **Awaiting SPRT verdict**
  (`sprt bin/stageH bin/stageG 100` or non-regression
  `sprt bin/stageH bin/stageG 100 2000 -5 0`) — node-flat stage, so the
  SPRT decides whether multicut/negext buys strength; clean revert
  candidate if negative.
- Stage I committed `5bd2ace` (correction history keyed on side and pawn
  structure): `corr_hist` per side, 16384 i16 entries indexed
  `side * CORR_HIST_SIZE + (pawn_hash & (SIZE-1))`, per-thread like the
  other histories. Read: `corr = entry / CORR_HIST_GRAIN (64)` added to
  `pruning_eval` only — raw eval keeps feeding the improving flag and TT
  stores (Stage-F convention). Update at both exits (beta cutoff FBETA;
  final store FEXACT/FALPHA with the stored score) as a depth-weighted
  moving average `entry = (entry*(256-w) + gap*64*w)/256`,
  `w = min(depth+1, 16)`, clamp ±`CORR_HIST_LIMIT (64*64)`; skips
  in-check (`eval == -INF`), mate scores, bound-inconsistent scores, and
  excluded (singular) nodes. **Capture-scored nodes blend at w=1**: full
  weight exploded shogi (nodes 1.79×, 10-run), full skip regressed czh
  (1.06×; captures feed the pocket, so their gap is partly structural
  there) — minimum weight split the difference. Suite vs H (6-run):
  fide 0.988, czh 0.942, shogi 1.044, xiangqi 0.934, nodes geomean
  0.976 / time 0.967 (shogi/czh flip sign between batches; cross-batch
  means ~0.98). Scale check: derived shogi avg ≈750 vs fide ≈870, so the
  fixed ±64-unit correction clamp is proportionate across variants.
  Tuning tail: aspiration stays at Stage-D values (corr shifts interior
  pruning evals, not root scores); ProbCut skipped per plan (multicut
  covers the territory). `bin/stageI` saved. **Awaiting SPRT verdict**
  (`sprt bin/stageI bin/stageH 100 2000 -5 0`).
- Stage J redefined per user (observability stage dropped): remove the
  cont-hist power-of-two fold. Committed `0af3dd6`: `cont_hist` halves
  are now `dim x dim` with `dim = piece_count * board_size`, indexed
  directly by the (piece, end) key — deleted `CONT_HIST_MAX_DIM`,
  `CONT_HIST_SHIFT`, the `statics.cont_dim` field, the XOR-fold at every
  site, and the `$cont_dim` arg of `score_move!`/`pick_by_score!`.
  Fold was identity for fide/czh/xiangqi (keys already < pow2 dim); only
  shogi (dim 2268 > 2048 cap) had real collisions. Suite vs I (6-run):
  fide 0.909, czh 0.954, shogi 0.921, xiangqi 1.069→1.030 on 10-run
  recheck (batch noise), nodes geomean 0.961 / time 0.980. Memory: fide
  table shrinks (768² vs padded 1024²), shogi grows ~20% (2268² vs
  capped 2048²). `bin/stageJ` saved.
- **Stale-binary incident discovered 2026-07-08** (commit `7468d58`
  fixed the artifacts): `bin/stageH` == `bin/stageI` == `bin/stageJ` ==
  scratch `i2`/`i3`/`j1` — all one file, the stage H build. The session
  shell had `CARGO_TARGET_DIR=target-stages` set from mid-Stage-I
  onward, so builds went to `target-stages/release/` while copies kept
  taking the stale `target/release/anekamacam`. Consequences, verified
  by rebuilding true I (`5bd2ace`) and J (`0af3dd6`) from worktrees and
  racing them against the stale trio (6-run suite, ratios vs trio=H):
  trueH ≈ 1.0 everywhere (trio = H confirmed); **trueI shogi 1.712** —
  the committed Stage I corr-hist explodes shogi; its w=1 capture blend
  "fix" was validated against phantom binaries (i2/i3 were H-vs-H
  noise; only i1's 1.79x full-weight explosion was real). Stage I czh
  0.832 is real. trueJ vs trueI ≈ neutral (shogi 1.02, czh 1.04,
  fide 1.05) — Stage J's published numbers were also noise.
  `bin/stageI`/`bin/stageJ` rebuilt and committed; **Stage I's shogi
  regression is unresolved** — candidate fix: full capture skip (the
  real i2 experiment, never actually run) or revert `5bd2ace`.
- Earlier Stage K attempt (exact table sizes + multiply-shift/modulo
  indexing, reverted uncommitted): its "non-mask index explodes shogi"
  conclusion is **invalid** — every candidate carried the Stage I
  regression while the baseline (stale j1 = H) did not. Worth re-testing
  against a clean baseline if revisited.
- Stage K (renamed from L per user) committed `9c7177d`: flatten
  `piece_list` from `Vec<Vec<Square>>` to one flat `Vec<Square>` with
  `board_size` slots per piece, packed row prefixes, `NO_SQUARE =
  Square::MAX` sentinel tails — and count-sized rows: `piece_squares!`
  slices `row[..piece_count[i]]` (no sentinel scan), `piece_list_push!`
  writes `row[count]` O(1), `piece_list_remove!` swap-removes within the
  occupied prefix, and both macros own the `piece_count` update, so the
  19 standalone `piece_count[..] ±= 1` lines at make/undo/parse sites
  were deleted (pairing audited 1:1 first, including the `!is_unload`
  guards). Sentinel tails kept as hygiene. An interim sentinel-scan
  version (scratch `kflat`) measured neutral vs trueJ (nodes 1.008,
  time 1.024) before the count-sized final form. `bin/stageK` saved;
  `build-stages.sh` gained the stageK entry plus a cksum
  duplicate-binary guard (`4105ecc`). **Corr-hist capture-skip fix
  attempted and dropped per user**: l1 (K + skip) measured shogi 1.751
  vs trueH (trueJ 1.640) — the Stage I shogi explosion is NOT capture
  pollution; root cause open (next lead: read-side `pruning_eval` shift
  misfiring RFP/razor/NMP margins — l3 read-disabled binary built but
  never measured). Stage I shogi regression remains in HEAD.
- Deferred within C: LMP threshold retune, PV/cut-node
  reduction adjustments. Deferred within D: fail-high re-search depth
  reduction. Deferred within F: Part B clusters. Deferred within H: +1
  singular extension (needs SF-style dampening machinery to not explode).
- Workflow per stage: implement → save pre/post binaries in `bin/` →
  node-verify (6-run means) → one commit, no attribution → **pause for user
  SPRT in the TUI console** → continue on verdict.

## Context

AnekaMacam's search NPS is at parity with Fairy-Stockfish (fsf) — startpos
d20 1.54M vs 1.57M. The remaining time-to-depth gap is **tree size**: at
d17 AnekaMacam searched 7.18M nodes vs fsf's 1.62M (~4.4×) before Stages
A–C. Target: pull EBF toward fsf's smooth ~1.6×, matching or beating fsf
time-to-depth on startpos d17/d20 without losing strength (SPRT-gated on
risky stages). Observability (seldepth/currmove/hashfull) is the final
cosmetic stage per user decision.

### The gap, quantified (startpos, original comparison)

| depth | fsf nodes | AM nodes | ratio |
|-------|-----------|----------|-------|
| d10 | 36k | 106k | 2.9× |
| d16 | 652k | 5.27M | 8.1× |
| d17 | 1.62M | 7.18M | 4.4× |
| d20 | 6.16M | 57.3M | 9.3× |

Aspiration spikes at root-flip depths (d15→d16 3.2×, d18→d19 2.6×,
d19→d20 2.4×) are addressed twice per user decision: ordering
stabilization (Stages B/C, done) and a dedicated aspiration retune
(Stage D).

## Constraints (uphold every stage)

- **Variant-agnostic**: no FIDE/dialect tokens; margins derived from average
  piece value in `derive_search_parameters`; tables sized from
  `piece_count * board_size` with byte ceilings.
- **Style**: 80-col, `/* */` comments col 81+ only, consts in `prelude.rs`,
  no comments inside functions, `///` docs, no silenced warnings.
- **No unit tests** — verify via `--release` CLI (debug build broken).
- **Nondeterministic across processes** — verify with multi-run averaging
  (6-run means); don't chase determinism.
- **One commit per stage**, no AI attribution trailer.
- FEN dict round-trips use `dict=None`.
- History updates stay all-node malus (fail-high-only regressed +19-47%).

## Verification protocol (per stage)

1. `cargo build --release`.
2. Bench 4 variants (fide, crazyhouse, shogi, xiangqi), 6-run mean node
   counts. Accept only if geomean ratio < 1.0 and no variant regresses >5%.
3. fsf startpos comparison (`go depth 20`): no checkpoint depth's ttd
   regresses vs pre-stage.
4. `perft fide <limit>` spot-check (always pass the limit arg).
5. SPRT via console (`sprt binA binB <movetime>`, bounds ±2.94,
   h0=0/h1=5) for risky stages: C, D, E, H, I — run by the user at each
   pause.

## Stage D — Aspiration retune + spike re-measure (SPRT)

`search.rs` `iterative_deepening` (~`:320-348`), `parameters.rs:1399`.

1. Re-measure spike depths after B+C — cumulative ladder already looks
   flat (d16→d17 1.17×, d17→d18 1.43×); stage may reduce to a small
   retune.
2. Gentler widening: failing side grows `delta += delta/3` (or `*3/2`)
   instead of doubling (`asp_delta.saturating_mul(2)`); keep the
   non-failing bound. Optionally lower initial `aspiration_delta`
   (`(avg/12).clamp(25,80)`).
3. Optional (SPRT): reduce re-search depth by 1 on 2nd+ successive
   fail-high.

Accept only if spike-depth per-ply ratios stay ~1.6× with no strength loss.

## Stage E — NMP strengthening, variant-safe (SPRT)

`search.rs:930-954`, `parameters.rs`, `prelude.rs`, `state.rs`.
`MIN_NMP_DEPTH=2`; derived `nmp_eval_div=(avg/2).max(1)`;
`reduct = 4 + depth/4 + ((static_eval-beta)/nmp_eval_div).clamp(0,3)`;
replace blanket endgame ban with `(phase != ENDGAME || depth >= 8)` +
deep-endgame zero-window verification search. Keep the material guard
(universal zugzwang defense). Bench + SPRT.

## Stage F — TT eval caching + replacement + pow-2 sizing

`transposition.rs`, `prelude.rs`, `search.rs`. Part A first:

- Store 23-bit static eval in free high bits of `slot[1]` (105-127); widen
  `probe_tt_entry!` to return it; use as `static_eval` and as
  `eval_for_pruning` when the stored bound brackets it. **Fix replacement**:
  delete the `old_score <= $score` clause (shallow entries evict deep).
  Pow-2 sizing + mask indexing for tt/qt/pt.
- Part B: 4-entry clusters, quality-based eviction. Multi-thread stability
  check (threads=4, legal PV).

Prerequisite for H and I; ~+5-10% NPS.

## Stage G — Capture history

`capt_hist` indexed `piece*bs*8 + end*8 + victim_bucket`; added to SEE score
in capture ordering; bonus/malus mirrored on capture cutoffs. Expected
−3-8%; cheap revert if flat.

## Stage H — Singular extensions + multicut + negative extension (needs F; SPRT)

`State.excluded` per ply; trigger after TT probe at `depth>=8` with TT move,
`tt_depth >= depth-3`, non-mate; zero-window `alpha_beta(depth/2,
singular_beta-1, singular_beta)` excluding the TT move; extend / multicut /
negative-extend on the result. If flat: keep multicut + negative-ext, drop
the +1.

## Stage I — Correction history + tuning tail (needs F; SPRT)

1. `corr_hist[2]`, 16384 entries indexed by pawn hash; gravity update at
   node end when `!in_check`, non-mate, bound-consistent;
   `corrected = static_eval + corr/64` where eval_for_pruning is used.
2. Aspiration delta final coordination with Stage D values.
3. ProbCut (optional) — skip if H's multicut leaves <2% headroom.

## Stage J — Cont-hist fold removal (redefined; done `0af3dd6`)

Original observability stage (seldepth, currmove/currmovenumber, hashfull
gating) **dropped per user decision 2026-07-08**. Stage J instead removed
the continuation-history power-of-two fold: direct (piece, end) indexing,
`CONT_HIST_MAX_DIM`/`CONT_HIST_SHIFT`/`statics.cont_dim` deleted. See the
status entry for measurements.

## Sequencing

```
[A ✓] → [B ✓] → [C ✓] → [D ✓] → [E ✓] → [F ✓] → [G ✓] →
[H ✓, round robin pending] → [I ✓, shogi regression open] →
[J ✓] → [K ✓, round robin pending]
```

Combined regression checkpoints after C (done) and after F: full 4-variant
matrix + fsf startpos comparison. Also still pending: rewrite the stale
status section of `plans/time-to-depth-optimization.md`.
