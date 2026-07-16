# Master Plan — Phase 2: Strength (Stages M–U)

> Part 3 of 3 — [01-search-roadmap.md](01-search-roadmap.md)
> (v1 design notes) · [02-time-to-depth.md](02-time-to-depth.md) (phase 1,
> stages A–L, complete).

Continues phase 1's stage letters at **M**. Target: ~1850 FIDE /
~1600 shogi → ~2300, keeping fsf time-to-depth parity.

## Status (2026-07-10)

- Stage M committed `8ec807f`: repetition scoring + material-scaled draw
  bias. `draw_score!` replaces all three `return 0` draw sites (qsearch
  game_over, alpha_beta game_over, stalemate — third site found during
  implementation, not in the original design). In-search two-fold return
  gated on the conf `repetition limit` rule; twelve confs gained the rule
  (limit 4 shogi/mini-shogi sennichite, limit 3 elsewhere), so
  repetition-as-draw semantics stay in conf data. Derived
  `draw_bias = (avg / 8).max(10)`, `DRAW_BIAS_DIV = 16`. Suite bench vs
  pre-M HEAD (6-run MEDSUM nodes): fide +2.2%, czh +2.8%, shogi +12%,
  xiangqi +19% (10-run confirmed) — semantic cost of scoring shuffle
  lines, accepted. SPRT fide 100ms elo0=0 elo1=5: 302W 193L 325D,
  LLR 2.967, **pass**. `bin/stageM` saved.
- Stage N committed `f20be48`: per-leg hopper mobility walk
  (`derive_piece_mobility` rewritten; xiangqi cannon 1675/2551 →
  776/918, fide + shogi params byte-identical). Acceptance ratios landed
  outside the plan's targets (C=R floor structural at f=0; C/H 2.34)
  — shipped at implicit f=1.0 with no ladder const, round robin
  arbitrates. Also: fixed the embedded-param lookup (path prefix made
  every lookup miss), flipped precedence to embedded-first so stage
  binaries are self-contained, and gave SPRT children per-binary temp
  sandboxes with canonicalized paths. Suite bench at parity
  (fide/czh), shogi/xiangqi deltas within known noise.
- Stage O committed `960cd9b`: royal opening PSTs → back-rank gradient
  (fide king +24 rank 0 → −24 rank 7, monotone); `pawn_board`
  maintained through `hash_in_or_out_piece!` on make and
  `pawn_board_in_or_out!` reversals on undo, checked by a new
  release-mode `verify_game_state` assert (UCI `d`); pawn-shield term
  over derived `royal_shield_mask` (`(avg/16).max(8)` per pawn, cap 3);
  shelter literal 10 → derived `(avg/32).max(5)`; castling incentive
  (`has_castled` flag, castled `(avg/12).max(15)` / rights-held
  `(avg/24).max(8)`, gated on the castling rule flag). Verified by
  self-play + `d` walks (fide/shogi/xiangqi, every search tree's
  make/undo balanced) and a 120-FEN sweep per variant (crash set
  identical to stageN). fide self-play castles 6/4 games. Bench at
  parity. Params regenerated (fide/shogi/xiangqi; king opening PST is
  the only block changed); other variant params deleted pending next
  console load. `bin/stageO` saved.
- Stage P committed `da65b17`: precomputed `zone_attack` table
  (royal-square-major layout, `derive_vector_chance` shared with
  piece-value derivation, params byte-identical), quadratic
  `king_danger` (`2*avg`, enemy non-royal non-pawn), `open_shield`
  penalty (`(avg/10).max(12)` via `royal_front_mask`); king-safety
  cluster now skipped in the endgame phase. First cut cost 13–16% nps
  on fide/shogi; enemy-half piece scan + cache transpose + endgame gate
  restored fide/xiangqi/czh parity; shogi +10% nodes / ~10% nps within
  its noise band — round robin arbitrates. `bin/stageP` saved.
- Stage Q committed `2372ed7`: stability-scaled soft deadline
  (`TM_STABILITY_PCT = [160,130,110,100,85,75]`, score-drop ×130%,
  capped at hard deadline; fixed-depth paths untouched, bench parity)
  plus `sprt` clock mode (`tc=base+inc` → wtime/btime/winc/binc,
  referee-banked clocks, flag = loss). Clock smoke test: startpos
  reached depth 13 in 146 ms on a 3000+100 bank. `bin/stageQ` saved.
- **Round robin pending (user)**: stageL/M/N/O/P/Q. No per-stage SPRT
  this round by user direction; results decide reverts. janggi and
  mini-xiangqi (and the other non-UCI variants) still need one console
  load each to rederive+export `latest.param` with the new walk/PSTs.

## Context

User-observed problems and verified root causes:

1. **Engine rarely castles / king marches up board in middlegame.**
   Royal opening PST is the centrality PST sign-flipped (`royal_sign = -1`,
   `parameters.rs:497-514`) — edge-positive but rank-symmetric, so enemy back
   rank scores as well as home back rank. `king_shelter!`
   (`evaluation.rs:34-56`) counts ANY friendly piece adjacent to royal at flat
   10cp (`evaluation.rs:413`) — uncastled e1 king already has max shelter;
   castling gains nothing. No pawn-shield, attacker, open-file, or
   castling-rights term exists.
2. **Repeats moves when winning.** Draws return exactly 0 (`search.rs:552`,
   `:850`); no contempt; repetitions inside search unscored until the conf's
   `repetition_limit` sets `game_over` (`move_list.rs:2602-2626`). Shogi and
   xiangqi confs have no repetition rule → can shuffle forever, no signal.
3. **Xiangqi cannon valued 1675 opening / 2551 endgame vs chariot 704/901**
   (`res/param/xiangqi/latest.param`). Mechanism verified in
   `derive_piece_mobility` (`parameters.rs:282-298`) vs generator semantics
   (`move_list.rs:835-1002`): cannon pattern `cdR-u#-cnR|mR` compiles one
   capture vector per (screen, landing) pair — O(L²) per ray vs rook's O(L) —
   and every vector is weighted `(1-occupancy)^(legs-1)`, treating the screen
   square (which the generator REQUIRES occupied) as if it must be empty.
   Worse: `empty_mobility` (occupancy=0, weight 0.3) gives every hop vector
   full 1.0 — on an empty board a cannon can capture nothing.
4. **Eval gaps vs strong engines:** no mobility, threats, king attack,
   open-file, castling incentive terms. FIDE/shogi piece values are sane
   (P100 N318 B315 R556 Q933; shogi R676 > B417 > G332 ≈ S295) — hopper fix
   must be hopper-scoped.

**Critical constraint (param load path, `game_io.rs:1783-1798`):** when
`res/param/{variant}/latest.param` exists (disk or embedded),
`derive_parameters` is SKIPPED — only `parse_tuned_parameters` +
`derive_search_parameters` run. Therefore: (a) every new derived eval field
must be derived inside `derive_search_parameters` or it is zero for shipped
variants; (b) piece-value/PST changes require param regeneration:
delete `latest.param` → build → load variant once (auto-exports) → rebuild to
re-embed → md5-verify shipped binary.

## Global constraints (every stage)

- Variant-agnostic: no variant tokens in engine code; weights derived from
  geometry / `avg` piece value in `derive_search_parameters`, or per-variant
  data in `latest.param`.
- Style: ≤80-col code, `/* */` at col 81+ only, no comments inside fn bodies,
  `///` docs, consts in `src/prelude.rs`. No `#[test]`.
- Verify per stage: `cargo build --release` → 4-variant suite bench
  (fide, crazyhouse, shogi, xiangqi), 6-run means, seeded → fsf startpos ttd
  spot-check → `perft` spot-check → console SPRT. Save `bin/stageM`… and
  md5-verify provenance before benching.
- One commit per stage, no attribution trailer. Commit/stash the current
  uncommitted diff (`select!` branchless refactor) first.
- Keep: all-node history malus; corr-hist fail-high-only read + capture blend.

## SPRT gates

- Default strength gate: `sprt bin/stageX bin/stageX-1 100 2000 0 5`.
- Behavior-fix stages (M, N): also run the targeted variant session
  (fide AND xiangqi/shogi) with non-regression floor `-5 0`, plus the stage's
  behavioral acceptance test.

---

## Stage M — Repetition scoring + material-scaled contempt (~10–30 ELO)

Fixes problem 2. Decision: material-scaled draw score (no fixed contempt).

**Where:** `search.rs:552`, `:850` (both `game_over` returns) + new
early-return in `alpha_beta`; new `draw_score!` macro in `evaluation.rs`;
derived `draw_bias` in `derive_search_parameters`
(`parameters.rs:1382+`); const `DRAW_BIAS_DIV = 16` in `prelude.rs`;
new `StaticState` field.

**What:**
1. `draw_score!(state)` — draw value from side-to-move perspective:
   `delta = material[stm] - material[opp]` (endgame_material in ENDGAME,
   else opening); return `-(delta / DRAW_BIAS_DIV).clamp(-max, max)` with
   derived `draw_bias = (avg / 8).max(10)` (FIDE ≈ 44cp). Winning
   side avoids draws, losing side seeks them, zero when level. Symmetric,
   variant-agnostic, no engine-identity contempt.
2. Replace both `return 0` sites with `return draw_score!(state)`.
3. First-repetition scoring in search: in `alpha_beta` after the `game_over`
   check, when `ply > 0` and
   `position_hash_map.get(&position_hash) >= 2`, return `draw_score!(state)`.
   Map already includes game history + search path (make_move increments,
   `move_list.rs:2602-2605`). Applies in ALL variants — revisiting a position
   in-search means no progress; mirrors sennichite practice for shogi without
   touching rules. Rule-based `game_over` at conf limit unchanged. Skip
   qsearch in v1.

**Risk:** low. Path-dependent scores reaching TT via parents is standard.
Extra HashMap probe bounded (make_move probes same map).

**Verify:** suite bench (accept ≤2% node/time growth); behavioral: winning
K+R-style position with shuffle-tempting history → PV no longer repeats;
SPRT fide + shogi `[0, 5]`.

---

## Stage N — Hopper piece-value fix + role re-check

~0 ELO fide/shogi (must be byte-identical), transformative for
xiangqi/janggi/mini-xiangqi. Fixes problem 3.

**Where:** `derive_piece_mobility` (`parameters.rs:282-298`); doc update in
`derive_piece_value` (`parameters.rs:204-263`); param regeneration for hopper
variants; log-verify `derive_piece_roles` (`parameters.rs:42-74`).

**What:** replace blanket `(1 - occupancy)^(legs - 1)` with per-leg walk
mirroring generator semantics:

```
for each multi_leg_vector:
    chance = 1.0; hopper = false;
    for each non-final leg:
        if x!(leg) == 0 && y!(leg) == 0 { continue; }      /* marker legs  */
        needs_piece = (c!(leg) || d!(leg)) && !m!(leg);     /* screen       */
        chance *= if needs_piece { occupancy }
                  else           { 1.0 - occupancy };
        hopper |= needs_piece;
    if hopper && final leg capture-only (c && !m):
        chance *= occupancy;                                /* needs target */
    mobility_sum += chance;
```

- Screen legs REQUIRE occupancy; at occupancy=0 hopper captures contribute 0.
- Hopper-scoped target factor covers remaining inflation. Non-hopper pieces
  (incl. FIDE pawn c-only legs) untouched → fide/shogi values byte-identical,
  protecting offset-normalization anchor (`parameters.rs:568-571`).
- **Acceptance gates:** xiangqi `C < R` both phases; `C/H ∈ [1.0, 2.2]`
  opening; fide + shogi `latest.param` byte-identical after regen. If `C ≥ R`
  persists: add one `HOPPER_TARGET_FACTOR` const multiplying hopper capture
  vectors, ladder {1.0, 0.75, 0.5} against the gate. Don't touch non-hopper
  math.
- Fix `derive_piece_roles` doc comment (code takes top 20%, doc says 30%) —
  doc only.

**Param regen:** `rm res/param/{xiangqi,mini-xiangqi,janggi}/latest.param` →
build → load each variant in console (auto-export) → rebuild to re-embed →
md5-check. Diff old/new headers into commit message.

**Risk:** low-medium. Only hopper variants shift (their `avg`-derived margins
move too — desired). No movegen change → perft untouched.

**Verify:** SPRT xiangqi stageN vs stageM `[0, 5]` (expect big win);
SPRT fide non-regression `[-5, 0]` (expect exactly neutral); perft
xiangqi/janggi spot-check.

---

## Stage O — King safety I: royal PST anchor, pawn shield, castling incentive

~20–40 ELO fide-like variants. Fixes problem 1.

**Where:** `derive_pst`/`derive_square_score` (`parameters.rs:395-521`);
`king_shelter!`/`evaluate_position!` (`evaluation.rs:34-56`, `380-466`);
`derive_search_parameters`; make/undo castling branches
(`move_list.rs:2452`, `:3231`); `State`/`Snapshot`/`StaticState`
(`state.rs:243-263`, `501-539`); prelude consts.

**What:**
1. **Royal opening PST anchor** (kills the march): for royal pieces in the
   opening, replace sign-flip with back-rank gradient in white frame:
   `scores[square] = -(square / files) as f64` (rank 0 best, monotonic worse
   forward); existing mean-centering / amplitude-24 / black mirroring apply
   unchanged. Endgame royal PST (centralization) unchanged. Delete
   `royal_sign`. Correct for FIDE and shogi; harmless for palace royals.
2. **Per-side pawn occupancy** `State.pawn_board: [Board; 2]`, maintained
   incrementally at every `pieces_board` update site in make/undo, gated
   `p_is_pawn!` (same sites as `pawn_hash`); init in `refresh_eval_state`.
   Substrate for shield/open-file terms and Stage P.
3. **Pawn shield term**: derive `royal_shield_mask: Vec<Board>`
   (2 × board_size) in `derive_search_parameters` AFTER
   `derive_pawn_parameters` (needs `p_is_pawn!`): squares one rank forward of
   royal square, files ±1 (reuse `adjacency_mask`, `state.rs:419`). Eval,
   opening/middlegame only:
   `count_bits!(mask[color][ksq] & pawn_board[color]).min(3) *
   pawn_shield_bonus`; derived `pawn_shield_bonus = (avg / 16).max(8)`
   (FIDE ≈ 22cp/pawn).
4. **Shelter rescale**: replace literal `10` (`evaluation.rs:413`) with
   derived `king_shelter_bonus = (avg / 32).max(5)` — keeps any-piece shelter
   (load-bearing for shogi castles), scales across variants.
5. **Castling incentive**, gated on `castling!(state)` rule flag
   (`state.rs:38`): `State.has_castled: [bool; 2]` set in `CASTLING_MOVE`
   branch of make_move, saved/restored via new `Snapshot` field. Eval,
   opening/middlegame, per side: `castled_bonus` (derived `(avg/12).max(15)`,
   FIDE ≈ 30cp) if castled, else `castling_rights_bonus` (`(avg/24).max(8)`)
   if side still holds rights (`castling_state` nibble, `state.rs:510`).
   Keeping rights worth something; castling doubles it; burning rights
   without castling forfeits both.

**Param regen:** all variants (PSTs live in `latest.param`), same flow as N.

**Risk:** medium. Multiple eval deltas in one stage — shared theme; per-term
derived consts allow bisection by zeroing fields in a scratch build if SPRT
fails. `pawn_board` maintenance needs 1:1 audit vs `pieces_board` sites
(Stage-K audit precedent).

**Verify:** suite bench (≤2% time); behavioral: 10 self-play fide games via
console, expect castling in most, no mid-board king before endgame phase;
SPRT fide `[0, 5]` (expect clear positive); SPRT shogi non-regression
`[-5, 0]`; make/undo touched → FULL perft suite on fide/shogi/xiangqi/
crazyhouse.

---

## Stage P — King safety II: king-zone attack table + open shield files

~15–40 ELO.

**Where:** new derivation in `derive_search_parameters`; eval integration in
`evaluate_position!`; `StaticState` fields; prelude consts.

**What:**
1. **Precomputed zone-attack table** (zero occupancy walks at eval time):
   `zone_attack: Vec<u8>` indexed `piece * bs * bs + from * bs + ksq`. At
   derive time, walk each vector of `relevant_moves[piece][from]` with the
   Stage-N per-leg chance model at `OPENING_OCCUPANCY`; if landing square in
   `adjacency_mask[ksq] | ksq`, add chance. Store
   `round(chance_sum * 16)` saturating u8. Memory ≤ ~2MB worst case.
2. **Eval term**, opening/middlegame only: per side,
   `units = Σ zone_attack[p][sq][enemy_ksq]` over enemy non-royal, non-pawn
   pieces (piece-list iteration, ≲16 lookups/side);
   `danger = units * units * king_danger_scale / 65536`; derived
   `king_danger_scale = 2 * avg`, ladder {avg, 2avg, 4avg} by suite + SPRT.
   Quadratic in attackers — standard king-attack shape.
3. **Open shield files**: derive `royal_front_mask: Vec<Board>`
   (2 × board_size: own + adjacent files, strictly forward ranks). Penalty
   `open_shield_penalty = (avg / 10).max(12)` when
   `royal_front_mask[color][ksq] & pawn_board[color]` empty,
   opening/middlegame. Generic open-file-toward-royal analog.

**Risk:** medium — first term with real tuning sensitivity. Blocked-slider
overcounting accepted (statistical occupancy in leg model). Cost ~40 adds +
2 mask tests per eval.

**Verify:** suite bench <2% time; SPRT fide and xiangqi `[0, 5]`; shogi
non-regression (drops make king attacks lethal — ladder scale down if shogi
regresses).

---

## Stage Q — Time management: stability-scaled soft deadline

~10–30 ELO at fast TC.

**Where:** `iterative_deepening` (`search.rs:298-471`, soft check at
`:467-470`); `compute_budgets` (`uci.rs:433-459`) unchanged; prelude consts.

**What:** track per-iteration `stability` (same best_move as previous depth →
+1, else reset 0) and `prev_score`.
`budget = soft_deadline - start_time`;
`effective = start_time + budget * TM_STABILITY_PCT[stability.min(5)] / 100`,
`TM_STABILITY_PCT = [160, 130, 110, 100, 85, 75]`; if
`score < prev_score - avg/8`, additionally `* 130 / 100`; always capped at
`hard_deadline`. Root-only, zero per-node cost.

**Required extra work:** movetime SPRT bypasses `compute_budgets` — extend
`run_sprt` (`sprt.rs:503`) with a `tc=base+inc` mode driving
`wtime/btime/winc/binc`; gate acceptance on clock-TC games
(e.g. `go wtime 10000 winc 100`).

**Risk:** low; contained to root loop + SPRT runner.

**Verify:** SPRT at clock TC `[0, 5]`; suite bench unchanged (fixed-depth
paths untouched).

---

## Stage R — Tuning: extend Texel pipeline to new scalar terms

~20–60 ELO per tuned variant.

**Where:** `src/debug/tuning.rs` (`TuneShape`, feature extraction);
`export_tuned_parameters_file`/`parse_tuned_parameters`
(`game_io.rs:334-401`, format versioning); console `datagen`/`tune`
(`console.rs:2906/2935`).

**What:** code stays variant-agnostic; per-variant DATA gets tuned. Append
scalar block to tunable vector: `pawn_shield_bonus`, `king_shelter_bonus`,
`castled_bonus`, `castling_rights_bonus`, `open_shield_penalty`,
`king_danger_scale`, `tempo_bonus`, the five pawn-structure scalars
(`parameters.rs:1363-1367`), two passed-table scale multipliers. All linear
features from White view (danger linear in scale given fixed units — extract
`units²/65536` as coefficient) → existing Adam/MSE pipeline applies
unchanged. Bump param-file format with backward compat: old files leave
scalar block at derived defaults.

Flow per variant (fide, shogi, xiangqi first): `datagen` self-play → `tune` →
SPRT tuned-vs-derived `[0, 5]` → commit regenerated `latest.param` only on
pass. Unseen variants keep pure derivation. Fold consistently repeated
cross-variant ratios back into derivation formulas.

**Risk:** medium; dataset quality bounds result. Re-run after Stage U if U
lands.

---

## Stage S — ProbCut (experiment, ~0–10 ELO)

**Where:** `alpha_beta` after NMP (`search.rs:1022`), before futility;
derived `probcut_margin = avg / 4`; `MIN_PROBCUT_DEPTH = 5` in prelude.

**What:** at non-PV, non-check, non-excluded nodes with
`beta.abs() < MATE_SCORE` and no TT entry proving `tt_score < pc_beta` at
`tt_depth >= depth - 3`: `pc_beta = beta + probcut_margin`; generate captures
into `move_buf[ply]` (regenerated later by main loop — same slot safe); for
up to 3 SEE-winning captures: make → qsearch zero-window
`(-pc_beta, -pc_beta+1)` → on pass, `alpha_beta(depth-4)` zero-window → if
still `>= pc_beta`, return `pc_beta`. Singular multicut overlaps this
territory — cheap experiment: SPRT `[0, 5]`, REVERT on failure, one commit
either way.

## Stage T — Qsearch checks at first q-ply (experiment, ~5–20 ELO if affordable)

**Where:** `quiescence_search` (`search.rs:540`) — add `qdepth: usize`
param (alpha_beta:891 passes 0; recursion passes qdepth+1).

**What:** when `qdepth == 0 && !in_check`, generate all moves; for quiets,
after make_move keep only those where `is_in_check!(state.playing, state)`
(mover gave check), else undo/skip; captures keep SEE/delta pruning. No cheap
variant-agnostic gives-check predicate exists → pays make/undo per quiet at
horizon. Strict gate: SPRT `[0, 5]` positive AND fsf ttd within 5%; else
revert.

## Stage U — Real mobility term (experiment, ~0–50 ELO, high cost risk)

**Where:** `evaluate_position!`; baseline table in
`derive_search_parameters`.

**What:** for non-pawn, non-royal pieces, walk `relevant_moves` against real
occupancy (pass-through legs need empty via `pieces_board`, screen legs need
occupied, final leg counts on empty-with-m or enemy-with-c); score
`(count - baseline[piece][sq]) * avg / 64` where `baseline` is derive-time
expected count at phase occupancy (Stage-N model — only DEVIATION from what
PST already encodes is paid). Mitigations if suite shows >5% time: restrict
to `p_is_big!`; lazy-eval gate (skip when `|plain_eval - alpha| > 2*avg`).
Explicit permission to reject whole stage: PSTs already carry average
mobility; ttd parity is hard constraint. SPRT `[0, 5]` + ttd gate.

## Stage V (deferred, unscheduled) — SMP polish, ponder, seldepth. Out of ELO-per-effort range.

---

## Sequencing

```
[M draws/contempt ~10-30] → [N hopper values, huge xiangqi-family]
→ [O king safety I ~20-40] → [P king safety II ~15-40]
→ [Q time mgmt ~10-30 fast TC] → [R tuning ~20-60/variant]
→ [S probcut] → [T qchecks] → [U mobility]        (experiments, revertable)
```

M, N, Q mutually independent; O precedes P (pawn_board + PST regen); R needs
O+P scalars. Combined regression checkpoints after P and after R: full
4-variant suite + fsf startpos d20 ttd + full perft matrix.

## Critical files

- `src/game/search/parameters.rs` — hopper mobility fix, royal PST anchor,
  all new derived weights/masks (`derive_search_parameters` is the only
  branch-safe derivation site given the param-load path)
- `src/game/position/evaluation.rs` — `draw_score!`, shield/castling/
  zone-attack/open-file terms, shelter rescale
- `src/game/position/search.rs` — repetition return, draw-score returns,
  ProbCut, qsearch qdepth, TM stability loop
- `src/game/moves/move_list.rs` — `pawn_board`/`has_castled` maintenance in
  make/undo (castling branch :2452/:3231, repetition bookkeeping :2602-2626)
- `src/game/representations/state.rs` — new `State`/`StaticState`/`Snapshot`
  fields
- `src/debug/sprt.rs` — clock-TC mode for Stage Q
- `src/debug/tuning.rs`, `src/io/game_io.rs` — Stage R scalar block + format
  versioning

## Verification (end-to-end)

Per stage: build release → 6-run seeded 4-variant suite bench → fsf startpos
`go depth 20` ttd comparison → perft spot-check (full suite when make/undo
touched) → console SPRT per stage gate → save `bin/stageX`, md5-verify.
Behavioral acceptance: M (no repetition when winning), N (xiangqi C < R),
O (castles in self-play, no king march). Update this plan's status table per
verdict, master-plan style.
