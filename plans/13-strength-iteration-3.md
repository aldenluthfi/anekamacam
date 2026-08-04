# Strength Iteration 3 — Structural Speed + Agnostic Eval Simplification

## Context

**Revised 2026-08-04 after RR-3.** A-3..E-3 are done and the termination patch
has landed on all five branches; the first clean RR on those binaries put us
136-172 Elo below Fairy-Stockfish at 1700 in crazyhouse with zero rule
failures, so the loss is pure play strength. Measuring it found three causes
where the plan had one: an effective branching factor of 1.90 against FSF's
1.38 in drop variants — stated unit-free, **our own engine needs 26x more
nodes to reach depth 13 in crazyhouse than in standard, where FSF needs
0.45x** — a time manager that spends 53% of the clock where FSF spends 96%,
and, new, grand: a variant with *no drops at all* sitting at 48x, which is on
its own enough to stop "the deficit is drops" from being the whole story. It
also found that the eval unit anchor this plan has reasoned with since
2026-08-02 is wrong by 2.7x, and that its replacement is unsound too, so
cross-engine unit multipliers are dropped from gates entirely. The ladder past
E-3 is renumbered (F→G … N→Q) to put time management first and to add three
stages; nothing past phaseE-3 has a branch, so the renumbering costs only the
`PHASES` table. Full evidence in **RR-3 diagnosis**, below.

Engine sits at ~1920 FIDE / <1700 shogi; target ~2300. Hard evidence from two
prior iterations:

- **12 search micro-stages (A-L) netted ~0 Elo combined** (RR cluster
  [-19, +1]). Eval + time stages (M-Q) carried all ~100 Elo: repetition
  scoring/contempt +48, king safety I +27, king safety II +21, TM +4.
- **Iteration-2 removal ablations** (unfinished): capture history removal
  costs nothing (worthless); singular family removal +16% nodes (marginal);
  correction history removal +26% nodes (load-bearing, keep).
- **Rejected forever**: leaf mobility walks (55% time), first-ply qsearch
  checks (shogi +16%), fail-high-only malus (+19-47% nodes), actual-army
  phase (shogi +90%), setup-derived occupancy.
- Plans 08-12 flushed variant rules (decisive `game_result`, `Termination`
  in State, N-check/counting/perpetual/goal/extinction/adjudication,
  stand-off restored as move-gen rule) — invalidating iteration-2's plan but
  not its measurements' direction.

Conclusion: remaining Elo lives in (1) structural speed fixes, not
micro-tweaks, and (2) eval quality per unit cost. Less machinery, faster
nodes, more agnostic terms.

User decisions (2026-08-02):
- Board-width work: in scope if feasible → feasibility review REJECTED
  whole-core monomorphization, adopted cheap width swap (see C3) + staged
  movegen as the real lever (D3).
- Castling bonuses: **replace outright** with exposure-derived royal PST.
- Pair bonus: **ablate, drop if neutral**; no battery generalization
  (ownership-only battery unsound; alignment checks = rejected leaf-walk
  cost; quadratic `king_danger` already rewards converging attackers).
- Texel tuning: **excluded** — pure-derivation iteration.

### Baseline facts verified at HEAD `9fb28ee`

- History was re-signed: every hash in plans/ and build-stages.sh is stale.
  Old Stage AC / Phase A-2 baseline `773d04e` == `adf3966` on main
  (patch-id verified). `tools/` scripts deleted; no bin/; no phase branches.
- `U4096` appears in only 3 places (board.rs:21,151 alias + `board!`;
  board_io.rs:103 formatter; prelude re-export). **Movegen does zero
  whole-board ops** — only single-bit `get!/set!/clear!`, which are O(1) on
  any width. The U4096 tax is confined to: 6 whole-board eval sites
  (`king_shelter!`/`pawn_shield!`/`open_shield!` — 514-byte Board copies +
  64-word AND/popcount per leaf), `offence_set` in termination.rs, mask-table
  cache footprint (~520-byte stride; ~5 MB for shogi where ~40 bytes do),
  and `MAX_SQUARES=2048`-sized Zobrist tables (~8 MB).
- Repetition: `position_hash_map: HashMap<u128,u8>` probed every node
  (search.rs:902), maintained in make/undo (move_list.rs:2736-2739,
  2808-2824), deep-cloned per SMP thread (state.rs:698).
- alpha_beta generates the FULL movelist + drops at every node
  (search.rs:1257 → move_list.rs:3697); `generate_all_captures`
  (move_list.rs:3735) and separate `relevant_moves`/`relevant_captures`
  precomputed tables already exist — staged generation is mechanical.
- No `bench` command and no cross-process node reproducibility (Zobrist RNG
  seeded from process start time, prelude.rs:355-358). Perft lives under
  `debug-headless perft <variant> <depth> [--suite|--branch|--limit]`
  (headless.rs:590-653).
- Eval ~3,200-3,500 lines incl. ~675 lines pawn-mask derivation for 7 pawn
  sub-terms; castling scalars gated on rule bit; pair bonus already
  auto-generalized (color-bound reach≈0.5, incremental `pair_score`);
  `mobility_opening/endgame` vestigial (always 0, still parsed/exported);
  `has_castled` is not FEN-serialized (already wrong for FEN-loaded
  midgames — extra reason to delete).

## Iteration 1 phase map (what A..V added — the requested record)

| Stage | Added | Verdict evidence |
|---|---|---|
| A | drop IID, keep IIR | neutral |
| B | 1+2-ply continuation history | best A-L delta (+14) |
| C | aggressive quiet LMR | slight loss (-10) |
| D | gentler aspiration widening (delta+=delta/2) | neutral |
| E | eval-scaled NMP + endgame verification | neutral, big node cut |
| F | TT static-eval cache + pow-2 mask indexing | mild positive (+9) |
| G | capture history | negative (-11); iter-2: worthless |
| H | singular + multicut + negative extension | worst rank (-19) |
| I | pawn-hash correction history | mild positive (+9) |
| J | direct cont-hist indexing | neutral simplification |
| K | flat piece lists | neutral speed |
| L | corr-hist read gated to fail-high pruning | correctness fix |
| M | repetition scoring + material draw bias | +48 — largest gain |
| N | per-leg hopper mobility derivation | correctness (xiangqi values) |
| O | royal back-rank PST, pawn shield, castling incentive | +27 |
| P | zone-attack king danger, open-shield penalty | +21 |
| Q | stability-scaled soft deadline | +4 |
| R | SPRT hardening, role fix, TT/PV fast path, pair cache | ~0 (infra) |
| S | check-extension cap (root+2) | provisional keep |
| T | scalar-tail param schema + PST layout fix | infra |
| U | ProbCut | standard leader in (contaminated) RR |
| V | sqrt/sqrt quiet LMR | regression, reverted in W |
| W-AC | legality/protocol/terminal fixes + fixtures | correctness base |

Static eval today: incremental material + tapered PST (4-phase interp),
tempo, major/minor imbalance, color-bound pair bonus, king shelter
(adjacency), pawn shield (`royal_shield_mask`), castling incentive,
quadratic zone-attack king danger, open-shield-file penalty, 7 pawn
sub-terms (passed/protected/chained/connected/doubled/isolated/backward)
cached in PTable, material-scaled draw contempt.

## Stage ladder (iteration 3 — phase labels A-3..Q-3)

One significant, RR-differentiable change per stage; one commit per stage on
its own `phaseX-3` branch via build-stages.sh. Rejected experiments consume
no letter.

### A-3 — baseline + measurement harness — DONE (as-built)

- `SEED` lazy static (prelude.rs): `ANEKAMACAM_SEED` env parsed as u64,
  unset → time-based; `RNG` seeds from it, bench walks read it too.
- `debug-headless bench <variant> <depth> [--limit n]` (headless.rs):
  live suite FENs (perft-1 > 0 — leading suite rows are terminal
  regression cases) padded to the limit with deterministic random-walk
  positions from startpos (fixed LCG over the SORTED formatted legal
  moves, so walks depend on the legal-move SET, not generation order —
  D-3's reordering keeps positions identical). Suites outside standard
  are tiny (shogi 4 / crazyhouse 1 / xiangqi 11 / grand 1 live), so
  walks carry most of the diversity. Fresh 16 MB TT per position,
  single thread; per-position `position NNN nodes .. time_ns .. nps ..`
  lines + `total nodes .. time_ns .. nps ..`. `state` command now also
  prints `Hash:` (cross-process Zobrist verification).
- **Determinism fix (root cause, unplanned)**: cross-process node counts
  varied ±8% at pinned seed because move-vector compilation
  (move_parse.rs) collected through `HashSet` — per-process hasher →
  random compiled-vector order → random movegen order. All 11
  order-leaking sites rewritten to Vec accumulation +
  `remove_duplicates_in_place` (first-seen order); membership-only sets
  kept. Perft suites all green after (set identity preserved).
- `tools/speed-suite.sh`: args `BIN_A [BIN_B]`; env `VARIANTS`, `DEPTHS`
  ("name=depth" overrides; defaults standard=11 xiangqi=9 grand=9 else
  8), `PROCS` (10 interleaved passes), `LIMIT` (16), `SEED` (42).
  Geomean nodes/time/nps per variant + B-vs-A deltas; fails loudly when
  same-binary node counts differ across passes.
- build-stages.sh: `PHASES` originally A-3..N-3, one row per stage (the
  F-3a/b/c sub-ablation rows are gone with the sub-stages); `phaseA-3`
  pinned via its own branch (created at the A-3 commit — hash unknowable at
  commit time), `PHASE_N_PARENT` overrides a conditional stage's parent for
  when that stage is dropped. round-robin.sh: `*-3*` patterns, VARIANTS
  defaults to the four campaign variants. **The `PHASES` table is rewritten
  for the 2026-08-04 renumbering** (A-3..Q-3, with the parent override
  generalised to the three conditional stages L-3, M-3 and P-3); that edit
  lands with F-3, since no branch past phaseE-3 exists.
- Verified: perft suites green (standard depth-4 x400 + 13 variants full
  depth-4); same-seed bench node counts IDENTICAL cross-process on all
  five campaign variants; A/A speed-suite delta ±0.8%.

### B-3 — lean search (delete proven-worthless machinery) — DONE

As-built notes: executed exactly as specified below; also un-gated the
beta-cutoff corr-hist/TT stores and dropped the singular paragraph from
the alpha_beta doc block + the `probe_tt_entry!` doc's singular mention
(now cites ProbCut's TT-refutation guard). Node gate vs A-3 baseline
(seed 42, limit 6, standard d9 / others d8): standard -1.0%, shogi
-0.8%, crazyhouse -17.3%, xiangqi -1.8%, grand +11.6% — all within the
+16% cap.

Delete **capture history**: `State.capt_hist` (state.rs:646, clone :704,
from_statics :952-954, reset :1039-1040), clear_search realloc
(search.rs:158-159), gravity-update branches (search.rs:1297-1298,
1497-1501, 1533-1537, 1576-1580), `capt_hist_index!` + `+ capt` term in
`score_move!` (move_ordering.rs:335-380), `CAPT_HIST_BUCKETS`
(prelude.rs:565), `StaticState.capt_hist_div` (state.rs:524) + derivation
(parameters.rs:1547-1548).

Delete **singular/multicut/negative-extension**: search.rs block
1183-1218, `excluded_here` :953 + TT-cutoff gate :962, ProbCut gate :1086,
move-loop exclusion :1279-1285, `ext_depth` first-move branch :1452-1457
(→ plain `depth - 1`), `if excluded_here { return alpha }` :1613-1615,
un-gate corr-hist/TT stores :1503-1512. `State.excluded` (state.rs:653 +
clone/from_statics/reset/clear_search), `singular_margin` (state.rs:525,
parameters.rs:1550-1551), `MIN_SINGULAR_DEPTH`, `SINGULAR_TT_DEPTH_SLACK`
(prelude.rs:254-255).

Keep: corr hist, cont hist, killers, ProbCut (`probe_tt_entry!` tuple
unchanged — ProbCut reads depth/flags), NMP, all pruning.

Verify: bench node counts per variant (cap: ≤ +16% from singular removal,
~0 from capture history); perft suites as insurance; RR arbitrates.

### C-3 — hot-path structural speed (repetition overhaul + board width) — DONE

As-built deltas from the design below:

- **No `hashes` Vec.** `Snapshot.position_hash` already stores exactly the
  pre-move hash the design wanted to mirror, so the scan reads
  `state.history` directly and `Repetition` carries only `clock: u16`.
  One less allocation per position, nothing to keep index-aligned across
  null moves. `Snapshot` gained `repetition_clock: u16` (plan-12 pattern).
- `has_repetition`/`count_repetitions` take a `cap` argument and derive the
  bound through a private `repetition_scan_bound` (full history when
  `drops!`, else the clock, then capped). Search passes `REP_SCAN_CAP`,
  game-truth callers pass `usize::MAX`.
- The clock resets on any non-quiet move type and on promotions; a pawn
  push only inflates the bound, which is safe (the bound must never be too
  small — extra entries simply cannot match the current hash).
- `has_repetition` returns false when no `repetition` rule is declared, so
  the old `is_some()` gate at the search probe is now internal.
- Restored `tools/run_endgame_fixtures.sh` + `tools/endgame_fixtures.txt`
  (deleted along with `tools/` when history was re-signed). Two sittuyin
  counting fixtures were already broken before this stage: they used `C`
  for the chariot (sittuyin uses `R`) and omitted the clock fields the
  variant's FEN dict regex requires, so the FEN was rejected and the case
  silently ran from startpos. Fixed to `R` + ` 0 1`; verified they fail
  identically on phaseB-3, so this is a fixture repair, not a behavior fix.

Verified: 32/32 endgame fixtures (all repetition, perpetual-check,
perpetual-chase, and sennichite cases) at both widths; FULL standard perft
suite 27008/27008 plus all 13 other variant suites at depth 4 (U256), and
a 300-position standard subset plus every drop/large-board suite under
`--features wide-board`; debug-build perft with `verify_game_state`
assertions green; cross-width eval equality on 41 standard FENs + 24
shogi/xiangqi/janggi/crazyhouse/grand FENs + 10 startpos (0 mismatches).

Speed vs phaseB-3 (seed 42, geomean of interleaved passes): shogi +5.8%,
crazyhouse +5.9%, xiangqi +6.2%, grand +4.9% NPS; standard +1.0% NPS /
-5.1% time at depth 10 (an initial -1.2% at depth 9 was pass noise).
Node counts shift both ways — Zobrist tables changed size with
`MAX_SQUARES`, so tree shape is not comparable across this stage.

**Repetition without HashMap.** Design constraints discovered in review:
there is NO unconditional halfmove clock (`Snapshot.halfmove_clock` mirrors
the optional `Counter` rule only), and in drop variants an
irreversible-move bound is UNSOUND (capture→drop cycles restore identical
positions incl. hands) — so the scan bound splits on `drops!`.

Ownership follows plan-12 conventions: rule progress lives in the
`Repetition` rule object inside `state.termination`, NOT loose State
fields (same pattern as `Counter.clock`, `Counting.progress`,
`Checks.delivered`).

- `Repetition` (termination.rs) gains `hashes: Vec<u128>` (entry i =
  pre-move hash of move i, mirrors `Snapshot.position_hash`) and
  `clock: u16` (plies since last capture/drop/promotion/castling).
  Maintained in make/undo ONLY when the rule is declared (present-flag
  discipline; parse already rejects `perpetual` without `repetition`, so
  perpetual variants are covered). `reset_progress` clears both.
- make_move!: drop map insert; push `hashes`; reset/increment `clock`.
  undo_move!: drop map ops (move_list.rs:2808-2824); pop; restore clock.
  `Snapshot` keeps the prior `clock` scalar (plan-12 pattern); the
  `hashes` push/pop is self-reversing, no snapshot field. Null moves
  push/pop too (keeps indices aligned).
- termination.rs helpers: `has_repetition(state, bound)` (backward scan,
  early-exit; scan every entry — no parity stepping, null moves break
  parity and side-in-hash makes wrong-side entries never match) and
  `count_repetitions(state, bound)`. Bounds: non-drop variants
  `min(clock, len)` exact; drop variants full `len` for game truth,
  new `REP_SCAN_CAP = 64` (prelude) for the per-node search probe.
- search.rs:899-906 → `has_repetition` probe returning `draw_score!`,
  gated on `state.termination.repetition.is_some()` (equivalent to
  today's count>=2 for declared variants). Behavior delta: variants with
  NO repetition rule lose the in-search twofold cutoff — acceptable,
  they define no repetition semantics; record in bench notes.
  termination.rs `repetition_outcome` (:675-677) →
  `count_repetitions`. graphics.rs:550-560 → same. Delete
  `State.position_hash_map` (state.rs:640, :698, :945, :1031). Side
  effect: root occurrence now counted correctly (map missed it;
  graphics.rs:554 `.unwrap_or(1)` papered over it).

**Board width swap** (~25 lines): board.rs
`type BoardBits = U256` default / `U4096` behind new `wide-board` cargo
feature; `Board = (u8, u8, BoardBits)`; `board!` uses `BoardBits::MIN`;
prelude exports; `MAX_SQUARES` cfg-conditional 256/2048 (shrinks Zobrist
tables automatically); board_io.rs:103 takes `&BoardBits`; config-load
guard `board_size <= MAX_SQUARES` with "rebuild with wide-board" message.
Largest shipped board is grand 10x10=100 — all fit U256 (headroom 16x16).

Expected: 5-15% NPS combined, most in drop variants (highest make/undo
rate + biggest mask tables). Verify: FULL perft suites both widths
(make/undo touched — perft-critical stage); cross-width eval equality on
sample FENs; sennichite fixture with a capture-containing repetition cycle
(the subtle failure = silently under-detecting shogi repetition);
perpetual-check fixture via `d`/state reason; bench (counts shift slightly
from root-occurrence fix — record, don't gate on identity).

### D-3 — staged move generation (the real movegen lever) — DONE

Two design premises below turned out to be wrong; both were settled by
measurement.

**1. The node-count identity gate is unachievable in principle.** Move
scores are computed lazily inside `pick_by_score!`, and the pick at index
0 scans the whole list — so today every quiet's history score is computed
at node entry, before any child search runs. Staged generation creates the
quiets only after some captures have been searched, and those subtrees have
already updated `search_hist`/`cont_hist`, so the quiets are scored against
fresher history and reorder relative to each other. Deferring generation
necessarily defers scoring; keeping identity would mean pre-scoring the
quiets at node entry, which means generating them there. Move-set equality
was proven instead: forcing stage 1 to derive its captures from the full
`relevant_moves` walk rather than `relevant_captures` produced byte-
identical node counts on all five variants, so the capture halves match
exactly, and the quiet half is that walk's exact complement by
construction.

**2. Staging is a net loss without hands.** Because the tables are not
disjoint, stage 2 re-walks and re-encodes the captures only to discard
them, so every node that reaches quiets pays the capture work twice.
Measured with staging forced on everywhere (vs C-3): standard -7.4% NPS at
identical node counts, xiangqi +20% time, grand +21% time; only shogi
(-19% time) and crazyhouse (-11% time) won, because skipping the drop list
on a capture cutoff dwarfs the duplicated walk. Staging is therefore gated
on `drops!(state) || game_phase == SETUP` — a cost model ("defer only when
the deferred list is large"), expressed with a rule bit movegen already
uses, not a variant name. Non-drop variants take the original single-stage
path unchanged.

As-built:

- `retain_captures!(out, start, keep)` in move_list.rs — stable in-place
  compaction to one side of the capture split. Replaces the `swap_remove`
  loop in `generate_capture_list!`, which scrambled the surviving captures'
  order. Effect measured alone (vs C-3): standard identical, xiangqi
  -0.03%, shogi +0.09%, crazyhouse +2.9%, grand -14.1% nodes — a qsearch
  tie-break lottery, net neutral in expectation, kept because stable order
  is what makes the capture stage a subsequence of the full walk.
- `generate_all_quiets_and_drops` — appends rather than clears, so the
  unsearched losing captures survive into stage 2.
- alpha_beta's move loop became an index `loop` (every `continue` now
  advances `i` explicitly). Stage 2 is injected when the buffer runs out,
  or when a pick scores below `LOSING_CAPTURE_SCORE` — the point where only
  losing captures remain, since the bands are disjoint (winning captures
  >= 4M, killers/quiets >= 1M, losing captures < 1M - MAX_HIST). Injection
  re-picks at the same index so the merged tail is ordered correctly.
- A TT move that is not among the captures (typically a quiet one) forces
  stage 2 immediately, so it is still the first move tried.

Result vs C-3 (geomean, seed 42): shogi -18.1% time / +7.9% NPS,
crazyhouse -10.8% time (nodes -17%), standard and xiangqi within noise
(staging bypassed), grand +6.0% time from the stable-filter node lottery.
Verified: 32/32 fixtures, perft green on 8 variants incl. every drop
variant, debug-build searches on shogi/crazyhouse/minishogi/janggi with
`verify_game_state` assertions active.

Original design follows.

alpha_beta currently pays full move+drop generation at every node even
when the first capture cuts off; drop variants pay hundreds of encoded
drop moves per node. Restructure:

- Table caveat: `relevant_captures` ⊆ `relevant_moves` — the tables are
  NOT disjoint. Dual-purpose `m|c` vectors appear in both; the
  quiet/capture split happens at runtime by destination occupancy, not
  by table. So the stage-2 generator walks `relevant_moves` + drops +
  castling and emits ONLY quiet moves (empty destination); dual vectors
  get walked in both stages. The saving on stage-1 cutoff is the skipped
  quiet/drop move ENCODING (the dominant cost, esp. drop lists), not
  skipped vector walks.
- New `generate_all_quiets_and_drops` counterpart in move_list.rs (~100
  lines).
- alpha_beta loop: stage 1 generates captures only; stage 2 generates
  quiets+drops+castling only if no cutoff yet.
- **Ordering-preservation requirement** (found in review; agent's naive
  "captures first" is wrong): current bands interleave — winning captures
  (4M) → killers (1M+7·MAX_HIST) → quiets (1M+3·MAX_HIST) → losing
  captures (1M−MAX_HIST). Stage 1 must iterate WINNING captures only;
  losing captures are deferred into stage 2's buffer so the merged order
  matches today's bands exactly.
- Gate: fixed-depth node counts must be UNCHANGED vs C-3 (same tree, same
  ordering) while NPS rises. Any node delta = ordering bug, fix or revert.
- Expected: largest gain in shogi/crazyhouse — precisely the weakest
  variants. Qsearch already capture-only; ProbCut unchanged.

### RR-1 findings and correctness fixes (2026-08-02, after D-3)

First RR exposed three bugs and invalidated part of the campaign. All three
are committed on main and cherry-picked onto phaseA-3..phaseD-3, so every
phase binary shares one rule set; the pre-fix drop-variant RR numbers are
void and must be re-run.

Data provenance: shogi figures from `rr/rr-shogi.pgn` (1079 games, the run
that aborted on an engine-init failure); crazyhouse figures from the full
`~/rr/rr-crazyhouse.pgn` (**2987 games**). The king-danger table below comes
from `debug-headless evaluate` on constructed positions, not from any game
file.

**Crazyhouse deficit, decomposed (2987 games, pre-fix binaries).** The
promoted-piece bug is separable from the eval hole because its trigger is
identifiable: a game is corrupted exactly when someone captures a promoted
piece, after which our pocket diverges from the arbiter's.

| | forfeits | claiming >= +10 six moves from death | sign agreement, \|fsf\|>=10 |
|---|---|---|---|
| all games | 466 (15.6%) | 22.4% | 68.9% |
| clean (no promoted capture, 66%) | — | 17.2% | 70.6% |
| corrupted (34% of games) | — | 31.8% | 65.8% |
| standard, for scale | 0 | 0.7% | 94.3% |

Reading: the bug cost **466 games outright** (15.6% of the run; 95-131
forfeits per phase binary out of ~843 games each) and roughly **doubled**
eval blindness where it fired (17.2% → 31.8%). But the clean subset is
still nowhere near standard — 17.2% blindness against 0.7%, 70.6% sign
agreement against 94.3% — so a large eval hole remains after the fix.

Search is exonerated: mean depth ours 17.3-18.1 ply vs FSF 17.8-18.7. We
see nearly as deep and still lose, which is why every remaining stage in
this iteration is eval work.

- **Crazyhouse promoted pieces** (`49bf8d4`). crazyhouse.conf promoted
  straight to `R/N/B/Q`, so a queen-from-pawn was indistinguishable from a
  real queen and `piece_demotion_map` — which the engine already applies on
  every capture — had nothing to demote. The pocket then diverged from the
  arbiter's and the engine eventually dropped a piece it did not own:
  **65 games forfeited**, every illegal move a drop. Fixed with the shogi
  pattern: distinct `Tt/Uu/Vv/Ww` types, `= demotions =` back to `P`, dict
  rules rendering them as `R~/N~/B~/Q~` (the lowercase `w` needs shogi's
  anchored rule, since it collides with the side-to-move field). Verified by
  replaying 121 real games against python-chess and comparing board, side,
  castling, pocket and ep: **53 of 121 diverged before the fix, 1 after** —
  and that one differs only in the ep field (see below), so no pocket
  disagreement remains.
- **Dropped pieces never marked unmoved** (`ac38125`). A pawn dropped on its
  home rank could not double-step. Gated on `initial_setup`, so only drops
  onto a piece's own starting squares count as unmoved. Differential perft
  against Fairy-Stockfish now matches exactly on all probe positions.
- **`pv_length` reset after the terminal return** (`9906c8d`). Behaviour
  fix only; never reproduced a failure from it.

**Not a bug — do not "fix" it.** Our FEN records an en-passant square after
every double push; the reference prints one only when the capture is legal
(game 15, ply 54, `b7b5`: white Ka4/Pa5 pinned by black Ra6, so `a5xb6`
e.p. self-checks). `make_move!` already rejects self-check and ep captures
are gated on our own ep square, so this cannot change a move the engine
plays. Its only cost is hash canonicality. Leave the hot path alone.

**Eval evidence for the drop-variant deficit.** `king_danger!`
(evaluation.rs:196) iterates `piece_squares!` — board pieces only — so
enemy pieces in hand contribute zero danger. Measured in crazyhouse with a
sheltered white king (g1 behind f2/g2/h2) versus an exposed one (d4), the
rest of the position held fixed at
`r2qk2r/pppppppp/8/8/8/8/PPPPPPPP/R2Q…`, varying only Black's hand:

| black hand | sheltered | exposed | delta |
|---|---|---|---|
| `-/-` | -648 | -668 | +20 |
| `-/rb` | -1688 | -1708 | +20 |
| `-/qrb` | -2798 | -2819 | +21 |
| `-/qqrrbb` | -4949 | -4969 | +20 |

The delta is constant: a hand holding two queens, two rooks and two bishops
makes the exposed king exactly as unsafe as an empty hand does, namely not
at all. This is the drop-variant hole, and it is why shogi and crazyhouse
trail standard while searching to comparable depth (crazyhouse: ours
16.9-18.2 ply vs FSF 17.3-18.4).

**Correction to the first version of this table.** It was measured on a
bare `4k3/8/8/8/8/8/5PPP/6K1` position, which the engine classifies as
ENDGAME — and `evaluate_position!` zeroes the whole `king_safety` block,
`king_danger!` included, in that phase. Those numbers therefore measured
phase gating, not the hand omission, and the claim they supported was
unearned. The table above is remeasured in MIDDLEGAME, where the term is
live.

**Phase gating investigated and dismissed.** `game_phase_score!`
(state.rs:427) counts `piece_count`, which is board-only, while
`opening_material`/`endgame_material` include held pieces in drop variants
— so a hand full of majors does not hold the phase up. Constructed
positions do reach ENDGAME with every big piece in hand, silently disabling
king safety. But it does not fire in real play: three self-played shogi
games taken to move 48/55/101, including one with a hand of
`PPPPPPPLLNNNNSSS`, all still evaluate as MIDDLEGAME. Not worth a stage
until a real game shows otherwise.

**Consequence for ordering: the eval block was renumbered.** Hand-drop king
danger is now **E-3** and runs first, since it is the only stage addressing
the deficit this data identifies. The exposure PST is **F-3**; the pawn
collapse, which addresses none of the measured failures, drops to **G-3**
as a simplification stage; schema shrink becomes **H-3**.

**Fix verified against the failures it was built for.** All **466** games
that were forfeited by an illegal drop replay through the fixed binary with
an exact board, side-to-move and pocket match against python-chess — 466/466,
zero mismatches. The phantom piece those games dropped no longer exists in
the engine's pocket.

### E-3 — hand-drop king danger — DONE

As-built: exactly the design below. `zone_attack_best` is reduced inside
`derive_zone_attack` rather than in its own pass, which removes the
ordering hazard entirely — neither table can exist without the other, on
either load path. The eval term sits inside `king_danger!`'s existing
enemy-piece loop (no new loop) behind a `drops!` test, so non-drop variants
execute nothing.

**Gate met, measured against Fairy-Stockfish.** Full-army crazyhouse,
white king castled g1 versus exposed d4, material identical, varying only
Black's hand. Our units are not centipawns, so a standard-variant row has to
anchor the scale — and **the anchor used here was wrong**. It came from one
king-exposure position and read 1.98 u/cp; measured on 2026-08-04 against
clean material imbalances in standard the ratio is **0.74 u/cp** (see RR-3).
Both columns are shown, the corrected one being the one to use:

| hand | FSF | at 1.98 (wrong) | at 0.74 (correct) | pre-E-3 | E-3 | vs FSF |
|---|---|---|---|---|---|---|
| `-/-` | 438 | 867 | 324 | 126 | 126 | **39%** |
| `-/rb` | 1954 | 3869 | 1446 | 126 | 220 | **15%** |
| `-/qrb` | 2869 | 5681 | 2123 | 126 | 503 | **24%** |
| `-/qqrrbb` | 3715 | 7356 | 2749 | 126 | 1632 | **59%** |

E-3 does what it was scoped to do: the penalty was flat at 126 for every
hand and now scales, growing 13x from empty to `qqrrbb` where FSF grows
8.5x. It does not close crazyhouse king safety, but the remaining gap is
15-59%, not the 6-22% first recorded — a real shortfall rather than a
chasm, and the empty-hand row is 126 against 324 rather than against 867.
That row is untouched by this stage, because the gap is in the base
exposure signal, not the hand; it is the exposure PST's target.

**Superseded as a priority, first by the RR-2 diagnosis below and then more
sharply by RR-3.** E-3 stays — the term was missing and is now correct — but
the eval block it was the first stage of sits behind the search block,
because the drop-variant deficit is dominated by branching factor, not eval.

Speed (seed 42, limit 12): standard and grand byte-identical node counts
and NPS within 0.3%, confirming the `drops!` gate costs nothing. Shogi
-7.0% NPS with +39% nodes and crazyhouse -2.3% NPS with -2.2% nodes; node
counts are not comparable across an eval change, and the NPS cost is the
real per-type multiply-add the term adds. Fixtures 32/32.

Original design follows.

The measurement
above shows enemy hand material contributes zero king danger, which is the
single largest identified deficit in the two weakest variants. `king_danger!`
accumulates `zone_attack[(royal * piece_count + t) * board_size + from]`
over each enemy piece's occupied squares; a piece in hand has no square, so
it is skipped entirely.

Fix: give a held piece the square it would choose. Derive, once per
variant, `zone_attack_best[royal * piece_count + t]` = max over `from` of
`zone_attack[(royal * piece_count + t) * board_size + from]` — a reduction
of a table that already exists, so no new hot-path scan. Then add, per
royal:

```
hand_units = Σ over enemy piece type t (non-royal)
             piece_in_hand[enemy][t] * zone_attack_best[royal * pc + t]
```

folded into the existing `units` total so the quadratic `king_danger_scale`
applies to board and hand pressure together. Cost is one loop over piece
types per royal, only when a hand is non-empty; variants without hands add
nothing and pay a single emptiness test.

Deliberately ignores drop legality and occupancy (a held piece cannot land
on an occupied square, and some variants forbid zones): the term is a
danger estimate, and over-estimating a held queen's reach is the correct
direction. Guard: `zone_attack_best` must be derived after `zone_attack`,
same ordering constraint as the PST work below.

Verify: rerun the table above — the sheltered/exposed delta must grow with
the enemy hand instead of staying at +16, and the sign must invert so the
sheltered king scores higher. Then bench (drop variants only should move),
and SPRT shogi and crazyhouse against phaseD-3.

### RR-2 diagnosis: the drop deficit is search, not evaluation — direction right, magnitude wrong (see RR-3)

Prompted by the question "can the abysmal crazyhouse and shogi performance
be caused by something other than king safety?". It can, and it is.

**The measurement.** Nodes to reach depth 9, single thread, **both engines
at `Hash=16`**, ours over UCI/USI and FSF over UCI:

| position | FSF | ours | ratio |
|---|---|---|---|
| startpos as chess | 18,080 | 9,663 | **0.5** |
| startpos as crazyhouse — identical board, empty pockets | 11,023 | 47,538 | **4.3** |
| crazyhouse midgame, N+n in hand | 8,971 | 52,130 | **5.8** |
| shogi startpos | 6,324 | 37,146 | **5.9** |

We need **half** FSF's nodes in standard and **4-6x** its nodes in the drop
variants — a 9-12x relative swing. Rows 1 and 2 are the same board at the
same depth with only the rule set changing.

**Hash sizes must match or the table lies.** The first version of this
table used `debug-headless search`, which hard-codes `TTable::with_mb(1)`
(headless.rs:470), against FSF's 16 MB default, and reported ratios up to
16.5x. Matching the hash cut shogi from 11.2x to 5.9x. Always drive both
engines through their protocol with an explicit `Hash` option; do not use
`debug-headless search` for cross-engine comparison, and do not "fix" it to
16 MB either, since that would move the A-3 baseline.

**Four mechanisms, all verified in the code.**

1. **Drop moves read a history index that belongs to another move.**
   `drop_list.rs:141-144` writes the drop square into `start` and
   `can_checkmate` into bit 23 — bit 0 of the `end` field
   (`moves.rs:714-719`). So `end!(drop)` is 0 or 1, never a square. Three
   consumers use it: `score_move!`'s `idx`/`cont_key`
   (`move_ordering.rs:312-318`), the move loop's `hist_idx`/`cont_key`
   (`search.rs:1273-1277`, which also drives the LMR history adjustment at
   `:1377-1388`), and — the one that hurts most — `cont_bases`
   (`search.rs:1192-1196`), which keys the continuation table on
   `end!(previous)`. When the *parent* is a drop, every child at that node,
   including ordinary quiet moves, collapses onto 2 of `P*B` keys. That is
   why crazyhouse with empty pockets is already 4.3x.
2. **Drops never write history.** Every update is gated on
   `is_quiet = m_quiet!(mv)` (`search.rs:1282`), and `m_quiet!` requires
   `QUIET_MOVE` (`moves.rs:388-392`). Sites `search.rs:1465`, `:1497`,
   `:1534`. Drops read history and never learn.
3. **Drops are exempt from both move-count prunings.** `&& !is_drop` in
   futility (`search.rs:1294-1300`) and LMP (`search.rs:1328-1336`).
   Measured drop share of the legal move list: crazyhouse with N+Q in hand
   **68 of 99 (69%)**, shogi with P+N+S+G **116 of 146 (79%)**. Most of the
   tree is exempt from the rules that bound it. They are also on the
   *capture* LMR curve (`search.rs:1759-1764`), which reduces about half as
   much as the quiet curve.
4. **Qsearch has no drops except check evasions.** Not in check calls
   `generate_all_captures` (`search.rs:649-657`), which never emits drops.
   Drop-checks and drop-mates — the characteristic tactic of both variants
   — are invisible at the horizon.

**Also true, eval-side, and smaller.** Held pieces are priced identically
to placed ones. FSF puts a held piece 54-154 cp *above* the same piece on
the board and compresses held values toward the pawn: FSF P:N:B:R:Q =
1 : 1.84 : 1.68 : 2.27 : 3.32 against our 1 : 3.19 : 3.63 : 6.70 : 11.1.
We undervalue held pawns and overvalue held majors by roughly 2x in ratio.
Hands also contribute nothing to `phase_score`, PST, pair bonus or
imbalance.

**Consequence for the ladder.** The eval block runs *after* a five-stage
search block, not before it, and is renumbered K-3..N-3. The search block's
gate is the node table above; the eval block keeps its FSF-anchored eval
gates. They must not interleave: the phase-reference stage moves drop
variants from MIDDLEGAME to OPENING, which selects the *larger* opening
futility margins (`parameters.rs:1490-1494`) and so pushes node counts the
wrong way — its own gate is standard-bench-identity, independent of this
table.

### Termination correctness patch — landed after E-3, no letter — DONE

*(Written when the next stage was called F-3; that stage is G-3 after the
2026-08-04 renumbering. Every "F-3 interaction" below means G-3.)*

**Context.** A shogi RR lost four games to "makes an illegal move" and logged
1271 illegal-PV warnings. The cause was real — search scored every repetition
a draw and never read the `perpetual` rule, so the engine steered into a
perpetual check that Cute Chess rejects outright — but the two patches
attempted on 2026-08-03 were each incomplete, and each was caught by the user
rather than by me. Auditing them turned up a wider set of defects in the same
subsystem: twenty in all, of which the illegal moves are one.

The intended outcome is that the engine and its declared rule set stop
disagreeing. Concretely: no arbiter can reject a move we choose; every
terminal rule names a subject that is documented and computed rather than
inferred from ply parity; and a rule that a `.conf` declares is the rule the
engine actually plays. This section replaces both attempts: **one commit,
everything below, nothing shipped piecemeal.**

Both attempted commits (`7c4a04a`, `3bd2998`) are local-only — `origin/main`
is at `24f74be` — so they are reset and rewritten, not amended. The RR-script
commit is re-made separately since it shares no code with this.

#### Decisions taken (user, 2026-08-03)

- **Subject convention is per-rule, documented on each rule.** No global
  convention is achievable: `extinct` names the colour whose count ran out,
  `adjudicate` names the points winner, and `perpetual` names the sole
  offender — three colours computed from position data, not from turn order.
  `Outcome`'s doc must stop claiming one. No config outcome word changes and
  no shipped variant changes behaviour.
- **Landing:** one clean commit on `main`, cherry-picked onto
  `phaseA-3..phaseE-3` so every binary shares one rule set. Binaries rebuild;
  the drop-variant RR re-runs.
- **Scope:** code *and* configs — a `.conf` that misstates its variant's real
  rules is fixed in the same patch, each change carrying a cited source.
- **Freshly-loaded FENs** evaluate the state-only rules after `parse_fen`.
  `checks`, double-pass and stand-off stay history-dependent: "the Nth check
  was just delivered" and "both sides passed" are facts about moves, not about
  a position.

  **As built, `counting` is NOT among them** — the design said its progress
  needs no serialization because the count is position-derivable. That is
  wrong, and measured: seeding `counting.progress` at load makes the first
  move tick from the seed instead of initialising, so the count runs one ply
  ahead, the game ends mid-fixture, the remaining moves cannot be applied and
  the whole position is rejected. Six counting fixtures failed. The *condition*
  that starts a count is a position fact; how far the count has run is a
  history fact the FEN does not carry, and seeding one would invent a number.
  Counting therefore still begins on the first move played from a loaded
  position. Root evaluation covers `goal`, `extinct` and `counter`, whose
  clock the FEN does carry.
- **The TUI joins the same oracle** — `graphics.rs:515-520` calls
  `game_outcome` rather than formatting `game_result` raw. Not runnable here,
  so it ships as a code-path match with no behavioural claim attached.

#### Per-rule subject table (the contract to document and enforce)

| rule | subject | today |
|---|---|---|
| `checkmate` / `stalemate` | the side with no legal move | correct (`util.rs:245`, `search.rs:1567`) |
| `checks` | the mover that delivered the Nth check | correct (`termination.rs:485`) |
| `goal` | the mover that reached the zone | correct (`:411`) |
| `extinct` | the colour whose count ran out | correct after dropping `opponent` |
| `perpetual` | the sole offender | correct after honouring the declared outcome |
| `adjudicate` | the points winner | correct (`:442-450`) |
| `counting` | **the material side** (the non-bare colour) | **wrong** — names `state.playing` at HEAD, `mover` in the working tree; both are ply parity |
| `counter` / `repetition` | the mover that triggered it | flipped in the working tree; needs documenting either way |

`counting`'s material side is already computed at `move_list.rs:2733`
(`let winner = if white_bare { BLACK } else { WHITE }`) but used only to pick
the limit and never carried to the resolution point. Reuse `side_is_bare`
(`termination.rs:296`) at the resolution point rather than plumbing a new
field.

#### Confirmed defects, each verified directly

1. **Search never consulted `perpetual`.** `alpha_beta` returned
   `draw_score!` for any repetition, so a perpetual check scored as a draw and
   the engine steered into it. Cute Chess implements shogi perpetual check as
   a move-generation prohibition (`ShogiBoard::vIsLegalMove`:
   `isIncheck && repeatCount() > 2 && (m_checks[opp] & 0b1111111) == 0b1111111`),
   so the cycle-closing check is rejected and the game forfeited.
2. **The declared per-offence outcome was discarded.** `Perpetual.check` and
   `Perpetual.chase` each hold an `Outcome` parsed at `game_io.rs:1915-1925`;
   only their `is_some()` was read, so `perpetual: check draw` scored as a
   loss. The `Option` should be the enable flag and the payload the result.
3. **The verdict fired at the first repeat, not the rule's count.** Scoring a
   perpetual terminal at the second occurrence forbids the checker a legal
   continuation — shogi's rule is `repetition: 4`, xiangqi's `3`. Verified:
   with the gate on the declared count, xiangqi scores `mate 1` at the third
   occurrence and matches the baseline's `cp 45` at the second.
4. **`perpetual_offender` does not restore `search_ply`.** `undo_move!` uses
   `saturating_sub` (`move_list.rs:2849`) while `make_move!` counts up with no
   floor, so a cycle reaching back past the search root leaves the counter
   inflated; every node below then reads its ply — and its PV row — from the
   wrong slot. Reproduced: the root returned `e1e7+` from an empty square
   where the baseline returned `d1d7+`.
5. **A cycle span holding a search null move must be rejected.** `null_move()`
   is all-ones and its snapshot has no move to undo; real passes are
   unaffected, since `is_pass!` requires `QUIET_MOVE` with `end == start`.
6. **Extinction misses promotion.** `extinct_outcome` gated on capture move
   types, but a promotion retypes a piece (`piece_list_remove!` on the old
   index, `piece_list_push!` on `promoted_piece`, `move_list.rs:1713-1718`).
   Demonstrated in kinglet: capturing the last black pawn gives `White wins`,
   promoting your own last pawn gives `Ongoing`. Capture-or-promotion is the
   complete predicate — drops only add, castling relocates without retyping,
   and `is_unload` moves already carry a capture move type.
7. **`Extinct.opponent` and the `any|own|opp` token are dead.** No shipped
   config used `opp` or `own`, the parser never distinguished `own` from
   `any`, and inverting the outcome word says the same thing. Removing them
   shrinks the grammar to `extinct: <set> [<count>] <outcome>`.
8. **Uchifuzume is scored in search and ignored by game truth.** An illegal
   mating drop is flagged by the generator (`enc_can_checkmate!(.., !drop_k)`,
   `drop_list.rs:144`) and read at exactly two sites, `search.rs:740` and
   `:1572`, which negate the mate score. `adjudicate_no_move`
   (`util.rs:239-248`) — the oracle behind `d`, self-play, datagen and SPRT —
   has no equivalent, so search and game truth disagree on who won. Note the
   flag moves from bit 23 to bit 112 in F-3; the two changes do not conflict.
9. **No eager rule can fire on a freshly-loaded FEN.** `position_terminal`
   opens with `state.history.last()?` (`termination.rs:473`) and `parse_fen`
   never touches `game_result` (no write anywhere in `game_io.rs`). A FEN of a
   position that already satisfies `goal`, `extinct` or `counter` reports
   `Ongoing` until someone moves.
10. **The TUI reports a different result from `d`.** `graphics.rs:515-520`
    formats `state.termination.game_result` raw instead of calling
    `game_outcome`, so repetition and perpetual verdicts never appear there.

#### Further defects, from the detector audit — each re-verified by hand

11. **`royal_pieces` is never maintained.** One writer in the whole repo,
    `game_io.rs:2368`, inside the FEN board loop; `make_move!` updates
    `royal_list` on a drop (`move_list.rs:2513-2516`) and never the counter,
    and `verify_game_state` does not assert it. `side_is_bare`
    (`termination.rs:298`) requires `royal_pieces[side] == 1`, so **sittuyin,
    whose kings start in hand, has `[0, 0]` forever and its
    `counting: burmese draw` rule can never fire.** Any variant that drops or
    captures a royal is wrong the same way. Fix by maintaining the counter
    beside `major`/`minor` and adding it to `verify_game_state`, which is what
    would have caught it.
12. **FEN round trip loses the move number, and fails outright past fullmove
    255.** `format_fen` emits the halfmove field only when a `counter` rule
    exists (`game_io.rs:2790-2793`) but always emits the fullmove; `parse_fen`
    unconditionally consumes the next field *as* the halfmove
    (`:2535-2543`) and only then reads the fullmove. For a counter-less
    variant the fullmove is eaten by the halfmove slot — which is typed `u8`
    — so `ply_counter` is silently zero and any fullmove above 255 returns
    `Invalid halfmove clock`. Verified by grep: of the campaign variants only
    `standard.conf` declares `counter:` — **shogi, crazyhouse, grand and
    xiangqi all round-trip wrong.** The lost `ply_counter` is mostly
    cosmetic; the parse failure past fullmove 255 is not, since fixtures,
    datagen and the position sweeps all feed FENs back in.
13. **`checks.delivered` and `counting.progress` are not serialized at all,**
    so `position fen <midgame threecheck>` restarts both check counters, and
    a restored makruk/sittuyin game restarts the pieces'-honour count *and*
    re-freezes its limit from the current material.
14. **`counting.progress` can restart and re-freeze.** The transition arm at
    `move_list.rs:2727-2735` recomputes `pieces + 1` and a fresh limit
    whenever bareness flickers off and back on; both sides going bare cancels
    counting outright rather than resolving it.
15. **`extinct_outcome` reports the first matching colour, WHITE first.** A
    colour already at or below threshold from a state the rule never
    evaluated — a loaded FEN, or a hand — is attributed to the *next* capture
    or promotion by anyone. Interacts with defect 9.
16. **`extinct` and `adjudicate` both ignore `piece_in_hand`**
    (`termination.rs:376-380`, `:437-440`). Latent — no shipped drop variant
    declares either — but the schema permits the combination.
17. **`goal_outcome` can fire for a piece that predates the last move.** Its
    doc justifies scanning by "the check runs after every move", which fails
    at a loaded FEN (defect 9) and when the mover's *unload* or castling
    relocates an enemy goal piece onto the zone.
18. **In SETUP, `is_in_check!` is hard-false** (`move_list.rs:87`), so a side
    with no legal placement is adjudicated stalemate rather than checkmate,
    and setup checks are never counted. Latent for shipped variants; becomes
    live the moment K-3 changes the SETUP exit predicate.
19. **`counter` and `counting` fire ahead of checkmate.** Once `game_result`
    is set, `generate_all_moves_and_drops` returns empty
    (`move_list.rs:3745`), so a move that both exhausts the clock and mates is
    scored as the clock rule. FIDE gives mate priority.
20. **`make_null_move!` ticks `repetition.clock` and nothing else**
    (`move_list.rs:3610-3631`). Leaving `counter.clock` and
    `checks.delivered` alone is defensible — a passed turn is not a halfmove
    and delivers no check. `counting.progress` is the odd one out: that count
    ticks every ply by rule, so search under-estimates how close a
    makruk-family position is to being counted out. Smallest of the twenty;
    fix or document, do not invent machinery for it.

Suspected, to settle with a `debug_assert!` rather than argument: the redo
loop in `perpetual_offender` (`termination.rs:673-675`) discards
`make_move!`'s bool, and `make_move!` self-undoes and returns `false` on a
rejected move — which would desynchronise history for the rest of the search.
A faithful replay should never be rejected; assert it and find out.

Also suspected: `virgin_board` is not part of the Zobrist hash while virginity
is a live move-generation input, so two positions differing only in virginity
hash alike and repetition can over-count. Needs a constructed test before it
is treated as real.

Not in scope, recorded so it is not re-found: `sprt.rs:693-699` scores an
operator-interrupted game as a loss for the side to move. It fires only on
`SYSTEM_INTERRUPT`, never on a no-move position (that path is
`adjudicate_no_move` at `:703`), so it is a harness detail, not a rules bug.

#### Why this is one patch and not several

The groups are entangled, not merely adjacent. `extinct` and `goal`
mis-attribution (15, 17) exists *because* the root is never evaluated (9), so
fixing either alone leaves the other looking arbitrary. Evaluating the root
(9) is what makes serializing `counting.progress` unnecessary, which is what
keeps the FEN fix (12) small enough to be safe. And the perpetual group (1-5)
cannot be split at all: routing search through the shared oracle is unsafe
until both the scan cap and the `search_ply` restore exist. Splitting any of
these ships a half-fix again, which is the failure this section exists to
correct.

The FEN format is the sharp edge. Adding `checks.delivered` and
`counting.progress` fields changes a format that `.dict` regexes rewrite for
each protocol dialect (see the anchored-ep lesson in `cbfea3a`) and that every
fixture FEN and `res/perft` entry is written in. Field order, optionality and
the dict interaction must be settled before any of it is written — the
existing halfmove field is already positionally ambiguous (defect 12), so the
fix is to make presence unambiguous, not to add more optional trailing fields.

The anchor for that: `parse_fen` reads board, side, `[castling]`, `[ep]`,
`[hands]`, `[halfmove]`, `[fullmove]` (`game_io.rs:2235-2560`). Every
conditional block is gated on the variant's rule bits — except the halfmove
block at `:2535`, gated only on "a field remains", while `format_fen` emits it
only when `counter.is_some()` (`:2790`).

**Decision: fix the ambiguity, do not serialize progress.** The three losses
are different species. The halfmove clock has a slot in every dialect our
dicts speak; `checks.delivered` and `counting.progress` have none, so
serializing them is a private extension that all 28 `= uci fen =` blocks would
have to strip outbound and rebuild inbound, and that Cute Chess and FSF would
reject — breaking the very harness this patch exists to satisfy. That is the
anchored-ep lesson of `cbfea3a` at larger scale. And `counting.progress` needs
no serialization once the root is evaluated: its start condition *is* the
position (exactly one side bare) and `counting_limit` re-derives the frozen
limit from material on the board, so re-deriving at load is the definition
rather than a loss. `checks.delivered` genuinely is lost; document that on the
field, and note that mid-game three-check state reaches us as
`position fen <start> moves …`, which replays it exactly.

**Mechanism:** disambiguate by *field count*, not by rule declaration.
`parse_fen`'s two independent `if parts.len() > part_index` blocks become one
match on the number remaining — `>= 2` is (halfmove, fullmove), `1` is
fullmove alone, `0` is neither. That is correct for the internal dialect, for
perft's clock-less FENs, and for an external six-field FEN handed to a
counterless variant, which gating on `counter.is_some()` would not be. Parse
the halfmove as `u32` and saturate into `counter.clock: u8`, which also
removes the fullmove-above-255 parse failure. `format_fen` keeps its
`counter`-gated emission and is then provably the reader's inverse.

**Blast radius, verified by reading rather than assumed:**

- `res/perft/*.perft` — **zero edits.** Every FEN carries no trailing numeric
  field (`standard.perft` ends `b KQ *`, `shogi.perft` ends `w LPP/psn`), so
  they take the `0 remaining` arm and counts cannot move.
- `tools/endgame_fixtures.txt` — **zero edits.** Its FENs are either two-field
  (`xiangqi | … w`) or already carry both clocks (`sittuyin … -/- 0 1`).
- `res/dicts/*.dict` — **moved to S6, last step of this patch.** The problem is
  larger than "capture the inbound move number". Measured:

  ```
  xiangqi --protocol uci -> rnbakabnr/9/… w - - 0 1   translated
  makruk  --protocol uci -> rnhmkhnr/8/…  w 0 1       NOT translated
  ```

  `makruk`'s outbound rule is `^([^ ]+) ([wb])$ -> $1 $2 - - 0 1`, matching a
  two-field FEN only, while `makruk` declares a `counter` so `format_fen`
  emits four — nothing matches and no translation fires. The inbound rules in
  `xiangqi`, `makruk`, `ouk-chaktrang`, `asean` and `ai-wok` separately drop
  both clock fields. **Inbound is on the gameplay path** —
  `handle_position` calls `parse_fen(.., dict.as_ref())`
  (`protocol.rs:534`) — so a GUI-supplied makruk FEN restarts the
  board's-honour count at zero. Not cosmetic.

  It goes last because it needs every dict mapped against `format_fen`'s real
  per-variant output first; editing one regex on a guess is what produced the
  board corruption behind `cbfea3a`. Gate: a round trip per affected variant,
  `state --protocol uci` out and `position fen` back in, board and both clocks
  preserved.

  **Ordering is safe:** verified the `parse_fen` rewrite does not interact. An
  inbound-translated FEN carries no clock fields, which is `trailing == 0`
  under the new reader and was skipped by both old blocks — S2 neither
  worsens nor improves the dict path.

#### Design notes, including one flaw in the attempted patch

- **The repetition probe must stay bounded.** C-3 capped the per-node scan
  at `REP_SCAN_CAP = 64` (`prelude.rs:279`) precisely because the drop-variant
  bound is the whole history. Routing search through `repetition_outcome`
  reintroduces an *unbounded* rescan, since that function calls
  `count_repetitions(state, usize::MAX)`. Give `repetition_outcome` a `cap`
  parameter alongside `min_count` — matching `has_repetition` and
  `count_repetitions`, which already take one — and have search pass
  `REP_SCAN_CAP` while game truth passes `usize::MAX`. Sound because a genuine
  perpetual is a short consecutive cycle: reaching 3 or 4 occurrences inside
  64 plies is not a constraint, and the neutral twofold draw already scans
  under the same cap.
- **One mirror site, not two.** `perpetual_offender` reports
  `(offender, declared_outcome)` and `repetition_outcome` re-expresses both it
  and the neutral outcome against the side to move in one place. The separate
  `perpetual_verdict` helper introduced mid-patch is deleted — it existed only
  to mirror one of the two.
- **`counting` reads `side_is_bare`** at the resolution point rather than
  carrying a new field; the material side is the non-bare colour, and
  `counting.progress` is `None` unless exactly one side is bare
  (`move_list.rs:2727-2728`).
- **F-3 interaction.** This patch adds a second reader of
  `drop_can_checkmate!` (the game-truth uchifuzume fix); F-3 moves that flag
  from bit 23 to bit 112. Independent changes to the same flag — land this
  first, and F-3's bit move then updates both readers together.
- **K-3 interaction.** In SETUP only drops are generated
  (`move_list.rs:3753-3767`) and `is_in_check!` is hard-false (`:87`), so a
  side that runs out of placements before its opponent has zero legal moves
  and is adjudicated *stalemate*. Shipped variants escape only because their
  hands are equal-sized — sittuyin `KSSFRRNN/kssfrrnn`, janggi `HHEEQ/hheeq`
  — so both empty on the same ply and SETUP ends
  (`move_list.rs:2567-2571`). K-3 replaces that exit with "no side has a
  legal placement", which removes the coincidence this currently rests on.
  Record the dependency; do not pre-empt K-3 here.
- **Delete `royal_pieces`, do not start maintaining it.** It has one writer
  and one reader, and `royal_list` already carries the same fact: it is
  updated on quiet moves (`move_list.rs:1599`), promotions (`:1787`),
  captures (`:2081`, `:2149`, `:2454`) and drops (`:2514`), and
  `verify_game_state` recomputes and asserts it (`util.rs:599-616`) while
  asserting nothing about `royal_pieces`. Keep the test at exactly `== 1`:
  it reproduces today's semantics, and whether a bare-king test should accept
  a side holding two royals is a separate rules question —
  `janggi.conf:40` declares `royal: KQkq`, so the question is real. Raise it,
  do not answer it inside this patch. `side_is_bare` becomes
  `royal_list[side].len() == 1`, which removes a field, removes the
  divergence, and inherits the assertion that would have caught this.

#### Implementation shape

**Root evaluation without duplicating `position_terminal`.** The last-move
reads only ever *narrow*, so invert the dependency rather than forking the
body. Drop the `let last = state.history.last()?` early return
(`termination.rs:473`) for a plain `let last = state.history.last();` and
derive `accepted_stand_off` / `double_pass` through `is_some_and`, so an empty
history yields `false` and the chain falls through to the state-only arms.
`checks` then stays history-dependent for free, because `last.in_check` is
simply absent. Hoist extinct's capture-or-promotion gate out of
`extinct_outcome` into `position_terminal` — called when history is empty or
when the last move retypes — which leaves `extinct_outcome` a pure position
scan and closes defect 15 in the same stroke, since the WHITE-first
mis-attribution exists only because the root was never evaluated. Have
`goal_outcome` scan `[mover, 1 - mover]` in that order, which closes defect
17's enemy-relocation case without changing any shipped verdict. Extract the
inline block at `move_list.rs:2725-2739` into the patch's **one new
function**, shared by `make_move!` and `parse_fen`; initialising only on the
`None -> Some` edge is also defect 14's fix. The root hook is then two calls
at the end of `parse_fen` (`game_io.rs:2563-2566`).

**Signatures.**

```rust
pub fn side_is_bare(state: &State, side: u8) -> bool
pub fn extinct_outcome(state: &State) -> Option<(u8, Outcome, &str)>
pub fn goal_outcome(state: &State, mover: u8) -> Option<(u8, Outcome, &str)>
pub fn position_terminal(state: &State) -> Option<(u8, Outcome, &str)>
pub fn repetition_outcome(
    state: &mut State, min_count: u8, cap: usize,
) -> Option<(Outcome, bool)>
fn perpetual_offender(state: &mut State, cap: usize) -> Option<(u8, Outcome)>
pub fn counting_progress(
    state: &State, last: Option<(u16, u16)>,
) -> Option<(u16, u16)>
```

No lifetime annotation appears, which is the signal that the shapes are right.
`perpetual_offender` takes `cap` because its own backward `find`
(`termination.rs:618-619`) is unbounded too, not just the count scan. Uchifuzume
needs no new signature: extract the `search.rs:740` / `:1572` predicate as an
`illegal_mating_drop!` macro beside `drop_can_checkmate!` and read it from all
three sites, which also makes F-3's bit move a single edit.

**Sequencing** — five steps, each building and passing fixtures:

- **S1 — field deletion and the subject contract.** Delete `royal_pieces`;
  `side_is_bare` reads `royal_list`. Rewrite `Outcome`'s doc and every rule's
  subject doc; `counting` names the non-bare colour. Gate: all perfts plus
  bench byte-identity.
- **S2 — FEN.** Count-based `parse_fen`, `u32` halfmove, dict inbound capture.
  Must precede S3, which reads the clock. Gate: perft unchanged, plus a
  `debug-headless state` round trip past fullmove 255 on shogi, xiangqi,
  crazyhouse and grand.
- **S3 — root evaluation, one indivisible group.** De-`?` `position_terminal`,
  hoist the extinct gate, two-colour `goal_outcome`, extract
  `counting_progress` and call it from both sites, hook `parse_fen`. Splitting
  it loses the extinct gate or leaves the re-freeze. Gate: the new extinction
  and counting fixtures.
- **S4 — perpetual, defects 1-5 as one group.** The search route is unsafe
  until both the `cap` and the `search_ply` restore exist, so they land
  together, with a `debug_assert!` on the redo loop's discarded
  `make_move!` bool. Gate: the `go`-based xiangqi fixture.
- **S5 — independents. As built, narrower than planned.**

  Shipped: uchifuzume parity, via an `illegal_mating_drop!` macro beside
  `drop_can_checkmate!` read by both search sites and by
  `adjudicate_no_move`, which had no equivalent and so disagreed with search
  on who won. The TUI now routes through `game_outcome` — **not the one-line
  change the plan claimed**: `from_state` takes `&State` while `game_outcome`
  needs `&mut` for the repetition walk, so it calls it on a clone rather than
  threading mutability through a render path that cannot be exercised here.
  `janggi.conf` gains `perpetual: check loss`, confirmed against FSF's
  `janggi_variant()` (`v->perpetualCheckIllegal = true;`).

  **Not shipped, deliberately:**
  - *makruk / ouk-chaktrang counting numbers.* The research put
    `count_limit()` in `position.cpp`; it is not there, so the Cambodian
    figures (63, 7/15/21/31/43) are unverified. FSF also counts in plies with
    doubled limits — `st->countingLimit = 2 * count_limit(..)`,
    `st->countingPly = 2 * count<ALL_PIECES>()` — so its numbers are not
    directly comparable to ours and a naive swap could silently break three
    variants. Needs the real function read first.
  - *`make_null_move!` ticking `counting.progress`.* A search null move is not
    a game ply; leaving it alone is consistent with `counter` and `checks`.
  - *SETUP `is_in_check!`.* Changing it moves drop legality in sittuyin and
    janggi; it belongs with K-3, which rewrites the SETUP exit anyway.
  - *Clock-vs-mate priority.* Real — FIDE gives mate priority when the 50th
    move mates — but deciding it needs legal-move generation inside
    `position_terminal`, which runs in every `make_move!`. Too costly a hot
    path change for a coincidence this rare; recorded, not attempted.

  Gate: bench nodes identical on 15 variants, perft identical on all 38,
  fixtures 33/33.
- **S6 — dict FEN rules.** Map every `= uci fen =` block against
  `format_fen`'s actual output for that variant, then fix outbound match
  arity and inbound clock capture together. Last because it is the only step
  whose failure mode is silent board corruption, and because S2 fixed the
  reader it has to agree with. Gate: per-variant round trip, out through
  `state --protocol uci` and back in through `position fen`, with the board
  and both clocks preserved.

**Hot path.** Every change is either behind a rule's `is_some()` or strictly
cheaper: hoisting the extinct gate means a non-extinction variant does one
`Vec::is_empty` in the caller instead of a call that early-returns, and
deleting `royal_pieces` turns two array loads into one `len()`. The one fix to
measure is **S4's `perpetual_offender`** — it is the only thing that walks
history driving `make_move!`/`undo_move!` in a loop and runs `offence_set`, a
full-board attack pass, per cycle ply. Compare bench NPS against the parent on
shogi, minishogi and xiangqi; every other variant is covered by the
byte-identity gate, which proves zero added work by construction.

#### Config corrections — what the grammar can express today

Checked against Fairy-Stockfish `variant.cpp` / `position.cpp`. **Confirm each
against the FSF source before editing** — these come from research, not from
my own reading, and one wrong number silently changes a variant's rules.

Expressible now, so in scope:

- **`janggi.conf` is missing `perpetual: check loss`.** FSF sets
  `perpetualCheckIllegal = true` for janggi; we declare only
  `repetition: 3 draw`, so a perpetual check resolves as a draw. One line,
  and it makes janggi's rule set match the one xiangqi already has.
- **`makruk.conf` has a spurious `N: 44` row.** One knight with no rook and
  no khon counts to 64, not 44, in both FSF and the published table.
- **`ouk-chaktrang.conf` ships makruk's numbers.** Cambodian counting is
  uniformly one lower: board's honour 63, table `RR: 7 / R: 15 / HH: 21 /
  NN: 31 / H: 43 / default: 63`.

Not expressible, so **not** in this patch — each needs schema work and its own
decision:

- **Minishogi sennichite is a loss for Sente, absolutely** (FSF
  `nFoldValue = -VALUE_MATE`, `nFoldValueAbsolute = true`), not a draw and not
  relative to whoever closed the cycle. Our `repetition: <n> <outcome>` is
  always relative to a subject; a colour-fixed result has no spelling.
- **Sittuyin uses ASEAN counting**, which is a plain 50-move rule with no
  board's honour at all, a three-row table (`R: 16`, khon `44`, `N: 64`) and a
  count starting at **zero** rather than the piece total.
- **Board's honour semantics generally**: the real count begins only once no
  pawns remain, is measured in full moves, starts at the both-sides piece
  total, and survives ordinary captures. `counter:` is a halfmove clock that
  starts at move 1 and resets on every capture — none of the four properties
  is expressible. Cambodian adds a further gate (counting side down to three
  pieces or fewer).
- **Xiangqi chase exemptions are half-modelled.** `exempt KkPp` correctly
  exempts general and soldier as *chasers*, but FSF also exempts them as
  *targets* — chasing the enemy general, or a soldier that has not crossed the
  river, is not a chase, while a crossed soldier is. `exempt` scopes to
  chasers only.
- Both honour counts are **claimed**, not automatic: the disadvantaged side
  starts, may stop, and may restart them. The grammar has no claim concept.

**The fixtures encode the current numbers.** `tools/endgame_fixtures.txt`
asserts ouk-chaktrang counting out at 8 and at 16; under Cambodian rules those
become 7 and 15. Update them as part of the same change, deliberately, and say
so in the commit message — a fixture edit that follows a rules correction is
fine, a fixture edited to make a test pass is not.

#### Files touched

- **`src/game/representations/termination.rs`** — the bulk. `Outcome`'s doc
  stops claiming a global convention; every rule struct's doc states its own
  subject. `Extinct` loses `opponent`. `extinct_outcome` gains the
  capture-or-promotion gate and names the extinct colour. `position_terminal`
  splits so the state-only rules can also run without a last move.
  `perpetual_offender` returns `(offender, declared_outcome)`, restores
  `search_ply`, and rejects a span holding a null move.
  `repetition_outcome` takes a scan `cap`, resolves the offender, and mirrors
  once. `side_is_bare` reads `royal_list`. `perpetual_verdict` is deleted.
- **`src/game/moves/move_list.rs`** — the `counting` transition stops
  re-freezing its limit when bareness flickers; `make_null_move!` ticks
  `counting.progress`; `royal_pieces` maintenance is not added because the
  field goes away.
- **`src/io/game_io.rs`** — `extinct` grammar drops the dead
  `any|own|opp` token; `parse_fen`'s halfmove block is gated on
  `counter.is_some()` to match `format_fen`; the `royal_pieces` write goes.
- **`src/game/position/search.rs`** — the repetition probe calls
  `repetition_outcome` with `REP_SCAN_CAP` and the rule's own `occurrences`.
- **`src/game/util.rs`** — `adjudicate_no_move` gains the uchifuzume branch
  search already has, so game truth and search agree;
  `verify_game_state` loses the `royal_pieces` gap by losing the field.
- **`src/game/representations/state.rs`** — `royal_pieces` deleted, with its
  init, `reset` and `fork` copies.
- **`src/debug/graphics.rs:515`** — result row routed through `game_outcome`.
- **`src/prelude.rs`** — export list follows the renames.
- **`configs/*.conf`** — `extinct:` lines lose the leading token
  (`extinction`, `horde`, `kinglet`, `example`); any rule the research shows
  misdeclared is corrected with its source in the commit message.
- **`configs/example.conf`** — schema block rewritten: per-rule subjects, the
  shortened `extinct` grammar, and `perpetual`'s per-offence outcome.
- **`tools/endgame_fixtures.txt`, `tools/run_endgame_fixtures.sh`** — new
  cases plus a `go`-based expectation mode.

#### Verification

The differential harness comes first, because two of the ten defects were
invisible to reading and only showed as a diff against the parent binary.

1. **Parent baseline.** `git worktree add --detach` at the parent commit,
   build to a scratch `CARGO_TARGET_DIR`, keep the binary. Every claim below
   is new-vs-parent, not new-vs-expectation.
2. **Agnosticism gate.** `debug-headless bench` at `ANEKAMACAM_SEED=42` must
   be **byte-identical** on every variant declaring no `perpetual` rule —
   standard, crazyhouse, grand, and the rest. Only shogi, minishogi and
   xiangqi declare one, and only they may move.
3. **Perft** on standard, shogi, xiangqi, crazyhouse, minishogi, extinction,
   kinglet, horde. Nothing here touches move generation, so every count must
   be unchanged; a change means a detector reached into make/undo.
4. **Fixtures.** `tools/run_endgame_fixtures.sh` currently has 32 cases and
   asserts the `d` Result line. Every behaviour this patch adds needs one,
   and the extinction family has none for the bug being fixed — all three of
   `kinglet`/`extinction`/`horde` (lines 44-46) are capture-driven. Add at
   minimum: extinction by promoting your own last pawn (kinglet and
   extinction chess); a perpetual that fires at exactly the declared
   occurrence; the same line one cycle short, expecting `Ongoing`; a
   `counting` case for each of makruk, ouk-chaktrang and sittuyin proving the
   subject change is inert; and an uchifuzume case proving `d` now agrees
   with search.
5. **Search-side assertions are not covered by that harness**, which reads
   `d` only. Extend it with a `go`-based expectation (the printer already
   emits `EngineScore::Mate` as `score mate N`, `prelude.rs:474-484`) so the
   perpetual scoring has a regression test: xiangqi's perpetual position must
   report `score mate` at the third occurrence and a plain `cp` score at the
   second.
6. **Debug build** with `verify_game_state` active over the perpetual and
   extinction repros — the `search_ply` defect produced a legal-looking
   illegal move, so assertions matter more here than usual.
7. **Whole-corpus load check.** `debug-headless state <variant>` over every
   config, since the `extinct` grammar changes and a stale `.conf` would
   otherwise panic only when that variant is first selected.
8. **RR** on rebuilt phaseA-3..phaseE-3 binaries, with the shogi log grepped
   for `Illegal PV move` — 1271 of them in the last run is the number this
   patch has to move, and it is a far more sensitive signal than the four
   forfeits.

#### Follow-up, landed after the patch (2026-08-03)

Two commits on top of `de04444`, both gated on tools that did not exist when
the patch shipped.

**`abfbe1c` — every dialect FEN is now one an outside reader accepts.** S6
fixed eight dictionaries; a sweep found ten broken, and the two it had
deferred were the least severe of them. Fairy-Stockfish, driven with the same
variant name, rejects six outright and silently misreads a seventh:
`shogi`/`minishogi`/`euroshogi`/`judkins` wrote `[-]` holdings in four fields
where FSF writes `[]` in six; `judkins` also carried an unanchored
`w -> +s` that rewrote the side to move as a promoted silver; `pocketknight`
leaked the internal `N/n` hand as its own field; `berolina` omitted the
halfmove entirely, so FSF read the move number as the clock; `extinction` and
`horde` inherited standard's `$1 * <- ([^wb]) -`, which needs a non-`wb`
character before the ep dash and therefore *cannot* match in a variant with
no castling field. `janggi` and `kinglet` fired no rule at all.

`tools/run_fen_roundtrip.sh` is the standing gate — every variant, every
declared protocol, out and straight back in, failing both a no-op dictionary
and one whose output will not reparse, plus an FSF read-back where the two
agree on the starting board. 44/44. It reports INFO rather than FAIL where
the boards genuinely differ, which is not a dialect question: capablanca's
rival setup, our janggi's setup phase, and `los-alamos`/`tjatoer`, whose
names FSF does not know and silently answers as chess.

**`737344c` — the piece letters were fixed in the `.conf`, not the `.dict`.**
Three configs named pieces with letters no outside reader shares: makruk and
ouk-chaktrang wrote the khon `H`, shatranj the alfil `A` and ferz `F`. Our
own `ai-wok` and `sittuyin` already spell that silver-shaped piece `S`, so
the repo disagreed with itself too. Renaming in the config removes the
translation instead of adding it, and removes a hazard rather than working
around one — inbound, a bare `a <- b` rule rewrites the *side to move*, so
the dictionary version needed the anchored repeat xiangqi uses for its
elephant. Piece order keeps its positions, so tuned parameters stay valid
with no regen, and the movement tokens are untouched (`Bb:A`, `Qq:F` are
still the alfil leap and the ferz step). The same commit made the promotion
suffix rank-agnostic: it was pinned in all four makruk-family dicts (`8=F`,
`8=M`, `6=A`) so it covered one colour only, and makruk had none, emitting an
uppercase `a5a6M`. Verified on the move path, which is what the RR exchanges.

Still open, and each its own question rather than a dialect bug:
**ouk-chaktrang** is `cambodian` to FSF and carries the king/met one-time
leaps in the *castling* field as `DEde`, while we model them with virgin
prefixes (`Kk:K|iN`, `Mm:F|inW.`) and serialise nothing — matching letters
alone would hand FSF a FEN it reads happily while losing the leap state, so
it stays mismatched on purpose, and it ties to the `virgin_board`-not-in-hash
suspicion. **shatranj lacks the bare-king rule** (FSF returns `mate 0` for a
bared king; found while testing promotion notation); its base case looks
expressible as `extinct: PRNBQprnbq 0 loss`, the counter-baring exception
does not.

**`7b92638` — the two fixed defects got the regression tests they shipped
without.** Extinction by promoting your own last pawn (kinglet and
extinction; all three pre-existing extinction cases are capture-driven), and
the search-side perpetual score. The latter needed a probe the harness
lacked: an expectation of `score cp` or `score mate` now runs
`go depth $GO_DEPTH` and asserts the score kind, because the `d` oracle
cannot see a verdict search reaches on its own. Each new case was checked to
fail on the parent binary — the closing perpetual answers `cp` there, which
is the bug behind the four forfeits and 1271 illegal-PV warnings. 33 → 38.

**New defect, found by trying to write the janggi fixture and left open.**
`move_list.rs:2766` resets `repetition.clock` on any promotion, so
`repetition_scan_bound` never looks past the last one. That is sound only
where promotions are irreversible. Janggi encodes palace-diagonal geometry as
a reversible `K:Q` / `Q:K` type swap, so **every general move is a
promotion**, the bound never exceeds a ply or two, and no cycle containing a
general is ever seen. Consequence: the `perpetual: check loss` line S5 added
to `janggi.conf` **is inert** — it was recorded as shipped and cannot fire.
Isolated three ways: janggi chariot-only repetition draws normally, the
identical xiangqi geometry resolves as a perpetual, and only the janggi
general cycle reads `Ongoing`. Two candidate fixes — drop the promotion term
(a too-large bound cannot produce a false positive, but widens the scan in
shogi) or derive a reversible-promotions flag from the promotion map. Not
attempted here: it moves shogi node counts, and the campaign baseline is
measured there.

### RR-3 diagnosis (2026-08-04): three causes, and only one was in the plan

The first RR on the patched A-3..E-3 binaries ran crazyhouse alone, ~620 games
per engine, `tc=30+0.3`, cutechess concurrency 30 on 32 cores, `Hash=64`, no
book. **Zero illegal moves, zero forfeits, zero time losses, 11 draws in 1558
games** — the termination patch did what it was for. The standings did not
follow:

```
fsf-1900  +217    fsf-1800  +100    fsf-1700    0
phaseE-3  -136    phaseA-3  -172
```

Three separate causes, measured. The plan had one of them.

**1. Effective branching factor, and it is not only about drops.**

The headline number is **intra-engine**, which is what makes it safe: same
binary, same harness, same depth, only the rule set changing. Mean over 3
standard midgame and 6 crazyhouse midgame positions, `go depth 13`,
single thread, `Hash=64`:

| | standard | crazyhouse-mid | change |
|---|---|---|---|
| **ours**, nodes @13 | 128 668 | 3 335 901 | **x26** |
| **ours**, EBF | 1.57 | 1.87 | +0.30 |
| FSF, nodes @13 | 409 146 | 182 473 | **x0.45** |
| FSF, EBF | 2.10 | 1.73 | -0.37 |

Read it as one sentence: **our engine needs 26x more nodes to reach depth 13
in crazyhouse than in standard, where FSF needs less than half.** That is a
58x relative swing and it needs no cross-engine unit, no matched `Hash`, and
no assumption that the two engines mean the same thing by "depth". It also
kills the lazy reading in the other direction — in standard our EBF is 1.57
against FSF's 2.10 and we use 0.31x its nodes, so the search machinery is not
generally weak. Only the drop path is.

The cross-engine columns below are kept because they size the gap, but they
are **not** the gate. FSF's nominal depth is not ours (different reduction
schedule), and the RR's FSF ran `UCI_LimitStrength=true` while these probes
ran it at full strength, so the two are not the same opponent.

Cumulative nodes per iteration on one crazyhouse midgame position (ply 36,
full pockets, taken from the live RR debug log):

| depth | 1 | 3 | 5 | 7 | 9 | 11 | 13 | EBF |
|---|---|---|---|---|---|---|---|---|
| ours | 425 | 2 855 | 10 100 | 35 684 | 62 822 | 477 825 | 1 786 459 | **1.90** |
| FSF | 726 | 1 922 | 3 631 | 4 590 | 6 819 | 25 312 | 45 660 | **1.38** |

39x the nodes at depth 13. Nodes to fixed depth over 12 such positions: 20.9x
at depth 8, 16.6x at 10, 12.0x at 12 — the ratio *shrinks* with depth, which is
what a compounding EBF gap looks like, not a fixed per-node surcharge.

The same probe across variants, from startpos unless noted:

| variant | depth | ours | FSF | ratio | EBF ours / FSF |
|---|---|---|---|---|---|
| standard (midgame) | 12 | 64 990 | 72 738 | **0.89x** | — |
| xiangqi | 13 | 57 703 | 94 465 | **0.6x** | 1.70 / 1.88 |
| janus | 10 | 59 061 | 77 959 | 0.8x | — |
| minixiangqi | 10 | 19 121 | 16 225 | 1.2x | — |
| los-alamos | 10 | 52 310 | 36 457 | 1.4x | — |
| modern | 10 | 58 932 | 33 725 | 1.7x | — |
| crazyhouse (startpos, empty pockets) | 13 | 372 743 | 203 697 | 1.8x | 2.11 / 2.16 |
| capablanca | 10 | 56 156 | 25 341 | 2.2x | — |
| shogi | 13 | 523 385 | 104 456 | **5.0x** | 2.11 / 1.97 |
| crazyhouse (midgame, full pockets) | 13 | 1 786 459 | 45 660 | **39x** | 1.90 / 1.38 |
| grand | 11 | 4 912 871 | 51 285 | **48x** | 2.66 / 1.95 |

Two readings the previous diagnosis did not have. First, **crazyhouse with
empty pockets is fine (1.8x) and only blows up once the hands fill** — that is
the drop hypothesis confirmed far more sharply than RR-2's 4.3x, and it is why
the search block is still the right block. Second, **grand is the worst variant
in the corpus at 48x and has no drops at all**, so a fix aimed only at drops
leaves a campaign variant untouched.

Grand's mechanism is separate and visible in the iteration log: our score
alternates `5, 33, 5, 36, 5, 46, 5, 33, 5, 33, 12` on consecutive plies while
the best move flips between back-rank rook shuffles (`a1f1`, `j1f1`) and only
reaches a developing move at depth 8; FSF settles on `c3c4` at depth 3 and
climbs `92 → 176`. Root move generation is not the problem — perft(1) is 65 for
both engines. A near-flat eval with an odd/even oscillation gives the ordering
nothing to work with, which is exactly what an EBF of 2.66 looks like. The
cause is not yet identified, so grand gets a diagnosis stage, not a guessed fix.

**2. The time manager spends half the clock.** `compute_budgets`
(`protocol.rs:707-733`) computes `soft = (time/40 + inc*2/3) / 2`, i.e.
`time/80 + inc/3` — 479 ms at 30.3 s remaining — and `hard = soft * 4`. The
per-iteration scale (`search.rs:483-500`) then multiplies the soft budget by
`TM_STABILITY_PCT = [160,130,110,100,85,75]`, so a settled search takes 75% of
an already-halved allocation.

Measured in the RR, total clock consumed per game against a ~38.7 s budget
(30 s + 29 moves x 0.3 s):

| | clock used / game | median move | median depth |
|---|---|---|---|
| fsf-1700/1800/1900 | 37.2 s (96%) | 0.96 s | 13 |
| phaseA-3 / phaseE-3 | 19.5 / 20.4 s (**53%**) | 0.58 s | 11 |

The formula predicts 0.48 s x 29 moves ≈ 14 s plus iteration overshoot ≈ 19 s,
which is what was observed, so this is the allocation and not the RR's CPU
oversubscription. It is variant-agnostic, it has been true of every RR this
project has ever run, and **it was not in the plan at all**.

**3. The eval unit anchor recorded in this plan is wrong — and so is the
replacement. Stop using a cross-engine scalar.** The plan states "FSF 100 cp =
198 of our units", derived from a single king-exposure position. Measured
fresh on standard positions with clean material imbalances:

| position | ours | FSF | ratio |
|---|---|---|---|
| remove g1 knight | -313 | -433 | 0.72 |
| remove f1 bishop + g1 knight | -594 | -949 | 0.63 |
| remove b1N + c1B + d1Q | -1558 | -2055 | 0.76 |

Median 0.74 — so the 1.98 figure is wrong by 2.7x in the direction that
*inflated* the eval block's case. But 0.74 is not a sound replacement either,
and saying so is the point. `derive_eval_parameters` already normalises the
cheapest opening value to exactly 100 (`parameters.rs:677-680`; standard's
param file opens `4140 1725 100 556 318 315 933`), so a pawn is 100 in both
engines *by construction* and the residual is not a unit difference at all —
it is FSF pricing above-pawn material and its positional terms higher relative
to its own pawn than we do. The three samples spread 0.63-0.76, a 20% band, so
there is no clean scalar to find; and none of them measures a pawn, which is
the one point that would pin a scale.

**Standing rule from here: express each engine's term as a fraction of that
engine's own knight, and compare fractions.** No cross-engine multiplier
appears in a gate again. Where 1.98 appears in the E-3 section both columns are
shown so the arithmetic is auditable, but neither is a gate.

What survives the correction: on the 12 crazyhouse midgame positions our eval
agrees with FSF on sign 92% of the time and is within a factor of two on 9 of
12; the three misses read ~0 where FSF reads +468..+653 and are attack
positions, not material ones. **The eval is not the dominant problem.**

**Corroborating symptoms, consistent with cause 1.** FSF makes a check with
42-50% of its drops; we do with 29-30%. 65-76% of our wins are drop mates
against FSF's 53-56%. We lose faster than FSF loses (p10 game length 34-35
plies against 39-45).

**4. NPS also collapses in drop variants, and two of its causes are untouched
by the whole drop block.** Our NPS falls 48% moving from standard to
crazyhouse (1.35-1.47 M to 0.66-0.81 M) where FSF falls 22% (1.62-1.65 M to
1.23-1.31 M). Per-search fixed overhead is *not* the cause and the earlier
suspicion of the `cont_hist` allocation is dismissed again: `go depth 1` costs
us 0.72 ms against FSF's 0.21 ms, against a 580 ms median move. Two causes
verified by reading, both of which G-3..K-3 leave in place:

- **Futility and LMP `continue`, they do not `break`** (`search.rs:1304-1313`,
  `:1336-1349`). A pruned move has already been generated, encoded and scored;
  only make/undo and eval are skipped. So the O(k·n) `pick_by_score!` selection
  cost over a 100-150 move list survives every pruning stage. Making drops
  prunable (I-3) removes their subtrees, not their selection cost.
- **`repetition_scan_bound` is drop-gated to the full history**
  (`termination.rs:810-822`): `if drops!(state) { length } else { clock }`,
  then capped at `REP_SCAN_CAP = 64`. So every node in crazyhouse and shogi
  walks up to 64 strided `Snapshot` reads where standard walks single digits.
  C-3 chose this deliberately — a capture-then-drop cycle restores an identical
  position, so an irreversibility bound is unsound with hands — but it is a
  per-node cost that scales with exactly the variants that are slow.

These make the NPS collapse a candidate stage of its own (R-3) rather than
something expected to fall out of the drop block.

**Harness note, not a stage.** The RR ran `-concurrency 30` on 32 cores, so 60
engine processes shared 32 cores and every engine got under half a core at a
wall-clock time control. That depresses both sides equally and cannot explain a
one-sided result, but it adds noise. Future RRs cap concurrency at cores/2.

**Consequence for the ladder.** The search block keeps its place and its
letters shift by one to make room for the time-management stage, which runs
first because it is cheap, variant-agnostic, and worth more than any single
drop stage. The eval block survives but its case is weaker than recorded and
it stays last.

### F-3 — time management: spend the clock

The largest single lever measured, and the cheapest. Two defects in one
formula: the per-move allocation is halved after it is computed, and the
sudden-death divisor of 40 is already conservative for games that last ~29
moves, so the increment bank is never spent and the engine finishes every game
with roughly half its clock unused.

Replace the clock arm of `compute_budgets` (`protocol.rs:707-733`):

```
usable  = (time - overhead).max(1)
divisor = movestogo > 0 ? min(movestogo, TM_MOVE_HORIZON)     /* new, 18 */
                        : TM_MOVE_HORIZON
raw     = usable / divisor + inc * 3 / 4
reserve = (usable * TM_MAX_SHARE_PCT / 100)                   /* new, 40 */
              .max(MIN_TIME_BUDGET_NS)
soft    = raw.clamp(MIN_TIME_BUDGET_NS, reserve)
hard    = (soft * HARD_BUDGET_FACTOR).clamp(MIN_TIME_BUDGET_NS, reserve)
```

Four corrections against the first draft of this stage, three of them found by
a design review and each verified before adopting:

- **The overhead never reached the budget.** Today `raw` divides `time_ms`
  while only `cap` subtracts `overhead_ms` (`protocol.rs:725-727`), so the lag
  allowance bounds the ceiling and never the allocation. Subtract once, up
  front, and both derive from `usable`.
- **A divisor of 25 lands at 82%, not 90%.** Simulating
  `T' = T − k·(T/D + 0.75·inc) + inc` over 30 moves at `30+0.3`, where `k` is
  the realisation factor (our own median move was 0.58 s against a 0.479 s soft
  budget, so `k ≈ 1.2`):

  | D | k=1.0 | k=1.2 | k=1.4 |
  |---|---|---|---|
  | 16 | 87% | 92% | 96% |
  | **18** | **84%** | **90%** | **94%** |
  | 20 | 81% | 88% | 92% |
  | 25 | 75% | 82% | 88% |
  | 40 (today, before the extra ÷2) | 60% | 68% | 75% |

  **18** is the ship value: it hits the 85-95% target across the whole
  plausible range of `k` and is the least sensitive to `k` being wrong.
- **`movestogo` needs a ceiling.** Left uncapped, `movestogo 40` spreads the
  clock over 40 moves and underspends exactly as today's divisor does. Cap it
  at the same horizon.
- **`clamp` panics when `min > max`.** On a nearly-exhausted clock `reserve`
  falls under `MIN_TIME_BUDGET_NS` and `raw.clamp(MIN, reserve)` aborts, so
  `reserve` is floored first. That also preserves today's behaviour of never
  handing a timed search the untimed sentinel of zero.

The `/ 2` goes. `TM_STABILITY_PCT` and `TM_SCORE_DROP_PCT` keep their meaning
and now scale a budget that means what its doc says it means, so a settled
search takes `0.75 x raw` and an unstable one `1.6 x raw`, bounded by `hard`.

**Invariant to write down in the code, not just here:**
`HARD_BUDGET_FACTOR >= 2.08`, because the worst-case iteration scale is
`TM_STABILITY_PCT[0]/100 * TM_SCORE_DROP_PCT/100 = 1.60 * 1.30 = 2.08` and
`search.rs:495` takes `.min(info.hard_deadline)`. Below 2.08 the ceiling
silently clips the stability logic and an RR result gets attributed to the
wrong mechanism. At 4 there is headroom; if the factor is ever tuned down,
2.08 is the floor.

Why this cannot flag, argued rather than hoped: the allocation is recomputed
from the clock every move, so spending `k·(usable/18 + 0.75·inc)` against a
gain of `inc` is a contraction — `T' = T(1 − k/18) + inc(1 − 0.75k)` — with a
positive fixed point (0.45 s at `k=1.2`, `inc=0.3`). `reserve` caps any single
move at 40% of what is left, `overhead` (`TIME_OVERHEAD_MS = 50`) is subtracted
before that, and cutechess is run with `timemargin=200`. Spot checks:
`T=1000 ms` gives soft 278 / hard 380, leaving 570 ms; `T=100 ms` gives soft =
hard = 20 ms. The existing `cap` at `protocol.rs:726` is subsumed by `reserve`
and is deleted rather than kept alongside it.

New constants in `prelude.rs` beside the existing block at `:585-591`:
`TM_MOVE_HORIZON: u128 = 18` and `TM_MAX_SHARE_PCT: u128 = 40`. No variant
knowledge enters; `movetime` and `infinite` arms are untouched.

Gate, and it is a clock gate rather than a node gate:

- A 200-game self-play or short RR at `tc=30+0.3` must land median clock
  consumed per game at **85-95%** of `30 + moves*0.3`, with **zero** time
  losses. Below 85% the divisor is still too large; a single time loss fails
  the stage outright.
- `tools/speed-suite.sh` is fixed-depth and must be **byte-identical** — this
  stage may not touch the search at all.
- Fixtures 38/38; `tools/run_endgame_fixtures.sh` drives `go depth`, so it is
  also unaffected and any movement means the change leaked.
- RR against phaseE-3 on all four campaign variants, since this is the first
  stage since A-3 that is expected to move standard. This arm queues behind
  the running RR-3 crazyhouse run; everything above it is local and gates the
  commit on its own.

**Files:** `src/io/protocols/protocol.rs` (`compute_budgets`, `:707-733`),
`src/prelude.rs` (two new constants beside `:585-591`), `build-stages.sh`
(the renumbered `PHASES` table), and the plan copies. `search.rs:483-500` is
read but not edited — the stability scale keeps its meaning.

### The EBF harness — `tools/ebf-suite.sh`, built with F-3

Every remaining search stage is gated on branching factor, and the numbers
above were produced by throwaway scripts. They become a committed harness in
the same shape as `tools/speed-suite.sh`: fixed positions, pinned settings, one
machine-readable line per case.

- `tools/ebf_positions.txt` — `variant | position command | depth`, seeded with
  the crazyhouse midgame positions used above plus shogi, standard, xiangqi and
  grand cases. Crazyhouse midgame and crazyhouse startpos are **both** present:
  the pair is what proves the deficit is the hand and not the board.

  Two sampling defects in the throwaway version to fix here. The crazyhouse
  positions were all taken from our own RR games, so they are the positions our
  own play reaches — mix in an equal number from `fsf`-versus-`fsf` games. And
  the standard control was two positions, which is far too thin to carry a
  claim that broad, especially with grand sitting at 48x; take standard to at
  least 8 and keep the per-position spread in the output rather than only the
  mean.
- `tools/ebf-suite.sh BIN_A [BIN_B]` — drives each case over UCI with an
  explicit `Hash` and `Threads 1`, parses the per-iteration `nodes` from the
  `info` lines, and reports cumulative nodes at the target depth plus the
  geometric-mean iteration ratio. With `fairy-stockfish` on PATH it runs the
  same cases through it and prints the ratio, matching `Hash` explicitly —
  RR-2 already recorded that a 1 MB-versus-16 MB mismatch inflated its table by
  up to 2x.
- It must keep the engine's stdin open until `bestmove` arrives. Feeding a
  script through a pipe that closes sends `quit` immediately and truncates the
  search at depth 2 — that is how the first version of the grand measurement
  went wrong.

**Block target, stated once so the individual stages can be judged against it,
and stated intra-engine so no cross-engine unit or depth convention enters
it:** our own `nodes@13(crazyhouse-mid) / nodes@13(standard)` from **26x** to
**≤8x**, and our own crazyhouse-mid EBF from **1.87** toward our standard 1.57
— target **≤1.70**. FSF's own ratio on the same cases is 0.45x, which is the
scale of what is being chased, not a threshold. Per-stage thresholds are
relative to the previous phase binary and are stated in each stage.

### G-3 — the drop square lives in `end`

Re-verified against HEAD on 2026-08-04, and one word of the original framing
was wrong: a drop's `end!` is 0 or 1, never out of range, so every index built
from it stays inside its table. The defect is **aliasing**, not overflow — a
drop's `hist_idx` lands on the quiet-move slot `(piece, start = drop square,
end = 0|1)`, and `cont_bases` (`search.rs:1188-1207`) keys the continuation
table on `end!(previous)`, so when the *parent* is a drop every child at that
node, ordinary quiet moves included, collapses onto 2 rows per piece type
instead of `board_size`. That parent-key collapse is why crazyhouse with empty
pockets already searches 1.8x FSF's nodes.

`generate_drop_list!` (`drop_list.rs:137-144`) also writes
`enc_end!(encoded_move, square)`, and `can_checkmate` moves from bit 23 to
**bit 112** (`drop.rs:33-37`, `:56-60`). Bits 112-127 are documented unused
in every move format — the highest field in use is `captured_unmoved` at
bit 111 — so no reader can confuse it, and unlike reusing `is_initial` at
bit 35 it does not overload a named field. Redraw the format diagrams
(`moves.rs:135`, `:166-209`).

Resulting indices: `hist_idx = piece*B² + sq*(B+1)`, the diagonal, which
only a same-square quiet could collide with and none exists; and
`cont_key = piece*B + sq`, deliberately the same key a normal move landing
on that square gets, so "this piece ends up here" is one signal.

Every `end!` consumer was checked: make/undo read it only inside
QUIET/SINGLE/MULTI/CASTLING branches, the drop branches (`move_list.rs:2505`,
`:3461`) use `start!`; `format_move` prints `end` only for those same types
(`move_io.rs:70-99`); `see!` is capture-only; TT stores the raw `u128`, so
bit 112 survives. `graphics.rs:2502` starts lighting the real drop square
instead of square 0, which is a display fix.

The termination patch added a third reader of bit 23, `illegal_mating_drop!`
(`drop.rs:77-84`), which search calls at `search.rs:1565-1578` and
`adjudicate_no_move` calls in `util.rs`. All three move together in this
commit; that is the interaction the termination patch recorded in advance.

Gate: `tools/ebf-suite.sh` against phaseF-3 — crazyhouse-midgame nodes at
depth 12 must fall at least **15%**; standard, xiangqi and grand bench
**byte-identical**, which is the proof the encoder change cannot reach a
variant without hands. Speed suite flat (one extra OR in the encoder).
**Perft is the insurance** — this touches move encoding, so full suites on
all 14 embedded variants, and every one must be *identical*, drop variants
included: no move's legality changes, only spare bits.

Honest risk of a null result: today drops read whatever a well-trodden
quiet slot happens to hold; afterwards they read a permanently-zero slot
and tie, falling back to generation order. Most of this stage's gain should
come from the `cont_bases` parent-key fix rather than from drop ordering
itself. If the table barely moves, do not roll back — merge with H-3 and
measure the pair.

### H-3 — drops write history and can be killers

`search.rs:1292` becomes `let is_quiet = m_quiet!(mv) || is_drop;`. The
bonus/malus sites (`:1475-1489`, `:1507-1519`, `:1544-1556`) then reach drops and
`killer_hist[ply]` can hold one; killers are matched by `==` on the whole
`Move`, which already works for drops, and a stale illegal killer drop is
rejected by `make_move!` exactly as a stale quiet killer is.

Do this at the call site rather than by widening `m_quiet!`, which is an
exported classifier documented as "quiet and not a promotion" — widening it
silently changes every future use.

**Hard dependency on G-3.** Writing bonuses through the aliased index would
corrupt the ordering of the *non-drop* tree. Do not reorder.

Gate: `tools/ebf-suite.sh` — a further **15%** off crazyhouse-midgame nodes at
depth 12 versus phaseG-3; standard/xiangqi/grand bench **byte-identical** —
that is the variant-agnosticism proof, not a nicety. Perft cannot change
(search shape only).

**Decision point, taken here rather than guessed.** Re-measure `cont_hist`
write density on shogi and crazyhouse-midgame immediately after this stage:
drops start writing, so the table that was 0.20 writes per slot per second
gets its first honest reading. If density is still under ~1 write per slot,
L-3 fires; if the drop writes saturate it, L-3 is dropped and consumes no
letter.

### I-3 — drops enter futility and late-move pruning

Delete `&& !is_drop` from `search.rs:1308` and `:1342`, and widen the
existing `dangerous_push` guard (`:1296`) rather than inventing a new
flag — it already gates both prunings and grants an LMR discount at `:1392`,
so it is exactly the "move-count pruning must not touch this" primitive.
Two extensions: remove `&& !is_drop` from its own definition, so a dropped
pawn gets a correct `pawn_advancement` lookup once G-3 lands; and add a
royal-zone term reading `zone_attack` (`state.rs:499`) so a drop is exempt
when it bears on *either* royal zone — the enemy's (attacking/checking drop)
or our own (interposing drop). Hoist `royal*P*B` per side outside the move
loop.

`zone_attack` is the right primitive because it is derived purely from
compiled movement vectors and adjacency under an occupancy model, with no
variant knowledge, and `derive_zone_attack` fills it for *all* piece types
including pawns and royals — the pawn/royal exclusion lives only in
`king_danger!`. It is indexed on every eval, so it is guaranteed non-empty
on both load paths.

Soundness against the mate-defence objection: LMP is already gated on
`!in_check` and `futile` is only set at `!in_check` nodes, so "only defence
to check" is unreachable; "only defence to a mate threat" lies on a line
into our own king's zone, which the own-royal term covers. Futility's
premise holds because a drop is currently exactly eval-neutral, and only
strengthens if the eval block later prices held pieces above placed ones.

Declare in the commit message that royal-zone drops also inherit the `-1`
LMR discount — same mechanism, but the RR attribution should be honest.

**Fold in here, not its own letter: NMP's material guard ignores hands.**
`search.rs:1020-1021` tests `state.big_pieces[playing]`, which counts the board
only, so in a drop variant a side with an empty board and a full hand passes or
fails the zugzwang guard for the wrong reason in both directions. It belongs
with this stage because both are "the pruning rules do not know hands exist",
and it costs nothing in the agnosticism proof: `piece_in_hand` is all zeros in
a non-drop variant, so standard/xiangqi/grand stay byte-identical either way.

Expected to be the largest single lever, and the measurement supports that:
drops are 69-79% of the legal move list once hands fill, and move-count
pruning is precisely the mechanism that bounds a list that size. Gate:
`tools/ebf-suite.sh` — at least **30%** off crazyhouse-midgame nodes at depth
12 versus phaseH-3, with shogi moving in the same direction;
standard/xiangqi/grand bench byte-identical. Risk is tactical blindness, not
game truth, so perft cannot catch it: run a depth ladder on the drop-mate
positions in `res/perft/shogi.perft` and require the mate score at the same
depth as the phaseH-3 binary, plus the endgame fixtures. If shogi regresses
while nodes collapse, widen the zone test before abandoning the stage.

### J-3 — drops move to the quiet LMR curve

Remove `$is_drop` from the capture-curve test in `reduction!`
(`search.rs:1765-1775`) so drops select `quiesce_lmr`. The capture curve
(base 1.0, div 4.0) reduces about half as much as the quiet curve
(0.75, 2.25); drops neither remove material nor resolve tension, so they
are under-reduced by accident. Safe only after H-3, because the history
adjustment at `:1382-1388` must read a real drop statistic.

Its own stage rather than folded into I-3 because the two act on different
axes — I-3 removes moves from the tree, J-3 shrinks the ones that stay —
and folded together a regression is unattributable. If the RR budget only
supports four search stages, merge this into I-3 rather than dropping it.

Gate: `tools/ebf-suite.sh`, at least **10%** off crazyhouse-midgame nodes at
depth 12; standard bench identical; watch the re-search rate, since
over-reduction shows up as nodes *rising*.

### K-3 — checking drops in quiescence search

Qsearch generates drops that give check, bounded by a check-ply budget.

Build the candidate list by inverting the query rather than filtering a
full drop list: `relevant_attacks[1 - playing][R]` (`state.rs:494`) already
holds every `(piece, from, vector)` that could attack royal square `R`.
Keep entries whose piece belongs to the side to move, whose hand count is
positive, and whose `from` square is empty; dedupe on `(piece, from)`; emit
through a new `generate_square_drops!` extracted from `generate_drop_list!`
so forbidden zones, hand counts and every allower/stopper constraint
(nifu, last-rank bans, uchifuzume flagging) are enforced by the same code
the main generator uses. That extraction must be mechanical — perft
identical after the split alone, before the new generator exists.

`relevant_attacks` gives candidates, not confirmed checks. Confirm after
the fact: once `make_move!` succeeds, `if !is_in_check!(state.playing,
state)` undo and continue. Reusing `is_in_check!` is cheaper and far safer
than validating an attack vector from an empty origin square, where
`virgin_board` and the leg filters would read state the dropped piece does
not have yet.

**Two hazards this stage creates, and the earlier wording of them was wrong.**
`search.rs:689` calls `victim_value!`, which ends in `unreachable!()` for any
non-capture (`move_ordering.rs:89`), and `search.rs:678-681` skips everything
scoring below `WINNING_CAPTURE_SCORE`, which would silently discard every drop.
Both sit inside the `if !in_check {` block opened at `search.rs:687`, and today
qsearch only ever holds drops *while in check* — so neither is reachable at
HEAD. They are not latent release-mode panics waiting to fire; they become
reachable the moment this stage puts drops in a non-check qsearch list, which
is why the `!m_drop!(mv)` exemptions belong in this commit and not an earlier
one. Verified 2026-08-04.

Bound the explosion with a `qdepth` parameter (new `QS_CHECK_PLIES` beside
`MAX_CHECK_EXTENSION`): entry points pass 1, the recursion passes
`saturating_sub(1)`, generation is gated on `qdepth > 0`. There is no way
to derive this from `state.search_ply` — qsearch does not record where it
was entered.

Gate is **different from the other four**: this stage buys horizon accuracy
and spends nodes. (a) crazyhouse-midgame nodes at depth 12 may rise by at
most 25% versus phaseJ-3, (b) the shogi/crazyhouse drop-mate positions must
find their mate at a strictly lower depth than phaseJ-3, (c) standard NPS
regression under 3% (the generator must be provably skipped there), (d) RR
decides. FSF searches drop checks in qsearch and still needs *fewer* nodes in
crazyhouse than in standard, so `tools/ebf-suite.sh` stays the reference and
the block target — our own crazyhouse-mid/standard node ratio ≤8x — still has
to hold after this stage.

Highest correctness risk of the five: full perft after the split alone and
again after the generator lands, debug-assertion runs, endgame fixtures
(which now traverse a qsearch that makes and unmakes drops). If the node
ceiling is breached, the first knob is a cheap safety filter — require the
drop square to be undefended by the opponent or defended by us, two
existing `is_square_attacked!` calls — added as a follow-up commit so it
stays attributable.

### Found while verifying, not stages

- **`cont_hist` is O((P*B)^2) and too sparse to learn anything in shogi.**
  `state.rs:948` and `search.rs:156` allocate `2 * cont_dim^2` `i16` where
  `cont_dim = pieces.len() * board_size`: standard 2.25 MiB, crazyhouse
  6.25 MiB, **shogi 19.62 MiB**.

  The allocation itself is *not* the problem — measured, and my first note
  claiming "20.6 MB zeroed every `clear_search`" was wrong. `vec![0i16; n]`
  uses `alloc_zeroed`, so a large block comes back as kernel zero pages and
  is faulted lazily: 300 `go depth 1` searches take 0.13 s in standard and
  0.14 s in shogi, i.e. ~0.45 ms each in both.

  The problem is **density**. Approximating one history write per node
  (real writes are fewer on cutoffs and several on the malus loop, so this
  is order-of-magnitude), a 1-second search writes 1,721,482 nodes into
  standard's 589,824 slots per ply-table — about 2.9 writes per slot, a
  saturated table carrying real statistics — versus 1,033,009 nodes into
  shogi's 5,143,824 slots, about **0.20 writes per slot**. Most of the
  shogi table is never touched, so continuation history is largely reading
  zeros in the variant whose move ordering needs it most, and every probe
  that does land is a cold miss in a 19.6 MiB working set.

  This compounds with G-3: while the parent is a drop, the parent key
  collapses to 2 of 2268 values, so two rows are hammered and the rest is
  dead. **Promoted to a conditional stage, L-3**, on the RR-3 evidence that
  the deficit is branching factor rather than eval; the decision is taken on
  a re-measurement after H-3, when drops finally write history.
- **NMP's material guard ignores hands.** `search.rs:1020-1021` tests
  `big_pieces[playing]`, board only, so a side with an empty board and a
  full hand passes or fails for the wrong reason in both directions.
- `MIN_LMP_DEPTH` (`prelude.rs:252`) is defined and never read.

### L-3 — continuation-history density (conditional on the H-3 measurement)

Fires only if `cont_hist` write density after H-3 is still below ~1 write per
slot. Direction, not yet a design: reduce the parent axis from
`(piece, square)` to `piece`, which takes shogi from 19.62 MiB to
`28 * 2268 * 2 * 2` bytes ≈ 248 KiB — 80x denser and back inside L2. Do not
fix the shape before the access pattern is real; that is the whole reason this
sits after H-3 rather than beside G-3.

Gate: `tools/ebf-suite.sh` on shogi and crazyhouse-midgame; standard bench
must move by less than 1% in nodes, because standard's table is already
saturated at 2.9 writes per slot and has nothing to gain — a large standard
swing means the reduction lost information rather than density.

### M-3 — grand: find out why it searches 48x FSF's nodes

**Diagnosis first, no design committed.** Grand is a campaign variant, has no
drops, and is the worst variant measured: 4 912 871 nodes to depth 11 against
FSF's 51 285, iteration ratio 2.66 against 1.95. Nothing in the search block
touches it, so it would sail through F-3..L-3 unchanged.

What is already known and must not be re-derived: root move generation is
correct (perft(1) is 65 for both engines); the board width is not the cause
(capablanca at 10x8 is 2.2x, janus 0.8x, modern 1.7x, minixiangqi 1.2x,
los-alamos 1.4x — grand is an outlier, not the top of a trend); and the
iteration log shows a near-flat eval oscillating `5, 33, 5, 36, 5, 46, …` with
the best move flipping between back-rank rook shuffles until depth 8, against
FSF settling on `c3c4` at depth 3 and climbing `92 → 176`.

That points at eval or parameter derivation rather than search, but it is a
hypothesis. The stage begins by isolating it: compare `debug-headless
evaluate` against FSF on constructed grand positions with known imbalances,
check whether `res/param/grand/latest.param` is degenerate by diffing a fresh
`debug-headless derive` against it, and check whether `promote to captured`
(the one rule grand declares that capablanca and janus do not) reaches the
promotion-gain terms that feed the pawn scores and the passed-pawn tables.
Only then is a fix scoped; if the cause turns out to be variant-specific
config data rather than engine code, this becomes a `.conf`/param correction
and consumes no letter.

Gate: whatever the fix is, `tools/ebf-suite.sh` grand nodes at depth 11 must
fall by at least half, and standard/shogi/crazyhouse must be byte-identical
unless the cause is genuinely shared — in which case say so explicitly and
re-gate.

### N-3 — phase reference from the board at first play

Was F-3, then K-3. Correctness, and it runs first *within the eval block*
because
every later eval measurement is read through the taper: measuring a
king-safety change while crazyhouse and shogi sit at blend weight 0.74 and
sliding means measuring it again afterwards.

`derive_eval_parameters` (parameters.rs:726) sets

```
opening_score = round(average_big_value) * pieces.len()
endgame_score = round(average_big_value) * 5
```

`pieces.len()` counts piece **types**, both colors, so declaring promoted
types raises the opening threshold without adding a single piece to the
starting army. `game_phase_score!` meanwhile sums the actual army. The two
no longer meet:

| variant | opening_score | startpos phase |
|---|---|---|
| standard | 4140 | Opening |
| grand | 8400 | Opening |
| xiangqi | 3332 | Opening |
| crazyhouse | 8280 | **Middlegame** |
| shogi | 7644 | **Middlegame** |
| minishogi | 4640 | **Middlegame** |

The split is exactly the variants that declare promoted types. Crazyhouse
carries the identical starting army to standard — its startpos phase score
is 6664 against standard's 6622 — yet its threshold is exactly double
(8280 = 2 x 4140) because the promoted types doubled the type count. A
crazyhouse game therefore starts at blend weight
`(6664 - 2070) / (8280 - 2070) = 0.74` and slides toward endgame from
there, discounting `king_safety` and the whole opening half of the taper
from move one. Shogi and minishogi are the same. Note this is partly
self-inflicted: `49bf8d4` had to add crazyhouse's promoted types to fix the
pocket, and doubled its opening threshold as a side effect.

The threshold should name the army the variant starts with, not how many
names that army goes by. Three corrections to the obvious fix.

**`initial_setup` alone is not the starting army.** It is parsed from the
startpos BOARD field only (game_io.rs:1159-1170), so in a setup variant
every piece that starts in hand has an empty `initial_setup`. Sittuyin
starts `8/8/4pppp/pppp4/4PPPP/PPPP4/8/8 w KSSFRRNN/kssfrrnn` — only pawns
on the board — and janggi starts with `HHEEQ/hheeq` in hand.

**The initial hand is not the starting army either.** A hand can be a
*menu* rather than a *reserve*. Chess with Different Armies is naturally
expressed here as a setup variant holding every selectable army in hand at
once, with the setup patterns locking out the rival armies as soon as the
first piece of one is placed — so a side deploying one of three armies
holds 3x its real army at startpos. Counting the hand would overshoot by
the number of armies on offer, which is the current defect with a bigger
multiplier.

**The threshold must be a fraction of the army, not the army.** The
comparison is `phase_score > opening_score`, so a threshold equal to the
full army is never exceeded and every variant reads MIDDLEGAME from move
one — today's bug with the sign flipped. Standard sits at 0.625 of its army
(4140 of 6622) and 0.26 for endgame (1725), and standard is the only
variant whose strength is validated, so calibrate to it.

What all three corrections point at: the reference is **the board when the
game proper begins**, and hands never enter it.

```
army = game_phase_score!(state) at the moment play begins
       /* config startpos board for a normal variant,   */
       /* the deployed board when SETUP ends otherwise  */
opening_score = 5 * army / 8
endgame_score = army / 4
```

| variant | army | opening now | opening new | endgame now | endgame new |
|---|---|---|---|---|---|
| standard | 6622 | 4140 | 4139 | 1725 | 1655 |
| crazyhouse | 6664 | 8280 | 4165 | 2070 | 1666 |
| shogi | 5662 | 7644 | 3539 | 1365 | 1415 |

Standard is unchanged to within a unit — that is the point of the
calibration. Sittuyin and janggi land on the same value either way, since
their whole hand deploys. CwDA lands on the army actually chosen.

**Prerequisite: SETUP currently cannot end in a menu variant.**
move_list.rs:2568 leaves SETUP only when `piece_in_hand[0]` and
`piece_in_hand[1]` are both entirely empty. Locked-out armies stay in hand
forever, so a CwDA game would never leave the setup phase at all —
independent of any eval question. The general predicate is *no side has a
legal placement*, which is equivalent for sittuyin and janggi (empty hand
implies no placement) and correct for a menu. That makes it a move-gen
rule, the same shape as the stand-off restoration in plan 08-12, and it is
also the moment at which the reference army above should be captured.

**Not affected, checked:** `setup phase` is rule bit 6 and `drops` is bit
3; sittuyin and janggi declare only the former. E-3's hand king-danger term
and the capture-to-hand material accounting both gate on `drops!`, so a
menu hand contributes to neither.

As-built shape:

- `opening_score`/`endgame_score` stay in the `.param` schema as the
  static default, derived from the config startpos board plus its initial
  hand — correct for every shipped variant, so no regen here and no
  collision with Q-3's atomic regen.
- The same two values also become `State` fields, initialised from the
  statics at load and overwritten by a capture when SETUP ends, so a menu
  hand cannot poison them. They cannot live only in `StaticState`:
  `static_mut` is `Arc::get_mut(..).unwrap_unchecked()`, undefined
  behaviour once SMP threads hold clones. `Snapshot` carries the army
  scalar for undo, plan-12 pattern.
- SETUP exit predicate becomes "no side has a legal placement" rather than
  "both hands empty" (move_list.rs:2568).

Verify: startpos reads Opening for all five campaign variants; **standard
bench byte-identical** — its thresholds move by one unit, so any tree
change at all means the derivation is wrong; sittuyin and janggi still
terminate setup and still pass their fixtures; perft suites; RR vs the last
search-block binary.

Scope honestly: this recovers the 0.74 discount, not the 5x gap. Crazyhouse
exposure would go from 126 to roughly 170 against an FSF-equivalent 867.
Worth a stage, not a substitute for one.

### O-3 — royal exposure PST replaces castling knowledge

**Re-argued 2026-08-04; the original justification does not survive.** This
stage was scoped to close the crazyhouse exposure gap. It cannot: the pressure
integral below is built from `initial_setup` and `zone_attack`, and both are
byte-identical between standard and crazyhouse, so it derives the *same* royal
PST in both. Whatever is variant-dependent about king safety in a drop variant
has to come from P-3, not from here.

Two justifications that do hold, both verified rather than argued:

- **The royal opening PST today carries no file information at all.**
  `parameters.rs:560-562` special-cases it to
  `(0..board_size).map(|square| -((square / files) as f64))` — a pure rank
  gradient. Confirmed in `res/param/standard/latest.param`, whose royal opening
  block is `24 24 24 24 24 24 24 24 / 17 17 … / … / -24 -24 …`: every file the
  same value, so g1 == e1 == a1 for the king. Every variant is in this state,
  and shogi, xiangqi and janggi have no royal file preference of any kind.
- **`State.has_castled` is not FEN-serialized**, so `castling_bonus!` is
  already wrong for every FEN-loaded position — fixtures, datagen and the
  position sweeps all reach the engine that way. Deleting it removes a term
  that is silently incorrect on one of its two load paths.

Gate accordingly: **standard SPRT non-negative** plus a per-variant PST dump,
*not* the exposure table, which this stage was never able to move.

- Move `derive_zone_attack` call (parameters.rs:1577) to run BEFORE PST
  derivation — top of `derive_eval_parameters`; ensure BOTH load paths
  (fresh derive AND tuned-param load, game_io.rs:2032) have zone_attack
  ready before PSTs (miss one → royal PSTs silently zero; guard on
  `zone_attack.is_empty()`).
- Replace royal opening branch of `derive_pst` (parameters.rs:560-562):

```
pressure(s) = Σ over enemy piece type t (non-royal, non-pawn)
              Σ over f in set_indices!(initial_setup[t])
              zone_attack[(s * piece_count + t) * board_size + f]
score(s) = -pressure(s)
```

  computed in the white frame against the black initial army (black
  mirrored afterward, as today), fed through existing mean-centering +
  amplitude-24 normalization. Castling destinations now score well only
  when genuinely less exposed. Endgame royal PST (centralization)
  unchanged.
- Delete: `castling_bonus!` (evaluation.rs:163-177) + its
  `evaluate_position!` line (:625); `State.has_castled` (state.rs:611 +
  clone/from_statics/reset) + writes (move_list.rs:2650, :3477). Scalars
  `castled_bonus`/`castling_rights_bonus`: stop reading here, delete in
  Q-3.
- Keep: `king_shelter!`, quadratic `king_danger!`, `pawn_shield!`,
  `open_shield!`. No sub-ablation arms — one change per stage.

Verify: dump derived royal opening PSTs and require they stop being a bare
rank gradient — standard (g1/c1/b1 ≥ e1, corners high), shogi (low ranks,
edge-file bias), xiangqi (palace gradient); non-degenerate PSTs for all
variants in the derive log. Standard carries the risk here — this deletes
castling knowledge from our strongest variant — so SPRT standard against N-3
before the RR, and the bar is non-negative rather than positive.

### P-3 — drop-aware king-zone porosity — promoted from conditional to planned

**Promoted 2026-08-04.** It was conditional on F-3/G-3 failing to close the
exposure gap; O-3 has since been shown structurally incapable of closing it
(its integral is variant-invariant), so this is now the *only* candidate that
produces a variant-dependent king-safety multiplier. It is planned, and the
conditional slot it vacated is taken by the held-piece valuation question
below.

The part of the gap E-3 did not touch. FSF charges 438 cp for the exposed
king in crazyhouse against 100 cp in standard — identical board, empty
hands on both sides — a 4.4x multiplier that comes from the rules alone.
Our number is the same in both variants: 198 in standard, and 126 in
crazyhouse, so relative to FSF's own standard figure we *over*charge exposure
in chess and undercharge it in crazyhouse. The defect is not that king safety
is too small, it is that it is **variant-invariant**. Note also that E-3's
worst row is `-/rb` at 15%, not the empty hand — the linear per-held-piece
term already covers large hands; what is missing fires the moment drops become
legal at all, which is exactly this term. E-3 added danger proportional to
what is *in* the hand; this is the danger that exists because material
*will* pass through hands at all.

Mechanism: where drops are legal an empty square in the royal's zone is a
landing pad, not merely an empty square, and `derive_zone_attack`'s
travel-based occupancy model cannot express that — every entry in it
assumes the attacker must reach the square by moving. The candidate term is
therefore a zone-porosity count (`adjacency_mask[royal] & !occupied`,
already the complement of what `king_shelter!` walks) weighted by the best
droppable attacker, gated on `drops!`.

**The form is deliberately not fixed here**, but the stage is. Rerun the
exposure table after N-3 and O-3 land, expressing each engine's charge as a
fraction of that engine's own knight rather than through a cross-engine
scalar, and design the term against what that shows. The measurement that
decides the *form* is the standard-versus-crazyhouse ratio within each engine:
FSF's is 4.4x, ours is 0.64x, and any candidate term has to move ours toward
FSF's without touching standard at all.

### [conditional] Held-piece valuation

Takes the conditional slot P-3 vacated, and fires only on a measurement.
`util.rs:200-213` and `:543-557` price a piece in hand at exactly
`p_ovalue!`/`p_evalue!` — its value on the board — so a held queen is worth
9.4 held pawns in crazyhouse.

**Do not act on the ratios recorded in the RR-2 section.** They are the
*endgame* row of `res/param/crazyhouse/latest.param` answering a midgame
question, and the two claims attached to them are in tension: "held is
54-154 cp above board" is an additive premium, which by itself compresses the
ratios toward the pawn, so "compressed toward the pawn" may be the same fact
counted twice. Measure first: compare each engine's held-piece value to its
own knight, in the phase the games actually reach.

### R-3 — drop-variant NPS (conditional, and it must land last)

Our NPS falls 48% from standard to crazyhouse where FSF's falls 22%. Three
candidate causes, two of which no other stage touches:

- **Pruned moves are still generated, encoded and scored.** Futility and LMP
  `continue` rather than `break` (`search.rs:1304-1313`, `:1336-1349`), so the
  O(k·n) `pick_by_score!` selection cost over a 100-150 move list survives
  I-3 entirely. n=150 against n=35 is roughly 18x the selection work per
  all-node.
- **`repetition_scan_bound` walks up to 64 snapshots per node in drop
  variants** (`termination.rs:810-822`), against single digits in standard.
  C-3 chose the full-history bound deliberately and the reasoning still holds
  — capture-then-drop restores an identical position — but the *cost* is
  addressable without changing the bound: a `Vec<u64>` of truncated hashes
  turns 64 strided reads across ~96-byte `Snapshot`s into ~8 cache lines,
  verified against the full `u128` only on a hit. C-3 removed a parallel hash
  vector on the grounds that `Snapshot.position_hash` already held it; that
  was right for correctness and wrong for locality.
- The `cont_hist` working set (6.25 MiB crazyhouse, 19.6 MiB shogi against
  standard's 2.25 MiB) — already L-3's target, which is why this stage must be
  measured *after* L-3 rather than before it.

**Deciding measurement, taken after J-3:** our own
`nps(crazyhouse) / nps(standard)` on one binary and one harness, against FSF's
own 0.78 (1.27/1.63) on the same axis. **≥0.70 drops this stage.** Below 0.70,
take a sampling profile of `bench crazyhouse 12` against `bench standard 13`
and scope the stage only to sites whose crazyhouse self-time share exceeds
their standard share by ≥8 percentage points — that keeps it a speed stage and
stops it becoming a second search stage.

Gate if it lands: NPS only, with **byte-identical node counts in every
variant**. A pure speed stage must not change the tree; that is what makes it
safe to land after everything else.

### Q-3 — simplification: pawn collapse + schema shrink + pair ablation

One stage, one param regen, one RR arm. These were separate stages while
the pawn work was a strength bet; it is not — it addresses none of the
measured failures — so it merges with the schema shrink it was only ever
deferring deletions into. Expectation for the whole stage is ~0 Elo and the
payoff is less machinery.

#### Pawn collapse

Replace the 7-sub-term scoring pass (evaluation.rs:439-563) with ~35
lines. Per pawn-like piece at `entry = index * board_size + square`, with
`q_o/q_e` = existing `pawn_passed_opening/endgame` tables renamed
`pawn_quality_*` (derivation unchanged: `passed_scale/100 *
promotion_gain * adv^2/256`):

```
own_blocked = friendly pawn on pawn_path_mask[entry]
contested   = enemy pawn on pawn_interference_mask[entry]
supported   = friendly pawn on pawn_support_mask[entry]

award = own_blocked ? -q/4          /* doubled/blocked malus */
      : contested   ?  q/4          /* obstructed path       */
      :                q            /* clean path = passer   */
if supported && award > 0 { award += award / 2 }
```

White-minus-black into the (opening, endgame) pair; PTable caching
unchanged (reads only pawn placements + static masks, pawn_hash key stays
valid). Passed-board pre-pass collapses into the per-pawn `contested`
test (one pass instead of two).

- `derive_pawn_like` (parameters.rs:819-873): drop the
  `start_count >= PAWN_MIN_START_COUNT` conjunct — geometry alone
  (forward-only + single quiet step + short range). Checked against
  shipped armies: gold/silver/advisor/elephant/ferz still excluded
  (backward steps), lance/shogi-knight excluded (range/no step); minishogi
  pawn + minixiangqi soldiers now qualify — intended.
- Keep: `pawn_advancement` (dangerous-push search.rs:1300-1304),
  `derive_pawn_stop`, PTable, `pawn_board`, `hash_pawns`.

#### Schema shrink and dead-weight purge

Delete end-to-end, in the same commit as the pawn collapse so there is one
regen and one token-count change: the pawn scalars the collapse stops
reading (`pawn_connected_opening/endgame`, `pawn_doubled_penalty`,
`pawn_isolated_penalty`, `pawn_backward_penalty`,
`pawn_passed_support_opening/endgame`), their masks
(`pawn_backward_mask`, `pawn_support_offsets`, derivation at
parameters.rs:1371-1396), the castling scalars O-3 stopped reading
(`castled_bonus`, `castling_rights_bonus`), and the vestigial
`mobility_opening/endgame`. Sites: StaticState (state.rs:538-539,
547-557), `State::new` inits, `derive_eval_scalars`
(parameters.rs:625-643), `parse_tuned_parameters` (game_io.rs:318-336),
`export_tuned_parameters_file` (game_io.rs:412-432), `export_theta`
(tuning.rs:663-683). `PARAM_SCALAR_TAIL` 19 → 10 (tempo, pawn_shield,
king_shelter, king_danger_scale, open_shield, imbalance_major/minor,
pair_bonus_value, passed_scale_opening/endgame). Delete
`PAWN_MIN_START_COUNT`.

#### Pair-bonus ablation

SPRT arm without the term on the lean baseline; if neutral, delete the term
and the incremental `pair_score` machinery (state.rs:350-392 hooks,
verify_game_state check); if it loses strength, keep it — it is incremental
and ~free, so "keep" is the expectation and the measurement is the point.

#### Atomic param regen

Parse asserts an exact token count and embedded params are compile-time
`include_dir!`, so: rm `res/param/*/latest.param` → `cargo build --release`
(empty embed, falls to fresh derive) → `debug-headless derive` (walks every
config, exports) → `cargo build --release` (re-embed) → single commit of
code plus regenerated params.

Verify: `debug-headless evaluate` monotonicity on passed/blocked/supported
FEN triples across geometries (standard, berolina, shogi); every variant
loads (`debug-headless state <variant>` over the config list); perft
suites; bench; SPRT against the P-3 winner. Expected ~0 — everything here
is either behaviour-neutral deletion of already-unread terms or the
deliberately-measured pair arm, so a large SPRT swing is a leaked bug, not
a result.

## Ordering, dependencies, RR campaign

```
A-3 → B-3 → C-3 → D-3 → E-3 → [termination patch] →
       └── speed block ──┘      cherry-picked onto A-3..E-3

  → F-3 → G-3 → H-3 → I-3 → J-3 → K-3 → [L-3] → M-3 →
    time  └──────── drop block ────────┘  cont   grand
                                          hist   diag

  → N-3 → O-3 → P-3 → [held pieces] → [R-3] → Q-3 → final RR
    └───────── eval block ─────────┘    nps    simplify
```

**Letters shifted by one on 2026-08-04** to make room for F-3 (time
management) at the front, and three stages were added. Nothing past E-3 had a
branch, so the renumbering costs only the `PHASES` rows in `build-stages.sh`.
Old → new: F→G, G→H, H→I, I→J, J→K, K→N, L→O, M→P, N→Q. New: L-3
(continuation-history density, conditional), M-3 (grand diagnosis), R-3
(drop-variant NPS, conditional, lands after Q-3's regen only if the tree is
already frozen — see its own gate). P-3 was promoted from conditional to
planned; a conditional held-piece valuation stage takes the slot it left.

- A-3 prerequisite for every gate (bench + seed). The RR-1 correctness
  fixes are cherry-picked onto phaseA-3..phaseD-3 so every binary shares
  one rule set; the drop-variant RR must be re-run from rebuilt binaries,
  and the pre-fix crazyhouse and shogi standings are void.
- **The termination patch landed after E-3 and takes no letter**, on the
  RR-1 precedent: it is rule correctness that predates the ladder, present
  in phaseA-3, and it changes shogi and xiangqi tree shape. Cherry-picked
  onto phaseA-3..phaseE-3 so no phase binary plays a different rule set.
  The port to phaseA-3/phaseB-3 is small but not automatic — their search
  probe still reads `position_hash_map` (C-3 replaced it with
  `has_repetition`), so the gate stays as it is on those branches and only
  the returned score changes. Every phase binary rebuilds; the running
  shogi RR is void for shogi and xiangqi once it lands.
- B-3 before C-3: C's NPS measured on lean search. C-3 before D-3: keeps
  speed-suite attribution clean.
- **Decisions taken 2026-08-04.** The RR-3 crazyhouse run stays up to its full
  1024 rounds rather than being cut short, so the VPS is busy and F-3's RR
  queues behind it; F-3 is still written, built and gated locally in the
  meantime, since every gate except the RR arm is local. The time fix ships
  **as F-3 only** and is *not* cherry-picked onto phaseA-3..E-3 — unlike the
  termination patch it is a strength change, not rule correctness, so
  back-porting it would leave F-3 with nothing to measure against. A-3..E-3
  therefore keep spending half their clock, which is consistent within RR-3
  and consistent with every earlier RR in this iteration.
- **F-3 first.** It is the cheapest stage in the ladder, it is worth more
  than any single drop stage (53% of the clock against FSF's 96%), and it is
  the only one that moves every variant. Running it first also means every
  later RR is played by an engine that uses its time, so later stages are
  measured on a realistic time control rather than on a self-handicapped one.
- **Drop block before eval block.** RR-3 measured crazyhouse-midgame at 39x
  FSF's nodes at depth 13 with an iteration ratio of 1.90 against 1.38, while
  standard sits at 0.89x and xiangqi at 0.6x. That is the dominant cause; the
  eval gaps are real but second-order and smaller than this plan used to
  claim once the 1.98 anchor is discarded. Interleaving would also break both
  gates: the phase-reference stage selects the larger opening futility margins
  and pushes node counts the wrong way, while its own gate is standard-bench
  identity, independent of the EBF suite.
- Drop block internal order is dependency, not taste. G-3 before H-3 is
  **hard**: writing history through the aliased index would corrupt the
  ordering of the non-drop tree. H-3 before I-3 is soft but strong — LMP
  prunes the ordering tail, so pruning noise-ordered drops would both
  under-report I-3's value and raise its tactical risk. H-3 and I-3 before
  J-3, since the reduction is corrected by history. I-3 and J-3 before
  K-3, which *adds* nodes and would be unreadable before the tree is
  bounded.
- L-3 and M-3 are both conditional and both are decided by measurement, not
  argument: L-3 by the `cont_hist` density re-reading after H-3, M-3 by
  whatever the grand diagnosis finds. Either may consume no letter.
- Eval block internal order: N-3 (phase reference) first as correctness, O-3
  (royal PST, re-argued as "the royal opening PST is a bare rank gradient")
  next, P-3 (porosity, now planned rather than conditional) after it, the
  conditional held-piece stage last. Q-3 (simplification) closes the ladder,
  and the conditional R-3 (NPS) is measured after J-3 but landed after Q-3,
  since its gate is byte-identical node counts and that is only meaningful
  once the tree has stopped moving.
- E-3 through the held-piece stage hold the `.param` schema fixed so binaries
  stay param-compatible for RR; all schema change and the single regen land
  together in Q-3.
- RRs on the VPS, **capped at `-concurrency $(cores/2)`** so engines are not
  fighting for cores at a wall-clock time control: (1) F-3 against
  phaseE-3 on all four campaign variants, since it is the first stage since
  A-3 expected to move standard, (2) drop block after K-3, (3) eval block
  after O-3, (4) final RR after Q-3 vs FSF 1700-1900, adding 2000+ anchors
  once 1900 is beaten.
- **Standing measurement rules, rewritten 2026-08-04 to be unit-free.**
  - *Search gates are intra-engine.* The primary number is our own
    `nodes@D(drop variant) / nodes@D(standard)` on one binary and one harness
    — 26x today against FSF's 0.45x. It needs no matched `Hash`, no shared
    depth convention, and no assumption that the RR's `UCI_LimitStrength`
    opponent is the same engine as the full-strength one on the bench. Keep
    the cross-engine column for scale, never as a threshold.
  - *Eval gates are ratio-to-own-knight.* Express each engine's term as a
    fraction of that engine's own knight and compare fractions. **No
    cross-engine unit multiplier appears in a gate again.** Both anchors this
    plan has used are unsound: 1.98 came from one king-exposure position, and
    0.74 came from three material positions spread across a 20% band that mix
    material with positional compensation. `parameters.rs:677-680` already
    normalises the cheapest piece to exactly 100, so a pawn is 100 in both
    engines and the residual is not a unit difference at all.
  - *Harness hygiene.* Drive both engines through their protocol with an
    explicit matching `Hash`; never `debug-headless search`, whose 1 MB TT
    against FSF's 16 MB default inflated an earlier table by up to 2x; never
    through a pipe that closes stdin, which sends `quit` mid-search and
    truncated the first grand measurement at depth 2; and never sample
    positions only from our own games, which selects for the positions our own
    play reaches.
  - In all cases, beating the last build while sitting far under FSF is not a
    result.

## Risks and rollback

- **A-3**: near-zero; seed env unset ⇒ behavior identical.
- **B-3**: search-shape only; node cap +16%; rollback = drop branch.
- **C-3**: HIGHEST correctness risk (make/undo + game-truth path). Full
  perft both widths, debug-assert runs, capture-containing sennichite
  fixture, perpetual fixture. Map restorable wholesale; width revert is
  one line.
- **D-3**: ordering-preservation is the risk; node-count identity gate
  makes any mistake visible immediately. Revert = drop branch.
- **E-3**: done; derive-order risk removed by reducing `zone_attack_best`
  inside `derive_zone_attack`.
- **Termination patch**: touches the one path every variant's result flows
  through, and two of its defects were found only by diffing against a
  pre-change binary rather than by reading. So the gate is differential,
  not inspectional: build the parent commit to a scratch binary and require
  byte-identical bench node counts on every variant that declares no
  `perpetual` rule, plus an explicit expected-result fixture for every
  behaviour the patch adds. A rule whose subject changes must be shown to
  produce the same result as before on every shipped config, since all of
  them declare `draw` where the subject was ambiguous. Rollback is a branch
  drop — nothing is pushed.
- **F-3**: the only stage whose failure mode is losing a game outright rather
  than playing it worse. A time forfeit is not a strength regression, it is a
  zero, so the gate is zero forfeits over ≥500 games and not a percentage.
  The three guards are independent — the fixed point at `time = 6.25 * inc`,
  the `reserve` share cap, and `overhead` — so a single one being miscoded
  still cannot flag. Second risk is leaking into search: the speed suite is
  fixed-depth and must be byte-identical. Rollback = drop branch, and the
  formula is four lines.
- **G-3**: move encoding, so perft-critical. Every suite must be
  *identical* on every variant — no legality changes, only spare bits. The
  subtle failure is a missed `end!` consumer that reads a drop; the
  audited list is in the stage, and the termination patch's
  `illegal_mating_drop!` reader is now part of it. Rollback = drop branch.
- **H-3/J-3**: search shape only, perft cannot change. The
  agnosticism proof is byte-identical bench on standard/xiangqi/grand; if
  that moves, a non-drop path was touched.
- **I-3**: tactical blindness, invisible to perft. Caught by the shogi
  drop-mate depth ladder and the endgame fixtures, arbitrated by RR. If
  shogi regresses while nodes collapse, the royal-zone guard is too
  narrow — widen it before abandoning.
- **K-3**: highest risk of the drop block. Touches move generation
  (perft after the mechanical split *and* after the generator), adds nodes
  by design, and makes two currently-unreachable hazards reachable, which
  must be fixed in the same commit (`victim_value!` is `unreachable!()` on a
  non-capture; the SEE-band skip silently discards drops — both are
  `!in_check`-gated today). Node ceiling breach is handled by a follow-up
  safety filter, not a revert.
- **L-3**: conditional; the risk is doing it before the density
  re-measurement and trading real statistics for cache locality that the
  drop stages had already bought. Standard bench moving more than 1% is the
  tell.
- **M-3**: the risk is shipping a fix before the diagnosis names a cause.
  Grand is one variant out of 38 and 48x is a startling enough number that
  the first plausible story will be tempting; require the same evidence
  standard as the termination patch, which means a differential measurement
  against a scratch binary, not a reading of the code.
- **N-3**: touches the phase taper, so it touches every eval in every
  variant. Standard must come out byte-identical — that is the gate, not a
  nicety. Second risk is the SETUP exit predicate: get it wrong and
  sittuyin/janggi either never leave setup or leave it early, both caught
  by their fixtures. Rollback = drop branch.
- **O-3**: derive-order risk (zone_attack before PSTs on BOTH load paths —
  miss one and royal PSTs are silently zero; guard on
  `zone_attack.is_empty()`), plus castling-variant strength risk in our
  strongest variant. PST sanity dump per variant, SPRT standard first. Its
  expected size shrank with the anchor correction, so a large measured swing
  in either direction deserves suspicion before celebration.
- **P-3**: now planned rather than conditional, which moves the risk from
  "inventing a stage" to "inventing a *term*". The measurement fixes the
  target (our standard-to-crazyhouse exposure ratio 0.64x against FSF's 4.4x)
  but not the mechanism; if no candidate moves that ratio without touching
  standard, drop the stage rather than shipping a term that only looks right.
- **R-3**: the risk is letting a speed stage change the tree. Its gate is
  byte-identical node counts in every variant, which is why it lands after
  Q-3 and not next to the pruning stages that would mask it. Second risk is
  scoping it from argument rather than a profile — the ≥8-point self-time rule
  exists to stop that.
- **Q-3**: strength risk from the lost isolated/backward pawn signal;
  masks and scalars survive until this stage, so re-enabling any single
  term is a small diff. Param regen schema mismatch panics loudly at load
  (exact-length assert) and cannot fail silently; the regen sequence is
  the rollback boundary.

## Out of scope (this iteration)

- Texel tuning / datagen (user decision — next iteration, after schema
  settles).
- Battery/pair generalization beyond the ablation.
- SMP work, ponder, seldepth.
- Any leaf evaluator walking compiled move vectors (permanently rejected).
