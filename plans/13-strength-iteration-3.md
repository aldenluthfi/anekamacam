# Strength Iteration 3 — Structural Speed + Agnostic Eval Simplification

## Context

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

## Stage ladder (iteration 3 — phase labels A-3..H-3)

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
- build-stages.sh: `PHASES` A-3..G-3 incl. F-3a/b/c; `phaseA-3` pinned
  via its own branch (created at the A-3 commit — hash unknowable at
  commit time), `PHASE_G_PARENT` overrides G-3's default F-3a parent.
  round-robin.sh: `*-3*` patterns, default VARIANTS = all five.
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
Black's hand. Our units are not centipawns, so the standard-variant row
anchors the scale: FSF charges 100 cp there and we charge 198 u, giving
1.98 u/cp.

| hand | FSF | FSF at 1.98 u/cp | pre-E-3 | E-3 | E-3 vs FSF |
|---|---|---|---|---|---|
| `-/-` | 438 | 867 | 126 | 126 | 15% |
| `-/rb` | 1954 | 3869 | 126 | 220 | 6% |
| `-/qrb` | 2869 | 5681 | 126 | 503 | 9% |
| `-/qqrrbb` | 3715 | 7356 | 126 | 1632 | 22% |

E-3 does what it was scoped to do: the penalty was flat at 126 for every
hand and now scales, growing 13x from empty to `qqrrbb` where FSF grows
8.5x. **It does not fix crazyhouse king safety.** Against FSF as the
standard we still charge 6-22% of what it charges, and the empty-hand row
— 126 against an expected 867 — is untouched by this stage, because that
gap is in the base exposure signal, not the hand. That is F-3's target,
and on this evidence F-3 matters more than E-3 did.

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

### Finding: drop variants can never reach OPENING phase

Found while calibrating E-3 against FSF; not yet a stage, and not folded
into E-3.

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
names that army goes by. Two corrections to the obvious fix:

**`initial_setup` alone is not the starting army.** It is parsed from the
startpos BOARD field only (game_io.rs:1159-1170), so in a setup variant
every piece that starts in hand has an empty `initial_setup`. Sittuyin
starts `8/8/4pppp/pppp4/4PPPP/PPPP4/8/8 w KSSFRRNN/kssfrrnn` — only pawns
on the board — and janggi starts with `HHEEQ/hheeq` in hand. Deriving from
`initial_setup` would hand sittuyin a pawns-only army and an opening
threshold near zero. The starting army is board **plus** initial hand, and
both are available where the threshold is computed: `derive_eval_parameters`
runs on the freshly loaded initial position, so `piece_count` already holds
the startpos board counts and `piece_in_hand` the startpos hands.

**The threshold must be a fraction of that army, not the army.** The
comparison is `phase_score > opening_score`, so a threshold equal to the
full army is never exceeded and every variant would read MIDDLEGAME from
move one — today's bug with the sign flipped. Standard currently sits at
0.625 of its army (4140 of 6622) and 0.26 for endgame (1725), and standard
is the only variant whose strength is validated, so calibrate to it:

```
army = Σ over big non-royal piece type t of
       ovalue(t) * (board_count(t) + hand_count(t))    /* at startpos */
opening_score = 5 * army / 8
endgame_score = army / 4
```

| variant | army | opening now | opening new | endgame now | endgame new |
|---|---|---|---|---|---|
| standard | 6622 | 4140 | 4139 | 1725 | 1655 |
| crazyhouse | 6664 | 8280 | 4165 | 2070 | 1666 |
| shogi | 5662 | 7644 | 3539 | 1365 | 1415 |

Standard is unchanged to within a unit — that is the point of the
calibration. Setup variants are unaffected during setup, where `game_phase`
is pinned to SETUP regardless, and land on a correct threshold once their
hands empty onto the board.

Scope honestly: this recovers the 0.74 discount, not the 5x gap. Crazyhouse
exposure would go from 126 to roughly 170 against an FSF-equivalent 867.
Worth a stage, not a substitute for one. It also needs a param regen
(`opening_score` is token 0 of every `.param` file), which H-3 already
schedules.

### F-3 — royal exposure PST replaces castling knowledge

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
  `castled_bonus`/`castling_rights_bonus`: stop reading at F-3, delete at
  G-3.
- Keep: `king_shelter!`, quadratic `king_danger!`, and initially
  `pawn_shield!`/`open_shield!`.
- Sub-ablation arms sharing the F-3 base: F-3a = full F; F-3b = F minus
  pawn-shield term; F-3c = F-3b minus open-shield term. Eval-block RR
  picks the winner before G-3.

Verify: dump derived royal opening PSTs — standard (g1/c1/b1 ≥ e1,
corners high), shogi (low ranks, edge-file bias), xiangqi (palace
gradient); non-degenerate PSTs for all variants in derive log; SPRT F-3
vs E-3.

### G-3 — pawn eval collapse → advancing-piece path quality

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
- Stop READING (deletion deferred to G-3 so `PARAM_SCALAR_TAIL` stays 19
  and no param regen mid-block): `pawn_connected_opening/endgame`,
  `pawn_doubled_penalty`, `pawn_isolated_penalty`,
  `pawn_backward_penalty`, `pawn_backward_mask`, `pawn_support_offsets`,
  `pawn_passed_support_opening/endgame`. Stop deriving the backward mask
  + support offsets (parameters.rs:1371-1396 region).
- Keep: `pawn_advancement` (dangerous-push search.rs:1300-1304),
  `derive_pawn_stop`, PTable, `pawn_board`, `hash_pawns`.

Verify: `debug-headless evaluate` monotonicity on passed/blocked/supported
FEN triples across geometries (standard, berolina, shogi); bench; SPRT
G-3 vs F-3 before RR.

### H-3 — schema shrink + dead-weight purge + pair ablation

- Delete fields deferred from E-3/F-3 plus `mobility_opening/endgame`
  end-to-end: StaticState (state.rs:538-539, 547-557), `State::new`
  inits, `derive_eval_scalars` (parameters.rs:625-643),
  `parse_tuned_parameters` (game_io.rs:318-336),
  `export_tuned_parameters_file` (game_io.rs:412-432), `export_theta`
  (tuning.rs:663-683). `PARAM_SCALAR_TAIL` 19 → 10 (tempo, pawn_shield,
  king_shelter, king_danger_scale, open_shield, imbalance_major/minor,
  pair_bonus_value, passed_scale_opening/endgame) — minus terms F-3b/c
  killed. Delete `PAWN_MIN_START_COUNT`.
- **Pair-bonus ablation** (user decision): SPRT arm without the term on
  the lean baseline; if neutral → delete term + incremental `pair_score`
  machinery (state.rs:350-392 hooks, verify_game_state check); if it
  loses strength → keep (it is incremental and ~free — expectation is
  "keep" but measurement decides).
- Atomic param regen (parse asserts exact token count; embedded params
  are compile-time `include_dir!`): rm `res/param/*/latest.param` →
  `cargo build --release` (empty embed, falls to fresh derive) →
  `debug-headless derive` (walks every config, exports) →
  `cargo build --release` (re-embed) → single commit of code +
  regenerated params.

Verify: every variant loads (`debug-headless state <variant>` over config
list); perft suites; bench; SPRT G-3 vs F-winner expected ~0 (G is
behavior-neutral except deletions of already-unread terms — non-neutral
SPRT = leaked bug, not a result).

## Ordering, dependencies, RR campaign

```
A-3 → B-3 → C-3 → D-3 → E-3 → F-3 → G-3 → H-3 → final RR
       └── speed block ──┘    └──── eval block ────┘
```

- A-3 prerequisite for every gate (bench + seed). The RR-1 correctness
  fixes are cherry-picked onto phaseA-3..phaseD-3 so every binary shares
  one rule set; the drop-variant RR must be re-run from rebuilt binaries,
  and the pre-fix crazyhouse and shogi standings are void.
- B-3 before C-3: C's NPS measured on lean search. C-3 before D-3: keeps
  speed-suite attribution clean.
- Eval block reordered by measurement: E-3 (hand-drop king danger) first,
  since it is the only stage addressing the drop-variant deficit that the
  RR-1 decomposition actually identified. F-3 (exposure PST) follows,
  G-3 (pawn collapse) after — it addresses none of the measured failures
  and is now a simplification stage, not a strength bet.
- E-3/F-3/G-3 defer schema changes to H-3 so eval-block binaries stay
  param-compatible for RR.
- RRs on the VPS: (1) re-run of the speed block from rebuilt A-3..D-3
  binaries — this also measures what the correctness fixes alone bought
  in shogi and crazyhouse, (2) eval block after F-3 (D-3 vs E-3 vs F-3),
  (3) final five-variant RR after H-3 vs FSF 1700-1900, adding 2000+
  anchors once 1900 is beaten.

## Risks and rollback

- **A-3**: near-zero; seed env unset ⇒ behavior identical.
- **B-3**: search-shape only; node cap +16%; rollback = drop branch.
- **C-3**: HIGHEST correctness risk (make/undo + game-truth path). Full
  perft both widths, debug-assert runs, capture-containing sennichite
  fixture, perpetual fixture. Map restorable wholesale; width revert is
  one line.
- **D-3**: ordering-preservation is the risk; node-count identity gate
  makes any mistake visible immediately. Revert = drop branch.
- **G-3**: strength risk (isolated/backward signal lost). Masks/scalars
  still exist until G-3 → re-enabling any term is a small diff. SPRT
  before RR.
- **E-3/F-3**: derive-order risk (zone_attack before PSTs on BOTH load
  paths) + castling-variant strength risk. Sub-ablations isolate
  shield-term regressions. PST sanity dump per variant.
- **H-3**: param regen schema mismatch panics loudly at load (exact-length
  assert) — cannot fail silently; the regen sequence is the rollback
  boundary.

## Out of scope (this iteration)

- Texel tuning / datagen (user decision — next iteration, after schema
  settles).
- Battery/pair generalization beyond the ablation.
- SMP work, ponder, seldepth.
- Any leaf evaluator walking compiled move vectors (permanently rejected).
