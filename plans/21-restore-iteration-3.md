# Restore iteration 3 onto the iteration 4 and 5 line

## Status

Drafted 2026-09-02. R1 through R6 have landed; R7 is next.

Two preparatory commits are already in: `316861a` ports the exchange
phase-pricing fix onto this line, and `224d167` is a whitespace fix. The
branch `strength-iteration-6` was renamed to `main`; the previous `main`
tip is preserved at tag `ablated-main-2026-09-02` and its only unique
content, `951eda0`, is the fix `316861a` carries.

The iteration 6 batch at `9d633d8` is deliberately abandoned. It was built
on the ablated `b971865`, not on this line, and it holds no ref.

One stage was attempted and reverted before the ladder below was fixed:
dropping the PST residual region from the param format. It is recorded as
a deferred open question, not as a stage.

## Goal

Restore the strength and speed the engine had at the end of iteration 3
(`e56980f`), on top of the iteration 4 and 5 line, keeping every iteration
4 and 5 mechanism that the user named as load-bearing.

## Why a revert cannot do this

Reverting the two ablation commits was probed on a scratch branch and
abandoned:

- `94b0759` (evaluation) conflicts in 38 `res/param/*/latest.param` files
  and 15 sources.
- `916cc16` (search) conflicts in 8 sources.
- `8586adf` (board width) conflicts in `src/prelude.rs`.

The param conflicts are not incidental. Iteration 3 stored absolute values
plus a 19-token scalar tail; this line stores material and PST residuals
against derived values. A revert would drag the old schema back and undo a
keeper. Every stage below is therefore written by hand against `e56980f`
as reference, not applied as a patch.

## Kept from iterations 4 and 5

These are preserved by every stage; a stage that would remove one is wrong.

- `capabilities: u16`, the rule-derived per-variant capability mask. Every
  search shortcut restored below is gated through it. Iteration 3 applied
  every shortcut to every variant unconditionally, which is the defect
  plan 18 deferred.
- `virgin_hash` and the separated canonical / search / qsearch hash
  identity. New hashes introduced below (pawn hash, correction hash) join
  that separation rather than reusing the canonical key.
- `reduction_surface`, the rule-derived LMR curve. Iteration 3's `LMR_*`
  constants are **not** restored.
- SETUP army resolution (`resolve_setup_army`, `walk_setup_endings`,
  `setup_census_key`) for phase thresholds, and drop moves carrying a real
  target square (`532ef34`).
- Variant-declared repetition and perpetual outcomes, deepest-worker SMP
  selection, and the wide-window bound fix.
Restored evaluation scalars are **derived**, via `derive_eval_scalars` and
`derive_pawn_parameters`, so no param file gains a scalar region and
iteration 3's 19-token tail does not come back.

## Open question, deferred: the PST residual region

The param schema is not on the keeper list. Its shape was measured, not
assumed: `res/param/standard/latest.param` is 780 tokens, 12 absolute
material values then 768 zeros, and every token past the material prefix
is zero in all 38 files. The region has one writer, `export_theta` in
`src/debug/tuning.rs`, which stores each final target minus its
rule-derived base. No tuning run has happened since the schema changed, so
every PST in use is the derived one.

Storing the derived numbers in place of the zeros does not fix the
redundancy. It is the same 768 numbers per file, and it costs the one
property the residual encoding buys: the derivation stays live. Change
`derive_base_pst` and all 38 variants pick the change up, keeping whatever
tuning added on top. Absolutes freeze it — a stale file silently overrides
an improved derivation with the old one, and nothing distinguishes a
square tuning touched from a square that is derivation output copied down.
Determinism is the argument for not storing the output at all, not for
storing it verbatim.

Removing the region was attempted and reverted, because it is not
independent of the tuner. `compute_gradient` optimises material and every
per-square PST value, and `export_theta`
(`src/debug/tuning.rs:663-678`) writes those residuals back through
`parse_tuned_parameters`, whose stricter token count then rejects its own
tuner's output: `tune` panics at export. A material-only format therefore
requires first deciding what `tune` optimises — material alone, or PSTs
into a region that has to keep existing. Deferred at the user's
instruction. No stage below reads a PST residual, so the ladder is
unaffected either way.

## Deliberately not restored

Both were measured negative in iteration 3 and re-measured in iteration 2's
ablations.

- Capture history: -11 Elo, and its iteration 2 removal cost nothing.
- Singular extension with multicut: -19 Elo, the worst stage of A-L.

## What is actually missing

Measured against the current tree, not against plan 20's narrative, which
describes the abandoned `9d633d8` lineage.

Evaluation. `src/game/position/evaluation.rs` is 142 lines against
iteration 3's 700. Present: `terminal_score!`, `royal_shelter!`,
`evaluate_position!`. Absent: `draw_score!`, `king_shelter!`,
`pawn_shield!`, `castling_bonus!`, `king_danger!`, `open_shield!`,
`pawn_structure!`. Draws score zero everywhere.

Search. `src/game/position/search.rs` is 1094 lines against iteration 3's
1779. Present: aspiration windows, futility, `improving`, delta pruning,
`reduction_surface` LMR, NMP, LMP, killers, butterfly history. Absent:
PVS, razoring, ProbCut, IIR, mate-distance pruning, check extensions,
continuation history, correction history, and the TT static-eval cache.

Structure. `BoardBits` is unconditionally `U4096`; the `wide-board` feature
is gone from `src/Cargo.toml`. Staged move generation
(`generate_all_quiets_and_drops`) is deleted. The soft and hard deadline
split is collapsed to a single `deadline`.

## Ordering

Iteration 3's own Elo record decides the order. Twelve search micro-stages
(A-L) netted about zero combined, on a round-robin cluster of [-19, +1].
Evaluation and time carried all of the roughly 100 Elo:

| iteration 3 stage | Elo | restored as |
| --- | --- | --- |
| M repetition scoring and material draw bias | +48 | R1 |
| O royal back-rank PST, pawn shield, castling | +27 | R2 |
| P zone-attack king danger, open-shield penalty | +21 | R3 |
| B continuation history | +14 | R6 |
| F TT static-eval cache | +9 | R8 |
| I correction history | +9 | R7 |
| Q stability-scaled soft deadline | +4 | R11 |

Evaluation therefore goes first. The search family that netted zero goes
last, and goes in gated by `capabilities` rather than unconditionally,
which is the one thing iteration 3 never tried.

## Stage ladder

Each stage is one commit. Each states what it restores, what it derives
rather than tunes, and what it is gated by.

Support state lands with the stage that consumes it, not as a preparatory
stage of its own. A stage adding a table no term reads yet is dead weight
and separates each term from the state it needs, so `adjacency_mask`,
`royal_shield_mask`, `royal_front_mask`, `zone_attack`,
`zone_attack_best`, the `pawn_*` masks, `pawn_hash`, `pawn_board`,
`pair_score`, `has_castled`, `draw_contempt`, and the `PTable` and `PTEntry`
pair each appear in the stage below that first uses them. The pawn hash
joins the identity separation from P4 rather than reusing the canonical
key.

### R1. Draw scoring and material draw bias — landed

Restore `draw_score!` and route `terminal_score!` and every repetition and
perpetual path through it, with `draw_bias` derived. Iteration 3's largest
single result, +48.

Acceptance: signature moves only where a draw is scored; a drawn endgame
that is winning on material no longer evaluates to zero.

Landed as two halves, because contempt on its own moved no node count at
depth 6 in any of the 38 configs. Iteration 3 scored a repeated position
from the first closed cycle, not from the occurrence count the rule names,
and that is what the search was missing.

The bias is `draw_contempt` and `draw_span`, both derived in
`derive_eval_products` from the mean deployed piece value:
`DRAW_CONTEMPT_RATIO` of one such piece, saturating at a lead of
`DRAW_CONTEMPT_SPAN` of them. Standard derives 34 at a span of 548. A
stalemate a queen up scores +34 for the stalemated side and −34 mirrored,
against 0 before.

The cycle cut is gated on whether the variant's perpetual rule names an
offender. Where none does, one closed cycle is enough. Where one does, the
search waits for the rule's own count, since which side is at fault is not
settled until the rule fires: `tools/run_endgame_fixtures.sh` pins exactly
this with `xiangqi perpetual one cycle short: the chariot mates the checked
side`, which an ungated cut turns from `mate -2` into `mate 1`.

Result: 38/38 endgame fixtures; two signature rows move, node counts only,
same move and score (newzealand 4643→4636, pocketknight 30319→30315).
Speed suite versus the pre-R1 build: standard 133695→131470 nodes at
+3.4% NPS, crazyhouse 777281→788390 at −2.1%, and shogi and xiangqi node
identical, their time deltas 0.2% either way.

### R2. Royal back-rank PST, pawn shield, castling incentive — landed

Restore `king_shelter!`, `pawn_shield!`, and `castling_bonus!`, folding
the existing `royal_shelter!` into them rather than running both. Scalars
`king_shelter_bonus`, `pawn_shield_bonus`, `castled_bonus`, and
`castling_rights_bonus` are derived from mean non-royal piece value.

Two of the three named terms were already here, which the plan's own
inventory got wrong. `derive_pst` already lays iteration 3's royal
back-rank opening gradient, unchanged, and `royal_shelter!` already is
iteration 3's `pawn_shield!` — same three forward squares, same cap of
three — with an extra gate for royals their rules confine. Restoring
either would have run the term twice. R2 therefore restores only what was
missing: the ring term and the castling incentive.

`royal_guard!` prices every friendly piece on the ring around a royal,
whichever side of it stands on, since each one blocks a line into the
royal's square. It reads a precomputed colour-blind `ring_squares` list
with `ring_counts`, built in the square loop `derive_shelter_parameters`
already walks. Iteration 3 held per-square adjacency as `Vec<Board>`;
`Board` is 514 bytes here, so one copy per royal per evaluation would have
cost the speed half of this plan's goal. The ring bounds the count itself
and needs no cap, and it is priced below shelter.

`castling_bonus!` ranks having castled above holding a right above having
spent both for nothing, on `has_castled` maintained by make and undo.
Castling spends the rights, so a side reaches that branch at most once and
no snapshot field is needed. Carried over from iteration 3: a position
entered by FEN cannot know a side has already castled, and reads as
rights-spent.

Derived from the dearest non-royal piece: `GUARD_RATIO` 6 with a floor of
2, `CASTLED_RATIO` 40, `CASTLING_RIGHT_RATIO` 20. Standard derives 5 per
guard against 11 per shelter, and 37 castled against 18 holding the right.
After `e2e4 e7e5 g1f3 b8c6 f1c4 g8f6`, castling scores −40 cp against +32
cp for `e1f1`, a 72 cp preference.

Result: 38/38 endgame fixtures, and the depth-6 signature moves broadly,
as a new evaluation term should. Speed suite versus R1: shogi 1,007,941
nodes to 828,784 at −10.3% time, xiangqi 493,122 to 478,259 at −4.6%,
crazyhouse 788,390 to 867,645 at +4.8%. Standard is node-identical, which
is not a null result but a blind instrument — see Measurement.

### R3. Zone-attack king danger and open shield — landed

Restore `king_danger!` and `open_shield!` with `king_danger_scale` and
`open_shield_penalty` derived.

`king_danger!` reads a precomputed zone-attack table. For every triple of
royal square, piece, and origin, `derive_danger_parameters` asks
`derive_vector_chance` under `OPENING_OCCUPANCY` for the expected number
of that piece's vectors landing on the royal square or any square of its
ring — R2's `ring_squares` is the adjacency source, so the two stages
share one notion of "beside the royal". The expectation is stored as
`ZONE_ATTACK_UNIT`ths of a landing in a `u8`, and reduced over the origin
axis into `zone_attack_best` for pieces held in hand.

Evaluation sums those units over every enemy piece that is neither royal
nor shield-like, and, where the rules drop, adds `zone_attack_best` once
per copy in hand. A hand is the whole attacking reserve of a drop
variant; leaving it out makes a hand of two queens as harmless as an
empty one. Drop legality is not checked, since over-stating a held
attacker errs toward caution. The total is charged as its square so one
attacker barely registers and several compound, capped where a further
attacker would say more than winning the dearest piece outright.

`open_shield!` charges a royal with nothing of its own anywhere ahead of
it on its own file or either neighbour. Shelter prices the squares
immediately in front and guard the ring; neither can say the ground ahead
is empty all the way out, which is the file an enemy rook or lance
arrives on. The test is geometric — file distance and the sign of the
rank difference against `forward_steps` — so it needs no pawn mask, and
only shield-like pieces count as cover, since a piece that can walk back
the way it came is not holding a file. `pawn_board` and the pawn masks
stay unbuilt for R4.

Derived from the dearest non-royal piece: `DANGER_RATIO` 600,
`DANGER_CAP_RATIO` 1000, `OPEN_SHIELD_RATIO` 33 with `OPEN_SHIELD_FLOOR`
12. Standard derives king danger worth 559 at sixteen landings, capped at
933, and an uncovered royal at 30.

Evaluation was restructured here for speed, not for score. It used to
compute both halves eagerly and then pick one, so every endgame node paid
for the whole safety family it never reads; the endgame-only standard
bench measured that as −8.4% nps against R2 on identical nodes. Splitting
the halves into `opening_score!` and `endgame_score!` and computing each
inside its own phase arm is arithmetically the same expression and left
every probe score unchanged, and standard now runs 3,979,603 nps against
R2's 3,711,460, +7.2%.

Behaviour: a queen approaching a bare royal raises danger monotonically
(a1 1144, h5 1156, e5 1186, d6 1176); an uncovered royal reads ±51
mirrored on full back ranks; a crazyhouse hand of two queens is worth
about 233 cp over the material it already counts.

Result: 38/38 endgame fixtures, and 32 of 38 signature rows move. Speed
suite versus R2, nodes then time: shogi 1,152,189 to 1,084,801 at +0.3%,
xiangqi 597,643 to 583,595 at +1.4%, crazyhouse 1,471,929 to 1,349,333 at
−7.1%, grand 1,112,355 to 1,274,326 at +16.9%, standard node-identical at
−6.7%. Grand's time is its node count, not its nps, which moved −2%.

### R4. Pawn structure

Restore `pawn_structure!` and its seven sub-terms, with connected,
doubled, isolated, backward, and passed scalars from
`derive_pawn_parameters`. This is the stage that introduces the pawn
masks, `pawn_board`, `pawn_hash`, and the `PTable` and `PTEntry` pair.

`derive_pawn_slots` calls a piece a pawn on the same four conditions
iteration 3 used: it never steps or captures backward, it has a quiet
single forward step, it has no non-initial quiet move reaching further
than one square, and at least `PAWN_MIN_START_COUNT` of it stand on the
opening board. Its colour twin takes the same answer. Three variants
therefore derive no pawn at all — judkins and minishogi field one pawn
each, below the floor, and sittuyin's `Pp` line carries a multi-leg
promotion chain whose `sW` legs read as backward steps. Iteration 3
excluded exactly the same three, so this is the term declining to speak
about a variant rather than a regression.

The nine scalars are shares against `COEFFICIENT_SCALE`, replacing
iteration 3's hand-written divisors: connected 200 opening and 350
endgame, doubled 250, isolated 250, backward 175, passer 100 opening and
350 endgame, and 400 for a pawn that promotes to nothing. Advancement
is `adv² * 256`, unchanged. The protected and connected passer bonuses
are folded into one `2 + connected + chained` multiplier over halves
rather than the two extra tables iteration 3 carried, which prices a
plain passer at 1×, a protected one at 1.5×, and a connected one at 2×,
as before.

`pawn_board` and a make-and-undo `pawn_hash` were not added. The key is
folded from the pawn piece lists at the point of use, which costs one
exclusive or per pawn, cannot desynchronise from the board, and does not
introduce a fourth hash identity alongside the canonical, search, and
qsearch keys. Only a probe miss pays to gather the roster. An
incremental hash was measured against this and the difference sat inside
the machine's ±6% run-to-run noise, so the cheaper thing to reason about
wins.

Result: 38/38 endgame fixtures, and every sub-term hand-checked by
evaluation delta — a lone passer on e2 reads −2 (+23 passer, −25
isolated) and on e6 reads +183, a doubled pawn −25, a connected d2 and
e2 pair +164 (46 passer, 72 connected, 46 chained), an isolated a2 and
h2 pair −4, and a blocked pawn 0. Uncached the term cost 5 to 32% of
nps; with the table, nodes are bit-identical to uncached and the cost is
2.2% standard, 4.0% shogi, 2.1% xiangqi, 5.7% crazyhouse, 10.5% grand.
Speed suite versus R3, nodes then time: standard 131,470 to 115,323 at
−10.3%, shogi 1,084,801 to 873,348 at −16.2%, xiangqi 583,595 to 572,743
at +0.3%, crazyhouse 1,349,333 to 619,809 at −51%, grand 1,274,326 to
1,428,999 at +25%. Grand's time is again its node count.

### R5. Tempo, imbalance, pair bonus

Restore the three remaining scalar terms, derived.

All three are shares of the dearest non-royal piece: tempo 24 with a
floor of 5, the heavy-piece imbalance 20 with a floor of 3, the light one
10 with a floor of 1, and the pair 60 with a floor of 10. For standard
these give 22, 18, 9, and 55, the first of which reproduces iteration
3's `avg / 20` exactly. Imbalance and the pair are worth the same at
either end of the taper and so are added once outside the blend rather
than into both halves; tempo is added after the score turns to face the
side to move.

Iteration 3's pair test asked for a piece that is neither royal nor big
and whose mean reach is within 0.02 of half the board. The big test made
the term dead: `derive_piece_roles` calls everything above the cheapest
tenth big, so a bishop never qualified and 36 of 38 variants derived an
empty pair list. The reach test already carries the whole intent -- a
piece free of the board reaches 1.0 and a piece confined to a corner of
it reaches far less than half -- so the big filter is dropped. The list
is now bishops in the chess-like variants, the shatranj ferz, the makruk
met, the crazyhouse promoted bishop, and tjatoer's bishop, camel,
bishop-camel, and short diagonal slider. Shogi, xiangqi, and janggi
field no colour-bound piece and derive nothing, correctly.

Result: every term hand-checked by evaluation delta on standard --
startpos reads exactly the tempo either side to move, an extra rook +31
(tempo, one light count, the rook being minor here since only the queen
is in the top fifth), an extra bishop +86 (tempo, one light count, the
pair the other side just lost), both sides down a bishop +22 with the
pair cancelling, and the same figures again in the endgame phase, where
imbalance and pair still read.

37 of 38 endgame fixtures pass; xiangqi's `perpetual one cycle short`
needs depth 7 where it wanted 6, and all 38 pass at `GO_DEPTH=7`. The
mate is still found, one ply later.

Speed suite versus R4, nodes: standard 115,323 to 199,129, shogi 873,348
to 1,129,608, xiangqi 572,743 to 386,294, crazyhouse 619,809 to
1,112,901, grand 1,428,999 to 1,448,720. Nodes per second rose in every
variant, the terms costing three array reads and a short loop.

Standard's node count is tempo alone: with tempo zeroed it reads 113,670
and with only imbalance and the pair zeroed it reads 198,751. Futility
and razoring prune on the static evaluation against alpha, and tempo
lifts that evaluation at every node, so fewer quiet moves fall under the
margin. The effect is real and is the price of the term rather than a
defect in it, but R5 is the first stage whose node cost is not obviously
repaid by what it buys, and it is the stage to put under SPRT first.

### R6. Continuation history

Restore 1-ply and 2-ply continuation history, the best of A-L at +14.
Iteration 2 measured its removal at +26% nodes on correction history and
kept continuation; both are load-bearing.

The table is `CONTINUATION_PLIES * move_keys * move_keys` cells of `i16`
with `move_keys = pieces * board_size`, the same key the butterfly table
uses, and it lives on `SearchInfo` beside it. Iteration 3 kept it on
`State`, where every worker paid to clone it; nothing here clones. A row
is addressed by the move played `plies_back` before this node, and reads
`usize::MAX` when that ply does not exist or held a null move, which is
the one case where no move was answered. Quiescence passes no rows at
all: its only quiet moves are evasions, which answer a capture rather
than a line worth learning a reply to.

The score bands had to widen. A quiet move now sums `HISTORY_TABLES`
cells rather than one, so the quiet band is three bounds wide either
side of its centre and killers move to seven bounds above `1_000_000` to
stay clear of it. Both constants are expressed in `HISTORY_TABLES` so
the layout follows the table count rather than being restated.

The history-shaved LMR of iteration 3 is deliberately not restored:
reductions come from `reduction_surface`, which is derived per variant,
and shaving a derived surface by a history score is a separate claim
that belongs in its own probe.

Result: 37 of 38 endgame fixtures, the same xiangqi `perpetual one cycle
short` case R5 left needing depth 7, and 38/38 at `GO_DEPTH=7`.

Speed suite versus R5, nodes: standard 199,129 to 190,760 at −4.2%,
shogi 1,129,608 to 1,391,464 at +23.2%, xiangqi 386,294 to 369,526 at
−4.3%, crazyhouse 1,112,901 to 1,009,750 at −9.3%, grand 1,448,720 to
1,432,374 at −1.1%. Node counts are deterministic under the pinned seed
and shogi's figure repeated exactly, so it is not noise. Nodes per
second moved +1.6, −9.0, −5.0, 0.0 and +3.0%; shogi's loss is its table,
which at 28 piece types on 81 squares is 20.6 MB against standard's 2.4.

One ply alone was probed and is worse: standard +21%, xiangqi +16% and
grand +10% against the two-ply table, with shogi −8% and crazyhouse
−10%. The second ply is load-bearing on three of five variants and on
the two largest margins, so both plies stay. If shogi is the variant
that fails under SPRT, the size of its table is the first thing to
attack, not the second ply.

### R7. Correction history

Restore pawn-hash correction history, +9, and the all-node malus update
rule. Do not standardise updates to fail-high-only: that was measured at
+19 to +47% nodes.

### R8. TT static-eval cache

Restore `tt_enc_eval!` and `tt_eval!` and the cached static evaluation in
the main transposition payload, +9.

### R9. Gated search family

Restore PVS, razoring, ProbCut, IIR, mate-distance pruning, and check
extensions, each gated on `capabilities`. This family netted about zero in
iteration 3 applied unconditionally; the hypothesis under test is that the
gate is what was missing, not the mechanisms.

### R10. Board width and staged move generation

Restore `BoardBits = U256` as the default with `wide-board` reinstated in
`src/Cargo.toml`, and restore `generate_all_quiets_and_drops` as the
second stage of staged generation. Both are per-node cost levers, not
strength changes.

Reference figures, 16-position bench, seed 42, one thread, `phaseF-3`
against the current line: standard depth 11 is 208,063 nodes at 50.2 ms
against 133,695 at 37.8 ms; grand depth 9 is 8,639,479 at 3048 ms against
2,400,555 at 2201 ms. The two run different evaluators, so nodes and nps
are confounded and only games settle the sign.

### R11. Soft and hard deadline split

Restore the stability-scaled soft deadline, +4. Carry forward the known
defect: F-3 spent 65% of the clock in the first 18 moves and less than
E-3 after move 21, so the SPRT sign on the time manager was wrong. Derive
the split rather than restoring F-3's constants.

## Measurement

Per stage: release build with no warnings, the 38-config depth-6 signature
from plan 18, and the 16-position bench. Games only where a stage claims
Elo, and always reported as raw W/L/D. The `sprt` verdict string reports
from `bin-a`'s view; state which binary is `bin-a` whenever it is quoted.

The standard bench is blind to every opening-half term. `bench` takes the
first `limit` perft cases whose node counts are non-zero, and in
`res/perft/standard.perft` those are sixteen bare endings — a king and one
or two pieces each, all of them ENDGAME phase, where the opening half of
`evaluate_position!` is never read. A stage touching shelter, guard,
castling, king danger, or pawn structure will read node-identical there
however large its effect, and R2 did. Read the other variants' benches and
the signature for those stages, and do not quote standard's bench as
evidence that an opening term changed nothing.
