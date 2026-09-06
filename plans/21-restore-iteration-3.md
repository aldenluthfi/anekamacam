# Restore iteration 3 onto the iteration 4 and 5 line

## Status

Drafted 2026-09-02. R1 through R9 have landed, and R9 was the last stage.
R10 and R11 stay deferred per the user's 2026-09-04 instruction.

Two preparatory commits are already in: `316861a` ports the exchange
phase-pricing fix onto this line, and `224d167` is a whitespace fix. The
branch `strength-iteration-6` was renamed to `main`; the previous `main`
tip is preserved at tag `ablated-main-2026-09-02` and its only unique
content, `951eda0`, is the fix `316861a` carries.

The iteration 6 batch at `9d633d8` is deliberately abandoned. It was built
on the ablated `b971865`, not on this line, and it holds no ref.

The deferred PST-schema question was resolved after the ladder on 2026-09-05:
parameter files now store full PST values and eleven evaluation weights, and
tuning uses quiescence scores. That work remains uncommitted.

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

The param conflicts were not incidental. Iteration 3 stored absolute values
plus a 19-token scalar tail; when this ladder was written, this line stored
material and PST residuals against derived values. A revert would have dragged
the old schema back wholesale. Every stage below was therefore written by hand
against `e56980f`
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
Derived weights seed the parameter files. Eleven are then loaded and tuned
beside material and PSTs: shelter, guard, castling, danger, open shield,
tempo, and the three imbalance weights.

## Full PST and scalar parameters — implemented, uncommitted

The old correction rows were all zero, so they stored 768 zeroes for
standard while the engine derived the actual PSTs on every load. Parameter
files now store those full values directly. The flat format has one shape:
opening material, endgame material, full White opening/endgame PST rows,
then the eleven weights. No version tag, old parser, or permanent conversion
command remains.

All 38 files were converted through their effective loaded state, preserving
material and evaluation. Start-position phase and evaluation match the
pre-conversion binary on every variant. The parser still rebuilds geometry,
search parameters, and evaluation products before installing full PSTs and
the eleven tuned weights.

Tuning now runs quiescence on every loaded dataset position. Each sample's
fixed offset is its White-view quiescence score minus its tunable terms, so
unresolved captures no longer make the initial model equal a static root
evaluation and untuned evaluation terms remain present. Adam changes the
same full values the runtime loads and the exporter writes. A five-game
scratch dataset and one epoch completed and wrote the expected 791-token
standard payload. Its training error fell, but its one-game validation error
rose, so validation correctly selected epoch 0 and exported unchanged values.

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

### R7. Correction history — landed

Restored the worker-local correction table, keyed by side to move and pawn
placement. It lives on `SearchInfo` beside butterfly and continuation
history rather than on `State`: every lazy-SMP worker learns independently,
`clear_search` gives each one zeroed cells, and no position clone carries a
64 KiB learning table.

R4 deliberately folded the pawn-table key from the piece lists because the
PTable alone did not repay an incremental field. R7 creates a second hot
consumer, so that decision changes here: `State::pawn_hash` is maintained by
the same piece-hash update macro as `position_hash`, with non-pawns masked
out branchlessly. `Snapshot` carries it through ordinary and null undo;
`hash_pawns` initializes it after FEN and config loading. Pawn structure now
reads the same field instead of walking every pawn once per evaluation.
`verify_game_state` still recomputes `temp_pawn_hash` independently, using
`pawn_pieces` while the update macro uses `pawn_slots`, so it can expose both
a stale key and disagreement between the two derived views. `pawn_slots`
starts as an all-`NO_PAWN` vector because derive-time move simulations run
before pawn roles exist; the finished config recomputes the key after roles
are derived.

The correction is the table entry divided by 64, clamped to ±64 cp, and is
added only to the score read by reverse futility and null-move pruning.
Futility keeps the raw static evaluation: feeding corrected values to
fail-low pruning was the shogi/drop-tree explosion fixed after iteration 3
Stage I. Every searched bound can teach the table — beta cut, exact result,
and fail-low — rather than a fail-high-only update. Capture-best nodes use
weight one; quiet-best nodes use `min(depth + 1, 16)`. The branchless spelling
is `1 + depth * !capture`. Do not copy `e56980f`'s `depth + capture`: commit
`4953239` changed that meaning during a style pass even though its doc still
said captures use the minimum weight, and the original capture blend is the
one later measured and kept.

Versus R6, deterministic nodes: standard 190760 to 191525 (+0.4%), shogi
1391464 to 1158816 (−16.7%), xiangqi 369526 to 312804 (−15.3%), crazyhouse
1009750 to 1133316 (+12.2%), grand 1432374 to 1415089 (−1.2%), sittuyin
12027791 to 10646371 (−11.5%), janggi 301548 to 272617 (−9.6%). One paired
speed pass reads 4.79M to 4.76M nps standard, 1.66M to 1.66M shogi, 1.26M to
1.29M xiangqi, 1.15M to 1.26M crazyhouse, and 1.15M to 1.16M grand. The
incremental key pays most or all of the correction lookup cost; speed signs
inside a single pass remain advisory.

Depth-6 signatures keep all 38 best moves. Twenty-nine node counts move,
and only tjatoer's score moves, 51 to 50 cp. All seven
perft suites pass; all seven static evaluations and both SEE checks stay
exact. Debug searches exercise make, null move, and both undo paths with the
independent pawn-hash assertion. The old UCI-driven fixture command reported
37/38 at depth 6; the harness correction below proves all 38 at the requested
depth. The apparent xiangqi horizon was an interrupted iteration, not search.

### R8. TT static-eval cache — landed

Restored the raw static evaluation in the 23 unused high bits of the main
TT payload: signed bits 105-127, above the 64-bit move signature. No slot or
entry grows — `HashEntry` remains 64 bytes — and the parity word already
covers every payload bit. `EVAL_NONE` moves beside `INF` in the prelude
because search and TT packing now share the sentinel; its value, 2,000,000,
fits the signed 23-bit field.

The plan named `tt_enc_eval!` and `tt_eval!`, but the debloat pass changed the
right answer: one store expression and one read expression do not earn two
exported macros and their doc blocks. Packing and sign extension stay inline
at their only sites. Power-of-two masking was already present from D5, so R8
does not pretend to buy that part of iteration 3's combined Stage F again.

Every valid TT hit returns the raw evaluation even when its searched depth is
too shallow for a score cutoff. The stored search bound also sharpens a
second evaluation: an exact score replaces it, a beta bound can only raise
it, and an alpha bound can only lower it; mate-range scores never sharpen.
Search reuses the raw value instead of calling `evaluate_position!`, keeps it
for the improving comparison and correction-history update, and reads the
bound-refined value for pruning. R7's split remains intact: correction is
added only for reverse futility and null move, while move-loop futility gets
the bound-refined value without correction. All three TT stores write the
raw evaluation, including `EVAL_NONE` at checked nodes.

Versus R7, deterministic nodes: standard 191525 to 186264 (−2.7%), shogi
1158816 to 1210223 (+4.4%), xiangqi 312804 to 304943 (−2.5%), crazyhouse
1133316 to 895573 (−21.0%), grand 1415089 to 1438362 (+1.6%), sittuyin
10646371 to 10624972 (−0.2%), janggi 272617 to 251880 (−7.6%). Paired nps
is flat for standard, xiangqi, and crazyhouse in the observed runs; shogi
reads roughly −15% and grand −4.5%, though changed node mixes make those
per-node rates advisory rather than a direct cache-cost measurement.

Depth-6 signatures keep 37 of 38 best moves and scores. Euroshogi alone
moves from `c3:c4`, +29 to `d1:c2`, +20; 28 node counts change. All seven
perft suites pass; all seven static evaluations and both SEE checks stay
exact. The fixture harness passes all 38, one case on the depth-7 budget it
carries for the reason written up under R9.

### Endgame fixture synchronization — landed

The one red fixture was not a horizon defect. Search cases piped `go depth 6`
and then `quit`; protocol exit deliberately interrupts and joins an active
search, so the xiangqi case returned its last completed depth-5 score. Asking
for depth 7 appeared to fix it only because depth 6 finished before quit won
the race.

Search fixtures now use synchronous `debug-headless search`, which returns
only after the requested depth and emits the same UCI `score cp` / `score
mate` text the assertions already parse. Game-truth `d` fixtures stay on UCI.
The race is gone, with no sleep and no weakened expectation, and the suite
settled at 37 of 38: the harness fix moved the xiangqi perpetual case from a
false red to a true one. That case is a search horizon defect, diagnosed
under R9 and now carrying its own depth field.

Fixture lines gained an optional trailing `depth`, and the runner searches
the deeper of that and `GO_DEPTH`. A search case asserts a verdict, not the
depth at which the engine must find it, so the one case that costs more
plies than its neighbours says so on its own line instead of every case
paying for it, and raising `GO_DEPTH` still deepens the whole suite. The
xiangqi perpetual case carries 7 for the two reasons written up under R9.

### R9. Gated search family — landed

Restored razoring, ProbCut, and internal iterative reduction, each behind
`forward_pruning!` and `static_movement!` on top of whatever capability its
own mechanism needs. PVS and mate-distance pruning were already present and
needed nothing. Check extensions were dropped, for reasons below.

Both margins derive from the mean non-royal piece value rather than from
constants: razoring uses `[0, mean/3 + 100, mean/2 + 200, mean + 300]` indexed
by depth, and ProbCut uses `max(mean/4, 100)`. A variant whose pieces are
worth a third of a chess piece gets a third of the margin without anyone
naming the variant.

Razoring asks quiescence to confirm a shallow fail-low before paying for the
node. ProbCut asks at most three winning captures to prove a surplus of
`beta + margin`, first in quiescence and then at `depth - 4`; it is near
neutral in nodes and is kept for the bound it proves, not for the nodes it
saves. Internal iterative reduction gives up one ply when no table move
exists — move-loop futility keeps reading the pre-reduction depth, so the
reduction changes what is searched and not what is pruned.

`static_movement!` is what makes the family safe: a screened variant's
evaluation cannot price a blocker-dependent attack, so none of the three
runs for xiangqi, janggi, or sittuyin, whose node counts are unchanged from
R8 by construction.

Versus R8, deterministic nodes: standard 186264 to 178418 (−4.2%), shogi
1210223 to 761185 (−37.1%), crazyhouse 895573 to 773768 (−13.6%), grand
1438362 to 1408835 (−2.1%); xiangqi, sittuyin, and janggi unchanged at
304943, 10624972, and 251880. All seven perft suites pass; all seven static
evaluations and both SEE checks stay exact. All 38 depth-6 signatures
complete. The xiangqi fixture reads the same score at the same node count as
the R8 binary; it is green now because the case carries its own depth, not
because search changed.

#### Check extensions, and the xiangqi fixture they were meant to fix

Not restored. Applied unconditionally the cost is +77% nodes on standard;
five narrower gates were measured and every one either cost more than it
returned or failed to recover the mate it was aimed at. The last of them
keyed on the root standing in a declared perpetual cycle, which is a rule
token inside search and not something this engine is allowed to read, so the
mechanism leaves the ladder entirely.

The fixture it was aimed at is a genuine defect and predates the whole
restore ladder: the R8 binary returns the same `score cp -950` at the same
943 nodes. The mate is `e10e9 a10f10 e9e8 f10f9`, three plies from the white
node, ending in a position with no legal move, which xiangqi scores as a
loss. Search needs depth 7 for it. Two causes stack:

- Quiescence never generates legal moves, so a terminal position one ply past
  the horizon is invisible. Worth exactly one ply: the mate-in-1 node needs
  depth 2 and the mate-in-2 node needs depth 3.
- Late move reduction reduces the quiet mating move `a10f10`. Its reduced
  search returns roughly +944, which does not beat alpha, so the re-search
  never fires. Worth two more plies: with the reduction forced to zero the
  white node resolves at depth 4 and the fixture reads `mate -2` at depth 6.

Neither is a variant question, and neither belongs in a restore stage. The
case therefore carries `| 7` in the fixture file rather than being weakened
or having the whole suite chase it: the assertion stays `score mate -2`, and
the two engine costs above stay recorded as what that 7 is buying. Lower it
when either one is genuinely paid off.

`game_outcome` reports `Ongoing` for that final position even though perft
depth 1 counts zero moves, because it consults only `game_result` and the
repetition rule and never asks whether the side to move has a move. That is
the documented boundary of the `d` oracle, stated in the fixture header —
no-legal-move outcomes are verified by search — not a third defect.

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
