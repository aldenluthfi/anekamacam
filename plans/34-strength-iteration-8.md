# Strength iteration 8: the audit, the speed map and the weak variants

## Status

Opened 2026-11-14 on build 48 (main 9791d0c), after an audit the user
asked for: the work so far, the style of code and comments, and a
profile run of build 48. The audit's quick fixes are done: the style
fixes, a display fix, and plainzone, merged on speed as build 49 (main
66a6ccd). The bigger jobs are the stages below. Stage 1, the evasion
skip, merged on speed as build 50 (main e3e3142; crazyhouse x0.912).
Build 50's sweep against FSF 2100 is done: 22 of 32 valid variants are
at or above it, and the baseline stays. The work was paused for the
user from 2026-11-15. Stage 6, two correctness fixes from the user's
audit, is built on `plan34-correct`; its regression arm waits for a
session on the Windows machine. The test opponent is FSF classical at `UCI_Elo`
2100. Plan 33 is the ledger of builds 34 to 48; plan 29 of the earlier
tries.

## Context

### Milestones

- The baseline is 2100. Once every valid variant passes it, it becomes
  2200.
- Each merge is one build. At build 50, and at every tenth build after
  it, a sweep plays every variant that FSF plays against FSF at the
  baseline, 400 games each (`~/aneka-q/sweep.sh`); the table goes here,
  and the work pauses for the user.
- The test set is the weakest valid variants of the latest sweep (build
  40): crazyhouse, threecheck, koth and grand, with standard as the
  guard.

### Standing

Build 47 against FSF 2100, main's rounds of 200 in queues 301 to 307:

| Variant | Rounds | Mean |
|---------|-------:|-----:|
| crazyhouse | 9 | -110.4 |
| threecheck | 5 | -69.6 |
| grand | 5 | -56.6 |
| koth | 7 | -43.6 |
| shogi | 2 | -4.4 |
| standard | 7 | +184.4 |

Build 48 has not played. Stage 0 plays the base rounds. (Corrected
2026-11-15: the table counts every full round of main in those queues;
it first left out rounds of threecheck, grand, koth and standard and
gave -79.2, -50.9, -39.9 and +180.2.)

### What plan 33 showed

- Merged, strength: each gave a term an input it lacked. Passed pawns
  in FSF's form +31.4 (build 36), the clock's 20 moves +36.9 (37), the
  opening pawn +27.1 (38), the archbishop as a major +15.2 (39), the
  unstable root +16.5 (40), the dearest type's unsafe check in a race
  +82 in fivecheck (41), the pawn mass +30.4 in horde (45). Build 46 is
  a correctness fix (a repetition from before the root).
- Merged, speed: builds 42, 43, 44, 47 and 48, each with the same
  nodes.
- Never paid:
  - a port that only raises the fit to FSF's term (kdmob, pstr2, KH,
    the typed check weights, the hand terms, dropmob);
  - a cut of the tree (FR, QP, chp, xf, xfc, lmptried);
  - the clock's shape by ply (tshape, tshape2, tshape3);
  - a split read off one round: three times it was noise.
- The resolution of three rounds of 200 is near 20 Elo on a mean; a
  speed claim on this machine needs more than 3% (its clock falls under
  a long load).

### Method notes

Faults of the last iteration, each with its guard:

- attackstarts, attackkeys and attackindex repeated "start index" and
  "attacker keys", which plan 33 had closed after build 42. Guard: a
  speed try searches the plans by the macro or function it touches, not
  only by the idea; the closed list below is read first.
- `hotpath` times of nested blocks were read as costs. Plan 32 had
  shown that the timers inflate them. Guard: the `hotpath` build gives
  counts only; a cost comes from running the block twice.
- Two wrapped doc list items reached main, and some reverts were made
  with `git checkout` or `git stash`. Guard: the style scan runs on
  each diff before a build; a revert is an Edit.

## Style audit

All 43 source files, 39159 lines, by a scan of each rule that a script
can test:

| Check | Hits |
|-------|-----:|
| code past column 80, line past 120, comment not at column 81 | 0 |
| `//` comment, comment on an empty line, em dash, `#[allow]`, test | 0 |
| doc block without its closing `///`, file without its header | 0 |
| wrapped doc list item | 2, fixed |
| Params or Return against the signature | 0 (7 scan errors, read) |
| one-letter `let` | 62, flagged |
| one-letter closure parameter | 58, flagged |

- Fixed (58a3086): the `pushed` item of `pawn_structure!` and the
  `landings` item of `derive_goal_race`; `timed` and `fs`, two unused
  workspace dependencies that warned on each build.
- Flagged, not changed (user, 2026-11-14): the one-letter names. The
  bindings are the modifier letters of the move notation in the leg
  walkers (`m`, `c`, `d`, `u`, `k`, `g`, `v`, `t`, `i`, `p`, `r`; 25 in
  `move_list.rs`, 14 in `move_parse.rs`) and `s` in `graphics.rs`; the
  closure parameters are mostly in `move_parse.rs` (37).
- Fixed (9791d0c): `SearchResult.total_elapsed` held milliseconds under
  a doc and a print for nanoseconds, so a search of 226 ms printed
  "226 ns". The info line is unchanged.
- Leftovers deleted: twelve probe scripts of closed chains in
  `~/aneka-q/tools`, a parameter backup, a stray log, and the scratch
  outputs. Kept there: `hopeless.py`, `summary.py`, `swings.py`,
  `games.py`, `timeuse.py`, `style.py`, `fsfscore.py`, `idlebench.ps1`.

## Speed map of build 48

Eight balanced middle games a variant (our first move from move 15 with
our score within 100 cp, queue305's games), each searched to a fixed
depth: standard 14, crazyhouse 10, grand 15, koth 12, threecheck 16.

### Calls for each node (the `hotpath` build, counts only)

| Call | standard | crazyhouse | grand | koth | threecheck |
|------|---------:|-----------:|------:|-----:|-----------:|
| make and undo | 2.68 | 5.20 | 3.53 | 2.45 | 2.27 |
| check test | 2.58 | 3.52 | 3.14 | 2.65 | 2.79 |
| pick | 2.75 | 7.28 | 3.94 | 2.33 | 2.50 |
| exchange | 0.63 | 0.78 | 0.79 | 0.49 | 0.47 |
| attacker list | 1.26 | 1.04 | 1.84 | 0.82 | 0.81 |
| evaluation | 0.68 | 0.57 | 0.66 | 0.67 | 0.71 |
| attack pass | 0.59 | 0.42 | 0.56 | 0.67 | 0.70 |
| generation, board and captures | 0.47 | 0.42 | 0.42 | 0.41 | 0.43 |

### True cost (one block run twice, the rise in time for each node)

A scratch build (`plan34-probe`) runs one block twice under
`std::hint::black_box`; medians of three alternating passes.

| Block | standard | crazyhouse | grand | koth | threecheck |
|-------|---------:|-----------:|------:|-----:|-----------:|
| evaluation, whole | 26.7% | 17.1% | 23.6% | 39.3% | 24.2% |
| the attack pass in it | 21.4% | 10.0% | 19.2% | 31.2% | 17.8% |
| exchange, with its makes | 19.5% | 15.3% | 22.8% | 11.4% | 12.2% |
| check test, every caller | 11.2% | 12.3% | 13.0% | 6.2% | 7.3% |
| board generation | 9.3% | 4.5% | 8.3% | 3.3% | 4.6% |
| attacker lists | 7.1% | 3.7% | 8.5% | 1.7% | 3.1% |
| capture generation | 5.6% | 0.7% | 2.8% | 0.1% | 0.7% |

- The blocks overlap: the exchange holds its attacker lists and the
  check tests of its makes.
- Sampled shares (line tables, the setup's samples taken off) agree in
  order. They add: the selection scan of `pick_by_score!` is 14.2% of
  crazyhouse (3% to 5% elsewhere), where a list with drops is long and
  7.3 moves are picked for each node; `goal_race!` is still 10.9% of
  koth.

### Where the makes go (the probe's counters, for each node)

| Makes | standard | crazyhouse | grand | koth | threecheck |
|-------|---------:|-----------:|------:|-----:|-----------:|
| all | 2.68 | 5.20 | 3.53 | 2.45 | 2.27 |
| of the exchange | 1.31 | 1.28 | 1.88 | 0.91 | 0.87 |
| illegal, at a node in check | 0.50 | 3.11 | 0.73 | 0.68 | 0.56 |
| of those, own reach untouched | 0.11 | 1.65 | 0.21 | 0.13 | 0.15 |
| undone as no check given | 0.13 | 0.09 | 0.22 | 0.09 | 0.10 |
| nodes in check | 0.066 | 0.096 | 0.037 | 0.094 | 0.053 |

- Nearly every illegal make is at a node in check: the search makes
  each pseudo-legal move to learn that it does not answer the check.
  That is 19% of all makes in standard, 21% in grand, 25% in
  threecheck, 28% in koth and 60% in crazyhouse, where each node in
  check makes near 32 illegal moves (the drops).
- LG (plan 29) skips the check test of a make whose squares are outside
  the royal's reach; the make and the undo still run, and the reach is
  wide (it holds every line to the royal), so it settles only a fifth
  to a half of these makes.

### The spots

A spot is substantial at 5% true cost or more in a weak variant, with a
named way to cut it.

| Spot | Cost | Way | Stage |
|------|------|-----|-------|
| makes that do not answer a check | 19% to 60% of the makes | test the move against the squares the checking attack reads, before the make | 1 |
| the exchange | 11% to 23% | piece lists that an undo restores, then exact shortcuts | 2, 3 |
| the attack pass | 10% to 31% | none exact yet; koth's goal race is a third of it | 4 |
| the pick's scan in crazyhouse | 14% sampled | none exact yet: a skipped late quiet move is still picked | 4 |
| board generation | 3% to 9% | plain vectors of pieces that can promote (plainzone) | done |
| the check test | 6% to 13% | closed in seven forms; stage 1 takes its share at nodes in check | 1 |

### Closed speed tries, not to repeat

| Try | Where | Result |
|-----|-------|--------|
| attacks of a square from the attack maps | plan 32 stage 1 | not exact for gates, conditions and target rules |
| attacker keys, attackkeys | plan 33 | x1.01 to x1.07 |
| start index, attackstarts | plan 33 | standard x0.94, grand x1.02 to x1.05 |
| plainattacks (a path for a plain attack) | plan 33 | x1.00 to x1.07 |
| attackindex (the pieces, not the entries) | plan 33 | x0.99, in the noise |
| prefix skip of a blocked slide | plan 33 | x1.02 to x1.06 |
| seecut, lvaclass, seecache | plan 33 | not exact, or slower |
| sortedlists | plan 33 | exact order, 1% to 9% slower |
| one sort in the pick (s32) | plan 32 | not faster, trees change |
| move tries (f30, f31) | plans 30, 31 | not faster |
| pattern sets as words, early exit (pc, pt) | plan 32 | not faster |

## Quick fixes

### plainzone

- Build 48 gave a vector a mask form only when its piece cannot
  promote, so each pawn and most shogi pieces kept the leg walk (shogi
  gained x0.983). A vector cannot promote when its origin and each of
  its leg squares are outside the promotion zones of its piece; the
  zones are static, so `generate_plain_steps` tests them at load.
- Retrospective: plainpath (build 48) is the one earlier try of the
  mask form; the prefix skip and the tries (above) cut the walk another
  way and were not faster.
- Exact: perft at depth 4 the same in eight variants; the perft suites
  of standard (20256 cases), shogi, crazyhouse and xiangqi pass; seeded
  nodes the same in the 39 variants of up to 128 squares; perft the
  same on chushogi and tjatoer.
- Speed against build 48:

| Measure | shogi | standard | others |
|---------|------:|---------:|--------|
| bench, medians of 5 | x0.888, x0.892 | not stable | crazyhouse x0.952, xiangqi x0.958, capablanca x0.943, minishogi x0.962, grand x0.988 |
| bench, 15 pairs | x0.975 | x1.001 | euroshogi x0.939 |
| perft, medians of 9 | x0.915 | x1.002 | xiangqi x0.967, grand x0.977, crazyhouse x1.005 |
| middle games, fixed depth | x0.952 | x0.988 | grand x0.984, koth x0.990, threecheck x0.994, crazyhouse x1.006 |

- A form that keeps the walk for a one-leg vector of a piece that can
  promote was not better (shogi perft x0.937), so every such vector is
  plain.
- Departure: the audit's bar for a quick fix was 3% in a test variant.
  The gain is in the shogi family, which is not in the test set; the
  five test variants are level. It is exact and slower in none, so it
  merged on the standing speed rule as build 49 (main 66a6ccd).
- A pair of runs on this machine now differs by up to 15% either way
  with nothing else running, so a claim rests on the median of the
  ratios of alternating pairs (`speed.sh`) and on more than one
  measure.

### Not taken

- The pick's scan tests each candidate for a missing score on every
  pass. Only the first pass of a node, or a list that drops joined,
  holds one, so the test can leave the later passes. At most 2% to 3%
  of crazyhouse, under what this machine can show: not built.

## Stages

Each speed stage is exact (the same perft, the same seeded nodes) and
merges on speed; each strength stage gets its retrospective table and a
rule set before its games. The next merge is build 50: the sweep, then
the pause.

### 0. Base rounds

- The newest build in crazyhouse, threecheck, koth, grand and standard,
  one round of 200 each, with the first candidate queue: the standing
  after builds 48 and 49.

### 1. evasion: a move that cannot answer the check is not made

- A (the counters above): at a node in check, each pseudo-legal move is
  made, tested and undone; 19% to 60% of all makes end so.
- Mechanism: an attack on the royal holds as long as no square it reads
  changes. The squares one attack reads are its start, each leg square
  and each square of its condition (the same set that the reach of LG
  joins over every attack). A move of another piece that changes none
  of the squares of one attack that holds now (its start, end, captured
  and unload squares) leaves that attack as it is, so the royal stays
  attacked and the move is illegal: it needs no make.

| Try | Result | Recorded cause | Precondition | What differs now |
|-----|--------|----------------|--------------|------------------|
| LG (plan 29, merged) | x0.93 to x0.98 | the check test reads what the move cannot change | the reach of every attack on the royal square, tested inside the make | one attack that holds, read before the make; the make and the undo are saved too |
| the check test in seven forms (plans 32, 33) | none faster | the scan is the cost | each test made cheaper | the tests of these moves are not run at all |

- Form: at a node in check with one royal, find the first attack entry
  that holds (the scan the check test makes) and keep its squares; in
  the move loops of `alpha_beta` and `quiescence_search`, skip a quiet
  move, a single capture or a drop of a piece that is not royal when
  none of its changed squares is among them. An attack with an en
  passant leg before its last reads a square no move names: no skip.
  `make_move!` and the skip share one form of the changed squares.
- Proof: the same seeded nodes in the 39 variants, the perft suites,
  large-board perft; the makes for each node fall by the "illegal, at
  a node in check" row. Expected: 5% (standard, grand, koth,
  threecheck) to 15% (crazyhouse) of the time.
- Built (`plan34-evasion`, from build 49): `check_squares` finds the
  first attack on the lone royal that holds and marks its squares with
  `mark_attack_reads`, which the reach derivation of LG now calls too;
  `changed_squares!` is the one form of a move's squares, for
  `make_move!` and for `leaves_check!`; the two move loops skip a move
  that `leaves_check!` names.
- Anomaly, chased: the first form changed the trees (crazyhouse bench
  442555 to 375447) with every perft the same. Cause: a make and its
  undo leave the mover, and a piece it captures, at the end of their
  piece list, an illegal make too; the skip left that out, and the
  later generation order moved (stage 2's cause). `unmade_lists!`
  moves the same squares to the end of their rows for a skipped move,
  and the nodes are build 49's again. It goes when stage 2 makes an
  undo restore the lists.
- Exact: seeded nodes the same in the 39 variants; perft the same on
  chushogi and tjatoer; perft at depth 4 the same in five variants and
  the four perft suites pass (the `make_move!` change); the same nodes
  on the 40 middle games below. A skipped legal move would take its
  node out of a count, so the equal counts also show that no legal move
  was skipped.
- Speed against build 49, the 8 middle games of each variant to a
  fixed depth, seven alternating passes (median of the pair ratios, and
  their range):

| Variant | Time | Range |
|---------|-----:|-------|
| crazyhouse | x0.912 | 0.886 to 0.918 |
| grand | x0.974 | 0.951 to 0.996 |
| standard | x0.980 | 0.948 to 1.007 |
| koth | x0.982 | 0.954 to 1.004 |
| threecheck | x0.985 | 0.957 to 1.004 |

- Under the expected 5% outside crazyhouse: a skipped move is still
  picked (its scan, and the exchange of a capture), and an illegal make
  was cheaper than a make that goes on to a node. Merged on speed as
  build 50 (main e3e3142).

### Build 50 against FSF 2100, all variants (400 games each)

- `~/aneka-q/sweep.sh b50 2100`, 36 variants, 2026-11-14 to 2026-11-15.
  Our engine forfeited no game. ± is the 95% bound; the change is
  against build 40's sweep (plan 33).

| Variant | Build 50 | ± | Build 40 | Change |
|---------|---------:|--:|---------:|-------:|
| shatranj | +332.2 | 43.9 | +356.6 | -24.4 |
| asean | +326.4 | 35.8 | +278.5 | +47.9 |
| amazon | +310.0 | 48.0 | +209.9 | +100.1 |
| los-alamos | +259.0 | 38.9 | +181.7 | +77.3 |
| almost | +242.2 | 42.4 | +143.1 | +99.1 |
| pocketknight | +200.2 | 38.6 | +149.3 | +50.9 |
| standard | +192.0 | 37.0 | +153.4 | +38.6 |
| ai-wok | +185.1 | 37.4 | +156.6 | +28.5 |
| makruk | +179.5 | 24.7 | +170.6 | +8.9 |
| kinglet | +171.7 | 36.7 | +100.9 | +70.8 |
| knightmate | +151.3 | 37.8 | +58.7 | +92.6 |
| minixiangqi | +148.2 | 34.5 | +113.3 | +34.9 |
| modern | +123.0 | 33.3 | +48.1 | +74.9 |
| xiangqi | +119.1 | 36.5 | +49.0 | +70.1 |
| minishogi | +111.4 | 31.5 | +124.0 | -12.6 |
| chancellor | +100.0 | 30.6 | +58.7 | +41.3 |
| hoppelpoppel | +80.4 | 33.8 | +25.2 | +55.2 |
| fivecheck | +42.8 | 35.3 | -75.0 | +117.8 |
| janus | +38.4 | 32.7 | +4.3 | +34.1 |
| judkins | +35.7 | 33.8 | +11.3 | +24.4 |
| embassy | +28.7 | 31.8 | +1.7 | +27.0 |
| newzealand | +8.7 | 32.3 | -6.9 | +15.6 |
| extinction | -0.9 | 32.8 | -20.0 | +19.1 |
| gothic | -3.5 | 33.1 | -21.7 | +18.2 |
| capablanca | -6.9 | 32.5 | -31.4 | +24.5 |
| euroshogi | -13.0 | 33.9 | -34.9 | +21.9 |
| grand | -13.9 | 31.4 | -82.3 | +68.4 |
| threecheck | -19.1 | 30.3 | -123.0 | +103.9 |
| koth | -20.0 | 32.4 | -118.1 | +98.1 |
| shogi | -23.5 | 31.9 | -52.5 | +29.0 |
| horde | -37.5 | 19.2 | -52.5 | +15.0 |
| crazyhouse | -125.0 | 35.5 | -143.1 | +18.1 |

- Not valid (FSF forfeits from the known rule differences): chigorin
  +171.7 (36), janggi (all 400), ouk-chaktrang +186.2 (54 illegal, 2
  time), sittuyin +169.5 (204). Shatranj counts 137 FSF forfeits and
  stands, as in build 40's sweep.
- 22 of 32 valid variants at or above 2100, 10 below (build 40: 20 and
  12). The baseline stays 2100.
- Only crazyhouse and horde are below by more than their bound; the
  other eight are within it.
- 30 of 32 rose against build 40: mean +46.5, median +36.7. The two
  drops, shatranj -24.4 and minishogi -12.6, are within the bounds.
- The weakest: crazyhouse, horde, shogi, koth, threecheck; then grand
  and euroshogi.
- Open anomaly: threecheck and grand stand above build 47's rounds by
  more than noise, with only the speed merges of builds 48 to 50
  between (a few percent each). Threecheck: -69.6 over five rounds
  against -19.1, +50.5 (+2.7σ), and each of the five rounds is below
  the sweep. Grand: -56.6 over five rounds against -13.9, +42.7
  (+2.2σ). Crazyhouse (-0.7σ), koth (+1.2σ), shogi (-0.8σ) and standard
  (+0.4σ) agree with their rounds. The cause is not known. Stage 0's
  base rounds of build 50 come first after the pause and show whether
  it holds.
- The work is paused for the user; nothing is queued.

### 2. listrestore: an undo restores the piece lists

- Make removes a square from a piece list by a swap with the last and
  pushes the new one at the end; undo does the same, so a make and its
  undo leave another order. The move order follows the lists, so it
  follows the path of the search, and every exchange shortcut that
  skipped a make changed the tree (seecache, seecut; plan 33).
- Form: the mover's square is written in place; a captured piece's
  slot is kept in the snapshot and filled again on undo. Less work than
  now.
- It changes the trees once, with no expected sign. Rule, set by the
  user: better, the strength rule (+2σ on the mean over three rounds
  of 200, no test variant below -2σ, no forfeits).

### 3. The exchange on stable lists

- With stage 2, an exchange leaves no trace, so each of these is exact
  and is checked by identical nodes: the exchange cache again
  (seecache hit too rarely on build 46; measure the hit rate first),
  and a light make for a plain capture where no rule can end the game
  by a capture (no count of checks, goal, or extinction rule).
- The exchange is 11% to 23% of the time; its makes are 0.9 to 1.9 for
  each node.

### 4. The attack pass and the pick

- The attack pass is 10% to 31%. No exact cut is named. Leads to
  measure before any design: how often a move changes a piece's map
  (an incremental pass); koth's goal race against the royal squares and
  the holds it reads (a third of koth's pass).
- The pick's scan in crazyhouse (14% sampled): a late quiet move that
  will be skipped is still picked by a full scan. An exact form must
  keep the order of the later captures, which the swaps of each pick
  change; a form that changes ties needs games.

### 5. Strength leads

From plan 33's parked chains, each to be read in full first:

- FS ("To pick FS up again"): the overshoot of a fail-high score by
  variant and depth; the refined evaluation.
- The pawn structure ("To pick the pawn structure up again"): what
  FSF's `e_pawns` switch turns off.
- The safe check weight: a rule-derived part that reads what the royal
  must do (checkmean gained where the royal stays home and lost where
  it must walk).
- The king's evaluation in the check and drop variants, the largest
  static gap (crazyhouse -3.56 pawns, plan 33): single terms of FSF's
  all closed, so it needs a form of its own.

### 6. correct: the quiescence score and the repetition clock

Two defects from the user's correctness audit (2026-11-16), on branch
`plan34-correct` from main 97f5160, one commit each.

| Try | Result | Recorded cause | Precondition | What differs now |
|-----|--------|----------------|--------------|------------------|
| quiescence table score width | none earlier | | | first try |
| repetition clock (plan 13) | found, left open | each janggi general move is a promotion, so the scan bound never passes it | "derive a reversible-promotions flag from the promotion map"; the other form moves shogi nodes | the derived flag, set only for a promotion cycle |

- C1, `qwide`: the quiescence table kept its score as an `i16`
  (since cf7560e). A win `INF - ply` (2,000,000 less the ply) read back
  as -31,873 and a loss as +31,876; an exact entry cuts at once; the
  mate correction of the probe could never fire. The score now has
  bits 96 to 127, the flag bits 0 and 1, as wide as the main table's.
  - A stored score inside the `i16` range packs, reads and compares as
    before, so a tree can change only where a score past it was stored.
  - Seeded bench (depth 9, 40 positions) against main: standard,
    crazyhouse, grand, xiangqi and shogi the same; capablanca 1,648,334
    to 1,652,407, all of it from position 39, whose trees part at depth
    3 (617 to 616 nodes), a full-window depth where a win in quiescence
    is stored as exact.
- C2, `finalpromo`: the repetition clock restarted at each promotion,
  so in a variant without drops the scan never passed one. Janggi's
  palace swaps (K:Q, Q:K, A:F, F:A) are promotions both ways, so no
  cycle with a general or an advisor counted. Capability bit 9,
  `final_promotion!`, is set unless a chain of promotions leads a piece
  back to its own type; the clock restarts at a promotion only under it.
  - Of the 44 configs, only janggi lacks the bit; every other variant
    keeps its clock as before.
  - The general shuffle from `r7r/4q4/9/9/4p4/4P4/9/9/4Q4/R7R w /`
    (`e2:d2=K e9:d9=k d2:e2=Q d9:e9=q`) was Ongoing at the third and
    fourth occurrence; it is a draw at the third, as the chariot
    shuffle was before. This also makes janggi's `perpetual: check
    loss` able to fire (plan 13 found it inert).
  - Seeded bench (depth 8, 30 positions) against C1: the same in 16 of
    the 17 suites; janggi 784,138 to 786,037. The perft suites of
    janggi (21), standard (900), xiangqi, crazyhouse, capablanca and
    shogi pass.
- Build clean, clippy at its 20 warnings, the style scan of the diff
  clean.
- Due: games. C2 moves no tree outside janggi, which FSF does not play
  validly. C1 moves trees wherever a game ends inside quiescence.
- Rule set before the games (user, 2026-11-17), a regression arm: the
  two are fixes, so they merge together as build 51 unless they play
  worse. Worse is a mean of the five below -2σ, any variant below -2σ,
  or a forfeit. A run may stop early once a loss is shown.
- Queue (the Windows machine, `queue.sh`): main and `correct`
  (`plan34-correct`, 1bf1572 or later), interleaved, in crazyhouse,
  threecheck, koth, grand and standard; three rounds of 200 against FSF
  classical at `UCI_Elo` 2100 on its own 10+0.1 clock; `watch.sh` on.
  This Mac session cannot reach that machine, and the server does not
  answer, so the queue waits for a session there.
- Smoke (the Mac, `correct` against FSF 2100, 4 games a variant,
  concurrency 4): crazyhouse 2-2, grand 3-1, koth 4-0, standard 3-1,
  threecheck 4-0; no forfeits, illegal moves or crashes on either side.
