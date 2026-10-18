# Strength iteration 7: the tree gap and the audit experiments

## Status

Opened 2026-10-18 on build 34 (main 3731789). Stage 0 runs.

## Context

Build 34 is at or above FSF at UCI_Elo 2000 in every variant FSF plays.
The next target is full FSF. The philosophy audit of 2026-10-18 gave
nine findings that the user selected as mid-priority experiments (A1,
A2, A3, A6, A7, A9, A13, A17, A22). They ride along the main thread as
extra arms. Plan 29 stays the ledger of the earlier tries.

Method: a parity change that does not gain gets a why chain (A because
B because C because D). D is tested first. If D holds, the work acts on
D and climbs back to C, B and A. If not, the chain is built again. A
cause that comes from the design of both engines is recorded here, and
the work moves to another lead.

### Gaps of build 34

| Variant | FSF 2000 | 50% point vs full FSF (`nodestime`) | FSF full speed over it | Doublings | Elo at full speed (slope / games) |
| ------- | -------- | ----------------------------------- | ---------------------- | --------- | --------------------------------- |
| standard | +222 | 28.8 | x9.1 | 3.2 | -690 / -920 |
| xiangqi | +222 | 36.1 | x4.4 | 2.1 | -510 / -590 |
| crazyhouse | +15 | 27.7 | x9.0 | 3.2 | -320 / -560 |
| capablanca | +42 | 28.7 | x7.1 | 2.8 | -820 / -1040 |

- Near 2000 within one interval (queue231, build 32): koth -2,
  threecheck +5, hoppelpoppel +9, newzealand +12, crazyhouse +15.
- Scaling: near our 50% point, a doubling of FSF's effort is worth 215,
  241, 101 and 292 Elo to FSF. A doubling of our time was worth 75 to 82
  (build 16). It was not measured again since.
- Speed: plan 32 found the speed gap in the rule interpreter, a design
  cost. No lead there.
- Tree: FSF's lab puts LMR (157 to 338) and continuation history (166
  to 293) first in every variant. On build 16 our depth-1 loop made 1.84
  to 4.47 moves for each node against FSF's 1.31 to 1.41, and we reduced
  0.18 moves for each main node against FSF's 0.41.

### Earlier tries of the tree (plan 29)

| Try | Result | Recorded cause or precondition | Difference now |
| --- | ------ | ------------------------------ | -------------- |
| H1, H1b (history size in LMR) | 0, -13 | cells empty, then no gain | none, not tried again |
| H2, H2b, H2c (continuation pruning) | +16, +6, -3 | shogi drops; parked | none |
| H4 (continuation plies 4 and 6) | +4 | plies 1 and 2 already filled | none |
| LM2 (LMR from the second move) | -11.3 | open | stage 1 gives a cause |
| RI, T0, RIT0, NT | -7, -13.5, -5.5, +6.6 | wrong cuts; the tempo holds the null move back | none |
| PC, CR, PCR (check rule) | PCRH merged, hands only | the rule is cheap without a hand | none |
| SE (singular extension) | -30.7 | tree width and hard table bounds | only after FS |
| FS (fail soft, four parts at once) | -3.8, shogi +42 | open; "split it" never done | stage 2 |
| CO, CO2, PB | +4.3, -19.3, +4.4 | the order alone is worse | none |
| Our own search parts off, in games | never | | stage 1 |

## Stages

### 0. Record and baseline

- Main fast-forwarded to the doc audit (docs only).
- A22 first, on a server clone (see below), so that every later derive
  change starts from parameter files that equal the derivation.
- Self-ladder of build 34: our clock at 10+0.1, 20+0.2 and 40+0.4
  against full FSF whose `nodestime` halves as our clock doubles, from
  the test-set budgets (standard 28, xiangqi 34, crazyhouse 24,
  capablanca 26). One round of 200 games for each rung. The result is
  our Elo for each doubling on build 34.

### 1. Own search map

The plan 29c map, in our engine: each search part off, paired with
build 34, against full FSF at the test-set budgets. One round of 200
games in each variant, and a second round where the result is within
one sigma of FSF's lab value. Each part is a branch
`plan33-off-<part>` with a minimal edit. None merges.

Parts: lmr, lmrhist (the sign term), nmp, rfp, razor, futility, lmp,
seeprune, probcut, iir, conthist, corrhist, killers, hk (new tables for
each `go`), checkext, qchecks.

The table compares the value of each part in our engine with its value
in FSF's lab (plan 29c for standard and xiangqi, queues 195 to 197 for
crazyhouse and capablanca). For the part with the largest shortfall,
the FSF lab arm with that switch plays full FSF at the budget where the
lab base scores near 50%, as a like-for-like target. Counters on the
same 20 positions in both engines then find the mechanism.

### 2. FS split

FS (`plan29-fs` be0897a, on build 19) on build 34, in two arms:

- FS-T: a table cut gives the stored score. A node gives and stores its
  best score. Quiescence gives its stand pat or its best score. The
  correction history learns from the node score. Shortcuts give the
  window edge, as now.
- FS-S: FS-T and the shortcut scores (the static cut gives the
  evaluation, the null move its score, ProbCut and razoring their search
  scores). This is FS.

### 3. Lead from the map

The candidate comes from the stage 1 cause:

- LMR short: the gates and adjustments the counters point at, in
  `search.rs`.
- continuation history short: H2 without drops and H4 again, with the
  measured cause as the difference.
- a part worth more to us than to FSF: no change, recorded.

### 4. SE again

Only if FS-T or FS-S merges (the recorded precondition of SE is the hard
table bounds). SE (`plan29-se19` f916afa) on the new base, off with free
drops.

## Audit experiments

Each one is an extra arm in a main-stage queue, against the same base,
one at a time in this order. The embedded parameter files override the
derivation at load, so a change to derive code makes the files again in
the same branch. A derive of all 44 variants and a diff of `res/param`
and of the capability lines give the variants that change. Only those
play.

- A22, parameter files equal the derivation. On a server clone: remove
  the files, build, derive all, diff. Equal: recorded. Different: the
  new set is a candidate and plays first. Plan 29 once found the tempo
  held only by the files.
- A2, royal opening table from the rules. In `derive_pst` the royal and
  stand-in score is minus the forward distance from the royal's start
  rank (forward from `derive_forward_directions`), not minus the rank
  index.
- A6, coverage set of k copies. A type of reach r with k = round(1 / r)
  and |k r - 1| inside the slack gets the bonus at k copies. The
  evaluation reads a copy count for each piece in place of two. The pair
  is k = 2, so standard stays the same.
- A7, margin unit. The aspiration, reverse futility, futility, exchange
  prune, delta, razor and ProbCut margins scale by the mean deployed
  piece in place of the dearest. Each ratio is multiplied by standard's
  dearest over its mean, so standard keeps its margins. Extra check:
  amazon at FSF 2000.
- A3, occupancy from the setup. The opening occupancy is the setup
  pieces over the squares; the endgame occupancy is a third of it, as
  now.
- A1, value map. First the FSF removal-test ratios (Q/R, A/R, C/R,
  xiangqi R/C, N/P) against build 34's values. If compounds are still
  under FSF's ratio, the map is not the lever (plan 27's cause): closed.
  Else PV's form, `100 (raw / cheapest)^0.7`, which never played, takes
  the place of the shift.
- A13, roles and promotion gradient. A13a: not big is the geometric
  pawn test without the start count; major is the upper group of a
  two-means split of the log values of the big pieces. A13b: the
  promotion gradient reads the passed pawn ratios in place of 0.06. Hand
  power has no form from the rules and is not tested.
- A9, mobility for each piece. First FSF's chess mobility tables (N, B,
  R, Q) against our single curve and against a curve for each piece,
  centred on its mean mobility and scaled by its value share. It plays
  only if the curve for each piece follows FSF's tables better in
  standard.
- A17, open shield from enemy lines. For each royal square and colour,
  a derived square list (with a stride, as `shelter_squares`) of the
  squares in front of the royal and its two side neighbours on the rays
  of the enemy riders. `open_shield!` reads it in place of the file
  test.

## Rules

- Merge rule: interleaved with the base against full FSF at the
  test-set budgets, three rounds of 200 games in each variant, +2 sigma
  on the mean, no variant below -2 sigma. A change only outside the test
  set plays its variants at FSF 2000, six rounds, as FP did.
- A change with equal perft, seeded games and parameter files in all 44
  variants merges on `ident.sh` alone.
- One harness run at a time. The base and the candidates share each
  session. After each merge, the open arms move to the new build.

## Results

### A22 (2026-10-18)

- A clone of main without its parameter files, built and derived again
  for all 44 variants: every file is the same as the shipped one, byte
  for byte, and every capability line is as before. The shipped files
  are the derivation. Closed.

### A1 (2026-10-18)

- Build 34 values against FSF's removal-test ratios (plan 27), opening
  then endgame: standard Q/R 1.91, 1.88 (FSF 2.19); grand Q/R 1.95, 1.92
  (2.12), A/R 1.44, 1.24 (1.65); xiangqi R/C 1.88, 1.91 (1.58; it was
  1.04 in plan 27).
- PV's map, `100 (raw / cheapest)^0.7`, on standard's raw values (the
  shift is +78.5): N 640, R 957, Q 1580, so Q/R 1.65. The map moves the
  compounds further from FSF, while the pieces rise against the pawn.
- Why chain: the shift distorts the ratios, but the compounds are low
  because the model adds the mobility of each family (plan 27); a map
  of the totals cannot add worth to a compound. The rule set before the
  measure holds: compounds are still under FSF, so the map is not the
  lever. Closed without games. The open root is in the value model (a
  compound worth more than its parts), not in the map.

### A9 (2026-10-18)

- FSF's chess mobility tables (`evaluate.cpp`, N, B, R, Q) against our
  single curve `150 (m - 2) / (8 + m)` (200 in the endgame) and against
  a curve for each piece, zero at its mean mobility on the model board
  (occupancy 0.36 and 0.12) and spanning the same total over its own
  range of squares: root mean square error 15.6 and 33.4 for the single
  curve, 46.3 and 125.8 for the curve for each piece.
- Why: the model-board mean mobility (N 5.3, B 5.5 to 7.4, R 7.4 to
  11.1, Q 12.9 to 18.5) lies far above FSF's zero points (2 to 4
  squares). FSF counts only its mobility area, which leaves out the
  squares that real positions block. The board model does not see that.
- The rule set before the measure holds: the curve for each piece does
  not follow FSF's tables better. Closed without games.
- A13b (promotion gradient from the passed pawn ratios) gets no arm: it
  would count a pawn's promotion twice, in the passed pawn term and in
  the table.

### Arms built (2026-10-18)

Each on its own branch from main, built and checked locally (release
build, 80 columns, a seeded search from the start, seed 7, depth 10).

| Arm | Branch | Seeded nodes against main | Variants that change |
| --- | ------ | ------------------------- | -------------------- |
| FS-T | `plan33-fst` | bench x0.94 standard, x0.955 xiangqi | all |
| FS-S | `plan33-fss` (on FS-T) | bench x0.89 standard, x1.03 xiangqi | all |
| A2 | `plan33-a2` | standard same | needs the derive diff (parameter files) |
| A3 | `plan33-a3` | | all, needs new parameter files |
| A6 | `plan33-a6` | | none of the test set; shogi (`P` x9) |
| A7 | `plan33-a7` | standard same; xiangqi 67789 (main 83780) | all but standard |
| A13 | `plan33-a13` | | capablanca (A major), xiangqi (soldier not big; H, C, R major), shogi |
| A17 | `plan33-a17` | standard same; xiangqi same | standard, crazyhouse, capablanca, shogi |

- Bench: depth 10, 20 positions, seeds 1 to 3, total nodes against
  main. One seeded search from the start gave FS-T twice main's nodes
  (55870 against 28316); the bench shows that this was the one
  position, not the change.
- A6 cover sets found: standard, capablanca, grand B x2; crazyhouse B,
  V x2; makruk M x2; shatranj Q x2 and B x8; xiangqi E x10 and A x8
  (never complete); shogi P x9, L x9, N x4, B x2. Only shogi's P x9 is
  new and reachable: a full row of file-bound pawns gets the bonus.
- A13a roles (big, major) by value: standard Q major as before (the
  split {N, B, R} | {Q} has squared error 0.157 against 0.211 for {N, B}
  | {R, Q}); crazyhouse the same; capablanca A, C, Q major (was C, Q);
  xiangqi soldier not big, advisor and elephant minor, H, C, R major
  (was: advisor not big, R and C major).
- A17 removes `forward_steps`, which nothing reads after it.
- A2's start rank: janggi's general starts on the second rank, so
  janggi is the first candidate to change.

### Other findings on the way

- `derive_search_parameters` writes the razor margins and the ProbCut
  margin with raw units: `mean / 3 + 100`, `mean / 2 + 200`, `mean +
  300` and at least 100. They do not scale with the variant.

### Queue236 (stages 0 and 1)

- The 16 parts off, one branch each (`plan33-off-<part>`, a one-line
  edit, never merged): lmr, lmrhist, nmp, rfp, razor, futility, lmp,
  seeprune, probcut, iir, conthist, corrhist, killers, hk, checkext,
  qchecks.
- Each plays one round of 200 games against full FSF at the test-set
  budget. Build 34 plays three rounds in each variant (at the start, the
  middle and the end), as the paired base and the 10+0.1 rung of the
  self-ladder. The rungs 20+0.2 and 40+0.4 follow, with FSF's
  `nodestime` halved for each doubling (xiangqi 17 and 8, capablanca 13
  and 6, rounded down).
