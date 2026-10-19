# Strength iteration 7: the tree gap and the audit experiments

## Status

Opened 2026-10-18 on build 34 (main 3731789). Stages 0 and 1 played
(queues 236 to 238); the LMR why chain ended (queues 239 to 241); pv1 merged as build 35
(queue242); stage 2 (FS) next.

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
- Base, three rounds (mean): standard -63.2, -90.6, -20.9 (-58.2);
  xiangqi -81.4, -77.7, -61.4 (-73.5); crazyhouse -8.7, -6.9, -38.4
  (-18.0); capablanca -54.3, -29.6, -61.4 (-48.4). Below queue235 at the
  same budgets; see queue237.
- Each part off, minus the base mean (σ near 26; a minus means the
  part is worth that much to us), beside FSF's lab value (plan 29c, FSF
  against FSF at 10+0.1, standard and xiangqi):

| Part off | standard | xiangqi | crazyhouse | capablanca | FSF lab standard, xiangqi |
| -------- | -------- | ------- | ---------- | ---------- | ------------------------- |
| lmr | -26.8 | +73.5 | -50.6 | -27.5 | -184.0, -156.6 |
| lmrhist | -19.5 | -4.2 | +14.5 | +41.5 | -40.1, -36.6 |
| nmp | -10.4 | +40.4 | -11.6 | +46.7 | -54.3, -23.5 |
| rfp | -21.3 | +19.2 | -89.5 | -11.2 | -27.9, -13.9 |
| razor | -26.8 | +24.5 | -8.1 | +6.5 | |
| futility | -34.3 | -7.9 | -2.9 | +36.2 | -11.3, -15.6 (fpp) |
| lmp | -70.8 | -24.6 | -27.4 | -27.5 | -58.7, -65.0 (mcp) |
| seeprune | -39.9 | +47.4 | -8.1 | +11.8 | -32.2, -9.6 (seecap) |
| probcut | -21.3 | -6.0 | +18.0 | -0.6 | -9.6, +12.2 |
| iir | -47.4 | +50.9 | -2.9 | +39.7 | +5.2, -1.7 |
| conthist | -17.7 | +40.4 | +4.1 | +6.5 | -166.2, -212.9 |
| corrhist | -14.0 | +15.6 | -20.4 | +1.2 | |
| killers | -14.0 | +6.7 | +4.1 | -11.2 | +6.1, -9.6 |
| hk | +11.0 | +28.1 | +0.6 | -55.3 | |
| checkext | +12.8 | +4.9 | -31.0 | +15.3 | -3.5, +15.6 |
| qchecks | -21.3 | -2.4 | +4.1 | +8.3 | |

- No part of ours is worth more than 90, and only rfp in crazyhouse
  (-89.5) and lmp in standard (-70.8) pass 2σ. LMR and continuation
  history, FSF's two largest parts, are worth near nothing to us.
- Self-ladder (base, then 20+0.2 and 40+0.4 with FSF's effort fixed):
  standard -58.2, +31.4, +164.1; xiangqi -73.5, +63.2, +139.0;
  crazyhouse -18.0, +83.2, +179.5; capablanca -48.4, +66.8, +186.2.
  A doubling of our time is worth 111, 106, 99 and 117 on average (build
  16: 75 to 82).

### Queue237, the base after the reboot (2026-10-18)

- The server rebooted at 15:43 into a new kernel (6.12.113, installed
  on 10-17 at 06:45; queue235 and every build 32 round ran before, on
  6.12.111). Queue236 was the first queue after it.
- Build 34 in the queue235 protocol, on the idle server: standard -56.1,
  -15.6; xiangqi -36.6, -40.1. All five rounds since the reboot: standard
  -49, xiangqi -59. Before it (build 32, six rounds, the same search in
  these variants, and queue235): standard -12.2 and +8.7, xiangqi +3.5
  and +20.9. Near -40 (2.8σ) and -63 (4.5σ).
- Not the machine speed: FSF from the start 272k nodes a second
  (queue235: 262k); build 22 by `fsfspeed.py` 0.448M, 0.279M, 0.309M,
  0.391M (queue170: 0.450M, 0.298M, 0.324M, 0.407M); our bench under 31
  copies loses 13% (plan 29: 22%). Clock source `kvm-clock`; our search
  reads the clock every 2048 nodes. Binaries, protocol and harness are
  the same.
- Cause not found. Paired tests in one session stay valid; levels from
  before and after 10-18 15:43 are not compared.

### Queue238, FSF's LMR and continuation history at our budget

The FSF lab arm against full FSF, both at the test-set `nodestime`, two
rounds of 200, minus the lab base (FSF against itself, near 0):

| Part off | standard | xiangqi | crazyhouse | capablanca |
| -------- | -------- | ------- | ---------- | ---------- |
| lmr | -126.5 | -147.8 | -122.7 | -166.8 |
| conthist | -46.4 | -84.3 | -44.2 | -66.0 |

At our budget FSF's LMR is still worth 120 to 170 and its continuation
history 44 to 84; ours -27 to +74 and -18 to +40. LMR is the largest
shortfall in every variant, so it is the stage 1 lead.

### Why chain: LMR

- A: our LMR is worth near nothing (mean of four variants -8 ± 13),
  FSF's 141 ± 12 at the same budget.
- Tree shrink to a fixed depth (nodes off over on, geometric mean, the
  first 17 positions of FSF's chess bench at depth 10 and the 11 xiangqi
  suite positions at depth 9, ours seed 1): ours x3.18 and x3.19, FSF
  x1.84 and x1.81. With LMR on, our tree is x1.72 and x1.94 FSF's to the
  same depth. So our LMR saves more nodes than FSF's, not fewer.
- Depth in 400 ms (standard, the same 17): 9.71 with LMR, 8.00 without.
  At our 106 to 117 per doubling, x3.2 nodes should be worth 150 to 230;
  FSF's x1.84 near 210, of which it keeps 127. We keep near 15%.
- B: a ply that our LMR buys is worth much less than a ply of the
  full tree. Counters (temporary, `plan33-cnt-lmr`, never merged; FSF in
  a copy of the lab): each reduced move, its first search, the re-search,
  and after a low first search a full-depth check search (a false low if
  it passes alpha). Standard, depth 10, sums over the 17 positions:

| Class | Engine | Reduced | Mean plies | First search high | Re-search confirms | False lows |
| ----- | ------ | ------- | ---------- | ----------------- | ------------------ | ---------- |
| capture, promotion | ours | 49236 | 1.42 | 1.0% | 80% | 0.28% |
| capture, promotion | FSF | 46717 | 1.92 | 3.4% | 79% | 1.12% |
| quiet, history above 0 | ours | 8245 | 1.02 | 8.2% | 77% | 1.23% |
| quiet, history above 0 | FSF | 106199 | 1.61 | 7.5% | 74% | 3.17% |
| quiet, history below 0 | ours | 289073 | 1.98 | 3.1% | 30% | 0.47% |
| quiet, history below 0 | FSF | 158873 | 2.22 | 2.5% | 61% | 1.15% |

- Xiangqi at depth 9 gives the same picture: false lows ours 0.18%,
  2.8%, 0.71%; FSF 0.61%, 4.2%, 1.2%.
- So our reductions are not wrong more often than FSF's: per class,
  fewer of our reduced moves would have passed alpha at full depth.
  Two differences stand out: 97% of our reduced quiet moves have a
  history below zero (FSF 60%), so our history term adds a ply to near
  every quiet move; and only 30% of our high first searches of such moves
  hold at full depth (FSF 61%).
- Queue239 tests B as stated: our off-lmr at 3.2 times the clock and
  FSF's lmr arm at 1.8 times the nodes, each against full FSF at the
  effort of the base, so each plays at the nominal depth of its LMR on.
  If B holds, our off-lmr beats our base by far more than FSF's lab arm
  beats its base.

### Queue239, LMR off at the depth of LMR on

Two rounds of 200 per arm, each against full FSF at the effort of the
base (σ near 23 per variant, 12 on the mean):

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| ours, off-lmr x3.2 minus base | +68 | +138 | +88 | +109 | +101 |
| FSF, lmr arm x1.8 minus its base | +44 | +18 | -18 | +43 | +22 |

- Depth check (`go movetime`, ours): standard, the 17 positions, main
  400 ms 9.76, off-lmr 1280 ms 9.71; xiangqi, 10 suite positions (one is
  a forced line at depth 241 or more and is left out), 9.2 and 9.8. So
  standard plays at equal depth and xiangqi's off-lmr is 0.6 ply deeper,
  near 35 of its 138.
- B holds: at equal depth our LMR costs near 70 to 100 Elo of play, FSF's
  22. Per doubling of nodes it saves, ours costs near 50, FSF's 25.
- The per-class counters above do not explain it: FSF makes 3.5 times
  as many false lows in all (near 5700 against 1600) and loses less.
  So where the false lows fall matters, not how many there are.

### Why chain: LMR, where the false lows fall

- Counters by node type (the same branch and lab copy, extended): per
  PV and non-PV node, the reduced moves, the false lows, and the false
  lows at nodes that end below beta, where the false low changes the
  value the node returns.

| Variant | Engine | PV reduced | PV false lows (below beta) | PV rate | non-PV rate |
| ------- | ------ | ---------- | -------------------------- | ------- | ----------- |
| standard | ours | 31220 | 185 (175) | 0.59% | 0.43% |
| standard | FSF | 9405 | 24 (22) | 0.26% | 1.78% |
| xiangqi | ours | 12601 | 154 (147) | 1.22% | 0.64% |
| xiangqi | FSF | 4543 | 6 (6) | 0.13% | 1.86% |

- C: our harmful false lows fall on PV nodes, 8 times FSF's count in
  standard and 25 times in xiangqi; off the PV ours are rarer than FSF's.
  A false low at a PV node changes the line the search returns.
- D: our reduction has no node type term (only the two extra full depth
  moves at a wide window), so a PV node is cut as deep as any node. FSF
  takes one ply off at a PV node and two more at a ttPv node, three at a
  PV node in all.
- Queue240 checks D, and is the candidate: `REDUCTION_PV` plies off the
  reduction at a PV node, 3 (`plan33-lmr-pv`) and 1 (`plan33-lmr-pv1`),
  interleaved with main under the merge rule. Tree to depth 10 (the 17
  positions): x1.69 and x1.15 main's.

### Queue240, the PV cut

Three rounds of 200, minus the base mean (σ near 20 per variant, 10 on
the mean). Base: standard -67.5, xiangqi -64.5, crazyhouse -28.7,
capablanca -58.5.

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| pv (3 plies) | -6.3 | +17.0 | +0.8 | +11.8 | +5.8 |
| pv1 (1 ply) | +17.8 | +11.8 | -10.9 | +32.3 | +12.8 |

- Neither passes +2σ (20); none falls below -2σ. Not merged.
- pv's tree, x1.69, is 0.76 doublings, near 82 at our ladder; pv1's
  x1.15 near 22. If the rest is play quality, pv wins back near 88 and
  pv1 near 35, so the PV cut buys about what it costs in depth. Queue241
  measures this at equal depth instead of inferring it.

### Why chain: what a protected PV node costs

- Counters (the same branch and lab copy), PV nodes that reach the move
  loop and the moves they search:

| Variant | Engine | PV nodes | PV share of nodes | Moves searched per PV node | Later move beats alpha, per PV node |
| ------- | ------ | -------- | ----------------- | -------------------------- | ----------------------------------- |
| standard | ours | 4236 | 0.48% | 20.97 | 0.59 |
| standard | FSF | 5703 | 0.81% | 6.41 | 0.14 |
| xiangqi | ours | 2065 | 0.69% | 16.53 | 0.48 |
| xiangqi | FSF | 2515 | 0.73% | 6.12 | 0.13 |

- We have fewer PV nodes than FSF, not more. Each searches three times
  the moves: our move pruning needs a zero window, FSF's runs at every
  node but the root. At a PV node FSF has 31.2 legal moves, picks 9.7
  (the move count skip drops the rest of the quiet moves) and searches
  6.4; continuation history pruning, futility and SEE remove the other
  3.3 (xiangqi 24.3, 8.4, 6.1).
- Pruning at PV nodes in ours (arms pp0, pp1, pp3, never played) leaves
  the tree as it was: x0.99, x1.13, x1.78 main's to depth 10. Most PV
  nodes are shallow and cheap; the PV cut costs at the deep ones, where
  FSF skips little too. Archived and deleted.
- FSF's own PV and ttPv terms (new lab switch `pvadj`, off by default,
  base bench nodes unchanged) grow FSF's tree x1.52 (standard) and x1.54
  (xiangqi). Protecting the PV costs both engines near the same tree.
- Queue241, two rounds: pv3x, our pv at 1.65 to 1.73 times the clock
  (equal depth) against FSF at the base effort; fsfpv, the lab `pvadj`
  arm against full FSF at the test-set budget.

### Queue241, the PV cut at equal depth

Two rounds of 200 (σ near 12 on the mean). Base: standard -89.7,
xiangqi -47.5, crazyhouse -35.8, capablanca -49.0.

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| pv3x minus base | +66.9 | +63.2 | +86.6 | +91.8 | +77.1 |
| fsfpv (FSF, its PV terms off) | +14.8 | +19.3 | +33.2 | +38.4 | +26.4 |

- D holds at equal depth: the PV cut wins back 77 of the 70 to 100 our
  LMR costs in play.
- With it our LMR matches FSF's: tree x3.18 / 1.69 = x1.88 (FSF x1.84),
  cost in play near 101 - 77 = 24 (FSF 22).
- FSF's own PV terms cost FSF 26 at this budget: at near 7000 nodes a
  move, PV protection does not pay for FSF either.

### End of the LMR chain

- The same LMR is worth near +14 to us (pv against off-lmr, queues 236
  and 240) and 141 to FSF, because a doubling of nodes is worth less to
  us: LMR off, ours gains 65 a doubling against full FSF (queue239),
  FSF's lab arm near 185.
- FSF's figure is FSF against itself, which inflates it, and FSF plays at
  near 7000 nodes a move (depth near 8.5) where a doubling is worth more;
  ours plays near 100k nodes (depth near 10). The value of a doubling is a
  property of the whole engine, evaluation and search, not of LMR.
  Recorded as fundamental; the next lead is stage 2 (FS).
- pv1 (+12.8, 1.3σ) plays three more rounds (queue242).

### Queue242, pv1 again; merged build 35

Three more rounds of 200, minus the base mean:

| Rounds | standard | xiangqi | crazyhouse | capablanca | mean |
| ------ | -------- | ------- | ---------- | ---------- | ---- |
| queue242 (d to f) | +26.0 | +9.4 | +76.8 | +11.0 | +30.8 |
| queues 240 and 242 (six) | +21.9 | +10.6 | +33.0 | +21.7 | +21.8 |

- Six rounds: σ near 14 per variant and 6.9 on the mean, so +21.8 is
  3.1σ; no variant below -2σ. Merged as build 35 (`plan33-pv1`, one
  commit): `REDUCTION_PV` 1, one ply off the reduction at a PV node.
- The arms still to play (FS-T, FS-S, A7, A13, A17, A2, A3, A6) rebase
  on build 35.
