# Strength iteration 7: the tree gap and the audit experiments

## Status

Opened 2026-10-18 on build 34 (main 3731789). Stages 0 and 1 played
(queues 236 to 238); the LMR why chain ended (queues 239 to 241); pv1
merged as build 35 (queue242); stage 2 (FS) parked near +8 after the
why chain (queues 243 to 249); A7, A13 and A17 closed; continuation
history: chp (the skip) and the reply malus (hm, chm) do not merge
(queues 250 and 251); the skip is precise but its region is half of
FSF's, as each check at the last ply is extended; xf (no extension
there) and xfc (chp on xf) do not merge (queues 253 and 254): the skip
costs us the play it costs FSF, and the saved tree buys us less, the end
of the LMR chain. Both tree chains end at the value of a doubling; next,
plies per doubling against Elo per ply. A2 and A3 closed (queue252).

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
- The part-off arms, the PV arms, the counter branch and the FSF counter
  copy are archived (`anekamacam-vps-archive/clones`, md5 checked) and
  deleted with their branches. The lab keeps `pvadj`, off by default.

### Queue243 (stage 2 and A7)

FS-T, FS-S and A7 on build 35, interleaved with main under the merge
rule, three rounds of 200. Base: standard -53.2, xiangqi -29.6,
crazyhouse -25.7, capablanca -26.2. Minus the base mean (σ near 20 per
variant, 10 on the mean):

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| FS-T | +0.2 | -23.8 | -37.1 | +4.7 | -14.0 |
| FS-S | -52.8 | -23.5 | -29.8 | -19.6 | -31.4 |
| A7 | -5.3 | +4.6 | +0.7 | +10.6 | +2.6 |

- None merges. FS-S falls to -3.2σ (standard -52.8, below -2σ). A7 is
  closed: margins in units of the mean piece play the same.

### Why chain: FS

- A: fail-soft scores lose (FS-T -14, FS-S -31), though FSF returns the
  same scores: the node and table score, the quiescence best score, the
  static cut's evaluation, the null move's score and ProbCut's.
- B (open): our correction history learns from the node score (`gap =
  score - eval`, weighted by depth). With hard bounds the gap stops at
  the window edge; a soft bound can lie a piece past it, and FS-S's
  shortcut scores (the evaluation of a child cut) are the farthest. The
  corrections then swing, and they steer the reverse futility, null move
  and ProbCut gates. FSF of this era has no correction history, so it
  pays nothing for soft scores there.
- Queue244 checks B: fstcc and fsscc, FS-T and FS-S with correction
  history fed the window edge as on main (`plan33-fstcc`,
  `plan33-fsscc`). If B holds, both come back to the base or above. A13
  rides along.

### Queue244, FS with the window edge in correction history

fstcc, fsscc and A13 on build 35, interleaved with main under the merge
rule, three rounds of 200. Base: standard -86.5, xiangqi -54.9,
crazyhouse -20.3, capablanca -40.3. Minus the base:

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| fstcc | +27.3 | +26.9 | -44.2 | -8.1 | +0.5 |
| fsscc | -16.0 | +14.6 | -49.3 | +33.4 | -4.3 |
| A13 | +6.9 | +14.7 | +9.3 | -12.3 | +4.7 |

- B holds in part: FS-T rises from -14.0 to +0.5 and FS-S from -31.4 to
  -4.3. Soft bounds in correction history were a cost, but with it gone
  FS plays level with the base, not above it.
- Crazyhouse loses in all four FS arms of queues 243 and 244 (-37.1,
  -29.8, -44.2, -49.3; near -40, σ near 10 over the four). The other
  variants sit at or above the base in fstcc.
- A13 is closed: +4.7, within one σ. Standard plays the same search, so
  its +6.9 is the noise of the session.

### Why chain: FS in crazyhouse

- A: FS plays level with the base, not above it, because crazyhouse
  loses near 40 in every FS arm.
- B: the crazyhouse loss is not a bigger tree. Seeded bench, depth 10 to
  12, 16 seeds, fstcc over main in nodes: standard x0.861 ± 0.077,
  xiangqi x1.049 ± 0.046, crazyhouse x0.988 ± 0.065, capablanca x1.178
  ± 0.112. Crazyhouse searches the same tree and plays worse, so its
  soft scores cut the wrong lines.
- C (open): quiescence plays no drops, so in a drop variant its stand
  pat and best capture are poor bounds; past the window they say less
  still. FS-T returns and stores them, and a later probe with a lower
  window cuts on them.
- Queue245 checks C: fstq, fstcc with quiescence giving the window edge
  in a drop variant (`plan33-fstq`; the same search as fstcc outside
  drop variants), against fstcc and main in crazyhouse, three rounds of
  200. If C holds, fstq comes back to main in crazyhouse, and fstq then
  plays the merge rule. A17 rides along in the full test set.

### Queue245, quiescence and A17

Base: standard -47.3, xiangqi -45.4, crazyhouse +1.8, capablanca -66.7.
Minus the base:

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| fstcc | | | -65.7 | | |
| fstq | | | -48.1 | | |
| A17 | -12.0 | +23.9 | -26.8 | +45.9 | +7.8 |

- C does not hold: fstq is within one σ of fstcc (+17.6), and both lose
  in crazyhouse. Quiescence is not where the soft scores go wrong.
- A17 is closed. Xiangqi plays the same search, so its +23.9 is the
  noise of the session; the three variants that change give +2.4.

### Why chain: FS in crazyhouse, again from B

- C2 (open): a node that fails low gives the best score of the moves it
  searched. Late move pruning, futility and SEE skip moves without a
  score, and one of them may hold more, so the bound is too low. A later
  probe with a lower window cuts on it. In crazyhouse the drops make the
  list long and most of it is skipped, so more of its fail-low bounds
  are wrong.
- FSF does the same: a skipped move leaves its best value as it is.
  Its own soft bounds carry the same error, so C2 is a cause only if our
  pruning skips more of the list than FSF's does.
- Seeded search, depth 9 crazyhouse and 10 standard, seed 7, four
  positions: fstlo (`plan33-fstlo`, fstcc where a node that fails low
  after a skip gives alpha) searches 145973 nodes in crazyhouse against
  fstcc's 191035 and main's 150474; standard 101361 against 121968.
- Queue246 checks C2: fstlo against fstcc and main in crazyhouse, three
  rounds of 200. If C2 holds, fstlo comes back to main in crazyhouse,
  and then plays the merge rule.

### Queue246, alpha after a skipped move

Crazyhouse, base -41.4. fstcc -20.6, fstlo -20.1 against it.

- C2 does not hold: fstlo plays as fstcc. Its tree is a quarter smaller
  and plays the same, so neither the size of the tree (B) nor the
  fail-low bounds carry the loss.
- Over six sessions FS loses in crazyhouse by 21 to 66 (fstcc, fsscc,
  fstq, fstlo).

### Why chain: FS in crazyhouse, which score

- Two soft scores of FS-T are left: the score of a node that fails high
  and the score of a table cut on a bound. Queue247 splits them in
  crazyhouse: fsthi (`plan33-fsthi`, fstcc where a node that fails high
  gives and stores beta) and fsttt (`plan33-fsttt`, fstcc where a table
  cut on a bound gives the window edge), against fstcc and main, six
  rounds of 200 (a crazyhouse round takes near two minutes).
- The arm that comes back to main names the score; the mechanism is
  then looked for in that score alone.

### Queue247, the fail-high score

Crazyhouse, six rounds, base -35.6 (σ near 14 for each arm against it):

| Arm | Against the base |
| --- | ---------------- |
| fstcc | -10.6 |
| fsthi | +9.4 |
| fsttt | -10.6 |

- The table cut is not it: fsttt plays as fstcc. The fail-high score is
  the lead: fsthi is 20 above fstcc (1.4σ) and level with the base.
- The score of a node that fails high goes two ways: into the table,
  where later probes cut on it and refine the evaluation with it, and
  to the parent, where it becomes a fail-low score. Queue248 splits the
  two in crazyhouse: fsths (`plan33-fsths`, the table keeps the score,
  the parent gets beta) and fsthr (`plan33-fsthr`, the table keeps
  beta, the parent gets the score), with fsthi again, against main, six
  rounds of 200.

### Queue248, the table and the parent

Crazyhouse, six rounds, base -22.1:

| Arm | Table | Parent | Against the base |
| --- | ----- | ------ | ---------------- |
| fsthi | beta | beta | +9.3 |
| fsths | score | beta | -15.5 |
| fsthr | beta | score | -28.2 |

- fsthi holds its queue247 value (+9.4); over twelve rounds it sits
  near +9 against the base and near 20 above fstcc.
- Each half loses alone. The cost is in the score itself, wherever it
  goes, and not in one use of it.
- D (open): the score is a mate. Crazyhouse mates by drops and checks
  far more than the other test variants. With hard bounds a wrong mate
  score stops at beta in the node that found it; a soft one reaches the
  parent and the table, and from there the root. Queue249 checks D in
  crazyhouse: fsthm (`plan33-fsthm`, fstcc where a node that fails high
  on a mate gives beta), with fsthi and main, six rounds. If D holds,
  fsthm plays as fsthi. Both play the merge rule in the full test set in
  the same queue (rounds a to c).

### Queue249, the mate and the merge rule

Base (rounds a to c): standard -71.7, xiangqi -50.8, crazyhouse -31.5
(six rounds -20.4), capablanca -39.6. Minus the base:

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| fsthi | -14.1 | +27.0 | +21.0 (six rounds +3.8) | 0.0 | +8.5 |
| fsthm | -26.1 | -21.0 | -30.4 (six rounds -32.5) | -2.9 | -20.1 |

- D does not hold: fsthm loses in crazyhouse as fstcc does. The fail-high
  score that costs is not a mate.
- fsthi does not merge (+8.5, under 1σ). Over three sessions it sits near
  +7 in crazyhouse.

### FS parked

- Tried: correction history fed the soft score (a cost, removed),
  quiescence (no), alpha after a skipped move (no), the table cut (no),
  the fail-high score (yes, in the table and in the parent alike), a mate
  (no). The best FS arm, fsthi (fail-soft below the window, hard above),
  plays near +8 and does not merge.
- The chain stops short of a root cause: why a soft score above the
  window costs us in crazyhouse is not found. FS is worth at most near
  10 on the data, so the next lead is the larger shortfall, continuation
  history (FSF 44 to 84 at our budget, ours -18 to +40). SE (stage 4)
  stays off, as FS did not merge.

### To pick FS up again

The chain is open, not closed. Where it stands and what comes next:

- Known: A (FS gains nothing) holds because crazyhouse loses near 40
  (queues 243 to 247). B: the crazyhouse tree is the same size, so the
  soft scores steer the search to worse moves. C (narrowed, queues 247 and
  248): the cost is the score of a node that fails high. Returned to the
  parent alone (fsthr) it loses, stored in the table alone (fsths) it
  loses, both as beta (fsthi) it plays level or better.
- Ruled out for that score: a mate score (fsthm, queue249), quiescence
  (fstq), the table cut (fsttt), the fail-low bound after a skip (fstlo).
- Leads not yet tested:
  - The size of the overshoot. Count, per variant, how far a fail-high
    score lies past beta at each depth, and how often a deeper search of
    the same node holds that score. If crazyhouse overshoots far more, or
    its overshoots fail at depth more often, the soft bound is not a
    sound bound there, and the next why is which shortcut or skip in the
    child makes it unsound.
  - The evaluation refined by a soft lower bound (`bound_eval`, FBETA
    gives `max(eval, score)`). fsths and fsttt keep it and both lose;
    fsthi removes it with the rest. An arm that stores the soft score but
    refines the evaluation only up to the window edge splits this out.
  - The root: a zero-window score at or above beta skips the full-window
    re-search, at the root too (FSF re-searches at the root).
  - Crazyhouse leans on reverse futility more than any part (worth 89 to
    us, queue236), and reverse futility reads the refined evaluation.
- Code and tools: `plan33-fsthi` is kept (fstcc's two commits plus beta
  on a fail high). The other arms are archived in
  `anekamacam-vps-archive/clones` (aneka-fstcc, fsscc, fstq, fstlo, fsttt,
  fsths, fsthr, fsthm). A crazyhouse round of 200 takes near two minutes,
  so six rounds per arm (σ near 14) cost near half an hour.
- Pick it up again after continuation history, or sooner if fsthi
  reaches the merge rule (queue250 adds three rounds).

### Why chain: continuation history

- A: FSF's continuation history is worth 44 to 84 at our budget, ours
  near nothing.
- The LMR chain found that a node saved is worth less to us than to FSF
  (a doubling 65 against 185). If continuation history saves us as much
  tree as it saves FSF, the same cause holds and the chain ends there; if
  it saves us far less, the cause is in the ordering, and the chain goes
  on from there.
- Check: nodes to depth 10 of `plan33-off-conthist` (ordering without
  continuation history, as the stage 1 arm) over main, and of FSF's lab
  `conthist` arm over the lab base, on the bench positions of FSF's chess
  bench that have no moves appended.
- Result, 15 positions, depth 10, geometric mean of nodes off over on
  (± one standard error of the log):

| Arm | Saving |
| --- | ------ |
| ours, continuation history off | x0.995 ± 0.091 |
| FSF `conthist` | x1.332 ± 0.138 |
| FSF `chprune` | x1.353 ± 0.081 |
| FSF `lmrhist` | x1.237 ± 0.128 |
| FSF `fpp` | x1.187 ± 0.100 |
| FSF `mainhist` | x0.773 ± 0.121 |

- B: our continuation history saves no tree, FSF's saves a third, so
  the LMR end (a node worth less to us) does not reach this part. FSF's
  whole saving comes through `chprune`: a late quiet move whose two
  continuation cells are both negative is skipped below a reduced depth
  of 5. In move order alone the cells save FSF near nothing, as they
  save us nothing. Ours never prunes on them.
- C: the skip is missing. Plan 29 tried it as H2 (+16, then H2b +6 and
  H2c -3 on build 16, parked as noise); its tree was never measured.
  `plan33-chp` ports H2 to build 35 (drops skipped too, as in FSF).
  First check: its tree saving against FSF's x1.35.
- On the 15 positions chp gives x1.004 ± 0.129, but our trees to a fixed
  depth swing from x0.46 to x2.15 between the two builds position by
  position, so 15 positions cannot tell x1.0 from x1.35. Counters
  (temporary, `plan33-cnt-chp`, archived): the skip takes 46.6% of the
  quiet moves at zero-window nodes, and 23.2% are moves no other skip
  would take. Cells are present in 97.5%; the first is negative in 67.7%,
  the second in 61.7%, zero in 28.9% and 35.4%.
- The seeded bench (`treesize.sh`, 16 seeds, depth 12, 10, 10, 11) gives
  nodes over main:

| Arm | standard | xiangqi | crazyhouse | capablanca |
| --- | -------- | ------- | ---------- | ---------- |
| chp | x0.858 ± 0.069 | x0.915 ± 0.046 | x1.064 ± 0.082 | x0.898 ± 0.099 |
| continuation history off | x0.832 ± 0.063 | x1.200 ± 0.035 | x1.058 ± 0.074 | x1.135 ± 0.092 |

- So chp saves x1.09 to x1.17 outside crazyhouse, short of FSF's x1.35,
  and grows the crazyhouse tree. Queue250 plays chp under the merge rule,
  with fsthi for three more rounds.

### Queue250, chp and fsthi

Base (rounds a to c): standard -89.5, xiangqi -62.1, crazyhouse -34.3,
capablanca -32.0. Minus the base:

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| chp | -21.5 | -12.7 | +34.9 | -32.6 | -8.0 |
| fsthi | +0.6 | +13.5 | +39.6 | -17.1 | +9.2 |

- chp does not merge. It loses where it saves tree (standard x1.17,
  capablanca x1.11) and gains where the tree grows (crazyhouse).
- fsthi does not merge. Over queues 249 and 250 (six rounds) it is +8.8,
  near 1σ on the mean; crazyhouse +16 over nine rounds.

### Why chain: continuation history, the skip

- A: chp does not gain (-8). B: it loses where it saves the most tree,
  so it skips moves that matter there; the saved tree is worth less than
  the skipped moves cost.
- C: our cells say little about a reply. A quiet move that fails takes
  the malus in every node, so in a node where all moves fail (most
  zero-window nodes) every quiet move tried loses credit as a reply. FSF
  takes the malus only when a later quiet move cuts, and only from the
  quiet moves tried before it. Thus our cells are negative for most
  replies (the first 67.7%, the second 61.7%), and "both negative" picks
  46.6% of the late quiet moves, good or bad.
- Plan 01 (stage 2) tried FSF's rule for the move table alone, before
  continuation history existed: trees grew 19 to 47%, no games. Here the
  move table keeps the all-node malus; only the reply table changes.
- Arms: `plan33-hm` (main, reply malus only when a later quiet move cuts,
  at most 64 moves) and `plan33-chm` (chp on hm). Nodes over main on the
  seeded bench:

| Arm | standard | xiangqi | crazyhouse | capablanca |
| --- | -------- | ------- | ---------- | ---------- |
| hm | x0.812 ± 0.074 | x1.102 ± 0.040 | x1.069 ± 0.060 | x1.132 ± 0.074 |
| chm | x0.838 ± 0.074 | x1.154 ± 0.037 | x1.185 ± 0.073 | x1.120 ± 0.087 |

- hm grows the tree as continuation history off does (standard aside),
  and on hm the skip fires less (chm over chp near x1.1 to x1.25). Tree
  size did not predict chp's games, so queue251 plays both under the
  merge rule.

### Queue251, the reply malus

Base (rounds a to c): standard -41.1, xiangqi -33.9, crazyhouse +12.2,
capablanca -49.8. Minus the base:

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| hm | -29.4 | -5.7 | -2.9 | +31.2 | -1.7 |
| chm | -47.9 | -13.8 | +14.6 | +32.9 | -3.6 |

- Neither merges. The base's standard is near 40 above its usual score
  (rounds b and c, -29.6 and -10.4), so the standard column leans low.
- C does not hold: cells that take the malus FSF's way do not make the
  skip pay. The arms are archived (aneka-hm, aneka-chm).

### Why chain: continuation history, from B again

- B said the skip cuts moves that matter. Check: the precision of the
  skip in both engines. Counters (temporary; ours `plan33-cnt-skip` on
  chp, FSF a lab copy) tag the moves the skip would take instead of
  skipping them, and count at zero-window nodes how often a tagged move
  that no other skip takes fails high, against the other quiet moves
  with a reduced depth below 5 (no checks). Our seeded bench (seed 1),
  FSF's chess bench:

| Engine | Tagged fail high | Others fail high | Ratio | Tagged share |
| ------ | ---------------- | ---------------- | ----- | ------------ |
| FSF chess, depth 11 | 2.76% | 7.28% | 0.38 | 51% |
| FSF chess, depth 13 | 2.48% | 7.79% | 0.32 | 51% |
| ours standard, depth 12 | 1.79% | 10.0% | 0.18 | 70% |
| ours xiangqi, depth 10 | 0.74% | 7.47% | 0.10 | 61% |
| ours crazyhouse, depth 10 | 2.32% | 11.1% | 0.21 | 51% |
| ours capablanca, depth 11 | 3.03% | 12.7% | 0.24 | 54% |

- B does not hold: our skip takes fewer good moves than FSF's, and our
  cells tell good replies from bad ones better than FSF's do.
- B2: the skip saves us little. Tagged moves are 9.7% of our nodes in
  standard (xiangqi 10.2%, crazyhouse 5.9%, capablanca 8.0%) against
  15.7% in FSF; each costs near the same subtree (1.5 nodes against 1.65,
  from the tree savings). chp's time saving follows its node saving
  (standard x0.858, xiangqi x0.973). At our 108 a doubling, x1.10 is
  worth near +11; chp played -8 ± 10.
- Our quiescence share is near FSF's (38% against 32%, plan 29), so the
  missing tagged moves are not spent in quiescence. Next check (after
  queue252, no builds while it plays): the funnel of the quiet moves in
  both engines, from the moves tried after the first, through the
  reduced depth below 5, to the tag, at zero-window nodes.

### A2 and A3, parameters derived again

- On build 35 clones without parameter files, built and derived for all
  44 variants (one at a time; three at once ran out of memory at
  taikyoku). Capability lines are the same as main's in both.
- A2 changes only grand (its royal starts on the second rank): it plays
  grand against FSF at UCI_Elo 2000, six rounds.
- A3 changes all 44 files: it plays the merge rule.
- The regenerated files are in each branch's commit. Cargo did not
  rebuild when only the parameter files changed, so the server builds
  were forced (crate root touched).
- Queue252: A3 and main, three rounds of the test set; then A2 and main
  in grand at FSF 2000, six rounds.

### Queue252, A3 and A2

- A3, minus the base (base: standard -68.8, xiangqi -49.0, crazyhouse
  -35.5, capablanca -19.2): standard -34.9, xiangqi +10.6, crazyhouse
  +26.2, capablanca -26.9, mean -6.3. Not merged; closed.
- A2 in grand at FSF 2000, six rounds: main +147.1, A2 +156.1, so +9.0
  (σ near 16). Not merged; closed. Grand is far above 2000 either way.
- Both clones archived (aneka-a2-b35, aneka-a3-b35). The audit arms left:
  A6.

### Why chain: continuation history, the region of the skip

- B2 (the skip saves little) from the region the skip works in. Counters
  (temporary, `plan33-cnt-skip`, and the FSF lab copy), at nodes out of
  check with a zero window:

| Engine | Loop nodes of all nodes | Quiet moves past the move count, per node | Reduced depth below 5 | Tagged per node |
| ------ | ----------------------- | ----------------------------------------- | --------------------- | --------------- |
| FSF chess, depth 13 | 28.7% | 1.74 | 95% | 0.63 |
| ours standard | 13.1% | 1.49 | 99% | 0.89 |
| ours xiangqi | 15.7% | 4.24 | 99% | 1.20 |
| ours crazyhouse | 17.9% | 6.67 | 100% | 1.32 |
| ours capablanca | 16.7% | 2.03 | 99% | 0.82 |

- Per node of its region our skip tags more moves than FSF's. The region
  is the smaller share: our node count holds every horizon node twice
  (`alpha_beta` counts it before it hands depth 0 to quiescence, which
  counts it again), and without that 28% of our nodes are main-search
  nodes out of check with a zero window, against FSF's 56%. Of those,
  60% reach the move loop (FSF 51%), so the cuts before the loop are not
  the cause.
- The rest of our main search is in check: in standard 231151 of 443231
  nodes with depth above 0 (PV nodes 4315). By depth, in check against
  out of check at depth 1:

| Engine | In check, depth 1 | Out of check, depth 1 | Share in check |
| ------ | ----------------- | --------------------- | -------------- |
| FSF chess | 130215 | 2919930 | 4.3% |
| ours standard | 151839 | 142976 | 51% |
| ours xiangqi | 9954 | 76165 | 12% |
| ours crazyhouse | 17216 | 151915 | 10% |
| ours capablanca | 31715 | 255118 | 11% |

- C2: the skip's region is half of FSF's because the check rule extends
  every check, at the last ply too, so each check at depth 1 opens a
  depth-1 node in check where FSF goes to quiescence. FSF extends a
  check only at depth above 6 with the evaluation more than 100 from
  zero. The lab put our extension rule at near nothing to FSF (`s1ext`:
  standard -4, xiangqi +14); the earlier tries of fewer extensions (XS,
  XS2, plan 27) played grand and shogi in self-play, never standard
  against FSF.
- Arms: `plan33-xf` (main; a check at depth 1 is not extended, so
  quiescence answers it) and `plan33-xfc` (chp on xf). Seeded bench over
  main, nodes and time:

| Arm | standard | xiangqi | crazyhouse | capablanca |
| --- | -------- | ------- | ---------- | ---------- |
| xf nodes | x1.039 ± 0.091 | x1.013 ± 0.055 | x1.152 ± 0.068 | x1.041 ± 0.076 |
| xf time | x0.909 ± 0.092 | x1.036 ± 0.081 | x1.188 ± 0.071 | x1.062 ± 0.089 |
| xfc nodes | x0.770 ± 0.082 | x0.890 ± 0.064 | x1.070 ± 0.080 | x0.863 ± 0.091 |
| xfc time | x0.673 ± 0.079 | x0.974 ± 0.093 | x1.126 ± 0.091 | x0.883 ± 0.101 |

- On xf the skip saves x1.35 in standard (xfc over xf x0.741 ± 0.074),
  FSF's figure; capablanca x1.21, xiangqi x1.14, crazyhouse x1.08 (on
  main: x1.17, x1.11, x1.09, x0.94). C2 holds in the tree. Queue253
  plays main, xf and xfc under the merge rule.

### Queue253, xf and xfc

Base (rounds a to c): standard -68.7, xiangqi -35.1, crazyhouse +0.6,
capablanca -50.0. Minus the base:

| Arm | standard | xiangqi | crazyhouse | capablanca | mean |
| --- | -------- | ------- | ---------- | ---------- | ---- |
| xf | -24.5 | +39.2 | -10.5 | +15.7 | +5.0 |
| xfc | -26.0 | -72.3 | -23.3 | -7.2 | -32.2 |

- Neither merges; xfc falls below -2σ in xiangqi.
- C2 holds in the tree, but the larger saving does not reach the games:
  xfc minus xf is -1.5 in standard, at x0.741 the time.

### Why chain: continuation history, what the saved tree buys

- The skip on xf, per variant: the time it saves (seeded bench; xiangqi
  again at depth 12, x0.786 ± 0.093, nodes x0.739 ± 0.081), the worth of
  that time at our 108 a doubling, the played result (xfc minus xf), and
  their difference, the cost of the skip in play at equal depth:

| Variant | Time, xfc over xf | Worth of the time | Played | Cost in play |
| ------- | ----------------- | ----------------- | ------ | ------------ |
| standard | x0.740 | +47 | -1.5 | 48 |
| xiangqi | x0.786 | +38 | -111.5 | 149 |
| crazyhouse | x0.948 | +8 | -12.8 | 21 |
| capablanca | x0.831 | +29 | -22.9 | 52 |

- FSF's `chprune`, the same arithmetic in standard: tree x1.353 (0.44
  doublings), near +81 at its 185 a doubling, worth 32 (plan 29c), so
  near 49 of play.
- Standard: the skip costs both engines near the same play (48 against
  49); the tree it saves buys FSF near 81 and us 47. That is the end of
  the LMR chain: a doubling is worth less to us. Both tree chains, LMR
  and continuation history, end at the value of a doubling.
- The counters on xf (`plan33-cnt-skip` with xf, seed 1) give the skip
  the precision it had on main:

| Variant | Tagged fail high | Others fail high | Ratio | Tagged per loop node |
| ------- | ---------------- | ---------------- | ----- | -------------------- |
| standard | 1.23% | 10.4% | 0.12 | 1.04 |
| xiangqi | 0.88% | 5.7% | 0.15 | 1.46 |
| crazyhouse | 2.62% | 11.3% | 0.23 | 1.21 |
| capablanca | 3.18% | 12.4% | 0.26 | 0.76 |

- Xiangqi is the anomaly: on xf the skip costs 149 of play, on main
  near 17 (chp -12.7 at x0.973 the time), and FSF's `chprune` is worth 66
  to FSF in xiangqi, its largest. The precision does not explain it.
  Quiescence after an evasion is not the cause either: wide quiescence
  holds in the whole test set, so it searches every capture there, as
  FSF's does. Queue254 plays xiangqi alone, main, chp, xf and xfc, six
  rounds, to separate the interaction of xf and the skip from noise.

### Queue254, the skip and xf in xiangqi

Six rounds of 200 (σ near 10 per arm). Base (rounds d to i): -53.2.
Minus the base:

| Arm | Queue254 | Nine rounds with queue253 (chp with queue250) |
| --- | -------- | --------------------------------------------- |
| chp | -12.3 | -12.4 |
| xf | +6.3 | +17.3 |
| xfc | -27.1 | -42.2 |

- The interaction (xfc - xf - chp) is -21 here, near 1σ: the anomaly
  of queue253 was mostly noise (xf +39 and xfc -72 there, both far from
  these rounds). Over nine rounds the skip on xf costs near 98 of play
  in xiangqi, against 48 to 52 in standard and capablanca; higher, but
  of the same kind.
- The continuation history chain ends where the LMR chain ended: the
  skip costs us the play it costs FSF, and the tree it saves buys us
  less, because a doubling is worth less to us. xf, xfc and chp are
  closed.
- Two tree chains end at the value of a doubling (ours 108 against
  full FSF, FSF's 215 against us near our 50% point). Next check: how
  many plies a doubling buys each engine (the growth of nodes per ply),
  which splits the value of a doubling into plies per doubling and Elo
  per ply.
