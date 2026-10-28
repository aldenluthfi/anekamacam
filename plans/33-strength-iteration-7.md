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
of the LMR chain. Both tree chains end at the value of a doubling: a
doubling buys us fewer plies and a ply is worth less to us. The eval
map (queue256): threats match FSF's value; the king (outside xiangqi),
passed pawns and pawn structure in standard fall far short; queue257
measures FSF's terms at the same budget. All audit arms are closed (A2
and A3 queue252, A6 queue255). The danger sum's mobility input (kdmob)
is closed (queue261). Passed pawns in FSF's form merged as build 36
(queue262, +31.4 ± 9.0). The test opponent is FSF at `UCI_Elo` 2100 from
queue262 on, on the Windows machine. Queues 267 to 272 cut nine
candidates near +10 or below; an A/A run (queue273) put one-round noise
at up to 88 Elo between two copies. The clock now plans for 20 moves
(15 where captures fill the hand): build 37, +36.9 ± 10.4 (queue277).
A pawn that promotes to the dearest piece is cheaper in the opening:
build 38, +27.1 ± 8.8 (queue280). An archbishop counts as a major for
the danger cut: build 39, +15.2 ± 7.2 (queue281). pieceg, xsee and
nmeval did not merge. An unstable root may start a depth until half the
time: build 40, +16.5 ± 7.2 over six pooled rounds (queues 281 and
285). The user asked (2026-10-27) for build 40's sweep against FSF 2100
as well as 2000; it runs now. At build 40, and at every tenth build
after it, a sweep plays every variant that FSF plays against FSF
at `UCI_Elo` 2000, 400 games each (asked by the user, 2026-10-26).

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

### Plies per doubling

- Growth per ply from depth 9 to 13 (ours: seeded bench, seed 1, 12
  positions; FSF: its chess bench):

| Engine | Nodes per ply | Time per ply | Plies per doubling | Elo per doubling | Elo per ply |
| ------ | ------------- | ------------ | ------------------ | ---------------- | ----------- |
| ours standard | x1.97 | x1.89 | 1.09 (time) | 108 | near 100 |
| ours xiangqi | x1.70 | x1.71 | 1.29 (time) | | |
| FSF chess | x1.68 | | 1.33 | 215 | near 160 |

- Both parts are short: a doubling buys us fewer plies, and a ply is
  worth less to us. A nominal ply is not the same unit in both engines
  (each depth holds its own reductions and extensions), so the split is
  rough; the two factors are near 1.2 and 1.6.
- Our tree without LMR still grows faster than FSF's without LMR (near
  x2.2 a ply against x1.79, from the LMR tree ratios at depth 10), so
  the extra growth is not in LMR. The ledger closes the check rule as
  its source (PC26, CR26, PCR26 near nothing outside crazyhouse; xf
  above).
- Elo per ply is the evaluation's side. The search map (stage 1) never
  had an evaluation half: our own terms off, in games, were never
  played. That is the next lead.

### Queue255, A6

- Derived again on the A6 build (parameter files removed, built,
  derived): all 44 files equal the shipped ones; the cover sets are
  derived at load, so A6 changes only the code.
- Shogi at FSF UCI_Elo 2000, six rounds: main +90.2, A6 +77.5, so -12.7
  (σ near 16). Not merged; closed. All audit arms are closed.

### Eval map

The stage 1 map for the evaluation: each group of terms off, paired with
main, against full FSF at the test-set budgets, one round of 200 per
variant (base three rounds), never merged. Arms (`plan33-off-<arm>`):

| Arm | Off | FSF lab switch |
| --- | --- | -------------- |
| kdanger | the danger sum of the attack pass | `e_kdanger` |
| kzone | zone pressure and proximity | part of `e_king` |
| shelter | shelter, guard, castling, open shield | part of `e_king` |
| threats | hanging, pawn threats, restricted squares | `e_threats` |
| mobility | mobility | `e_mobility` |
| passed | passed pawn bonus | `e_passed` |
| pawns | pawn structure, passed pawns with it | `e_pawns` and `e_passed` |
| imbalance | major, minor and cover set terms | `e_imbalance` |
| lazy | the attack terms are never skipped | |

- FSF's lab values (self-play, 400 games): standard `e_king` 295,
  `e_passed` 229, `e_mobility` 87, `e_threats` 83, `e_imbalance` 57,
  `e_pawns` 45; xiangqi `e_king` 95, `e_mobility` 56, `e_threats` 36;
  crazyhouse `e_king` 214, `e_mobility` 96; capablanca `e_king` 288,
  `e_threats` 85, `e_mobility` 80.
- The term with the largest shortfall against FSF's value gets the why
  chain, as LMR did for the search.

### Queue256, the eval map

One round of 200 per arm, the base three (σ near 27 per arm against the
base mean). Base: standard -69.1 (-27.9, -98.1, -81.4), xiangqi -29.9
(-65.0, -50.7, +26.1), crazyhouse -28.5, capablanca -25.0. Worth of
each term to us, the base minus the arm (above 0, the term helps):

| Term | standard | xiangqi | crazyhouse | capablanca |
| ---- | -------- | ------- | ---------- | ---------- |
| kdanger | +3 | +105 | +5 | +13 |
| kzone | -15 | +10 | +31 | -13 |
| shelter | +40 | -6 | +21 | +17 |
| threats | +78 | +42 | +5 | +79 |
| mobility | +37 | +30 | +13 | +79 |
| passed | -6 | +42 | -4 | -15 |
| pawns | -1 | +52 | -8 | -46 |
| imbalance | +18 | -7 | +5 | +15 |
| lazy | +16 | -2 | +10 | +24 |

- Lazy changes nothing in standard (its tree equals main's), so its +16
  there is the noise of one arm.
- Near FSF's value: threats (standard 78 against 83, xiangqi 42 against
  36, capablanca 79 against 85), mobility in capablanca (79 against
  80), the king in xiangqi (danger 105 against `e_king` 95).
- Short of it: the king in standard, crazyhouse and capablanca (the
  three parts near 29, 57 and 17 against 295, 214 and 288), passed pawns
  in standard (-6 against 229), pawn structure in standard and
  capablanca (-1 and -46 against 45), mobility in crazyhouse (13
  against 96).
- FSF's values are its own self-play under its own check rule; under
  ours its king was worth 161 in standard, its danger sum 41 and its
  shelter 39 (plan 29, queue160), near ours for shelter. Queue257 plays
  FSF's lab arms against full FSF at the test-set budgets, as queue238
  did for the search, so both maps stand on the same budget: base two
  rounds, `e_king`, `e_kdanger`, `e_kshelter`, `e_passed`, `e_pawns`,
  `e_mobility`, `e_threats`, `e_imbalance` one round each.

### Queue257, FSF's eval terms at our budget

The lab arm against full FSF at the test-set budgets, one round of 200,
minus the lab base (two rounds, near 0). Ours from queue256 in brackets:

| Term off | standard | xiangqi | crazyhouse | capablanca |
| -------- | -------- | ------- | ---------- | ---------- |
| `e_king` | 122 (29) | 127 (110) | 115 (56) | 129 (18) |
| `e_kdanger` | 100 (3) | 53 (105) | 92 (5) | 100 (13) |
| `e_kshelter` | 37 (40) | -6 (-6) | 4 (21) | 9 (17) |
| `e_passed` | 112 (-6) | -10 (42) | -34 (-4) | 106 (-15) |
| `e_pawns` | 64 | -29 | -15 | 67 |
| `e_threats` | 129 (78) | 96 (42) | 72 (5) | 62 (79) |
| `e_mobility` | 6 (37) | 64 (30) | 38 (13) | 19 (79) |
| `e_imbalance` | 48 (18) | 13 (-7) | 25 (5) | -17 (15) |

- Our king is the sum of the three king arms (they need not add); our
  pawns arm holds the passed pawns too, so it has no FSF match alone.
- The danger sum is the largest shortfall in three variants: near 100
  in standard and capablanca, 87 in crazyhouse; in xiangqi ours is worth
  more than FSF's. Passed pawns follow (near 120 in standard and
  capablanca). Shelter is level. The danger sum gets the why chain.

### Why chain: the danger sum

- A: our danger sum is worth near nothing in standard, crazyhouse and
  capablanca, FSF's near 100; both read the same kind of inputs.
- Static check: positions from queue258 (main against full FSF, one
  round per variant, the games kept), move 9 on, one ply in 12 (standard
  and capablanca one in 24), not in check. Ours is main minus
  `plan33-off-kdanger`, FSF's the lab classical score minus `e_kdanger`,
  in pawns from White's view; level is FSF's score within 1.5:

| Variant | Positions | sd ours / FSF | Corr | Fires, ours / FSF | Level: sd ours / FSF, corr |
| ------- | --------- | ------------- | ---- | ----------------- | -------------------------- |
| standard | 1011 | 1.08 / 1.50 | +0.61 | 69% / 48% | 0.61 / 0.46, +0.63 |
| xiangqi | 1475 | 0.68 / 2.49 | +0.79 | 83% / 78% | 0.33 / 0.91, +0.61 |
| crazyhouse | 721 | 2.14 / 8.22 | +0.88 | 93% / 87% | 0.49 / 2.16, +0.49 |
| capablanca | 726 | 1.64 / 2.76 | +0.69 | 82% / 79% | 1.08 / 1.44, +0.43 |

- Neither size nor fit sorts the variants: xiangqi, where ours works,
  has the smallest sum against FSF's; standard, where it does not, the
  closest size.
- Seeded bench, off-kdanger over main (nodes, time): standard x0.850 ±
  0.075, x0.799 ± 0.070; xiangqi x1.215, x1.255; crazyhouse x1.079,
  x1.100; capablanca x1.027, x1.027. FSF's chess bench at depth 13,
  `e_kdanger` off over the lab base: nodes x1.065, time x1.087.
- B: in standard our danger sum grows our tree x1.18 and our time x1.25,
  near 35 at our 108 a doubling; FSF's shrinks its own tree. Elsewhere
  ours keeps or shrinks the tree.
- C, noise (no): the change of the term from ply to ply over 12 games,
  over its spread, is ours 0.23 and FSF's 0.25 in standard, ours 0.19
  and FSF's 0.26 in xiangqi. Ours is not jumpier than FSF's.
- FSF's sum (`king()`, evaluate.cpp) against ours: its weak square is
  held at most once and only by the king or queen, ours held at most
  once by anything; its ring moves off the edge (nine squares for a
  king on its first rank, ours six) and drops squares two own pawns hold;
  its attackers weigh by type; its maps x-ray through queens and its own
  rooks; it adds pins and flank attacks and subtracts its shelter, a
  knight defender, flank defence and the mobility difference. The
  per-type weights and the knight defender name pieces and are not
  ported; the rest has a form from the rules.
- C2 (open): standard is the one variant where half of our depth-1
  nodes are in check (51%, the others 10% to 12%), as every check is
  extended. A term that rewards an attack on the royal steers the search
  into check lines, and each extended check grows the tree. Check: the
  seeded bench of xf (no extension at the last ply) with and without
  the danger sum (`plan33-xfkd`).
- Standard, xfkd over xf: nodes x0.968 ± 0.068, time x0.966 ± 0.056
  (on main x0.850 and x0.799). Without the last-ply extension the
  danger sum no longer grows the tree: C2 holds in the tree.
- Queue259 climbs back: main, xf and xfkd, interleaved, three rounds of
  200 per variant against full FSF. If C2 holds in play, the danger sum
  is worth more on xf (xf minus xfkd) than on main (3 in standard).
- Queue259 runs on the local machine (the server is down), concurrency
  8, local FSF 14.0.1. Ours plays on the clock and FSF on nodes, so the
  absolute level moves with the machine: main scores +90 here against
  -69 on the server. Only differences within the queue count.
- Standard (mean of three rounds, σ about 13 per arm, 18 per
  difference): main +90.0, xf +71.0, xfkd +23.9. xf minus xfkd is +47,
  2.6σ, where main minus off-kdanger was +3 on the server. xf itself is
  -19 against main (1.0σ).
- All four (mean of three rounds; σ 13 to 16 per arm, 18 to 22 per
  difference; no forfeits):

| Variant | main | xf | xfkd | xf - main | xf - xfkd | server main - off |
| ------- | ---- | -- | ---- | --------- | --------- | ----------------- |
| standard | +90.0 | +71.0 | +23.9 | -19.0 | +47.1 | +3 |
| xiangqi | +119.9 | +132.3 | +11.0 | +12.4 | +121.3 | +105 |
| crazyhouse | +107.9 | +132.7 | +18.6 | +24.8 | +114.1 | +5 |
| capablanca | +139.4 | +170.2 | +86.7 | +30.8 | +83.5 | +13 |

- xf over main is +12.2 on the mean of the four (σ 10.2, 1.2σ): short
  of the merge rule, and standard is the one variant that falls.
- Queue259x plays main and off-kdanger on this machine, interleaved,
  three rounds per variant, for the like-for-like danger worth on main.
- Queue259x (rounds d to f; no forfeits): main minus off-kdanger on this
  machine, against xf minus xfkd (queue259):

| Variant | main (d-f) | off-kdanger | main - off | xf - xfkd | server main - off |
| ------- | ---------- | ----------- | ---------- | --------- | ----------------- |
| standard | +87.3 | +33.7 | +53.6 | +47.1 | +3 |
| xiangqi | +125.0 | +40.4 | +84.6 | +121.3 | +105 |
| crazyhouse | +106.3 | +54.3 | +52.0 | +114.1 | +5 |
| capablanca | +148.6 | +102.8 | +45.8 | +83.5 | +13 |

- C2 is rejected in play for standard: the danger sum is worth the same
  on xf as on main (-7, σ 26). Elsewhere it gains on xf (mean of the
  four +32, σ 14), but xf alone does not merge.
- The like-for-like worth on main here is 46 to 85 in every variant,
  where the server gave 3 to 13 in three of them. The difference is the
  level: main scores -69 to -25 on the server and +87 to +149 here. A
  is not "the danger sum is worth nothing" but "worth nothing at the
  server's level". Check: main and off-kdanger in standard here, with
  FSF's `nodestime` raised so main scores near the server's level
  (queue260).
- Queue260 (standard, `nodestime` 46, six rounds, no forfeits): main
  -71.1, off-kdanger -88.9, so the danger sum is worth +17.8 ± 13.1 at
  the server's level, against +53.6 ± 18.5 at +88 (1.6σ apart). Our
  clock is the same in both, so the sum is worth less against a stronger
  defender, not at a shallower search of ours. At this level FSF's sum is
  worth near 100 (queue257).
- Local budgets from here on: the `nodestime` that puts main at the
  server's level, from each variant's FSF doubling worth: standard 46,
  xiangqi 53, crazyhouse 61, capablanca 39.

### Why chain: the danger sum, defence

- D: our sum adds only attack inputs. FSF's also subtracts defence: the
  mobility difference, its shelter, flank defence and a knight
  defender. An attack-only sum rewards an attack a strong defender
  holds, which fits a worth that falls as the defender gets stronger.
- Static check (local FSF build that prints the parts of `kingDanger`,
  and a probe of ours, on 4632 royal sides of 200 games at queue260's
  level): ours against FSF's total 0.86, against its attack part 0.90,
  against its defence part 0.06. Sd: ours 382, FSF total 836, attack
  786, defence 159. FSF's total regressed on ours leaves a residual of
  sd 426 that the defence part follows at 0.68.

| FSF defence part | sd | Corr with FSF total | Corr with ours |
| ---------------- | -- | ------------------- | -------------- |
| mobility difference | 82 | 0.36 | 0.16 |
| shelter (6/8) | 93 | 0.26 | 0.00 |
| flank defence | 16 | 0.08 | -0.19 |
| knight defender | 50 | 0.18 | -0.01 |

- The knight defender names a piece and is not ported. The mobility
  difference is the largest and is in our maps already: our opening
  mobility of the enemy minus ours follows FSF's term at 0.88. Added at
  FSF's weight as a ratio (a lead worth the most valuable piece is 154.1%
  of a full weak ring), the fit to FSF's total goes from 0.861 to 0.877.
- `plan33-kdmob` (a49bb36): the mobility lead joins the sum, off where
  captures fill a hand (FSF's rule). Queue261 plays main and kdmob,
  interleaved, three rounds per variant at the local budgets.
- Earlier tries of the same input:

| Try | Result | Recorded cause | Recorded precondition | Difference now |
| --- | ------ | -------------- | --------------------- | -------------- |
| `e_kmobsum`, FSF's input off in FSF (plan 29, queue162) | standard -10.4 ± 23, xiangqi +5.2, grand -10.4 | none (the shelter coupling guess was wrong) | FSF's other defence inputs stay | ours has no defence input at all; the static fit puts the residual on defence at 0.68 |

### Queue261, kdmob (partial, old machine)

- Played on the old local machine at its budgets, interrupted. Mean of
  the rounds played (σ near 18 per difference):

| Variant | main | kdmob | kdmob - main |
| ------- | ---- | ----- | ------------ |
| standard | -82.0 | -69.3 | +12.7 |
| xiangqi | +10.5 | -24.6 | -35.1 (-1.75σ) |
| crazyhouse | | | not played |
| capablanca | | | not played |

- The one crazyhouse run was invalid: the machine hibernated in it.
- Partial, and not pooled with later runs. The work moved to a new
  machine (Windows, concurrency 6); queue261 plays again there
  in all four variants after the budgets are calibrated.

### The Windows machine

- An i5-10400F, 6 cores and 12 threads; concurrency 6 is one game a
  core. Build: nightly `x86_64-pc-windows-gnu` (no MSVC tools on the
  machine). Perft and the 4-game smoke in each variant pass; no
  forfeits.
- FSF is the official 14.0.1 XQ largeboard BMI2 release. It embeds the
  xiangqi and janggi networks and no chess network, so by default it
  plays xiangqi with NNUE and chess classical. The first calibration
  round gave xiangqi -541 at `nodestime` 53, where the Mac gave +10.
- Full FSF on the server was classical in all four variants: the lab
  base, which runs the classical eval, scored near 0 against it
  (queues 238 and 257). Every run here sets `Use NNUE=false` on FSF.
  Chess was classical before and after, so the standard rounds stand.
- Under load our search slows more than FSF's: six searches at once run
  at 0.48 of one search alone (xiangqi, 217k to 104k nodes a second),
  FSF at 0.62 (269k to 168k). Alone, ours is 0.59 of FSF's speed in
  standard and 0.80 in xiangqi.
- Calibration, main against classical full FSF, one round of 200 per
  point (no forfeits):

| Variant | Points (`nodestime`: Elo) | Target | Budget |
| ------- | ------------------------- | ------ | ------ |
| standard | 46: -263.4, 25: -117.2, 21: -52.5 | -69 | 22 |
| xiangqi | 26: -12.2 | -30 | 27 |
| crazyhouse | 13: +75.9, 26: -109.5 | -28 | 19 |
| capablanca | 21: -77.7 | -25 | 18 |

- Standard is the fit of the three points (-181 a doubling), crazyhouse
  the line through its two (-186 a doubling, not 101). Xiangqi and
  capablanca scale by FSF's doubling worth near our 50% point (241,
  292).

### Why chain: passed pawns

- A: our passed pawn term is worth near nothing (queue256: standard -6,
  capablanca -15), FSF's 112 and 106 at the same budget (queue257).
- Static check (main minus `plan33-off-passed` against FSF's classical
  `Passed` line, blended by FSF's phase; 200 standard games at the local
  budget, ply 16 on, one ply in 12; positions where FSF's term is zero
  kept one in five), in pawns:

| Positions | n | sd ours | sd FSF | Corr | Ours fires |
| --------- | - | ------- | ------ | ---- | ---------- |
| all | 1070 | 3.43 | 1.38 | +0.56 | 82% |
| FSF phase 64 and up | 326 | 0.45 | 0.52 | +0.68 | 69% |
| FSF phase under 64 | 744 | 4.11 | 1.62 | +0.57 | 88% |

| Size of ours | n | Mean ours | Mean FSF | FSF over ours |
| ------------ | - | --------- | -------- | ------------- |
| 0.5 to 1 | 163 | 0.71 | 0.60 | 0.65 |
| 1 to 2 | 183 | 1.41 | 0.74 | 0.37 |
| 2 to 4 | 157 | 2.91 | 0.94 | 0.25 |
| 4 to 8 | 114 | 5.65 | 1.39 | 0.20 |
| 8 and up | 49 | 11.35 | 2.49 | 0.22 |

- B: in the middlegame ours has FSF's size; toward the endgame it is 2.5
  times FSF's, and 3 to 5 times where it is large. The largest gaps are
  endings with two or three far passers (ours -20 pawns, FSF -4), and a
  few have the other sign (ours +11.7, FSF -1.4).
- C: ours prices every passer as unstoppable. Its endgame value one step
  from promotion is 35% of the promotion gain times the progress (27%),
  and up to twice that when connected and chained. FSF's base there is
  10.6%; it reaches near our value only when the path is free (its top
  tier, 36 w with w = 17, adds 24.7%) and the enemy king is far. Our
  term has no test of the path and no king distance.
- D (the candidate): FSF's form. A small base by steps to promotion, and
  for a passer at most four steps away, a free path part by tiers from
  the attack maps and a king distance part to the stop square, all as
  parts of the promotion gain.
- Earlier tries of the passed pawn term:

| Try | Result | Recorded cause | Recorded precondition | Difference now |
| --- | ------ | -------------- | --------------------- | -------------- |
| E1 (plan 29, double count out) | grand +165 | the bonus was counted twice | | |
| M3 (value times the safe share of the walk) | standard 0, xiangqi -16, grand -32, shogi +2 | middlegame paths are all attacked, so passers fell too low | | additive tiers on a small base; never a cut of the full value |
| M3b, M3c (unsafe: held, or attacked and not defended) | -26, +19, -29, -14 (two rounds, stopped) | the discount does not pay at this budget | parked until our passer value is measured against FSF's on positions with a passer | measured above: ours is FSF's free path value everywhere |
| PR (progress part on connected passers) | horde +31, standard -56, xiangqi -30, crazyhouse -45 | progress counted twice; our passed value already too high | | the candidate lowers the base |
| King distance to the stop square | never | | | new |

- `plan33-pfsf`, the port, from the rules:
  - Steps to promotion `s` from the promotion field (rounded). The base
    is FSF's rank table over its gain (queen minus pawn), by steps: one
    step 11.5% and 10.6% (opening, endgame), two 7.0% and 7.0%, three
    2.7% and 2.9%, four 0.7% and 1.6%, five 0.7% and 1.3%, six 0.3% and
    1.1%. It takes the place of 10% and 35% times the squared progress.
    The connected and chained factors stay on the base.
  - At most four steps, the dynamic part, in units of the gain times the
    step shape `w(s) / w(1)` (FSF's `5 r - 13`: 1, 0.71, 0.41, 0.12):
    free path tiers 25.0%, 20.8%, 11.8%, 4.9%, 0 (FSF's 36, 30, 17, 7,
    0 times 17 over its gain), and 3.5% more when we hold a stop square;
    endgame only, 3.3% for each step of the enemy royal from the stop
    square and -1.4% for each step of ours, each distance at most five.
  - The path is the pawn's path mask, the span its interference mask,
    the stop squares the path squares in the pawn's ring. A square is
    unsafe when the enemy attacks or holds it. Tiers: no unsafe square in
    the span; all unsafe squares held by our pawns; none on the path;
    none on a stop square; else 0. The path part needs an empty stop
    square. The dynamic part is in `attack_terms!`, where the maps are;
    the passer test there is the interference mask against the enemy
    pawn map.
  - Left out: FSF's x-ray of a rook or queen behind the passer (named
    pieces), its file penalty, the second push in the king part, and its
    removal of blocked candidates. A pawn that cannot promote keeps its
    present value and has no dynamic part.

### Queue261 on the Windows machine (cut after two rounds)

Main and kdmob interleaved against classical full FSF at the budgets
above, two rounds of 200 (no forfeits). kdmob minus main:

| Variant | main | kdmob | Difference |
| ------- | ---- | ----- | ---------- |
| standard | -80.5 | -83.2 | -2.8 ± 24.1 |
| xiangqi | -38.4 | -44.7 | -6.3 ± 25.2 |
| crazyhouse | -20.1 | -45.6 | -25.5 ± 24.3 |
| capablanca | -12.2 | +25.4 | +37.5 ± 23.8 |
| mean | | | +0.7 ± 12.2 |

- Crazyhouse plays the same search in both (the input is off where
  captures fill a hand; seeded search to depth 9: 17773 nodes in both),
  so its -25.5 is the noise of the session.
- Cut after round b (the user's rule: a run that cannot reach the merge
  rule stops): round c would need near +55 on the mean. Not merged.
- Why: the input is small in FSF too. Its static fit moved FSF's total
  only from 0.861 to 0.877, and FSF's own sum without it lost 10 ± 23 in
  standard (queue162). The result is the size the ledger said. Closed.
- Next lead by size: passed pawns (FSF 112 and 106 at our budget, ours
  near 0) before the shelter input (shelter is level with FSF's in
  standard, 40 against 37).

### FSF at UCI_Elo 2100, the baseline from here

- The user set FSF at `UCI_Elo` 2100 as the test opponent: classical
  (`Use NNUE=false`), `UCI_LimitStrength`, its own clock at 10+0.1, no
  `nodestime`. The merge rule is the same: interleaved with main, three
  rounds of 200 per variant, +2σ on the mean, no variant below -2σ.
- A run may stop early once the merge rule is out of reach.
- FSF's level at a fixed `UCI_Elo` comes from its skill level (the move
  is picked at depth 7 or 8 among four, with noise; plan 29), so it
  depends little on the machine.
- Queue262: main and pfsf against FSF 2100, three rounds per variant.

### Before queue262, pfsf against FSF's term

- The static check again, pfsf minus `plan33-off-passed` (the same
  positions): correlation with FSF's passed term 0.85 (main 0.56), in
  the middlegame 0.88 (0.68), toward the endgame 0.85 (0.57). Sd 2.10
  against FSF's 1.38 (main 3.43). FSF's term is near 0.55 of ours in each
  size band: the shape now follows FSF's, the size is still near 1.8
  times. Two parts known to add size: the connected and chained factors
  on the base (FSF has none in its passed term), and our endgame gain
  (13.5 pawns, FSF's 11.9).

### Queue262, pfsf against FSF 2100; merged build 36

Three rounds of 200, interleaved (no forfeits). pfsf minus main:

| Variant | main | pfsf | Difference |
| ------- | ---- | ---- | ---------- |
| standard | -9.3 | +83.5 | +92.8 ± 19.7 (+99, +89, +90) |
| xiangqi | +19.1 (one round) | | 0, the same search |
| crazyhouse | -139.7 | -165.6 | -26.0 ± 21.4 (-8, -70, 0) |
| capablanca | -161.2 | -102.5 | +58.7 ± 20.8 |
| mean | | | +31.4 ± 9.0 |

- A xiangqi soldier cannot promote, so pfsf plays main's search there
  (seeded search to depth 9: 32355 nodes in both); its rounds were left
  out after round a and the difference is 0.
- +3.5σ on the mean, no variant below -2σ: merged as build 36 (af5c07b,
  one commit). The played binary and the committed code search the same
  trees (seeded, depth 10: standard, crazyhouse, capablanca, horde).
- The why chain's climb back: D holds (FSF's form, with path and royal
  terms, follows FSF's term at 0.85 and gains 93 in standard and 59 in
  capablanca, near FSF's own 112 and 106 at our budget).
- Crazyhouse leans down (-1.2σ). A cause to test: a held piece drops on
  any empty square, so a path that is free on the board is not free with
  an enemy piece in hand; FSF's passed term loses 34 in crazyhouse too
  (queue257). `plan33-pfsfd` (build 36 plus that rule: an empty square is
  unsafe while the enemy has a piece in hand) changes only variants
  with drops. Queue263: pfsfd against build 36 in crazyhouse, six rounds.
- After it: the size (1.8 times FSF's), from the connected and chained
  factors on the base.

### Queue263, pfsfd in crazyhouse (cut after three rounds)

- pfsfd minus build 36, crazyhouse: +6, +26, -66; -11.4 ± 21.8. Six
  rounds need near +31 to pass, so the last three would need near +73:
  cut. Closed, the branch deleted.
- The drop rule does not move crazyhouse, so blocking drops are not why
  pfsf leaned down there; queue262's -26 (-1.2σ) stays within noise.
- The size note above is withdrawn. FSF also pays a connected advanced
  passer about twice: its `Connected[r]` bonus (pawns.cpp) for a
  supported phalanx pawn one step from promotion is near 283 units,
  11% of its gain, close to its passed base there. It sits in FSF's
  Pawns row, which the static check did not read, so our connected and
  chained factors stand in for it. No size candidate.

### Why chain: pawn structure

- A: FSF's pawn structure (`e_pawns`) is worth 64 in standard and 67 in
  capablanca at our budget (queue257); our pawn arm, structure and passed
  pawns, -1 and -46 (queue256).
- Static check, build 36 minus `plan33-off-structure` (a diagnostic with
  the structure terms off and the passed pawns on, never merged) against
  FSF's classical Pawns row, blended by FSF's phase; 100 games each of
  build 36 against FSF 2100 (standard +59.6 ± 66, capablanca -155.5 ±
  76), ply 16 on, one ply in eight:

| Variant | n | sd ours | sd FSF | Corr | FSF on ours |
| ------- | - | ------- | ------ | ---- | ----------- |
| standard | 1137 | 0.96 | 0.40 | +0.75 | 0.32 |
| capablanca | 1030 | 1.01 | 0.38 | +0.75 | 0.28 |

- B: our structure term is 2.5 times FSF's in spread, and FSF's is near
  0.3 of ours on the fit; the shape is near (0.75).
- C: our parts are flat shares of the pawn value, from iteration 3's
  hand divisors: isolated 25% and doubled 25% in both phases, backward
  17.5%, connected 20% and 35% at any rank. FSF's (over its pawn, 126
  and 208): isolated 2.4% and 7.2%, 10.3% and 11.5% more when unopposed;
  backward 7.1% and 10.6%, the same more when unopposed; doubled 10.3%
  and 24.5%, only without a supporter; connected grows with the rank,
  `Connected[r] (2 + phalanx - opposed) + 22 supporters`, from near 25%
  on the second rank to near 90% one step from promotion (opening), and
  the endgame part is `(r - 2) / 4` of it.
- Earlier tries of these values:

| Try | Result | Recorded cause | Recorded precondition | Difference now |
| --- | ------ | -------------- | --------------------- | -------------- |
| Iteration 3 values, restored as R4 (plan 21) | never played alone | | | |
| Q-3 (plan 13), the pawn table collapse | a simplification | | | |
| PR, PR2 (plan 29, the mass progress) | standard -56 (PR) | progress counted twice | | the mass term stays as it is |
| off-pawns (queue256) | standard -1, capablanca -46 | | | static size measured: 2.5 times FSF's |
| FSF's form | never | | | new |

- D (the candidate, `plan33-pstr`): FSF's form, as parts of our pawn
  value in each phase. Steps to promotion `s` give FSF's rank (`r = 7 -
  s`); a pawn that cannot promote counts its steps to the far rank. A
  supporter is a support square pawn behind the pawn's rank, a phalanx
  one on its rank; opposed is an enemy pawn on its path. The passed
  pawn base loses its connected and chained factors, as the connected
  term now pays a connected passer, as in FSF. Left out: FSF's early
  doubled, weak lever, blocked pawn and doubled isolated parts, the
  second rank factor with drops, and the edge file exception of the
  unopposed backward pawn.
- The first build gave horde +1053 at the start (build 36 -527): thirty
  six connected pawns each took the unit. FSF scores a pawn mass (more
  pawns than a quarter of the board, our `pawn_mass` too) with
  `HordeConnected[opposed][r]` for each supporter and phalanx pawn, and
  pstr takes that form for a mass, as parts of the pawn value over one to
  seven steps. Horde then starts at -335. The four test variants start
  at build 36's values.
- Queue264: build 36 and pstr against FSF 2100, three rounds per
  variant.

### Queue264, pstr (stopped after round a)

| Variant | build 36 | pstr | Difference |
| ------- | -------- | ---- | ---------- |
| standard | +31.4 | +74.1 | +42.7 |
| xiangqi | +63.2 | -22.6 | -85.8 |
| crazyhouse | -105.6 | -214.8 | -109.2 |
| capablanca | -121.1 | -107.5 | +13.6 |

- σ near 33 for one round. Xiangqi and crazyhouse fall near 2.5σ and
  3.3σ, so the two rounds left could not bring pstr to the merge rule.
- Xiangqi: a soldier cannot promote, and FSF scores no structure for it,
  so pstr's form had no source there. Our pawn arm was worth +52 in
  xiangqi (queue256).
- Crazyhouse: FSF triples `Connected` on the second rank when captures
  fill a hand (`pawns.cpp`, the shield against drops); pstr left that
  out and cut the unit of an unmoved connected pawn to near a tenth of
  a pawn.
- `plan33-pstr2` (pstr and both causes): a pawn that cannot promote
  keeps build 36's structure (flat connected, the passed factors, the
  old faults, `PAWN_FLAT_RATIO`), and where captures fill a hand the
  connected unit counts three times on the second rank
  (`PAWN_SHIELD_FACTOR`). Bench nodes (seed 1, depth 8): pstr2 equals
  pstr in standard and capablanca (85720, 281832) and build 36 in
  xiangqi (243647). So queue264's standard and capablanca round a counts
  for pstr2, and xiangqi plays no more (0, the same search).
- Queue265: build 36 and pstr2, standard and capablanca rounds b and c,
  crazyhouse three rounds.

### Queue265, pstr2 (cut after round b in standard)

pstr2 minus build 36 (round a in standard and capablanca from
queue264): standard +43, -35 (+4.0 ± 25.2); crazyhouse -17.4 ± 38.3;
capablanca +13.6 ± 36.6; xiangqi 0 (the same search). The rounds left
needed near +39 each on the mean: cut. Not merged.

- D rejected in play. Static check of pstr2 (pstr2 minus a diagnostic
  with its structure off, never merged) against FSF's Pawns row, 4708
  standard positions of queue265: correlation 0.93 (build 36 0.75), sd
  0.56 against 0.41 (0.96). Most of the size left is the unit: FSF's
  trace counts its opening values in endgame pawns (208), pstr2's
  ratios take its opening pawn (126). The port follows FSF's term, and
  plays as build 36.
- So the worth that queue257 gave `e_pawns` (64 in standard, 67 in
  capablanca) is not in the form of FSF's structure term as our engine
  plays it. The cause is not known.

### To pick the pawn structure up again

- Known: build 36's structure term is 2.5 times FSF's in spread, at a
  correlation of 0.75; FSF's form (pstr2) follows FSF's at 0.93 and
  gains nothing (standard +4 ± 25).
- Not known: what the lab's `e_pawns` switch turns off. FSF's pawn entry
  also carries the shelter and storm (`evaluate_shelter`) and the passed
  pawn set that `passed()` reads; if the switch clears the entry, its 64
  holds more than the structure. Test: read the switch in the lab copy
  (on the old machine) or rebuild the lab here, and measure FSF without
  its pawn score alone against full FSF at our budget.
- Kept: `plan33-pstr2` (2308a89). Deleted: pstr, both diagnostics.

### Royal safety against FSF's King safety row

- Static check: build 36 minus `plan33-off-king` (a diagnostic with all
  royal safety terms off: the danger cost, zone and proximity, shelter,
  guard, castling, open shield; never merged) against FSF's classical
  King safety row, on queue265's build 36 games, one ply in 12:

| Variant | n | sd ours | sd FSF | Corr | FSF on ours |
| ------- | - | ------- | ------ | ---- | ----------- |
| crazyhouse | 508 | 3.64 | 10.16 | +0.86 | 2.39 |
| capablanca | 1295 | 2.47 | 3.42 | +0.75 | 1.04 |
| standard | 1415 | 1.25 | 1.52 | +0.61 | 0.74 |

- Crazyhouse is the outlier: the shape follows FSF's, the size is 2.4
  times smaller. Where captures fill a hand FSF's sum also counts the
  hand: held pieces that can drop on the ring are attackers, and their
  drops next to the king are hits. Its flank term is six times larger
  there, but its own lab put the flank at -0.9 in crazyhouse (queue224).
- Earlier tries:

| Try | Result | Recorded cause | Recorded precondition | Difference now |
| --- | ------ | -------------- | --------------------- | -------------- |
| E2 (plan 27, a hand type's best drop pressure once) | shogi -6.4 ± 12.4 | | | the old static danger table, gone since KD |
| KH (plan 29, hand attackers in the danger sum, queue226) | crazyhouse -24.4 against -18.0 | our danger moved little at the tips (attackers mean 0.6) | | KH played before LZ: the lazy margin (56 cp in crazyhouse) skipped the danger sum in quiescence, so KH's inputs were mostly never read; LZ merged after it (+18.5) |
| FSF's `e_khand` (lab, queue225) | -58.5 for FSF without it | | | |
| FSF's flank (lab, queue224) | -0.9 | not the gap | | no candidate |

- `plan33-kh`: KH (edd5f93) on build 36. Only variants with drops
  change; it plays crazyhouse, six rounds against build 36.

### Queue266, KH in crazyhouse (cut after three rounds)

- KH minus build 36: +46, -2, -40; +1.3 ± 21.2. Six rounds need near
  +31: cut. Not merged; the branch deleted (KH stays as `plan29-kh`).
- Static check of KH's royal safety (KH minus off-king, the same
  positions): correlation 0.90 (build 36 0.86), sd 4.85 against FSF's
  10.16 (3.64), FSF on ours 1.88 (2.39). The hand closes part of the
  gap in size and fit, and plays the same.
- Standard, the gaps (build 36): where FSF's king term passes two pawns
  ours points the same way in 89% to 96% of positions, near half the
  size; where FSF's is under half a pawn (62% of the positions) ours
  has the same sign in only 58%.
- Three ports moved a term toward FSF's in the static check and did not
  play better: kdmob, pstr2, KH. The one that gained, pfsf, changed a
  term whose fit was poor (0.56 to 0.85) and gave it inputs it lacked.
  The next leads are ranked by fit: threats and mobility were not yet
  checked.

### Threats and mobility against FSF's rows

Build 36 minus a build with the term set to zero (`plan33-off-threats36`,
`plan33-off-mobility36`), on queue265's build 36 games, against FSF's
classical Threats and Mobility rows blended by FSF's phase.

| Variant    | Term     |    n | sd ours | sd FSF | corr | FSF on ours |
|------------|----------|-----:|--------:|-------:|-----:|------------:|
| standard   | threats  | 1182 |    0.31 |   0.47 | 0.59 |        0.89 |
| standard   | mobility | 1398 |    0.33 |   0.48 | 0.72 |        1.04 |
| crazyhouse | threats  |  508 |    0.34 |   0.89 | 0.79 |        2.10 |
| crazyhouse | mobility |  508 |    0.40 |   0.80 | 0.70 |        1.39 |
| capablanca | threats  | 1280 |    0.43 |   0.61 | 0.71 |        0.99 |
| capablanca | mobility | 1296 |    0.58 |   0.69 | 0.89 |        1.05 |

- The poorest fit with the largest FSF worth is threats in standard
  (corr 0.59; FSF's threats row was worth 129 there in queue257). Next
  is the standard royal safety (0.61), then mobility in crazyhouse
  (0.70) and threats in capablanca (0.71).
- Mobility in capablanca (0.89) is near pfsf's fit after the port; it
  is not a lead.
- Crazyhouse threats fit well (0.79) at half FSF's size; size alone did
  not gain three times above, so it is not a lead.
- The probes ran one after another on one core; later probes run side
  by side.

### Why chain: threats in standard

- Threats in standard fit poorly (0.59) because FSF's row holds parts we
  do not have. A probe build of FSF (`fsf-tparts.exe`, its threats with
  a trace row for each part) on the same positions: the three parts we
  port (hanging, safe pawn, restricted) correlate 0.93 to 0.98 with ours;
  the parts we lack sum to an sd of 0.35 in standard (ours 0.29) and
  correlate 0.80 with FSF's row and 0.04 with ours.
- Ours plus a missing part, against FSF's row:

| Added to ours | standard | crazyhouse | capablanca |
|---------------|---------:|-----------:|-----------:|
| none | 0.59 | 0.79 | 0.71 |
| by piece (minor, rook) | 0.81 | 0.88 | 0.83 |
| by piece and king | 0.84 | 0.89 | 0.85 |
| queen (knight, slider on queen) | 0.73 | 0.84 | 0.84 |
| by piece and queen | 0.89 | 0.88 | 0.93 |
| all missing parts | 0.97 | 0.93 | 0.99 |

Past tries of these parts:

| Try | Result | Recorded cause | Precondition | What differs now |
|-----|--------|----------------|--------------|------------------|
| queue165 (lab, `e_tpiece`, `e_tqueen` off) | standard -25, -24; xiangqi, grand, shogi near 0 | FSF's piece names fit only standard | FSF self-play | never ported; the port reads values, not names |
| TH (plan 29, build 22) | +0.1 | eval +20 eaten by the map pass speed | a separate map pass | the pass exists; the new part reads it |
| `e_tpiece` in TH's static check | corr 0.68 with the part off | not a cause, a check | | |

- Candidate tpiece (`plan33-tpiece`, fd263c3): four kinds on the attack
  maps, by the cheapest attacker that is not a pawn or a royal: lesser
  (an enemy piece with a higher opening value than the attacker), even
  (a weak enemy piece, no lesser attacker), loose pawn (a weak enemy
  pawn), royal (a weak enemy piece the royal attacks, once a side). The
  values are FSF's tier means over its queen: 3.0% and 3.5%, 1.7% and
  2.0%, 0.2% and 1.4%, 0.9% and 3.3%.
- Static check (tpiece minus `offthreats`, FSF's Threats row): standard
  0.59 to 0.74, crazyhouse 0.79 to 0.86, capablanca 0.71 to 0.77. The new
  part against FSF's minor, rook and king parts: 0.78 standard, 0.71
  capablanca. The largest gaps: FSF's lazy eval (its row is 0 in
  lopsided positions), and our queen as an attacker where FSF counts
  only light pieces and rooks.
- Speed: the first form (a loop over the attackers of each enemy piece)
  cost 7% in capablanca. The pass now marks lesser hits as each piece is
  mapped; the cost is near 3% (capablanca 136.0k against 131.4k nodes a
  second, standard 759k against 744k), the same evaluations on 450
  positions.
- Smoke: 0 of 8 in capablanca (main 1 of 4); the loss in game 0 is a
  pawn grab by the chancellor that main plays too at depth 12.
- Queue267: main and tpiece, three rounds of 200 per variant.
- Next on the same chain, tqueen (`plan33-tqueen`, on tpiece): FSF's
  knight and slider attacks next to its queen, read as the squares from
  which a cheaper piece of ours, not a pawn, would hit the enemy's only
  copy of its most valuable piece, in our area, not strong, attacked
  twice. FSF asks for the double attack of sliders only; with no slider
  mark in the piece data, every type needs it. Each square is worth
  FSF's slider value over its queen (2.4% and 0.7%), twice when we have
  no copy of our most valuable piece.

### Why chain: mobility in crazyhouse

- Mobility in crazyhouse fits poorly (0.70) because FSF's row holds a
  part we do not have: drop mobility, (10, 10) for each type in the hand
  and each empty square of the enemy half that the enemy does not
  attack. On the probe positions (FSF probe build, a trace row for it):
  sd 0.48 against 0.80 for FSF's whole row and 0.40 for ours; it
  correlates 0.68 with FSF's row and 0.09 with ours; ours plus it fits
  0.93.
- FSF's trace prints its mobility before the hand doubling
  (`evaluate.cpp` 1495: times 1 plus `captures_to_hand`); ours is
  doubled the same way, so the drop part takes the same factor.
- Past tries: none. No plan names a drop or hand mobility term; the hand
  tries (HB, E2, KH, KH2, KH3, HM) are all royal safety.
- Candidate dropmob (`plan33-dropmob`, ca6221f, from main): each type in
  the hand, not royal, adds the empty squares of the enemy half where it
  can drop (a drop template on the square, past the middle rank in its
  forward direction) that the enemy does not attack. Each is worth FSF's
  drop value over its queen (0.4% and 0.4%), doubled with the mobility.
  A knight in hand at the start position: +32, the same with either side
  to move.
- The builds of tqueen and dropmob and the probes ran at idle priority
  beside queue267, on the two cores the games leave. A `derive` run at
  normal priority took all cores for near two minutes during round a of
  tpiece in xiangqi.
- Static check of dropmob (dropmob minus `offmobility`, FSF's Mobility
  row): crazyhouse 0.70 to 0.92, sd 0.59 against FSF's 0.80.

### tqueen, three forms

| Form | standard | crazyhouse | capablanca |
|------|---------:|-----------:|-----------:|
| main | 0.59 | 0.79 | 0.71 |
| tpiece | 0.74 | 0.86 | 0.77 |
| tqueen, one type, full squares | 0.82 | 0.86 | 0.73 |
| tqueen, all top types | | 0.90 | |
| tqueen, squares weighed by gain (99786b9) | 0.80 | 0.90 | 0.85 |

- One type: the dearest piece was the last type of the top value. In
  crazyhouse that is the promoted copy of the queen, a type that is
  almost never on the board, so the part was near 0 there (fit with
  FSF's queen part 0.06). The target is now every type of the top value.
- Full squares: in capablanca the archbishop and the chancellor are
  cheaper than the queen (967 and 1148 against 1285) and count with all
  their squares; the part reached 4 pawns where FSF's was 0.3 (sd 0.63
  against 0.26). FSF counts only knights, bishops and rooks there. Each
  square now weighs the target's value less the attacker's, over the
  target's value (knight near 0.69, rook 0.48, archbishop 0.25,
  chancellor 0.11).
- The new part against FSF's queen part: standard 0.78, crazyhouse
  0.79, capablanca 0.81.

### Queue267 round a, and tqueen joins it

| Arm | standard | xiangqi | crazyhouse | capablanca |
|-----|---------:|--------:|-----------:|-----------:|
| main | +54.3 | +22.6 | -177.2 | -161.9 |
| tpiece | +77.7 | +13.9 | -179.5 | -125.0 |
| tpiece - main | +23.4 | -8.7 | -2.3 | +36.9 |

- The mean of the four is +12.3 after one round (σ near 25 per round).
- The queue loop was stopped at the end of round a and started again
  with tqueen as a third arm (smoke 1 of 4, normal games). tqueen plays
  its round a first, then rounds b and c run all three arms
  interleaved. tqueen holds tpiece, so the queue gives both.
- dropmob changes only variants with drops; it plays crazyhouse after
  queue267, against main, three rounds.

### Royal safety in standard, FSF's parts

- FSF probe build with trace rows for the parts of `king()`: shelter and
  storm, the danger penalty, pawnless flank, flank attacks; on the 1415
  standard positions of the royal probe.

| FSF part | sd | Corr with FSF's row | Corr with ours |
|----------|---:|--------------------:|---------------:|
| shelter and storm | 0.28 | 0.42 | 0.24 |
| danger penalty | 1.36 | 0.98 | 0.60 |
| pawnless flank | 0.08 | 0.08 | 0.02 |
| flank attacks | 0.11 | 0.57 | 0.35 |

- Ours plus a part against FSF's row: shelter 0.65, pawnless flank 0.61,
  flank 0.64 (ours alone 0.61). The gap is not a missing part: FSF's row
  is its danger penalty, and ours follows it at 0.60. Next: our danger
  sum alone against FSF's danger penalty (`plan33-off-kdanger36`, the
  sum off on build 36), and the rest of ours against FSF's shelter.
- Our danger sum alone (main minus off-kdanger) against FSF's row: 0.62
  opening, 0.57 endgame. Our other royal terms (off-kdanger minus
  off-king): 0.39 and 0.44, small (sd 0.37 against 2.25).
- Per side, raw units (a probe build that prints our sum,
  `plan33-probe-danger`; FSF probe rows for its raw `kingDanger` and its
  inputs; ours times 1.647, FSF's full weak ring): 2718 sides where FSF
  is not lazy. Units 0.88, FSF's penalty on both 0.74. Ours is near 0.63
  of FSF's in every band of FSF's value (198 of 276, 525 of 743, 884 of
  1384, 1591 of 2602).

| Input | Corr | Mean FSF | Mean ours |
|-------|-----:|---------:|----------:|
| weak ring squares | 0.86 | 0.9 | 1.0 |
| attackers | 0.52 | 1.1 | 0.7 |
| ring hits | 0.98 | 1.3 | 1.3 |
| unsafe checks | 0.62 | 0.5 | 0.6 |
| safe checks (units) | 0.85 | 296 | 226 |
| whole sum (units) | 0.88 | 269 | 148 |

- FSF's sum less ours (least squares) follows its safe checks most
  (0.58), then its shelter (0.42), flank defence (0.42), weak ring
  (0.38) and mobility lead (0.37).
- Three forms of FSF's safe checks on the same sides: its fairy weights
  (600, 900) for all types, as ours: mean 192, 0.96 with its real check
  sum; its weights by type: 300, 0.99; fairy weights with the slider
  checks through the defender's queen: 198, 0.95. Our sum with FSF's
  typed checks put in fits FSF's sum at 0.94 (0.88 now); with the
  x-ray checks 0.90. Our checks scaled as one (times 1.0 to 2.0) stay
  at 0.88 to 0.89 and only move the mean.
- Cause: FSF's safe check weights and attacker weights are tables by
  named piece (checks: knight 803, bishop 639, rook 1087, queen 759;
  attackers: 81, 52, 44, 10); ours takes its fairy rows (600, 900 and
  40), as the port rules ask. The spread of the table is the fit gap;
  the mean is only size, which did not gain before (DX, KH, kdmob). No
  rule-derived form of the table was found: it does not follow value,
  reach or the attacker table. The standard royal chain stops here;
  the root is a FSF table by piece name.
- termprobe used the chess phase formula in crazyhouse; FSF's phase
  there is the board's non-pawn material over the board's and the
  hand's. With the fixed phase the crazyhouse fits are the same:
  mobility 0.70, threats 0.79.
- Crazyhouse, the same per side check (1016 sides): units 0.91, FSF's
  mean 1782 against ours 1252. The largest input gap is the ring hits
  (FSF 5.5 a side, ours 1.4): FSF counts each drop square next to the
  royal for each type in the hand. That is KH's input, played in
  queue266 (+1.3); the crazyhouse royal chain stays closed.

### Queue267, tpiece and tqueen (cut after round b)

| Round | Arm | standard | xiangqi | crazyhouse | capablanca |
|-------|-----|---------:|--------:|-----------:|-----------:|
| a | main | +54.3 | +22.6 | -177.2 | -161.9 |
| a | tpiece | +77.7 | +13.9 | -179.5 | -125.0 |
| a | tqueen | +43.7 | +34.9 | -143.1 | -132.9 |
| b | main | +83.2 | +29.6 | -181.7 | -164.1 |
| b | tpiece | +50.7 | +56.1 | -190.8 | -113.3 |
| b | tqueen | +17.4 | +52.5 | -164.1 | -125.0 |

| Arm minus main, two rounds | standard | xiangqi | crazyhouse | capablanca | mean |
|----------------------------|---------:|--------:|-----------:|-----------:|-----:|
| tpiece | -4.6 | +8.9 | -5.7 | +43.9 | +10.6 |
| tqueen | -38.2 | +17.6 | +25.9 | +34.1 | +9.9 |

- σ near 25 for a variant over two rounds, 12.5 for the mean. A merge
  needs near +20 on the mean of three rounds, so round c would have to
  give near +40 against an expected +10 (σ near 17): near 4%. Cut; no
  forfeits. Not merged; the branches stay for the record of the form.
- Capablanca gains in both rounds and both arms (+44, +34, near 1.5σ
  each); the other three are level, and tqueen loses in standard.
- So the threat fit rose from 0.59 to 0.80 in standard and to 0.85 in
  capablanca with no gain on the mean. pfsf is still the one port that
  gained from a fit; the rule "rank leads by fit" does not hold for the
  threats. Not known why; the capablanca gain is the one sign, and a
  capablanca-only cause is not a candidate under the merge rule.
- The speed of the arms could not be told apart at idle priority under
  the games (standard 39.5k, 35.0k, 37.6k nodes a second; capablanca
  24.2k, 27.1k, 26.3k; seeded searches of eight probe positions). The
  bench positions are mostly pawn endings with no queen, where tqueen's
  part cannot fire, so its bench nodes equal tpiece's.

### Queue268, dropmob in crazyhouse

- dropmob changes the evaluation only where a hand exists: its seeded
  bench nodes equal main's in standard, xiangqi and capablanca. It
  plays crazyhouse only, against main, three rounds of 200; the other
  three variants are 0 by construction, so the crazyhouse difference
  stands for the mean of four at the same z.
- Smoke 0 of 4 (main scores near 25% here), no forfeits.

| Round | main | dropmob | dropmob - main |
|-------|-----:|--------:|---------------:|
| a | -137.0 | -147.2 | -10.2 |
| b | -200.2 | -179.5 | +20.7 |
| c | -190.8 | -145.1 | +45.7 |

- Mean +18.7, σ near 22 (0.85σ); no forfeits. Not merged under the rule.
  The static fit rose from 0.70 to 0.92; the play leans the same way but
  is under 1σ.

### Xiangqi against FSF's rows

Main minus the off builds on the 1821 sampled positions of queue267
round a (FSF's phase from its xiangqi values):

| Term | Corr | sd ours | sd FSF |
|------|-----:|--------:|-------:|
| royal safety | 0.80 | 0.70 | 2.91 |
| mobility | 0.41 | 0.25 | 0.71 |
| threats | 0.80 | 0.19 | 0.30 |

- Mobility fits worst. FSF counts, for a fairy piece, its attacks on
  occupied squares and its quiet moves to empty squares
  (`evaluate.cpp`, `Pt > QUEEN`); ours counts its attack map only. A
  cannon moves by sliding and takes by a hop, so the two counts differ
  most there. FSF probe rows with its own mobility counted both ways, the
  fairy formula for every type: the attack form fits FSF's row at 0.69
  and ours at 0.68; the move form fits FSF's row at 0.85 and ours at
  0.59. The count of quiet moves is an input we lack.
- Size: our mobility is a part of the most valuable piece, FSF's fairy
  value is fixed (150, 200); in xiangqi the most valuable piece is the
  chariot, half a queen, so ours is half FSF's there by construction.
- A port needs quiet move maps beside the attack maps: the same recipe
  builder over the move legs. Only variants with a piece that moves and
  takes in different ways change (xiangqi in the test set), so a gain
  must reach 2σ in xiangqi alone. Not started.

### Where the losses come from (falls)

- A fall: our score at one of our moves at least 150 cp below our score
  at our move before, both of the two before it at or above 0 (a rise to
  a repetition draw and back is left out; such draws score near +48).
- Share of our moves (queue267 rounds a and b, main): standard 1.6% and
  1.8%, xiangqi 1.6%, crazyhouse 6.7% and 6.9%, capablanca 2.9% and 3.3%.
  Lost games that have a fall from 0 or more: standard 54 of 77 and 49
  of 69, xiangqi 76 of 92 and 67 of 91, crazyhouse 115 of 147 and 110 of
  148, capablanca 99 of 141 and 101 of 139.
- In about three quarters of our losses we stood at 0 or more and then
  fell within two of our moves. Test of the cause on 40 capablanca falls:
  our search and FSF's at depths 8, 11 and 14 from the position before
  the fall.
- Most falls are not mates: to a mate 4% (standard) to 8% (xiangqi), to
  -500 or less 4% to 13%; the rest fall by 150 to 500 cp.
- The 40 capablanca falls split in two. In near a third, our own search
  at depth 8 from a fresh table already sees the fall (20 to -567: -524
  at depth 8; 32 to -740: -1002; mates): in play we did not reach that
  depth (at a 5 s clock a move gets 130 to 430 ms, depth 6 to 10 here).
  In near a third, our search does not see it at depth 14 and FSF's does
  at depth 8 (742 to -120: ours +734 at depth 14, FSF +44; 177 to -190:
  ours +171, FSF -299; 697 to 516: ours +293, FSF -138). These are the
  evaluation.
- On 16 positions that we rate 250 cp or more above FSF's depth 8, the
  term that parts is the royal safety: FSF's King safety row at -5 to -16
  pawns (opening) for one side where ours is near -1 or of the other
  sign. Per side, FSF's safe checks there match ours at fairy weights;
  its attacker part is 520 units where ours is near 40: its ring for a
  royal on the edge reaches one rank further.

### kring, the danger ring moves in from the edge

- FSF's king ring is the 3 by 3 block around the royal's square moved in
  from each edge (`evaluate.cpp`, `kingRing`), nine squares for a royal on
  its first rank; ours is the same block around the royal's own square,
  six squares there. The ring decides the attackers, the weak squares and
  the pawn hits; the hits count on the squares next to the royal in
  both. A ring square the royal may not enter is weak in FSF (a palace).
- Past tries: none; plan 33 named the edge as a difference of FSF's sum
  (the danger sum chain) and did not port it.
- kring (`plan33-kring`, from main): a derived centre for each square
  (its file and rank moved in by the ring radius); the zone is the
  centre and its ring. Attackers, weak squares and pawn hits read the
  zone; a zone square the royal may not enter is weak; the hits keep the
  royal's own ring. Weights unchanged: the full ring is still nine
  squares.
- Static check (kring minus `offking`, FSF's King safety row):

| Variant | main | kring | sd kring | sd FSF |
|---------|-----:|------:|---------:|-------:|
| standard | 0.61 | 0.71 | 1.54 | 1.52 |
| crazyhouse | 0.86 | 0.90 | 3.88 | 10.16 |
| capablanca | 0.75 | 0.81 | 2.93 | 3.43 |

- Standard by the size of FSF's term: 4 pawns or more, ours +4.70
  against +6.57 (main near half of FSF's there); 98% the same sign.
- Xiangqi: 0.80 to 0.84 (sd 0.70 to 0.91, FSF's 2.91).
- kring is 5e50f8d. Smoke 2 of 4 in capablanca, no forfeits. Queue270:
  main and kring, all four variants, three rounds of 200.
- Tree: nodes to a fixed depth, kring over main, seeded, idle priority
  beside the queue: standard +35% (16 probe positions, depth 10) and +10%
  (32 others), capablanca +19% (16, depth 9), crazyhouse -11% (16, depth
  7). A larger sum draws the search into checks, which are extended
  (the C2 chain above); where the tree grows, the better fit pays for it.

### Queue269, pim (cut after round a)

| Variant | main | pim | pim - main |
|---------|-----:|----:|-----------:|
| standard | +74.1 | +65.0 | -9.1 |
| xiangqi | +61.4 | +34.9 | -26.5 |
| crazyhouse | -195.5 | -141.0 | +54.5 |
| capablanca | -129.0 | -147.2 | -18.2 |

- Mean +0.2. A merge needs near +30 a round from here: near 1%. Cut, no
  forfeits; not merged.

### Queue270, kring (cut in round b)

| Round | Arm | standard | xiangqi | crazyhouse | capablanca |
|-------|-----|---------:|--------:|-----------:|-----------:|
| a | main | +115.2 | +74.1 | -205.0 | -135.0 |
| a | kring | +65.0 | +63.2 | -145.1 | -101.8 |
| b | main | +38.4 | +83.2 | -130.9 | |
| b | kring | +94.3 | +27.9 | -166.2 | |

- kring minus main: standard -50.2, +55.9 (+2.9); xiangqi -10.9, -55.3
  (-33.1); crazyhouse +59.9, -35.3 (+12.3); capablanca +33.2 (one
  round). Mean near +4; round c would need near +50. Cut, no forfeits;
  not merged.
- The tree does not explain xiangqi: kring's nodes there are +2% (24
  probe positions, depth 9), and its fit rose (0.80 to 0.84).

### The order of the arms

- In each round the queue played main first, then the candidate. Four
  candidates in a row lost in standard (tpiece -4.6, tqueen -38.2, pim
  -9.1, kring -50.2 in their first rounds), where standard is the first
  variant of a round and my idle builds and probes often started. Not
  shown to be a bias, but an order that never changes could carry one.
  `queue.sh` now plays the arms in the reverse order every other round.

### The parity bundle

- Each FSF-parity port since pfsf moved its fit and played near +10 on
  the mean: tpiece +10.6, tqueen +9.9, dropmob +18.7 in crazyhouse alone,
  kring near +4, all short of the merge rule (near +20 at three rounds).
  A gain of near +10 cannot pass alone. A new candidate that holds
  several of them is a new test, with no selection bias in its own
  result.
- parity (`plan33-parity`, cc288b4, from main): tpiece, dropmob and kring
  merged; kring's map regions moved to 9 and 10 beside tpiece's at 11 to
  13. The terms do not read each other: on one probe position in each of
  crazyhouse, capablanca and standard, parity minus main equals the sum
  of the three differences (-995, -494, -881 against the same sums).
- Smoke 1 of 4 in crazyhouse, no forfeits. Queue271: main and parity,
  all four variants, three rounds of 200, the arm order alternating.

### zhscale, the parts and the hand power

- Every crazyhouse term probed is near half of FSF's size against a
  pawn: threats (FSF on ours 2.1), royal safety (2.4), mobility (1.39 in
  FSF's trace, which is before its doubling for a hand, so near 2.8).
- Cause: all derived parts and margins are parts of the most valuable
  piece (`dearest_piece_value`). CP2's hand power (0.7 on the value over
  the cheapest piece, plan 27, +90 in crazyhouse) brings the material
  closer together: the queen is 565 there, 1186 in standard. So every
  part shrank with it, against a pawn of 100 in both. FSF halves its
  material in a drop variant and keeps its other terms whole.
- Past tries of the size in crazyhouse: KH (the royal hand inputs, size
  2.39 to 1.88 of FSF's, +1.3) and DW (danger, cap, shelter and guard
  scaled where captures return, -9). Neither moved all parts together,
  nor through the cause.
- zhscale (`plan33-zhscale`, b871da6, from main): with free drops,
  `dearest_piece_value` takes the value before the hand power (565 to
  1187). The material is unchanged. Only crazyhouse has free drops. Its
  param file was made again (the file was removed, the engine derived and
  wrote it, the build embeds it): the 11 eval scalars change by near 2.1
  (tempo 13 to 28, the danger cap 565 to 1187), the material and tables
  are the same. Bench nodes equal main's in standard, xiangqi and
  capablanca (depth 7), and differ in crazyhouse.
- It plays crazyhouse after queue271, against the base of that time.

### Queue271, parity (cut in round c)

| Round | Arm | standard | xiangqi | crazyhouse | capablanca |
|-------|-----|---------:|--------:|-----------:|-----------:|
| a | main | +111.4 | +29.6 | -151.3 | -145.1 |
| a | parity | +26.1 | +54.3 | -100.0 | -83.2 |
| b | parity | +121.1 | +47.2 | -115.2 | -123.0 |
| b | main | +98.1 | +47.2 | -155.5 | -119.1 |
| c | main | +79.5 | +79.5 | | |
| c | parity | +72.2 | +72.2 | | |

- parity minus main: standard -85.3, +23.0, -7.3 (-23.2); xiangqi +24.7,
  0.0, -7.3 (+5.8); crazyhouse +51.3, +40.3 (+45.8); capablanca +61.9,
  -3.9 (+29.0). Mean near +14. For +20 the last two variants needed near
  +71 each in round c: near 5%. Cut, no forfeits; not merged.
- Crazyhouse gains in both rounds (+46, near 1.6σ over two); it holds
  dropmob and kring there, and both leaned the same way alone. Standard
  loses; parity's tree there is smaller than main's (-18% nodes at depth
  10, 32 positions), so the loss is not the tree.
- The four standard pairs since the order alternates: parity first in
  round b, +23.0; main first in rounds a and c, -85.3 and -7.3.
- The fall rate (a fall of 150 cp between two of our moves from 0 or
  more) is higher for every arm that adds an eval term, also where the
  arm gains (tpiece in capablanca 3.2% and 3.5% against 2.9% and 3.3%):
  it does not follow the Elo and is not a lead.

### Queue272, zhscale in crazyhouse (cut after round b)

| Round | main | zhscale | zhscale - main |
|-------|-----:|--------:|---------------:|
| a (main first) | -186.2 | -159.8 | +26.4 |
| b (zhscale first) | -135.0 | -179.5 | -44.5 |

- Mean -9.0. As a change in crazyhouse alone it needs near +44 over
  three rounds; round c would have to give near +150. Cut, no forfeits;
  not merged.
- So doubling all the parts against the material, through the cause
  the probes named, plays no better than the size moves before it (KH,
  DW). The size of the crazyhouse terms against FSF's is closed as a
  lead: four tries, none above noise.

### Queue273, main against main (A/A)

- Nine first rounds of candidates lost in standard nearly every time,
  and the arm that plays first in standard scored near +17 over the
  second (seven pairs, 1.3σ). Main's own standard results across eight
  rounds (54 to 115, sd near 25.5) match the harness error (σ near
  23.5). Queue273 plays main against a byte-identical copy, all four
  variants, two rounds, the order alternating: it shows a first-arm
  effect if there is one, and the spread of a difference of two equal
  arms.

### mobmove, quiet moves in the mobility

- From the xiangqi chain: FSF counts a fairy piece's attacks on held
  squares and its quiet moves to empty ones; ours counted its attack map
  only (fit 0.41; FSF's own row counted our way fits FSF's at 0.69, its
  move way at 0.85).
- Past tries: none.
- mobmove (`plan33-mobmove`, 7b8b632, from main): the recipe builder
  takes a quiet flag (a vector's target is a last leg that can move); the
  quiet tables sit beside the attack tables, `piece_attacks!` takes either
  set, and `quiet_apart` marks a piece whose quiet tables differ from
  its attack tables on some square. Such a piece counts the held squares
  of its area that it attacks and the empty ones that its quiet moves
  reach. Bench nodes equal main's in standard, crazyhouse and capablanca
  (no non-pawn piece there moves apart from its captures) and change in
  xiangqi (the cannon).
- Static check in xiangqi (mobmove minus `offmobility`, FSF's Mobility
  row, one ply in 24 of queue267 round a): 0.41 to 0.77 (opening 0.88,
  endgame 0.69); sd 0.29 against FSF's 0.72.
- Smoke 2 of 4 in xiangqi, no forfeits. Queue274: main and mobmove,
  xiangqi only (the others search the same tree), three rounds.
- Queue274 round a: main +47.2, mobmove +6.9 (-40.3). As a xiangqi-only
  change it needs near +44 over three rounds; the two rounds left would
  need near +86 each. Cut, no forfeits; not merged.

### What the ports since pfsf show

| Port | Fit before, after | Play (mean of four, or the one variant) |
|------|-------------------|------------------------------------------|
| pfsf (merged, build 36) | 0.56, 0.85 | +31.4 |
| pstr2 | 0.75, 0.93 | +0.1 |
| KH | 0.86, 0.90 | +1.3 (crazyhouse) |
| tpiece | 0.59, 0.74 | +10.6 |
| tqueen | 0.59, 0.80 | +9.9 |
| dropmob | 0.70, 0.92 | +18.7 (crazyhouse) |
| kring | 0.61, 0.71 | near +4 |
| zhscale | sizes near FSF's | -9.0 (crazyhouse) |
| mobmove | 0.41, 0.77 | -40.3 (xiangqi, one round) |

- A rise of the static fit to FSF's term has not predicted a gain in
  play, eight times out of nine. The method that picked these leads (the
  term with the poorest fit and the largest FSF worth) is refuted as a
  predictor for our engine. Not known why pfsf is the one exception; its
  term was the one with a large worth to us before (passed pawns, -6 in
  queue256 where FSF's was 112 to 229).
- Every gain these ports could still hold is near the size that one
  round of noise moves (queue273). Under the merge rule (three rounds, 2σ
  on the mean of four) a gain under near +20 on the mean, or near +44 in
  one variant, cannot be shown.
- The user's call (2026-10-26): keep the rule, look for larger levers
  (search, time use, how games are lost).

### Time use (queue275, the clock in the match record)

- The match record now holds the mover's clock at each `go` (ec995ec,
  merged: the harness only, the engine's play is the same). Queue275 is
  one round of main with it.

| Our moves | standard: ours / FSF clock, spent | xiangqi | crazyhouse | capablanca |
|-----------|-----------------------------------|---------|------------|------------|
| 0-10 | 8992 / 6766, 278 / 760 | 8970 / 6680, 277 / 778 | 8961 / 6509, 285 / 806 | 9002 / 6666, 277 / 784 |
| 10-20 | 7086 / 2262, 229 / 316 | 7031 / 2056, 225 / 313 | 6925 / 1971, 240 / 294 | 7065 / 2176, 234 / 308 |
| 20-30 | 5662 / 971, 193 / 144 | 5586 / 935, 194 / 135 | 5341 / 942, 188 / 125 | 5548 / 966, 195 / 140 |
| 30-45 | 4358 / 763, 165 / 108 | 4269 / 739, 159 / 107 | 4142 / 746, 159 / 96 | 4226 / 754, 160 / 104 |
| 45-70 | 3134 / 739, 135 / 84 | 2964 / 666, 130 / 100 | 2599 / 619, 126 / 96 | 2928 / 723, 128 / 79 |

- Our clock at our last move in lost games (median): standard 3.3 s,
  xiangqi 4.3 s, crazyhouse 5.0 s, capablanca 3.7 s; FSF ends near
  0.6 to 0.8 s. In crazyhouse a game is near 30 of our moves, so near
  38% of our time (10 s and the increments) is never spent.
- Cause: a move uses near 0.67 of its share (remaining / 30 plus the
  increment), because a depth may start only in the first quarter of
  the budget; at that rate the clock settles near 1.5 s and a game of
  30 to 60 moves ends far above it. FSF spends near 8% of its clock a
  move at first and then plays on the increment.
- Past tries (plan 27): T1 (a depth may start until half the budget)
  -34.6 in grand; T1b (a quarter) +105.8, merged; T2 (15 moves) -36 in
  grand, as grand's long games ran short later; T3 (30 moves) +15.3,
  merged; T4 (40 moves) +6.6. All in grand, where games are long.
- zhclock (`plan33-zhclock`, from main): where the captures fill the
  hand, the clock plans for half the moves (15). No material leaves such
  a game, so it ends by a mate and sooner; it is the variant that wastes
  the most, and grand's long games (T2's loss) do not change. Only
  crazyhouse changes.
- zhclock is d2149c3. At 30 s on the clock it spends 1122 ms where main
  spends 552 (depth 12 against 11).

### Queue276, zhclock in crazyhouse

| Round | main | zhclock | zhclock - main |
|-------|-----:|--------:|---------------:|
| a (main first) | -155.5 | -141.0 | +14.5 |
| b (zhclock first) | -168.4 | -77.7 | +90.7 |
| c (main first) | -117.2 | -123.0 | -5.8 |

- Mean +33.1 ± 20.8, 1.6σ; no forfeits. A change in crazyhouse alone
  needs 2σ: not merged.
- Its time use (round a): 442 ms a move in moves 0 to 10 (main 282), 301
  in 10 to 20 (235); lost games end with 2.4 s on our clock (main 5.0).
- The lead stands: the clock left unspent is real in all four variants,
  and spending it in crazyhouse played +33. The next try is the general
  form, on the mean of four.

### tclock, 20 moves

- tclock (`plan33-tclock`, 8a75e2a, on zhclock): the clock plans for 20
  moves, and 15 where the captures fill the hand (75%). T3 chose 30 over
  20 by +15.3 ± 10.5 in grand alone, where games are long; the clock
  record shows the reserve left unspent in all four test variants, which
  grand's runs could not show.
- At 30 s on the clock: standard 1285 ms, depth 13 (main 577, depth 11);
  crazyhouse 1430 ms, depth 12 (742, 11). Smoke 2-2 in standard, no
  forfeits. Queue277: main and tclock, all four variants, three rounds.

### Queue277, tclock; merged as build 37

| Round | Arm | standard | xiangqi | crazyhouse | capablanca |
|-------|-----|---------:|--------:|-----------:|-----------:|
| a | main | +56.1 | +26.1 | -172.8 | -129.0 |
| a | tclock | +107.5 | +27.9 | -129.0 | -125.0 |
| b | tclock | +109.5 | +63.2 | -115.2 | -98.1 |
| b | main | +66.8 | +19.1 | -172.8 | -92.5 |
| c | main | +65.0 | -26.1 | -119.1 | -147.2 |
| c | tclock | +96.2 | +70.4 | -98.1 | -92.5 |

| Variant | tclock - main | σ |
|---------|--------------:|---:|
| standard | +41.8 | 19.7 |
| xiangqi | +47.5 | 20.1 |
| crazyhouse | +40.8 | 21.6 |
| capablanca | +17.7 | 21.6 |
| mean of four | +36.9 | 10.4 (3.6σ) |

- No variant below 0, no forfeits in 2400 games. Merged as build 37
  (main f5970fe: zhclock and tclock). At 30 s on the clock build 37
  reaches depth 13 in near 1.1 s where build 36 reached 11 in 0.6 s.
- So the time left unspent was the largest lever found since passed
  pawns, and it came from the match record, not from a fit to FSF.
- The closed eval ports were measured on build 36's clock. With deeper
  searches their worth can differ; tpiece, tqueen, dropmob, kring and
  mobmove keep their branches and binaries for a retest on build 37.
- Deleted: zhclock and tclock (merged), pim, zhscale, parity, the danger
  probe builds and their binaries.
- Build 37's clock (queue277 round b): lost games end with 1.9 s in
  standard (build 36 3.3 s) and 2.4 s in crazyhouse (5.0 s); FSF's near
  0.7 s. Moves 0 to 10 take 362 ms (FSF 756).

### tclock15

- tclock15 (`plan33-tclock15`, 75a290e, from build 37): 15 moves, 11
  where the captures fill the hand. T2 (15 moves) lost 36 in grand on
  build 8's clock, where a move used more of its share and grand's games
  are long; the test set still leaves time at the end on build 37.
- Smoke 4 of 4 in standard, no forfeits. Queue278: build 37 and
  tclock15, all four variants, three rounds.
- Queue278 (cut in round b): tclock15 minus build 37, standard -11.2,
  -31.1 (-21.2); xiangqi +78.8, +21.4 (+50.1); crazyhouse -13.5, -9.4
  (-11.5); capablanca +12.0 (one round). Mean near +7; round c would
  need near +50. Cut, no forfeits; not merged, branch and binary
  deleted. 20 moves is near the best point on this clock; xiangqi alone
  still asks for more time.
- Build 37 (main) against FSF 2100 in queue278: standard +103.7, +92.5;
  xiangqi +17.4, +47.2; crazyhouse -103.7, -98.1; capablanca -139.0.

### parity37, the bundle on build 37

- The eval ports were measured on build 36's clock (depth near 11 in
  0.6 s); build 37 searches near two plies deeper. The worth of a static
  term to a search can change with its depth, so the bundle plays again:
  parity37 (`plan33-parity37`, eadaf97, from build 37) holds tpiece,
  dropmob and kring, merged as in parity.
- Smoke 1 of 4 in crazyhouse, no forfeits. Queue279: build 37 and
  parity37, all four variants, three rounds.
- Queue279 round a (main first): standard +115.2 / +119.1 (+3.9),
  xiangqi +49.0 / +90.6 (+41.6), crazyhouse -100.0 / -151.3 (-51.3),
  capablanca -72.2 / -83.2 (-11.0); mean -4.2. Rounds b and c would
  need near +32 each. Cut, no forfeits; not merged.
- The deeper search does not make the ports pay either. The eval ports
  of this plan (tpiece, tqueen, dropmob, kring, mobmove) are closed;
  their branches stay as the record of the forms, their binaries are
  deleted.

### pawnmg, the pawn's opening value

- Our standard values, opening: P 100, N 383, R 621, Q 1186 (N 3.8 R
  6.2 Q 11.9 pawns); endgame P 101, N 383, R 773, Q 1454. FSF's middle
  game: N 6.2, R 10.1, Q 20.1 pawns; endgame N 4.1, R 6.6, Q 12.9. Our
  endgame ratios are near FSF's, our opening pawn is near 1.6 times as
  dear against the pieces. FSF's pawn is 126 in the middle game and 208
  in the endgame (0.61); its pieces change much less (near 0.92).
- Past tries of the value table: plan 27 (the shift flattens all ratios;
  values in proportion to the raw value too steep, not played); A1 (the
  map of the totals, closed without games: compounds stay low); plan 28
  (a learned value model, not better on unseen variants). None split a
  pawn's value by phase.
- pawnmg (`plan33-pawnmg`, 70c6400, from build 37): a pawn (a piece with
  a pawn slot) that promotes to the dearest piece has an opening value
  of `PAWN_OPENING_RATIO` (60.6%, FSF's 126 over 208) of its endgame
  value. Its worth is the promotion, which comes as the board empties.
  The opening pawn over the knight in standard goes from 0.26 to 0.16,
  FSF's middle-game 0.16.
- The parameter files were made again from a clone of the tree without
  them: 24 change (the chess-like variants; the pawn opening value 100 to
  59 to 64, and the tables built from it by 1), 20 are the same byte for
  byte (xiangqi, the shogi family, makruk, grand, whose pawn promotes
  only to a captured piece, and others). A knight for a pawn at the
  start: 169 to 216 cp.
- Smoke 4 of 4 in standard, no forfeits. Queue280: build 37 and pawnmg,
  standard, crazyhouse and capablanca, three rounds; xiangqi's tree is
  the same (its file is the same), so it counts 0 in the mean of four.
- Round a, standard: build 37 +88.7, pawnmg +139.0. Speed probes ran at
  normal priority for near two minutes of build 37's run, with half the
  processor free.

### Queue280, pawnmg; merged as build 38

| Round | Arm | standard | crazyhouse | capablanca |
|-------|-----|---------:|-----------:|-----------:|
| a | main | +88.7 | -115.2 | -143.1 |
| a | pawnmg | +139.0 | -132.9 | -113.3 |
| b | pawnmg | +143.1 | -159.8 | -94.3 |
| b | main | +38.4 | -147.2 | -127.0 |
| c | main | +72.2 | -147.2 | -94.3 |
| c | pawnmg | +159.8 | -135.0 | -56.1 |

| Variant | pawnmg - main | σ |
|---------|--------------:|---:|
| standard | +80.9 | 19.5 |
| xiangqi | 0 (the same tree) | |
| crazyhouse | -6.0 | 20.6 |
| capablanca | +33.6 | 20.9 |
| mean of four | +27.1 | 8.8 (3.1σ) |

- No variant below -2σ, no forfeits in 1800 games. Merged as build 38
  (main b3fa21c). The opening pawn's price was the cause in standard:
  the largest one-variant gain of this plan.
- dmajor, tstab and xhand are rebased on build 38; queue281 plays them
  (see below), and their binaries are the rebuilt ones.

### Capablanca falls on build 37: search or evaluation

- Falls (the rule of "Where the losses come from") in queue278 and
  queue279 round a, build 37 in capablanca: 562, 2.6% and 2.8% of our
  moves. 30 falls from 0 or more, scored by our search at 250, 1000 and
  4000 ms and by FSF's (classical, full strength) at 250 and 1000 ms.
  The machine ran the queue meanwhile: our depth 3 to 8 at 250 ms, 9 to
  15 at 4000 ms; FSF's 9 to 14. Under `movetime` this FSF stops near
  130 ms, so its scores use at most the time ours had.
- In 17 of the 30, FSF's score is 250 cp or more under ours at 4 s, so
  16 times the time does not show us the fall. Each of the 17 then walks
  FSF's depth 16 line: our score at 1 s after each of its first ten
  plies. In 4, our search agrees within one to three plies (search: the
  reply is found once it is on the board). In 9 to 10, we stay 200 to 600
  cp above FSF along the whole line, and our static score at its end
  agrees with our search (evaluation).
- At the ends of those lines, FSF's King danger row against our royal
  terms (build 37 less the build with them off): of the 9 ends where
  FSF's row is 3 pawns or more, 5 have an attacker with an archbishop and
  no queen or chancellor. There FSF has 3.4 to 19.7 pawns, ours 0.1 to
  1.7. In the other four, one attacker has no major at all and two
  have a queen (FSF 14.5 and 4.4, ours 1.0 and 3.1).
- Cause: the cut for a missing major (`DANGER_NO_MAJOR`, FSF's -873)
  reads the major role, the dearest ceil(20%) of the piece types: in
  capablanca the chancellor and the queen of seven types. FSF's
  `major_pieces` holds the queen, archbishop, chancellor, aiwok and
  amazon. So an attack led by an archbishop loses 53% of a full ring and
  most often falls under the threshold.
- Not a lever through trades: the archbishop's value. It is cheap
  against FSF (A over Q 0.75 and 0.66 against 0.87 and 0.82), but trades
  that our values rate fair and FSF's rate 2.4 pawns or more bad come in
  30 of 1200 build 37 games (our score 0.28 in them, 0.33 over all
  archbishop losses), near 1 to 2 Elo.
- It does part the static scores of unbalanced material. At one line end
  (archbishop and bishop against chancellor, two knights and a pawn) ours
  is -8.79 against FSF's -3.38, nearly all material. Over the knight, our
  opening values follow FSF's middle game for B, R, C and Q (1.02, 1.69,
  2.89, 3.24 against 1.06, 1.63, 2.94, 3.25), the archbishop not (2.44
  against 2.82). In the endgame our pieces are dear against the pawn (R
  8.4 pawns against 6.6, Q 15.9 against 12.9), and FSF's Winnable term
  (no form of ours) takes 2.1 pawns off a queen against rook and bishop.
  The value model stays the open root (A1): no candidate.
- Noted, not acted on: nodes a second from the start position, ours over
  FSF's on this machine under load, standard 0.54, xiangqi 0.73,
  crazyhouse 0.61, capablanca 0.48 (plan 29, build 14, laptop: 0.84 in
  standard). The builds with threats, mobility or royal safety off run
  at near the same speed, so the eval terms are not the gap.

| Try | Result | Recorded cause | Precondition | What differs now |
|-----|--------|----------------|--------------|------------------|
| KD (plan 29) | the danger sum and its cut, merged | | | |
| A13 (build 35) | two-means roles for every reader: capablanca -12.3, mean +4.7, closed | within one σ | the role read by the imbalance, the material test and the cut | dmajor changes only the cut; the cause is from the falls; build 37 |

### dmajor, the majors of the danger cut

- dmajor (`plan33-dmajor`, 6c71521, from build 37): a major of the cut
  is a big piece worth more than the geometric mean of the cheapest and
  the dearest big piece (opening values). The role, the imbalance and the
  material test stay. Standard and crazyhouse keep the queen (with drops
  the cut is 0); capablanca adds the archbishop to the chancellor and the
  queen; xiangqi adds the horse to the cannon and the chariot.
- At the five line ends, our royal terms (dmajor less the build with
  them off) against FSF's row: +0.71 to +2.72 (+3.36), +0.06 to +3.02
  (+6.22), +0.85 to +2.49 (+7.05), +0.16 to -1.13 (-4.59), -1.69 to
  -3.12 (-19.74).
- Seeded bench, depth 8: standard 82548 and crazyhouse 333912 nodes, the
  same as build 37; xiangqi 254503 (258386), capablanca 376325 (391675).
  Smoke 1-3 in capablanca, no forfeits.
- Queue281 (on build 38): main
  and dmajor, xiangqi and capablanca, three rounds; standard and
  crazyhouse count 0 in the mean of four.

### Crazyhouse falls on build 37

- The same probe on 30 crazyhouse falls (780 in queue278 and queue279
  round a, 6.4% and 6.8% of our moves): at 250 ms our depth is 2 to 7,
  FSF's 9 to 12. In 20 of the 30, FSF's score is 250 cp or more under
  ours at 4 s.
- Along FSF's lines: in 4 our search agrees after three to seven plies;
  in most of the others we stay 300 to 2500 cp above FSF for all ten
  plies, and our static score at the end agrees with our search.
- At the line ends FSF's King danger row is 7 to 21 pawns in 12 of the
  20 and carries most of its total; our royal terms there are 0.2 to 7.5
  pawns, some of the other sign. In FSF a drop variant takes the opening
  danger in both phases, scaled by the material density; ours keeps the
  hand in the phase score, so crazyhouse stays near the opening and the
  phase is not the cause.
- The size of the crazyhouse royal terms is closed (KH, DW, zhscale and
  the hand inputs, four tries, none above noise). The probe adds no new
  cause, so it gets no candidate.

### tstab, more time for an unstable root

- TI (plan 29) let an unstable root (the best move changed, or the score
  fell by more than the aspiration delta) start a new depth until half
  the time, a stable one until a quarter: shogi -19.8 ± 12.3 on build
  10, cause not measured ("the clock has no reserve"). Build 37 ends its
  lost games with near 2 s of 10 left; the test set and the depth
  differ.
- tstab (`plan33-tstab`, 3066e53, from build 37) is TI's form: the
  parts are `DEPTH_START_PART` (4) and `UNSTABLE_START_PART` (2).
  Fixed-depth nodes equal build 37's (standard depth 8, capablanca depth
  7). On a 30 s + 100 ms clock, 20 standard positions of moves 20 to 30,
  two passes: build 37 1193 and 1153 ms a move (depth 12.5, 12.4),
  tstab 1303 and 1284 ms (12.6, 12.7). Smoke 3-1 in standard, no
  forfeits.
- Queue282, after queue281: main and tstab, all four variants, three
  rounds.

### xhand, no check extension with a piece in hand

- Crazyhouse, one middlegame (a pawn in each hand): FSF reaches depth 8
  in 9730 nodes (91 ms) and depth 11 in 765 ms; build 37 needs 58821
  nodes (834 ms) for depth 8. The gap is there from depth 1 (164 nodes
  against 64) and depth 5 (5194 against 570).
- With a piece in hand a check is common (PCRH, build 27: it falls to the
  move count, the exchange and LMR), but it still gets one more ply.
  FSF extends a check only past depth 6 with `abs(eval) > 100`.

| Try | Result | Recorded cause | Precondition | What differs now |
|-----|--------|----------------|--------------|------------------|
| XS2 (plan 27, a checking drop is not extended) | shogi -2.0 ± 12.4 | the deeper search on the slow shogi positions gives no strength | build 8, checks never cut; the crazyhouse arm stopped, never measured | PCRH cuts checks with a hand; crazyhouse plays; build 37 |
| XS (a check with a losing exchange is not extended) | shogi +13.0, grand -16.8 | grand needs sacrifice checks extended | every variant | only a mover with a hand changes, so grand and every variant without drops keep their trees |
| xf (no extension at depth 1) | +5.0 mean, closed | the saved tree buys less | every variant | |
| PC26, CR26, PCR26, PCRH | PCRH merged (+26.2 in crazyhouse) | a hand makes every check common | the extension kept | the extension is the one check rule left with a hand |

- xhand (`plan33-xhand`, 436792a, from build 37): while the mover holds
  a piece in hand, a check gets no extra ply. Seeded bench equal to build
  37 in standard, xiangqi and capablanca; crazyhouse depth 8 373897
  nodes (333912). The middlegame above: depth 8 in 20792 nodes, depth 10
  in 46113 (106930). On a 30 s + 100 ms clock, 20 crazyhouse positions of
  moves 20 to 30, two passes: build 37 1891 and 1818 ms at depth 10.10
  and 10.05, xhand 1622 and 1723 ms at 11.25 and 11.20. Smoke 2-2, no
  forfeits.

### Queue281, one session for dmajor, tstab and xhand

- The arms share the base's rounds: each round plays main and tstab in
  all four variants, dmajor in xiangqi and capablanca, xhand in
  crazyhouse (33 runs over three rounds; 42 as three queues). An arm's
  untouched variants count 0 in its mean of four. `queue.sh` takes an
  arm as `name:v1,v2` for that.
- If queue280 merges pawnmg as build 38, the arms play on it: dmajor,
  tstab and xhand were built again on `plan33-pawnmg` (seeded bench equal
  to pawnmg in standard; in crazyhouse only xhand differs).

### Queue281, dmajor, tstab and xhand on build 38; dmajor merged as build 39

| Variant | main (a, b, c) | tstab | dmajor | xhand |
|---------|----------------|-------|--------|-------|
| standard | +117.2, +168.4, +117.2 | +166.2, +139.0, +181.7 | | |
| xiangqi | +45.4, +36.6, +13.9 | +88.7, +66.8, +19.1 | +10.4, +59.6, +68.6 | |
| crazyhouse | -81.4, -119.1, -121.1 | -121.1, -147.2, -92.5 | | -121.1, -179.5, -159.8 |
| capablanca | -63.2, -100.0, -74.1 | -79.5, -56.1, -68.6 | -20.9, -33.1, -43.7 | |

| Arm minus main | standard | xiangqi | crazyhouse | capablanca | mean of four |
|----------------|---------:|--------:|-----------:|-----------:|-------------:|
| tstab | +28.0 | +26.2 | -13.1 | +11.0 | +13.1 ± 10.2 (1.3σ) |
| dmajor | 0 | +14.2 | 0 | +46.5 | +15.2 ± 7.2 (2.1σ) |
| xhand | 0 | 0 | -46.3 | 0 | -11.6 (crazyhouse -2.2σ) |

- No forfeits in 6600 games. dmajor passes (+2σ on the mean, capablanca
  +2.3σ alone, no variant below -2σ): merged as build 39 (main
  b18a34d). The archbishop's attacks were the danger the cut threw away.
- tstab is under 2σ: not merged; the branch stays as the record of the
  form. TI's precondition (a clock with a reserve) held, and the gain is
  near +13, under what three rounds can show.
- xhand loses in crazyhouse (-46.3, -2.2σ): the extra ply of a check
  with a hand is worth more than the plies the tree gains (+1.1 on a
  30 s clock). Not merged; the branch stays as the record.
- Binaries of tstab and xhand deleted; `~/aneka-q/bin/main.exe` is build
  39.

### The danger parts after dmajor

- A diagnostic (`plan33-dparts`, deleted) logged our danger parts at two
  capablanca line ends. With an archbishop leading: our sum 1.10 full
  rings against FSF's 1.43; attackers 0.38 against 0.53, unsafe checks
  0.45 against 0.54, ring hits 0.25 against 0.25, and FSF's mobility and
  flank parts (0.06, 0.04) that we lack. The gap is spread over the
  parts, with no one cause: no candidate. The queen-led end is near the
  endgame, where FSF's middle-game danger counts little; its gap there is
  not royal.

### pieceg, the endgame value of a piece

- Build 39's endgame values come from the moves at 12% occupancy, the
  opening values at 36%, both shifted by the opening offset. The
  sliders gain 23% to 24% in the endgame (standard R 621 to 773, Q 1186
  to 1454), the knight 0%; FSF's pieces gain 6% to 11% (N 1.093, B
  1.109, R 1.082, Q 1.057) and its pawn 65%, which build 38's pawn has.
  Over the endgame pawn our R is 7.7 and Q 14.4 pawns, FSF's 6.6 and
  12.9.

| Try | Result | Recorded cause | Precondition | What differs now |
|-----|--------|----------------|--------------|------------------|
| the shift, values in proportion (plan 27) | proportion too steep, not played | | | |
| A1 (a map of the totals) | closed without games | compounds stay low | | |
| plan 28 (a learned value model) | not better on unseen variants | | data | |
| A3 (opening occupancy from the setup) | -6.3, closed | | the endgame a third of it | the endgame part, not the occupancy |
| pawnmg (build 38) | +27.1, merged | the opening pawn was dear | | the pieces' endgame part, the other half of the same table |

- pieceg (`plan33-pieceg`, 0eba6fb, from build 39): a piece that is not
  a pawn and not royal has an endgame value of `PIECE_ENDGAME_RATIO`
  (108.5%, the mean of FSF's four) of its opening value. A value port,
  as pawnmg. Standard endgame: N 416, B 412, R 674, Q 1287 (FSF over its
  pawn 4.1, 4.4, 6.6, 12.9); capablanca R 727, Q 1394, A 1049, C 1246;
  xiangqi chariot 901 to 764, cannon 472 to 406.
- All 44 parameter files made again from an empty folder. A lone rook
  ending: build 39 832 cp, pieceg 733. Smoke 4-0 in standard, no
  forfeits. Queue282: build 39 and pieceg, all four variants, three
  rounds.
- Queue282, pieceg minus build 39: standard -6.8, xiangqi -28.1,
  crazyhouse +23.8, capablanca +4.6; mean -1.6 ± 10.3. No forfeits in
  4800 games. Not merged; the branch stays as the record, its binary is
  deleted. The endgame part of the table is not the lever the opening
  pawn was: matching FSF's endgame ratios plays the same.

### xsee, a check with a hand keeps its ply only if it does not lose

- xhand (no extension with a hand) lost 46 in crazyhouse; PCRH (the cut
  of checks with a hand) gained 26; FSF extends a check only past depth
  6. So some checks with a hand earn their ply and some do not. XS (a
  check with a losing exchange is not extended, plan 27) gave shogi
  +13.0 and grand -16.8, whose sacrifice-check mates need the ply; it
  never played crazyhouse. xsee is XS for a mover with a hand only, so
  grand and every variant without drops keep their trees.
- xsee (`plan33-xsee`, 00f0b22, from build 39): with a piece in hand, a
  check under the extension cap is taken back, priced by `see!` and made
  again; it gets the extra ply only if the exchange does not lose.
  `victim_value!` gives 0 for a move that takes nothing (no caller
  reached that before). Seeded bench equal to build 39 in standard,
  xiangqi and capablanca; crazyhouse depth 8 328175 nodes (442555). On
  a 30 s + 100 ms clock, 20 crazyhouse positions, two passes: build 39
  1549 and 1532 ms at depth 12.15 and 12.00, xsee 1367 and 1375 ms at
  12.30 and 12.25. Smoke 1-3, no forfeits.
- Queue283: build 39 and xsee, crazyhouse, three rounds. Rule set before
  the games: it merges at +2σ in crazyhouse; between +1σ and +2σ it
  plays three more rounds and merges at +2σ over six.
- Queue283, crazyhouse: build 39 -135.0, -139.0, -129.0; xsee -166.2,
  -175.0, -177.2; xsee minus build 39 -38.5 ± 21.6 (-1.8σ). No forfeits.
  Not merged; the branch stays as the record, its binary is deleted.
- With xhand (-46.3), two cuts of the extension with a hand lose near
  40: in crazyhouse the extra ply of a check pays even when the check
  gives its piece away (a sacrifice drop that opens the royal). The
  extension with a hand is closed in this direction.

### nmeval, the null move's lead over beta

- Our null move reduces `4 + d / 4`; FSF's adds `min((eval - beta) /
  205, 3)`. Stage E (plan 1) played that term with an endgame
  verification in a self-play pool: -3, neutral, no cause recorded. The
  engine, the opponent and the test set have all changed since; the term
  plays alone here.
- nmeval (`plan33-nmeval`, 53ec589, from build 39): one more ply of
  reduction for each `null_eval_unit` of lead (`NULL_EVAL_RATIO`, 8.1%
  of the most valuable piece, FSF's 205 over its queen of 2538), at
  most `NULL_EVAL_PLIES` (3). Derived at load, so no parameter file
  changes. Seeded bench: standard the same (bare endings, no null move),
  xiangqi 146815 (146946), capablanca 225325 (225368), crazyhouse 434332
  (442555). On a 30 s + 100 ms clock, 20 standard positions, two passes:
  build 39 1072 and 1066 ms at depth 14.10 and 14.00, nmeval 1169 and
  1232 ms at 14.30 and 14.35. Smoke 4-0, no forfeits.
- Queue284: build 39 and nmeval, all four variants, three rounds.
- Queue284 (cut in round b): nmeval minus build 39, standard +12.3,
  +36.0; xiangqi +7.0, +1.7; crazyhouse -90.3 (round a); capablanca
  -15.9 (round a). Mean -11.7 ± 14.7; the six round differences left
  would need near +48 each. Cut, no forfeits; not merged, the branch
  stays as the record, its binary is deleted.
- The cut came late: queue284 was hopeless (z 4.0 at half the runs)
  before it was read. From queue285 on, `~/aneka-q/watch.sh` reads each
  finished run with `tools/hopeless.py` (the mean the runs left must
  reach for the merge rule, against the trend; past 2.5σ it is
  hopeless) and wakes the session at once.

### tstab again, on build 39

- tstab on build 38 (queue281): +13.1 ± 10.2, positive in three of four;
  an effect of that size cannot show in three rounds. Rule set before
  the games, as for PCRH: tstab (rebased, 1c57655; fixed-depth nodes as
  build 39's) plays three more rounds on build 39 (queue285) and merges
  if the six rounds pooled give +2σ on the mean of four, near +14.4;
  queue285 then needs near +15.7 alone. Smoke 3-0-1, no forfeits.

| Variant | queue285 tstab - build 39 | six rounds pooled |
|---------|--------------------------:|------------------:|
| standard | +23.2 | +25.6 ± 15.1 |
| xiangqi | +22.8 | +24.5 ± 14.2 |
| crazyhouse | +19.9 | +3.4 ± 14.4 |
| capablanca | +13.7 | +12.3 ± 13.9 |
| mean of four | +19.9 ± 10.1 | +16.5 ± 7.2 (2.3σ) |

- One forfeit in 4800 games: FSF lost one crazyhouse game on time
  (round c, the tstab run); without it the pooled mean is near +16.4.
  The pooled rule holds, no variant below -2σ: merged as build 40 (main
  7b346e0).
- The build 40 sweep (`~/aneka-q/sweep.sh`): the 36 variants that both
  engines play, 400 games each, against FSF at `UCI_Elo` 2100, then at
  2000.

### Where build 37's time goes

- The `hotpath` build of build 37, seeded bench at depth 7 (the large
  blocks only; plan 32 found the small nested ones inflated). A node
  costs 14.4 µs in capablanca. `eval::attack_terms` takes 12.4 µs a call
  in capablanca, 8.1 in crazyhouse, 9.0 in xiangqi and 2.5 in standard
  (whose bench is bare endings), at 0.48 calls a node in capablanca:
  near 40% of the search time there.
- Timers inside it (each costs near 0.4 µs, so only the order is read):
  the map pass first, then the danger part, then pawn targets and passed
  pawns; the goal race is near nothing.
- A diagnostic that casts every map twice (the same output) benches the
  same as build 37 within the noise of the loaded machine (±10%), so the
  casts are not the cost. Finer speed work waits for a quiet machine.

### Queue273, the A/A result (stopped after round a)

| Variant | main (first) | copy (second) | copy - main |
|---------|-------------:|--------------:|------------:|
| standard | +66.8 | +88.7 | +21.9 |
| xiangqi | -10.4 | +22.6 | +33.0 |
| crazyhouse | -149.3 | -190.8 | -41.5 |
| capablanca | -184.0 | -96.2 | +87.8 |

- Two copies of one binary, one round: the mean of four is +25.3, which
  a candidate would read as a gain. The spread of the four (sd near 53)
  is above the σ near 35 that the harness error gives a difference of
  two rounds of 200. No steady first-arm edge: the second arm led in
  three of four.
- So one-round differences of 50 to 90 are noise here, the capablanca
  and crazyhouse first rounds of tpiece, tqueen, kring and parity among
  them. The three-round rule is the guard; a cut on one round only stops
  a run that cannot pass, it shows no loss. The order hypothesis above
  is not supported; the alternation stays as it costs nothing.

### pim, the move of a stopped depth

- An iteration that the clock stops was thrown away whole; the move of
  the previous depth was played (plan 24 kept a fallback for depth 1
  only, and recorded the later depths as a choice, not tested in play).
  pim (`plan33-pim`, 612a66c, from main): each depth starts with no
  candidate; a root move becomes the candidate when its subtree completes
  with a score above alpha (above the window or the moves before it); a
  stopped depth plays its candidate if it has one. Depth 1 keeps its old
  rule. The score of a stopped depth is never kept.
- The time series of plan 27 (T1, T1b, T2, T3, T4) set when a depth may
  start and the move horizon; none used a stopped depth's move.
- Fixed-depth nodes equal main's (capablanca bench, depth 8, 391675).
  Smoke 1-1-2 in standard, no forfeits. Queue269: main and pim, all four
  variants, three rounds of 200.
