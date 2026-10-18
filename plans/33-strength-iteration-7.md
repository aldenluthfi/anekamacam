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
