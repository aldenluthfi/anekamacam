# Frozen Baseline

## Status

**Frozen 2026-09-01 at `1df1185`. Four correctness defects fixed on top of the
ablated engine; one known defect deferred. This document is the behaviour
record every later change is measured against. The signature below was
re-recorded after the fourth fix, which moved three of the 38 rows.**

The engine returned to `2c6f5db` in `7bac6ab` after iterations 4 and 5 were
abandoned. See `plans/17-strength-iteration-5.md` for why. What follows is the
state after that ablation plus the fixes below.

## Build and provenance

- HEAD: `1df1185806a7ce4d2f5dd0cf4c3c33f389278f21` on `main`.
- Binary: `bin/frozen`, 5,498,576 bytes.
- Binary MD5: `0f52f5b50db16c8cab1b4e3d96af38fc`.
- `content_md5`: `5af33329ac02272d64c4c80e73ec8d99`.
- `tools/provenance.sh verify bin/frozen` rebuilt `1df1185` and matched
  `content_md5` exactly.
- Config tree MD5: `c09f443db4fe35a46cec187ed619c637`.
- Dictionary tree MD5: `39bee9589de36c8d7562b8c42c09af44`.
- UCI option MD5: `f3f291236fa49c71c5569f2beec47411`.
- Embedded variants: 38.
- Compiler: `rustc 1.97.0-nightly (e96c36b6f 2026-05-21)`.
- Host: macOS 26.5.1, arm64.

## What was fixed

1. **A repetition is scored by the rule that declares it.** `3a6aaf9`.
   Search returned a neutral 0 for any position seen twice, before consulting
   the variant. Shogi and its relatives declare four occurrences; xiangqi
   punishes whoever sustained the cycle rather than halving the point. One
   fold short of xiangqi's third, chariot and general against a bare general
   scored `cp 0` in eight nodes and now scores `mate -2` with the mating line.
   At shogi's second fold a drawn score becomes `cp -1042`.

2. **A stored bound no longer answers a wide window.** `218c5fd`.
   A bound names a score and no move, so cutting on one at a PV node left the
   printed line to come from walking the table. Six sampled cases printed
   variations shorter than their own depth -- `asean` at 8 printed five,
   `hoppelpoppel` at 8 printed six, `almost` and `fivecheck` at 10 printed
   seven, `capablanca` and `chancellor` at 9 printed eight -- and all now
   print in full.

3. **SMP picks the deepest finished worker.** `1df1185`.
   The pool compared scores reached at different depths, and its `>=` handed
   ties to whichever worker joined last. `SearchResult` now carries
   `completed_depth`, written only where `best_score` and `best_move` are.
   Nodes are summed across workers rather than taken from the winner:
   standard at depth 10 reports 2,318,268 nodes at one thread and 8,812,872
   at four.

4. **The exchange prices each piece at the phase it actually leaves in.**
   `p_value!` interpolates between opening and endgame values by
   `phase_score` while in MIDDLEGAME, and the exchange itself moves that
   score as pieces come off. `see!` priced the attacker before its capture
   was made, then spent that figure deciding a recapture that happens after
   the victim has already gone; the same off-by-one repeated for every
   attacker in the loop. Measured against two independent oracles over
   18,882 standard captures, disagreement with its own least-valuable-
   attacker order fell from 1047 to 2, sittuyin from 1345 to 0, minishogi
   500 to 0, janggi 102 to 0, xiangqi 54 to 0. Evidence in
   `plans/19-exchange-oracle-evidence.md`. Three signature rows moved:
   judkins and minishogi by node count, xiangqi from `h3:h5` to `h3:e3`.

## Known defect, deferred

**Every search shortcut is applied to every variant unconditionally.** There
is no capability mask. A capture pays twice in crazyhouse and the shogi
family, which hand the piece back, and in `grand` and `sittuyin`, which feed a
promotion pool, and the exchange does not price that. Null-move pruning runs
on janggi, which offers a real pass, and through sittuyin's setup phase.
Reverse futility, futility and late-move pruning run on `koth`, `threecheck`,
`fivecheck`, `extinction`, `horde` and `kinglet`, where a positionally quiet
move ends the game with no material signal.

Deferred by instruction, not by judgement.

**Screened movement is NOT among these, corrected 2026-09-01.** The abandoned
iteration disabled the exchange on `xiangqi`, `minixiangqi`, `janggi` and
`sittuyin` on the premise that a cannon's screen leaving mid-exchange leaves a
phantom attacker. That premise is false. `lva!` regenerates candidates through
`process_multi_leg_vector!` against live occupancy after every make, so it is
occupancy-exact where a classical bitboard exchange would not be. Verified
three ways on xiangqi: with the screen gone `a5*e5` is neither generated nor
counted, with it restored both happen; blocked sliders are excluded and x-ray
revelation works; and the destroy-then-unload screen is correctly not counted
as captured material.

Two earlier readings that looked like the bug were confounded -- one position
had the cannon pinning the recapture, so the uncontested capture was right,
and a residual difference between two positions was piece values shifting with
game phase, reproduced by parking an idle cannon where it attacked nothing.
Any future attempt to gate the exchange by variant needs new evidence, not
this claim.

## Suites at the freeze

- Debug fixed-depth search, all 38 configs: 0 assertions.
- End-condition fixtures: 38 passed, 0 failed.
- FEN round trip: 44 passed, 0 failed, 0 skipped.
- Crazyhouse drop integrity against fairy-stockfish: 24 games, 0 mismatches.
- Perft: standard 20256/20256 at depth 3; crazyhouse 16/16 at depth 4; shogi
  12/12, xiangqi 33/33, janggi 21/21, minishogi 9/9, judkins 9/9, euroshogi
  12/12, pocketknight 9/9, sittuyin 12/12, minixiangqi 3/3 at depth 3.

## Fixed-depth signature

`ANEKAMACAM_SEED=42`, Threads 1, depth 6, via
`debug-headless search <variant> 6 1`, which builds a 1 MB main table and a
1 MB quiescence table. Any later change that claims to leave behaviour alone
must reproduce this table exactly.

```text
config         best        score      nodes
ai-wok         d3:d4          0      20254
almost         b1:c3          4      27325
amazon         e2:e4          1      27829
asean          d3:d4          3      19936
berolina       b1:c3         10      30879
capablanca     b1:c3          1      48721
chancellor     b1:c3          3      18640
chigorin       b1:c3        -52      14646
crazyhouse     d2:d4          0      19782
embassy        i1:h3          1      47112
euroshogi      f3:f4          4      28873
extinction     g1:f3          0      15285
fivecheck      d2:d4          0      17255
gothic         i1:h3          0      63674
grand          e3:e5          4      49208
hoppelpoppel   g1:f3          4      20052
horde          d4:d5       -514      41457
janggi         Q@e2           0        490
janus          f2:f4          0      47542
judkins        d1:c3          8      42171
kinglet        g1:f3          0       5227
knightmate     c2:c4          0      19342
koth           d2:d4          0      17255
los-alamos     c2:c3          2      15759
makruk         b1:d2          3      11994
minishogi      d1:c2          0      12911
minixiangqi    f1:f4         13       7301
modern         b1:c3          1      22448
newzealand     d2:d4          0      37876
ouk-chaktrang  g1:e2          0      14333
pocketknight   g1:f3          3      71355
shatranj       g1:f3          0       6464
shogi          c3:c4          2      24459
sittuyin       K@a1           0   16960479
standard       d2:d4          0      17255
threecheck     d2:d4          0      17454
tjatoer        a3:d9         71     159733
xiangqi        h3:e3         24      59218
```
