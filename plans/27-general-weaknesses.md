# General weaknesses against Fairy-Stockfish

## Status

Saved 2026-09-27 for future use. Not started. The analysis of all four
benchmark variants and the deep investigation are done (2026-09-29). The
recorded-game analysis (both engines' scores on each move) is done too and
shows where games are really lost. The fix stages below wait for the user.
Stages that have proof come first. Stages without proof are marked
"hypothesis".

## Context

The parallel `debug-headless sprt` rates HEAD 451dec3 against FSF UCI_Elo
at 10+0.1 in each variant (`plans/26-variant-benchmark.md`). The user asked
for the causes on four fronts: evaluation, search, parameter derivation and
move generation. The engine is variant-agnostic. Thus a finding in one
variant is a defect in a generic mechanism, and each fix must be generic.

| variant  | FSF UCI_Elo        | analysis |
| -------- | ------------------ | -------- |
| standard | ~1977..1982 (1967+) | done     |
| shogi    | ~1824, 1823..1838  | done     |
| xiangqi  | ~1800, 1794..1809  | done     |
| grand    | ~1760, 1751..1780  | done     |

Who did what: Sonnet agents did the shogi, grand and standard log analysis,
the code audit, the 100-position fatal samples and the eval sweep. I checked
each number that changes a conclusion. A number that I did not check is
marked "agent".

The raw data, the probe patch (`probe-search.diff`, counters and the S1
modes) and the scripts were in the session scratchpad
(`/private/tmp/claude-501/.../scratchpad/`). That folder is temporary. If
it is gone, the method below rebuilds everything from the harvested logs in
`res/sprt/`.

## Method

This method applies to each variant, so the results can be compared.

1. **Rebuild the games.** No PGN exists. The harvested logs of the rating
   run closest to 50% (`res/sprt/<variant>/[0-7]-engine-a_latest.log`) have
   every `position`, `go` clock line, and the depth, score and PV of each
   search. A `ucinewgame` line starts a new game. The sign of the final
   score gives the result.
2. **Game shape.** Get the win and loss length, the time for each move of
   the two engines, the clock left at the end, and the median depth.
3. **Loss type.** A loss is sudden if the score goes from >= -100 to <=
   -300 in one move. Otherwise it is gradual. Measure how many losses end in
   a mate, the mate distance when the mate first appears, and the depth of
   the search before it.
4. **Fatal positions.** Take 100 random positions (seed 1) before the
   sudden drop. Compare FSF at depth 16 (best move, and the result of our
   move with `searchmoves`) with our search at depth 16. Use a thread pool,
   not `multiprocessing` (macOS cannot spawn from a stdin script).
5. **Material.** Remove one piece of each type from the start position and
   compare `debug-headless evaluate` with FSF `eval`.
6. **Speed.** Compare perft from the start with FSF `go perft`, and nodes to
   depth 12 on six middlegame positions. Use UCI for the two engines with
   Hash 64 and one thread. Time only on an idle machine.
7. **Eval sweep.** 500 quiet positions per variant from the logs: our
   static eval against FSF `eval`, by phase, material and king danger.

Rules for the runs:

- Nothing else runs on the machine during a rating run.
- Start every analysis engine from a scratch folder, not from the repo. An
  engine that starts in the repo renames `logs/latest.log`, and a running
  SPRT then writes to the renamed file.
- A monitor pipe must not use `cut` or other buffered filters. They hold the
  lines until the monitor expires.

## Results by variant

### Game shape and loss type

| metric                         | standard | xiangqi | shogi | grand |
| ------------------------------ | -------- | ------- | ----- | ----- |
| losses that end in a mate      | 885/886  | 1321/1323 | 1525/1525 | 647/647 |
| sudden losses                  | 80%      | 67%     | 81%   | 85%   |
| mate distance when first seen  | 8        | 6       | 4     | 6     |
| score >= 0 just before the mate | 15-19%  | 37%     | 58%   | 80-88% |
| median game depth              | 15       | 13      | 10    | 12    |
| ms per move, ours / FSF        | 253/264  | 264/275 | 287/292 | 253/247 |

Time use is not a cause in any variant. We end games with the same or more
clock than FSF. In standard, the mate usually comes from a position that we
already see as lost. In the other variants it often comes from a position
that we see as equal or better.

### Fatal positions (100 per variant, depth 16)

The probe binary has three search modes: 0 is the HEAD search, 1 does not
prune a move that gives check (LMP, futility, SEE), and 2 is mode 1 plus no
LMR for a move that gives check. I recomputed these counts from the raw
files.

| variant  | FSF: lost before our move | FSF: forced mate | mode 0 finds | mode 1 finds | mode 2 finds |
| -------- | ---- | ---- | ---- | ---- | ---- |
| xiangqi  | 88   | 40   | 8    | 17   | 20   |
| grand    | 96   | 45   | 6    | 10   | 13   |
| shogi    | 82   | 35   | 17   | 14   | 22   |
| standard | 91   | 21   | 0    | 0    | 2    |

- Most fatal moves come after the position was already lost. The
  collapse point in the log is late: the real mistake is earlier.
- Mode 2 finds more of FSF's mates than mode 0 in every variant. Mode 1
  alone helps in xiangqi and grand, and is worse in shogi. Thus the gain
  comes mainly from not reducing checking moves.
- Most mates stay hidden in all modes. So S1 is a real cause, but not the
  main one.
- In shogi, 21 of 100 of our depth-16 searches did not end in 120 s.
- Caveat: mode 0 agrees with the HEAD score in only 1 to 5 of 10 positions
  per variant. The probe makes and undoes each pruned move to count
  checks, and this changes the search. The three modes are comparable with
  each other. Mode 0 is only close to HEAD.

### S1 proof on the reproductions

- Xiangqi mate 2 (`... e2e1 f8f1 e7e5`, Black to move): mode 0 does not see
  it at depth 8. Mode 1 sees it at depth 8. Mode 2 sees it at depth 3. The
  first move `f1h1` is a capture that checks, and LMR reduces it.
- Grand mate -7, White to move, from the grand start after `f2f1 e8e6 i2g1
  j10g10 h2i1 g9e8 c2b1 e8b5 g2f4 b5c6 f4e6 g8g6 e6g4 h8h6 g4h6 f9h8 h6j8
  h8h7 j8h7 i9h7 b2c4 h9e6 g3g4 d8d6 h3h4 c9e7 g1h3 b9c7 h3f4 e6h9 d3d4
  e7f6 b1g6 g10g6 f4g6 h9f7 g6f8 e9f8 i1d6 f8g8 d6c7 b8c7 c4e5 f6e5 d4e5
  d9c9 f3f4 f7b3 d2d4 c6d4 c3d4 b3c4 e2e1 c9a7 f1g3 a7a5 e1f2 a10b10 j1c1
  a5d2 f2f3 b10b3 g3e4 c4d5`. FSF line: `c1c3 b3c3 g4g5 d2e3 f3g4 e3f3
  g4f5 d5e4`. No mode sees it at depth 16. With all pruning off (NMP,
  RFP, razor, ProbCut, LMR, LMP, futility),
  depth 14 gives cp -862 after 774M nodes and still no mate. The mate is 13
  plies. FSF finds it at depth 14 because it extends checks. Each pruning
  rule that is switched off moves our score from +43 toward a loss, so the
  pruning also hides the danger. These runs were not seeded; the trend is
  firm, the single numbers are not.
- Conclusion: two check defects. LMR reduces checking moves (xiangqi), and
  there is no check extension or quiescence check to see long mates
  (grand).

### Node profile (depth 12, six positions per variant, seeded)

Nodes to depth 12, ours / FSF: standard 0.2 to 3.0, xiangqi 0.3 to 17.5,
shogi 3.9 to 15.8, grand 0.8 to 16.5. The blow-up is in all three variants
and is worst and most regular in shogi. The first sample of six xiangqi
positions (all near 1×) was chance.

| counter                          | standard | xiangqi | shogi | grand |
| -------------------------------- | -------- | ------- | ----- | ----- |
| qsearch nodes per main node      | 1.86     | 1.89    | 2.22  | 6.73  |
| moves searched per node          | 2.60     | 2.96    | 5.18  | 3.20  |
| LMP skips per searched move      | 2.05     | 2.47    | 0.91  | 4.75  |
| first move gives the cutoff      | 90%      | 81%     | 91%   | 81%   |
| LMR re-search rate               | 3.8%     | 3.9%    | 1.8%  | 8.9%  |
| drops in generated moves         | 0%       | 0%      | 39%   | 0%    |

- Grand: quiescence explodes (6.7 per node). The first move cuts less
  often, and LMR re-searches more.
- Shogi: drops are exempt from LMP and futility (`search.rs:1318`), so
  5.2 moves are searched per node and LMP rarely fires.
- Xiangqi and grand: move order is weaker (81% first-move cutoffs).

### Material (removal test from the start position)

| variant  | finding |
| -------- | ------- |
| xiangqi  | cannon 7.89 > chariot 6.98 pawns (FSF 4.38 < 6.92) |
| grand    | all pieces 19-38% cheap against the pawn; Q/R 1.77 (FSF 2.14), A/R 1.27 (FSF 1.65) (agent) |
| standard | pawn 1.8× FSF, pieces ~0.8×; no inversion (agent) |
| shogi    | ratios follow FSF; no move-only legs in shogi.conf (agent) |

### Eval sweep (500 quiet positions per variant, agent, checked)

- Our static eval is symmetric: a flip of the side to move flips the
  score, apart from the tempo. The agent's "constant optimism" of +550 to
  +770 is a selection effect. Each sampled position is one that our search
  chose to play into, so it holds our eval errors in our favour.
- The effect grows with the game length and as material comes off, in
  all four variants (mean residual ply 81-150: standard +471, xiangqi +256,
  shogi +1174, grand +1207).
- It is larger when enemy pieces are near our royal (shogi and grand
  about +1000).
- Checked example: standard `N7/4n3/2p5/3p1k1p/3K4/8/8/5B2 b`, knight and
  three passed pawns against knight and bishop. FSF +0.07, ours +637 for the
  pawns. A bare king against four pawns gives 901. Pawns, mainly passed
  pawns in the endgame, have too much value against pieces.
- Correlation with FSF: xiangqi 0.77, standard 0.70, shogi 0.60, grand
  0.40.

### Speed

Perft matches FSF node for node in all variants. Raw generation is 3.4× to
5× slower. Xiangqi search NPS is equal to FSF on an idle machine.
Standard, measured again on an idle machine (2026-09-29, six positions,
depth 12): our NPS is 1.09M and FSF's is 1.70M, and our time to depth 12 is
2.9× FSF's (0.6× to 5.8×). In standard, speed is a real handicap: lower NPS
and more nodes in half of the positions.

### Where games are lost (recorded games, 2026-09-29)

The SPRT harness now writes `res/sprt/<variant>/latest.games`: one row for
each engine move, `game;fen;move;score;result`, with the last `info` score
of the engine that moved. 100 games per variant against FSF near our
rating (standard 1975: 46W 52L 2D, xiangqi 1800: 51W 49L, shogi 1825: 49W
51L, grand 1760: 48W 51L 1D). Our engine's harvested logs of the same run
tell which rows are ours (position and score match; 95-98 games per
variant).

For each loss, the losing move is our move after which FSF's score, from
our view, is -150 or less and never comes back above 0.

| variant  | losses | one-move (FSF >= -50 before) | our score >= 0 at it | our median score | game fraction | plies until we agree |
| -------- | ------ | ---- | ----- | ---- | ---- | -- |
| standard | 42     | 15   | 36/42 | +162 | 0.35 | 42 |
| xiangqi  | 38     | 18   | 32/38 | +244 | 0.25 | 39 |
| shogi    | 46     | 19   | 41/46 | +404 | 0.32 | 24 |
| grand    | 42     | 29   | 42/42 | +878 | 0.50 | 47 |

- The losing move comes early or in the middle of the game, not at the
  end. It is a quiet move in 64% to 87% of the losses.
- At the losing move we think that we are better, in 85% to 100% of the
  losses.
- Our score agrees only 24 to 47 plies later. The sudden mates of the
  earlier sections are the end of a long misjudgment, not its start.
- Full-strength FSF at depth 16 agrees on 20 sampled losing moves per
  variant: median -213 (standard), -228 (xiangqi), -204 (shogi), -356
  (grand); lost (<= -150) in 14, 20, 14 and 18 of 20.
- Many of our wins come from a one-move collapse of the limited FSF
  (standard 7 of 43, xiangqi 13 of 50, shogi 29 of 48, grand 20 of 47).
  The strength limit of FSF is part of the rating scale.

Depth or evaluation (the losing moves again, 2026-09-29):

- A fresh depth-12 search (about our game depth) plays the same losing
  move in 20/41, 21/36, 17/46 and 18/42 positions (standard, xiangqi,
  shogi, grand). The rest was search noise of the game (history, tables).
  Two depth-20 runs of standard differ by 2 positions, so the noise is
  small at a fixed depth.
- Of these stable losing moves, depth 20 plays another move in 8, 11, 12
  and 8. Full-strength FSF says that move is better by more than 50 cp in
  7, 9, 5 and 6, and not lost in 7, 9, 5 and 3.
- So 8 more plies fix about 35% (standard), 43% (xiangqi), 29% (shogi)
  and 17% to 33% (grand) of the stable losing moves. The rest stay wrong at
  depth 20: an evaluation problem, or a horizon further than 20 plies.
- Static eval after the losing move: FSF's static eval says about equal
  (median +30 to +54 cp), and only its search finds the loss. Ours says
  good (+320 to +1274 internal, about +160 to +640 cp). On control moves
  of games we did not lose, our static scale matches FSF's (about 2 units
  per cp). So we are also statically too optimistic on these moves.

What kind of move loses (losing moves against 2 random moves per game that
we did not lose):

- Shogi: 45% of losing moves are drops (control 19%); captures 13% (44%).
- Grand: an enemy piece within 2 squares of our royal in 28% (14%); two
  or more in 7% (1%).
- Xiangqi: two or more enemy pieces near the royal in 10% (5%).
- Standard: losing moves come early (27 of 42 in the opening phase), with
  more pieces on the board; no royal-danger signal.
- Openings: FSF is at -100 or worse at the first recorded move of 11/42,
  8/38, 7/46 and 16/42 lost games. The random opening loses some games
  before the engines play, most in grand.

Time use (our logs, median ms per move, ours / FSF; our clock / FSF clock):

| plies | standard | xiangqi | shogi | grand |
| ----- | -------- | ------- | ----- | ----- |
| 0-19  | 540/1048; 8.8s/7.0s | 540/1108 | 540/1024 | 540/1234 |
| 20-39 | 397/270; 6.0s/0.7s  | 397/270  | 397/266  | 392/182  |
| 40-59 | 277/101; 3.6s/0.15s | 278/100  | 277/101  | 278/101  |
| 100+  | 121/71; 0.5s/0.27s  | 119/98   | 129/100  | 122/99   |

FSF spends its time first and then plays on the increment from about ply
40. We keep a large reserve (3.6 s at ply 40 to 59) in the phase where the
losing moves come. No losses on time or by illegal move in these runs.

Consequence: the losses have three parts. Search depth fixes about a
third. Our static eval is too optimistic on the losing moves. Time is
kept back in the middle game where the losing moves are. E1 and the eval
work move up; S1 and X1 stay (they decide how late we see the mate, and
the proofs above hold); T1 (time use) is new.

Sittuyin could not be played: our referee shows both hands empty after
White's last setup drop, and FSF's next drop (`R@d8`) is illegal for us.
Look at this later.

## Findings

Each finding lists its evidence and the variants where it shows.

1. **LMR and pruning of checking moves (S1).** Proof: mode 2 finds more
   mates in all four variants; xiangqi reproduction at depth 3 instead of
   more than 8. No gives-check test exists in `src`.
2. **No check extension and no quiescence checks.** Proof: the grand mate
   -7 stays hidden with all pruning off at depth 14. FSF extends checks.
   Plan 16 phase G tried a 1-ply check extension without a result. That
   test did not include S1, and the evidence now shows both are needed.
3. **Endgame eval: pawns too strong against pieces, royal safety off.**
   Eval sweep in all four variants; checked example above.
   `evaluation.rs:690-700` has no royal safety in `endgame_score!`.
   `phase_score` does not count pieces in hand (`move_list.rs:2159`,
   `:2590`), so shogi moves to the endgame phase while the hand is full.
4. **Node blow-up.** Shogi: drops exempt from LMP and futility and from
   history learning (`search.rs:1318-1372`, `:1459`). Grand: quiescence
   explodes. Xiangqi and grand: weaker move order.
5. **Leg semantics in value derivation.** `derive_vector_chance`
   (`parameters.rs:805-841`) gives chance 1 to a move-only final leg and to
   a capture-only leg without a hop. Xiangqi cannon > chariot; grand pieces
   too cheap against the pawn. Shogi has no such legs.
6. **Capability bits for the whole variant.** One screened leg clears
   `static_movement` (`parameters.rs:1750`). In xiangqi this turns off SEE
   pruning, ProbCut and the quiescence early stop for all captures. Plan 17
   AA-5 proposed a fix and it never ran. (Code fact; effect not measured.)
7. **Drops outside quiescence** (`search.rs:837-841`), except as check
   evasions. (Code fact; effect not measured.)
8. **King danger counts the hand badly.** Each copy in hand adds the best
   drop pressure, then the total is squared (`evaluation.rs:244-289`).
   (Code fact; effect not measured.)

Not defects:

- Move generation is correct (perft matches). Slower, but not the limit
  in xiangqi, shogi and grand.
- The xiangqi perpetual-check fixture passes now (38 of 38 on 2026-09-28).
  The failure in `plans/22` is out of date.

## Harness defects found on the way

- SIGINT kills `debug-headless sprt` without a `cancelled` result, and the
  sandbox folder `/tmp/anekamacam-sprt/<pid>` stays. A stopped run does not
  harvest its logs.
- `Elapsed: N ns` (headless search) and `Total Time: N ns` (engine log) print
  milliseconds as ns.
- The harness charges the wall time. With 8 slots on mixed cores, FSF lost
  28 of 2028 standard games on time (we lost 3).

## Cross-variant findings

| finding                          | xiangqi | shogi | grand | standard |
| -------------------------------- | ------- | ----- | ----- | -------- |
| losses end in sudden mates       | yes     | yes   | yes   | yes, from lost positions |
| S1 (checks reduced or pruned)    | proven  | yes (mode 2) | yes (mode 2) | small (0 to 2 of 21) |
| no check extension / qsearch checks | likely | not tested | proven | not tested |
| endgame pawns too strong         | yes     | yes   | yes   | yes (checked) |
| royal safety off in endgame      | yes     | yes, plus hand not in phase | yes | yes |
| node blow-up                     | yes     | yes, worst | yes | small |
| cause of the blow-up             | move order | drops not pruned | qsearch | n/a |
| leg-semantics values             | yes (move-only) | no | yes (capture-only) | small |
| variant-wide capability bits     | yes (code) | no | not tested | no |

## Plan

One stage for each finding, one commit for each stage, and SPRT after each
stage. Record each stage in this file, and add the benchmark numbers to
`plans/26-variant-benchmark.md`. All changes are generic. They reason from
the rules (check, confinement, leg semantics) and never from variant names.
Each SPRT runs one arm for each benchmark variant.

1. **S1: checking moves.** Add a gives-check test (make, test
   `is_in_check!` for the side that received the move, undo), and use it in
   the move loop. A checking move is not skipped by LMP, futility or SEE,
   and gets no LMR. Proven. Gate: xiangqi mate 2 at depth <= 4; the
   100-position samples find at least the mode 2 counts above; nodes to
   depth with `tools/ebf-suite.sh`.
2. **E1: eval.** Keep `king_danger!` in both phase halves for all variants
   (it falls by itself when attackers leave, so it needs no confinement
   gate), count hand material in `phase_score`, and correct the endgame pawn
   value against pieces. Moved up by the recorded games: we misjudge the
   losing move for 24 to 47 plies. Gate: the eval sweep late-game residual
   falls; the removal and passed-pawn examples move toward FSF; on the
   losing moves of `latest.games`, our score at the move moves toward FSF.
3. **X1: check extension with S1.** Extend a move that gives check, capped
   per line, and play quiet checks at the first quiescence ply (the old S2).
   Plan 16 phase G tested an extension alone; test it again on top of S1.
   Proven need (grand mate -7). Gate: the grand reproduction finds mate at
   depth <= 16.
4. **N1: node blow-up.** Shogi: bring drops into LMP, futility and history
   (plan 17 Q-5/R-5). Grand: find why quiescence explodes (counters by
   rule inside quiescence). Xiangqi and grand: move order. Measure with
   the probe counters (`scratchpad/probe-search.diff`) and the EBF suite.
5. **P1: leg semantics.** Move-only and capture-only final legs get their
   occupancy chance. Regenerate the embedded params (the blob hides any
   derivation fix until it is regenerated, `game_io.rs:1876`). Gate: xiangqi
   chariot/cannon >= 1.4; grand ratios nearer FSF; standard stays near.
6. **C1: capability bits per piece** (plan 17 AA-5). Hypothesis.
7. **D1: drops in quiescence** (checking drops at the first ply).
   Hypothesis.
8. **E2: king danger calibration and hand counting.** Hypothesis; measure
   first. The grand and shogi losing-move features (royal danger, drops)
   support it.
9. **T1: time use.** Spend more of the reserve in plies 20 to 60, where the
   losing moves come; we keep 3.6 s at ply 40 to 59. Depth fixes about a
   third of the stable losing moves. Gate: SPRT, as for the other stages.
10. **H: harness fixes.** SIGINT result, ms-as-ns units, and a time-loss
    count in the result file.
11. **R: re-rate all variants** with the final build. Each bisection starts
    from a range around the old rating.

## Stage outcomes

Work started 2026-09-29. Goal from the user: all four variants above 2000
on the FSF UCI_Elo scale, with the smallest change that passes. Baseline
binary `base0` is b970373.

### S1 alone (2026-09-29)

- Change: LMP, futility and SEE skips test for check after the move, and a
  checking move gets no LMR.
- Xiangqi mate 2: found at depth 3 (base: not at depth 8). Perft is
  unchanged in all four variants.
- Cost: each skipped move is made and undone for the test. NPS falls by
  about 20% (xiangqi 1.3M to 1.0M, grand 1.0M to 0.82M, depth 11, four
  positions each).
- SPRT xiangqi `0 5`: stopped at 2124 games, +0.8 ± 12.1, LLR -0.22. The
  gain pays only for the lost speed.
- Two cheaper forms lose the mate: no LMR for checks only (S1b), and S1b
  plus no SEE skip for checks (S1c). The mate needs a quiet check that LMP
  or futility skips. So S1 goes on as the base of X1, as this plan says.

### S1 + X1 (2026-09-29)

- Change: S1, plus one ply of extension for a move that gives check while
  `ply + depth < 2 * root_depth`, plus quiet checks (not drops) at the
  first quiescence ply when the stand pat is not below alpha.
- Grand mate -7: found at depth 16 (base: -124 at depth 16). Xiangqi mate
  2 at depth 3.
- SPRT grand `0 5`: inconclusive at 3000 games, +10.9 ± 10.9, LLR 1.35.
- Pivot (x1b): before the make, a skipped move is tested only if its piece
  has an attack line from its landing square to an enemy royal square
  (`relevant_attacks`, the table of `is_square_attacked!`). Other skipped
  moves are not made. The mates stay found; grand mate -7 takes 4.7 s
  instead of 7.2 s.
- SPRT grand `0 5` (x1b against base0): paused by the user at 514 games,
  254W 184L 76D, +46.6 ± 27.9, LLR 1.13. No verdict; an SPRT cannot
  resume, so the arm starts again from zero.
- SPRT grand `0 5` again on the compute server (32 cores, 30 slots,
  branch `plan27-s1x1`, 416069e against b970373): H1 at 1795 games,
  832W 664L 299D, +33.6 ± 14.3, LLR 2.95.
- The `-5 5` arms on the server, same binaries: xiangqi H1 +26.3 ± 18.5,
  shogi H1 +77.6 ± 33.2, standard H1 +33.0 ± 20.8. S1 + X1 passes in all
  four variants (commit 416069e on `plan27-s1x1`).

### E1 (2026-09-29, branch `plan27-e1`, 41d7954)

- `king_danger!` is in `endgame_score!` too.
- In a drop variant, `phase_score` counts the big pieces in hand. A capture
  to the hand keeps the phase, a drop does not add to it, and a
  promotion that takes a piece from a hand removes it. The debug build
  compares the sum with `game_phase_score!` after each move; searches on
  shogi and crazyhouse positions do not panic.
- The endgame promotion gradient of the piece-square tables falls from
  40% to 6% of the gain, as in the opening. The passed pawn term gave the
  same race a second time. The tables are in `res/param/*/latest.param`,
  so the params are made again; only the endgame tables of pieces that
  promote change, the material stays the same.
- Example `N7/4n3/2p5/3p1k1p/3K4/8/8/5B2 b` (FSF +7 cp): 637 before, 406
  after. The rest is the pawn value itself (P1).
- SPRT grand `0 5` (e1 against S1 + X1): H1, +165.2 ± 39.2.
- `-5 5` arms: xiangqi +5.3 ± 9.5 and shogi +1.4 ± 12.2 (both
  inconclusive at 3000 games; lower ends -4.2 and -10.8, so a loss is not
  ruled out), standard H1 +79.9 ± 33.5. Not proven: the two inconclusive
  arms run again with `-5 0`.

### N1, drops (2026-09-29, branch `plan27-n1`, 628cf00)

- A drop is a quiet move: LMP, futility, the quiet LMR table, killers and
  history (bonus and malus) apply to it. A checking drop stays safe
  through S1. Drop keys already use the landing square.
- Shogi, depth 10, four positions: 1.59 s to 0.86 s, nodes -48%.
- SPRT shogi `0 5` (n1 against e1): H1, +195.1 ± 46.0. The other arms do
  not run: without drops the search is the same move for move.
- The grand quiescence and move order parts wait. They run only if a
  later rating shows the need.

### P1 (2026-09-29, branch `plan27-p1`, b523a23)

- `derive_vector_chance`: a move-only last leg needs an empty square
  (`1 - occupancy`), a capture-only last leg needs a piece
  (`occupancy`), a last leg that moves and takes keeps 1. The old hopper
  case is the capture-only case. Params made again.
- Removal test, pawn = 1: standard N 2.57 to 2.95, B 3.03 to 3.44, R 4.45
  to 4.85, Q 7.78 to 8.21 (nearer FSF). Xiangqi chariot 5.41 to 5.40,
  cannon 6.12 to 5.22: the cannon falls but stays near the chariot (gate
  1.4 not met). The rest of the cannon value is its move-only slides,
  which the mobility model counts as the chariot's. A half share for
  enemy pieces (`occupancy / 2`) was tried: standard moved away from FSF
  and xiangqi did not move, so it was not kept.
- SPRT xiangqi `0 5` (p1 against n1): H1, +311.3 ± 84.7. `-5 5` arms:
  grand H1 +34.9 ± 21.3, standard H1 +74.0 ± 32.0, shogi +3.9 ± 12.3
  (inconclusive at 3000 games, lower end -8.4). Not proven: the shogi arm
  runs again with `-5 0`.
- Rule from now on: a regression arm uses bounds `-5 0` (H1: not worse
  than 0; H0: a loss of 5 or more). `-5 5` asked the wrong question. The
  gain arm keeps `0 5`. An arm counts only with H1. The three arms above
  run again with `-5 0`.
- Shogi `-5 0` (p1 against n1): inconclusive at 6000 games, +2.0 ± 8.5
  (lower end -6.5). Not proven. P1 stays for its gains in the other three
  variants; the shogi result is open.

### Rating check against FSF at UCI_Elo 2000 (2026-09-30)

- p1 (b523a23) against `fairy-stockfish` with `UCI_LimitStrength=true
  UCI_Elo=2000`, `-5 5`, 3000 games, 30 slots on the compute server, in
  the order standard, xiangqi, shogi, grand. H1 means above 2000.
- Standard: H1, +18.6 ± 15.6. Standard is above 2000.
- Xiangqi: H0, -73.8 ± 32.3, about 1926 (from about 1800).
- Shogi: H0, -124.6 ± 45.0, about 1875 (from about 1824).
- Grand: H0, -196.7 ± 66.7, about 1800 (from about 1760).
- The self-play gains are much larger than the gains against FSF.

### Grand piece values (2026-09-30)

- Removal test from the start, pawn = 1, p1 against FSF `eval`: N 2.85 /
  3.8, B 3.63 / 4.8, R 5.0 / 5.8, Q 8.6 / 12.3, C 7.7 / 10.0, A 6.2 / 9.5.
  All pieces are 20 to 35% cheap against the pawn, the compounds most.
- Cause: `derive_material_values` shifts all raw values by one offset so
  the cheapest piece is 100. The shift adds the same amount to each
  piece, so the ratios are flat, and a compound (A = B + N moves) is worth
  less than its parts.
- Tried: values in proportion to the raw value (`raw * 100 / cheapest`).
  Much too steep: standard Q 15.9, grand N 6.3. Not kept.

### C1 (2026-09-30, branch `plan27-c1`, 1a09ee8)

- `static_movement!` no longer gates SEE ordering, SEE pruning, ProbCut,
  the quiescence losing-capture break, razoring and IIR. `see!` makes
  each capture and `lva!` reads the attackers from the board again, so a
  screen that a capture adds or takes away is seen. Check: `see xiangqi
  h3h10` from the start gives -329 (cannon for knight, the chariot takes
  back), which is right.
- The `static_movement` bit and its facts (screened leg, CPMN condition)
  are removed; `wide_quiescence` moves to bit 6. A seeded search gives
  the same nodes and moves as before the removal.
- Xiangqi, depth 12, four positions: 2.0 s to 0.37 s.
- Only variants with a screened leg or a CPMN condition change, so only
  the xiangqi arm runs (`0 5` against p1), then xiangqi against FSF 2000.
- SPRT xiangqi `0 5`: H0, -49.6 ± 18.7. Five times faster to a depth, but
  weaker: the exchange shortcuts cut lines that matter when cannons are on
  the board. C1 is not merged; p1 stays the base.

### E2 (2026-09-30, branch `plan27-e2`, 52a7be5)

- `king_danger!`: a hand type adds its best drop pressure once, not once
  for each copy. Only drop variants change, so only the shogi arm runs
  (`0 5` against p1).
- SPRT shogi `0 5`: inconclusive at 3000 games, -6.4 ± 12.4. Not merged.

### Eval against FSF, term by term (2026-09-30)

- 400 to 600 positions per variant from the p1 rating games, our static
  eval against FSF `eval`, and each of our terms against FSF.
- Correlation of the full eval: grand 0.79 (0.45 in the opening phase),
  standard 0.67, xiangqi 0.76, shogi 0.73. In grand, naive material alone
  correlates better (0.87).
- At equal material in the opening, some terms point the wrong way:
  pawn structure (grand -0.40, shogi -0.54, standard -0.15), shelter and
  guard (standard -0.44 and -0.35). King danger is positive everywhere.
- A joint fit asks for 3 to 8 times more king danger against material in
  standard, xiangqi and grand. This gives KD below.
- Tried on the same data and not kept: no guard term (worse in 3 of 4),
  no opening pawn structure (better in grand and standard, worse in
  xiangqi and shogi).
- Since E1, all shogi positions are in the opening phase: material in
  hand keeps `phase_score` at the start value.

### KD (2026-09-30, branch `plan27-kd`, 7ce34df)

- `ZONE_ATTACK_FULL` 16 to 8: the royal danger cost is 4 times larger for
  the same pressure, still capped at the most valuable piece. Correlation
  with FSF: grand 0.832 to 0.847, xiangqi 0.758 to 0.774, shogi 0.732 to
  0.740, standard 0.673 to 0.665.
- SPRT grand `0 5` against p1, then xiangqi, shogi, standard `-5 0`.
- Grand: inconclusive at 3000 games, -10.4 ± 11.1. No gain, so KD is not
  merged and its other arms were stopped (xiangqi stood at +6.7 ± 13.9
  after 1725 games).

### HB (2026-09-30, branch `plan27-hb`, 05c3478)

- A piece in hand gets the best square bonus of the squares where it can
  be dropped (`hand_bonus!`, derived in `derive_advantage_parameters`
  from the final tables). A piece on the board already gets its square
  bonus; in hand it got none.
- Reason: a fit of FSF `eval` on our shogi eval plus the hand material
  gives about 28 cp for each pawn unit in hand above our eval.
- Correlation with FSF, shogi: 0.740 to 0.756.
- SPRT shogi `0 5` against p1: H0, -46.4 ± 18.1. Not merged.

### HL (2026-09-30, branch `plan27-hl`, 3b7d543)

- Late move reduction reads the history of a quiet move (not a killer):
  one ply less when it is positive, one ply more when it is negative,
  kept in `[0, depth - 2]`.
- Nodes to depth 11, four positions each: standard -26%, shogi -34%,
  grand -31%, xiangqi -67%.
- SPRT xiangqi `0 5` against p1; if H1, grand, shogi, standard `-5 0`.
- Xiangqi: inconclusive at 3000 games, +11.1 ± 9.9 (lower end +1.2). No
  H1, so not merged and the other arms did not run. The fewer nodes give
  less Elo than their size suggests.

### SO (2026-09-30, branch `plan27-so`, 078da70)

- Correction to C1: xiangqi never had SEE. The flying-general leg (a
  capture that must take a royal) set `royal_capture`, which cleared
  `see_valid` for the whole variant. So C1 changed only razoring, IIR and
  the quiescence losing-capture stop, and those lost 50 Elo.
- SO: `royal_capture` is removed (an exchange square never holds a royal
  that a move can take), and the capture order uses SEE without the
  `static_movement` gate. The SEE skips still need `static_movement`, so
  xiangqi gets the exchange order only.
- Xiangqi, depth 12, four positions: 1.99 s to 1.01 s, nodes 2.17M to
  0.69M. Perft unchanged.
- SPRT xiangqi `0 5` against p1: H1, +42.1 ± 16.2. Only variants with a
  screened or royal-capture leg change, so no other arm runs. Merged.

### GS (2026-09-30, branch `plan27-gs`, d604934)

- `promote to captured` no longer clears the exchange bits (`see_valid`,
  `see_pruning`, `recapture_order`); only drops count as recycled
  captures. A capture gives the piece back to its owner's pool, not to
  the taker, and `see!` makes real moves. Grand had no exchange order and
  no losing-capture stop (6.7 quiescence nodes per main node).
- Grand, depth 11, four positions: 1.85 s to 1.24 s, nodes 1.72M to
  0.83M.
- SPRT grand `0 5` against p1: H1, +136.7 ± 33.6. Only variants with
  promotion to a captured type and no drops change. Merged.

### Server disk full (2026-09-30)

- The clones kept harvested engine logs and full `target/` trees (e1
  alone 5.5 GB); the 20 GB disk filled. The T1 and KD builds failed, and
  the queue went on to the first rerun. The stage scripts now delete
  build files and harvested logs after each run, and the queue runs T1,
  KD, HB and the reruns again.

### T1 (2026-09-30, branch `plan27-t1`, a9dd7a2)

- A clock move gets a hard limit of two shares (a share is remaining / 20
  plus increment), at most half the clock. Iterative deepening starts no
  new depth after half the time to the limit. Before, the deadline cut
  the last depth and its time was lost. Movetime keeps its limit.
- Smoke test at 10+0.1: depth 14 in 812 ms, limit 1195 ms.
- SPRT grand `0 5` against p1: H0, -34.6 ± 15.6 (478W 618L 311D). No
  losses on time. Cause: a depth that started before half the window
  could run to two shares, so a move used more than one share on
  average and the clock ran low later.
- T1b (c47125c): no new depth after a quarter of the window (half a
  share), same limit of two shares. SPRT grand `0 5` against p1: H1,
  +105.8 ± 28.0. Time is generic, so the FSF ratings of the merged build
  check the other variants.

### Merged build (2026-09-30, branch `plan27-main`, 529e9e4)

- p1 + T1b + GS + SO. KD, HB, E2, C1 are not in it; HL waits for its
  SPRT. Rated against FSF 2000 in xiangqi, shogi, grand and standard.
- Xiangqi H0, -39.2 ± 22.8 (about 1961, from 1926). Shogi H0, -132.4 ±
  47.2 (about 1868, no change). Grand H0, -125.4 ± 44.8 (about 1875, from
  1800). Standard H1, +51.6 ± 26.5 (about 2050, from about 2019).

### Merged build 2 (2026-09-30, branch `plan27-main2`, ac05b42)

- The merged build plus HL, CS and SP. HL and CS each ended their gain arm
  without H1 but with a lower end above 0 (+1.2, +2.4). The build goes to
  the FSF 2000 ratings in xiangqi, grand, shogi and standard, which also
  check it against a loss. Perft unchanged.
- FSF 2000: xiangqi H1 +26.0 ± 18.5 (about 2026, passes), standard H1
  +41.8 ± 23.5 (about 2042, passes), grand H0 -102.5 ± 39.5 (about
  1898), shogi H0 -148.0 ± 51.2 (about 1852). Merged build 2 is the new
  base.

### DP (2026-09-30, branch `plan27-dp`, 23771d3)

- Drops no longer clear SEE pruning and the recapture order. With drops,
  each capture of an exchange also fills the hand of the taker, so every
  step is worth two times as much; the sum doubles and keeps its sign.
- Shogi, depth 11, four positions: nodes -14%, time equal.
- SPRT shogi `0 5` against merged build 2: H1, +88.3 ± 25.0. Only drop
  variants change.

### GP (2026-09-30, branch `plan27-gp`, c40c014)

- `derive_promotion_target` replaces the two copies of "best piece". With
  `promote to captured`, a pawn can become only a type that was captured;
  the pool is empty at the start, so only the cheapest target (not royal,
  cannot promote itself) is sure. The PST gradient and the passed pawn
  term use it. Params made again; only grand and sittuyin change.
- Correlation with FSF, grand: 0.844 to 0.893 (opening 0.561 to 0.621).
- SPRT grand `0 5` against merged build 2: inconclusive at 3000 games,
  +17.3 ± 11.5 (lower end +5.8).

### Merged build 3 (2026-09-30, branch `plan27-main3`, adbef5b)

- Merged build 2 plus DP and GP. DP changes only drop variants, GP only
  variants with promotion to a captured type, so xiangqi and standard
  keep their build 2 ratings. Rated against FSF 2000 in shogi and grand.
- Shogi: H0, -99.0 ± 38.6 (about 1901, from 1852). Grand: H0, -65.8 ±
  30.0 (about 1934, from 1898).

### T2 (2026-09-30, branch `plan27-t2`, 7379833)

- Without `movestogo`, the clock plans for 15 moves, not 20. T1b gave
  grand +105.8, so time use is a large lever; FSF also spends more per
  move early.
- SPRT grand `0 5` against merged build 3: H0, -36.0 ± 16.0. More time
  early hurts.
- T3 (28c6162): 30 moves, the other way. SPRT grand `0 5` against merged
  build 3: inconclusive at 3000 games, +15.3 ± 10.5 (lower end +4.8).
- T4 (44a0118): 40 moves, further the same way. SPRT grand `0 5` against
  merged build 3.

### PX (2026-09-30, branch `plan27-px`, d6b4d99)

- `royal_proximity!`: each enemy piece (not royal) within two files and
  two ranks of a royal costs half the most valuable piece, in drop
  variants only (derivation gives 0 otherwise, and the count is skipped).
  In both halves of the eval. With drops, a piece in hand joins such a
  piece at once.
- The earlier fit: the nearby enemy count is the largest feature missing
  from our shogi eval; it helps shogi and xiangqi and hurts grand and
  standard, so it is tied to the drop rule.
- Shogi: correlation with FSF 0.741 to 0.796; depth 11, four positions,
  641 ms to 430 ms.
- SPRT shogi `0 5` against merged build 3: H0, -305.9 ± 84.1. At half the
  dearest piece (about four pawns) for each nearby piece, the term rules
  the eval: the search values a piece next to the royal even when it
  hangs. A better fit to FSF's static eval is not a better engine.
- PX2 (de70e48): a tenth of the dearest piece. Correlation 0.759. SPRT
  shogi `0 5` against merged build 3: inconclusive at 3000 games, +16.3
  ± 12.4 (lower end +3.9).

### Merged build 4 (2026-09-30, branch `plan27-main4`, e1eeb09)

- Merged build 3 plus PX2. PX2 changes only drop variants.
- Not rated yet. Rule from now on: rate against FSF 2000 only when the
  self-play gains stacked since the last rated build cover the gap two
  times (self-play gains shrink about half against FSF): shogi needs
  about +200, grand about +130 over merged build 3.

### PI (2026-09-30, branch `plan27-pi`, 5e6f173)

- Bug: a shogi pawn captures and steps on its own file, so its only
  support offset was 0. Two own pawns never share a file there, so every
  shogi pawn on the board paid the isolated penalty, and a pawn in hand
  did not.
- Fix: the own file is the path, not a neighbour, so offset 0 is not a
  support file; a pawn type without support files is never isolated.
  Standard and Berolina keep {-1, +1}.
- Correlation with FSF unchanged (shogi 0.756 to 0.753). SPRT shogi `0 5`
  against merged build 4.
- Shogi: H0, -30.1 ± 14.7. The penalty on every board pawn works as a
  preference for a pawn in hand. Not merged.

### PT (2026-09-30, branch `plan27-pt`, 393db12)

- Bug: `derive_promotion_target` gave every piece the most valuable
  non-royal piece of the variant as its promotion. A shogi pawn becomes a
  tokin, not a dragon. Now it reads the `promotions` list of the piece:
  the best of its own targets, or the cheapest with `promote to
  captured`.
- 13 param files change (the shogi family and others with limited
  promotion); standard, xiangqi, grand, crazyhouse do not. Perft
  unchanged. Shogi correlation 0.759 to 0.764.
- SPRT shogi `0 5` against merged build 4.

### PV (2026-09-30, branch `plan27-pv2` on merged build 4, 1c425b7)

- Hint from the user: shogi pieces are too cheap. Hand values (bare kings,
  one piece in hand, pawn = 1) with the shift map: lance 1.5, knight 1.2,
  silver 3.2, gold 3.7, bishop 5.0, rook 8.8; Kaufman 4, 5, 7, 8, 11, 13.
  Standard with the shift: N 3.1, Q 9.8, against FSF's own tables N 4.1
  to 6.2, Q 12.9 to 20.
- The plain ratio (`raw / cheapest`) is too wide: standard N 8.1, Q 29.5
  (the pawn is small after SP); shogi rook 48.
- `value = 100 * (raw / cheapest)^0.7`: standard N 4.97, B 5.71, R 7.38,
  Q 12.47; shogi hand L 2.6, N 1.6, S 6.4, G 7.3, B 9.6, R 15; xiangqi
  R 5.23, C 3.17, N 2.75 (was 5.4, 3.0, 2.5); grand against the knight B
  1.24, R 1.59, Q 2.78, C 2.51, A 2.17 (FSF 1.26, 1.51, 3.23, 2.62, 2.49).
  The shogi knight stays low; the move model does not see its value.
- SPRT shogi `0 5` against merged build 4, then standard, xiangqi, grand
  `-5 0` so no other variant gets overvalued pieces.
- Cancelled before a verdict for the value model of plan 28.

### Shogi eval against FSF, board features (2026-09-30)

- A fit of FSF `eval` on our eval plus features read from the FEN: R2
  0.545 to 0.738. The largest is the net count of enemy pieces within two
  squares of each royal (about +300 cp each), then hand material (about
  +52 cp for each pawn unit).
- A linear royal danger cost (in place of the square) moves the
  correlation little: shogi +0.005, xiangqi +0.013, grand +0.014,
  standard +0.003.
- Our eval plus w times the nearby enemy count: correlation rises with w
  in shogi (0.699 to 0.760) and xiangqi (0.745 to 0.800) and falls in
  grand and standard. No single weight fits all, so it is not used.

### Piece value map (2026-09-30, not kept)

- Tried `value = 100 * (raw / cheapest)^0.8` in place of the shift. The
  pawn ratios move toward FSF, but the ratios between pieces get worse.
- FSF removal test, piece against piece (the pawn removal is too noisy to
  use): standard Q/R 2.19 (ours 1.69), grand Q/R 2.12 (1.73), grand A/R
  1.65 (1.25), xiangqi R/C 1.58 (1.04).
- The value adds the mobility of each move type, so a compound piece
  (Q = R + B, A = B + N) is worth only its parts. FSF prices compounds
  above the sum. This needs a change to the value model, not the map.

### CS (2026-09-30, branch `plan27-cs`, db54e03)

- `derive_family_mobility` splits the vectors whose last leg moves and
  takes into three direction families (orthogonal, diagonal, oblique). A
  piece gets half the mobility of all families but its largest on top:
  two sets of lines that one enemy piece cannot both avoid.
- Pawns get nothing (their step and their capture are single-purpose).
  Xiangqi has no piece with two families; its params do not change.
- Removal test against FSF: standard Q/R 1.69 to 2.01 (FSF 1.99 to
  2.19); grand Q/R 1.73 to 2.12 (2.12), C/R 1.55 to 1.84 (1.74 to 1.94),
  A/R 1.25 to 1.52 (1.62 to 1.65); shogi, rook = 1, silver 0.43 to 0.47
  (0.49), gold 0.49 to 0.55 (0.59). Perft unchanged.
- SPRT grand `0 5` against the merged build; then standard and shogi
  `-5 0`.
- Grand: inconclusive at 3000 games, +13.8 ± 11.4 (lower end +2.4). No
  H1; the regression arms did not run.

### SP (2026-09-30, branch `plan27-sp` on CS, c595836)

- A move-only or capture-only last leg counts half in the vector chance:
  the piece moves to a square it cannot guard, or guards a square it
  cannot move to. Params made again.
- Xiangqi removal test: chariot/cannon 1.04 to 1.81, cannon/horse 2.08 to
  1.19 (FSF classical values: about 2.1 and 1.1). Standard: N 3.09, B
  3.68, R 4.96, Q 9.81 against the pawn. Perft unchanged.
- SPRT xiangqi `0 5` against the merged build (CS does not change
  xiangqi, so the arm measures SP alone).
- Xiangqi: H1, +178.4 ± 42.1. The cannon priced as a chariot cost the
  most of all stages. SP is in merged build 2.

### D1 (2026-09-30, branch `plan27-d1`, 258964d)

- A drop that gives check is played at the first quiescence ply, like a
  quiet board move that gives check. X1 generated the drops there and
  skipped them.
- Shogi, depth 10, four positions: nodes equal or fewer.
- SPRT shogi `0 5` against the merged build: inconclusive at 3000 games,
  +0.9 ± 12.4. No effect; not merged.

### T4 result (2026-10-01)

- Grand `0 5` against merged build 3: inconclusive at 3000 games, +6.6 ±
  10.6. Not merged; the horizon stays at 20 moves.

### Merged build 5 (2026-10-01, branch `plan27-main5`, 90da58c)

- Merged build 4 plus PT and the ASEAN config fix (plan 28). All 44
  param files derive the same as the committed ones.
- Rating sweep queued on the server: every variant that FSF plays (36),
  400 games each against FSF 2000. The harness sends our variant name to
  both engines, so the sweep gives FSF its own name with `--option-b
  UCI_Variant=...` (5check, kingofthehill, losalamos, cambodian, 3check).
  Without it FSF plays chess in those five.

### Robustness (2026-10-01, branch `plan27-sec` on merged build 5, 285ff67)

- Self-play, 16 games at 2+0.02 in each of the 44 variants: no crash, no
  illegal move, no forfeit, except four.
- Chu, dai, dai dai and taikyoku shogi panicked when a UCI GUI selected
  them (`Missing mandatory sections: uci moves`). `split_sections`
  dropped empty sections and then paired titles with bodies by position,
  and these four dicts end with an empty `= uci moves =`. An empty
  section in the middle of a file would have moved every later body to
  the wrong title. Now only the text before the first title is dropped;
  an empty section keeps its title. All 44 param files are the same; chu
  and dai shogi answer `bestmove`.
- Taikyoku shogi takes 10.4 s from `setoption` to `readyok` (36x36, 402
  pieces), past the 10 s handshake of the harness. A GUI with a longer
  wait plays it.
- Start positions against FSF `d`: all agree (hand and letter spelling
  aside) except Capablanca, which had `RANBQKBNCR` for `RNABQKBCNR`.
  Fixed on branch `plan27-capa` (1e15f11); perft equals FSF to depth 4.
  The harness gives both engines our start FEN, so the sweep games below
  are valid games from the wrong setup.

### Sweep of merged build 5 against FSF 2000 (2026-10-01)

400 games per variant at 10+0.1, `-5 5`, Elo of our engine. A result
marked "invalid" came from a rules or protocol fault, fixed below.

| variant       | Elo            | variant       | Elo                   |
| ------------- | -------------- | ------------- | --------------------- |
| ai-wok        | +175.2 ± 58.8  | kinglet       | invalid (rules)       |
| makruk        | +139.2 ± 48.1  | pocketknight  | invalid (400-0)       |
| embassy       | +135.1 ± 47.9  | threecheck    | invalid (400-0)       |
| almost        | +109.5 ± 41.2  | fivecheck     | invalid (400-0)       |
| janus         | +106.8 ± 40.3  | sittuyin      | invalid (400-0)       |
| minixiangqi   | +101.0 ± 39.1  | newzealand    | invalid (400-0)       |
| standard      | +94.8 ± 37.5   | janggi        | invalid (setup phase) |
| chigorin      | +93.4 ± 37.2   | ouk-chaktrang | 889 ± 850 (rules)     |
| los-alamos    | +69.9 ± 31.1   | horde         | 585 ± 1006 (rules)    |
| amazon        | +49.0 ± 28.2   | hoppelpoppel  | -27.9 ± 33.9          |
| knightmate    | +49.0 ± 31.3   | grand         | -61.4 ± 34.8          |
| xiangqi       | +30.5 ± 29.2   | chancellor    | -84.5 ± 34.8          |
| minishogi     | +25.6 ± 18.4   | modern        | -87.5 ± 35.6          |
| euroshogi     | +20.9 ± 34.8   | Capablanca    | -90.4 ± 36.1 (setup)  |
| judkins       | +19.6 ± 15.6   | shogi         | -96.6 ± 38.0          |
| shatranj      | 42-4-1, H1     | crazyhouse    | -127.9 ± 45.5         |
| asean         | 44-0-2, H1     | gothic        | -142.0 ± 49.1         |
|               |                | extinction    | -167.3 ± 56.6         |
|               |                | koth          | -246.2 ± 86.2         |

- How to read it: an SPRT that stops early gives only its verdict, not a
  rating. Ouk chaktrang 889 ± 850, horde 585 ± 1006, shatranj, ASEAN and
  koth -246 ± 86 stopped after a few pairs: above (H1) or below (H0)
  2000, Elo unknown.
- The "invalid (400-0)" rows printed `2400.0 +/- 0.0` and "inconclusive":
  when all pairs score the same, the variance was zero, the ratio stayed
  at 0 and the run could not decide. The harness now gives each empty
  pentanomial bucket the weight of half a pair (3e51894), so such a run
  accepts H1 and the interval is finite.
- Embassy and janus are invalid too: their castling differed from FSF
  (fixed below). Re-rated: embassy -139.2 ± 48.8, janus -80.8 ± 34.1.

### Rules and protocol fixes (2026-10-01, branch `plan27-cfg`)

Found with perft at depth 1 to 5 against FSF from each start position and
from castling positions, and by replaying games through a logging wrapper
around FSF. Each fix makes perft equal FSF.

- Kinglet: the pawn had no double step and no en passant, there was no
  castling, and the pawn promoted to a queen; FSF promotes to the king
  (not royal). Castling moves put the royal piece in the main slot, so a
  variant without a royal could not castle. Now the leader is the first
  piece of its colour in the `pieces:` line, and the attack test on the
  castling path runs only for a royal leader (a piece that is not royal
  is never in check).
- Threecheck and fivecheck: FSF reads a FEN without a check counter as
  `1+1`, so it played a one-check game. The dicts write `3+3` / `5+5` and
  strip the counter on input.
- Pocketknight: the pocket knight is `O` in our FEN; FSF knows only `N`.
  The dict writes `N` and `N@`, and reads a hand `N` as `O`.
- Sittuyin: black drops were lowercase (`s@d6`); UCI drops use uppercase.
- Embassy: the king castles three squares (h1, b1). Janus: to i1 and b1.
- New Zealand: the rook captures only as a knight and the knight only as
  a rook (the config also let them capture their own way).
- Ouk chaktrang: the first king leap goes only to the wide forward
  squares (b2, f2), `i<[2]Kn[2]K>|i<[8]Kn[8]K>`. Perft equal to depth 5.
- Not fixed: horde lets any unmoved pawn double-step; FSF allows it only
  from ranks 1 and 2. CPMN has no rank condition. Janggi starts with a
  setup phase; FSF starts from a fixed setup, so it cannot be rated.

### GZ (2026-10-01, branch `plan27-gz` on merged build 5)

- Koth lost 89 of 107 games, nearly all to the enemy king walking onto
  the centre. A `goal` rule removed SEE, forward, null and quiet pruning
  and recapture ordering, so koth searched to depth 9 where standard
  reaches 13 in 3 s.
- Now the goal rule keeps them all. A move of a goal piece is never
  pruned or reduced, as a checking move. Koth reaches depth 14 in 3 s.
- SPRT koth `0 5` against merged build 5, then the sweep again.

### Merged build 6 (2026-10-01, branch `plan27-main6`)

- Merged build 5 plus the section fix, the rules fixes and GZ. All 44
  param files derive the same as the committed ones.
- Queued: koth `0 5` against merged build 5, then 400 games against FSF
  2000 in koth and the ten variants with fixed rules.
- The koth arm against merged build 5 wrote to the same folder as the
  rating that followed, so its verdict is lost.
- Against FSF 2000 (400 games, `-5 5`): koth H0 and threecheck H0 (both
  stopped after 26 pairs, so -391 ± 256 is no rating), fivecheck -190.8 ±
  64.5, pocketknight -144.8 ± 50.3, New Zealand -61.4 ± 32.0, sittuyin H1
  (early stop), kinglet +23.5 ± 30.9, Capablanca -109.6 ± 40.8, embassy
  -139.2 ± 48.8, janus -80.8 ± 34.1, ouk chaktrang H1 (early stop).

### VP (2026-10-01, branch `plan27-vp` on merged build 6)

- Extinction lost 121 of 171 to FSF, often after an early king walk
  (Kd3, Kc3, Kb3, Ka4). The king there is not royal: its capture ends the
  game through the `extinct` rule, but no royal term read it, and its
  table sent it to the centre.
- A vital piece is not royal but is the only piece of its colour in a set
  of an `extinct` rule that loses at zero (extinction: king and queen).
  The shelter, guard, danger, proximity and open-file terms read the
  vital pieces after the royals, and the opening table keeps them home
  like a royal. Only the extinction params change.
- SPRT extinction `0 5` against merged build 6.
- Extinction: H0, -146.7 ± 36.1. Not merged. Likely cause: the queen is
  vital too, so it stays home and fears every attacker; the king walk
  needs a narrower rule.

### Large boards (2026-10-01)

- With the fixed castling, embassy is -139.2 ± 48.8 and janus -80.8 ±
  34.1 (the earlier +135 and +107 were games where FSF played a
  different castling). Capablanca with the right setup: -109.6 ± 40.8.
  Every chess-family variant larger than 8x8 is below 2000 (Capablanca,
  embassy, gothic, janus, chancellor, modern, grand); the 8x8 ones pass.
- Depth in 4 s, ours against FSF (classical eval): standard 16/19,
  Capablanca 14/20, embassy 11/19, chancellor 14/19, janus 12/18. The
  nodes per second ratio is the same on all boards (0.6 to 0.7), so the
  gap is breadth. Nodes to depth 12, ours / FSF: standard 1.2, embassy
  7.1, chancellor 6.4, janus 2.6, gothic 2.2, Capablanca 0.8.
- The pruning counts and reduction surfaces do not depend on the board.
  Hypothesis: the compound pieces give many checks, and S1/X1 never
  prune or reduce a check and extend it.

### GR (2026-10-01, branch `plan27-gr` on merged build 6)

- GZ gave depth but koth still lost at FSF 2000: by move 15 FSF scored
  +4276 with its king two steps from the centre, and we saw -283.
- A goal race term: the goal piece of a colour nearest to the goal zone
  is worth the most valuable piece at one king step, half of it at two,
  a quarter at three. The lost position now reads -1173.
- SPRT koth `0 5` against merged build 6.
- Koth: inconclusive at 3000 games, -5.8 ± 11.9. Not merged. Three steps
  out the term is already a quarter of the dearest piece, so each early
  king step gains about 300 and the king leaves its shelter.

### CC (2026-10-01, branch `plan27-cc` on merged build 6)

- A `checks` rule removed the same pruning as a goal rule: threecheck
  searched to depth 9 where standard reaches 14. A checking move is never
  pruned or reduced (S1), so the rule now keeps all pruning; depth 17.
- A check race term: the checks a colour has given are worth the most
  valuable piece when one check is left to win, half with two, a quarter
  with three. After Bxf7+ Kxf7 the score is +12, was -284.
- SPRT threecheck `0 5` against merged build 6.
- Threecheck: H1, +248.3 ± 60.6. Merged into merged build 7 (branch
  `plan27-main7`). Queued: fivecheck `-5 0`, then merged build 7 against
  FSF 2000 in threecheck and fivecheck.

### VP2 (2026-10-01, branch `plan27-vp2` on VP)

- Only the cheapest vital piece of a colour stands in for the royal
  (extinction: the king; the queen stays active). SPRT extinction `0 5`
  against merged build 6.

## Verification

- Each stage: SPRT against the previous stage, one arm for each benchmark
  variant, one arm at a time.
  `debug-headless sprt <variant> ./new ./old 10000+100 3000 0 5
  --concurrency 8 --option-a Hash=64 --option-b Hash=64`. The arm of the
  variant that found the defect uses `0 5`. The other arms use `-5 5` to find
  regressions. Nothing else runs on the machine during an SPRT.
- S1 and X1: the reproductions and the 100-position samples
  (`scratchpad/run/fatal_<variant>.txt`), with
  `debug-headless search <variant> <depth> --protocol uci --fen ... --moves
  ...`. `debug-headless perft` is unchanged.
- P1: the removal-test table against FSF `eval`.
- E1: the eval sweep (`scratchpad/run/evalsweep/`).
- The debug build compiles clean. `cargo fmt` is never run. The style guide
  applies (80 columns, comments at column 81).
