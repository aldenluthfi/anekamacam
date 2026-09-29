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
