# Lessons from Fairy-Stockfish

## Status

Naming (2026-10-08): "full FSF" in this record means FSF without
`UCI_Elo` or `UCI_LimitStrength`, so each move is its best by its own
search. In our-engine tests it plays on a node budget (`nodestime`: 14
standard, 16 xiangqi, 29 grand, 44 shogi), where we score near 50%.
FSF's own speed on the loaded server (30 games, `info nps` from the
start position, 2 s) is 518 nodes a ms in standard, 232 in xiangqi,
400 in grand and 390 in shogi, so the budgets are 1/37, 1/14.5, 1/14
and 1/9 of its effort on the same machine (an earlier note took 1400
nodes a ms, from a faster machine, and gave 1/100 to 1/30). A gain there is real
against FSF's own choices, but at a fraction of its effort. The distance
to FSF at full effort is the ladder: the budget at which we score 50%
must rise. Lab runs (FSF against FSF) use no budget at all.

Archive (2026-10-08): the plan 29c result files (lab map, CR3, CR3b, H1,
H2, H3, M3, gate D, H2b, M3c, H1b) are in
`../anekamacam-vps-archive/p27b-plan29c/`; the finished clones (cr3,
cr3b, b15, h1, h2, h3, m3, m3b, h2b, m3c, h1b) are in
`../anekamacam-vps-archive/clones/aneka-<name>.tar.gz`, without builds.

Plan 29c (2026-10-06): an Elo map of FSF, one feature off at a time
(queue121). The plan 29b Phase 3 order and gate A are withdrawn; see
the retrospective.

Merged build 15 (2026-10-06) = build 14 + CK (same nodes, NPS +2% to +8%).

Merged build 14 (2026-10-06) = build 13 + SY. Against FSF 2000 (1000
games each, build 13 in brackets): knightmate +31.4 (-12.9), kinglet
+77.7 (+40.5); shogi 2000 games -63.3 (-63.9).

Merged build 13 (2026-10-05) = build 12 + RZ. Build 12 = build 11 +
F1, F2, M2 + HMF + the plan29-meas harness. HMF: crazyhouse self-play
H1 +60 (near +11 against FSF). RZ: shogi +29 against FSF (2000 games
each), near 0 in self-play. Key variants also play FSF from here.

## Context

Merged build 10 against FSF at UCI_Elo 2000 (400 games each, plan 27):
14 variants above, 6 uncertain, 15 below. Key variants: standard +66,
xiangqi +13, grand -50, shogi -90. The user cloned the FSF source to
`tmp/` and asked for a deep analysis: what we can take, what we cannot,
and whether each change fits the engine's philosophy. The engine must
play any variant well from its rules alone; data and tuning are the
last resort.

## What FSF at UCI_Elo 2000 is

- `search.cpp:367` maps the Elo to a skill level:
  `((2000 - 1346.6) / 143.4)^(1 / 0.806)` = 6.56. Each game plays at
  skill 6 or 7, picked at random with the fraction as chance.
- `Skill::time_to_pick` picks the move when the iteration depth is
  `1 + level`, that is depth 7 or 8. The search goes on to use its
  time, but the move stays (`search.cpp:519`, `:645`).
- MultiPV is at least 4. `pick_best` (`search.cpp:1919`) adds
  `(weakness * gap + delta * rand(weakness)) / 128` to each of the
  four moves, with weakness 106 or 108 and delta the gap from the first
  to the fourth move, at most a pawn (126 cp). The gap to the best move
  thus falls to about 16%, plus a random part of up to 0.83 delta.
  Moves within one pawn are close to a coin flip.
- The embedded NNUE is for `chess` and `chess960` only
  (`variant.cpp:50`; `Variant::init` clears `nnueAlias`). All other
  variants use the classical eval.

Thus in each variant below 2000, our full 10+0.1 search loses to the
classical FSF eval at depth 7 or 8 with about one pawn of noise. XS2
gave shogi 3 to 4 more plies and no Elo; plan 13 found that the search
micro-stages net about 0 and the eval stages carried the gain. The
lever is eval knowledge, not depth.

## FSF eval against ours

| Term | FSF | Ours |
| ---- | --- | ---- |
| Material, built-in pieces | tuned table (`types.h`) | derived from the rules |
| Material, custom pieces | `60 c + 30 q + 185 slider% + hoppers`, times `exp(v / 10000)` | derived |
| Board size | slider value grows with the board (`psqt.cpp:234`) | in the mobility |
| Drops | `v * 7000 / (7000 + v)`, then all eval values halved; SEE keeps full values | CP2 hand power |
| No checks allowed | values capped at 1800, then cut | none |
| PST | tuned chess tables; centre formulas for fairy pieces; king on the back rank, x2 with drops | derived |
| Hand | board value + (35, 10), x2 for a non-slider | hand value |
| Mobility | largest term; area without pawn-attacked squares; x2 with drops or check counting | none |
| Threats | hanging, attacked by a lesser piece, safe pawn threats, extinction threats | none |
| King safety | attack maps: ring attackers, safe checks for each piece type (drop squares too), weak squares, pins, flank; `danger^2 / 4096` | static zone table, squared |
| King safety, drops | hand attackers, weak ring x2, flank x6; scaled by material density, not phase | best drop pressure, `royal_proximity!` |
| Check counting | danger x `(1 + 7 / (3 + left))`; `3600 / left^2` | `check_race!` |
| Goal zone | flood fill to the zone; attacked squares held back; `4000 w / (w + d^2)`; both phases | `goal_race!`, Chebyshev steps, endgame only |
| Passed pawns | rank, king distance, free path, scaled by best promotion | static pawn structure |
| Imbalance | quadratic matrix of chess pieces | major, minor, pair |
| Scale factor | pawnless small leads, opposite bishops, KXK | draw contempt only |
| Grain | 16 cp; draw value +-1 | none |

## FSF search against ours

| Feature | FSF | Ours |
| ------- | --- | ---- |
| Null move | `(1090 + 81 d) / 256 + min((eval - beta) / 205, 3)`, verified at depth 14 | `4 + d / 4` |
| Singular, multicut | yes | removed (plans 13, 16) |
| Check extension | depth > 6 and `abs(eval) > 100` only | each check, line cap |
| Checks in LMR | reduced like other moves | never reduced (S1) |
| SEE prune, captures | `-218 d`, `-338 d` with drops | derived allowance |
| Ordering | capture history, countermove, low ply, four cont-hist plies | history, cont-hist, killers |
| Correction history | none | yes |

## Philosophy rubric

A stage must pass all five:

1. Rule-derived: it reads only compiled rules and derived values, never
   a variant or piece name.
2. General: it holds for any perfect-information, alternating game on a
   square board (no royal, many royals, no drops, hoppers, multi-leg
   pieces, chu lion).
3. No data: constants are fixed priors scaled by derived values.
4. Minimal: it reuses the existing primitives, with no dead code.
5. Bounded: the term cannot take over the eval. PX gave shogi H0 -306
   at half the dearest piece for each nearby enemy; a better fit to the
   FSF static eval was not a better engine.

## Ledger

| FSF idea | Tried | Result | Verdict |
| -------- | ----- | ------ | ------- |
| Never reduce or prune a check; extend checks | S1 + X1 | H1 in all four, shogi +78 | kept |
| Fewer check extensions | XS, XS2 | grand -17 stopped; shogi -2 | excluded: grand's long mates need sacrifice checks extended |
| Reduce checks in LMR, never prune them | never (S1b tested the reverse: no LMR, pruning kept, mate lost) | | stage CR |
| Quiet and drop checks at the first qsearch ply | X1, D1 | X1 kept; D1 +0.9 | done |
| Singular, multicut, double extension | plans 2, 13, 16 | rank -19, delta -5 ± 8 (plan 04), removed | excluded |
| Capture history | plan 4 G | -14, removed | excluded |
| Countermove | plan 17 K-5 | never run | not in this plan |
| Mobility | plans 3 U, 13 | 55% time to depth | excluded until attacks are cheap |
| Hanging pieces | never | LA: FSF threats about 0 where we lose | dropped (TH) |
| Safe checks from board pieces | never | LA: king safety is the gap in grand, capablanca | stage SB |
| More king danger | KD, grand | -10 | drop form only (DW) |
| Hand bonus | HB | shogi H0 -46 | excluded |
| Hand counted once in danger | E2 | -6 | excluded |
| Enemies near the royal, drops | PX, PX2 | -306; +16 merged | done |
| Drop value squeeze | DC, CP, CP2 | CP2 merged | done |
| Halved material with drops | never | | stage DW |
| Safe drop checks | never | | stage SD |
| Goal flood fill with attacks | GR (Chebyshev, both phases) | -6 | stage GA |
| Check-count king danger | never (CC is the check race) | | stage CD |
| Extinction threats | never | | stage EX |
| Scale factor | never | | excluded: no rule-derived mating set |
| Space, outposts, rook files, imbalance, KBNK, KPK | | | excluded: named pieces |
| NNUE | | | excluded: data |

## Stages

Each stage branches from merged build 10 (`plan27-main10`); a stage that
passes merges into merged build 11 (`plan27-main11`).

| Stage | Target | Change | Arms |
| ----- | ------ | ------ | ---- |
| SD | drops | safe drop checks add `safe_check_units` to `king_danger!` | shogi `0 5`, crazyhouse `-5 0` |
| DW | drops | danger, cap, shelter, guard x `DROP_SAFETY_RATIO` where captures return | shogi `0 5`, crazyhouse `-5 0` |
| LA | goal, wide | loss analysis against FSF, no code; gates GA, CD, EX, TH | |
| GA | koth | goal steps for each goal piece from its moves; attacked squares add a step; both phases | koth `0 5` |
| CD | n-check | danger and cap x `(10 + left) / (3 + left)` | threecheck `0 5`, fivecheck `-5 0` |
| EX | extinction | attacked vital pieces cost `value / left^2` | extinction `0 5`, kinglet, horde `-5 0` |
| ~~TH~~ | | dropped after LA: FSF threats are about 0 at the grand and capablanca fatal positions | |
| SB | royal variants | safe checks from board pieces, as SD | grand `0 5`, standard, xiangqi, shogi `-5 0` |
| TI | all | a new depth may start until half the time when the last depth changed the best move or lost more than the aspiration delta (a quarter otherwise) | shogi `0 5`, grand, standard, xiangqi `-5 0` |
| CR | checks | LMR may reduce a checking move; no pruning skips it, and its extension stays | shogi `0 5`, grand, xiangqi, standard `-5 0` |
| CS | checks | on CR, one ply more reduction for a check whose SEE is negative | as CR |
| FL | eval | flight squares of the royal with the SD2 safe drop checks, after CR | shogi `0 5`, crazyhouse `-5 0` |

Added after the first plan (2026-10-04, user discussion):

- Why SD scored better and played worse: its static eval agreed more
  with FSF's static eval (0.805 to 0.822), FSF's depth-12 search (0.641
  to 0.650) and the game result (0.292 to 0.307), and at a fixed depth
  of 8 its moves were 27 cp better by FSF depth 14. At 300 ms per move
  its moves were 47 cp worse, and its drops next to the enemy royal 348
  cp worse: they are refuted at depth 8 to 9, and we reach a median
  depth of 7 there (9 at 1 s, 10 at 3 s; FSF's nominal depth 14, 16,
  17). At 1 s the gap was +21 cp, at 3 s SD was 15 cp better.
- Why the tree is deep for checks: each checking move is exempt from
  pruning and reduction and extended one ply (S1, X1). S1 exists
  because our king danger is a static table blind to checks, so a
  pruned quiet check hid mates (xiangqi mate in 2 found at depth 3 with
  S1, past 8 without). The mates were lost to pruning (S1b kept
  pruning and lost the mate), not to reduction, so CR keeps the
  pruning exemption and lets LMR reduce: a reduced check that beats
  alpha is searched again at full depth.
- Gates for CR and CS: xiangqi mate in 2 at depth 3, grand mate in 7
  within the time the base needs (user choice: a time gate, as a
  reduction makes each ply cheaper), perft unchanged; then the
  move-choice screen at 300 ms.
- Measure first for CR (probe copy of merged build 10, midgame
  positions): checks are 12.8% of searched moves and their subtrees
  37.1% of nodes in shogi; grand 9.1% / 10.6%, xiangqi 12.1% / 12.8%,
  standard 9.8% / 12.2%. A shogi check costs about three times an
  average move.
- Note from plan 27: in the xiangqi mate in 2, LMR reduced the first
  move `f1h1` in the base of that time; checks exempt from LMR found it
  at depth 8 and S1 at depth 3. Reduction and pruning both delayed it.
- Move-choice screen: before each SPRT, compare the candidate's moves
  with the base at 300 ms per move on 400 game positions, judged by FSF
  at depth 14 (`scratchpad/la/movediff.py`). Correlation with FSF is a
  diagnostic only.

Departures from the first plan:

- DW: CP2's free-drop test is false in shogi (the pawn rule has a
  stopper), so it would miss the main target. Captures return when more
  than half of the droppable piece types have only free patterns: shogi
  13 of 14, crazyhouse 10 of 10, pocketknight 1 of 7 (no weight).
- DW: shelter, guard, danger and cap are stored in `res/param`, so the
  params of the six drop variants were regenerated (annanshogi,
  crazyhouse, euroshogi, judkins, minishogi, shogi).
- GA: LA puts 33 of the 40 koth fatal positions in our opening phase,
  where the endgame-only goal race (GR2) is zero. The new race is in
  both phases; the attack hold is what GR lacked.
- TH dropped and SB added: see LA.

Acceptance (user choice, hybrid): self-play SPRT under the plan 27
rules. After an H1, base and candidate each play 400 games against FSF
2000 in the target variants. The stage merges only if the candidate is
not below the base in each. A stage tied to a rule keeps the seeded
node counts of the variants without it. Perft unchanged, build
warning-free, params regenerated.

## Plan 29b: evidence first (2026-10-04)

After nine candidates with two passes, the user asked for a serious
analysis and no more futile attempts. Three read-only audits (search
and eval interaction, eval history, test method) found:

- **Biased measurement.** 30 games on 32 hyperthreaded cores, with wall
  time charged from before `position` (the move list replay included).
  Openings are 8 uniform random plies and often decided at once, so pair
  colour swaps reduce variance very little (grand pentanomial [214, 215,
  589, 227, 255]). No game length cap. Inconclusive merges were not
  calibrated.
- **Misleading diagnostics.** Correlation with FSF rose for every losing
  term. The 400-position move screen has about ±25 cp of noise. Fixed
  depth hides the NPS cost (SD +27 cp at depth 8, -47 cp at 300 ms).
- **Wasted speed.** King danger, proximity, check race, goal race and
  extinction threat are computed twice per middlegame eval. Qsearch
  evaluates before its check test. `clear_search` reallocates the history
  tables every move. Qsearch ignores the stored TT eval.
- **Structural gaps.** Qsearch skips every quiet drop (drop checks too).
  Null move has no eval margin and no verification. Futility is not
  history-gated. All margins are fractions of the dearest piece while
  king danger can swing a whole one. Drop variants never leave OPENING.
- **Eval pattern.** Every losing term pays for a piece standing near the
  royal without checking that it is safe; the loss shrinks with the
  weight. Winners price a game-end rule or a slow structural fact.

Program (each step gated; nothing reaches an SPRT on a guess):

- **M, measurement.** Harness flags `--book FILE` (move lists, pairs in
  order) and `--max-plies N` (draw at N). A book of 8-12 random plies
  kept when FSF depth 12 says |eval| <= 60 cp (100 with drops). The
  replay cost is measured. A vs A at 30 slots with random openings and
  at 15 slots with the book; bench NPS at 1, 15 and 30 copies; SD2 and
  CD again under the repaired protocol. An inconclusive result no longer
  merges. (`--seed` is not needed: `ANEKAMACAM_SEED` seeds the harness,
  and the book fixes the openings.)
- **F, free speed.** Shared king and rule terms computed once (F1), no
  qsearch eval in check (F2), history tables kept allocated (F3), TT
  eval as stand pat (F4). Each must give identical seeded node counts
  and best moves on 192 positions, perft unchanged, NPS up under load.
- **D, diagnosis.** A node budget per variant from game logs (scaled by
  the candidate's NPS), fatal and good position sets from build 11's
  FSF games labelled at FSF depth 16, probe switches
  (`ANEKAMACAM_PROBE`), and a score of fixed, broken and net against a
  measured noise floor σ0. The predictor is calibrated on SD, SD2, SB,
  CR, DW, CD, GA, EX, PX, PX2 and CC before it is trusted. A census of
  removal switches ranks the mechanisms.
- **C, candidates** (null-move margin and verification, history-gated
  futility and LMP, qsearch drop checks with the king term, board-only
  phase with drops, drop tempo, kept history), in census order. Each
  gets one SPRT only after it passes the D gate.

## Results

### Referee audit (2026-10-04)

The SPRT harness is its own referee (`src/debug/sprt.rs`): a move that
does not parse or that `make_move!` refuses loses for the side that
played it, by our rules. The log line is deleted after each run, so the
audit reads the game files (`scratchpad/illegal.py`): a game whose last
mover scores 0 on a non-mate move ended on an illegal move, a time loss
or a rule that makes the mover lose (perpetual check or chase).

- Self-play (both sides ours, same rules): rare and even between A and
  B, all very late (move 300 to 1800) in shogi, xiangqi and grand. These
  are time losses and perpetual rules. The self-play SPRTs are not biased.
- Merged build 10 against FSF 2000: our engine never lost this way. FSF
  did in six variants. Their ratings are not valid:

| variant | FSF games lost on own move | rating as swept | without those games |
| ------- | -------------------------- | --------------- | ------------------- |
| horde | 352 of 400 | +376.9 | -307.1 (48 games) |
| ouk-chaktrang | 330 of 400 | +546.5 | +204.3 (70 games) |
| sittuyin | 193 of 400 | +187.4 | +6.7 (207 games) |
| asean | 23 of 400 | +386.6 | +375.1 |
| chigorin | 20 of 400 | +120.1 | +106.5 |
| xiangqi | 10 of 400 | +13.9 | +5.3 |

- Removing the games biases the result (FSF did not lose them on the
  board), so the right column is an estimate only.
- Causes by example: horde `g6f7` (double step rule), ouk `g8e7`,
  sittuyin `f6f6f` (promotion in place), asean `f7f8r` and chigorin
  `h2h1c` (promotion choice), xiangqi moves FSF scored `cp 0` that our
  perpetual rule calls a chase or check loss. Earlier sweeps (merged
  builds 5 to 8) also had this in embassy, janus, kinglet, newzealand,
  pocketknight, janggi; the config and dict fixes of plan 27 cleared
  those in build 10.
- Fixes (branch `plan29-ref`), each checked with a local match against
  FSF and the FSF input captured (`scratchpad/ouktest`):
  - horde: a pawn double steps only from the first two ranks
    (`m<nW-pnW>@@@sW{2}~*?`, no first-move flag). Perft now equals FSF
    to depth 5 (265223). Local match: 0 forfeits, 4W 12L.
  - asean: the dict lowercased only `=Q`; FSF and we disagreed on
    `f7f8r`. Local match: 0 forfeits in 32 games.
  - ouk-chaktrang: FSF keeps the king leap and met jump as gates from
    the FEN castling field; the start FEN now sends `DEde`. Promotion is
    on rank 6, so `=M -> m` replaces `8=M`. Forfeits fell from 330 of 400
    to 4 of 32. The rest: FSF drops the leap when a rook aims at the
    king and drops the rights when the king moves; our rules keep them,
    so our leap desyncs FSF. Not fixed.
  - The SPRT result file now has a `forfeits:` line (illegal move, time)
    for each engine.
- Rated again on the fixed build (`ref`, 400 games, bounds `0 0`):
  horde -99.2 ± 27.2 (FSF forfeits 2 illegal, 2 time), asean +328.3 ±
  40.7 (no forfeits), ouk-chaktrang +144.7 ± 21.8 but FSF forfeits 3
  illegal and 39 on time, so ouk is still not valid.
- FSF time losses (ouk, local capture of 16 games): the harness charges
  FSF what FSF reports (median difference -1 ms over 1249 moves), so the
  clock is fair. FSF sometimes thinks 1.5 to 1.8 s early, then lives on
  the 100 ms increment with its default 10 ms `Move Overhead`; with 8 to
  30 games at once, the pipe and the scheduler take more than that, and
  FSF flags. Ouk games are long (median 174 plies), so ouk shows it
  most. With `Move Overhead=100` for FSF: 0 time losses in 32 games. The
  rating script (`~/p27b/sweep2.sh`) now sets it for FSF from here on.
- Not fixed: sittuyin (FSF lets the last pawn promote anywhere; our
  promotion zone has no pawn count), chigorin (FSF 14.0.1 lets both
  sides promote to any piece; its newer source does not), xiangqi
  (perpetual chase rules differ in 10 of 400 games).

### SD (2026-10-04, branch `plan29-sd`, 53e8bc5)

- Seeded node counts at depth 9: standard, xiangqi, grand the same as
  merged build 10; perft of shogi and crazyhouse the same.
- Cost on the four slow shogi positions at depth 8: NPS -11% to -18%
  (mean about -13%), inside the 15% gate. Scores: g341 +416 to -18,
  g176 -284 to -686 (FSF: mate), g74 +161 to +286, g19 +1067 to +1049.
- SPRT shogi `0 5` against merged build 10 (`final`): H0, -115.3 ±
  30.4 (538 games). The crazyhouse arm was stopped. Eval correlation
  with FSF on 250 shogi positions rose (0.802 to 0.831), so the term
  points the right way; at half a pressed zone for each piece type, a
  few pieces in hand push the danger to its cap.

### SD2 (2026-10-04, branch `plan29-sd2` on SD, 5f0f581)

- `SAFE_CHECK_RATIO` 500 to 125. Correlation 0.797 to 0.803.
- SPRT shogi `0 5` against merged build 10: H0, -32.5 ± 15.2. The loss
  shrinks with the weight, so the term itself hurts (and costs about 13%
  NPS). The crazyhouse arm was stopped. The safe drop check line ends.

### DW (2026-10-04, branch `plan29-dw`, 561b83b)

- Seeded node counts: standard, xiangqi, grand, pocketknight the same.
- Shogi slow positions at depth 8: g341 +416 to +265, g176 -284 to
  -379, g74 +161 to +194, g19 +1067 to +1075.
- SPRT shogi `0 5` against merged build 10: inconclusive at 3000 games,
  -8.6 ± 12.5. Crazyhouse `-5 0` stood at -118.3 ± 56.6 after 131 games
  and was stopped. Not merged: doubling the royal safety values does not
  help, as KD did not in grand.

### LA (2026-10-04)

40 fatal positions per variant from the merged build 10 games against
FSF 2000 (`scratchpad/la/la.py`). The position is the last one before
our score fell from -0.5 or better to -3 or worse. Means in pawns, side
to move = us; an FSF term is the mean of its MG and EG trace values.

| variant | our static | FSF static | FSF d12 | FSF king safety | FSF threats | FSF variant | FSF mobility |
| ------- | ---------- | ---------- | ------- | --------------- | ----------- | ----------- | ------------ |
| koth | +3.33 | -4.06 | -6.01 | -0.70 | -0.02 | **-4.23** | -0.55 |
| threecheck | +2.64 | -2.17 | -1.97 | **-2.01** | +0.01 | 0.00 | -0.61 |
| extinction | +1.49 | -2.15 | -8.23 | 0.00 | **-1.25** | -0.56 | -0.49 |
| shogi | +6.87 | -3.70 | -5.96 | **-4.25** | -0.10 | 0.00 | -0.19 |
| crazyhouse | +3.46 | -6.63 | -14.58 | **-6.88** | -0.09 | 0.00 | -0.47 |
| grand | +2.49 | -4.22 | -12.74 | **-2.16** | -0.14 | 0.00 | -0.48 |
| capablanca | +0.35 | -3.79 | -17.86 | **-1.71** | -0.01 | 0.00 | -0.76 |

- Our static eval is optimistic at all of them; FSF at depth 12 agrees
  with its static sign.
- Koth: the goal term is the gap (30 of 34 below -0.5); 33 of 40
  positions are in our opening phase. GA goes, in both phases.
- Threecheck: king safety is the gap. CD goes.
- Extinction: threats on vital pieces are the gap. EX goes.
- Shogi, crazyhouse: king safety is the gap. SD and DW aim there.
- Grand, capablanca: king safety is the gap, not threats. TH is
  dropped. SB (safe checks from board pieces) takes its place.

### CD (2026-10-04, branch `plan29-cd`, 9bea04f)

- Seeded node counts: standard, xiangqi, grand, shogi the same;
  threecheck 53117 to 62565, fivecheck 40694 to 29870.
- SPRT threecheck `0 5` against merged build 10: inconclusive at 3000
  games, +9.0 ± 10.7 (lower end -1.7). Not merged; the fivecheck arm
  was stopped.

### GA (2026-10-04, branch `plan29-ga`, ae69bbb)

- `Goal.steps` and `Goal.closer` for each goal piece, from its moves on
  an empty board. `goal_race!` adds one move for each step where all
  next squares are attacked or hold an own piece (within
  `GOAL_HOLD_STEPS` = 3), in both phases.
- Seeded node counts: standard, xiangqi, grand, shogi the same; koth
  49623 to 12692. Koth eval correlation with FSF (252 positions): 0.737
  to 0.900.
- SPRT koth `0 5` against merged build 10: H1, +137.4 ± 33.8.
- FSF check (400 games, FSF 2000): koth -109.6 ± 33.8 (build 10:
  -185). Merged into merged build 11.

### EX (2026-10-04, branch `plan29-ex`, fcf1122)

- `extinct_threat!`: each attacked set piece of a losing rule costs
  `threat / left^2` (`threat` = 25% of the dearest piece, on the rule),
  only with at most `EXTINCT_THREAT_LEFT` = 2 copies left. Both halves.
- Seeded node counts: standard, xiangqi, grand, shogi, kinglet, horde the
  same; extinction 93767 to 50090. Extinction eval correlation (291
  positions): 0.700 to 0.883.
- SPRT extinction `0 5` against merged build 10: H1, +184.3 ± 43.2.
  Kinglet `-5 0`: H1, +94.5 ± 26.8 (a gain there too).
- FSF check (400 games, FSF 2000): extinction -51.0 ± 32.1 (build 10:
  -151), kinglet +72.5 ± 33.9 (build 10: -6). Merged into merged build
  11.

### SB (2026-10-04, branch `plan29-sb`, 0184cb5)

- `safe_board_checks!`: for each enemy piece type on the board, the
  check squares (empty or own piece) that a piece of the type reaches
  with a vector that can move, and that the royal side does not attack.
  Same units as SD2.
- First form: NPS -70% to -80% at depth 10 (grand, standard, xiangqi
  midgame positions), far outside the gate. Now the test runs only when
  the zone pressure is at least `SAFE_CHECK_GATE` = 4 expected moves;
  NPS is then the same as merged build 10.
- Seeded node counts: standard, xiangqi, shogi the same; grand 424988 to
  478804. Correlation (164 grand positions): 0.850 to 0.852.
- SPRT grand `0 5` against merged build 10: H0, -31.3 ± 14.9. The
  regression arms were stopped. Same pattern as SD: check knowledge in
  the eval, with a search that plays every check out, loses. Retry only
  with CR.

### CR (2026-10-04, branch `plan29-cr`, 504e6e8)

- The LMR gate no longer exempts a checking move. Pruning still never
  skips one, and the check extension stays.
- Gates: xiangqi mate in 2 at depth 1 (base: 1). Grand mate in 7: base
  at depth 15 in 1.9 s (3.4M nodes to depth 16); CR at depth 18 in 1.5
  s (248k nodes to depth 16, where it scores -1693). Passes the time
  gate.
- Move-choice screen in shogi at 300 ms (94 of 400 positions differ):
  CR's moves 18 cp worse on average, inside the noise (about ± 25 cp).
  Board moves worse in 33 and better in 20; drops near the enemy royal
  better (worse in 1, better in 7). Not decisive, so the SPRT decides.
- Median depth on the 24 shogi midgame positions: 8 at 300 ms and 10.5
  at 1 s (base 7 and 9).
- SPRT shogi `0 5` against merged build 10: H0, -50.9 ± 19.0. The other
  arms were stopped. Not merged; CS (on CR) and FL (after CR) are
  dropped with it.
- Why, from the CS screen: the same reduction acts in our own tree on
  the enemy's checks. A mating threat against us then shows one or two
  plies late, and the defending move (in shogi, a drop next to our own
  royal) is skipped. The extra depth (+1 to 1.5 plies) does not pay for
  that. S1's full-depth checks thus protect the defender as much as the
  attacker. Ply parity gives the side of each check relative to the
  root, so a reduction can spare the defender: CR2 reduces only the root
  side's checks and keeps the enemy's checks at full depth. That tests
  the mechanism directly.

### CR2 (2026-10-04, branch `plan29-cr2` on CR, screen only)

- LMR may reduce a check of the root side (even ply), not of the
  opponent.
- Gates: xiangqi mate in 2 at depth 1; grand mate in 7 (a threat against
  the side to move) in 1.7 to 2.2 s, as the base (1.9 s).
- Median depth 7.5 at 300 ms, 9.5 at 1 s (base 7, 9; CR 8, 10.5).
- Move-choice screen in shogi at 300 ms (76 positions differ): 49 cp
  worse; it still skips the defending drop next to its own royal in 4
  positions (564 cp each), which give about 30 of the 49 cp.
- So the defender mechanism does not explain the CR loss: keeping the
  opponent's checks at full depth fails the same way. The cause is not
  known. With CR at H0 and a worse screen, CR2 gets no SPRT. The check
  reduction line ends here. Open question for later: what in the 4
  defending-drop positions makes every reduced form choose another move
  (a probe of those positions at fixed depth would show it).

### TI (2026-10-04, branch `plan29-ti`, 3e1a460)

- An unstable root (best move changed, or score fell by more than the
  aspiration delta) may start a new depth until half its time, a stable
  one until a quarter. Fixed-depth node counts unchanged.
- SPRT shogi `0 5` against merged build 10: inconclusive at 3000 games,
  -19.8 ± 12.3 (the whole interval below zero). The other arms were
  stopped. Not merged. Likely cause, as in E1 (more time early, -36 in
  grand): time spent on unstable moves comes out of later moves, and the
  clock has no reserve for it. Not measured.

### CS (2026-10-04, branch `plan29-cs` on CR, cf7c364)

- On CR, a check whose landing square the defender attacks is reduced
  one ply more (`is_square_attacked!` after the move).
- Gates: xiangqi mate in 2 at depth 1; grand mate in 7 at depth 18 in
  1.6 s (base 1.9 s). Median depth: 7 at 300 ms, 10 at 1 s, no more
  than CR.
- Move-choice screen in shogi at 300 ms (85 positions differ): 42 cp
  worse; board moves 83 cp worse. In the 5 positions where the base
  dropped a piece next to its own royal, CS played another move and lost
  744 cp each. Likely cause: in our own tree the enemy's checks are cut
  too, so a mating threat against us shows late and the defending drop
  is skipped. CR shows the same direction more weakly. CS waits: it runs
  only if CR passes.

### Merged build 11 (2026-10-04, branch `plan27-main11`)

- Merged build 10 + GA + EX + the referee fixes (`plan29-ref`: horde
  double step, asean and ouk dicts, forfeit counts in the result file).
- Seeded node counts at depth 9: koth equals GA, extinction equals EX,
  standard, xiangqi, grand, shogi and kinglet equal merged build 10.
  Perft at depth 3 unchanged in standard, shogi, xiangqi, grand,
  crazyhouse.

### Plan 29b M: harness (2026-10-04, branch `plan29-meas`, c377a7b)

- `--book FILE` (one move list per line in protocol notation; pair n
  plays line n, wrapping) and `--max-plies N` (a draw at N plies). The
  result file names the openings and the ply limit. Tested: a 2-line
  book with a 20-ply limit gives the book positions and capped draws; a
  shogi line with a promotion (`c3b4+`) plays.
- Book builder `scratchpad/book/mkbook.py`: each ply is drawn from the
  moves that both our engine (`perft 1 --protocol uci`) and FSF list, so
  every line is legal under both rule sets; kept when FSF depth 12 gives
  |eval| <= the limit. About 6 s of CPU per shogi line.
- A local run of the identity check failed on 5 of 192 positions because
  parallel engines in one working directory clash when they roll
  `logs/latest.log`; each run now gets its own directory. Parallel perft
  calls are not affected (40 of 40 correct).
- SD at 30+0.3 (600 games, bounds `0 0`): -89.1 ± 27.5 at 592 games. SD
  loses at three times the time per move too, so "it only lacks depth"
  does not hold. A calibration point for D5. Final: -85.0 ± 27.5 at 600
  games.

### Plan 29b M-a: A vs A, shogi, 30 slots, random openings (failed)

- Same binary on both sides, 2000 games: -22.3 ± 15.4 for A (the
  interval excludes 0; p about 0.5%). Pentanomial [283, 10, 480, 6,
  221]. Time forfeits 24 for each side (2.4% of games, above the 0.5%
  limit). **M-a fails on both conditions**, so stop rule 1 holds: no
  candidate SPRT until the harness is understood.
- By colour and order: A as White in the first game of a pair scores
  0.4545, A as Black in the second game 0.4815; White scores 0.4865 in
  all. B wins both halves, so colour is not the cause.
- By resources: median nodes per move A 21,684, B 21,507 (means 28,310
  and 28,114) over about 176,000 moves each. CPU share is not the cause.
- Open: chance (p about 0.5%) or a subtler harness effect. M-b (15
  slots, book) and M-a2 (an exact repeat of M-a) separate chance, load
  and openings.
- Side result for D1: the median search under 30-slot load is about
  21.7k nodes per move in shogi.
- If a bias of about -20 Elo against A is real, every candidate verdict
  since it began is shifted down by that much (SD2, DW, CD, TI, CR and
  others). The re-test plan of M-d then covers them.

### Plan 29b M-b: A vs A, shogi, 15 slots, book, 800-ply cap

- 2000 games: +17.7 ± 14.6 for A, pentanomial [199, 29, 490, 35, 247],
  no forfeits (the time losses of M-a are gone at 15 slots).
- So two A vs A runs fall outside their 95% intervals on opposite sides
  (M-a -22.3, M-b +17.7). For an unbiased harness with correct error
  bars that has a chance of about 0.1%.
- Checked and ruled out:
  - time drift: pair scores in game order show block variance 0.97 and
    0.68 of the independent value and lag-1 autocorrelation -0.03 and
    -0.02;
  - colour: the second game of a pair follows the stronger engine of the
    run in both runs (M-a: White 0.4545 then 0.5185; M-b: 0.5195 then
    0.4685);
  - CPU placement: in M-a the per-slot A/B ratio of median nodes per move
    is 0.96 to 1.05 (sd 2.3%), worth well under 5 Elo.
- Open: chance, or a per-process effect not yet found. M-a2 (an exact
  repeat of M-a) decides between them.

### Plan 29b M-a2 and M-c

- M-a2 (exact repeat of M-a): +8.7 ± 14.9, pentanomial [223, 8, 513, 8,
  248]. M-a's -22 does not repeat.
- The three A vs A runs (-22.3, +17.7, +8.7, each about ±15 at 95%)
  spread more than the stated error allows: the sum of squared z is
  15.3 on 3 degrees of freedom (p about 0.2%). The real error of a run
  looks about twice the reported one. The source is not known (no time
  drift, no per-slot speed gap, no colour effect). Two more A vs A runs
  under the repaired protocol will size this factor.
- Time forfeits (our engine on both sides): M-a 48, M-a2 46 (18 A, 28
  B), M-b 0. All 46 of M-a2 are in games of 840 to 3058 plies (median
  1640; the other games have a median of 125). Shogi has no move-count
  rule, so such a game runs on with each side living on the 100 ms
  increment, and the engine replays the whole move list before each move
  (a cost that grows with the game, charged to its clock). The 800-ply
  draw cap removes these games. An engine fix (reuse the board when the
  new move list extends the last one) is stage M2.
- M-c, one seeded depth-9 shogi search: 0.34 s alone, 0.52 s with 15
  copies, 0.68 s with 30. 30 copies are 31% slower than 15, above the
  25% limit.
- **Repaired protocol:** 15 slots, a balanced book, `--max-plies 800`,
  bounds `0 5` / `-5 0`; an inconclusive result never merges; the error
  bars are read as about twice their stated width until the calibration
  runs give a better factor.

### Plan 29b F1 (branch `plan29-f1`, 2a47f64) and F2 (`plan29-f2`, b32f0d4)

- F1: `shared_score!` computes king danger, proximity, check race, goal
  race and extinction threat once per eval and adds the same value to
  both halves before the blend, so the blend inputs are bit-equal.
- F2: quiescence does not evaluate a checked side (its stand pat is never
  read); the MAX_DEPTH return evaluates it then.
- Identity (`scratchpad/speed/verify.py`, seeded depth 9, 24 positions
  in each of standard, xiangqi, grand, shogi, crazyhouse, koth,
  extinction, threecheck): node counts and best moves identical for F1
  against merged build 11 and for F2 against F1.
- Speed: not measured yet; the local machine was busy with the book, so
  the timings are not valid. To be measured under game load on the
  server (M-c method).
- F3 note: `vec![0; n]` takes lazily zeroed pages, while `fill(0)` writes
  the whole table (about 20 MB in shogi) every move; which is faster has
  to be measured, so F3 waits for that. F4 note: the main search hands
  depth 0 to quiescence without an eval, so the TT seldom holds one for a
  q-node; F4 waits for the speed numbers.

### M2 (2026-10-04, branch `plan29-m2` on F2)

- `position` with a line that extends the current game (same root hash
  and ply, the history moves as the first tokens) plays only the new
  moves. No new field: the known moves come from `state.history`.
- Shogi, 150 plies, one `position` per ply: 118 ms in total before,
  17 ms after. Board (`d`) and seeded depth-7 search equal a fresh full
  replay every 25 plies in shogi, chess, xiangqi and crazyhouse.

### F3 (2026-10-04, not made)

- Each `go` makes a new `SearchInfo` in a new thread
  (`protocol.rs` go handler). To keep the tables between moves needs a
  new session field.
- Ceiling: `vec![0; n]` gets zero pages from the OS, and only touched
  pages fault. Shogi, 72 moves of `go nodes 200000` in one UCI
  session: 20.8 s, 28k to 29k minor faults in total, near 400 for each
  move. At 1 to 2 us for each fault, this is below 1 ms of 290 ms, so
  near 0.2%. The gate is 1%.
- `fill(0)` writes the full table each move. `cont_hist` is at the
  1 GB cap in taikyoku, chu and dai-dai shogi, so near 100 ms for each
  move there. Not made.

### F4 (2026-10-04, measured, reverted)

- Change: qsearch probes the main table before the stand pat and uses
  its raw eval when the entry has one. Node counts equal in bench at
  depth 10 in shogi, standard, xiangqi and grand.
- Hit rate, non-check qnodes with an eval in the main table: cold bench
  shogi 2.6%, standard 13.8%, xiangqi 3.5%, grand 1.9%. Warm UCI
  session, shogi, 71 moves: 4.0%.
- Cost: the probe, the repetition count and the key move in front of
  the stand pat cutoff, so each cutoff node pays them.
- Wall time, one UCI session for each variant, Hash 64, `go nodes
  200000` on the positions of one side of game 6 to 9 of the final
  sweep, 3 paired rounds (F4 against base, + is slower):
  - shogi +0.5, +1.4, -2.3 (mean -0.1%)
  - standard -0.7, +2.0, +1.4 (mean +0.9%)
  - xiangqi +3.6, -4.2, +1.3 (mean +0.2%)
  - grand -4.9, +1.3, +0.5 (mean -1.0%)
- The sign changes in each variant and no mean reaches 1%. An earlier
  shogi run on the positions of both sides gave -2.2%, but that run
  searches each ply, so the table is warmer than in a game. Reverted.

### D2 sets and M-d (2026-10-04, server)

- `sets.py` (D2) stopped for 22 min: 1998 of 2000 shogi labels were
  done, and two FSF `go depth 16` searches did not end. The script
  kept all labels in memory, so the queue waited. Now each search
  stops at depth 16 or after 60 s and keeps the depth it got. Each
  label goes to disk when it is done, and a restart skips the done
  labels. The two stopped positions were lost shogi ends with full
  hands (near -4300 cp), not in the fatal or good sets.
- D2 sets, 2000 labels each (fatal, good): shogi 156, 1208; grand 147,
  1296; xiangqi 95, 1375; standard 88, 1480. Grand 6 and xiangqi 7
  labels stopped at 60 s.
- The opening books (20 workers, near 8 to 10 h for 7 variants) stop
  with SIGSTOP while a timed run (M-d, A vs A, D1 budgets) plays, so
  those runs have a quiet machine. Queue79 and queue80 do this.
- M-d, SD2 against merged build 10, shogi, repaired protocol: H0,
  -56.7 ± 20.0. Same as the old H0 (-32.5 ± 15.2), so the past
  verdicts stand and DW and TI get no new run.
- D1: median nodes for each move in M-d (15 slots, both engines,
  156k moves): 27,497 in shogi.

### D3 (2026-10-04, branch `plan29-probe`, 528a504, never merged)

- `ANEKAMACAM_PROBE="name=v,..."` fills `PROBE`; `probe!(name, default)`
  reads it. Switches: `king_danger`, `proximity`, `open_shield`
  (scale), `s1`, `x1`, `null_move`, `futility`, `lmp`, `rfp`,
  `razoring` (0 = off).
- Without the variable, node counts equal plan29-m2 in shogi,
  standard, xiangqi, grand, crazyhouse (bench depth 9) and koth,
  extinction, threecheck, kinglet, fivecheck (start, depth 9). Each
  switch at 0 changes the shogi bench count.

### D5 (2026-10-04): the position predictor fails

- `ablate.py` on the shogi sets (156 fatal, 400 good), each branch
  against its parent at the same node count, 0.7, 1.0 and 1.4 × 27,497.
  `net` at 1.0 with its standard error over the positions:

  | Branch | SPRT Elo | net (cp) |
  | --- | --- | --- |
  | PX | -306 | -9.1 ± 4.3 |
  | SD | -115 | +2.1 ± 3.0 |
  | CR | -51 | -2.5 ± 1.7 |
  | SD2 | -33 | +0.1 ± 2.1 |
  | DW | -9 | -0.2 ± 2.9 |
  | PX2 | +16 | +3.0 ± 2.8 |
  | TI | -20 | 0, no move changes (time use only) |

- Spearman ρ 0.60 (gate 0.7). SD has the wrong sign at all three
  budgets (+2.1, +2.1, +2.7), and the gate needs SD negative. The
  1.4 retry fails too. `fixed - broken` is negative for all six, PX2
  too, so it does not separate them.
- The null perturbations do not work: nodes ±1% and +2% change no move
  (the interrupt test runs every 2048 nodes, and a stop inside an
  iteration keeps the last full one), and Hash 32 / 128 change 4 to 6.
  σ0 from them is near 0.05 cp. The position standard error (1.7 to
  4.4 cp) is the real noise.
- Why: each set position tests one move from a build 10 game. SD loses
  by drops that lead to positions build 10 did not reach, and the loss
  comes moves later. A one-move test off the candidate's own games does
  not see this. Only PX (-306) is large enough to show.
- As the plan says, a short screen replaces the predictor: 600 games
  at 5+0.05 with the book, 15 slots, calibrated on the same branches
  (queue81: SD, SD2, CR, DW, TI against build 10; PX, PX2 against
  build 3). The D1 budgets and the koth, extinction and threecheck
  labels were for the predictor only, so queue80 was stopped.

### D6 traced questions (2026-10-04)

- Q1, which gate drops the defending drop. Shogi set position 119
  (White to move, king e1; build 10 plays `S@d2`, FSF -30; CR, SD
  and SD2 play `d1d2`, -646). CR with the D3 switches (probe build,
  CR's eval), seeded `go nodes`:

  | Nodes | build 10 | CR | CR, `s1=0` |
  | --- | --- | --- | --- |
  | 20,000 | `S@d2`, depth 6 | `b8a9`, depth 4 | `d1d2`, depth 5 |
  | 27,497 | `S@d2`, depth 7 | `d1d2`, depth 5 | `S@d2`, depth 8 |
  | 40,000 | `S@d2`, depth 8 | `S@d2`, depth 7 | `S@d2`, depth 8 |

  No one gate drops it: with LMP, futility, null move, RFP or razoring
  off, CR gets less deep and still misses it. CR finds `S@d2` from
  depth 7, but reaches each depth later than build 10 here (a reduced
  check that fails high is searched again at full depth). Thus CR
  costs depth in check-heavy positions, where it was to gain depth.
- The position is also an eval gap. Our search gives White +12.7, our
  static eval +14.5 (White has 2 silvers and 2 pawns more). FSF static
  -9.85, FSF search -0.30: Black's horse on e3, knight on f5 and gold,
  silver and knight in hand attack the king on e1. `king_danger!`,
  `royal_proximity!` and `open_shield!` together give 16 cp there
  (all three off: 1450 to 1466 cp).
- Q2, can qsearch see the refutation of SD's bad drops. FSF depth 14
  lines after five SD and SD2 drops that lose 300 cp or more (set
  positions 5, 141, 377, 12, 84): `S@d4` is taken (`h8d4`), then the
  loss comes from drops `S@e3`, `S@f2`; `B@e1` gets a king move and
  `R@c9`; `S@f7` gets the drop check `L@e8` and mate; `P@e2` gets
  `G@c7`; `N@d4` gets `N@c7`. In 4 of 5, the refutation is a quiet drop
  or king move at ply 2 or later. Qsearch (captures, checks at its
  first ply) does not see them.
- Both answers point to one gap: the royal terms do not price an attack
  that the pieces in hand can still make. The C candidates must aim at
  that, and the screen must show it.

### Screen calibration (2026-10-04, queue81, queue82)

- 600 games at 5+0.05, book (400-line screen books for grand, koth,
  extinction, threecheck), 15 slots, each branch against its parent:

  | Branch | Variant | SPRT Elo | Screen Elo |
  | --- | --- | --- | --- |
  | PX | shogi | -306 | -357.0 ± 44.6 |
  | SD | shogi | -115 | -116.5 ± 29.5 |
  | SD2 | shogi | -33 | -64.4 ± 28.7 |
  | CR | shogi | -51 | -53.1 ± 26.4 |
  | TI | shogi | -20 | -8.7 ± 26.6 |
  | DW | shogi | -9 | +29.6 ± 27.2 |
  | PX2 | shogi | +16 | +16.8 ± 27.5 |
  | SB | grand | -31 | -26.7 ± 23.8 |
  | GA | koth | +137 | +123.0 ± 29.1 |
  | EX | extinction | +184 | +187.0 ± 32.4 |
  | CD | threecheck | +9 | +57.9 ± 27.7 |
  | CC | threecheck | +248 | +340.4 ± 42.5 |

- Spearman ρ 0.97, Pearson 0.99, no sign miss in the 8 decisive
  results. The screen passes D5 and replaces the predictor.
- CD is 3 standard errors above its SPRT: at 5+0.05 the check-count
  danger gains more than at 10+0.1. A screen gain is thus a filter,
  not a result; each screen winner still needs the SPRT.

### KH (2026-10-04, branch `plan29-kh`, bcd0f97)

- Bug: `king_danger!` skipped every enemy shield piece, the hand too.
  `derive_shield_pieces` marks the shogi gold and silver as shields
  (local, forward lean), so three golds and two silvers in the enemy
  hand added 0 danger. Over 335 shogi set positions, our danger was
  median 0 cp, p90 54 cp; FSF King safety median 0.95, p90 8.0 pawns;
  correlation 0.35. SD, DW and PX tuned a term that did not see the
  main shogi attackers.
- Fix: a shield on the board is still skipped, a shield in the hand
  counts. Node counts equal plan29-m2 in standard, xiangqi and grand;
  shogi and crazyhouse change. The test position (enemy hand
  `ssggg`) goes from -6449 to -7378 cp.
- Screen: shogi `0 5` arm, crazyhouse regression arm (queue83).
- Screen: shogi -69.2 ± 28.3, crazyhouse -12.2 ± 25.9. Fails.
- Why, from its 600 shogi games: KH is near 100 cp more hopeful than
  the base on the same positions (wins +132, losses +123), and it keeps
  more pieces (not pawns) in the hand: 2.24 against 1.34 at ply 80. A
  gold in the hand counted, a dropped gold (a board shield) did not, so
  each drop lowered the danger it gave.

### KH2, KH3, KB, HR (2026-10-04)

All against plan29-m2, 600-game screens at 5+0.05:

| Change | Shogi | Crazyhouse | Others |
| --- | --- | --- | --- |
| KH2 (dded47c): shields count, hand and board | -66.8 ± 28.9 | +4.6 ± 26.1 | grand -0.6 |
| KH3 (f625360): KH2, hand pressure halved | -8.1 ± 27.2 | | |
| KB (plan29-kb): shields count on the board, no hand term | +15.6 ± 27.6 | +75.9 ± 28.6 | grand -15.1, standard +16.2, xiangqi -0.6 |
| HR (ff6a405): m2 without the hand term | -16.8 ± 27.2 | +64.9 (468 games) | node counts equal |

- KH2 still keeps pieces in the hand (2.01 against 1.51 at ply 80)
  and is still 106 to 132 cp more hopeful. Cause: a piece in the hand
  reads `zone_attack_best`, the pressure of its best square on the
  whole board. Nearly every real drop square gives less, so a drop
  lowers the danger. The base has this for the rook, bishop, knight
  and lance in the hand.
- Less hand weight is better in each step: full -67, half -8, none
  +16. The hand term goes. KB goes to the shogi SPRT `0 5` at 10+0.1
  (queue87); its grand screen (-15.1) needs a regression check before
  a merge.

### Shogi tactics suite (2026-10-05)

- The 156 fatal shogi D2 positions (our move loses 100 cp or more, FSF
  depth 16). Solved: our move within 30 cp of FSF's best (FSF depth 14
  `searchmoves`, cached).
- Base at 1x, 4x, 16x of 28,000 nodes: 15, 24, 41 solved. At 16x, 101
  still lose 100 cp or more, and our depth is near 12. FSF finds its
  move within 448,000 nodes in 75 of those 101 (median 207,000 nodes,
  depth 14). FSF's static eval terms do not separate its move from
  ours (mean +0.28 pawns, no term stands out): these are tactics that
  our search does not reach, not a missing static term.
- In 73 of the 101 the enemy hand has fewer than two pieces that are
  not pawns: drop attacks on our royal are near one quarter of them.
- Each gate off (D3 switches), 448,000 nodes, noise ±2 (base at 400k
  to 500k: 41 to 43): S1 52, razoring 49, null move 46, RFP 45, X1 45,
  futility 42, base 41, LMP 35. S1 split: pruning part 39, LMR part 46;
  drop checks only 41. The LMR part is CR, which lost -51 in its SPRT,
  so the suite is a filter like the screen, not a result.

### RZ (2026-10-05, branch `plan29-rz`, 92f4a58)

- No razoring in a drop variant. Razoring asks quiescence to confirm
  a fail low; quiescence plays no drops, so it confirms a fail low
  that a drop would save. Node counts equal plan29-m2 in standard,
  xiangqi and grand. Suite: 49 solved (base 41).
- Screens: shogi +37.8 ± 28.3, crazyhouse +45.9 (488 games). Shogi SPRT
  `0 5` at 10+0.1, then crazyhouse (queue90).
- Suite noise is larger than the node test showed: the base over six
  hash seeds solves 39 to 45 (mean 40.7). RZ (49) and DK (51) are each
  above that, but RZ and DK together solve 43: changes do not add.
- RZ shogi SPRT: near 0 at 2665 games (+0.3 ± 12.8), running to the
  cap. Short shogi screens gave +16 to +58 for DW, CD, KB and RZ, and
  the SPRTs gave about 0; shogi screens now run at 10+0.1.

### How the shogi games against FSF are lost (2026-10-05)

- Build 10 against FSF 2000, 400 shogi games, 251 losses (median 92
  plies). In 184 of them our score reached +300 or more after our move
  5; in 152 it fell to -300 or less before our move 30.
- FSF's score of our side, at our moves where we gave +300 or more, in
  games we lost: median -115. Where we gave +100 to +300: -200 (lost),
  -100 (won). We take lines that we think win and FSF does not.
- The gap (our score minus FSF's, from our side) grows with our plain
  material lead: -3 or less +345, -2 to 2 +403, 3 to 7 +627, 8 or more
  +897. The cp scales differ (one pawn in hand: ours +100, FSF +31),
  so the slope is partly units; the +345 when behind is not.
- One piece in hand at the start, relative to a silver (ours / FSF):
  pawn 0.31 / 0.15, lance 0.43 / 0.56, knight 0.37 / 0.63, gold
  1.16 / 1.04, bishop 1.34 / 1.30, rook 2.19 / 1.37. We value a rook in
  the hand near 60% higher and a pawn near two times higher, the lance
  and knight lower. One squeeze of all values does not fit: what lowers
  the rook raises the pawn. A rule-based cause is needed before a
  candidate. CP (plan 27) already gave shogi this squeeze: H0, -25.0;
  CP2 then kept it to free drops. A per-piece form (every piece but the
  pawn) is the same in shogi, so it was not run.
- Correction: the score gap above does not show that we are too
  hopeful. FSF at UCI_Elo 2000 reports the score of its best line but
  plays a weaker move (skill level), so its scores favour FSF: in grand
  games that we won, at our scores of -100 to +100, FSF gave our side
  -176. Units differ too. Only the depth-16 suite and the hand values
  stand as facts.

### RZ and DK results (2026-10-05)

- RZ shogi SPRT `0 5` at 10+0.1: inconclusive at 4000 games,
  +4.1 ± 10.5. Not merged on shogi; its crazyhouse screen (+41.3) goes
  on in HRZ.
- DK (plan29-dk, 353fde7): a drop reads its own plane of history keys.
  Node counts equal plan29-m2 without drops. Suite 51. Screens at
  10+0.1: shogi -10.4 ± 27.9. Fails.
- The suite picked RZ (+8) and DK (+10); both are near 0 in games. The
  suite does not predict shogi strength and is dropped, as the
  predictor was.
- HRZ (plan29-hrz, e1012f6, HR + RZ): crazyhouse SPRT `0 5`, then
  shogi `-5 0` (queue93).
- HRZ crazyhouse SPRT `0 5`: H1, +42.0 ± 16.3 (1586 games). Shogi
  `-5 0`: inconclusive at 4000 games, -16.6 ± 10.6 (the upper end is
  -6). A shogi loss, so HRZ is not merged.
- The hand term is worth keeping in shogi and harmful in crazyhouse. A
  likely cause: the crazyhouse hand holds a queen and rooks, so the sum
  of best-square pressures grows past use; shogi hand pieces are weak.

### HM (2026-10-05, branch `plan29-hm`, 15e892a)

- The hand adds only its largest `zone_attack_best`: a side drops one
  piece in a move. Node counts equal plan29-m2 without drops.
- Screens at 10+0.1 against plan29-m2 (queue96), then RZ crazyhouse
  SPRT `0 5`.
- Screens at 10+0.1: crazyhouse +32.5 ± 27.4, shogi +4.1 ± 27.8.
- Crazyhouse SPRT `0 5`: H1, +60.0 ± 19.9 (1002 games). Shogi `-5 0`:
  -16.5 ± 14.9 at 2024 games (LLR -1.23), stopped for HMF: any change
  of the hand term so far costs shogi.

### HMF (2026-10-05, branch `plan29-hmf`, 7f42603)

- HM only where all drops are free (`free_drops!`, capability bit 8).
  `drops_are_free` is the CP2 test, now shared by the material
  derivation and the capability mask. With drop rules the hand keeps
  the sum of its pressures, as in plan29-m2.
- Node counts: standard, xiangqi, grand, shogi equal plan29-m2 (bench
  depth 9); minishogi, euroshogi, annanshogi, judkins, pocketknight,
  kinglet equal plan29-m2 (search depth 8); crazyhouse equals HM. Of
  all variants only crazyhouse has free drops (CP2), so HM's
  crazyhouse H1 holds for HMF and no other variant changes.

### Merged build 12 (2026-10-05, branch `plan27-main12`)

- Merged build 11 + F1, F2, M2 (node-identical speed work) + HMF + the
  plan29-meas harness (`--book`, `--max-plies`, forfeit counts).
- Perft at depth 3 unchanged in standard, shogi, xiangqi, grand,
  crazyhouse.
- Not merged: KH, KH2, KH3, KB, HR, HRZ (shogi loss), RZ (shogi 0), DK.
- Build 12 against FSF 2000, crazyhouse, 400 games (book, 15 slots,
  800-ply cap, FSF Move Overhead 100): -132.2 ± 36.2 (build 10:
  -143.1 ± 34.7). HMF's +60 in self-play is near +11 against FSF.

### Self-play is not the target (2026-10-05)

- Each SPRT of plan 29b measures a change against our own last build;
  the goal is strength against FSF. HMF gave +60 in self-play and about
  +11 against FSF; plan 27 saw self-play gains near halved against FSF.
- From here a key-variant candidate also plays FSF 2000: candidate and
  base each on the same book lines, and the Elo difference between the
  two ratings decides. At 2000 games each the difference has near
  ± 16 Elo; the self-play SPRT stays the first filter.

### Build 12 baseline against FSF 2000 (2026-10-05)

1000 games each, repaired protocol (book, 15 slots, 800-ply cap, FSF
Move Overhead 100):

| Variant | Build 12 | Build 10 (old protocol, 400 games) |
| --- | --- | --- |
| standard | +67.2 ± 22.5 | +66 |
| xiangqi | +60.0 ± 22.1 | +13 |
| grand | -34.2 ± 20.6 | -50 |
| shogi | -92.3 ± 23.0 | -90 |

The four play the same as build 11 (node counts equal), so the xiangqi
rise is the protocol (book lines, Move Overhead), not the engine.
Standard and xiangqi meet the goal; grand needs near +35, shogi +92.
Next: RZ on build 12 against FSF in shogi, 2000 games, against a
build 12 run of 2000 on the same book lines (queue100).
- Result: build 12 again on the same 1000 book lines, 2000 games:
  -74.7 ± 15.5 (the first 1000-game run: -92.3 ± 23.0). RZ on build 12:
  -45.7 ± 15.5. Difference +29.0, near 2.6 standard errors. RZ is near
  0 in shogi self-play (+4.1 ± 10.5) and gains against FSF: FSF attacks
  with drops, razoring cut the drop that defends, and in self-play the
  two sides make the same error.

### Merged build 13 (2026-10-05, branch `plan27-main13`)

- Merged build 12 + RZ (no razoring in a drop variant), on the FSF
  result above (shogi +29 against FSF; crazyhouse self-play +16.9 ±
  10.3, inconclusive). Standard, xiangqi and grand play as build 12.

### Shogi against FSF after the reboot (2026-10-05)

The server rebooted; DK's run (1278 games, -65.2 ± 19.6, level with
build 12) was lost and not run again. 2000 games each, the same book
lines as the build 12 and RZ runs:

| Build | Elo | Note |
| --- | --- | --- |
| build 12 | -80.6 | two runs, 3000 games |
| build 13 (build 12 + RZ) | -63.9 ± 15.5 | same code as the RZ run |
| RZ, both runs | -54.8 | 4000 games, +26 on build 12 |
| HM (on plan29-m2) | -117.0 ± 15.9 | -42 on build 12 |
| RF (build 13 + no RFP with drops) | -78.0 ± 15.5 | -14 to -23 on build 13 |

- Two runs of the same code gave -45.7 and -63.9: one 2000-game run
  has near ± 16, so a change needs two runs or 4000 games.
- HM loses in shogi against FSF too; HMF keeps it to free drops.
- RF fails: unlike razoring, reverse futility does not ask quiescence,
  and the static score cut is worth more than the drops it misses.

### SY (2026-10-05, branch `plan29-sy`, a4a84bd)

- From the general loss analysis (E2, see `plan29-ps`): our capture
  where FSF does not take, 440 against 132. In 140 of 240 such cases
  FSF takes the capturing piece back within two replies, so it is a
  trade we value and FSF does not. Most: knightmate bishop takes mann
  (25), kinglet bishop takes the (not royal) king (10).
- Values against a knight, ours (build 13) and FSF (`types.h`):
  commoner 1.45 / 0.90, pawn 0.26 / 0.16, bishop 1.19 / 1.06, rook
  1.64 / 1.63, queen 3.16 / 3.25.
- Cause: `derive_piece_value` adds half the mobility of all families
  but the largest (synergy). It is meant for lines that one enemy piece
  cannot both avoid, but a stepper of two families (commoner, gold) got
  it too, and the knight with the same 8 squares did not.
- Fix: synergy only for a rider (an offset and its double are both
  usual moves). Params derived again (42 of 44 files change; in most
  only the royal value, which does not change play).
- Result: commoner 546 to 457 cp (bishop 453). Node counts equal build
  13 in 26 variants; 9 change: ai-wok, euroshogi, horde, judkins,
  kinglet, knightmate, minishogi, ouk-chaktrang, shogi.
- Test: build 13 and SY each 400 games against FSF 2000 in the 9
  variants, the sum decides (queue105).
- Queue105 was replaced by screens (queue106, queue107): SY against
  build 13, 600 games, 5+0.05 (shogi 10+0.1), book where one exists:
  knightmate +93.7 +/- 24.6, kinglet +64.4 +/- 26.1, horde +12.2 +/-
  22.7, ouk-chaktrang +5.8 +/- 7.7, judkins +4.9 +/- 16.3, euroshogi
  +2.3 +/- 26.5, ai-wok -4.1 +/- 23.5, shogi -8.7 +/- 27.2, minishogi
  -9.2 +/- 14.7. No variant falls; the two with a commoner gain.
- Next (queue109): SPRT `0 5` at 10+0.1 in knightmate, then SY and
  build 13 each 1000 games against FSF 2000 in knightmate and kinglet.
- Queue109 was replaced (queue112, queue113); the SPRT did not run.
  Against FSF 2000, 1000 games each, no book: knightmate SY +31.4 +/-
  19.5, build 13 -12.9 +/- 19.6; kinglet SY +77.7 +/- 20.9, build 13
  +40.5 +/- 20.3. SY gains about 40 against FSF in both.
- SY also lowers shogi gold (370 to 332) and silver (318 to 295), while
  FSF prices them near 0.78 of a bishop (ours 0.55, SY 0.49). Shogi
  is a key variant, so SY plays FSF there too before the merge: 2000
  games (queue115); build 13 had -63.9 and -45.7.
- Shogi: -63.3 +/- 15.5 over 2000 games on the same book (build 13
  -63.9 +/- 15.5). A run before the server crash stopped at 930 games
  near -71.7. SY costs shogi nothing.
- Merged as build 14 (`plan27-main14`).

### Full-strength FSF (2026-10-06, user request)

The user asked for FSF without UCI_Elo: our limits measured part by
part, the ceiling against full FSF, and our real strengths. Wins
against the skill handicap do not count as strength. All local runs:
one thread, Hash 64, FSF 14.0.1 XQ (also the server's), build 14.

- Perft (make and undo at each leaf for us; FSF counts its leaves in
  bulk): leaves per second standard 12.8M / 54M, xiangqi 7.8M / 33M,
  grand 10.9M / 61M, shogi 11.0M / 43M.
- Search NPS from the start (10 s): standard 1.32M / 1.57M (84%),
  xiangqi 0.77M / 0.96M (80%), grand 0.79M / 1.23M (64%), shogi
  1.01M / 1.20M (84%).
- Depth at 10 s (`go infinite`): standard 21 / 24, xiangqi 18 / 23,
  grand 15 / 20, shogi 19 / 27. Late in the search the trees grow alike
  (about 1.45 nodes for each ply). At low depth ours is much larger:
  depth 8 standard 55,934 / 5,247 nodes; depth 9 grand 606k / 13k,
  shogi 93k / 6.3k. The check rule (S1) explains much of it; CR and
  XS already showed that cutting checks loses.
- Timed build, bench standard depth 10 (each timed call adds about 15
  to 20 ns): make 92 ns, undo 20 ns, check test 28 ns (2.8 for each
  make), eval 69 ns, all moves 88 ns, captures 173 ns, hash probe 41
  ns; 540 ns for each node in all.
- With `movetime` our clock starts no new depth after a quarter of the
  time, so a fixed-time test uses `go infinite` and `stop`.
- Speed alone: 0.25 to 0.64 doublings, near 15 to 45 Elo at 60 to 70
  for each doubling. Speed is not what keeps grand and shogi low.
- CK (branch `plan29-ck`, 6b5f823): the move loop passes its gives-check
  test to the child, which tested the same position again. Node counts
  equal at depth 9; NPS standard +8%, xiangqi +3.5%, grand +2.3%,
  shogi +2%.
- Ladder (queue120): build 14 against full FSF on a node budget
  (`nodestime` 350, 88, 22, 5: 1/4 to 1/280 of its effort), 200 games
  at 10+0.1 in the four key variants, makruk and ouk-chaktrang. The
  budget at 50% gives the gap to full FSF in doublings; the gap less
  the speed share is the search and eval share. Local check: no time
  forfeits; FSF won 4 of 4 standard games at 1/16 (it uses NNUE in
  chess).
- From here a change is judged against full FSF on the budget where we
  score near 50%: no skill noise, so each win is against its real play.

### Paired batch result (2026-10-06, queue119)

Against FSF 2000 at 10+0.1, 5 rounds of 200 games, 1000 for each engine
and variant (build 14 / ST = build 14 + OS2 / FM2B = build 14 + FM2):

| variant | build 14 | ST | FM2B |
| ------- | -------- | -- | ---- |
| standard | +37.3 +/- 21.7 | +31.0 +/- 21.6 | +25.1 +/- 21.6 |
| grand | -60.4 +/- 21.9 | -69.7 +/- 22.0 | -98.8 +/- 22.4 |

- ST gap -7.8, FM2B gap -25.3 (mean of the two variants): both 0 or
  less, so both drop by the rule set before the batch.
- Why, from the analysis below: each was one prior term without attack
  information, and FM2 paid for full-width boards on each node.

### How we lose against FSF (2026-10-06, analysis before any candidate)

Data: the 1000-game build 12 runs against FSF 2000; scripts in the
scratch dir. A decision is our move just before FSF first claims +300
while we are ahead in material.

- Net material from before our move to 8 plies of FSF's depth-16 line
  (349 standard losses): no net loss 62%, net loss 2+ 34%, mate or check
  attack 5%. In the real games the no-loss group is 2 down within 16
  plies in 54%: two thirds of all losses lose material within 16 plies of
  a move we rated 0 or more.
- Time to solution (40 decisions, 10 s, one thread): FSF agreed with our
  move in 7; of the other 33 we never played FSF's move in 24, and in the
  9 we did we needed a median 13x its time. FSF found most of the 24 in
  under 0.2 s (g458 7 ms, g961 14 ms): its eval picks the move, so the gap
  is judgment, not depth.
- Replays at 5 s (40 decisions): another move FSF rates 150 cp better in
  16, the same losing move in 14, another but no better move in 10.
  Builds without LMR, without null move, or without futility and LMP
  each fixed 0 or 1 of the 14: no single cut hides them.
- At the depth-12 leaves FSF's move gives about 2 pawns of material for
  +1.0 king safety and +0.2 mobility against ours (FSF's classical terms,
  but FSF plays chess with NNUE; to repeat with the classical FSF).
- M3, passed pawns: the value grows with progress and doubles when
  connected and chained, with no test of the pieces that can stop the
  pawn. Game 575: four pawns against a rook, ours +7.6 (pawn term +1111
  cp), FSF -2.69. In losses where we hold a passer on rank 5 or more our
  static eval is 3+ pawns above FSF's in 68% (26 of 38).
- Static gaps measured at decisions sit inside exchanges (we have just
  taken a piece), so raw static evals mislead there; the next step uses
  depth-1 scores and quiet positions.
- Structure: FSF builds its eval on attack bitboards per piece type
  (mobility area, king ring attackers, safe checks, threats), from 13
  rider magics and leaper tables on 128-bit boards. We have no attack
  maps at run time, and `Board` is `U4096` for every variant, so each OR,
  AND or count on a board is 64 words. Mobility (plan 04) and FM paid
  for that.
- CK (6b5f823) searches the same nodes as build 14 in 12 variants
  (bench: standard, xiangqi, grand, shogi, crazyhouse, makruk, janggi;
  search from the start: koth, chushogi, threecheck, extinction).
  Merged as build 15 (`plan27-main15`).
- Approved plan: measure first (board width cost, eval error against a
  classical FSF oracle with a ranking of the missing terms, decisions
  with a classical FSF), then attack maps derived from any move vector,
  then eval terms in that order, each through offline gates before games.
  Weights are rule-derived priors; tuning on data stays a last resort.

### Phase 1 measurements (2026-10-06, build 14 and 15)

1a, board width. A pass over 16 pieces (OR of attack sets, AND with an
area, count) costs: U4096 141 ns, U256 114 ns, U128 44 ns, u64 3.3 ns;
a fixed `[u64; 64]` that loops only over the words in use: 1 word 12.5
ns, 2 words 20 ns, 4 words 35 ns, 21 words (taikyoku) 112 ns. A U128
build of the engine searches the same nodes at the same NPS (key
variants), so the width costs nothing today; it costs the attack maps.
Board sizes: 30 variants have 64 squares or fewer, 10 have 65 to 128,
4 have more (chushogi 144, daishogi 165, tjatoer 256, daidaishogi 289),
taikyoku 1296. Choice: one board type that loops over the words in use
(set at variant load), so all sizes stay in one build.

Ladder: build 14 against full FSF on a node budget (`nodestime`), 200
games at 10+0.1 (score from our side; tf = FSF time forfeits):

| variant | 1/4 | 1/16 | 1/64 |
| ------- | --- | ---- | ---- |
| standard, classical FSF | 0-200-0 | 3-188-9 | 56-122-22 (-119) |
| grand | 1-198-1 | 21-173-6 | |
| xiangqi | 33-167-0 (tf 32) | 14-185-1 | |
| shogi | 48-152-0 (tf 47) | 43-157-0 | |
| makruk | 0-132-68 | 4-64-132 | |
| ouk-chaktrang | 28-118-54 | 29-58-113 | |

- In standard, classical FSF at 1/64 of its effort still wins by 119:
  near 6 doublings plus 119, so full classical FSF is about 500 Elo
  above us. The NPS gap explains about 15 of it.
- With `nodestime` FSF overstepped its clock in xiangqi and shogi at 1/4
  (32 and 47 forfeits): those wins are not ours. Every rung shows the
  count; a rung with forfeits does not count.
- Makruk and ouk-chaktrang, where we beat FSF 2000 by +160 and +529,
  lose to full FSF too: that lead was FSF's skill noise.

1b, eval against a classical oracle. 616 to 666 positions from our FSF
games per variant, FSF classical (`Use NNUE=false`) at depth 13 as the
label; each engine's depth-1 score against it (correlation; error in
win probability after the best scale factor):

| variant | corr ours | corr FSF | error ours | error FSF | our scale |
| ------- | --------- | -------- | ---------- | --------- | --------- |
| standard | 0.55 | 0.86 | 0.067 | 0.021 | 0.30 |
| xiangqi | 0.36 | 0.80 | 0.075 | 0.023 | 0.20 |
| grand | 0.38 | 0.80 | 0.095 | 0.032 | 0.25 |
| shogi | 0.36 | 0.77 | 0.103 | 0.037 | 0.20 |

- Our judgment has three times FSF's error in all four, and our scores
  are 3 to 5 times too large.
- Our residual against FSF's own terms (correlation; FSF's residual in
  brackets): king safety +0.30 to +0.41 (near 0), mobility +0.28 to
  +0.32 (near 0), threats +0.15 to +0.26 in xiangqi, grand and shogi,
  rooks +0.08 to +0.28. Passed and pawns are negative in standard and
  shogi: we overvalue them (M3).
- All FSF terms together explain 34% to 44% of our residual; king safety
  alone 9% to 17%, mobility 8% to 10%.
- Our own terms on quiet positions: king danger varies by 8 to 22 cp
  only (a flat signal where FSF's king safety carries the most); the
  pawn term spreads 90 to 175 cp in standard and grand with a fitted
  weight near 0 (noise there); in xiangqi and shogi it helps.
- Order for Phase 3, from these numbers: king safety from attack maps,
  mobility, threats, passed pawns with stoppers (M3).

### Phase 2 (2026-10-06)

BW (branch `plan29-bw`, dc53bed): `BoardBits` is `[u64; MAX_SQUARES /
64]` and `or!`, `and!`, `count_bits!`, `is_empty!` loop over the words of
`files * ranks` only (`board_words!`). bnum is gone. Perft suites pass in
17 variants; node counts equal build 15 (bench standard, xiangqi, grand,
shogi, crazyhouse; search chushogi, tjatoer, taikyoku, koth); NPS equal.

Attack maps, design:
- At variant load, each piece's compiled move vectors become, for each
  square, an attack recipe: the capture landings, each with the squares
  that must be empty (pass legs) or occupied (screens). No list of piece
  types: riders, leapers, lame leapers, hoppers and any leg pattern come
  out of the same walk over the legs.
- At run time a rider direction is a ray mask cut at its first blocker:
  along any fixed step the square index changes by a constant, so the
  first blocker is the lowest or highest set bit of `ray & occupied` on
  any board width. Leapers read one board for each square; lame leapers
  and hoppers walk their few squares.
- Target-dependent conditions (royal, rank, unmoved, en passant, unload)
  stay with move generation; an attack map is for the eval, as in FSF.
- Gate before any eval term: on random positions in all 44 variants the
  map of each piece equals the capture targets of move generation where
  no target condition applies; cost measured on the bench. Estimate:
  150 to 300 ns for both sides on 8x8 against 540 ns for each node now,
  so a term must win more than the 25 to 35 Elo the speed costs.

AM (branch `plan29-am`, a142b09): `derive_attack_recipes` and
`piece_attacks!`, no reader yet. Against the capture targets of move
generation on random positions: exact in 34 of 44 variants, the four key
variants among them. Not exact: annanshogi (move patterns), hiashatar,
janggi (cannon rules), sittuyin, and a few squares in the large shogis.
Cost of the maps of both sides: standard 108 ns, shogi 140, xiangqi
163, capablanca 178, grand 191, chushogi 1115.

KS1 (branch `plan29-ks`, d0a3ae6): `king_danger!` counts the zone
squares that each enemy piece reaches on the maps, in place of the
static `zone_attack` table. Gate A error: standard 0.0673 to 0.0655,
xiangqi 0.0745 to 0.0737, grand 0.0953 to 0.0938, shogi 0.0984 to
0.0981.

KS2 (branch `plan29-ks2` on KS1, 55860e2, parked): safe checks (an enemy type that reaches a
check square that we do not guard) and weak squares (attacked zone
squares that only the royal defends). Gate A error: standard 0.0555,
xiangqi 0.0681, grand 0.0819, shogi 0.0979. KS2 repeats SB, and KS3
(drop checks) would repeat SD; the retrospective below stops both.

### Retrospective (2026-10-06)

Phase 3 came from FSF's eval code and the oracle; the ledger was not
read before it. What the ledger and the result sections say:

| Mechanism | Tries | Games | Recorded cause |
| --------- | ----- | ----- | -------------- |
| Larger king danger | KD, DW, PX | -10, -9, -306 | PX: the term rules the eval |
| Safe checks | SB, SD, SD2 | -31, -115, -33 | SD: its drops are refuted at depth 8 to 9, we reach 7 at 300 ms |
| Hand pressure | HB, E2, KH, KH2, KH3, HM | -46, -6, -69, -67, -8, FSF -42 | a drop lowers the danger the hand gave |
| Cut checks | CR, CR2, CS, XS, XS2 | -51, -49 cp, -42 cp, -17, -2 | not known; CR2 rules out the defender cause |
| Fewer nodes | QP, HL, XS2 | -2, +11, -2 | fewer nodes to a depth gave no strength |
| Rule misreadings fixed | P1, N1, E1, SP, EX, GA, CC, RZ, SY | +311, +195, +165, +178, +184, +137, +248, FSF +29, FSF +40 | gains |

- KS2 repeats SB and KS3 would repeat SD, with no change on the
  recorded cause. SB said: retry only with CR; CR failed.
- SD was 47 cp worse at 300 ms and 15 cp better at 3 s: the king terms
  ask for depth that we do not reach at 10+0.1.

Gate A against old changes with known results, each against its parent
(error in win probability, lower is better):

| Change | Variant | Gate A | Games |
| ------ | ------- | ------ | ----- |
| SD | shogi | 0.0988 to 0.0968 | -115 |
| SD2 | shogi | 0.0988 to 0.0982 | -33 |
| SB | grand | 0.0953 to 0.0952 | -31 |
| PX | shogi | 0.1010 to 0.1003 | -306 |
| PX2 | shogi | 0.1010 to 0.1006 | +16, inconclusive |
| KD | grand, xiangqi | 0.0980 to 0.0970, 0.0690 to 0.0676 | -10, +7, inconclusive |
| HB | shogi | 0.1003 to 0.1009 | -46 |
| KH, KH2 | shogi | 0.0988 to 0.1002 | -69, -67 (screens) |
| CS | grand | 0.0980 to 0.0963 | +14, inconclusive |
| SP | xiangqi | 0.0690 to 0.0745 | +178 |

Gate A passes four of the five large losses and fails SP, the largest
gain of plan 27 (SP's xiangqi depth-1 scores equal build 15's, so the
row is not a harness fault). It does not predict games and is dropped. Corrections to Phase 1:

- "Full classical FSF is about 500 Elo above us" took 60 to 70 Elo for
  each doubling above 1/64. The ladder gives near 200 for each doubling
  from 1/64 to 1/16 (standard -119 to -527, shogi +182 to -222), and at
  1/4 we scored 0 of 200 in standard and 1 real win in 153 shogi games
  (47 of the 48 wins are FSF time forfeits). The gap at full effort is
  not measured: well above 500 in standard, above 300 in shogi.
- "Our scores are 3 to 5 times too large": the best scale also shrinks
  with low correlation. Scale over correlation gives 1.5 to 2 times.
- "S1 explains much of" the low-depth tree: checks hold 10% to 37% of
  the nodes, so S1 cannot explain trees 10 to 46 times FSF's.

### Plan 29c: an Elo map of FSF (2026-10-06)

Each candidate so far guessed which FSF feature matters and tested it in
our engine. Plan 29c measures it in FSF first.

- Lab: FSF from `tmp/src` (scratch copy, server `~/fsflab`) with
  switches in `FSF_ABLATE`, a comma list. Search: `rfp`, `nmp`,
  `probcut`, `iir`, `mcp`, `seecap`, `chprune`, `fpp`, `seequiet`,
  `singular`, `checkext`, `lmr`, `lmrhist`, `lmradj` (base table only),
  `conthist`, `caphist`, `mainhist`, `killers`, `countermove`. Eval:
  `e_imbalance`, `e_pawns`, `e_pieces`, `e_hand`, `e_mobility`,
  `e_king`, `e_passed`, `e_variant`, `e_threats`, `e_space`. `s1` puts
  our check rule in FSF: no pruning or reduction of a check, and one ply
  for each check while the line is shorter than twice the root depth.
- Each switch changes the nodes to a fixed depth (chess 16, shogi 14),
  except where its term does not exist (`e_hand`, `e_variant` in chess;
  `e_variant`, `e_pawns` in shogi).
- Step 1, queue121: each switch against the lab base, 400 games at
  10+0.1 on the screen books, in standard (classical), xiangqi, grand
  and shogi; base against base gives the noise. Books paused meanwhile.
- Step 2: our behaviours in FSF (`s1` and `lmradj` in queue121, then
  their combinations) show which of them cost FSF the most.
- Step 3: port in rank order, each with a target from the map. A port
  below its target is a defect to find with counters from FSF on the
  same positions, not a dropped idea. Games against full FSF decide.
- KS2 is parked; the Phase 3 order is withdrawn.

Results (400 games each, switch against the lab base, Elo of the
switch side):

| Switch | standard | xiangqi | grand | shogi |
| ------ | -------- | ------- | ----- | ----- |
| base (noise) | +13.9 ± 20.7 | +15.6 ± 23.8 | -7.8 ± 30.1 | +7.8 ± 32.0 |
| s1 (our check rule) | -108.5 ± 23.9 | -20.9 ± 24.8 | -160.9 ± 31.7 | -503.0 ± 79.7 |
| s1prune (no check pruned) | -56.1 ± 21.1 | -23.5 ± 23.9 | -100.0 ± 32.2 | -179.5 ± 34.8 |
| s1lmr (no check reduced) | -2.6 ± 21.5 | +21.7 ± 23.0 | -22.6 ± 29.6 | -91.5 ± 35.8 |
| s1ext (each check extended) | -4.3 ± 22.2 | +13.9 ± 22.4 | -22.6 ± 30.9 | +10.8 ± 33.7 |
| e_king (no king safety) | -294.6 ± 37.9 | -95.3 ± 33.0 | -298.5 ± 42.2 | -52.2 ± 33.8 |
| s1 and e_king | -330.2 ± 43.1 | -95.3 ± 31.4 | -361.0 ± 49.5 | -309.6 ± 48.4 |
| lmradj (LMR base table only) | -27.0 ± 21.7 | -6.9 ± 24.2 | -15.6 ± 29.9 | -2.6 ± 33.7 |
| lmrhist (no history in LMR) | -40.1 ± 22.2 | -36.6 ± 23.5 | -78.6 ± 31.2 | -63.2 ± 33.3 |
| singular (no singular extension) | -55.2 ± 22.3 | -32.2 ± 23.1 | -50.7 ± 31.2 | +39.3 ± 30.9 |
| conthist (no continuation history) | -166.2 ± 25.3 | -212.9 ± 30.9 | -293.0 ± 43.0 | -221.1 ± 38.9 |
| e_mobility (no mobility) | -86.9 ± 24.6 | -56.1 ± 27.1 | -15.6 ± 32.4 | -12.6 ± 32.9 |
| e_threats (no threats) | -83.2 ± 22.4 | -35.7 ± 25.8 | -79.5 ± 30.2 | -62.3 ± 33.2 |
| e_passed (no passed pawn term) | -228.9 ± 31.5 | -0.9 ± 20.8 | -150.3 ± 31.8 | -43.7 ± 33.4 |
| lmr (no LMR) | -184.0 ± 25.4 | -156.6 ± 26.8 | -299.6 ± 42.1 | -337.5 ± 52.1 |
| mcp (no move count pruning) | -58.7 ± 23.4 | -65.0 ± 24.1 | -103.7 ± 29.4 | -179.5 ± 40.0 |
| seecap (no SEE pruning of captures, checks) | -32.2 ± 23.1 | -9.6 ± 24.4 | -75.0 ± 30.8 | -34.9 ± 34.2 |
| seequiet (no SEE pruning of quiet moves) | -11.3 ± 21.7 | -9.6 ± 25.6 | -37.5 ± 29.6 | -46.3 ± 33.9 |
| rfp (no reverse futility) | -27.9 ± 21.9 | -13.9 ± 25.8 | -35.7 ± 28.5 | -17.4 ± 34.2 |
| nmp (no null move) | -54.3 ± 23.2 | -23.5 ± 24.5 | -32.2 ± 32.1 | -26.1 ± 33.8 |
| caphist (no capture history) | -3.5 ± 23.1 | -12.2 ± 23.0 | -41.0 ± 29.3 | -65.9 ± 35.0 |
| killers (no killer moves) | +6.1 ± 22.7 | -9.6 ± 25.5 | -48.1 ± 29.4 | -17.4 ± 32.6 |
| countermove (no countermove) | -3.5 ± 23.1 | +11.3 ± 22.9 | +1.7 ± 29.8 | -1.7 ± 33.5 |
| mainhist (no butterfly history) | -21.7 ± 22.0 | -14.8 ± 23.5 | -13.9 ± 31.5 | -53.4 ± 31.6 |
| chprune (no cont-hist pruning) | -32.2 ± 21.8 | -65.9 ± 23.4 | -18.3 ± 28.9 | -38.4 ± 34.2 |
| fpp (no parent futility) | -11.3 ± 23.8 | -15.6 ± 22.4 | -19.1 ± 30.2 | -43.7 ± 34.1 |
| probcut (no ProbCut) | -9.6 ± 19.8 | +12.2 ± 24.2 | +10.4 ± 31.2 | -13.0 ± 31.9 |
| iir (no internal iterative reduction) | +5.2 ± 21.7 | -1.7 ± 24.4 | +17.4 ± 29.8 | +17.4 ± 32.8 |
| checkext (no check extension of FSF's) | -3.5 ± 21.7 | +15.6 ± 25.0 | -18.3 ± 28.7 | -4.8 ± 32.1 |
| e_pieces (no per-piece terms) | -66.8 ± 23.7 | +4.3 ± 22.3 | -84.1 ± 30.5 | -52.8 ± 33.1 |
| e_imbalance (no material imbalance) | -57.0 ± 24.4 | +11.3 ± 24.0 | -15.6 ± 30.0 | +13.9 ± 33.3 |
| e_pawns (no pawn structure) | -45.4 ± 24.9 | -17.4 ± 25.0 | -6.1 ± 29.1 | |
| e_space (no space) | -25.2 ± 23.0 | -5.2 ± 25.9 | +32.2 ± 28.6 | -20.4 ± 32.9 |
| e_hand (no hand terms, shogi only) | | | | -20.0 ± 33.3 |

The map, what FSF would lose without each part (Elo, 2 standard errors
near 45 to 65), against what our engine has:

| Part | FSF value | Ours |
| ---- | --------- | ---- |
| LMR | 157 to 338 | have; reduce 0.18 moves per main node against FSF's 0.41 |
| Continuation history | 166 to 293 | plies 1, 2 for order only; FSF 1, 2, 4, 6 and in pruning and LMR |
| King safety | 52 to 299 | flat (8 to 22 cp); under our check rule it loses in shogi |
| Our check rule (s1) | costs 21 to 503 | ours; CR does not transfer |
| Passed pawns | 150 to 229 (standard, grand) | overvalued, no stopper test (M3) |
| Move count pruning | 59 to 180 | have (LMP) |
| Threats | 36 to 83 | none |
| History in LMR | 37 to 79 | the sign only |
| Mobility | 56 to 87 (standard, xiangqi) | none |
| Per-piece terms | 53 to 84 (not xiangqi) | generic part only is a candidate |
| Chprune | 18 to 66 | none |
| Singular | 32 to 55, shogi +39 | none |
| Capture history | 41 to 66 (grand, shogi) | none |
| Quiet SEE pruning | 38 to 46 (grand, shogi) | none (QP flat) |
| Imbalance, pawns, space | standard only, 25 to 57 | named pieces |
| Null move, RFP, ProbCut, IIR, countermove, killers, FSF's check extension, parent futility | at most 54, mostly noise | have most |

Port order for our engine. Each port is tested against full FSF on a
node budget, paired with its base in the same session; its earlier tries
are read first (retrospective-first):

1. Histories as FSF uses them: continuation plies 4 and 6, the
   continuation pruning rule, and the history score in LMR. No rule
   knowledge; worth 37 to 293 together in FSF.
2. Passed pawns with stoppers and path control on the attack maps (M3).
3. Threats on the attack maps.
4. Mobility on the attack maps (standard and xiangqi first).
5. King safety with the check rule question; the lab shows the two go
   together, and CR alone did not transfer.

Port 1, earlier tries:

| Try | Change | Result | Note |
| --- | ------ | ------ | ---- |
| plan 04 B | continuation history, plies 1 and 2 | +14 (FIDE self-play), kept | ply 1 alone is worse in three of five variants (plan 21 R6) |
| plan 13 | shogi table density | 0.20 writes per slot, mostly zeros | L-3 (piece-type parent) was planned |
| plan 27 HL | LMR one ply by the sign of the history | +11.1 ± 9.9 xiangqi, inconclusive | the sign form is in the code now |
| never | LMR by the size of the history, plies 4 and 6, the continuation pruning rule | | FSF: lmrhist 37 to 79, chprune 18 to 66 |

What differs now: the three parts never tried, each with a value in FSF,
and a test against full FSF in place of self-play.

H1 (`plan29-h1`, 4a2748a, on build 15): LMR moves a quiet move's
reduction by one ply for each `HISTORY_REDUCTION_STEP` of its summed
history (seven steps span the full sum, as FSF's divisor spans its own;
an offset of a third of a step leans toward a deeper cut), in place of
one ply by the sign. Queue129 plays H1 and build 15 against full FSF,
interleaved rounds of 200 games, three in each key variant (`nodestime`
standard 14, xiangqi 16, grand 29, shogi 44).

H1 against full FSF, three rounds of 200 games each (W-L-D):

| Variant | build 15 | H1 | difference |
| ------- | -------- | -- | ---------- |
| standard (`nodestime` 14) | 301-254-45 | 292-246-62 | 0 |
| xiangqi (16) | 283-303-14 | 271-321-8 | -17 |
| grand (29) | 285-284-31 | 289-280-31 | +5 |
| shogi (44) | 291-309-0 | 261-339-0 | -35 |

No gain (each difference ± 40); H1 is not merged. A likely cause:
our history cells grow by depth squared and only clamp, with no decay,
so a busy cell sits at the bound; its sign means something, its size
little. FSF's update pulls each cell toward zero in proportion to its
size, so the size stays a measure. Next, measure the share of our cells
at the bound; if it is large, H1b adds that decay to H1.

The measure (temporary print after a 3 s search, one mid-game position
each; bound 16383):

| Variant | butterfly used, at bound, mean size | continuation used, at bound, mean size |
| ------- | ----------------------------------- | -------------------------------------- |
| standard | 469, 68, 4746 | 43733, 0, 146 |
| grand | 778, 0, 1653 | 53297, 0, 49 |
| shogi | 941, 46, 2113 | 36855, 0, 123 |

- The cause is not saturation: the continuation cells are near empty.
  A step is depth squared (36 at depth 6); FSF's is near 229 times the
  depth, capped at 2000, with a pull toward zero. A continuation cell
  (one move after another) is hit a few times in a search, so with our
  steps it stays near zero: our order and H1 read butterfly history
  alone, where FSF loses 166 to 293 without its continuation history.
- Earlier tries: history gravity and a bonus table existed until plan
  15 removed them, with no value measured; plan 16 Phase P and plan 17
  I-5 (gravity history) were proposed and never run.
- H3: a cutoff at depth d moves a cell d / 128 of the way to its bound,
  at most a sixteenth (depth 8 on), and every step pulls the cell toward
  zero in proportion to its size. On build 15, against full FSF as H1.
- H3 (`plan29-h3`, 011efae): the same print after a 3 s search gives
  continuation cells of mean size 2470 (standard), 1446 (grand) and 1444
  (shogi), 10 to 50 times build 15, with almost none at the bound.
  Queue132 plays H3 and build 15 after queue131.

H3 against full FSF, three rounds of 200 games each (W-L-D):

| Variant | build 15 | H3 | difference |
| ------- | -------- | -- | ---------- |
| standard | 299-252-49 | 309-233-58 | +17 |
| xiangqi | 303-283-14 | 320-267-13 | +19 |
| grand | 301-256-43 | 330-238-32 | +28 |
| shogi | 261-337-2 | 286-313-1 | +29 |

- Mean +23 with near 10 of standard error (+2.3 sigma), positive in all
  four: the first change against full FSF to pass the merge rule. Its
  cause was measured first (near empty continuation history).
- H3 merges as build 16 after a screen in variants FSF does not play.
  H2 reads the continuation cells H3 fills, and M3b changes the eval, so
  both play again on build 16 (queues 133 and 134 stop).
- Build 16 (`plan27-main16`, 4cb4bdd) = build 15 + H3. H2b
  (`plan29-h2b`) and M3c (`plan29-m3c`) are H2 and M3b merged onto it.
  Queue135: gate D, build 16 against build 15 in self-play, 200 games in
  chushogi and tjatoer (600-ply cap); then H2b and M3c, each paired with
  build 16 against full FSF, three rounds in each key variant.
- Gate D, build 16 against build 15 in self-play, 200 games each:
  chushogi -8.7 ± 40.0 (80-85-35), tjatoer +34.9 ± 42.6 (90-70-40;
  time forfeits 2 and 3 on the two sides). No sign of harm; build 16
  stands.
- H2b against full FSF (W-L-D, build 16 then H2b): standard 311-242-47,
  310-225-65 (+10); xiangqi 299-291-10, 307-283-10 (+9); grand
  302-261-37, 324-239-37 (+26); shogi 277-321-2, 261-339-0 (-20). Mean
  +6; not merged. Shogi loses where the others lean up; in shogi it also
  skips drops. H2 without drops would be near +15 on the other three,
  still short; parked.
- M3c against full FSF, two rounds (stopped early): standard -26,
  xiangqi +19, grand -29, shogi -14 against build 16, mean -12; a third
  round could not reach the merge rule. Grand loses again, as with M3.
  The passer discount, attacked or only undefended, does not pay at this
  budget; the M3 family is parked until passer value is measured on its
  own (our value against FSF's on positions with a passer).
- H1b against full FSF, stopped after one round and a quarter: standard
  -4 then -25, xiangqi +5, grand -4, shogi -38 against build 16 (mean
  near -13). The size of the history in LMR does not pay in our engine
  even with filled cells; shogi loses again, as with H1. Parked.
- Ladder of build 16 (queue137, 200 games a rung; Elo from our side):

  | Variant | rungs (`nodestime`: Elo) | 50% near | build 14 50% near |
  | ------- | ------------------------ | -------- | ----------------- |
  | standard | 5: +358, 14: +49, 44: -360 | 16 | not run |
  | xiangqi | 5: +330, 16: +28, 44: -249 | 17.7 | 15.7 |
  | grand | 11: +343, 29: +16, 88: -326 | 30.5 | 29 |
  | shogi | 22: +175, 44: -73, 88: -254 | 36 | 41 (wide span) |

  On FSF's server speed, the 50% points are near 1/32 (standard), 1/13
  (xiangqi, grand) and 1/11 (shogi) of its effort: 5.0, 3.7, 3.7 and
  3.4 doublings of FSF's search from FSF at full effort.
  These are doublings of FSF's effort; ours per doubling is not measured
  yet (a ladder of our clock against a fixed FSF budget will).
- Queue140 measures ours: build 16 at 10+0.1, 20+0.2 and 40+0.4 against
  FSF whose `nodestime` halves as our clock doubles (standard 20, 10, 5;
  shogi 36, 18, 9), so FSF keeps its nodes a move; 200 games each. The
  rise of our score for each step is our Elo for each doubling, which
  turns the gap into our own doublings and a port into a share of one.
  Build 16's +23 moves the 50% point near a quarter, inside what rungs
  of ± 50 to 70 at two to four times apart resolve. The ladder shows a
  step of one or two doublings (60 to 130 Elo), not one port. The
  standard rungs 5 and 14 aborted (two harness runs at once, OOM) and ran
  again after H4 (queue139).
- H4 (`plan29-h4` on build 16, 6836e7c): continuation history follows
  the moves 1, 2, 4 and 6 plies back, as FSF's does; `HISTORY_TABLES`
  5, the quiet band 10 bounds wide. The note "a third ply does not help"
  was measured when the cells were near empty. Queue138 plays H4 and
  build 16 after the ladder (queue137).
- H4 against full FSF, three rounds (W-L-D, build 16 then H4): standard
  315-237-48, 311-235-54 (-1); xiangqi 303-283-14, 310-278-12 (+7);
  grand 325-245-30, 316-255-29 (-11); shogi 258-341-1, 275-324-1 (+20).
  Mean +4 with near 10 of standard error (+0.4 sigma); not merged. Only
  shogi leans up (one sigma), and grand leans down. The plies 4 and 6
  add little where plies 1 and 2 are already filled; parked.
- Self-ladder of build 16 (queue140, 200 games a rung, FSF's nodes for
  each move fixed):

  | Variant (FSF `nodestime`) | 10+0.1 | 20+0.2 | 40+0.4 | Elo for each doubling |
  | ------------------------- | ------ | ------ | ------ | --------------------- |
  | standard (20, 10, 5) | -54 | +38 | +96 | 93, 58; mean 75 ± 34 |
  | shogi (36, 18, 9) | +45 | +103 | +210 | 58, 107; mean 82 ± 38 |

  Near our 50% point, a doubling of our time gives 75 to 82, and a
  doubling of FSF's nodes gives 200 to 250 (its ladder in standard: 5:
  +358, 14: +49, 44: -360). FSF turns each doubling of its effort into
  near three times the strength that we get from one of ours. A speed
  gain alone closes little: build 16's whole NPS gap to FSF is under one
  doubling.
- Tree profile at a fixed depth (temporary counters in build 16 and in
  the FSF lab, reverted; 20 oracle positions each). Nodes, ours over
  FSF's: standard 2.6 (depth 10), xiangqi 3.6 (9), grand 6.0 (8), shogi
  10.8 (10). At each depth left, a node of ours searches more moves: at
  depth 1, 1.4 to 1.8 against FSF's 0.6 to 0.7; at depth 7, 8 to 14
  against 3 to 4.5. Quiescence holds 50 to 66% of our nodes (FSF 42 to
  47%). In shogi our depth 1 nodes are 12 times FSF's (1.67M, 138k).
- PC, earlier tries: CR, CR2 and CS reduced checks and XS, XS2 cut the
  extension; no try let a check be pruned. S1's rule that no check is
  skipped came from one xiangqi mate in plan 27, and S1 alone gave +0.8
  in xiangqi; its gain came with X1. In the lab, letting checks be
  pruned under the rest of S1 (`s1_le`) gave back 91 in standard, 121
  in grand and 380 in shogi, the largest part of S1's cost.
- PC (`plan29-pc` on build 16, b2b8a2a): as in FSF, the move count and
  the exchange test skip a check as any move, before it is made; only
  futility still tests for check. Gates: xiangqi mate 2 at depth 1 (as
  build 16); grand mate -5 at depth 16 in 1.9 s (build 16: depth 14 in
  0.9 s; plan 27 gate: depth 16). Queue141 plays PC and build 16 against
  full FSF, three rounds in each key variant.
- Our node count to a fixed depth changes with the Zobrist keys (a new
  seed each run unless `ANEKAMACAM_SEED` is set): 20 positions moved by
  20 to 50% between runs of one build. A node ratio is now the geometric
  mean over 40 positions and 3 seeds (120 searches, ± two standard
  errors). PC against build 16: standard x0.97 ± 0.07, xiangqi x0.96 ±
  0.08, grand x0.98 ± 0.09, shogi x0.70 ± 0.09 (time x0.69). Only the
  shogi tree shrinks; the first 20-position figures were noise.
- PC against full FSF, two rounds (W-L-D, build 16 then PC): standard
  205-164-31, 183-181-36 (-34); xiangqi 213-175-12, 183-213-4 (-59);
  grand 200-175-25, 193-184-23 (-14); shogi 188-212-0, 164-234-2 (-41).
  Mean -37 ± 12, lower in all four; stopped after two rounds. In FSF the
  same change gives back 91 to 380. With CR, CR3 and CR3b this is the
  fourth form of cutting checks that loses in our engine; FSF's king
  safety is what makes a cut check safe there (`s1_eking`), and our
  engine finds the threats by search instead. The check rule stays until
  the eval carries king safety.
- Tree profile, extended (pre-loop exits; temporary counters, reverted):
  at depth 5 and more FSF leaves 45 to 65% of its nodes before the move
  loop (null move 17 to 35%, ProbCut 8 to 16%, reverse futility up to
  depth 8), ours 20 to 25%. At depth 1 FSF's reverse futility cuts 41 to
  45% of the nodes, ours 15 to 26%. Our loop nodes search more moves in
  each class (captures, quiet moves, checks).
- FR (`plan29-fr`, 7df373e): futility reads its margin at the move's
  reduced depth, as FSF's `lmrDepth`, so late moves of deep nodes are
  judged as shallow ones; the margin table gains depth 0. Nodes: standard
  x1.03 ± 0.07, xiangqi x0.90 ± 0.04, grand x0.98, shogi x0.98. Not
  played: futility is not where our tree is larger.
- NE (`plan29-ne`, 76c197b): the null move search is one ply shallower
  for each 100 (the cheapest piece) the score stands above beta, at most
  3, as FSF's `(eval - beta) / 205`. Nodes: standard x1.04 ± 0.03,
  xiangqi x0.93 ± 0.05, grand x1.00, shogi x1.04. Not played: the null
  move's depth is not why FSF's null move cuts more nodes.
- Where the search ports stand: H3 passed; H1, H1b, H2b, H4, CR3, CR3b
  and PC did not, and FR and NE do not change our tree. The lab's large
  eval parts are untested against full FSF in our engine: king safety
  (under our check rule FSF still gains 161 in standard and 97 in grand
  from it, and loses 140 in shogi), threats, mobility. KS2 was parked on
  "retry only with CR"; the lab contradicts that outside shogi, and SB,
  KD and DW were self-play runs on the static zone table.
- KS3 (`plan29-ks3`, 4f9b7a3): build 16 with the KS2 line merged (BW,
  AM, KS1, KS2: zone attacks counted on the attack maps, safe checks,
  weak squares). Perft equal to build 16 in six variants. Against build
  16 (geometric means, 120 searches): nodes to a fixed depth standard
  x0.86 ± 0.09, xiangqi x0.74 ± 0.07, grand x0.77 ± 0.09, shogi x1.06 ±
  0.09; time to that depth x1.20, x1.01, x1.02, x1.37. The maps cost 29
  to 40% of our NPS. Queue142 plays KS3 and build 16 against full FSF,
  three rounds in each key variant.
- KS3 against full FSF, one round (W-L-D, build 16 then KS3): standard
  94-82-24, 70-98-32 (-70); xiangqi 113-82-5, 100-90-10 (-37); grand
  104-78-18, 53-126-21 (-178); shogi 91-109-0, 66-133-1 (-88). Mean -93;
  stopped. Cause, measured: KS3 moves our depth-1 score by 133 cp on
  average in standard, 72 in xiangqi, 213 in grand and 84 in shogi (up
  to 1450). A safe check adds the whole zone (nine squares) for each
  enemy type, so one checking piece counts as nine attacks, and the
  square of the sum runs to the cap. The term's size is wrong, not yet
  its idea; grand, where it moves most, loses most.
- Reusing the boards in `king_danger!` (one zeroing each, the words in
  use cleared) left KS3's nodes unchanged and saved 0.5 to 1% of time:
  the cost is the map walk itself, done for both sides in each call.
- Gate B, tried as accuracy against FSF: 400 positions from build 16's
  games in each key variant, each build's move at 200 ms (seed fixed),
  scored by the lab FSF at 300k nodes with `searchmoves`; loss is FSF's
  best minus our move, capped at 1000. Against build 16: PC -2.8, -2.5,
  +3.7, +12.1 cp; KS3 -8.9, +1.6, -0.9, -8.0 (± 9 to 17). KS3, -93 in
  games, scores better than build 16 in standard and shogi. Gate B does
  not predict games and is dropped; games against full FSF decide.
- KS1b (`plan29-ks1b`, a795d35): KS3 with KS2 reverted, so the zone
  attacks are counted on the maps at the old scale. Depth-1 scores move
  by 6 to 13 cp on average; time to depth x1.10 standard, x1.04 grand
  (NPS -10 to -12%). Queue143 plays KS1b and build 16 against full FSF,
  three rounds in each key variant: whether real attacks beat the static
  table at the same scale.
- KS1b against full FSF, three rounds (W-L-D, build 16 then KS1b):
  standard 316-224-60, 302-220-78 (-6); xiangqi 305-280-15, 283-304-13
  (-27); grand 323-240-37, 316-254-30 (-12); shogi 266-333-1, 271-328-1
  (+6). Mean -10 ± 10; not merged. Real attacks at the old scale give
  nothing over the static table, and the loss is near what the 10 to 12%
  of NPS costs. A term on the maps must carry new information worth more
  than its share of the map cost.
- KS2b (`plan29-ks2b`, 5367099): KS3 with a safe check counted as one
  move onto the royal (one unit), the pressure unit's own definition, in
  place of the whole zone. Depth-1 scores move by 29 to 34 cp on average
  (shogi 73, max near 500); time to depth x1.37 standard, x1.11 grand
  (NPS -27%).
- KS2c (`plan29-ks2b`, f044e92): one pass makes each piece's attacks
  once for both colours (`attack_reach`, `attack_zones` in `Scratch`;
  `clear_board!`, `count_common!` in `board.rs`). Node counts equal KS2b
  with a fixed seed; time -7 to -8%. The rest of the cost is the map walk
  itself, more than the whole old evaluation (69 ns). Queue144 plays
  KS2c and build 16 against full FSF, three rounds in each key variant.
- KS2c against full FSF, two rounds (W-L-D, build 16 then KS2c):
  standard 199-149-52, 190-166-44 (-23); xiangqi 212-184-4, 198-196-6
  (-23); grand 195-179-26, 202-175-23 (+10); shogi 181-219-0, 152-247-1
  (-51). Mean -22 ± 12; stopped. Safe checks and weak squares at a sane
  scale do not pay for the map cost (NPS -26%); shogi, under our check
  rule, loses most, as the lab predicts.
- Why FSF cuts more nodes before the move loop, step by step (temporary
  counters in build 16 and the lab, reverted; seeds fixed):
  1. Not the gates of our null move: allow, endgame and material block
     about 0% of the eligible nodes. Our null move is tried about as
     often as FSF's, but succeeds in 17 to 44% of tries, FSF's in 60 to
     89%.
  2. Not FSF's eval: with `e_threats` or `e_king` off, FSF's null move
     still succeeds 76 to 89%.
  3. The tempo: our eval gives the side to move `tempo_bonus` (2.4% of
     the dearest piece), so a pass swings it by two bonuses. Of our tries,
     36 to 70% stand less than that above beta and succeed 2 to 6%; above
     it, 60% (standard, grand) and 36 to 41% (xiangqi, shogi). FSF's try
     needs a margin near half a pawn at low depth.
  4. NT (`plan29-nt`, cc2d188): try only two tempo bonuses above beta.
     Nodes x1.05 standard, x0.93 xiangqi, x1.04 grand, x1.03 shogi: a
     doomed try at depth 3 to 4 costs one quiescence, and its rare
     success cuts a subtree. Not the lever; not played.
  5. Not the make-up of the tree: before reverse futility, 69 to 71% of
     our non-PV nodes stand above beta against FSF's 61 to 66%, with
     alike shares for each class of parent move (first, 2-3, 4-7, 8+).
  6. The exit gap is at depth 5 and more (ours 18 to 29%, FSF 46 to
     51%); at depth 3 to 4 the two are alike (42% and 39%). RFP8
     (`plan29-rfp8`, 623ef12, reverse futility to depth 8 as FSF's `depth
     < 9`): nodes x1.00 in standard, grand and shogi, x0.95 xiangqi; the
     margin there is near eight tenths of the dearest piece and rarely
     holds. Not played.
  The single gates do not move our node count; the gap in the move loop
  does (depth 1: 2.2 to 2.6 moves for each loop node, FSF 1.3 to 1.45).
  FSF also skips quiet moves with negative continuation cells
  (`chprune`) and captures with bad capture history there.
- H2c (`plan29-h2c`, 0efb2d0): H2b with drops left out of the failed
  reply skip. H2b gained +10, +9 and +26 in standard, xiangqi and grand
  and lost 20 in shogi, the one variant where the skip took drops.
  Queue145 plays H2c and build 16 against full FSF, three rounds in each
  key variant.
- The depth-1 loop: FSF's reverse futility cuts 41 to 45% of its depth-1
  nodes, ours 15 to 26%, and the rest enter our move loop. FSF's margin
  is `214 * (depth - improving)`: an improving side is judged one ply
  shallower, so at depth 1 it needs only eval >= beta. Ours gave an
  improving side 75% of the margin (near 74 cp at depth 1 in standard).
- RI (`plan29-ri`, 6d7a593): an improving side gets the margin of one
  ply less (`RFP_IMPROVING` removed). Against build 16 (geometric means,
  120 searches): nodes to a fixed depth x0.82 standard, x0.70 xiangqi,
  x0.62 grand, x0.86 shogi; time x0.80, x0.67, x0.56, x0.87. The first
  change to cut our tree by much. Xiangqi mate 2 at depth 1; the grand
  mate -5 at depth 17 in 3.7 s (build 16: depth 14 in 0.9 s), one
  position against a 13 to 44% faster search in all four. Queue146 plays
  RI and build 16 against full FSF first; H2c's rounds 2 and 3 follow
  (its round 1: standard -33, xiangqi +8, grand +6, shogi +50).
- RI against full FSF, two rounds (W-L-D, build 16 then RI): standard
  216-152-32, 202-161-37 (-20); xiangqi 190-198-12, 202-197-1 (+11);
  grand 211-165-24, 211-160-29 (+4); shogi 174-225-1, 161-239-0 (-24).
  Mean -7 ± 12; stopped. A search 13 to 44% faster to the same depth
  gave no strength, so its extra cuts lost what the speed gave.
- Why, measured (temporary counters in build 16, reverted): a node where
  RI cuts and build 16 does not was searched anyway, and its score fell
  below beta at depth 1 in 14% (standard), 13% (xiangqi), 15% (grand)
  and 9% (shogi); build 16's own cuts are wrong in 0.6 to 3%. FSF's
  depth-1 cuts with margin 0 for an improving side are wrong in 2.6 to
  4.4% (5 to 5.3% with `e_threats` off).
- Why our static score fails there: at a depth-1 node, a search of one
  ply at the static score (a null window at it) falls below it in 35 to
  41% of the nodes in standard, xiangqi and grand (shogi 19 to 24%);
  FSF's in 8.5 to 20%. With the tempo bonus removed from our evaluation
  (2.4% of the dearest piece: 28 in standard, 33 in grand), ours falls
  to 16 to 20% in standard and 13 to 17% in grand. Each move hands the
  bonus to the other side, so a one-ply search loses two bonuses against
  the static score unless the move gains them back.
- The tempo comes from the derived param files (`res/param/<variant>/
  latest.param`, scalar 1), which overwrite the derivation at load;
  `TEMPO_RATIO` has no effect until they are derived again.
- H2c against full FSF, three rounds (W-L-D, build 16 then H2c):
  standard 319-228-53, 297-234-69 (-17); xiangqi 303-288-9, 288-300-12
  (-16); grand 312-247-41, 318-254-28 (-1); shogi 262-336-2, 280-317-3
  (+22). Mean -3 ± 10; not merged. With drops left out, shogi turns
  positive and the others negative: H2's sign is noise at this size.
- T0 (`plan29-t0`, 8d7b89c): no tempo bonus in the evaluation (the term
  is removed; the scalar stays in the param files). RIT0 (`plan29-rit0`,
  e55e103): T0 with RI. Against build 16, nodes to a fixed depth: T0
  x0.88 standard, x0.80 xiangqi, x0.62 grand, x1.00 shogi; RIT0 x0.73,
  x0.61, x0.52, x0.85. Queue147 plays build 16, T0 and RIT0 against full
  FSF, interleaved, three rounds in each key variant.
- T0 and RIT0 against full FSF, three rounds (W-L-D; build 16 standard
  317-241-42, xiangqi 308-283-9, grand 310-260-30, shogi 281-318-1): T0
  280-255-65 (-30), 300-297-3 (-13), 313-251-36 (+7), 265-334-1 (-19),
  mean -13.5 ± 10; RIT0 307-240-53 (-5), 291-294-15 (-16), 313-243-44
  (+12), 270-328-2 (-12), mean -5.5 ± 10. Neither merged.
- The measure behind T0 was one-sided: removing the bonus moves every
  static score down, so the share of nodes where a one-ply search falls
  below it must fall. With the bonus, a one-ply search ends above our
  static score at 59 to 65% of the nodes, so the score is not biased
  high at its median; its error is wide in both directions. RI's wrong
  cuts come from that width, not from the tempo.
- Why T0 lost (temporary counters in build 16 and T0, reverted; 30
  positions each, seed fixed). For each main node, build 16 then T0:
  null move cuts standard 0.017 to 0.027, xiangqi 0.011 to 0.016, grand
  0.005 to 0.011, shogi 0.003 to 0.003; futility skips up 5 to 14%;
  quiescence stands pat less often (standard 0.43 to 0.32 of its nodes).
  Checked by a search without the cut: null cuts are wrong in 2.4, 4.6,
  7.6 and 4.0% (build 16) against 3.5, 6.6, 9.8 and 7.5% (T0); futility
  skips are wrong in 0% of the checks in both. So T0 lost through the
  null move: without the bonus a pass costs nothing, and the null move
  cuts where having the move was the advantage. The tempo bonus holds
  the null move back.
- The same check in build 16, split at two tempo bonuses above beta:
  null cuts below that margin are wrong in 14.6% (standard), 30.6%
  (xiangqi), 20.3% (grand) and 37.7% (shogi); above it in 0.8, 2.8, 3.9
  and 2.7%. NT (`plan29-nt`, cc2d188) removes exactly the cuts below the
  margin. It was not played because its node count did not fall; the
  count is a proxy, and its removed cuts are the wrong ones. Queue148
  plays NT and build 16 against full FSF, three rounds in each key
  variant.
- NT against full FSF, three rounds (W-L-D, build 16 then NT): standard
  306-232-62, 316-237-47 (+3); xiangqi 310-277-13, 308-287-5 (-7);
  grand 313-256-31, 326-247-27 (+13); shogi 254-343-3, 270-329-1 (+18).
  Mean +6.6 ± 9.9, up in three of four; not merged, parked as a small
  gain. Why small: the null cuts it removes are wrong in 15 to 38% but
  right in 62 to 85%, and the right ones end subtrees for the price of
  a quiescence; NT trades the errors for 3 to 5% more nodes.
- Why PC lost (temporary counters in PC, reverted): each move that PC
  skips by the move count or the exchange was searched anyway in one of
  eight cases, at a null window over alpha. A skipped check raised
  alpha in 5.4% (standard), 8.7% (xiangqi), 3.7% (grand) and 3.7%
  (shogi); a skipped move that is not a check in 1.4, 1.3, 0.9 and
  0.2%. A late check is 4 to 18 times as often a good move, so the
  move count may not treat it as any other move in our engine.
- The same check in the FSF lab (moves its move count and its exchange
  test skip, one in eight searched): skipped quiet moves that are not
  checks raise alpha in 1.5, 1.6, 1.2 and 1.1%; checks skipped by the
  move count in 5.5, 4.7, 2.4 and 3.9%; checks skipped by the exchange
  in 1.0, 1.7, 0.5 and 1.5%. FSF also skips good checks, at our rate in
  standard and shogi, so our move order is not the cause.
- What the skip buys: with `s1prune` (no check pruned) FSF's nodes to a
  fixed depth grow x1.37 standard, x1.24 xiangqi, x1.71 grand, x1.39
  shogi (30 positions each); PC shrank ours only x0.97, x0.96, x0.98
  (shogi x0.70). In FSF the late checks are a large share of a tree
  whose other quiet moves are cut hard; ours is thick everywhere (2.2
  to 2.6 moves for each loop node at depth 1 against 1.3 to 1.45), so
  the checks are a small share and keeping them costs little. In shogi
  PC saved 30% of the nodes (near +35 Elo of speed) and still scored
  -41: the dropped checks, mostly drops, cost near 76 there.
- So the check rule stays: it is cheap in our tree. The lever is the
  width of our move loop, not the checks in it.
- Is our static score noisier than FSF's? At depth-1 nodes, four null
  window searches at the static score plus and minus half a pawn and a
  pawn (ours 50 and 100; FSF 104 and 208 internal, its endgame pawn).
  Share of nodes beyond each bound, ours then FSF: search above +1 pawn
  standard 50.7 / 43.4%, xiangqi 46.1 / 39.8%, grand 48.9 / 43.4%, shogi
  78.6 / 53.2%; below -1/2 pawn 14.0 / 11.5, 15.0 / 12.3, 15.9 / 13.2,
  9.5 / 23.7%; below -1 pawn 5.2 / 8.3, 7.5 / 9.1, 5.9 / 9.0, 6.2 /
  15.4%. Outside shogi the two spreads are alike, so our score is not
  noisier. The earlier comparison for RI (its band near beta against all
  of FSF's cuts) was not like for like and is withdrawn as a cause.
- Shogi alone: our one-ply search beats our static score by a pawn at
  79% of the nodes, FSF's at 53%. Not FSF's terms (with `e_threats` off
  53%; with threats, king and hand off 47%). Not the hand value (drops
  are not free in shogi, so a piece in hand keeps its board value). D1
  (checking drops at the first quiescence ply) moves ours to 74%: a
  small part. Open: the value a drop gains by its square (promotion zone
  share in the piece-square tables).
- Speed first, since a gain in speed alone changes no move: a sampled
  profile of build 16 (macOS `sample`, symbols kept, a 12 s search of a
  standard middlegame) gives, by top of stack, `alpha_beta` 34% and
  `quiescence_search` 31% (make, check test, evaluation and picking are
  inlined there), `generate_all_captures` 18%, `generate_all_moves_and_
  drops` 12%.
- CX (`plan29-cx`, bc953d1): at load, each piece and square gets the
  set of squares that its capture legs land on (boards of at most
  `CAPTURE_REACH_WORDS` = 2 words); the capture generation skips a piece
  with no enemy piece in that set, except a piece with an own-piece
  capture leg and while an en passant square is open. Node counts equal
  build 16 in 31 variants (bench or a fixed search, seed fixed). Time to
  a fixed depth x0.960 standard, x0.976 xiangqi, x0.966 grand, x0.969
  shogi: near +4 Elo at no risk. The skip removes the short-range
  pieces, whose walks were cheap; the cost is in the line pieces'
  vectors, which need attack bitboards to drop.
- KS1b and KS2c in Elo of their parts, at 75 to 82 for each doubling
  of our time (self-ladder): KS1b's NPS -11% is 0.17 doublings, near
  -13; it scored -10, so real attacks at the old scale carry about +3.
  KS2c's NPS -26% is 0.43 doublings, near -34; it scored -22, so safe
  checks and weak squares carry about +12. With a cheap map they would
  pay; the map walk is the cost to remove first.
- AM2 (`plan29-am2`, 0cb39c1, on KS2c): on a board of at most two words,
  the attacks of a piece run on one `u128`: the leap board from its
  table, each line ray cut at its first blocker by one bit scan, a hop
  by a second scan past the screen. The map pass of `king_danger!` (mean
  for each call, 10 positions; standard, xiangqi, grand, shogi) falls
  from 296, 454, 516, 485 ns (KS2c) to 278, 384, 443, 446 ns. Nodes equal;
  time to a fixed depth x0.980 standard, x0.987 xiangqi, x0.994 shogi.
- BW2 (`plan29-am2`, 91f3938): union, intersection, difference, counts,
  the empty test and clearing take the two words of a small board with
  no loop. The map pass falls to 279, 340, 387, 382 ns. Nodes equal, and
  perft equal in eight variants; time to a fixed depth against AM2
  x1.002, x0.986, x0.980, x0.979.
- BW2 against build 16 (20 positions, three seeds) is still x0.79, x0.82,
  x0.81 and x0.87 in nodes per second. Breaking even needs KS2c's loss
  under about 10% (its +12 of information is 0.15 doublings).
- How often does an evaluation repeat? A temporary cache of static scores
  in build 16, keyed by the position and unmoved piece hashes: with 2^16
  slots 32.5% (standard), 26.0% (xiangqi), 23.5% (grand) and 33.6% (shogi)
  of the evaluations find their position (2^20 slots: 34.1, 27.7, 24.5,
  39.3%), and no stored score differs from a new one. The main search
  takes its static score from the table, but quiescence stores none, so
  each search again of a subtree scores its leaves again.
- EC (`plan29-ec`, 93aa1c6, on build 16): `evaluate_position!` reads a
  per-thread cache of 2^16 static scores first. Nodes equal; time to a
  fixed depth x0.984, x1.001, x0.990, x0.971. Build 16's evaluation is
  cheap (incremental material and tables, cached pawns), so a hit saves
  little. EC2 (`plan29-ec2`, 0ef6a9f) is EC on BW2, where each evaluation
  makes the attack maps: nodes equal, time against BW2 x0.915, x0.933,
  x0.944, x0.928. Against build 16 the map line now loses 13, 12, 14 and
  6.5% in nodes per second, near -16, -14, -17 and -8 at the ladder rate,
  against the +12 that KS2c's terms carry: near zero, so not played. The
  map pass must cost less again before the map terms can gain.
- The shogi depth-1 gap is withdrawn: the two bars were not in the same
  unit. FSF halves its shogi piece values (the capture fills a hand) and
  adds a hand bonus, so one pawn capture moves its score near 140; ours
  moves 188 (+100 in hand, +88 off the board). FSF's bar of 208 is near
  1.5 pawn captures, our bar of 100 near 0.5. At 280, 1.5 of ours, our
  one-ply search beats the static score at 55.4% of the nodes against
  FSF's 53.2%. Which move beats it (temporary counters in build 16, 30
  positions): a capture in 68.5%, a promotion in 21.9%, a drop in 7.7%.
  So the drop's square value was not the cause either.
- Width of the depth-1 move loop, moves made for each node (ours: zero
  window nodes out of check; FSF: all nodes, so its counts are upper
  bounds): standard ours 1.84 (captures 0.89, quiet checks 0.21, other
  quiet moves 0.74) against FSF 1.38 (captures and promotions 0.66,
  checks 0.10); xiangqi 1.94 against 1.31; grand 1.90 against 1.40;
  shogi 4.47 (drops 2.21, promotions 0.67, captures 0.72) against 1.41.
  A capture futility test (static score + victim + 200 at or below
  alpha) would skip only 0.00 to 0.08 captures outside shogi. In shogi,
  the check rule keeps each checking drop, and a promotion is never
  pruned; FSF counts a shogi promotion as a quiet move.
- A likely reason PC lost in shogi: our quiescence skips every quiet
  drop, a checking drop too, while FSF's first quiescence ply plays
  quiet checks, drops included. FSF can prune a checking drop at depth 1
  because its horizon still sees one; ours cannot. D1 (checking drops at
  the horizon) alone was neutral (+0.9 in self-play); the pair was never
  tried.
- QD (not kept): the quiet drops leave the quiescence list before the
  loop, as the loop skips them anyway. Shogi nodes x0.983 (the order of
  ties moved), time x0.994: the drops cost little there.
- Where the time goes, again: a sampled profile with picking, scoring,
  the exchange, make, undo, the check test and the evaluation moved out
  of line (temporary build, reverted). Search samples, standard /
  xiangqi / shogi: exchange simulation 36.8 / 30.1 / 27.5%, capture
  generation 18.1 / 15.4 / 8.9%, full generation 14.9 / 17.5 / 25.7%,
  evaluation 6.6 / 4.3 / 9.3%, make 6.6 / 9.2 / 4.5%, `alpha_beta` 4.9 /
  8.8 / 9.3%, the pick scan 3.5 / 3.4 / 6.5%, the check test 2.9 / 7.2 /
  3.4%, scoring 2.1 / 1.4 / 2.5%. The exchange ran for every capture at
  the first pick of a node, though most nodes cut after one or two
  moves; each run makes every capture of the sequence and generates the
  attackers again at each step.
- LS (`plan29-ls`, 4524f8a, on build 16): a capture starts untested,
  ranked by victim, then the cheaper attacker; the pick runs the
  exchange only for the capture it picks, and a losing one falls to its
  band. Time to a fixed depth x0.978, x0.884, x0.853, x1.048 (nodes
  x1.054, x0.947, x0.941, x1.081).
- LS2 (`plan29-ls`, c484215): a capture whose victim is worth at least
  its attacker skips the exchange; the side that captures can stop after
  each recapture, so it keeps at least the difference, and a winning
  capture is played at once, so only the sign counts. Time x0.904,
  x0.889, x0.830, x0.975 (nodes x1.024, x0.985, x0.975, x1.038). Perft is
  untouched (ordering only).
- LS3 (not kept): the exact exchange order back, by the victim value as
  a bound on the untested captures. Time x0.882, x0.900, x0.907, x1.002:
  no better than LS2, and more code.
- NH (NT and H4 together) against full FSF, six rounds (W-L-D, build 16
  then NH): standard 590-498-112, 613-471-116 (+14.6 ± 13.6); xiangqi
  619-559-22, 579-598-23 (-22.9 ± 14.1); grand 607-517-76, 634-497-69
  (+13.7 ± 13.8); shogi 501-694-5, 539-657-4 (+22.1 ± 14.3). Mean +6.9 ±
  7.0, one sigma; not merged. The parts alone gave +6.6 (NT) and +3.6
  (H4), and together about the same: each is a few Elo, below what six
  rounds can show at two sigma (+14). Xiangqi alone falls (1.6 sigma)
  where NT gave -7 and H4 +7; one variant in four that far out is in
  the spread at this size. Parked with its parts.
- LS2 against full FSF, three rounds (queue150; W-L-D, build 16 then
  LS2): standard 301-251-48, 326-216-58 (+35.4 ± 19.3); xiangqi
  314-279-7, 325-263-12 (+15.7 ± 20.0); grand 314-265-21, 315-249-36
  (+9.9 ± 19.7); shogi 250-348-2, 280-319-1 (+34.6 ± 20.2). Mean +23.9 ±
  9.9, 2.4 sigma, up in all four: merged as build 17. The time to depth
  predicted near +12 (x0.83 to x0.98 at 75 to 82 for each doubling); the
  games gave twice that, inside two standard errors. Open: whether the
  speedup is larger in game positions than in the 20 test positions.
- Profile of build 17 (same method): standard / xiangqi / shogi, the
  exchange 24.1 / 16.1 / 17.3%, capture generation 21.8 / 16.2 / 10.4%,
  full generation 16.6 / 21.4 / 27.8%, make 8.4 / 11.1 / 6.3%,
  evaluation 7.5 / 5.5 / 11.6%. Move generation is now the largest part,
  near 38% in each.
- CX and EC on build 17 (`plan29-cx17`, c025295 and baec9a1): node
  counts first differed in fivecheck and threecheck. The N-check score
  reads the checks given, and EC keyed only the board and the unmoved
  pieces. `eval_key` (1b85abb) now holds the checks too, and
  `search_key` starts from it. EC and EC2 above have the defect; their
  timings in other variants stand. With the fix, nodes equal build 17 in
  all 44 variants (bench where a suite exists, else a start search).
  Time to a fixed depth x0.949, x0.958, x0.956, x0.937 (CX alone x0.951,
  x0.961, x0.962, x0.962); on the server, bench nodes equal and time
  x0.85, x0.969, x0.955, x0.919. Merged as build 18 (`plan27-main18`,
  333cc46), node-identical as CK was.
- How much of the full move list does `alpha_beta` need? Temporary
  counters in build 18, 30 positions: a loop node picks a quiet move
  other than the table move in 40.7% (standard), 44.2% (xiangqi), 37.1%
  (grand) and 32.8% (shogi) of the cases; the table move is quiet in
  14.9, 15.2, 12.6 and 9.4%. Quiescence makes few full lists (23 to 41
  thousand, against 231 thousand to 1.5 million loop nodes).
- SG (`plan29-sg`, 8208699, not kept): the captures first; the other
  moves join when no winning capture is left. Time x0.984, x0.980,
  x1.038, x0.828 (nodes x0.983, x0.973, x1.019, x0.927). Why it gains
  only in shogi: a node that needs the quiet moves walks every capture
  vector twice, as a line piece's ray gives both its quiet moves and its
  capture, and our capture list costs near as much as the full list; in
  shogi the full list is mostly drops, which walk no vector, and CX makes
  its capture list cheap.
- DS (`plan29-ds`, c119f11): the board moves first; the drops join when
  no winning capture is left or the list runs out, or at once when the
  table move is a drop. Nodes equal build 18 in the 38 variants without
  drops; shogi time x0.889 (nodes x1.022 ± 0.059). Queue151 plays DS and
  build 18 in shogi, six rounds.
- The board and map line on build 18 (`plan29-am18`): BW (word boards,
  3650782, with `meets_row!` on words, f5205aa), AM (29d9fc2), AM2
  (30f7950) and BW2 (c2a0439, without the macros only the KS line used).
  Nodes equal build 18 in all 44 variants; time x1.000, x1.007, x1.006,
  x0.992. The maps have no reader yet.
- AF (`plan29-am18`, 601df5f): capture generation skips a whole piece
  whose attack map holds no enemy: a line piece blocked by its own side,
  after CX's reach test. A piece is whole when its maps hold all its
  captures. The first try lost taikyoku nodes (126 635 to 70 649): a
  lion takes on the way, a square the map treats as a screen. Now a
  piece is not whole when a vector takes on the way and no unload puts
  the piece back, when its last leg can take an own piece, or when a
  left-out vector (move condition, first move, royal only) can capture.
  A take that an unload puts back is a screen, as the cannon's; CX now
  counts such a piece as one that takes no own piece, so the cannons
  join both filters. Nodes equal build 18 in all 44 variants; time
  against the board and map line x0.976, x0.978, x0.964, x0.981.
- Where the time goes after AF (same method, the check test, the
  exchange's make and its attacker list also out of line): standard /
  xiangqi / shogi, full generation 20.5 / 24.4 / 30.9%, capture
  generation 15.0 / 9.2 / 5.7%, the legality test inside each make 9.7 /
  13.4 / 9.2%, the exchange's attacker lists 8.6 / 7.9 / 7.4%, the
  evaluation 8.3 / 6.2 / 11.3%, the exchange's make and undo 7.7 / 2.5 /
  5.2%, the search's check tests 3.7 / 8.5 / 3.3%. A check test reads
  each precomputed attacker of the royal square; it is the number of
  calls (each make, each exchange step, each check given) that costs.
- KS on the AF line (`plan29-ks18`, KS1, KS2, KS2b, KS2c ported,
  a27aa78): nodes per second against AF x0.853, x0.869, x0.833, x0.883.
  The map pass costs as much as before against a faster engine, near
  -16 at the ladder rate against KS2c's +12: not played.
- DS against full FSF in shogi, six rounds (queue151; W-L-D, build 18
  then DS): 582-617-1 (-10.1), 622-576-2 (+13.3); +23.5 ± 14.2, 1.65
  sigma; by round +78.6, -5.2, +38.4, +24.4, +22.6, -17.4. Under the
  merge line. Queue152 plays six more rounds of both, with PCD.
- PCD (`plan29-pcd`, 30a84c3 and 7fb18eb, on DS): a drop past the move
  count is skipped even when it checks, and the horizon of quiescence
  plays checking drops, made only on the squares from which a piece in
  hand has a line to an enemy royal. This is the pair that PC lacked:
  FSF prunes checks by the move count and sees checking drops at the
  horizon. Shogi nodes against DS x0.732, time x0.728; nodes equal DS in
  the 38 variants without drops.
- Queue152 (shogi, six rounds, build 18, DS and PCD in each round): DS
  against build 18 by round +17.4, -17.4, +29.6, +54.1, -5.3, +36.6.
  With queue151's six rounds: build 18 1135-1262-3 (-18.4), DS
  1208-1188-4 (+2.9), +21.3 ± 10.0, 2.1 sigma. DS is node-identical in
  the other variants, so the mean over the four is +5.3 ± 2.5, also 2.1
  sigma: merged as build 19 (`plan27-main19`, 7ccc4fe).
- PCD against DS: 586-612-2 (-7.5) against 554-646-0 (-26.7), -19.2 ±
  14.2; by round +3.5, -27.9, -13.9, -75.5, +14.0, -15.6. Not merged.
  Why, as far as it goes: the horizon plays the checking drops of the
  side that replies, one ply later, so the drops that the move count
  takes at depth 1 stay lost, as in PC. FSF has the same structure, so
  this does not say why FSF keeps the rule. Next measure: the share of
  skipped checking drops that would raise alpha, ours against FSF's.
- That measure (temporary counters in PCD and in the FSF lab; each
  checking drop the move count skips at depth 1 is searched anyway at a
  null window on alpha; shogi, 20 positions, depth 10): ours skips
  246 494, of which 4.0% beat alpha; FSF skips 12 915, of which 9.6%
  do. Our skips are not worse than FSF's, one by one. Yet PCD's time
  (x0.728) is near +37 at the shogi ladder rate, and PCD scored -19.2 ±
  14.2, 3.9 sigma under it: the few skipped drops that beat alpha cost
  far more than their share. A checking drop that beats alpha at depth
  1 is a mating attack the static score does not see; FSF's king safety
  sees that danger without the search (the lab: `s1_eking`), ours does
  not. The root is the same as CR's: the evaluation has no king safety
  of that kind, so the search must keep the checks.
- The board and map line on the server (x86, the game machine): bench
  nodes equal build 18; time x1.006 standard (depth 10, five seeds),
  x0.991 shogi (depth 9), and in a shorter run x0.939 xiangqi, x0.934
  grand. On the local machine it was x0.97 to x0.985. Not merged: its
  gain is not shown where the games run. The maps stay on the branch for
  a reader.
- CH (`plan29-ch`, 7ec21c3; on build 19 `plan29-ch19`, eacc2e1): a
  capture history (move key and captured piece), raised when a capture
  beats alpha and lowered when it does not, as the quiet history; at
  depth 1 a capture with a negative cell is skipped (with
  `see_pruning!`, and a check is still made and tested), as FSF's
  capture history rule. Against build 18: nodes x0.874, x0.820, x0.982,
  x1.006; time x0.881, x0.837, x1.014, x1.054. Queue153 plays CH and
  build 19 against full FSF, three rounds in each key variant.
- CH against full FSF, three rounds (W-L-D, build 19 then CH): standard
  329-216-55, 323-212-65 (-1.2); xiangqi 318-273-9, 320-268-12 (+4.1);
  grand 337-219-44, 330-227-43 (-9.0); shogi 295-304-1, 284-315-1
  (-12.8). Mean -4.7 ± 9.9; not merged. Why, as far as it goes: the
  smaller tree in standard and xiangqi (x0.88 and x0.84 in time, near
  +15 to +20 at the ladder rate) bought nothing, so the captures it
  skips at depth 1 cost about as much as the time saved. Next measure:
  the share of those skipped captures that would raise alpha, against
  the share in FSF's own capture history rule.
- That measure (temporary counters in CH and in the FSF lab: each
  capture the rule skips at depth 1 is searched anyway at a null window
  on alpha; 20 positions, depths 10, 9, 8, 10): ours skips 36 350,
  13 555, 44 024 and 226 748 captures in standard, xiangqi, grand and
  shogi, of which 2.7, 2.3, 2.3 and 1.1% beat alpha; FSF skips 26 310,
  14 120, 12 303 and 33 266, of which 4.8, 3.3, 2.9 and 4.2% do. Our
  skips are as safe as FSF's, so the guess above is wrong. What CH's
  time gives at the ladder rate (x0.88, x0.84, x1.014, x1.054) is +14,
  +19, -1 and -6, mean near +6.5; the result, -4.7 ± 9.9, is 1.1 sigma
  under it. CH's skip is sound and small, not a loss. The larger use of
  capture history in FSF is the order of captures (MVV plus the cell),
  which CH did not port; plan 4 G (capture history, -11 in a self-play
  round robin, "revisit ordering later") is its only earlier try.
- SE (`plan29-se` 4ec8577; on build 19 `plan29-se19`, f916afa): the
  singular extension of the table move, as FSF's: at depth 7 and more,
  a table move whose entry is a lower or exact bound from at most three
  plies less gets one more ply when a search of the other moves at half
  the depth stays below the table score less one point for each ply;
  when that search beats beta too, the node cuts. The test searches
  with the table move left out and stores nothing. It is off in drop
  variants (FSF's own test loses 39 in shogi) and obeys the extension
  limit of the checks. Standard nodes to depth 10 x2.0: the tests and
  the extended moves cost a ply. Queue154 plays SE and build 19 in
  standard, xiangqi and grand, three rounds.
- SE against full FSF, three rounds (W-L-D, build 19 then SE): standard
  328-222-50, 307-245-48 (-26.0 ± 19.4); xiangqi 334-251-15, 301-289-10
  (-41.4 ± 19.9); grand 340-238-22, 316-256-28 (-24.8 ± 19.8). Mean of
  the three -30.7 ± 11.4; not merged. Why, as far as it goes: the time
  to a fixed depth doubled, near one doubling (near -75 at the ladder
  rate), and the extensions bought back less than half of it. FSF's own
  test is worth +32 to +55 to FSF. Next measure: how often a node at
  depth 7 and more runs the test and extends, ours against FSF's; our
  table may hold far more lower bounds that qualify.
- That measure (temporary counters in SE and in the FSF lab, standard,
  20 positions, depth 10): of the loop nodes at depth 7 and more, the
  test runs at 43.7% (FSF 39.2%), extends in 40.4% of the tests (FSF
  44.7%) and cuts by two moves in 13.3% (FSF 24.3%). So the test fires
  as often as FSF's. Why it costs us more: an extended table move adds
  a ply to the largest subtree, and a ply costs as much as the branching
  factor, which is wider in our tree (depth-1 loops of 1.84 moves against
  1.38). And the cut by two moves comes half as often: we fail hard and
  store beta as the lower bound, so the table score less the margin
  rarely clears a later beta, where FSF's soft bounds often lie above
  it. Roots to act on: the width of our tree (the same root as PC), and
  the hard bounds in the table.
- HK (`plan29-hk`, 62da57a, on build 19): the session keeps the search
  worker of the last move, so the search, continuation and correction
  histories learn over the moves of a game, as FSF's do; a new game or
  variant drops them. Before, each `go` made a new worker with empty
  tables. In one session, the second of two searches makes another tree
  (standard, depth 10: 111 009 nodes against 79 412). Queue155 plays HK
  and build 19 against full FSF, three rounds in each key variant.
- Next root, earlier tries: the size of the history in LMR is done (H1
  and H1b, no gain). Fail-soft search was planned in plan 17 (E-5) and
  never run; the search has failed hard since plan 16.
- FS (`plan29-fs`, be0897a, on build 19): the search fails soft, as
  FSF's does. A table cut gives the stored score, not the window edge. A
  shortcut gives the score that made it: the static score cut its
  evaluation, the null move its score (beta when it finds a mate, since
  a pass is no legal move), ProbCut and razoring their search scores. A
  node gives and stores its best score, and the correction history
  learns from it. Quiescence gives its stand pat or best score; a
  capture its delta test skips raises the best score to the most it
  could give, as in FSF. Cost to a fixed depth against build 19 (20
  positions, three seeds): standard depth 10 nodes x0.90, time x0.90;
  xiangqi 9 x0.79, x0.77; grand 8 x0.87, x0.85; shogi 10 x0.74, x0.74.
  Debug games in five variants run clean, and mates come out as before.
  Queue156 plays FS and build 19 against full FSF after queue155, three
  rounds in each key variant.
- HK against full FSF, three rounds (queue155; W-L-D, build 19 then HK):
  standard 337-217-46, 319-230-51 (-18.5 ± 19.5); xiangqi 309-282-9,
  354-234-12 (+54.8 ± 20.1); grand 330-220-50, 346-215-39 (+12.7 ±
  19.7); shogi 303-295-2, 302-296-2 (-1.2 ± 20.0). Mean +11.9 ± 9.9,
  1.2 sigma: under the merge line. Xiangqi gains in each round (+60,
  +68, +36); the others go both ways. Queue156 plays HK in each of its
  rounds as a third arm, so HK gets six rounds paired with build 19, as
  DS got more rounds.
- LG (`plan29-lg`, 0fb9b12, on build 19): the check tests of a move
  read what the move cannot change. For each side and royal square, the
  reach is each square that an attack test on that square reads: the
  attacker's square, each leg, each square of its condition and the
  royal square. A quiet move, single capture or drop changes only its
  named squares (start, end, captured, unload). When none is in the
  reach of a royal of the mover, the mover's check state is the one the
  last snapshot holds, and the legality test is skipped. When none is in
  the reach of an enemy royal and the last move was not a pass, the
  enemy is not in check. Make keeps that answer only when it is free;
  else the search tests once and the snapshot keeps the result. A royal
  that moves, lands, is taken, or a setup phase, always tests. Nodes
  equal build 19 in all 44 variants, perft suites pass. The first form
  gave nothing in standard and grand (time x1.016, x1.010): counters
  showed 0% skipped there, because a pawn's capture leg can take en
  passant, and that filled the reach of every royal square. The leg onto
  the royal never reads the en passant square; only a leg before it can,
  and only such a leg fills the reach now. Time to a fixed depth against
  build 19 (20 positions, three seeds): standard x0.981, grand x0.958,
  xiangqi x0.928, shogi x0.959. In xiangqi and shogi, 29 to 41% of the
  legality tests are skipped. It merges when the server confirms the
  speed (x86), as CK did; the server plays queue156 first.
- Queue156 against full FSF, three rounds (W-L-D): build 19 standard
  351-203-46, xiangqi 333-254-13, grand 331-231-38, shogi 263-337-0; HK
  (its rounds 4 to 6) 322-224-54, 316-270-14, 359-200-41, 332-266-2; FS
  324-222-54, 309-282-9, 334-232-34, 297-299-4.
- HK over six rounds (queues 155 and 156, W-L-D, build 19 then HK):
  standard 688-420-92, 641-454-105 (-24.3 ± 13.8); xiangqi 642-536-22,
  670-504-26 (+17.6 ± 14.1); grand 661-451-88, 705-415-80 (+24.2 ±
  14.0); shogi 566-632-2, 634-562-4 (+40.0 ± 14.2). Mean +14.4 ± 7.0,
  2.05 sigma: merged. Standard falls in four rounds of six (1.8 sigma
  over all); why is open. Not the killers: `clear_search` clears them at
  each search, as FSF does. The kept tables are the butterfly and
  continuation histories (FSF keeps both) and the correction history
  (FSF has none).
- FS over three rounds (W-L-D, build 19 then FS): standard -27.9 ±
  19.6, xiangqi -30.4 ± 20.0, grand +1.2 ± 19.7, shogi +41.9 ± 20.1.
  Mean -3.8 ± 9.9; not merged. Its nodes to depth fell in all four
  (x0.90, x0.79, x0.87, x0.74), most in shogi, the one variant where it
  gains. Why standard and xiangqi lose is open. FS changes four things
  at once: table cuts on soft bounds, shortcut scores (the static score
  cut returns the evaluation), node scores, and what the correction
  history learns from. Next: split it, the table and node scores alone
  against the shortcut scores, to find which part loses.
- LG on the server (x86, idle; bench, five seeds): nodes equal build
  19; time x0.970 standard (depth 10), x0.908 xiangqi (9), x0.933 grand
  (8), x0.977 shogi (9). Confirmed.
- Merged build 20 (`plan27-main20`, 94ffe79) = build 19 + HK + LG.
  Bench nodes equal build 19 in all 44 variants (HK changes only the
  second and later searches of a session).
- CO on build 20 (`plan29-co20`, f7aea13; the capture history is kept
  between moves as HK's tables are). Cost to depth against build 19, CO
  on build 19: standard x1.020, xiangqi x0.873, grand x0.912, shogi
  x0.911. Queue157 plays CO and build 20 against full FSF, three rounds.
- CO against full FSF, three rounds (queue157; W-L-D, build 20 then
  CO): standard 325-226-49, 358-188-54 (+43.4 ± 19.7); xiangqi
  360-233-7, 344-247-9 (-18.0 ± 20.3); grand 358-209-33, 372-190-38
  (+20.7 ± 20.2); shogi 315-284-1, 290-309-1 (-29.0 ± 20.1). Mean +4.3 ±
  10.0; not merged. Standard gains in all three rounds and xiangqi loses
  in all three; standard is the variant where CO's time to depth did
  not fall (x1.02), so the gain there is from the order itself. Why the
  same order loses in xiangqi and shogi is open.
- Queue158: build 20 and build 14 against FSF at UCI_Elo 2000 (the user
  asked for this yardstick once), interleaved, five rounds of 200 games
  in each key variant, with build 14's own settings (concurrency 15,
  screen books, 800 plies). Build 14's earlier results on it: standard
  +37.3 ± 21.7, grand -60.4 ± 21.8 (1000 games each), shogi near -63
  (2000 games); xiangqi was not run.
- Queue158 result, 1000 games each against FSF 2000 (W-L-D, build 14
  then build 20; no forfeits): standard 527-430-43 (+33.8 ± 10.8),
  564-398-38 (+58.2 ± 10.9), gain +24.4 ± 15.4; xiangqi 540-454-6
  (+30.0), 601-392-7 (+73.7), gain +43.7 ± 15.7; grand 371-594-35
  (-78.8), 459-509-32 (-17.4), gain +61.4 ± 15.5; shogi 404-596-0
  (-67.5), 493-507-0 (-4.9), gain +62.7 ± 15.7. Mean gain +48, up in all
  four. Build 14 repeats its own earlier results (standard +37, grand
  -60, shogi -63), so the yardstick held. The gains are near 40 to 80%
  of the progress table above (+57, +71, +78, +140): that table leans
  high by selection, and FSF 2000 is a different opponent from full FSF
  on a node budget, so the two need not agree.
- Three candidates on build 20, earlier tries read first (none had
  one): LM2 (`plan29-lm2`, acb1b3e: late move reduction from the second
  move at a zero window, as FSF's `moveCount > 1`; wide windows keep
  four full moves), CO2 (`plan29-co2`, 977eed1: CO without the depth-1
  skip, the order alone) and PB (`plan29-pb`, 198bf79: a node of depth 3
  or more where no move beats alpha credits the quiet move that led to
  it in that move's continuation cells, as FSF does). Time to depth
  against build 20: LM2 x1.021, x0.900, x0.994, x0.910; CO2 x1.020,
  x0.924, x0.921, x0.902; PB x0.953, x0.951, x0.994, x0.927.
- Queue159 against full FSF, three rounds (W-L-D; build 20 standard
  309-240-51, xiangqi 335-254-11, grand 316-245-39, shogi 275-323-2):
  LM2 -11.7, -16.4, -16.4, -0.6, mean -11.3 ± 9.9; CO2 -4.1, -31.0,
  -12.9, -29.4, mean -19.3 ± 9.9; PB +32.1, -29.2, +5.9, +8.7, mean +4.4
  ± 9.9. None merged. CO2 loses where CO with its skip was even: the
  order alone is worse, so CO's standard gain did not come from the
  order. Why LM2 and PB do not pay is open; LM2's faster depth in
  xiangqi and shogi bought nothing, as RI's did.
- Profile of build 20 (macOS `sample`, symbols kept, 10 s searches): the
  largest named cost after `alpha_beta` itself is the skip test's scan
  for a line from the landing square to an enemy royal (11 to 14% in
  standard, xiangqi and shogi): each skippable move read every attack
  entry of each enemy royal square. The check tests come next (near 12%).
- RL (`plan29-rl`, d9385ab): a table of royal lines, one bit for each
  side, royal square, piece and start square, set from the attack lists;
  the skip test reads one bit for each royal. Kept for boards of at most
  `CAPTURE_REACH_WORDS` words (12 kB in standard, 36 kB in shogi), as the
  capture reach is; larger boards read the lists as before. Nodes equal
  build 20 in all 44 variants. Time to depth against build 20: standard
  x0.963, xiangqi x0.901, grand x0.947, shogi x0.902; on the server
  (bench, five seeds, idle) x0.937, x0.977, x0.932, x0.931.
- Merged build 21 (`plan27-main21`, d9385ab) = build 20 + RL, the same
  nodes. At the ladder rate RL is worth near +7, +3, +8 and +9.
- Archive: all results of queues 155 and 156 and the 900 older result
  files left on the server are in `p27b-plan29c`; the 31 finished clones
  are in `clones/` (without builds). The server keeps the harness
  (`aneka-b14`), the book builder's clone (`aneka-final`) and build 20.

Progress since build 14 (the start of play against full FSF), by
variant. Games give a merge's paired result; a change with the same
nodes gives its time at the ladder rate (75 standard, 82 shogi, 78
assumed for xiangqi and grand):

| Build | Change | standard | xiangqi | grand | shogi |
| ----- | ------ | -------- | ------- | ----- | ----- |
| 15 | CK (speed) | +8 | +4 | +3 | +2 |
| 16 | H3 (games) | +17 | +19 | +28 | +29 |
| 17 | LS2 (games) | +35 | +16 | +10 | +35 |
| 18 | CX, EC (speed, server) | +18 | +4 | +5 | +10 |
| 19 | DS (games, shogi only) | 0 | 0 | 0 | +21 |
| 20 | HK (games) | -24 | +18 | +24 | +40 |
| 20 | LG (speed, server) | +3 | +11 | +8 | +3 |
| 21 | RL (speed, server) | +7 | +3 | +8 | +9 |
| | total | +64 | +74 | +86 | +149 |

Each total carries near ± 30 from its game results, and a merge is
chosen when it measures well, so the totals lean high.
- H1 failed because the cells it sums were near empty; build 16 fills
  them. H1b (`plan29-h1b`, 8c2c28e, H1 merged onto build 16) plays in
  queue136 after queue135, as H2b. GitHub refused its push with server
  errors, so the server built it from a git bundle.

CR after AM: the attack maps alone change no move, so CR3's result
stands for this build. What the lab says may change it is king safety
on the maps; that is step 5, CR with KS2 together.

Port 2 (M3), earlier tries: plan 13 Q-3 merged the passed, protected,
chained and connected terms into one pawn table; E1 (+165 grand) took
out a passed bonus that was counted twice; Phase 1 found M3 (a passer
is valued as unstoppable, up to doubled when connected and chained;
game 575: four pawns against a rook, ours +7.6, FSF -2.69). No try has
read the pieces. FSF's passed term (229 in standard, 150 in grand) adds
path tiers from attacked and defended squares, king distances to the
stop square and a halving for candidates; its tier numbers are FSF's
tuning, so they are not copied.

M3 design (rule prior, no new weight): a passed pawn is worth its
promotion gain times the share of its walk that it can make safely,
`(safe steps + 1) / (path length + 1)`. The safe steps are the path
squares before the nearest one that an enemy piece attacks or occupies
(distance in king steps, so it holds for any one-step pawn). The pawn
cache keeps the full value; a new per-evaluation term subtracts the
unsafe share, and builds the enemy attack map only for a side that has
a passer.

M3 (`plan29-m3` on `plan29-am`, f78363e): `passer_walk!` as designed,
in `evaluate_position!`. The plan 27 example `N7/4n3/2p5/3p1k1p/3K4/8/8/
5B2 b` (FSF +7 cp): build 15 444 cp, M3 131 cp. NPS from the start
position: standard -7%, grand -8%, shogi -6%. Queue131 plays M3 and
build 15 against full FSF after queue130, as H1 and H2.

M3 against full FSF, three rounds of 200 games each (W-L-D):

| Variant | build 15 | M3 | difference |
| ------- | -------- | -- | ---------- |
| standard | 300-249-51 | 302-251-47 | 0 |
| xiangqi | 297-292-11 | 281-303-16 | -16 |
| grand | 295-269-36 | 269-298-33 | -32 |
| shogi | 252-348-0 | 254-346-0 | +2 |

- No gain, mean -11; not merged. The loss is in grand, where FSF's
  passed pawn term is worth 150. In a middlegame nearly every path
  square is attacked by some enemy piece, so a far passer falls to
  1 / (path length + 1) of its value; a defended path counts the same
  as a blocked one. M3 now values passers too low.
- M3b: a path square is unsafe only when an enemy piece holds it, or an
  enemy piece attacks it and no own piece does.
- M3b (`plan29-m3b` on M3, 0bf7253, with `and_not!` in `board.rs`):
  the closure builds both attack maps once for a side with a passer.
  The plan 27 example stays at 131 cp (no own piece defends those
  paths). Queue134 plays M3b and build 15 after queue133.

H2 (`plan29-h2`, ef88f9c, on build 15): a late quiet move is skipped
when both of its continuation cells are negative and its reduced depth
(depth less one less the quiet surface) is below `FAILED_REPLY_DEPTH`
= 5, as FSF's continuation pruning (`chprune`: 18 to 66 in FSF). Gated
by `quiet_pruning!` as the move count skip is; a skipped move that may
give check is still made and tested. Queue130 plays it as queue129 does
H1, after queue129.

H2 against full FSF, three rounds of 200 games each (W-L-D):

| Variant | build 15 | H2 | difference |
| ------- | -------- | -- | ---------- |
| standard | 277-259-64 | 320-238-42 | +37 |
| xiangqi | 297-290-13 | 290-294-16 | -6 |
| grand | 281-288-31 | 281-292-27 | -2 |
| shogi | 247-353-0 | 276-319-5 | +37 |

- Mean +16.5 with near 10 of standard error (1.6 sigma): short of the
  merge rule. Queue133 adds rounds 4 to 6 after queue132.
- Build 15 in shogi moved from -10 (H1 session) to -62 (H2 session) on
  the same budget and book: a reason the base plays in every session.

`e_pieces` holds generic terms (a piece in or near its promotion zone
gets a share of the promotion gain; demotion and promoted captures; in
drop variants a piece far from both kings costs) and terms for named
pieces (outposts, bishop pawns, long diagonals, rook files, trapped
rook, queen pins, fers and silver distance, connected soldiers). Only
the generic ones are port candidates; the named ones are out.

- Continuation history is worth 166 to 293 in all four, the largest
  switch that needs no rule knowledge. FSF keeps plies 1, 2, 4 and 6
  and uses them in the move order, in a pruning rule (`chprune`) and in
  the LMR history score (`lmrhist`). Ours keeps plies 1 and 2 for the
  order and gives LMR only the sign of the history.
- Singular extension is worth 32 to 55 in standard, xiangqi and grand
  and loses 39 in shogi; a port needs a rule gate (drops are the first
  guess).

- Our check rule costs FSF 108 in standard, 161 in grand and 503 in
  shogi (17 wins in 400), in the order of our own gaps to FSF. CR (our
  engine without the LMR part) lost 51 in shogi, so our eval does not
  cover what FSF's covers there.
- The parts: pruning costs the most in standard, grand and shogi. The
  extension alone costs nothing anywhere. In shogi the parts add to
  -261 and the whole to -503: an extended check that is also never cut
  grows the tree more than each rule alone.
- In shogi FSF loses 92 when it stops reducing checks; our engine lost
  51 when it started (CR). The same change has opposite signs, so some
  part of FSF makes the reduction safe. Queue123 runs `e_king` and
  `s1,e_king` (if s1 costs much less without king safety, the king term
  is that part), then the pairs `s1_pe` (our engine after CR), `s1_le`
  (checks may be pruned) and `s1_pl` (no extension) in standard, grand
  and shogi, before queue121 resumes.
- King safety is worth 295 to FSF in standard, 299 in grand, 95 in
  xiangqi and 52 in shogi. Without it, s1 costs less: standard 35 (108
  with it), grand 62 (161), shogi 258 (503).
- Shogi with s1: king safety on -503, off -310. In a search that never
  cuts a check, FSF's own king safety loses near 190 in shogi. Our shogi
  king terms on S1 (SD, SD2, KH, PX, HB) all lost; this is the same
  pair. In standard and grand king safety stays worth near 200 with s1.
- These differences come from separate runs at extreme scores, so
  queue125 plays them directly: `s1_eking` against `s1` and against
  `e_king`, in standard, grand and shogi.

King safety played directly, `s1_eking` against `s1` (no king safety
against king safety, both with our check rule): standard -160.9 ± 33.0,
grand -97.1 ± 34.9, shogi +140.1 ± 37.2. Under our check rule king
safety is worth 161 in standard (295 with FSF's rule), 97 in grand
(299), and in shogi it loses 140 (+52 with FSF's rule). This is the
cause of the shogi king term losses (SD, SD2, KH, PX, HB): a king term
in a search that never cuts a check. The order is fixed: the check rule
first, king safety after.

Pairs of s1 parts against the base (the part left out is FSF's own):

| Pair | standard | grand | shogi |
| ---- | -------- | ----- | ----- |
| s1 (all three parts) | -108.5 ± 23.9 | -160.9 ± 31.7 | -503.0 ± 79.7 |
| s1_pe (checks may be reduced, our engine after CR) | -51.6 ± 22.2 | -138.0 ± 31.3 | -291.0 ± 47.2 |
| s1_le (checks may be pruned) | -17.4 ± 20.9 | -40.1 ± 28.6 | -123.0 ± 33.2 |
| s1_pl (FSF's check extension) | -73.2 ± 24.3 | -127.0 ± 31.3 | -390.8 ± 59.6 |

- Letting checks be pruned gives back the most in all three: 91 in
  standard, 121 in grand, 380 in shogi. FSF prunes a check by SEE
  (below -218 times the depth) and by the move count; futility and
  history pruning never skip it. Our S1 skips no check at all.
- That is the first port, but FSF's gain is not ours: in FSF letting
  checks be reduced gave back 212 in shogi, and CR lost 51 in ours.
  Queue125 tests a proxy, FSF like our engine (`usbase`: no king
  safety, mobility, threats or space, with s1). The proxy must first
  lose with CR in it (`us_pe`), as our engine did; then `us_le` gives
  the port's expected gain and `us_king` the worth of king safety there.

Each lab switch against what our engine already tried (plan 04 deltas
are FIDE round robins, near ± 8 each; plan 27 to 29 are SPRTs):

| Switch | Tried in our engine | Result |
| ------ | ------------------- | ------ |
| s1 | S1 + X1 kept; CR, CS, XS, XS2 | +33 to +78; CR -51 shogi, XS -17 grand, XS2 -2 |
| e_king | plan 04 O, P; KD, DW, PX, SB, SD, SD2, KH, HB | +27, +21 in FIDE; later 0 to -306 |
| singular | plan 04 H | delta -5 ± 8 (the -19 is its pool rank), never shown to lose |
| caphist | plan 04 G | -11 to -14, removed |
| conthist | plan 04 B | +14, kept |
| countermove | plan 17 K-5 | never run |
| lmradj, lmrhist | plan 04 C (-10), plan 03 V (worse), HL (+11 ± 10) | none kept |
| seequiet | QP | -2 ± 22 grand |
| nmp, iir, probcut | plan 04 E, A, plan 03 | neutral, kept |
| e_mobility | plan 03 U | not run; excluded on cost (55% time to depth) |
| e_threats | TH | dropped without a run |
| e_passed | E1 (double count removed) | +165 grand; M3 open |

- A lab value is not our gain: king safety is +52 in shogi with FSF's
  check rule and -140 with ours. A port can also lose through a
  different implementation, and the old numbers are self-play.
- So the proxy must first reproduce the in-engine results it can
  emulate (CR in shogi, king safety under S1, XS and XS2); each port is
  then judged by games against full FSF, and its record row is read
  before it is built.

CR inside the proxy (`us_pe` against `usbase`): standard +43.7 ± 25.4,
grand +71.3 ± 31.0, shogi +240.8 ± 41.8. Our engine lost 51 in shogi
with CR, so the proxy fails its test: something in our engine that the
proxy lacks makes a reduced check unsafe in shogi. The proxy moves
toward our engine one difference at a time until CR loses:

- `usq`: our quiescence. `qsnodrop` plays no drop out of check (FSF's
  quiet checks include drops), `qsquiet` plays quiet checks only when
  the stand pat is not below alpha, `qssquare` captures only on the
  square of the exchange below the first ply.
- `usl`: our LMR, the base table without FSF's adjustments or history.
- Queue126 plays CR inside each in shogi (`usq_pe` against `usq`,
  `usl_pe` against `usl`).
- Results: with our quiescence +200.2 ± 38.3, with our LMR +247.5 ±
  43.1. Neither is the cause. The proxy still gains near 200 to 250.
- The only evidence that CR hurts our engine is one self-play SPRT
  against build 10. Self-play has been wrong by near 50 before (RZ: 0
  in self-play, +29 against FSF; OS2: +48 in self-play, -26 against
  FSF). CR3 (`plan29-cr3`, 7d7dae3) is CR on build 15. Queue127 plays it
  and build 15 against full FSF on a node budget near our 50% point,
  interleaved rounds of 200 games: shogi (`nodestime` 44) five rounds,
  grand (29) three. If CR3 gains, the proxy holds and the check rule is
  the first port; if it loses, the walk-down goes on.
- Shogi, 1000 games each: build 15 455-540-5 (-29.6), CR3 443-553-4
  (-38.4), a difference of -9 ± 30. Reducing checks gains nothing in our
  engine against full FSF, where the proxy gains 200 to 250. The proxy
  fails, and its CR results in standard and grand do not hold either.
  Not yet emulated: our tree shape (10 to 46 times FSF's nodes at a
  depth), our move order (no capture history or countermove), our
  pruning margins. Next: count what a reduced check does in each engine
  on the same positions (fail high after the reduction, search again).
- A defect in CR, found in the code: a reduced move that beats alpha is
  searched again at `depth - 1`, not at `child_depth`. Under S1 no check
  was reduced, so the two were equal; under CR a reduced check that
  turns out to matter loses its check extension in the search again.
  FSF searches again at `newDepth`, extension included. CR3b
  (`plan29-cr3`, 87a1238) searches again at `child_depth`. Queue128
  plays CR3b and build 15 against full FSF, three rounds each in shogi
  and grand.
- FSF lab counters (`Lab::report`, printed with the best move): in the
  proxy with CR, at the shogi start to depth 12, 2% of reduced checks
  fail high and are searched again (85 of 4262), 6% of all reduced
  moves, and 92% of the cutoffs come from the first move.

Our engine against full FSF, rounds of 200 games, each build in the same
session (W-L-D, Elo):

| Build | shogi (`nodestime` 44) | grand (`nodestime` 29) |
| ----- | ---------------------- | ---------------------- |
| build 15 (with CR3) | 455-540-5, -29.6 | 300-259-41, +23.7 |
| CR3 | 443-553-4, -38.4 | 290-276-34, +8.1 |
| build 15 (with CR3b) | 281-319-0, -22.0 | 282-286-32, -2.3 |
| CR3b | 237-362-1, -73.4 | 293-280-27, +7.5 |

- Reducing checks gains nothing in our engine: shogi -9 ± 30 (CR3) and
  -51 ± 40 (CR3b), grand -16 and +10 (± 40). The proxy gave +240 and
  +71. The re-search with the extension was not the cause; in shogi it
  makes CR worse, as each extended check costs more again.
- Counters on 40 shogi positions, 500k nodes each (temporary counters
  in our engine, reverted; `lab_compare.py`):

  | Engine | reduced checks failing high | first move cuts | main nodes | reduced moves per main node |
  | ------ | --------------------------- | --------------- | ---------- | --------------------------- |
  | ours, CR3b | 2.6% | 94.7% | 62.5% | 0.180 |
  | FSF proxy, CR | 2.3% | 95.0% | 79.1% | 0.249 |
  | full FSF | 3.1% | 89.3% | 88.3% | 0.410 |

- A reduced check behaves alike in both engines, and the move order is
  as good. The trees differ in make-up: our quiescence takes 37.5% of
  the nodes (FSF 11.7%), and we reduce 0.18 moves for each main node
  against FSF's 0.41. The proxy does not model our engine, so a lab
  result is a ranking of FSF's own sources, not a forecast for ours.

The proxy's other runs (`usbase` is FSF without king safety, mobility,
threats or space, with s1):

| Run | standard | grand | shogi |
| --- | -------- | ----- | ----- |
| us_le against usbase (checks prunable) | +15.6 ± 24.5 | +92.5 ± 33.4 | +203.8 ± 38.0 |
| us_king against usbase (king safety back) | +251.9 ± 38.1 | +130.9 ± 35.5 | -240.7 ± 41.6 |
| usbase against full FSF | -570.3 ± 90.1 | -410.0 ± 59.7 | -273.4 ± 44.2 |

- King safety under our check rule loses 241 in shogi in the proxy too;
  there the proxy agrees with our engine (SD, SD2, KH, PX, HB).
- The proxy is 570 below full FSF in standard, 410 in grand and 273
  in shogi, near the size of our own gaps. A match of the proxy against
  our engine shows how near; it follows the map.

### Code review of plans 27 to 29c (2026-10-10, branch `plan29-review`)

The user read the full diff since plan 27 and found drift from the
style and the design. Four commits on build 21 (`plan27-main21`,
d9385ab), 633 lines added and 1010 removed:

- 5ddb0f3: EC is removed. The tables already keep the scores that
  repeat: the main table stores the static score of each main node.
  From the build 17 timings (CX alone against CX with EC), EC saved
  0.2, 0.3, 0.6 and 2.6% of the time in standard, xiangqi, grand and
  shogi, near +3 Elo in shogi and below +1 elsewhere. It also needed
  its own key, `eval_key`, which is gone. Nodes equal in all 44.
- 41ab60a: the move list fills (`push_board_moves!`, `push_drops!`) and
  the history step (`update_history!`) are macros; `join_drops` is
  inlined; `generate_all_drops` is `generate_drops`. In `make_move!`,
  the closure `royal` is replaced by named facts (`moves_royal`,
  `promotes_royal`, `takes_royal`, `royals_stay`, `changed_squares`).
  Nodes equal in all 44; all perft suites pass.
- b9c9fc5: the match harness is smaller (sprt.rs 1831 to 1658 lines).
  A forfeit is one variant with a reason (`Illegal`, `Time`, `Crash`);
  the engine error type, the pentanomial floor, the log harvest and the
  book are gone. A pair now plays random quiet plies that engine A
  judges at depth 8; a score within 50 is level, else up to 64 tries.
  The floor was an edge case: it kept a run where all pairs score the
  same from a zero variance, but it biased small samples. Tested with
  8 games each in standard, xiangqi and shogi: first-move scores within
  about 60, no forfeits, the sandbox deleted.
- 805b0fb: the doc blocks touched since plan 27 follow the style again:
  one-line list items, aligned Params and Return, short notes.

- Queue scripts must not pass `--book` any more; the flag is rejected.
  The server's `~/books` and the `aneka-final` clone are only for the
  book builder; archive them, then delete them, and take the book lines
  out of `~/p27b/status.sh`. The server did not answer on 2026-10-10.
- The next build line starts from `plan29-review`: the same nodes as
  build 21, with near 3% more time in shogi.
