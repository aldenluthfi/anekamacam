# Lessons from Fairy-Stockfish

## Status

Opened 2026-10-04. Merged build 11 = build 10 + GA + EX + the referee
fixes. Not merged: SD, SD2, DW, CD, SB. TH dropped (see LA). Running or
queued: TI, CR, SD at 30+0.3. Planned: CS, FL.

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
| Singular, multicut, double extension | plans 2, 13, 16 | -19, removed | excluded |
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

### Screen and KH chain (2026-10-04, records on `plan29-kb`, c0e7358)

- The 600-game screen at 5+0.05 passes D5 on 12 past branches
  (Spearman 0.97, no sign miss); a screen gain still needs the SPRT.
- KH, KH2, KH3, KB, HR: the hand term of `king_danger!` reads the best
  drop square, so each drop lowers the danger and the engine keeps its
  pieces in the hand. Less hand weight was better at each step.
- KB shogi SPRT `0 5` at 10+0.1: inconclusive at 4000 games,
  -10.2 ± 10.7. Not merged. KB and HR screen +76 and +57 in crazyhouse.

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
