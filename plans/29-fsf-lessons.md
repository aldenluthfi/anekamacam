# Lessons from Fairy-Stockfish

## Status

Opened 2026-10-04. Stage 0 (this document) is done. Stages SD, DW, LA,
GA, CD, EX and TH are next, in one server queue.

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
| Reduce checks, extend few (the FSF form) | XS, XS2 | grand -17 stopped; shogi -2 | excluded, against S1 |
| Quiet and drop checks at the first qsearch ply | X1, D1 | X1 kept; D1 +0.9 | done |
| Singular, multicut, double extension | plans 2, 13, 16 | -19, removed | excluded |
| Capture history | plan 4 G | -14, removed | excluded |
| Countermove | plan 17 K-5 | never run | not in this plan |
| Mobility | plans 3 U, 13 | 55% time to depth | excluded until attacks are cheap |
| Hanging pieces | never | | stage TH |
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
| DW | drops | danger, cap, shelter, guard x `DROP_SAFETY_RATIO` with free drops | shogi `0 5`, crazyhouse `-5 0` |
| LA | goal, wide | loss analysis against FSF, no code; gates GA, CD, EX, TH | |
| GA | koth | goal steps for each goal piece from its moves; attacked squares add a step | koth `0 5` |
| CD | n-check | danger and cap x `(10 + left) / (3 + left)` | threecheck `0 5`, fivecheck `-5 0` |
| EX | extinction | attacked vital pieces cost `value / left^2` | extinction `0 5`, kinglet, horde `-5 0` |
| TH | wide, xiangqi | attacked, undefended pieces cost `hanging_value` | grand `0 5`, standard, xiangqi, shogi `-5 0` |

Acceptance (user choice, hybrid): self-play SPRT under the plan 27
rules. After an H1, base and candidate each play 400 games against FSF
2000 in the target variants. The stage merges only if the candidate is
not below the base in each. A stage tied to a rule keeps the seeded
node counts of the variants without it. Perft unchanged, build
warning-free, params regenerated.

## Results

None yet.
