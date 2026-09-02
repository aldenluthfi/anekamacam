# Exchange oracle: settling the SEE half of the frozen baseline's known defect

## Why

`plans/18-frozen-baseline.md` deferred one gap: search shortcuts apply to every
variant unconditionally. Its largest claimed component was that the static
exchange evaluation is asked about positions its model does not describe. Two
attempts to characterise that by reading code and hand-building positions both
reached wrong conclusions, so it was settled by measurement instead.

## Method

A throwaway build carrying two independent oracles, compared against `see!`
over every legal capture in each variant's perft suite, extended to the
position limit with `bench_walk_fen`. Neither oracle shares code with `see!`.

- **Exact oracle** — the true material outcome of an exchange on one square:
  exhaustive minimax over every legal capture order, either side free to stop
  at any point.
- **LVA oracle** — the same, restricted to the least-valuable-attacker order
  `see!` assumes.

The pair is what makes the result readable. Disagreement with the exact oracle
alone is the known, accepted cost of the LVA heuristic. Disagreement with the
**LVA oracle** is `see!` failing to implement its own algorithm.

Run at `348e2d8`, `ANEKAMACAM_SEED=42`. The throwaway was deleted after the run
and reached no commit.

## Results

| Variant | Positions | Captures | vs exact | vs LVA | Worst |
| ------- | --------- | -------- | -------- | ------ | ----- |
| standard | 6752 | 18882 | 1050 | 1047 | +604 |
| minishogi | 2000 | 5243 | 502 | 500 | -42 |
| sittuyin | 2000 | 1508 | 1345 | 1345 | +5 |
| janggi | 2000 | 135 | 102 | 102 | +9 |
| xiangqi | 2000 | 3111 | 54 | 54 | +22 |
| minixiangqi | 2000 | 3635 | 14 | not run | +100 |
| los-alamos | 2000 | 6676 | 3 | not run | +100 |
| capablanca | 2000 | 5133 | 0 | 0 | 0 |
| crazyhouse | 2000 | 3637 | 0 | 0 | 0 |
| shogi | 2000 | 908 | 0 | not run | 0 |
| shatranj | 2000 | 1509 | 0 | not run | 0 |
| makruk | 2000 | 2153 | 0 | not run | 0 |
| grand | 2000 | 2636 | 0 | not run | 0 |

Where both oracles were run the two columns are nearly identical, so the LVA
heuristic explains essentially none of the disagreement.

## Root cause

`p_value!` (`src/game/representations/piece.rs:40`) returns `ovalue` in OPENING
and SETUP, `evalue` in ENDGAME, and **interpolates by `state.phase_score` in
MIDDLEGAME**. `phase_score` falls as pieces leave the board, so a piece is
worth slightly less once the exchange has begun than it was when the exchange
was priced.

`see!` reads `initial_attacker` and `initial_attackee` once, before the first
`make_move!`, and telescopes the swap list with those figures. The true outcome
prices each piece at the phase in effect when it actually leaves. The two
therefore differ by the amount the phase moved during the exchange.

Confirmed by phase, not by argument: the disagreeing standard positions
`3Q4/3n4/3k4/7P/3P4/N3K3/8/8 w` and `8/6p1/8/qR3k2/2K3n1/8/8/3b4 b` both report
**Middlegame**, while the agreeing capablanca and crazyhouse walk positions
report **Opening**, where the interpolation does not run and `see!` is exact.

Worked example: `d8*d7`, queen takes knight, king recaptures. `see!` gives
−825, both oracles give −844. `see!` prices the queen before the knight leaves;
the truth prices it after, 19 units lower on a queen worth about 1174.

## What this settles

**There is no variant-specific SEE defect.** The inaccuracy is universal and is
worst in **standard chess** — 1047 of 18882 captures, 5.5%, worst 604 units,
roughly half a piece. It is not caused by any rule any variant declares.

**Screens are exonerated, again and by a second method.** xiangqi disagrees on
1.7% of captures and minixiangqi on 0.4%, both *better* than standard's 5.5%.
The abandoned iteration disabled the exchange on the screened variants and paid
up to +54.6% nodes in xiangqi to fix something that was never wrong.

**Drops do not disturb the exchange mechanically.** crazyhouse, shogi and grand
disagree zero times.

**janggi and sittuyin are noise.** Rates are high, 76% and 89%, but the worst
deltas are 9 and 5 units against piece values in the hundreds. Both sit in
MIDDLEGAME almost always, so the phase term is nearly always active; the error
is the same universal one, not a rule interaction.

## What this does not settle

The oracle counts board material, exactly as `see!` does, so it is blind to
every question about whether material is the right currency:

- whether a capture that fills a hand is worth more than its victim, in
  crazyhouse, the shogi family, `grand` and `sittuyin`
- whether `extinct`, `goal` or `checks` variants should let a material score
  decide a capture at all

Neither can be answered by a material oracle. Both concern SEE-based *pruning*,
which the ablation removed and which does not exist in the frozen engine, so
neither bites today. They need game evidence when pruning returns, not an
exchange oracle.

## Recommendation

The phase inconsistency is real, universal, and currently affects move ordering
only. Two defensible repairs: hold `phase_score` fixed for the duration of an
exchange, or price each piece as it leaves. The first is cheaper and matches
what the swap list already assumes.

It should be its own change with its own before-and-after over this same
oracle, and it is not a capability question. Whatever remains of the frozen
baseline's deferred item after this document is about pruning currency, not
about the exchange.
