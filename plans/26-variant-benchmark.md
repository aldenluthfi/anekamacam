# Variant benchmark against Fairy-Stockfish

## Status

Opened 2026-09-27. Harness done. Xiangqi, shogi and grand rated on the
local machine (10 cores, 4 P + 6 E), 2026-09-27 to 2026-09-28. Paused by
the user after the grand 1765 step, which was stopped before a verdict.
Standard was rated locally on 2026-09-28 as the control (one SPRT at 1967).

Time-loss bias: the harness charges the wall time. With 8 slots on mixed
cores, FSF sometimes loses on time. In the standard run this was 28 of 2028
games, against 3 for us. The controller logs of the earlier runs were
rotated before a count was made, so their bias is not known.

Engine logs for analysis: the logs of the last finished run of each variant
are `res/sprt/<variant>/[0-7]-engine-a_latest.log` (xiangqi 1794, shogi
1830, grand 1780). A stopped run does not harvest its logs.

## Context

Standard chess (HEAD 451dec3) rates about 1967.5 ±2.5 on the UCI_Elo scale
of Fairy-Stockfish 14.0.1 XQ at 10+0.1 (cutechess 1.5.1 on the VPS). The
VPS is gone. Cutechess has no xiangqi, so the other variants use the
built-in `debug-headless sprt`, which played one game at a time.

FSF calibrates UCI_Elo on standard chess. A rating in another variant is a
position on that scale, not a chess-comparable Elo.

## Harness

`debug-headless sprt` changes:

- `--concurrency n`: n match slots, one thread each, each with its own two
  engines. Slots take pair numbers from one counter. The runner owns the
  tally and stops all slots at a bound.
- `--option-a name=value`, `--option-b name=value`: `setoption` after the
  variant and thread count, repeatable.
- Sandbox per slot and side: `/tmp/anekamacam-sprt/<pid>/<slot>-engine-x`.
  Removed after the log harvest. Fixes the startup crash of two engines
  that roll one `logs/latest.log`.
- No `uci` argv. FSF runs argv as one command and exits.
- Start FEN goes through the dict, so FSF reads it in its own notation.
- Tally keeps pentanomial counts; pair mean and variance come from them.
  Result file adds `pentanomial (A)` and `elo (A): x +/- 95%`.
- Loss on time and loss by illegal move are logged with slot and binary.

Notation check: FSF move sequences for xiangqi, shogi and grand parse in
the engine with `--protocol uci`. Smoke runs had no illegal-move loss.

## Method

```text
debug-headless sprt <variant> <head> fairy-stockfish 10000+100 3000 -5 5
    --concurrency 8 --option-a Hash=64 --option-b Hash=64
    --option-b UCI_LimitStrength=true --option-b UCI_Elo=<anchor>
```

Bisection over `UCI_Elo` in [1000, 2850]: H1 raises the low end, H0
lowers the high end, stop at a 10 Elo bracket or an inconclusive test.
Random harness openings (`OPENING_RANDOM_PLIES`), each played with both
colours. Engine is HEAD 451dec3 plus the harness change (engine code
unchanged).

## Results

| variant | bracket (FSF UCI_Elo) | notes |
|---------|-----------------------|-------|
| standard | > 1967, ~1982 (~1977) | H1 1967 +14.8 ± 13.9 (1022W 934L 72D), 2026-09-28; FSF lost 28 games on time and we lost 3, which adds about 5 Elo for us |
| xiangqi | [1794, 1809], ~1800   | H1 1794 +12.0 ± 12.5; H0 1809 -12.5 ± 12.7 |
| shogi   | [1823, 1838], ~1824   | H1 1823 +51.6 ± 26.3; H0 1838 -26.6 ± 18.6; 1830 inconclusive at 3000 games, -6.5 ± 12.3 |
| grand   | [1751, 1780], ~1760   | H1 1751 +19.8 ± 16.0; H0 1780 -25.9 ± 18.4; 1765 stopped by user at 2624 games, -5.8 ± 12.9, LLR -1.35 |
