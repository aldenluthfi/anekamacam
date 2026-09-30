# Piece value model

## Status

Opened and closed 2026-09-30. Not kept. A fitted model does not predict
piece values of an unseen variant better than the current derivation, and
it gives wrong values to unusual pieces. The current derivation stays.

## Context

Each value fix of plan 27 (P1, CS, SP, PV) helped one variant and moved
the others. The user asked for a small learned model: predict the value
of a piece from rule features and from the rest of the army, with weights
fitted one time to known values. The goal of the engine is strength in
any variant on first load, so the test is the error on variants that the
fit did not see.

## Method

- Features for each non-royal piece and phase, from the rules: expected
  moves (all, empty board), capture and slider share, direction-family
  synergy, reach, reversible share, moves against the army mean and
  maximum, promotion gain and distance, drops, board size, phase, and the
  number of copies in the setup.
- Model: `ln(value) = w · features + b`, ridge regression.
- References (9 variants, 61 pieces): FSF `types.h` for standard, grand,
  Capablanca, amazon, shatranj, makruk; Kaufman for shogi (P1 L4 N5 S7 G8
  B11 R13, tokin 10, +L 9, +N 9, +S 8, horse 15, dragon 17); ubdip for
  crazyhouse (P1 N1.73 B1.69 R3.05 Q3.94); common xiangqi values (R9
  C4.5 N4 A2 E2 P1, endgame P2). FSF gives the xiangqi advisor its
  generic fers value, which a palace piece does not have.
- The references do not share a unit, so each variant gets its own level
  in the fit. Only the ratios inside a variant count. The error is the
  mean `|ln(predicted / reference)|` after both sides are centred per
  variant.

## Results

Error on each variant when the fit leaves it out, against the current
derivation (the shift map of plan 27, which saw none of these tables):

| variant    | current | model, rule features | model, ln(current raw) + terms |
| ---------- | ------- | -------------------- | ------------------------------ |
| standard   | 0.114   | 0.096                | 0.134                          |
| grand      | 0.106   | 0.062                | 0.179                          |
| Capablanca | 0.100   | 0.058                | 0.180                          |
| amazon     | 0.152   | 0.100                | 0.104                          |
| shatranj   | 0.102   | 0.219                | 0.137                          |
| makruk     | 0.107   | 0.204                | 0.127                          |
| xiangqi    | 0.258   | 0.481                | 0.432                          |
| shogi      | 0.239   | 0.272                | 0.227                          |
| crazyhouse | 0.223   | 0.225                | 0.194                          |
| mean       | 0.169   | 0.192                | 0.190                          |

- No feature set got below 0.19 on unseen variants. The in-sample error
  (0.13) is lower, so the model learns the reference tables, not a rule.
- The shogi knight stays at about half of its Kaufman value in every fit.
  Its forward jumps give few moves, and no rule feature sees what makes it
  strong.
- Unusual pieces outside the fit break: janggi cannon 12 and elephant 472
  (current 288 and 231), chu shogi lion 854 (current 2475, the strongest
  piece of the game). The current derivation keeps them in order.

## Noise floor

The reference tables disagree with each other about as much as we
disagree with them (same error): standard FSF against Kaufman 0.218,
xiangqi FSF against common values 0.166, shogi FSF against Kaufman 0.079.
A better fit to one table is not a better value.

## DC: hand material (2026-09-30, branch `plan27-dc`, 1885c4e)

- Two rules, only with drops. A captured piece comes back to the hand of
  the capturer and can drop on any empty square, so a slow piece loses
  less of its worth: the value over the cheapest piece gets the power 0.7
  (FSF compresses drop variants the same way). A piece whose capture
  gives only its demoted form (tokin to pawn) keeps half of the
  difference as extra value.
- Error against the references: shogi 0.239 to 0.178, crazyhouse 0.223
  to 0.119 (power alone: 0.191 and 0.119; demotion alone: 0.231). Mean of
  all 9: 0.169 to 0.151.
- Shogi: P 100, L 121, N 108, S 225, G 250, B 272, R 381, tokin 311,
  horse 503, dragon 532. The pawn is dear against the rook (Kaufman 1 to
  13, now 1 to 3.8). Crazyhouse: N 257, B 255, R 359, Q 565.
- Only the 7 drop variants change their params. Perft unchanged.
- SPRT shogi `0 5` against merged build 4, then crazyhouse `-5 0`.

## Removal scan of all variants against FSF (2026-10-01)

- For each variant that FSF plays (36 of 44), remove one of each White
  non-royal piece from the start position in both engines, and compare
  the eval drops as ratios, centred per variant. Pawn drops are too noisy
  (FSF: xiangqi soldier 8, shogi pawn -16) and do not count.
- ASEAN: the config gave the bishop and queen their chess moves. In ASEAN
  they move as the khon (`F|nW`) and the met (`F`), the pawn promotes to
  R, N, B or Q, and a bare king is counted as in makruk. Fixed on branch
  `plan27-asean` (1ddfd34); perft equals FSF to depth 4 and on a
  promotion position. After the fix all ASEAN pieces are within x1.1.
- Other outliers past x1.5: Hoppel-Poppel knight x0.57 and New Zealand
  rook x1.61 (a piece that moves one way and takes another: FSF prices
  the capture part above the quiet part, we count each half), knightmate
  commoner x1.52, xiangqi horse x1.65 and advisor x0.61. All other pieces
  in 34 variants are within x1.5. Janggi and sittuyin start with a setup
  phase and horde has one piece type, so the scan says little there.

## Conclusion

The current derivation is a better prior for an unknown variant than a
model fitted to 9 tables. The value work of plan 27 stays. A new value
term must be a rule that holds in every variant; checks for it are the
removal test against FSF and a look at the unusual pieces of chu shogi,
janggi and tjatoer.
