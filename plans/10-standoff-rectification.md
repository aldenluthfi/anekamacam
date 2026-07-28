# 10 — Restore the CPMN stand-off (janggi bikjang) as a move-gen rule

> Repo record. The plan-mode file is kept in sync with this.
> Sittuyin promotion moved to `plans/11-sittuyin-promotion.md`.

## Context

Plan 08 (commit `b286c17`) deleted janggi's stand-off (bikjang) machinery on the
belief that *"a persistent facing position repeats, so the repetition rule
already draws it."* That reasoning is wrong on two counts:

1. **Bikjang is a move-generation rule, not a terminal.** Once the two generals
   face on a clear line, the side to move must *break* the facing — every
   non-pass move that keeps them facing is **illegal** — or pass to accept it.
   The draw is incidental (only on the accepting pass); the load-bearing effect
   is a **legality restriction** on the move list.
2. **Repetition cannot subsume it.** After plan 09, repetition/perpetual are
   *on-demand* (`game_outcome`) and deliberately **do not restrict move
   generation**. So dropping stand-off enlarged the legal-move set at every
   facing node.

That enlarged move set is why `res/perft/janggi.perft` (7 positions, depths to 6)
no longer matches. **The perft files are FSF-sourced**, and FSF confirms the
old behaviour exactly (see below), so restoring it restores the match.

Xiangqi does not need this: its generals attack each other (facing = check via
the general's rook-vs-royal leg), so facing can never persist. Janggi's generals
do not attack, so facing is a legal, persistent position the next mover must
resolve — hence "stand-off".

## Why this is a general mechanism, not a one-variant rule

We are **not** adding a "royal-facing rule" (that would only ever serve the
facing family). We are re-binding the **existing CPMN pattern engine** —
`match_pattern!` (`pattern_match.rs`), `parse_pattern` (`pattern_parse.rs`),
`PatternSet`/`Pattern` (`pattern.rs`) — which is **still in the tree and already
multi-variant**: it powers drop restrictions (shogi, crazyhouse) and setup-phase
placement (janggi). Only the stand-off *binding to move legality* was removed.

Re-adding it introduces **no variant-specific engine code** (pure config + the
shared primitive), sits on the same footing as the retained config-driven
move-gen modifiers `setup phase` and `forbidden zones`, and expresses a general
"must-break-a-declared-configuration" restriction any variant can declare.

Decision (user): re-add via **CPMN patterns** (not a geometric detector);
scope **janggi-only, family-ready**.

## FSF ground truth (source of the perft numbers)

`Fairy-Stockfish/src/position.cpp`:
- `st->bikjang` = `attacks_bb(us, ROOK, enemy_general_sq, occupied) & own_general`
  — generals see each other on a clear rook ray.
- `legal()`: under a standing bikjang, `is_pass(m) → return true` (pass always
  legal); any other move whose result still has the generals facing → illegal.
- `is_immediate_game_end()`: bikjang standing **two consecutive plies**
  (reachable only via a facing-preserving pass) → empty legal list (terminal).

The old engine reproduced this: legality `!in_check && (!after || !before ||
(after && before && pass))`, and `passing_in_stand_off → draw`. Passing while
facing yields a 0-child node in both engines, so perft is identical. Re-adding
the old logic (adapted to the new code) is the proven path to the FSF match. The
accepted bikjang is scored through the existing `adjudicate: janggipts` (points,
matching FSF base `janggi`) rather than a flat draw; perft is unaffected either
way.

## Design — restore the move-gen half of `b286c17` (not the terminal table)

1. **`configs/janggi.conf`** — re-add the `stand-offs` `= rules =` token and the
   `= stand-off patterns =` section (verbatim). `= termination =` unchanged.
2. **`state.rs`** — `stand_offs!`/`enc_stand_offs!` on **bit 7**, doc/diagram,
   `relevant_stand_offs` field + init, `generate_piece_stand_off` +
   `populate_relevant_stand_offs`, `stand_off_expr_set` param on `precompute`.
3. **`pattern_match.rs`** — `is_in_stand_off!`. **`pattern_parse.rs`** —
   `generate_stand_off_patterns`, `generate_relevant_stand_offs`.
4. **`move_list.rs make_move!`** — capture `stand_off_before` before applying;
   `let legal = !in_check && (!after || !before || (after && before && pass))`;
   on `pass_move && stand_off_before`, set the accept result via
   `adjudicate_outcome` (fallback draw), eager so the pass is a 0-child leaf.
5. **`game_io.rs`** — read the `stand-offs` token → `enc_stand_offs!`; assert the
   section; build `pieces_stand_off`; pass to `precompute`.
6. **Display + docs** — `format_special_rules` shows Stand-offs; fix
   `example.conf`'s false "repetition subsumes stand-off" note and document the
   rule; update README.

## Verification

- **Acceptance gate:** janggi `perft <depth>` matches `res/perft/janggi.perft`
  through the tested depths.
- `standard.perft` + every other suite unchanged.
- `tools/run_endgame_fixtures.sh` green (janggi adjudication).
- Debug build drives a janggi endgame `go depth N` with no parity panic.
- `./anekamacam derive` parses every config; `d` shows Stand-offs for janggi.

## Out of scope (named for the record)

- Migrating xiangqi/minixiangqi flying-general onto a shared facing mechanism.
- FSF's out-of-check pass legality beyond the proven `!in_check` gate.
