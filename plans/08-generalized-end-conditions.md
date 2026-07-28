# 08 — Generalized end conditions

## Context

Grounded in the real variant catalogue ([chessvariants.com](https://www.chessvariants.com/),
[Fairy-Stockfish options](https://github.com/fairy-stockfish/Fairy-Stockfish/wiki/Variant-configuration),
[Lichess variants](https://lichess.org/variant)). Goal: generalize a **large**
variant space, not just the configs in-tree. Root blocker: every terminal can
only DRAW (`State.game_over: bool` → `draw_score!` in both search entries), so
nothing is won or lost except by the hard-coded checkmate path.

## Rule: no single-variant mechanisms, no variant names in the engine

Every terminal rule must be `(reused detector → outcome)`, **parametric**, and
cover a *family* of variants. If a mechanism serves one variant, it is either
generalized or dropped:

- **Counting is a generic material-conditioned counter, not a `makruk` mode.**
  Makruk counting is a counter that *starts on a material predicate* and whose
  *limit comes from a material table*
  ([board honour: ≤3 pieces → 64; piece honour: bare-king-no-pawns, start
  pieces+1, limit 8/16/22/32/44/64 by strongest material](https://www.chessvariants.com/play/makruk-thai-chess)).
  Cambodian/ASEAN/Sittuyin are the same mechanism with different tables. The
  engine gets one generic counter; config supplies the predicate + table (a
  config-local label, never a variant name in code).
- **`facing`/stand-off (janggi bikjang) is dropped.** No companion variant (even
  FSF hard-codes `bikjangRule`). A facing position that persists *repeats*, so
  the **repetition rule already produces the draw**. `facing` is redundant; the
  stand-off terminal machinery is removed.

## The rule set (each covers a family; detector already in the engine)

| rule (parametric) | detector | variants covered |
|---|---|---|
| **checkmate / stalemate** outcome | no legal moves + `is_in_check!` | all; antichess (stalemate=win), losing, shatar |
| **repetition** N → value (draw/loss) | `position_hash_map` | standard; drop-loss; *subsumes bikjang-style facing draws* |
| **counter**: start (always \| material-pred), limit (const \| material-table), reset pieces → draw | halfmove counter + `piece_count` | 50-move; makruk/cambodian/asean/sittuyin counting |
| **checks** N → outcome | `gave_check`/ply counter | three-check, five-check, N-check |
| **perpetual** check → checker loses; chase(mask) → chaser loses | `gave_check` / `see!` + repetition cycle | shogi, xiangqi (+ any chase variant) |
| **extinct**: side(any\|own\|opp) + piece-set + count → outcome | `piece_count` threshold | extinction chess, kinglet, horde, antichess/losers (own→win), bare-king, atomic-royal |
| **goal**: piece-set on a zone `Board` → outcome | piece-on-zone (`get!`) | king-of-the-hill, racing kings, capture-the-flag, campmate, fussball, rooksquare |
| **adjudicate**: at ply/pass limit, decisive by weighted material sum (+handicap) | material sum | janggi points, point-win variants |
| **connect-N** (later) | alignment scan | connect-style variants |

Detectors reuse what exists: the per-piece `Board` behind `forbidden_zones`
(goal zones), `piece_count` (extinct / counter start), the repetition map,
`is_in_check!` / `see!`. No new positional engine — wiring existing detectors to
named outcomes.

## Model: flat, directly-named table

`StaticState.end_conditions: EndConditions` — a plain struct, one directly-named
field per rule (no trigger enum; `checkmate`/`stalemate` distinct; `repetition`
and `perpetual` distinct). Config is one flat `name: args` line per rule, parsed
by a single flat `match` on the name — no indented blocks, no accumulators.
Tabular parameters (a counter's material table, adjudicate's point weights) and
goal zones use a named sub-section like `= castling =` / `= forbidden zones =`
already do — the label is config-local, never a variant name the engine
special-cases on.

```
= end conditions =
stalemate: win                 # default draw; list only overrides
repetition: 3 draw
counter: 100 Pp                # simple n-move progress draw
counter: endgame               # material-conditioned -> = counter endgame = table
checks: 3 win
perpetual: check
perpetual: chase RrCcNnEe
extinct: any Pp loss           # kinglet/horde: a side's pawns gone -> it loses
extinct: own * win             # antichess/losers: lose all your pieces -> you win
goal: Kk center win            # king reaches zone 'center' (-> = zone center =)
adjudicate: 200 janggipts      # at ply 200, decide by weighted material
```

`special_rules` sheds every terminal bit (repacked); keeps only move-gen
modifiers (castling, en passant, promotions, drops, forbidden zones, setup
phase).

## Decisive-result foundation (prerequisite)

Replace `State.game_over: bool` with `game_result: u8`
(`ONGOING/DRAW/BLACK_WIN/WHITE_WIN`); add `is_terminal!` + `terminal_score!` (Draw →
`draw_score!`, Loss → `-INF+ply`, Win → `+INF-ply`, matching checkmate scoring);
snapshot save/restore parity; update the ~11 `game_over` read sites; datagen/sprt
map by result. Make `d` / `format_game_state` print the terminal result (needed
to verify decisive outcomes, which the protocol can't otherwise show at a reached
terminal). Behaviour-neutral.

## Hot-path discipline
Per-rule "present" flags set at parse so make_move!/search evaluate only declared
rules. `goal` fires only when a masked piece moves onto its zone; `extinct` only
on captures; perpetual/counter/adjudicate only at their (rare) trigger points.
Variants that declare nothing new pay nothing.

## Critical files
- `src/game/representations/state.rs` — result consts/field, `is_terminal!`,
  Snapshot `gave_check`, remove terminal bits, repack setup-phase bit.
- `src/game/representations/end_condition.rs` (new) — `EndConditions`, `Outcome`,
  counter/adjudicate params.
- `src/game/moves/move_list.rs` — decisive terminal block; `gave_check`;
  perpetual resolver + chase (`see!`); extinction on capture; goal on move.
- `src/game/position/{search.rs,evaluation.rs}` — `terminal_score!`, read sites,
  check-counting, no-moves outcomes.
- `src/io/game_io.rs` — flat `= end conditions =` parser; zone/counter/points
  sub-sections; drop old terminal parsing; `d` result display.
- `src/game/search/move_ordering.rs` — reuse `see!` for chase.
- `configs/*.conf` — migrate; add demonstrators (three-check, KotH, racing,
  kinglet/horde, extinction, antichess). `example.conf` documents the grammar.

## Stages (one commit each, shippable)
1. **Decisive result** + `d` result line. Behaviour-neutral.
2. **End-condition table + migrate** — flat struct + flat parser; reproduce
   today's behaviour (checkmate/stalemate/repetition/counter); migrate in-tree
   configs; delete terminal bits; drop stand-off terminal. Gate: perft +
   self-play unchanged.
3. **Check family** — `gave_check`, perpetual-check cycle resolver, `checks` N;
   wire shogi/xiangqi + three-check demo.
4. **Perpetual chase** — `see!` favorable-threat + cycle target-identity +
   check>chase; wire xiangqi.
5. **Extinct + goal** — extinction (side/set/count) + zone goals; ship kinglet,
   horde, extinction, antichess, king-of-the-hill, racing kings.
6. **Generic material counter + adjudicate** — material-start + material-table
   limit → draw; weighted-material adjudication → decisive; wire makruk + janggi
   (+ cambodian/asean via config-only tables).
7. **example.conf schema doc.**
8. **(optional) connect-N + capture-the-flag flag-passing.**

## Progress (2026-07-26)

Done and committed:
- **Stage 1** (`game_result: u8`, `is_terminal!`/`terminal_score!`, `d`/TUI
  result line; datagen/sprt map by result). Behaviour-neutral.
- **Stage 2** (`EndConditions` in `src/game/representations/end_condition.rs`;
  flat `= end conditions =` parser; checkmate/stalemate/repetition/counter;
  all 31 configs migrated; terminal special-rules bits + stand-off machinery
  removed; setup-phase repacked to bit 6). All 36 configs parse (verified via
  headless `derive`, no panics).
- **Stage 3, `checks`** only — `gave_check` + per-colour `check_count` on
  State/Snapshot, `checks: N <outcome>`, `resolve_outcome_for!`. Ship
  three-check + five-check.
- **Stage 5** — `extinct: <side> <set> [count] <outcome>` (a `Vec`, one per
  type) + `goal: <set> <zone> <outcome>` with `= zone <name> =`. Ship
  king-of-the-hill, kinglet, extinction, horde.
- **Stage 7** — example.conf + README document the new grammar.

Remaining (deferred — complex, and mostly refine UCI-untestable regional
variants):
- **Stage 3 perpetual-check** + **Stage 4 perpetual chase**: repetition-cycle
  resolver reading `gave_check` (already stored) / `see!`; wire xiangqi/shogi.
- **Stage 6** material counter (makruk counting: material-start predicate +
  material-limit table) + adjudicate (janggi points, weighted material at a
  ply limit).
- Plan variants still needing new move-gen, not just end conditions: racing
  kings (no-check legality), antichess/losers (forced capture).

New-variant mechanics: a variant needs a `res/dicts/<name>.dict` listing the
protocol and a `touch src/prelude.rs` re-embed to be selectable — see
memory `adding-a-new-variant`.

## Verification (no `#[test]`)
Build clean; `standard.perft` unchanged; Stage 2 behaviour-neutral via self-play +
protocol spot-checks (threefold → draw, fool's-mate → mate). Decisive outcomes
read via the `d` result line on fixed scripted sequences (search
nondeterministic): perpetual check → checker loses; chase → chaser loses /
protected → draw; three-check → win; kinglet last pawn → loss; KotH king→center →
win; racing → far-rank win; makruk bare-king can't mate in limit → draw; janggi
limit → points decide. Every rule behind a present-flag; benchmark make_move! on a
no-new-rules variant.

## Risks
Broad scope — staged, 1–2 behaviour-neutral, each later stage one family with
demonstrator configs. Perpetual cycle parity + chase identity invert-prone
(verify via `d`). Mid-search repetition stays a draw. Keep checkmate as the
no-moves detector, not folded into extinction. Perf via present-flags.

## Progress (2026-07-26, cont.) — Stage 4 DONE
Perpetual check + chase shipped for xiangqi (commits 3093f64 check, c182e94
chase, 5d6e678 docs). `perpetual: [check <o>] [chase <o>] [exempt <p>]` refines
the repetition arm via `perpetual_fired` in make_move!.
- Check: per-colour `check_count` delta over the cycle == that colour's cycle
  move count. gave_check/check_count now maintained when perpetual-check is
  declared too (a 2nd is_in_check per node, gated to those variants).
- Chase: `favorably_chased` (pure attack-table scan, no make/undo → no
  reentrancy) records per ply the enemy non-royal squares the mover threatens
  undefended with a non-exempt piece, stored in `Snapshot.chase: Vec<Square>`
  (empty/free for other variants); intersect a colour's cycle plies → constant
  square held throughout = perpetual chaser.
- Priority: check outranks chase; both-offend → repetition outcome stands.
- LIMITATION: constant-square only. A target that dodges square-to-square each
  ply is not caught and draws (safe, non-wrong direction). Full WXF rooted/
  rootless + dodging-target chase left as future refinement.
Verified with constructed forced cycles (sole checker/chaser → loss; unsustained
threat + non-offence + standard 3-fold → draw; full-board xiangqi search clean).

Remaining deferred: Stage 6 (makruk material-count + janggi points). Racing
kings / antichess still need move-gen work (no-check legality / forced capture).

## Progress (2026-07-26, cont.) — Stage 6 DONE
Generic material counter + adjudication shipped (commits 6dfcbad counter,
79199e9 adjudicate, b09cbbc fixtures, b8ff070 docs).
- Material counter: `Counter.material: Option<u32>`; parser accepts
  `counter: <limit> <resets|-> [outcome] [material <m>]`; make_move gates the
  limit on `piece_count.sum() <= m` (summed only once the limit is reached, so
  the plain 50-move counter and other variants pay nothing). makruk selectable
  (new dict), `counter: 64 - draw material 3` = board's-honour bare-king count.
- Adjudicate: `Adjudicate{weights, handicap[2]}`; `adjudicate_fired` runs in the
  double-pass arm, sums weighted material + per-colour handicap, greater wins
  (tie draws). `adjudicate: <name>` + `= adjudicate <name> =` section of
  `<piece>: <weight>` (both colours) and `handicap: <w|b> <n>`. janggi
  selectable (new dict); points doubled so the 1.5 second-player handicap is
  integral.
- janggi FEN gotcha: setup_phase variants require a hand field; load post-setup
  positions with empty hands `-/-`, general auto-promotes K->Q in the palace.
Fixtures extended to 18 (makruk count draw + material-gate ongoing; janggi
adjudication by material, by handicap on equal material, lone pass ongoing).

All plan-08 stages 1-7 now DONE. Remaining out-of-plan: racing kings / antichess
need move-gen work (no-check legality / forced capture); perpetual chase is
constant-square only (dodging-target chase draws). Stage 8 (connect-N /
capture-the-flag) optional, not started.
