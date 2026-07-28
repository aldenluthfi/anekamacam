# 09 — End-condition rectification: correct rules, no deferrals

> Saved copy of record: `plans/09-end-condition-rectification.md` (repo). This
> plan-mode file is kept in sync with it.

> **Revision (post-review).** Stage 1 was first built SEE/lva-based; on review
> that was over-engineered. Two user decisions reshape it:
> (1) detection is a plain **undefended** predicate that **unifies check and
> chase** — no SEE/lva; (2) repetition + perpetual move **out of `make_move!`**
> into an on-demand `repetition_outcome` surfaced through a `game_outcome`
> helper (**search-driven**, scope: repetition/perpetual only — position-local
> terminals stay eager). Design in D0/D1. Research: four code-exploration passes
> (search flow, uchifuzume deferral, attack primitives, `game_result` consumers)
> plus AXF rules from H.G. Muller / Fairy-Stockfish / Pikafish. The SEE-based
> Stage-1 code sits uncommitted in the working tree and is reworked in place.

## Context

Plan 08 shipped the parametric end-condition system, but while building it the
assistant repeatedly made **scope decisions that were the user's to make** —
shipping simplified/partial rules and labelling the rest "deferred", against an
explicit instruction not to defer. This plan removes that judgement call: it
implements the **full, correct** rules for the terminal families the engine
already claims to support, and wires the variants those mechanisms unlock.

Research against primary rules turned up not just omissions but **incorrect
implementations that ship wrong results today**. Those are the priority.

Only sittuyin's isolated **promotion** repair (its bogus `S` piece, per-leg
promotion gates, and full promotion-move constraints) is split into
**plan 11** — its counting and dict are NOT isolated (they come with the
makruk/Cambodian counting and the dict batch) and stay here.

## What is wrong today (verified against primary rules)

### 1. Perpetual check — only xiangqi wired; shogi missing
- Xiangqi: perpetual check → checker **loses**, at the **3rd** occurrence.
- Shogi *sennichite*: same position (board + **pieces in hand** + side to move)
  **4×** → draw, EXCEPT if one side's moves in the repeated sequence were **all
  checks**, then that side **loses**. Threshold is 4-fold.
- The code already couples the perpetual trigger to each variant's `repetition:`
  count and analyses the last cycle correctly for any fold — but shogi/minishogi
  were never wired (they carry `repetition: 4 draw` and no `perpetual`). So the
  "xiangqi 3 vs shogi 4" distinction is data, already handled; xiangqi is simply
  the only one switched on. `position_hash` folds in-hand pieces
  (hash.rs:235-237, verified), so sennichite identity is correct.
- Sources: [Sennichite](https://en.wikipedia.org/wiki/Sennichite),
  [Perpetual check](https://en.wikipedia.org/wiki/Perpetual_check).

### 2. Perpetual chase — SEE/lva over-engineered; plain "undefended" is the rule
- Xiangqi chase (Asian/AXF rules — H.G. Muller / Fairy-Stockfish / Pikafish):
  a side that on **every** move of the repeat cycle makes a **new direct attack
  on the same enemy piece that is unprotected** **loses** (both chase → draw),
  unless a perpetual **check** occurred (check outranks). Exempt: attacks **by**
  a Pawn or King never chase; a Pawn not across the river is never chased.
- **Detection is plain "undefended", not SEE.** The first Stage-1 attempt scored
  each threat with `see!`/`lva!` (a make/undo per attacker) — needless cost and
  complexity. "Attacked and undefended" is one `is_square_attacked!` pair. Strict
  AXF adds exactly two nuances plain-undefended drops — Horse/Cannon-on-Rook
  counts even if protected, and equal-piece exchange-offers don't count — which
  is the *only* thing SEE bought. Per the user's directive we use plain
  undefended; those two AXF nuances are a documented, accepted simplification.
- **Check and chase are one mechanism.** A check is just "the enemy **royal** is
  attacked" (a royal is never protectable). So a single per-ply predicate — *does
  the mover leave an undefended enemy piece attacked, royal or not* — yields
  both: royal ⇒ check, non-royal ⇒ chase. This drops the separate per-move
  check-count coupling for perpetual.
- The chased piece **dodges square to square**, so detection still tracks the
  target by **identity** across the cycle (a square remap through each quiet
  cycle move), not a constant square — the one thing the first attempt got right.
- Sources: [TalkChess / H.G. Muller](https://talkchess.com/viewtopic.php?t=35403),
  [Fairy-Stockfish #55](https://github.com/fairy-stockfish/Fairy-Stockfish/issues/55),
  [Pikafish](https://github.com/official-pikafish/Pikafish).

### 3. Makruk counting — flat "≤3 → 64" is badly wrong
- Two systems: **board's honour** (neither king bare) = 64 moves from count 1;
  **pieces' honour** (one bare king) = limit from a table by the winner's
  strongest piece (2R→8, 1R→16, 2 khon→22, 2 knights→32, 1 khon/knight→44,
  else→64), count starting at **pieces + 1**. Moves-to-mate = limit − pieces.
- Current `counter: 64 - draw material 3` gives 64 for every ≤3-piece endgame,
  where the rule gives 8-44. Wrong.
- Makruk officially has **no 3-fold and no 50-move** (counting replaces both) —
  makruk.conf still has `repetition: 3`. **User chose strict**: drop it.
- Sources: [Makruk (chessvariants)](https://www.chessvariants.org/play/makruk-thai-chess),
  [Makruk (gambiter)](https://gambiter.com/chess/variants/Makruk.html).

### 4. `perpetual` silently dead without `repetition`
- `perpetual_fired` is only ever called inside the `repetition` arm
  (move_list.rs:2670-2678). A variant declaring `perpetual` but no `repetition`
  gets a dead rule that never adjudicates, yet still pays per-move cost. The
  parser imposes no check. Fix: reject that combination.

### 5. Configs not selectable (no dict)
- capablanca, los-alamos, ouk-chaktrang, shatranj, sittuyin, tjatoer have no
  dict, so are not selectable at all. Cambodian (ouk-chaktrang) and Burmese
  (sittuyin) also need their counting tables.

## Scope of THIS plan (user-confirmed)

In (each with regression fixtures; the 18 existing fixtures stay green except
the 3 makruk lines that are replaced):
1. **shogi/minishogi perpetual check** — add `perpetual: check loss`.
2. **makruk two-system counting** — board's + pieces' honour; drop makruk
   `repetition: 3` (strict).
3. **xiangqi perpetual chase** — piece identity + plain undefended (no SEE),
   unified with perpetual check.
4. **reject `perpetual` without `repetition`** in the parser.
5. **Cambodian (ouk-chaktrang) + Burmese (sittuyin) counting** — same mechanism,
   their own tables; plus sittuyin's `stalemate:` direction.
6. **dicts for all 6 non-selectable configs** — capablanca, los-alamos,
   ouk-chaktrang, shatranj, sittuyin, tjatoer.

Out (separate plans, NOT dropped):
- **plan 11**: full sittuyin **promotion** repair — promote soldier to General
  on the sit-ke-myin diagonal; drop the bogus `S` piece; add generic per-leg
  `r`/`!r` promotion gates; reuse `promote to captured` for one-General
  availability; express in-place/diagonal promotion and the rule that the
  resulting General attacks no enemy piece through existing CKN primitives.
- **later plan**: racing kings (no-check legality move-gen bit) + antichess/
  losers (forced-capture move-gen bit); these touch legality for all variants.

## Design

### D0 — Repetition & perpetual become on-demand terminals (out of make_move!)
User-chosen architecture (scope: repetition/perpetual only). Principle from the
consumer map: a **no-successor** terminal (goal, extinct, N-check, counter,
double-pass) truly ends the position, so make_move! keeps setting these eagerly
and they correctly gate move generation (`is_terminal!` empties move-gen at
move_list.rs:3925/3972). A **repetition / perpetual** claim leaves the position
with legal successors and is history-cyclic; search already half-computes it
(search.rs:900). So repetition + perpetual move **out of make_move!** into one
on-demand adjudicator, joining checkmate/stalemate/uchifuzume as *computed* (not
stored) terminals.

- **New `repetition_outcome(&mut State, min_count) -> Option<Outcome>`**: `None`
  if no `repetition` rule or `position_hash_map[hash] < min_count`; else adjudicate
  the cycle (D1 walk) — `Some(Draw)` when no sole offender, else `Some(Win/Loss)`
  from the **side-to-move** perspective. Non-perpetual variants short-circuit to
  `Some(Draw)` (no walk). `&mut` because the walk uses `undo_move!`/`make_move!`
  and restores state exactly (incl. `game_result`, saved/restored around it).
- **New `game_outcome(&mut State) -> u8`**: `if game_result != ONGOING
  { game_result } else { repetition_outcome(state, repetition.count).map(|o|
  resolve_outcome!(state,o)).unwrap_or(ONGOING) }`; plus `game_over!(state)` =
  `game_outcome(state) != ONGOING`. This is the single game-truth oracle.
- **make_move! (simplify)**: delete the repetition arm (move_list.rs:2670-2678),
  the deferred `perpetual_fired` call (2713-2719), `State.chase_scanning` + its
  guard, and the whole SEE walk. Keep the checks/goal/extinct/adjudicate/counter
  arms, the `position_hash_map` increment, and counter maintenance; gate the
  `gave_check`/`check_count` block on `checks.is_some()` **only**. No reentrancy
  guard needed — make_move! no longer triggers adjudication, so the walk's redo
  can't recurse.
- **Search**: keep the plain 2-fold draw cut at search.rs:900 (`repetition
  present && ply > 0 && count >= 2 → draw_score!`). Search does **not** run the
  perpetual walk: the search stack carries **null moves** (null-move pruning), and
  the walk's `undo_move!` would decrement a `position_hash_map` entry that
  `make_null_move!` never added — a panic (observed at depth 20). Game-truth via
  `game_outcome` walks only real game history (no null moves), so perpetual
  verdicts stay correct there. Search-valuing-perpetuals is deferred (would need a
  null-move-aware walk). **Hot paths keep raw `is_terminal!`** (move-gen
  early-return 3925/3972, node-entry 222/589/889, PV-walk) — position-local
  terminals only; repetition intentionally no longer stops move-gen.
- **Consumers → route through `game_outcome`/`game_over!`** (the exact read sites
  from the map): `format_game_result` (game_io.rs:2786 → `d` Result line + console
  Result row), `format_game_phase` (game_io.rs:2795), `print_bestmove`
  (protocol.rs:179), datagen (datagen.rs:69-70), sprt (sprt.rs:650,653), console
  (console.rs:515,2906,2933,2934). These already hold `&mut`/owned state.
- **Perft/move-gen risk (verify)**: since repetition no longer sets `game_result`,
  move-gen stops treating a 3-fold as a leaf. Confirm `standard.perft` is
  unchanged (standard perft does not treat repetition as terminal, so a matching
  suite should be unaffected — but verify; if it changes, the old
  repetition-stops-movegen behaviour was baked into the reference and we decide
  then).

### D1 — The unified undefended walk (inside `repetition_outcome`)
A repetition cycle is **capture-, drop-, promotion-, castle- and pawn-move-free**
(all irreversible, so the position could not return), so every cycle move is a
pure quiet relocation `start→end` — identity tracking is a simple square remap.
This is where the SEE/lva mistake is undone: detection is **plain undefended**,
and **check and chase are the same predicate** (royal ⇒ check, non-royal ⇒ chase).

- `offence_set(&State, mover) -> (bool, Board)` — read-only, no make/undo:
  - **check** (bool): `is_in_check!(quarry, state)` (quarry = the side to move
    after the mover's ply) — full-board, so **discovered checks count**.
  - **chase** (`Board`): for each **non-royal** enemy piece square `s`, set it iff
    attacked by a **non-exempt** mover piece **and** undefended. The attack test
    reuses the old `favorably_chased` inner loop (iterate
    `relevant_attacks[quarry][s]`, keep `perpetual.chasers[idx]`, run
    `validate_attack_vector!`); the defence test is `is_square_attacked!(s, mover,
    …)` negated. **No make/undo, no SEE, no lva.**
- `repetition_outcome`'s walk: from the current (repeated) position walk backward
  with `undo_move!` to the previous same-hash occurrence; per colour AND the
  per-ply `check` bool (→ perpetual checker) and intersect (`and!`) the per-ply
  chase `Board`, remapping the chase board through each undone quiet move (clear
  `end`, set `start`); `make_move!`-redo to restore. **Rank: check outranks
  chase**; a sole offender ⇒ that colour loses (map to `Outcome` from the
  side-to-move perspective); both-same-offence or none ⇒ `Outcome::Draw`.
- **State hygiene**: save `game_result` before the walk and restore after — a
  redone quiet cycle move can legitimately trip an *eager* position-local terminal
  (e.g. a KOTH king stepping onto the zone) inside `make_move!`, which must not
  leak out of the read-only adjudication. No `chase_scanning` guard is needed
  (make_move! no longer adjudicates repetition, so no reentrancy).
- **Drop** the per-move perpetual check-count coupling entirely (check is now
  recomputed in the walk). `check_count` stays only for the three-check `checks`
  family. *Verify at impl:* whether `gave_check` has any other cross-variant
  consumer (e.g. a search check-extension); if so keep computing `gave_check` but
  still drop the perpetual coupling.
- **Verify at impl** (do not assume): `is_square_attacked!` / `is_in_check!`
  argument order and the "defended-by" convention (mirror the pre-rewrite
  `favorably_chased`); that the undo/redo walk leaves `verify_game_state` clean in
  a debug build; the side-to-move sign when mapping "mover loses" → `Outcome`.

### D2 — Makruk-family counting: board's + pieces' honour
- **Board's honour** (neither bare): existing `Counter`, no material gate —
  `counter: 64 - draw` on `halfmove_clock`.
- **Pieces' honour** (one bare king): a frozen clock that must NOT reset on
  capture, so `halfmove_clock` cannot serve.
  - **Remove** `Counter.material` (the wrong Stage-6 field) + its parser handling.
  - **Add** `Counting { table: Vec<(Vec<(usize,u32)>, u16)>, default: u16,
    outcome }` + `EndConditions.counting`; `State.counting: Option<(u16,u16)>`
    (count, frozen limit) + a `Snapshot` field saved/restored like
    `halfmove_clock`.
  - **make_move!**: a guarded block updates `counting` — on the bare-king
    transition set `(pieces_on_board+1, limit)`, else `+1`/ply frozen; a
    `terminal_outcome` branch (before the board's-honour counter) fires when
    `count >= limit`. Helpers `side_is_bare` / `counting_limit` are O(N) on
    `piece_count` + `p_is_royal!`.
- **Config**: `counting: <name> [<outcome>]` + a `= counting <name> =` ordered
  first-match section. makruk: RR→8, R→16, HH→22, NN→32, H→44, N→44, default→64.
  Cambodian/Burmese: same mechanism, their own tables (research exact numbers;
  sittuyin appears to match makruk: KRk=16, 2R=8).

## Stages (one commit each, shippable)

1. **Repetition/perpetual → search-driven + undefended detection (D0+D1)** — one
   cohesive rework of the working-tree SEE attempt:
   - **Architecture (D0)**: add `repetition_outcome` + `game_outcome`/`game_over!`;
     delete the make_move! repetition arm, the deferred perpetual call,
     `chase_scanning`, and the SEE walk; wire search.rs:900 to `repetition_outcome`;
     route the `d`/datagen/sprt/`print_bestmove`/console/`format_game_phase`
     consumers through `game_outcome`.
   - **Detection (D1)**: undefended `offence_set`, check+chase unified, identity
     remap; no SEE/lva.
   - Fixtures (xiangqi) — still asserted via `d` (now `game_outcome`-backed):
     - keep F17 perpetual check, F18 constant-square undefended chase → loses,
       F19 chase-not-sustained → draw, F20 no-offence → draw.
     - **dodging undefended chase → chaser loses** (identity remap) — keep.
     - **defended victim → NOT a chase → draw** — the working-tree fixture built as
       "defended chariot chased by a cheaper knight → loses" was SEE semantics;
       under plain undefended a **protected** piece is not chased, so **flip its
       expected result to Draw and relabel** (now tests the undefended cutoff).
     - **mutual equal attack → draw** — keep.
   - Confirm `standard.perft` unchanged (repetition no longer stops move-gen) and
     the standard 3-fold + all 18 fixtures stay green.
2. **Shogi/minishogi perpetual check**: add `perpetual: check loss` to both.
   Fixtures: shogi 4-fold perpetual check → checker loses; plain 4-fold → draw.
3. **Parser guard**: reject `perpetual` without `repetition`.
4. **Makruk two-system counting (D2)**: remove `Counter.material`; add
   `Counting`; drop makruk `repetition: 3`. Replace the 3 old makruk fixtures
   with KRRk→draw@8, KRk→draw@16, not-yet-expired→Ongoing.
5. **Dicts for all 6 non-selectable configs** (capablanca, los-alamos,
   ouk-chaktrang, shatranj, sittuyin, tjatoer) — right template per board size +
   protocol list + `touch src/prelude.rs` re-embed. Confirm each loads + plays.
6. **Cambodian + Burmese counting**: ouk-chaktrang + sittuyin counting tables
   (+ sittuyin `stalemate:` direction, verified) + per-variant fixtures (now
   selectable). Sittuyin's promotion stays broken until plan 11 — does not block
   counting/selectability.
7. **Docs**: example.conf grammar (`counting`; drop the stale `counter … material`
   note) + README end-conditions table.

## Verification
- `tools/run_endgame_fixtures.sh` green incl. new cases and the existing 18
  (makruk lines replaced, not added). Repetition/perpetual fixtures still pass
  via `d` — proving the `game_outcome` consumer routing works.
- **Perpetual is now SEE/lva-free**: `grep see!/lva!` in the perpetual code path
  returns nothing; detection is only `is_in_check!` + `is_square_attacked!`.
- **make_move! no longer sets a repetition/perpetual result**: grep confirms the
  repetition arm + perpetual call are gone; `game_result` for those flows only
  through `game_outcome`.
- **Consumer routing**: `d` Result, datagen game-end, sprt, and `print_bestmove
  (none)` all still see repetition/perpetual terminals (spot-check a xiangqi
  perpetual via `d` and a self-play/datagen run that hits a repetition).
- **`standard.perft` unchanged** — the decisive check that dropping
  repetition-stops-move-gen did not shift node counts.
- **Debug build (`verify_game_state` active)** drives the xiangqi fixtures and a
  xiangqi endgame `go depth N` with no hash/parity panic — the undo/redo walk
  restores state exactly (this caught real bugs in the first attempt).
- `./anekamacam derive` parses every config, incl. the 6 newly-dicted.
- a xiangqi + a makruk search run clean (no panic).
- Spot-check each newly-selectable variant loads via `position fen … / d`.
- md5 the release binary before benching (per prior habit).
