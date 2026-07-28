# 11 — Per-leg promotion gates and full Sittuyin promotion

> Repo record. Plan 10 is the janggi stand-off rectification; this plan owns
> Sittuyin promotion.

## Context

Sittuyin's current promotion model is wrong. It uses a synthetic `S/s`
"Promoting Feudal Lord" and a `P:S`, `S:P` cycle instead of promoting a soldier
to a General. Zone-only promotion is insufficient because Sittuyin promotion
is tied to specific moves:

- normal forward moves and captures must not promote;
- promotion happens on a later turn while the soldier is on the sit-ke-myin
  diagonal;
- the soldier may promote in place or move one diagonal square while promoting;
- promotion is available only after that side's General has been captured;
- the resulting General may not attack an enemy piece.

Add generic per-leg promotion gates to CKN, then express every Sittuyin-specific
restriction in `configs/sittuyin.conf`. No Sittuyin name or rule enters engine
code. The same gates support variants such as Gigachess, where a piece cannot
promote through one of its movement options; adding a Gigachess config is out
of scope.

## 1. Leg modifiers: `r` and `!r`

Add two movement-leg modifiers:

- `r`: this leg must be used to promote.
- `!r`: this leg must not be used to promote.

Keep positive and negated modifier groups contiguous and in matching order.
This fills the existing 16-bit modifier field:

| group | `LegVector` modifier bits | runtime `Leg` bits |
|-------|---------------------------|--------------------|
| positive | `m0 c1 d2 u3 k4 v5 g6 t7 i8 p9 r10` | 16–26 |
| negated | `!k11 !v12 !g13 !i14 !r15` | 27–31 |

Thus `r!` reads runtime bit 26; existing `not_k!`/`not_v!`/`not_g!`/`not_i!`
shift to bits 27–30; `not_r!` reads bit 31. The same modifier bits occupy
`LegVector` bits 32–47.

### Files

- `src/game/representations/vector.rs`
  - extend `LegVector::parse_modifiers`;
  - add `r!` and `not_r!`, shifting existing negated readers;
  - extend `get_modifiers_str`;
  - update `Leg`/`LegVector` layouts and modifier docs.
- `src/game/moves/move_parse.rs`
  - add lowercase `r` to `LEG`, `LEG_TOKENS`, and `MODIFIERS`;
  - add it to both `LEG`'s modifier group and body-negated class.

Lowercase `r` is free: Betza atoms are uppercase and cardinals use `nsew`.

## 2. Per-leg promotion semantics

Implement the gates inside `process_multi_leg_vector!` while each leg's exact
`start_square` and `end_square` are available. Do not inspect only the final
leg or only the vector's original/final squares.

For a promotable piece in a promotion-enabled variant:

1. Read mandatory and optional zone membership for this leg's start and end.
2. An `r` leg is invalid unless its own start or end lies in either promotion
   zone. A valid `r` leg forces the completed move to be a promotion; no plain
   counterpart is emitted.
3. A `!r` leg never generates a promotion. It is invalid when its own end lies
   in a mandatory promotion zone, because that destination requires promotion.
   Ending on an optional zone remains legal as a plain move.
4. Unflagged legs retain existing zone-promotion behaviour.
5. `r!r` has no defined meaning and must not be used in configs.

Promotion moves are emitted only after the whole multi-leg vector succeeds.
Encode quiet/single-capture/multi-capture type and capture payload before cloning
`encoded_move` into promotion alternatives; otherwise capture-promotions lose
their captured-piece record. Keep one outer `if !invalid` guard around final
move encoding and emission instead of repeating the check on every branch.

Reuse existing `promote_to_captured!` target availability. No new
special-rule bit is needed.

## 3. Full Sittuyin config

### Rules and pieces

In `configs/sittuyin.conf`:

- add `promote to captured` under `= rules =`;
- remove `Ss:Promoting Feudal Lord`;
- change piece order to `PGKCHEpgkche`;
- set promotions to `P:G` and `p:g`;
- remove `S/s` promotion-zone rows;
- use the sit-ke-myin diagonals as `= optional promotion zones =` so a `!r`
  forward move may land there without promoting.

`promote to captured` already provides the one-General rule. Capturing a
General places its colour-swapped counterpart in the opponent's hand;
promotion consumes it. Setup-phase pieces do not cause false availability
because normal moves are disabled during setup and both hands are empty when
setup completes.

### Soldier movement

Use the user-supplied movement definition:

```text
Pp:m!rnW|c!rnF|mrF-(mnW|dnW-u#)-sW-(mnW|deW-u#)-sW-(mnW|dwW-u#)-sW|mr#-(mseF|dseF-u#)-sW-(mnW|dnW-u#)-sW-(mnW|deW-u#)-sW-(mnW|dwW-u#)-sW
```

The alternatives mean:

- `m!rnW`: normal forward move, never promotion;
- `c!rnF`: normal forward-diagonal capture, never promotion;
- `mrF...`: diagonal move that must promote;
- `mr#...`: in-place (`#`) move that must promote.

The chained suffix checks every square the resulting Ferz-moving General would
attack. Each probe succeeds on an empty square through `m`, or temporarily
handles a friendly occupant through `d...-u#` and restores it before returning
to the promotion square. An enemy occupant satisfies neither branch, so that
promotion vector is not generated. This enforces "the resulting General must
not attack an enemy piece" entirely through existing CKN primitives; no attack
special-case belongs in engine code.

Keep `Gg:v!vF` and every other Sittuyin piece, forbidden zone, counting rule,
setup rule, and dict unchanged.

## 4. Parameters and embedding

Removing `S/s` changes the piece count and PST ordering. Delete the stale
`res/param/sittuyin/latest.param`, rebuild without it embedded, load/derive
Sittuyin so the default 6-piece parameter vector is exported, then touch
`src/prelude.rs` and rebuild to embed the new config and parameters.

## 5. Verification

1. `cargo build` and `cargo build --release`: zero warnings.
2. `./target/release/anekamacam derive`: every embedded config parses, including
   the long Sittuyin movement expression and `#` null leg.
3. Finish a Sittuyin setup, then test promotion positions in the debug console:
   - own General still alive: no promotion move;
   - own General captured, soldier on sit-ke-myin, no attacked enemy: diagonal
     and in-place promotion alternatives appear;
   - enemy on each possible resulting-General attack square: corresponding
     promotion is absent;
   - empty or friendly piece on each probed square: promotion remains legal,
     probe leaves board unchanged, and `verify_game_state` passes;
   - normal `!r` advance/capture never promotes, including entry onto the
     optional sit-ke-myin zone;
   - `r` movement off all promotion zones is absent;
   - captured General is consumed, so another soldier cannot promote without a
     later captured General.
4. Standard capture-promotion probe (for example `b7a8q`) still captures and
   promotes correctly; `d`/`verify_game_state` passes.
5. Shallow standard and makruk perft regression checks: unflagged promotion
   remains unchanged.
6. UCI move round-trip through `res/dicts/sittuyin.dict`, including in-place and
   diagonal promotion notation.

## Out of scope

- Gigachess config/dict/parameters.
- Variant-specific engine code for Sittuyin's attack restriction.
- New unit-test modules; use derive, debug-console move lists/perft, UCI probes,
  and `verify_game_state` per repository policy.
