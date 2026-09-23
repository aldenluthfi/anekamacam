# CPMN move conditions

## Status

Opened 2026-09-23. Plan approved. Stages 1 to 7 done.

Baseline binary: `bin/base-cpmn`, built from `22d4aa6`. Gate signature:
`debug-headless bench <v> 8 --limit 16` total nodes and
`debug-headless perft <v> 3` at seed 42, for standard, shogi, xiangqi,
janggi, chushogi, crazyhouse, grand and makruk.

## Context

Move vectors today are unconditional: a compiled `MoveVector` (`Arc<[Leg]>`)
is legal whenever its legs pass occupancy and modifier tests. Rules that
depend on the board around the mover cannot be written. CPMN already exists
(drop rules, stand-offs: `src/game/moves/pattern_parse.rs`,
`pattern_match.rs`, `representations/pattern.rs`). The goal is to let a
move branch carry a CPMN pattern. The branch is playable only while the
pattern matches, anchored at the mover's origin square.

Targets:

- Annan shogi: a piece moves as the friendly piece directly behind it.
- Hiashatar: a piece moving onto a square adjacent to an enemy guard stops
  there. Expressed per ray length: branch `nW{k}` carries stoppers "enemy
  guard adjacent to path squares 1..k-1".
- Chu lion exchange: only partly. "Protected lion" is an attack test, and
  the counter-strike rule needs the previous move. Pure board patterns
  cannot express either. Follow-up, not in scope.

Decisions: origin anchor only. Colour via split `P:`/`p:` lines, no
own/enemy tokens. Scope is engine plus Hiashatar and Annan configs.

## Grammar

The exclusion slot is positional. The `@` after the exclusion slot opens
the CPMN:

```text
nR          no exclusion, no CPMN          (unchanged)
nR@nW       exclusion nW, no CPMN          (unchanged)
nR@@P       empty exclusion, CPMN P
nR@nW@P     exclusion nW, CPMN P
P      :=   [offsets '~' pieces] '@' [offsets '~' pieces]
```

- Split the raw expression at top-level `|` (paren depth 0) first. Each
  branch owns at most one CPMN. OR is more branches. AND is more offsets.
- Split rule: the CPMN starts after the first `@` that follows another `@`
  with no `-` leg boundary between them. An exclusion is a compound atomic
  and has no `-`. Thus earlier legs with their own `a@x-` exclusions stay
  CMN, and CPMN offsets can use `@` exclusions of their own.
- Stopper-only condition: the allower half becomes optional in
  `PATTERN_PATTERN`. `nR@@@sW~G` is empty exclusion, no allowers, stopper
  `sW~G`.
- Generation tests the pattern on the board before the move. Attack tests
  use the current board.

## Blast radius

Tier 1: must change

| File | Change |
|---|---|
| `representations/vector.rs` | `MoveVector` becomes a struct: `legs: Arc<[Leg]>`, `pattern: PatternSet` (empty is unconditional). The leg word has no free bit (all 32 used). Update `vector_offset!`, `vector_moves_quietly!`, `vector_is_initial!` to read `.legs`. |
| `representations/pattern.rs` | Doc update only. Reuse `Pattern` and `PatternSet`. |
| `moves/move_parse.rs` | `generate_move_vectors` stays CMN only. Add a top-level branch splitter and the CMN/CPMN split. Dedupe: equal legs merge their patterns into one `PatternSet` (OR). An unconditional duplicate clears the set, so no move is emitted twice. Update the CMN doc. |
| `moves/pattern_parse.rs` | `PATTERN_PATTERN`: allower half optional, anchored. Drop, setup and stand-off grammars stay valid. Add a clip for moves: allower off board drops the vector on that square, stopper off board drops the stopper (the drop semantics of `generate_relevant_drops`). One shared clip function for drops and moves. |
| `moves/pattern_match.rs` | `match_pattern!` is the one matcher. Add `match_pattern_set!` (empty or any match). `generate_drop_list!` uses `match_pattern!` in place of its inline copy. |
| `representations/state.rs` | `generate_piece_moves` compiles each branch and attaches its pattern. |
| `moves/move_list.rs` | `generate_relevant_moves` and `_captures` clip the pattern per square. `generate_attack_masks` carries the struct. `process_multi_leg_vector!` and `validate_attack_vector!` test `match_pattern_set!` first. Movegen, check, castling attacks, LVA ordering and perpetual chase inherit it. |
| `representations/moves.rs` | `AttackMask` holds the new `MoveVector`. |

Tier 2: callers that read `.iter()` or `.len()` on vectors (mechanical
`.legs`)

- `search/move_ordering.rs:123` (LVA via `process_multi_leg_vector!`)
- `representations/termination.rs:709` (perpetual chase)
- `search/parameters.rs`, about 12 sites: 567, 651, 755, 1532, 2212, 2342,
  2419, 2477, 2542, 2639, 2650

Tier 3: derive-time reasoning (rules only, no tuning)

- `derive_piece_reach`, `derive_piece_mobility`, `derive_vector_chance`:
  weight a conditioned vector by its chance of a match. Use the product
  over offsets of the occupancy chance of each piece set, from the model
  in `derive_vector_chance`. Stoppers use 1 minus that chance.
- Structural derivations read unconditioned vectors only:
  `derive_piece_roles`, `derive_pawn_*`, `derive_forward_directions`,
  `derive_shield_pieces`, `derive_royal_confinement`. Otherwise an Annan
  pawn with a copied rook branch is classed as a slider.
- `derive_search_capabilities`: audit `see_valid` and `recapture_order`. A
  removed piece can now change the move set of another piece, not only
  through line blockers. Clear the SEE shortcut flags when a vector has a
  pattern and SEE relies on static attacker sets.

Unaffected (checked): Zobrist and TT (patterns read only the board), `Move`
encoding and make/undo (the move does not store the vector), move I/O and
dicts, stand-off detection, drop generation semantics.

Cost: unconditioned vectors pay one `is_empty()` branch per vector in
movegen and attack tests. Only conditioned vectors hold a per-square
clipped `PatternSet`. Share it with `Arc` when the clip keeps everything.

## Stages

One commit each. Record outcomes here as they land.

1. Plan doc. Done.
2. Representation: the `MoveVector` struct, `.legs` migration of all Tier 2
   sites, empty patterns everywhere. Gate: fixed-depth node counts and
   bench signature identical to the pre-stage binary on standard, shogi,
   xiangqi, janggi and chushogi. `standard.perft` suite unchanged.
   Done. The condition is `Option<Arc<PatternSet>>`, not a bare
   `PatternSet`: the struct stays 24 bytes and the legs stay one pointer
   away. Gate identical on all 8 variants; perft suite 20256/20256.
3. Parse and compile: branch splitter, positional `@`/`@@` CPMN split,
   optional allower half, per-branch pattern, OR-merge dedupe, shared clip,
   `example.conf` `= piece moves =` doc with an example. Gate: existing
   variants give identical vectors (same perft).
   Done. `generate_move_set` (`move_parse.rs`) owns the split and the
   merge. `clip_pattern` and `clip_move_vector` (`pattern_parse.rs`) serve
   drops and moves. Gate identical on all 8 variants, and perft 2 equal to
   the baseline on every variant.
4. Match: `match_pattern_set!` in `process_multi_leg_vector!` and
   `validate_attack_vector!`. Drops use `match_pattern!`. Gate: node counts
   identical on existing variants. Hand-checked
   `debug-headless perft <variant> 1..3` divide on a crafted example-conf
   position with a conditioned branch.
   Done. Gate identical on all 8 variants. Speed suite (3 passes) against
   `bin/base-cpmn`: standard +9%, shogi +4.5%, xiangqi +3% nps, all noise,
   no loss. A temporary standard copy (not committed) with
   `P:...|R@@sW~R@`, `p:...|R@@sW~r@` and `N:N@@@nW~Q` showed: a pawn with a
   rook behind gets rook moves, a pawn without one does not, that pawn gives
   check along the file (the king cannot step onto it), Black mirrors, and
   a queen in front stops the knight. Two branches with different
   modifiers to one square (`mnW` and the rook `nW`) stay two moves. An
   author avoids this with a stopper on the own branch, as Annan needs.
5. Derive-time: chance weighting, structural filter, SEE capability audit.
   Done. `derive_condition_chance` gives a set member the chance
   `occupancy` times its share of the start census, and `?` the chance
   `1 - occupancy`. `derive_vector_chance` multiplies by it, so mobility,
   value and zone attacks weight a conditioned vector. `usual_vectors`
   keeps vectors with a chance of at least `USUAL_CONDITION_CHANCE` (50%)
   at opening occupancy, for reach, offsets and all pawn terms. SEE audit:
   `see!` walks live moves, so it obeys conditions. All SEE, recapture and
   static-exchange uses are also gated on `static_movement`, and a
   conditioned vector now clears that bit, as a screen does. Gate
   identical; startpos `evaluate` identical on six variants.
6. Annan shogi: `annan.conf` and `annan.dict` from shogi, split colour
   lines. For each piece X: its own moves with stopper `sW~<friendly set>`,
   plus for each friendly type Y: `<Y moves>@@sW~Y@`. Confirm the rules
   from a primary source first (king copying, promoted-piece copying,
   drops). `touch src/prelude.rs`. Verify with perft divide on hand-built
   positions (FSF has no annan) and a selfplay smoke run.
   Done. Rules from lishogi's shogiops (`src/position/rules/annanshogi.ts`):
   every piece, the king too, moves as the friendly piece directly behind
   it and loses its own moves. Promotion is never forced, there are no
   forbidden squares, and doubled pawns can stand but nifu and uchifuzume
   stay for drops. Start `lnsgkgsnl/1r5b1/p1ppppp1p/1p5p1/9/1P5P1/
   P1PPPPP1P/1B5R1/LNSGKGSNL`. Move keys are one or two letters, so the
   gold-like pieces have one line each.
   New blast radius item: `game_io.rs` scans the raw move text for the `p`
   and `t` modifiers, and a black pawn `p` in a condition tripped it.
   `strip_move_conditions` removes conditions before that test.
   Oracle: npm `shogiops` as a perft reference, with a strict legality
   filter added. Its pin shortcut accepts a move that uncovers an attack by
   changing the piece behind an enemy (after `4b3d` the 3c bishop loses its
   silver and checks). Startpos perft 1..4 = 28, 784, 22726, 658600, all
   equal. 694 random positions at depth 2: equal, except 4 where the engine
   counts a pawn-drop mate. That is the engine convention:
   `illegal_mating_drop!` adjudicates it, perft does not prune it. A
   200-ply self-play game at depth 6 ran without error.
7. Hiashatar: `hiashatar.conf` and `.dict` (10x10). Confirm the rules first
   (guard movement, whether a piece that starts adjacent can leave, king
   exemption). Split colour lines for each slider: one branch per direction
   and length, stopper half over the 3-wide band next to path squares
   1..k-1 against the enemy guard letter. Verify as in stage 6.
   Done. Rules from Wikipedia and Mats Winther's page (chessvariants.com
   refused the fetch): 10x10, `rnbgkqgbnr` on both back ranks. The guard
   `G` moves as a queen for one or two squares, and cannot capture or check
   a king (`!k`). An enemy piece next to a guard moves one square only. An
   enemy slider stops on the first square next to a guard. Knights leap
   past it. Pawns step one to three squares on the first move, with en
   passant, and promote to queen only. No castling.
   Encoding: an exact slide of k squares is `nW-{k}` (probed: `-{k}` is a
   blocked slide, `{k}` a leap). Its stopper band is `K` (next to the
   origin) plus `nW{1..k-1}nK` (next to each passed square). For diagonals
   it is `K` plus `neF{1..k-1}K`. Lines come from a throwaway generator:
   queen 65 branches, rook and bishop 33, guard 9.
   Assumption: the engine keeps one en passant square, so the triple step
   marks the square behind its landing square and the double step marks
   the passed square. The dict maps both.
   Checks: startpos perft 1..4 = 34, 1156, 42150, 1535305 (1 and 2 are
   34 and 34^2 by hand). By hand: a rook stops on the first zone square, a
   rook in the zone moves one square, a queen diagonal stops at the zone,
   a king can step next to a guard (no check), a pawn's two and three
   steps are cut, and a knight in the zone keeps 8 moves. A 200-ply
   self-play ran without error. There is no oracle (FSF has no hiashatar).
   Derived guard value 618/689 against rook 604/820: the value reads only
   the guard's own moves, not its zone.

## Follow-ups (not in scope)

- Direction-span rotation of an attached CPMN (shorter Hiashatar lines).
- Attack and last-move predicates, for the Chu lion exchange rules.

## Verification

- Regression: pre-stage and post-stage binaries, same `ANEKAMACAM_SEED`,
  same Hash. Fixed-depth `go depth N` node counts on existing variants must
  match exactly. They have no conditioned vectors, so any drift is a bug.
- New semantics: `debug-headless perft <variant> <depth>` divide on crafted
  FENs, compared by hand. An Annan pawn with a rook behind has rook moves
  and no pawn step. A Hiashatar rook stops at the first square next to an
  enemy guard.
