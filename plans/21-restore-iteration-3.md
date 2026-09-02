# Restore iteration 3 onto the iteration 4 and 5 line

## Status

Drafted 2026-09-02. No stage has landed.

Two preparatory commits are already in: `316861a` ports the exchange
phase-pricing fix onto this line, and `224d167` is a whitespace fix. The
branch `strength-iteration-6` was renamed to `main`; the previous `main`
tip is preserved at tag `ablated-main-2026-09-02` and its only unique
content, `951eda0`, is the fix `316861a` carries.

The iteration 6 batch at `9d633d8` is deliberately abandoned. It was built
on the ablated `b971865`, not on this line, and it holds no ref.

One stage was attempted and reverted before the ladder below was fixed:
dropping the PST residual region from the param format. It is recorded as
a deferred open question, not as a stage.

## Goal

Restore the strength and speed the engine had at the end of iteration 3
(`e56980f`), on top of the iteration 4 and 5 line, keeping every iteration
4 and 5 mechanism that the user named as load-bearing.

## Why a revert cannot do this

Reverting the two ablation commits was probed on a scratch branch and
abandoned:

- `94b0759` (evaluation) conflicts in 38 `res/param/*/latest.param` files
  and 15 sources.
- `916cc16` (search) conflicts in 8 sources.
- `8586adf` (board width) conflicts in `src/prelude.rs`.

The param conflicts are not incidental. Iteration 3 stored absolute values
plus a 19-token scalar tail; this line stores material and PST residuals
against derived values. A revert would drag the old schema back and undo a
keeper. Every stage below is therefore written by hand against `e56980f`
as reference, not applied as a patch.

## Kept from iterations 4 and 5

These are preserved by every stage; a stage that would remove one is wrong.

- `capabilities: u16`, the rule-derived per-variant capability mask. Every
  search shortcut restored below is gated through it. Iteration 3 applied
  every shortcut to every variant unconditionally, which is the defect
  plan 18 deferred.
- `virgin_hash` and the separated canonical / search / qsearch hash
  identity. New hashes introduced below (pawn hash, correction hash) join
  that separation rather than reusing the canonical key.
- `reduction_surface`, the rule-derived LMR curve. Iteration 3's `LMR_*`
  constants are **not** restored.
- SETUP army resolution (`resolve_setup_army`, `walk_setup_endings`,
  `setup_census_key`) for phase thresholds, and drop moves carrying a real
  target square (`532ef34`).
- Variant-declared repetition and perpetual outcomes, deepest-worker SMP
  selection, and the wide-window bound fix.
Restored evaluation scalars are **derived**, via `derive_eval_scalars` and
`derive_pawn_parameters`, so no param file gains a scalar region and
iteration 3's 19-token tail does not come back.

## Open question, deferred: the PST residual region

The param schema is not on the keeper list. Its shape was measured, not
assumed: `res/param/standard/latest.param` is 780 tokens, 12 absolute
material values then 768 zeros, and every token past the material prefix
is zero in all 38 files. The region has one writer, `export_theta` in
`src/debug/tuning.rs`, which stores each final target minus its
rule-derived base. No tuning run has happened since the schema changed, so
every PST in use is the derived one.

Storing the derived numbers in place of the zeros does not fix the
redundancy. It is the same 768 numbers per file, and it costs the one
property the residual encoding buys: the derivation stays live. Change
`derive_base_pst` and all 38 variants pick the change up, keeping whatever
tuning added on top. Absolutes freeze it — a stale file silently overrides
an improved derivation with the old one, and nothing distinguishes a
square tuning touched from a square that is derivation output copied down.
Determinism is the argument for not storing the output at all, not for
storing it verbatim.

Removing the region was attempted and reverted, because it is not
independent of the tuner. `compute_gradient` optimises material and every
per-square PST value, and `export_theta`
(`src/debug/tuning.rs:663-678`) writes those residuals back through
`parse_tuned_parameters`, whose stricter token count then rejects its own
tuner's output: `tune` panics at export. A material-only format therefore
requires first deciding what `tune` optimises — material alone, or PSTs
into a region that has to keep existing. Deferred at the user's
instruction. No stage below reads a PST residual, so the ladder is
unaffected either way.

## Deliberately not restored

Both were measured negative in iteration 3 and re-measured in iteration 2's
ablations.

- Capture history: -11 Elo, and its iteration 2 removal cost nothing.
- Singular extension with multicut: -19 Elo, the worst stage of A-L.

## What is actually missing

Measured against the current tree, not against plan 20's narrative, which
describes the abandoned `9d633d8` lineage.

Evaluation. `src/game/position/evaluation.rs` is 142 lines against
iteration 3's 700. Present: `terminal_score!`, `royal_shelter!`,
`evaluate_position!`. Absent: `draw_score!`, `king_shelter!`,
`pawn_shield!`, `castling_bonus!`, `king_danger!`, `open_shield!`,
`pawn_structure!`. Draws score zero everywhere.

Search. `src/game/position/search.rs` is 1094 lines against iteration 3's
1779. Present: aspiration windows, futility, `improving`, delta pruning,
`reduction_surface` LMR, NMP, LMP, killers, butterfly history. Absent:
PVS, razoring, ProbCut, IIR, mate-distance pruning, check extensions,
continuation history, correction history, and the TT static-eval cache.

Structure. `BoardBits` is unconditionally `U4096`; the `wide-board` feature
is gone from `src/Cargo.toml`. Staged move generation
(`generate_all_quiets_and_drops`) is deleted. The soft and hard deadline
split is collapsed to a single `deadline`.

## Ordering

Iteration 3's own Elo record decides the order. Twelve search micro-stages
(A-L) netted about zero combined, on a round-robin cluster of [-19, +1].
Evaluation and time carried all of the roughly 100 Elo:

| iteration 3 stage | Elo | restored as |
| --- | --- | --- |
| M repetition scoring and material draw bias | +48 | R1 |
| O royal back-rank PST, pawn shield, castling | +27 | R2 |
| P zone-attack king danger, open-shield penalty | +21 | R3 |
| B continuation history | +14 | R6 |
| F TT static-eval cache | +9 | R8 |
| I correction history | +9 | R7 |
| Q stability-scaled soft deadline | +4 | R11 |

Evaluation therefore goes first. The search family that netted zero goes
last, and goes in gated by `capabilities` rather than unconditionally,
which is the one thing iteration 3 never tried.

## Stage ladder

Each stage is one commit. Each states what it restores, what it derives
rather than tunes, and what it is gated by.

Support state lands with the stage that consumes it, not as a preparatory
stage of its own. A stage adding a table no term reads yet is dead weight
and separates each term from the state it needs, so `adjacency_mask`,
`royal_shield_mask`, `royal_front_mask`, `zone_attack`,
`zone_attack_best`, the `pawn_*` masks, `pawn_hash`, `pawn_board`,
`pair_score`, `has_castled`, `draw_bias`, and the `PTable` and `PTEntry`
pair each appear in the stage below that first uses them. The pawn hash
joins the identity separation from P4 rather than reusing the canonical
key.

### R1. Draw scoring and material draw bias

Restore `draw_score!` and route `terminal_score!` and every repetition and
perpetual path through it, with `draw_bias` derived. Iteration 3's largest
single result, +48.

Acceptance: signature moves only where a draw is scored; a drawn endgame
that is winning on material no longer evaluates to zero.

### R2. Royal back-rank PST, pawn shield, castling incentive

Restore `king_shelter!`, `pawn_shield!`, and `castling_bonus!`, folding
the existing `royal_shelter!` into them rather than running both. Scalars
`king_shelter_bonus`, `pawn_shield_bonus`, `castled_bonus`, and
`castling_rights_bonus` are derived from mean non-royal piece value.

### R3. Zone-attack king danger and open shield

Restore `king_danger!` and `open_shield!` with `king_danger_scale` and
`open_shield_penalty` derived.

### R4. Pawn structure

Restore `pawn_structure!` and its seven sub-terms, with connected,
doubled, isolated, backward, and passed scalars from
`derive_pawn_parameters`. This is the stage that introduces the pawn
masks, `pawn_board`, `pawn_hash`, and the `PTable` and `PTEntry` pair.

### R5. Tempo, imbalance, pair bonus

Restore the three remaining scalar terms, derived.

### R6. Continuation history

Restore 1-ply and 2-ply continuation history, the best of A-L at +14.
Iteration 2 measured its removal at +26% nodes on correction history and
kept continuation; both are load-bearing.

### R7. Correction history

Restore pawn-hash correction history, +9, and the all-node malus update
rule. Do not standardise updates to fail-high-only: that was measured at
+19 to +47% nodes.

### R8. TT static-eval cache

Restore `tt_enc_eval!` and `tt_eval!` and the cached static evaluation in
the main transposition payload, +9.

### R9. Gated search family

Restore PVS, razoring, ProbCut, IIR, mate-distance pruning, and check
extensions, each gated on `capabilities`. This family netted about zero in
iteration 3 applied unconditionally; the hypothesis under test is that the
gate is what was missing, not the mechanisms.

### R10. Board width and staged move generation

Restore `BoardBits = U256` as the default with `wide-board` reinstated in
`src/Cargo.toml`, and restore `generate_all_quiets_and_drops` as the
second stage of staged generation. Both are per-node cost levers, not
strength changes.

Reference figures, 16-position bench, seed 42, one thread, `phaseF-3`
against the current line: standard depth 11 is 208,063 nodes at 50.2 ms
against 133,695 at 37.8 ms; grand depth 9 is 8,639,479 at 3048 ms against
2,400,555 at 2201 ms. The two run different evaluators, so nodes and nps
are confounded and only games settle the sign.

### R11. Soft and hard deadline split

Restore the stability-scaled soft deadline, +4. Carry forward the known
defect: F-3 spent 65% of the clock in the first 18 moves and less than
E-3 after move 21, so the SPRT sign on the time manager was wrong. Derive
the split rather than restoring F-3's constants.

## Measurement

Per stage: release build with no warnings, the 38-config depth-6 signature
from plan 18, and the 16-position bench. Games only where a stage claims
Elo, and always reported as raw W/L/D. The `sprt` verdict string reports
from `bin-a`'s view; state which binary is `bin-a` whenever it is quoted.
