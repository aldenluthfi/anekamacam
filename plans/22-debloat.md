# Plan 22 — debloat pass

Goal: large red diff, same behaviour, one revertible commit per stage.
Plan 21's restore ladder stays paused at R6 until this lands.

## Scope decisions

1. **Const rule** — shared up, families unified, trivia inlined. Prelude
   keeps cross-file consts; the four families straddling the
   prelude/module boundary (`OPT_*`, `*_DIR`, `EMBEDDED_*`, `HASH_*`) get
   unified into prelude; file-private detail stays local; single-use
   trivia is inlined and deleted.
2. **Behaviour** — every refactor commit must reproduce bench node counts
   exactly. Semantic fixes land as separate `[SEMANTIC]` commits.
3. **Ordering** — debloat first, then plan 21 R7-R11.
4. **`move_list.rs`** — out of scope, except D11's two mechanical lines.

## Baseline

Captured on `8e57c1a`, clean tree, release build.

    ANEKAMACAM_SEED=42 ./target/release/anekamacam \
        debug-headless bench <variant> <depth> --limit <n>

| variant    | depth | limit | nodes   | nps     |
|------------|-------|-------|---------|---------|
| standard   | 11    | 16    | 190760  | 4307297 |
| shogi      | 9     | 8     | 1391464 | 1697568 |
| xiangqi    | 10    | 8     | 369526  | 1237322 |
| crazyhouse | 9     | 8     | 1009750 | 1248544 |
| grand      | 9     | 8     | 1432374 | 1160025 |

Stages touching derivation, `StaticState`, or `State::new` — D3, D11,
D12, D13 — additionally run `sittuyin 9 --limit 8` and
`janggi 9 --limit 8`. Never run `derive` while verifying: it overwrites
`res/param/*/latest.param` and shifts every later bench.

## Ladder status

| Stage | What                                       | Δ lines | Status |
|-------|--------------------------------------------|---------|--------|
| D1    | delete unreachable code                    | −170    | done   |
| D2    | unify const families, inline trivia        | −5      | done   |
| D3    | collapse five `populate_relevant_*`        | −86     | done   |
| D4    | io dedupe                                  | −278    | done   |
| D5    | TT/QT unification                          | −139    | done   |
| D6    | search dedupe                              | +27     | done   |
| D7    | `graphics.rs` idiom dedupe                 | −308    | done   |
| D8    | `move_parse` small dedupe                  | −25     | todo   |
| D9    | fold single-caller helpers, `game/`        | −200    | todo   |
| D10   | fold single-caller helpers, `io/`+`debug/` | −180    | todo   |
| D11   | fold `has_castled` into `castling_state`   | −5      | todo   |
| D12   | group `StaticState` eval/search fields     | −25     | todo   |
| D13   | move param structs out of `state.rs`       | +1      | todo   |
| D14   | three `thread_local!`s onto `SearchInfo`   | −10     | todo   |
| D15   | `move_parse` atomic/multi_leg unification  | −500    | needs  |
|       |                                            |         | go-ahead |

Branchless rewrites are a running rule applied inside whichever stage
already touches the line, never a standalone sweep. Transform only when
both arms are side-effect-free, cannot panic, and are cheap; neither arm
guards an expensive call such as `see!`; and the result is shorter.
Prefer a mask over a multiply for wide types.

## D1 — delete unreachable code · done

Removed, with their prelude re-exports:

- `vector.rs` `set_atomic`, `set_whole`
- `termination.rs` `has_repetition` (its null-move paragraph merged into
  `count_repetitions`, which the deleted doc had cross-referenced)
- `util.rs` `benchmark_search`
- `logger.rs` `configured_log_level`, `verbosity_enabled!`
- `board.rs` `toggle!`, `xor!`, `not!`
- `moves.rs` `m_pseudocapture!`, `enc_captured_unmoved!`

197 deletions, 27 insertions across 7 files. All five bench variants
reproduce the baseline node counts exactly.

### Why `enc_captured_unmoved!` is dead despite a live decoder

Worth recording, because the bit-111 flag looks unwritten and is not.

`Snapshot` (`move_list.rs:2774-2789`) carries `virgin_hash` but **not**
`virgin_board`, so undo restores the board incrementally, guarded by the
captured-was-unmoved flag at `move_list.rs:3093` (single capture) and
`:3312` (multi). Only `enc_multi_move_captured_unmoved!` is ever called
— `:1047`, `:1092`, `:1129` — which sets bit 33 of a 34-bit payload
word. There is no single-capture call site, yet the flag still arrives:

- `move_list.rs:720-830` is a **validator** (`valid` / `break`); it never
  builds a move. Its three `capt_unmoved` reads only feed the Betza
  `v`/`not_v` capture predicate (`vector.rs:91`, bit 21).
- The builder is `process_multi_leg_vector!`, and it pushes a payload
  word per capture regardless of how many there turn out to be.
- At `:1173-1181` the move type is chosen by `$scratch.len()`. For one
  capture it emits `enc_capture_part!(encoded_move, $scratch[0])`, which
  is `|= (payload & 0x3_FFFF_FFFF) << 78`. Payload bit 33 lands at bit
  111 — exactly what `captured_unmoved!` reads.

So single captures inherit the flag by payload aliasing, and a dedicated
encoder would be a second way to write the same bit. The aliasing is now
stated in the `enc_capture_part!` doc block.

`legal_moves!` at `move_list.rs:317-318` is the castling builder and
needs no flag: castling undo (`:3501`) restores virgin unconditionally.

## D2 — unify split const families, inline trivia · done

Moved into the `prelude.rs` shared block so no family straddles the
prelude/module boundary:

- `*_DIR`: `PARAMS_DIR` (`game_io.rs`), `LOG_DIR` (`logger.rs`), joining
  `DATA_DIR`
- `EMBEDDED_*`: `EMBEDDED_PARAMS` (`game_io.rs`), joining the other three
- `OPT_*`: the five in `protocol.rs`, joining `OPT_THREADS`
- `HASH_*`: `HASH_MAX_MB` (`protocol.rs`), joining `HASH_DEFAULT_MB`

Inlined and deleted: `DEFAULT_PROTOCOL` and `LOG_HISTORY_KEEP`, one use
each. `DEFAULT_DROP` was on the list and is **kept** — it has two use
sites (`game_io.rs:1419`, `:1520`), so inlining would duplicate a magic
string. `SPRT_DIR` stays local: it belongs to `sprt.rs`'s cohesive
eight-const debug block, not to the runtime `*_DIR` family.

Only −5 lines. Moving a const does not delete it; this stage buys
coherence, and the ladder's line budget should not have counted it.

### Score-band retype: dropped, not deferred

The plan wanted `TABLE_MOVE_SCORE`, `KILLER_MOVE_SCORE`, and
`UNMAKEABLE_CAPTURE_SCORE` (`usize`) unified with
`WINNING_CAPTURE_SCORE`, `QUIET_MOVE_SCORE`, and `LOSING_CAPTURE_SCORE`
(`i32`), dropping the casts in `score_move!`. Not worth doing:

- The cast is not incidental. `score_move!` computes
  `(LOSING_CAPTURE_SCORE + see_score) as usize` with `see_score`
  negative, and correctness rests on the sum never going negative —
  `LOSING_CAPTURE_SCORE` is 983_617 and the `see_score == -INF` case is
  caught by the `UNMAKEABLE_CAPTURE_SCORE` branch above it. Under `i32`
  a negative sum would sort last; under `usize` it wraps and sorts
  first. Whether that is reachable depends on derived piece values, so
  the bound cannot be settled by inspection.
- `search.rs:917` does `scores[index] as i32 - LOSING_CAPTURE_SCORE` —
  a subtraction recovering the signed SEE score, which is exactly the
  "not a plain compare" case the plan said to split out.
- The win is six casts on existing lines and **zero deleted lines**.

Bad trade against a bit-identical gate. Skipped.

## D3 — collapse the five `populate_relevant_*` · done

The five bodies were identical modulo set type, generator, and target
field, and all four generators already share one signature:

    fn(&Piece, u32, &State, &[Vec<T>]) -> Vec<T>

so a generic `populate_relevant<T: Clone>` taking the generator as a
`fn` pointer replaces all five, with `precompute` naming the static it
fills. A macro was unnecessary. `MoveSet`, `DropSet`, and `PatternSet`
are all `Vec<_>` aliases, so `vec![Vec::new(); n]` is the same
initializer the five wrote out by hand.

`populate_relevant_attacks` folded into `precompute` at the same time —
three lines, one call site, and it sat inside the same doc region.

128 deletions, 42 insertions in `state.rs`.

Gates: the five bench variants plus `sittuyin` (12027791 nodes) and
`janggi` (301548) — new numbers, since neither had a prior baseline —
and `perft <variant> 3 --suite` across all seven, which is what actually
proves the tables: sittuyin 12/12, janggi 21/21, crazyhouse 12/12,
shogi 12/12, xiangqi 33/33, grand 3/3, standard 20256/20256.

## D4 — io dedupe · done

Two helpers absorb eleven copies.

`split_sections(&str) -> HashMap<String, Vec<String>>` in `game_io.rs`,
prelude-exported. The plan named three copies; there are **four** —
`protocol.rs:127-150` (`list_variants`) is a fourth, differing only in
local names and `str::trim` vs a closure. `util.rs:737`
(`parse_perft_content`) shares only the `COMMENT_PATTERN` strip, has no
section grammar, and stays as it is.

`config_text(&str) -> String` replaces `embedded_config`, which had two
call sites both wrapping it in the same seven-line disk fallback. The
fallback moves into the helper and both parsers open with one line:

    let sections = split_sections(&config_text(path));

`piece_indices(&str, &HashMap<char, usize>) -> Vec<usize>` replaces the
eight piece-char lookup blocks, each a 20-30 line `len() == 2` /
`len() == 1` / `else panic!` chain that resolved one or two characters
and then did the same thing per index. `.len()` is kept as the length
test rather than `chars().count()` so a multi-byte key still panics
exactly where it did; every shipped `.conf` is ASCII.

Two orderings were preserved deliberately: the promotions block resolves
its piece key *before* walking promotion characters, so a config bad in
both places still panics on the piece character; and the zone blocks call
`parse_bit_fen` once per index, as the unrolled pairs did, not once
hoisted out.

398 deletions, 120 insertions across four files.

Gates: all seven bench variants node-identical, all seven perft suites at
depth 3, and a `uci` handshake — the 39-variant list is built by
`list_variants`, one of the deduped copies, and the janggi board after
`position startpos` proves the dictionary translator's copy too.

## D5 — TT/QT unification · done

`TTEntry` and `QTEntry` were the same 64-byte struct twice (`[u128; 3]`
+ `u64` + `AtomicU64`), so one `HashEntry` replaces both. `TTable` and
`QTable` become aliases of

    pub struct HashTable<const NUM: usize, const DEN: usize>

with `type TTable = HashTable<2, 3>` and `type QTable = HashTable<1, 3>`,
so the two `Default` budgets survive as `HASH_DEFAULT_MB * NUM / DEN`
(170 MB and 85 MB, as before) and **no call site changed** — every use is
`Arc<TTable>`, `&TTable`, or `TTable::with_mb(n)`. Unused const generic
parameters are legal; only unused *type* and lifetime parameters need a
`PhantomData`.

Entry size is what the gate actually rests on: `with_mb` divides by
`size_of::<HashEntry>()`, so a layout change would resize both tables and
move every node count. Both old structs had identical fields, so 64 bytes
is preserved by construction, and the five node-identical benches confirm
it end to end.

Three more macros were shared out rather than left mirrored:

- `table_index!` replaces `tt_index!` and `qt_index!` — byte-identical
  bodies.
- `probe_hash_slot!` holds the seqlock/parity front half of all three
  probes (`probe_tt_entry!`, `probe_pv_move!`, `probe_qt_entry!`). The
  caller names the two payload words and supplies the miss value:

      probe_hash_slot!($table, $key, None, |move_slot, data_slot| { .. })

  Passing the names as `ident` fragments rather than binding them inside
  the macro is what makes this work — a macro-internal `let` is in the
  macro's hygiene context and the caller's body cannot see it, whereas an
  ident taken from the call site resolves at the call site. A closure
  would have worked too, but this keeps the bodies plain expressions.
- `commit_hash_entry!` holds the store tail: replacement counter, then
  the seqlock write with the parity word last before `age`.

The two `should_write` predicates stay written out. They differ by the
`old_depth <= $depth` term, and folding that into a shared macro would
have cost more lines in parameters than the one line it saves.

Also deleted: `is_empty` on all three tables — no call site anywhere, and
the only surviving `is_empty()` in the tree is on a `Vec` in
`graphics.rs`. `HashTable::with_entries` folded into `with_mb`, its only
caller. `PTable::with_entries` is left alone: D14 would need it back.

The two bit-layout ASCII diagrams document the *packing*, not the
container, so they moved onto the `tt_*` and `qt_*` packing macro
clusters rather than dying with the structs.

428 deletions, 289 insertions across two files — **−139, not the −200 the
ladder budgeted.** The QT container was ~180 duplicated lines, but the
three shared macros cost ~90 in body and doc to buy them back. The
estimate was too optimistic about how much a shared macro is free.

Gates: `cargo build --release` warning-free and the five bench variants
node-identical. D5 touches no derivation path, so sittuyin and janggi
were not required.

## D6 — search dedupe · done

Two of the four planned items landed; two were dropped, and the stage
came out **line-positive**. That is the finding, not the failure.

`no_move_verdict!` (`termination.rs`, beside `outcome_score!`) reads the
variant's verdict on a side with no moves — checkmate outcome in check,
stalemate outcome otherwise — and returns it with a flag for whether the
verdict is inverted, which it is when a drop barred from mating delivered
the mate. Three readers now share it: `util.rs adjudicate_no_move`, the
quiescence leaf, and the negamax leaf. They had drifted: the quiescence
leaf hardcoded `state.termination.checkmate` instead of selecting on
`in_check`. That reads the same today only because its guard is
`in_check && legal_moves == 0`; a variant whose stalemate rule ever
reached that leaf would have read the wrong field. The three sites cannot
drift again.

Both search leaves take the branchless form, which is where the line win
in `search.rs` came from:

    return outcome_score!(state, outcome) * (1 - 2 * inverted as i32);

and `adjudicate_no_move` picks the losing side the same way — exact,
because `WHITE == 0` and `BLACK == 1`:

    let subject = state.playing ^ inverted as u8;

`move_key!` (`search.rs`, after `SearchInfo`) names `piece * board_size +
end`, the cell every history table is indexed by. Four sites built it and
called it three different things — `history_index`, `key`, `index`. The
`search_hist` field comment now says `[move key]` to match. `board_size`
stays a macro parameter rather than a `statics` read: the scoring loops
hoist it, and `state` is `&mut` through `pick_by_score!`, so LLVM cannot
hoist the dereference itself.

**Dropped, both symmetric rather than inconsistent:** the TT and QT
halves of `log_table_stats`, and the eight-line counter reset at
`search.rs:189-197`. `HashTable<2, 3>` and `HashTable<1, 3>` are distinct
types, so no array covers them; a method or generic function costs about
as many doc lines as it deletes body lines.

Which is the stage's real lesson. **At this codebase's doc density a
two-site dedupe loses lines; only three sites and up pay.** Every shared
macro owes ~10-14 lines of `///` block before its first line of body. D6
budgeted −25 and delivered 66 added against 39 removed — **+27 lines
total, −9 of actual code**, the rest doc. D5 undershot for the same
reason. The remaining estimates in the ladder are built on raw duplicate
counts and should be read as upper bounds.

Gates: warning-free release build, five bench variants node-identical,
and `run_endgame_fixtures.sh` at 37/38. That one failure — xiangqi,
`perpetual one cycle short`, expecting `mate -2` and getting `cp -950` —
**is pre-existing**, confirmed by stashing the stage and re-running
against `85b4637`, which fails identically. It is not a no-move-leaf
path: the position has legal moves. Logged below.

## D7 — `graphics.rs` idiom dedupe · done

3512 lines to 3204. 573 deletions against 265 insertions, of which 70 are
doc — **−308 total, −238 of actual code**, and the first stage where the
doc tax did not eat the win, because the sites number in the dozens
rather than in twos.

Three file-private macros, one grouped doc cluster, no `#[macro_export]`
— nothing outside the debug interface builds ratatui widgets, so none of
this belongs in the prelude.

- `split_area!` — the four-call `Layout::default()` builder chain, 34
  sites. The constraint list stays a plain expression, so the four sites
  that choose their list with an `if` still fit in one invocation. The
  `overlap` arm carries `Spacing::Overlap(1)`, which is what makes
  adjacent panes share a border line.
- `padded_block!` — full border plus one column of inside padding, 13
  sites, with `style` and `merge` arms for the focus border and the
  merged-border form. Builder-call order differed across those 13
  (`.padding` before `.borders`, `.border_style` in the middle); each
  call writes a distinct field, so the unified order is value-identical.
  Panes deliberately drawn frameless keep their explicit `Borders::NONE`
  and were not converted.
- `guide_label!` — the help overlay's 20 layout-guide labels, which were
  each a `Paragraph::new(vec![Line::from(vec![Span::from(..)])])` inside
  a merged-border box. `Paragraph::new(&str)` renders identically: `&str`
  into `Text` is one line, one span, no style, and no label contains a
  newline. The `center` arm is the board pane, drawn bare so its box is
  not mistaken for the board's own. Its box form is now one line of
  `padded_block!(merge)`.

`clamp_scroll` was a 16-line non-capturing closure written out twice
(`draw_game_tab`, `draw_playground_tab`); it is now one file-private
function. Its four-arm chain collapsed to three: the pinned-at-`u16::MAX`
arm and the sitting-at-the-end arm both end up writing the sentinel, and
they can share because **`current` is always `scroll_map[key]`** — every
caller reads it from there one line above — so re-pinning a pane already
at the sentinel writes the value it already had. That precondition is now
in the doc block, since it is the only thing holding the merge up.

The three `TAB_FOCUS_*` were function-local consts declared twice, three
in `draw_game_tab` and one in `draw_playground_tab`. Hoisted to file
scope beside the other TUI layout constants.

Not converted: the 51 `Style::default()` sites. They are one line each
already and share no shape worth naming. The `Block::default()` sites
with `Borders::NONE`, or with a border style but no padding, are also
left alone — folding them in would have meant arms for shapes with one
or two uses.

Gates: `cargo build --release` warning-free, and `standard 11 --limit 16`
still at 190760 nodes — which proves only that the binary is intact, not
that the interface draws right. **The TUI smoke run is outstanding and is
the user's to make**: launch, cycle every tab, open the help popup,
scroll both ends of a scrollable pane, quit. Scripting the ratatui
console is not something this engine supports. Line-length and
comment-column checks pass; the 20 lines over 80 columns are the
pre-existing help-text literals at `:723-780`, byte-identical to `HEAD`.

## Deferred, not resolved in this ladder

- PST-residual / param-schema question — stays in plan 21.
- `game_phase` ratchet-vs-refresh divergence (`move_list.rs:2732-2741`
  ratchets with `cmp::max`, `util.rs:260-268` recomputes without it) —
  real bug, own `[SEMANTIC]` commit.
- Pawn table ignoring `setoption Hash` and surviving `ucinewgame` — the
  fix moves node counts, so it follows D14 as `[SEMANTIC]`.
- Endgame fixture `xiangqi / perpetual one cycle short` fails on
  `85b4637` and every commit this ladder has touched, expecting
  `score mate -2` and getting `score cp -950`. Pre-dates the debloat
  pass; the position is not at a no-move leaf, so it is a perpetual
  adjudication or search-horizon question, not a refactor artefact. Own
  `[SEMANTIC]` investigation, outside this ladder.
