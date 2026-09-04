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
| D8    | `move_parse` small dedupe                  | −6      | done   |
| D9    | fold single-caller helpers, `game/`        | −105    | done   |
| D10   | fold single-caller helpers, `io/`+`debug/` | −181    | done   |
| D11   | fold `has_castled` into `castling_state`   | +4      | done   |
| D12   | group `StaticState` eval/search fields     | −23     | done   |
| D13   | move param structs out of `state.rs`       | 0       | done   |
| D14   | three `thread_local!`s into `State`         | −13     | done   |
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

## D8 — `move_parse` small dedupe · done

Estimated −25, landed **−6** (112 insertions, 118 deletions). Third stage
running to roughly net zero for the same reason, and the reason is now
worth stating as a rule rather than a surprise: **at this codebase's doc
density a shared macro or function owes 10-21 lines of `///` before its
first body line, so a two-site dedupe loses lines. Only three sites and
up pay.** Every remaining estimate built on a raw duplicate count is an
upper bound; D9 and D10 should be read that way.

Three changes, all four-site or eight-site, all bit-identical.

**`irregular_vector_direction` returns `&'static str`.** The name comes
out of `CARDINAL_INDEX_TO_STR` (`:168`), a `lazy_static` map of string
literals, so the data always outlived the vector it was read from — but
the elided lifetime in `-> &str` tied it to the argument, and every one
of the eight callers therefore had to bind the displacement to a local
first. Saying `&'static str` and dereferencing the map hit lets all eight
pass the temporary directly. Two atomic sites collapse to one line each;
the four multi-leg sites drop their `rotation_vector` binding, and the
two at shallow indentation also fit their `.expect()` chain onto fewer
lines. A dedicated `trailing_rotation` helper for those four blocks was
measured and rejected — net zero after doc tax.

**`range_bounds!`, four sites.** `{i..j}` and `:{i..j}` are matched by
different regexes but read their bounds identically, and all four
handlers — `process_atomic_range_token`, `process_multi_leg_range_token`,
and the two `*_colon_range_token` — had the same 13-line preamble:
capture, log, `.get(1)`, `.get(2)`, then a parse in each match arm. One
macro parameterised on `(regex, label, token)` replaces it with a single
line and lets both `(Some, Some)` arms bind `start_count`/`end_count`
directly and both `(Some, None)` arms bind `count`. `label` is a literal
spliced through `concat!` into both the log line and the panic message,
so the two forms cannot report each other's name — which the hand-written
copies were one careless paste away from doing.

**`colon_range_head!`, two sites**, on top of `range_bounds!`. Rejects a
colon-range with nothing repeatable in front of it and returns the
element with the bounds. Two sites is below the threshold above and would
have lost lines alone; it earns its place only by delegating the bounds
read to the four-site macro. The evaluated-expr variant is an `ident`
parameter because the atomic and multi-leg element enums are distinct
types — passing the name as `ident` resolves it at the call site.

Gates: warning-free `cargo build --release`; all seven perft suites pass
(standard 20256, shogi 12, xiangqi 33, crazyhouse 12, grand 3, sittuyin
12, janggi 21); all seven bench node counts reproduce exactly. Perft
across all seven variants is the strong gate here — the parser derives
every piece's vectors from Betza strings at load time, so a changed
expansion moves movegen immediately. The 184 lines over 80 columns are
byte-identical to `HEAD`: box-drawing doc diagrams that `awk length`
measures in bytes. Comment-column check clean, and the file carries four
fewer double-blank lines than `HEAD`.

Not attempted: the 14 mirrored `atomic`/`multi_leg` function pairs. That
is D15 and still needs an explicit go-ahead.

## D9 — fold single-caller helpers, `game/` · done

−105. The stage's own list named sixteen helpers, but eight of them live
in `io/` or `debug/`, not `game/` — `compute_budget`, `stop_search`,
`replay_moves`, `format_search_keys`, `clamp_material`, `mirror_square`,
`opening_line`, `dot`. Splitting the ladder by directory and then listing
across it was the plan's error, not a discovery; those eight move to D10
so each commit reverts one area.

Folded, seven:

- `expand_wildcard` into `parse_pattern`. Two `replace` calls; the caller
  already documented that `*` means the whole piece alphabet.
- `generate_stand_off_patterns` into `State::generate_piece_stand_off`,
  and out of the prelude. The empty-expression case has to stay an
  explicit `PatternSet::new()` rather than a `filter` on the split: an
  empty branch inside a non-empty expression is malformed config and must
  keep reaching `parse_pattern`, which panics on it.
- `pass_class` into `search_key`. Every other context slot in that
  function is already computed inline, so the helper was the odd one out.
  Kept as `map_or(0, ..)`, not an `if let`: a position with no history
  still folds the zero class in, and skipping the XOR would change the
  key.
- `derive_piece_maneuverability` into `derive_piece_value`. The only
  member of the `derive_piece_*` family with a single call site — the
  other four are shared, which is what earns them their names.
- `setup_census_key` into `walk_setup_endings`. The census is now built
  before the cap checks instead of inside the third `||` term; the
  short-circuit on `visited.insert` is unchanged, only the key
  construction became unconditional.
- `derive_square_score` into `derive_pst`, which also hoists the phase
  occupancy and the two weights out of the per-square closure. Same
  operands in the same order, so the sum is bit-identical.
- `AtomicVector::from_tuple` into the one `From` impl that called it.

Not folded, by judgement: `derive_pawn_interference`. Its call sits in a
column of six same-shaped `[entry] = derive_pawn_*(..)` assignments, and
its doc carries an ASCII diagram of the interference mask that has
nowhere better to live.

A crude single-caller scan over `game/` turned up ~50 more, and almost
all of them are `move_parse.rs` (D15) or the `derive_*`/`termination.rs`
families, where the sibling name is the documentation and the bodies are
20-200 lines. `continuation_bases` (`search.rs:1139`) is single-caller
and stays: folding a 20-line loop into the hot search buys nothing.

Gates: warning-free build; all seven perft suites pass; all seven bench
node counts reproduce exactly. `parameters.rs` is derive-time and the
bench reads stored params, so the three folds there are proven by
construction (same operands, same order), not by the node gate.

## D10 — fold single-caller helpers, `io/` + `debug/` · done

−181, against a −180 estimate — the first stage in the ladder to land on
its number. It should not be read as the estimate getting better: the
folds here happen to sit on the doc-tax rule's good side, where every
deleted helper takes an 11-to-18-line `///` block with it and the body
moves across as-is.

Folded, ten:

- `extract_fen_components` into `parse_config_file`. Three capability
  flags set from one `split_whitespace().skip(2)` pass; the early `break`
  once all three are set is preserved.
- `format_bitboard` into `format_board`, the only caller. The set/clear
  cell picks branchlessly out of `["0  ", "1  "]` rather than through an
  `if/else`, which is both the house idiom and two lines shorter.
- `archive_stamp` into `roll_latest`, with its created-else-modified-else-
  now fallback chain spelled as one `and_then`/`or_else`. `roll_latest`'s
  own doc absorbed the explanation, since the reason the stamp exists is
  that backups must sort chronologically under a lexicographic ordering.
- `level_to_verbosity` into the `init_logging` format closure. The
  five-arm `match` stays a `match`, not `record.level() as u8`: the cast
  would couple the on-disk log format silently to the `log` crate's
  discriminant values.
- `terminal_reason` into `game_outcome`, and out of the prelude — the
  re-export had no user outside `termination.rs`.
- `replay_moves` into `handle_position`. The two `Err(String)` returns
  become the caller's own `log_2!` + `position_valid = false` + `return`,
  so the diagnostic strings are byte-identical and the `Result` round trip
  disappears. Needs a `let replayed = &mut scratch;` binding: `make_move!`
  names `$state` many times, so substituting the expression `&mut scratch`
  takes a fresh mutable borrow at each use and does not compile.
- `compute_budget` into `start_search` as an `if / else if / else` block
  expression. The two early returns map onto the first two arms, so the
  arithmetic and its saturation are unchanged; the policy sentence moved
  into `start_search`'s doc.
- `mirror_square` into `extract_sample`. The one-line "same file, flipped
  rank" clause moved into `extract_sample`'s doc, which already explains
  that Black pieces subtract their PST at the mirrored square.
- `dot` into `model_score`, its only caller — and, as the cluster doc
  said, the whole of the linear model. The "Tuning math primitives"
  cluster keeps its three remaining members.
- `clamp_material` into the Adam epoch loop, where it sits as a flat
  sibling loop at the same nesting depth. The 14-bit-export rationale
  moved into `run_tuning`'s doc.
- `opening_line` into the `'pairs:` loop in `run_sprt`. The reason it is
  captured once per pair — both games open the same way from opposite
  colours — is a fact about the call site, so it became a col-81 comment
  there rather than a doc block somewhere else.

Not folded, five, all by the family-symmetry rule:

- `format_en_passant_square` and `format_search_keys` (`game_io.rs`) are
  two of a seven-strong, prelude-exported display-helper family under one
  doc cluster. Folding one makes the cluster lie.
- `elo_from_score` and `log_likelihood_ratio` (`sprt.rs`) are named
  statistical formulas sharing a cluster with `expected_score`; folding
  puts the math inline in a `format!` argument and a loop body.
- `stop_search` (`protocol.rs`) is one of the `handle_*` command handlers
  the dispatch `match` calls uniformly. Its arm would stop looking like
  its neighbours.

`run_derive_headless` (`util.rs`) is single-caller but stays: folding it
into the `"derive" =>` arm makes that arm about five times the length of
its `run_*_command` siblings. Moving and renaming it into `headless.rs`
was considered and dropped — churn, no lines. `benchmark_headless_perft`
correctly stays in `util.rs`; `graphics.rs:2842` is a second caller.

Gates: warning-free build; all seven perft suites pass; all seven bench
node counts reproduce exactly; endgame fixtures 37/38, the one failure
being the pre-existing xiangqi `perpetual one cycle short`. UCI smoke on
the rewritten replay path — a good line, a garbage token, and a repeated
move — leaves the session state and the two diagnostics exactly as
before. `extract_fen_components` feeds capability flags that drive
movegen, so seven identical perft suites across seven configs is the real
gate on that fold.

## D11 — fold `has_castled` into `castling_state` · done

`State.has_castled: [bool; 2]` is gone. The castled mark now rides in
`castling_state` two bits above the rights it outlives:

```
bit  3210
     KQkq        CASTLE_RIGHTS  = 0b0000_1111
    ^            CASTLED << WHITE
   ^             CASTLED << BLACK   (CASTLED = 0b0001_0000)
```

`CASTLE_RIGHTS` and `CASTLED` join the four `*_CASTLE` bits in
`prelude.rs`, so the whole layout reads in one place.

Four sites moved. `make_move!` sets `castling_state |= CASTLED <<
piece_color` where it wrote the array. The paired `undo_move!` clear was
deleted outright rather than translated: `Snapshot.castling_state` is
captured before any modification (`move_list.rs:1555`, stored at `:2778`)
and restored as the whole byte at `:2846`, which runs before the
move-type dispatch the clear lived in — so the line was already dead
weight, undoing something the byte restore had undone one statement
earlier. That plus the set are the two `move_list.rs` lines this pass
sanctions.

`castling_bonus!` became the branchless form, which is what actually
paid here:

```rust
        castling!($state) as i32 * [
            $state.statics.castling_right_value * holds as i32,
            $state.statics.castled_value,
        ][castled as usize]
```

Two indexed picks over two bit tests, replacing a four-arm chain.

### The masking, which was wider than planned

`CASTLING_HASHES` is `[u128; 16]`, and growing it would re-draw every
later Zobrist value from the seeded `StdRng`. So the new bits are masked
off at every index: `hash.rs:53`, `util.rs:632`, and both indexes inside
`hash_update_castling!`.

The plan named three index sites. It missed the fourth thing that needs
masking — the **comparison**. `hash_update_castling!` early-outs on
`old != new`; unmasked, a move that sets a castled mark without spending
a right reads as a rights change and XORs `CASTLING_HASHES[r]` twice
against itself. Value-identical by luck, but only because the two
indexes would have been equal after masking; with one masked and one not
it is an out-of-bounds read. Masking all three turned the macro from an
expression into a block so the two mask results are named once.

`util.rs:673` iterates all 16 entries and needs no change once the
indexes are masked.

### The two risks the plan flagged, both discharged

All six rights-clearing writes in `move_list.rs` (`:1722`, `:1910`,
`:1946`, `:2272`, `:2319`, `:2685`) are `&= !{...}` of a `u8` literal,
whose high bits are set — the new marks survive every one. No write uses
`= 0` except `game_io.rs:2045` in `parse_fen`, which is bit-identical
because every `parse_fen` call site is immediately preceded by `reset()`
(`protocol.rs:272`, `:341`, `:465`, `:787`, `:877`; `headless.rs:192`;
`state.rs:1110` inside `load_fen`), and `reset()` already cleared
`has_castled`.

### Gates

Warning-free build; all seven bench node counts reproduce exactly; all
seven perft suites pass; endgame fixtures 37/38 with the pre-existing
xiangqi `perpetual one cycle short` failure.

None of that proves the bit works. The standard bench is sixteen bare
endings with no castling term in reach, and the perft suites do not call
eval at all — a build that never set `CASTLED` would pass every gate
above. The real gate is a two-position eval comparison over an identical
board:

```
debug-headless evaluate standard --protocol uci \
  --moves e2e4 e7e5 g1f3 g8f6 f1c4 f8c5 e1g1          -> -28 cp
debug-headless evaluate standard --protocol uci --fen \
  "rnbqk2r/pppp1ppp/5n2/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQ1RK1 b kq - 5 4"
                                                       ->   9 cp
```

Same placement, same rights, same phase; the FEN load has no castling
history. The 37 cp swing toward the side that castled is
`castled_value`, and `castled_value` is exactly twice
`castling_right_value` — so 18 would have meant the mark was misread as
a right and 0 would have meant it was never written. Only one side
castles in the test on purpose: with both castled the term cancels and
the comparison proves nothing.

Delta is **+4**, not the −5 the plan guessed: the mask fix costs more
lines than the field saves. The stage buys the hash-index invariant and
one fewer field beside `Snapshot`, which is what it was for.

## D12 — group `StaticState` eval/search fields · done

`StaticState`'s 76 flat fields become 28 plus two grouped ones. The
`EVALUATION FIELDS` banner group minus its hot four is now
`EvalParams`; the `SEARCH FIELDS` group is now `SearchParams`. Both
`#[derive(Default)]`, both declared directly under `StaticState` so the
reader meets the type right where the field names it.

`pst_opening`, `pst_endgame`, `opening_score` and `endgame_score` stay
flat, and that choice is what kept the stage cheap. Those four are the
only members of either group the incremental make/undo path reads —
`move_list.rs` touches `statics.pst_opening` 34 times,
`statics.pst_endgame` 34 times, and the two thresholds once each, and
nothing else from either group. Leaving them at the top level means
`move_list.rs` is not in this diff at all.

The line win is `State::new`, exactly as forecast: 48 zeroing
initializers become seven, because only three fields are not `Default`
at rest.

```rust
            eval: EvalParams {
                shield_pieces: vec![false; piece_count],
                forward_steps: [1, -1],
                draw_span: 1,
                ..Default::default()
            },
            search: SearchParams::default(),
```

`shield_pieces` keeps its explicit `vec![false; piece_count]` rather
than defaulting to empty — the derivation passes index it before they
fill it, so an empty vector is a different program, not a shorter one.

### The rename

91 field accesses across three files: `evaluation.rs` 45,
`parameters.rs` 48, `search.rs` 10. Zero elsewhere — no `io/`, no
`debug/`, no `move_list.rs`. Five of the `parameters.rs` sites write
through `state.static_mut().<field>` with no `statics.` binding in the
text and so were invisible to the access grep; the compiler found all
five at once.

Two 80-column casualties, both in `parameters.rs`:

- `for slot in 0..state.statics.ring_counts[landing] as usize {` was 76
  chars and does not survive `.eval`. The count is hoisted to a local,
  which is what the same function already does with `local_stride`.
- `aspiration_delta`'s trailing comment needed re-padding after the
  code grew by `.search.`.

Everything else fit, which the pre-edit length sweep predicted: the
sites are short because the field names are long.

No hot-path cost. Both groups are inline struct fields behind the same
`Arc`, so the offset resolves at compile time and the allocation is
unchanged.

### Gates

Warning-free build; all seven bench node counts reproduce exactly; all
seven perft suites pass (20256/12/33/12/3/12/21). Because this stage
rewrites `State::new` and the derivation writers, the plan's suite gap
applies and `sittuyin` and `janggi` were run alongside the five.

The benches are endgame-only and cannot see most of what moved, so the
D11 eval pair was re-run as the eval-side gate — `-28 cp` and `9 cp`,
unchanged — plus a static eval per variant, all non-zero and stable.

## D13 — move the param structs out of `state.rs` · 0

`EvalParams` and `SearchParams` move verbatim from `state.rs` into
`parameters.rs`, the only file that writes them, and `prelude.rs` gains
them at the end of its existing `parameters::{...}` re-export block —
after `reduction_surface`, matching the block convention of functions
first, types last. `state.rs` sheds 77 lines and drops to 1237.

### Deviation from the plan text

The plan predicted `+1: one mod line in main.rs and one pub use in
prelude.rs`, which assumes a new module file. No new file was made. A
new file costs a `//!` header, a `mod` line, and a directory decision —
and the only honest home for it would be `representations/`, where a
derived-coefficient table does not belong. Putting the structs in
`parameters.rs` costs nothing but the one `pub use` and keeps each
coefficient's declaration in the same file as the code that computes it
and documents what it means.

The net is 0, not +1: 77 lines out of `state.rs`, 77 into
`parameters.rs`, and the `pub use` edit rewrites a line rather than
adding one.

### The one text change

`EvalParams`'s doc said the four excepted tables "stay on
[`StaticState`] beside this". `StaticState` is not re-exported through
prelude, so the intra-doc link would resolve to nothing from
`parameters.rs`; it becomes plain `` `StaticState` in `state.rs` ``, and
"beside this" becomes "flat on", which is now true. Both docs also drop
"`parameters.rs` derives" for "this file derives".

### Gates

Warning-free build; all seven bench node counts reproduce exactly; all
seven perft suites pass (20256/12/33/12/3/12/21). The stage moves the
`State::new` initializer targets, so the plan's suite gap applies and
`sittuyin` and `janggi` were run alongside the five.

No eval-side gate beyond the benches: the move is textual, the field
paths at every read and write site are byte-identical, and nothing in
`State::new` changed.

## D14 — the three `thread_local!`s into `State::scratch` · −13

`SEE_BUFFERS`, `PAWN_BUFFERS` and `PAWN_TABLE` become `see_moves`,
`see_scratch`, `pawn_rosters` and `pawn_table` on a new `Scratch` struct,
held as the single field `State::scratch`. The whole `thread_local!` block
goes, and with it the last one in the tree; the `cell::RefCell` import in
`prelude.rs` goes with it. The roster tuple gets a name, `PawnEntry`,
because a field declaration wants one where a `thread_local!` initializer
did not.

`Scratch` rather than four loose fields, because the four are not what the
position *is*: `State`'s own doc has to draw that line once instead of
four times, and every construction site collapses to one
`Scratch::default()`. `State::clone` keeps only `pawn_table` and takes the
rest from `Scratch::default()`; `State::reset` clears the pawn table,
which is where `ucinewgame` reaches it.

`see!`, `pawn_structure!` and `evaluate_position!` lose the `$info`
argument instead of gaining one, and the three debug call sites lose the
`SearchInfo::default()` they would otherwise have had to fabricate.
`headless.rs`'s `evaluate` takes `mut position` and binds
`let state = &mut position.state;` first, because `$state` expands inside
a loop and a fresh `&mut position.state` per expansion is a second borrow
across iterations.

### Borrowing it back out

`pawn_structure!` borrows in place. It makes no move, and `scratch` is a
different field from the `statics` and the piece lists its sweeps read, so
the roster and the cache borrow independently of everything around them.

`see!` cannot: `lva!` binds `let state: &State = $state`, a whole-struct
shared borrow, and the exchange loop calls `make_move!`, which wants the
whole struct mutably. So the two vectors are `mem::take`n at entry and put
back at exit — three words each way, against one allocation per scored
capture.

Only the two vectors, never the whole `Scratch`: `mem::take` fills the
hole with `Default::default()`, and `Scratch`'s `Default` allocates a
`PAWN_TABLE_ENTRIES` pawn table. Taking the struct built and dropped a
196 KB table per scored capture, which cost 30% of nps on standard and
grand before it was caught. Node counts never moved, so only the nps
comparison found it.

### The allocation is unchanged, the lifetime is not

Each of the four was already one per worker; they are still one per
worker, because a worker searches its own `State` clone. What changed is
that they die with the position that owns them instead of with the
thread, which is the point: the pawn table now answers `ucinewgame` and a
new `setoption Hash` like every other table a search owns, because there
is nothing left for it to outlive.

Node counts cannot move: the table is a transparent cache keyed on a full
128-bit fold of the piece lists with an exact compare, so a changed
hit-and-miss pattern changes which nodes pay for the sweep and never what
the sweep returns.

### Gates

Warning-free build with no reference to any of the three names left; all
seven bench node counts reproduce exactly (190760 / 1391464 / 369526 /
1009750 / 1432374 / 12027791 / 301548). `State::clone`, `from_statics`
and `reset` all change, so the `sittuyin`/`janggi` suite gap applies and
all seven perft suites were run: 20256 / 12 / 33 / 12 / 3 / 12 / 21.

The benches never build the debug `SearchInfo`, so both `headless.rs`
commands were run directly: `evaluate` reproduces all seven per-variant
static scores recorded in D12 (22 / 20 / 18 / 17 / 12 / 22 / 26), and
`see` was run on a one-recapture exchange (`e4d5`, 0) and a
three-attacker one (`f3e5`, -218), proving `lva!` refills the vectors
across iterations and the undo chain unwinds. `graphics.rs`'s `see` is
the same one-line change and is covered by the TUI smoke run still owed
from D7.

nps is not a gate here and was not treated as one: an A/B of the two
binaries back to back put standard at 2.94M vs 2.61M on one pair of runs
and 3.63M vs 3.50M on the next, with grand crossing the other way both
times. The spread between repeats of the same binary is wider than the
spread between binaries, so the honest reading is no measurable change.

## Deferred, not resolved in this ladder

- PST-residual / param-schema question — stays in plan 21.
- `game_phase` ratchet-vs-refresh divergence (`move_list.rs:2732-2741`
  ratchets with `cmp::max`, `util.rs:260-268` recomputes without it) —
  real bug, own `[SEMANTIC]` commit.
- Pawn table sized at a fixed `PAWN_TABLE_ENTRIES` regardless of
  `setoption Hash`. The `surviving ucinewgame` half of this item is gone:
  D14 gave the table the lifetime of the position that owns it. The sizing
  half remains, and the claim recorded here that fixing it moves node
  counts was wrong — the table is a transparent cache with an exact key
  compare, so sizing changes what a node costs and never what it scores.
  It is an ordinary change, not a `[SEMANTIC]` one.
- Endgame fixture `xiangqi / perpetual one cycle short` fails on
  `85b4637` and every commit this ladder has touched, expecting
  `score mate -2` and getting `score cp -950`. Pre-dates the debloat
  pass; the position is not at a no-move leaf, so it is a perpetual
  adjudication or search-horizon question, not a refactor artefact. Own
  `[SEMANTIC]` investigation, outside this ladder.
