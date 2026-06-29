<picture>
  <source media="(prefers-color-scheme: light)" srcset="/.github/meta/dark.png">
  <source media="(prefers-color-scheme: dark)" srcset="/.github/meta/light.png">
  <img alt="AnekaMacam">
</picture>

<pre>
[ ABOUT ]

A chess engine that can (hopefully) play most chess variants. It speaks
UCI on the surface, but under the hood it is built on a stack of custom
notations — a move notation, a modified move-pattern notation, a drop
notation, and an expandable, fully data-driven variant configuration
format. Define a board, a set of pieces and their movement, and a
handful of rule flags, and the engine will play it.

Written in Rust as a Cargo workspace. "Aneka macam" is Indonesian for
"all sorts" — which is the point: all sorts of chess.

[ FEATURES ]

Variant-agnostic core  --> board sizes, piece sets, drop rules, and
                           royal-piece definitions are all configurable;
                           no FIDE-specific assumptions baked in.
Data-driven variants   --> each variant is a plain .conf file; default
                           variants are embedded into the binary.
Custom notations       --> CFEN / CKN / CPMN / CDN (see below).
Search                 --> parallel search (rayon), transposition table,
                           move ordering, and per-variant tunable
                           pruning parameters.
Evaluation             --> piece-square tables and parameters derived
                           per-variant (avg piece value, board size,
                           game phase).
Protocol translation   --> dictionaries convert CFEN to/from target
                           protocol FENs (UCI, USI-style, etc.).
TUI & tooling          --> a ratatui/crossterm interface, a debug
                           console, logging, and perft test suites.

[ NOTATIONS ]

┌──────────┬─────────────────────────────────┬────────────────────────────────────┐
│ Notation │ Expands To                      │ Purpose                            │
├──────────┼─────────────────────────────────┼────────────────────────────────────┤
│ CFEN     │ Cheesy Forsyth–Edwards Notation │ Board position &amp; full game state   │
│ CKN      │ Cheesy King Notation            │ Move patterns (Betza-style atoms)  │
│ CPMN     │ Cheesy Pattern Match Notation   │ Drop legality &amp; stand-off matching │
│ CDN      │ Cheesy Drop Notation            │ Per-piece drop rules               │
└──────────┴─────────────────────────────────┴────────────────────────────────────┘

CFEN extends ordinary FEN with board dimensions, pieces-in-hand, and the
extra state that variants need (e.g. setup phase, drops). Move patterns
are written in CKN, a Betza-like atom language. For example, the FIDE
pawn's full move set is:

    Pp:mnW|im&lt;nW-pnW&gt;|tcnF

Drops and stand-offs are matched with CPMN; per-piece drop rules use CDN.

[ VARIANT CONFIGURATION ]

A variant lives in a single configs/&lt;name&gt;.conf file. It declares, in
labelled sections, the title, the starting CFEN, the enabled rules, and
the pieces with their CKN move patterns. Supported rule flags include:

    castling            promote to captured   setup phase
    en passant          stalemate loss        stand-offs
    promotions          halfmove clock        repetition limit
    drops               forbidden zones

Each rule, when enabled, requires its matching section (e.g. castling
geometry, promotion zones, drop rules) and a correctly-formatted CFEN —
the parser validates this and errors out otherwise. See
<a href="configs/example.conf">configs/example.conf</a> for a fully commented
reference of every section. Protocol translation is configured per
variant in res/dicts/&lt;name&gt;.dict; evaluation parameters live in
res/param/.

[ SUPPORTED VARIANTS ]

Variants bundled in configs/ (the names double as their config files):

┌───────────────┬───────────────────────────────────┐
│ Config        │ Variant                           │
├───────────────┼───────────────────────────────────┤
│ fide          │ Standard (FIDE) Chess             │
│ capablanca    │ Capablanca Chess (10×8)           │
│ grand         │ Grand Chess (10×10)               │
│ los-alamos    │ Los Alamos Chess (6×6)            │
│ berolina      │ Berolina Chess (diagonal pawns)   │
│ crazyhouse    │ Crazyhouse (captured pieces drop) │
│ shatranj      │ Shatranj (medieval Persian chess) │
│ xiangqi       │ Xiangqi (Chinese chess)           │
│ mini-xiangqi  │ Mini Xiangqi (7×7)                │
│ janggi        │ Janggi (Korean chess)             │
│ shogi         │ Shogi (Japanese chess)            │
│ mini-shogi    │ Mini Shogi (5×5)                  │
│ makruk        │ Makruk (Thai chess)               │
│ sittuyin      │ Sittuyin (Burmese chess)          │
│ ouk-chaktrang │ Ouk Chaktrang (Cambodian chess)   │
│ tjatoer       │ Tjatoer (Javanese chess)          │
└───────────────┴───────────────────────────────────┘

example.conf is the documented template you copy when authoring a new
variant. res/perft/ holds perft suites used to validate move generation.

[ USAGE ]

Build with Cargo (release is strongly recommended — the dev profile is
much slower):

    cargo build --release

The workspace produces a binary named anekamacam. It takes a subcommand:

    anekamacam uci      # UCI protocol mode (this is also the default)
    anekamacam debug    # interactive debug console

With no subcommand it starts in UCI mode. Default variant configs and
parameters are embedded in the binary, so it runs standalone.

[ PROJECT LAYOUT ]

    src/
    ├── main.rs                  # Entry point + subcommand dispatch
    ├── prelude.rs               # Project-wide re-exports
    ├── game/
    │   ├── representations/     # board, piece, move, drop, pattern, vector, state
    │   ├── moves/               # move & drop generation, pattern parse/match
    │   ├── search/              # parallel, transposition, ordering, parameters
    │   ├── position/            # evaluation, hash, search
    │   └── util.rs
    ├── io/
    │   ├── board_io / piece_io / game_io / move_io
    │   ├── logger.rs
    │   └── protocols/           # uci, translation
    └── debug/console.rs         # debug console
    configs/                     # per-variant .conf files
    res/
    ├── dicts/                   # protocol translation dictionaries
    ├── param/                   # evaluation parameter sets
    └── perft/                   # perft test suites
    util/                        # benchmarking & tuning notes

[ LICENSE ]

This repository is licensed under the <a href="LICENSE">GNU General Public License v3.0</a>.

With this license, you have the freedom to:
- Use, study, and run the software for any purpose
- Modify the software and adapt it to your needs
- Distribute copies of the original and your modified versions

However, if you distribute this software (modified or unmodified), you
must:
- Provide the source code under the same GPL-3.0 license
- Include copyright notices and license information
- Make your modifications available under GPL-3.0
</pre>
