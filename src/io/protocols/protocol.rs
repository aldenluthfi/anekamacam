//! protocol.rs
//!
//! The shared engine loop behind every text protocol the engine speaks.
//!
//! UCI, USI, and UCCI differ only in a handshake word, a few dialect `go`
//! clauses, and the notation their dictionaries emit; everything else — the
//! position, the variant list, the hash tables, the search threads, and the
//! ponder plumbing — is identical. That common machinery lives here as the
//! `Session` and the `execute_common` dispatcher. A protocol is a tiny
//! `Protocol` implementor that intercepts only the lines that behave
//! differently and defers the rest.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                            SESSION TIMING CONSTANTS
\*----------------------------------------------------------------------------*/

/// Session constants.
///
/// Transmission overhead: how much of a timed search's budget is given up
/// before it starts, to cover the trip out to the GUI and back. A move
/// decided in time but delivered late is a loss on the clock, so the engine
/// spends less than it was given rather than exactly what it was given.
///
/// - TIME_OVERHEAD_MS : 50, the default, what a local pipe costs
/// - MAX_OVERHEAD_MS  : 1000, the ceiling a GUI may raise it to
///
/// The ceiling exists because the option is set from outside: a full second
/// covers any real link, and anything past it would be a mistyped number
/// silently costing the engine its whole clock.
const TIME_OVERHEAD_MS: u128 = 50;
const MAX_OVERHEAD_MS: u128 = 1000;

/*----------------------------------------------------------------------------*\
                               DIALECT INTERFACE
\*----------------------------------------------------------------------------*/

/// Protocol
///
/// One text protocol the engine speaks. The trait is deliberately two methods
/// wide: a dialect that had to declare its handshake, its option names, and
/// its command table would be a table to keep in step with the code, whereas
/// a dialect that only says its own name has nothing left to drift.
///
/// - name    : one word, from which the handshake and options derive
/// - execute : only the lines that genuinely behave differently
///
/// Everything else — the position, the variant list, the tables, the search
/// threads, the ponder plumbing — is shared, so adding a dialect is writing
/// a marker type and the clauses its `go` spells differently.
pub trait Protocol {
    /// name
    ///
    /// The protocol's identifier, and the only string a dialect declares.
    /// Five separate things are spelled out of this one word, which is what
    /// keeps a dialect from half-renaming itself.
    ///
    /// - `uci`           : the handshake command a GUI sends
    /// - `uciok`         : the reply that ends the handshake
    /// - `UCI_Variant`   : the combo option naming the variant
    /// - `= uci fen =`   : the dictionary section for board states
    /// - `= uci moves =` : the dictionary section for move text
    ///
    /// Return:
    /// &str -> the protocol name, e.g. "uci"
    fn name(&self) -> &str;

    /// execute
    ///
    /// Handles one input line the universal dispatcher left unclaimed.
    /// `execute_common` runs first and serves every protocol-independent
    /// command, so this only ever sees the lines that genuinely differ
    /// between dialects.
    ///
    /// - new-game : the same reset under three different spellings
    /// - `go`     : the clause each dialect words its own way
    ///
    /// An unrecognized line is ignored rather than reported: the protocols
    /// require a GUI to be able to send a word this engine has never heard
    /// of, and a complaint on stdout would be read as a reply to it.
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the whitespace-split input line
    ///
    /// Return:
    /// bool                    -> true when the session should terminate
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool;
}

/// PROTOCOLS
///
/// Every dialect the engine speaks, in the order the handshake lists them.
/// The active one is chosen at runtime — by the handshake word a GUI sends
/// or by the `Protocol` option — never by a launch flag, so one running
/// process serves a UCI GUI, a shogi GUI, and a xiangqi GUI without being
/// restarted, and a session can change its mind mid-game.
///
/// The markers are zero-sized types, so the whole table is three `'static`
/// trait objects and no allocation.
const PROTOCOLS: [&dyn Protocol; 3] = [&Uci, &Usi, &Ucci];

/// find_protocol
///
/// Resolves a name to its dialect by asking each marker what it is called.
/// The handshake word, the `Protocol` option's value, and the dictionary
/// section key are all the same string, so there is no second table of
/// tokens that could disagree with the trait.
///
/// Params:
/// - name: &str                  -> a protocol name, e.g. "usi"
///
/// Return:
/// Option<&'static dyn Protocol> -> the dialect, or None if unknown
pub fn find_protocol(name: &str) -> Option<&'static dyn Protocol> {
    PROTOCOLS.into_iter().find(|protocol| protocol.name() == name)
}

/// list_variants
///
/// Discovers which variants can be served over one protocol by walking the
/// embedded dictionaries. Nothing declares the list: a variant appears in the
/// combo option because its own files say it can be spoken, so shipping a new
/// variant is shipping two files and nothing else.
///
/// - `<name>.dict`   : must be embedded, and must not be the example
/// - `<name>.conf`   : must be embedded beside it, or the rules are gone
/// - `= protocols =` : must name this protocol among its lines
///
/// All three are required together. A dictionary without a config would offer
/// a variant the engine cannot set up, and a config without a dictionary
/// would offer one whose moves the GUI could not spell.
///
/// Params:
/// - protocol: &str    -> the protocol name to match in `protocols`
///
/// Return:
/// Vec<String>         -> sorted names of variants serving `protocol`
fn list_variants(protocol: &str) -> Vec<String> {
    let mut variants = Vec::new();

    for dict_file in EMBEDDED_DICTS.files() {
        let path = dict_file.path();

        let Some(stem) = path.file_stem().and_then(|s| s.to_str()) else {
            continue;
        };

        if path.extension().and_then(|e| e.to_str()) != Some("dict")
        || stem == "example" {
            continue;
        }

        if EMBEDDED_CONFIGS
            .get_file(format!("{}.conf", stem))
            .is_none()
        {
            continue;
        }

        let Some(content) = dict_file.contents_utf8() else {
            continue;
        };

        let sections = split_sections(content);

        let has_protocol = sections
            .get("protocols")
            .map(|lines| lines.iter().any(|l| l.trim() == protocol))
            .unwrap_or(false);

        if has_protocol {
            variants.push(stem.to_string());
        }
    }

    variants.sort();
    variants
}

/*----------------------------------------------------------------------------*\
                                 SEARCH OUTPUT
\*----------------------------------------------------------------------------*/

/// print_bestmove
///
/// Emits the `bestmove` line that ends a search. A GUI waits on this line and
/// on nothing else, so the one thing this must never do is fail to produce
/// one — every path below ends in a printed move or a printed `(none)`.
///
/// - terminal position : `(none)`, whatever the result says
/// - null best move    : any legal move, or `(none)` if there are none
/// - real best move    : the move, plus the ponder move if there was one
///
/// A terminal position is answered from the board rather than the result,
/// because a search that was never run leaves a stale result behind. A null
/// move at a live position means the search was cut off before it finished
/// its first depth, or died; either way a legal move beats no answer, and a
/// fallback move is not worth pondering on, so no ponder is offered with it.
///
/// Params:
/// - result: &SearchResult       -> finished search outcome
/// - state : &mut State          -> position for move formatting
/// - dict  : Option<&Translator> -> translator for printed move names
fn print_bestmove(
    result: &SearchResult,
    state: &mut State,
    dict: Option<&Translator>,
) {
    if is_terminal!(state) {
        emit(EngineEvent::BestMove {
            best: "(none)".to_string(),
            ponder: None,
        });
        return;
    }

    let fallback = result.best_move == null_move();

    let best_move = if fallback {
        legal_moves!(state).first().unwrap_or(&null_move()).clone()
    } else {
        result.best_move.clone()
    };

    let best = format_move(&best_move, state, dict);

    let ponder = if !fallback && result.ponder_move != null_move() {
        Some(format_move(&result.ponder_move, state, dict))
    } else {
        None
    };

    emit(EngineEvent::BestMove { best, ponder });
}

/*----------------------------------------------------------------------------*\
                                 SESSION STATE
\*----------------------------------------------------------------------------*/

/// SearchHandle
///
/// A running search thread, and everything needed to launch it again. The
/// second half is what makes `ponderhit` possible: a ponder search has to be
/// stopped and restarted as a timed one, and the limits it was born with are
/// gone by then unless they were kept here.
///
/// - handle              : the thread, joined on stop, abort, or quit
/// - is_ponder           : whether its bestmove is being withheld
/// - search_depth        : the depth limit to restart under
/// - search_nodes        : the node limit to restart under
/// - ponderhit_budget_ns : the clock the restart gets, unspent until the
///                         hit
struct SearchHandle {
    handle: JoinHandle<SearchResult>,                                           /* the running search thread          */
    is_ponder: bool,                                                            /* true if launched as a ponder       */
    search_depth: usize,                                                        /* depth limit for restart            */
    search_nodes: u128,                                                         /* node limit for restart             */
    ponderhit_budget_ns: u128,                                                  /* time budget after ponderhit        */
}

/// SearchLimits
///
/// Launch parameters for one search thread, bundled so `spawn_search` takes
/// one argument no matter which of the two callers built it.
///
/// - deadline            : absolute, in nanoseconds since engine start
/// - ponderhit_budget_ns : a duration, not yet anchored to any moment
///
/// The two clocks are different kinds on purpose. A timed search knows when
/// it must stop, so it carries an instant; a ponder search does not know when
/// its clock will start, so it carries a length and is anchored later, at the
/// moment the hit actually arrives.
struct SearchLimits {
    is_ponder: bool,                                                            /* launch as a ponder search          */
    depth: usize,                                                               /* search depth limit                 */
    nodes: u128,                                                                /* search node limit                  */
    deadline: u128,                                                             /* deadline, ns since launch          */
    ponderhit_budget_ns: u128,                                                  /* time budget after ponderhit        */
}

/// Session
///
/// Everything one conversation with a GUI owns, shared by every dialect. The
/// dialect markers hold nothing at all, so this is the whole of the engine's
/// mutable protocol state and there is exactly one of it per process.
///
/// - protocol, translator  : which dialect is spoken, and its notation
/// - state, position_valid : the board, and whether it can be trusted
/// - variants, variant     : what this dialect serves, and what is loaded
/// - threads, hash_mb      : the options a GUI may change mid-session
/// - overhead_ms           : the clock given up before a timed search
/// - ttable, qtable        : shared with the workers, rebuilt on Hash
/// - active                : the search in flight, if a go is outstanding
///
/// `position_valid` is false only between a failed `position` and the next
/// clean load. A half-replayed position is worse than no position, because
/// it is a real board that is not the one the GUI meant, so `go` refuses it
/// with an immediate `bestmove (none)` instead of searching a plausible
/// wrong board and answering with conviction.
pub struct Session {
    protocol: String,                                                           /* the protocol being spoken          */
    state: State,                                                               /* the engine's working position      */
    variants: Vec<String>,                                                      /* discovered protocol-capable list   */
    variant: String,                                                            /* the active variant name            */
    translator: Option<Translator>,                                             /* protocol notation translator       */
    max_threads: usize,                                                         /* hardware thread ceiling            */
    threads: usize,                                                             /* configured worker threads          */
    hash_mb: usize,                                                             /* hash budget in megabytes           */
    overhead_ms: u128,                                                          /* move overhead in milliseconds      */
    ttable: Arc<TTable>,                                                        /* shared main transposition table    */
    qtable: Arc<QTable>,                                                        /* shared quiescence table            */
    active: Option<SearchHandle>,                                               /* in-flight search, if any           */
    position_valid: bool,                                                       /* false after a failed position cmd  */
}

impl Session {
    /// Session::new
    ///
    /// Boots a session for one protocol, ready to answer before a GUI has
    /// said anything beyond the handshake.
    ///
    /// - variant    : standard if this dialect serves it, else the first
    ///                one found
    /// - state      : that variant's config, reset to its start position
    /// - translator : the dialect's section of that variant's dictionary
    /// - tables     : allocated at the default budget, two thirds to the
    ///                main one
    ///
    /// Standard is preferred rather than assumed: a shogi-only or xiangqi-only
    /// dialect has no standard to fall back to, and the first variant it does
    /// serve is a better default than a variant it cannot speak.
    ///
    /// Params:
    /// - protocol: &str -> the protocol name to serve
    ///
    /// Return:
    /// Self             -> a session ready to accept commands
    fn new(protocol: &str) -> Self {
        let variants = list_variants(protocol);
        let default_variant = variants
            .iter()
            .find(|v| v.as_str() == "standard")
            .or_else(|| variants.first())
            .cloned()
            .unwrap_or_else(|| "standard".to_string());

        let config_path = format!("{}.conf", default_variant);
        let mut state = parse_config_file(&config_path);
        let startpos = state.statics.startpos.clone();
        state.reset();
        parse_fen(&mut state, &startpos, None)
            .unwrap_or_else(|error| panic!("{}", error));
        refresh_eval_state(&mut state);

        let translator = Translator::find(&default_variant, protocol);
        let max_threads = thread::available_parallelism()
            .map(|n| n.get())
            .unwrap_or(1);

        let (ttable, qtable) = spawn_hash_tables(HASH_DEFAULT_MB);

        Session {
            protocol: protocol.to_string(),
            state,
            variants,
            variant: default_variant,
            translator,
            max_threads,
            threads: 1,
            hash_mb: HASH_DEFAULT_MB,
            overhead_ms: TIME_OVERHEAD_MS,
            ttable,
            qtable,
            active: None,
            position_valid: true,
        }
    }

    /// Session::variant_option
    ///
    /// The name of this dialect's variant combo option, spelled out of the
    /// protocol name rather than stored, so the option the handshake lists
    /// and the option `setoption` answers to cannot drift apart.
    ///
    /// - `uci`  : `UCI_Variant`
    /// - `usi`  : `USI_Variant`
    /// - `ucci` : `UCCI_Variant`
    ///
    /// Return:
    /// String -> the `<NAME>_Variant` option name
    fn variant_option(&self) -> String {
        format!("{}_Variant", self.protocol.to_uppercase())
    }

    /// Session::set_protocol
    ///
    /// Switches the session to another dialect mid-run, as the handshake word
    /// or the `Protocol` option asks.
    ///
    /// - stop        : any search in flight, before anything else moves
    /// - rediscover  : which variants the new dialect can serve
    /// - reseat      : onto its default variant, only if the current one
    ///                 is gone
    /// - retranslate : the notation, always, since the dialect changed
    ///
    /// A variant both dialects serve is kept along with the position on it,
    /// which is what lets a GUI change its mind about notation mid-game. A
    /// variant the new dialect cannot speak is not carried over at all: the
    /// config is reloaded from scratch and the board reset, so no piece list
    /// or dictionary from the old variant survives the switch.
    ///
    /// Params:
    /// - protocol: &str -> the dialect name to switch to
    fn set_protocol(&mut self, protocol: &str) {
        abort_search(self);
        self.protocol = protocol.to_string();
        self.variants = list_variants(protocol);

        if !self.variants.contains(&self.variant) {
            self.variant = self.variants
                .iter()
                .find(|variant| variant.as_str() == "standard")
                .or_else(|| self.variants.first())
                .cloned()
                .unwrap_or_else(|| "standard".to_string());

            let config_path = format!("{}.conf", self.variant);
            self.state = parse_config_file(&config_path);
            self.state.scratch.pawn_table =
                PTable::with_hash_mb(self.hash_mb);
            let startpos = self.state.statics.startpos.clone();
            self.state.reset();
            parse_fen(&mut self.state, &startpos, None)
                .unwrap_or_else(|error| panic!("{}", error));
            refresh_eval_state(&mut self.state);
            self.position_valid = true;
        }

        self.translator = Translator::find(&self.variant, protocol);
    }
}

/// spawn_hash_tables
///
/// Splits one megabyte budget into the two shared tables. A GUI sets a single
/// `Hash` figure and expects that to be the engine's whole appetite, so the
/// split happens here rather than being two options to keep in agreement.
///
/// - main search : two thirds, the deeper tree and the longer-lived
///                 entries
/// - quiescence  : one third, a shallower tree that still wants its own
///                 table
///
/// Each side is floored at one megabyte, so even a one-megabyte budget yields
/// two usable tables instead of one empty one. Rebuilding is how both `Hash`
/// and `Clear Hash` are served: a fresh table of the right size is a cleared
/// table, so one path covers both commands.
///
/// Params:
/// - hash_mb: usize          -> total table budget in megabytes
///
/// Return:
/// (Arc<TTable>, Arc<QTable>) -> the freshly sized tables
fn spawn_hash_tables(
    hash_mb: usize,
) -> (Arc<TTable>, Arc<QTable>) {
    (
        Arc::new(TTable::with_mb((hash_mb * 2 / 3).max(1))),
        Arc::new(QTable::with_mb((hash_mb / 3).max(1))),
    )
}

/*----------------------------------------------------------------------------*\
                                COMMAND HANDLERS
\*----------------------------------------------------------------------------*/

/// Session command handlers
///
/// Each applies one command's side effects to the session. They share one
/// discipline: a command that touches the position or the tables aborts any
/// search first, because a worker holds a clone of the state and a copy of
/// the table handles, and changing either underneath it makes its answer
/// about a board that no longer exists.
///
/// - handle_position  : rebuild the board from startpos or FEN, replay
///                      moves
/// - abort_search     : interrupt and join, returning what the search had
/// - stop_search      : abort, and release a ponder search's withheld move
/// - handle_ponderhit : rejoin the ponder search, relaunch it on the clock
/// - handle_setoption : apply Protocol, Variant, Threads, Hash, or
///                      Overhead
///
/// `start_search` and `spawn_search` interleave below and keep their own
/// docs, being the two halves of what `go` does.
///
/// handle_position
///
///   The FEN keyword is skipped rather than matched, so `fen` and `sfen`
///   are both accepted without either dialect being named here. The board is
///   built in a fork and only installed once the whole line has succeeded, so
///   a move that fails to parse halfway through a replay leaves the previous
///   position standing rather than a partial one.
///
///   Params:
///
///     session: &mut Session
///     session whose position is replaced, on success
///
///     tokens: &[&str]
///     the split command line, a `startpos` or a FEN, optionally followed by
///     `moves` and the plies to replay onto it
///
/// abort_search
///
///   Raises the interrupt, joins, and lowers it again, so the flag is never
///   left set for the next search to trip over. A panicked worker is absorbed
///   into None rather than propagated: a crashed search should cost a move,
///   not the session.
///
///   Params:
///
///     session: &mut Session
///     session whose active search is interrupted and joined
///
///   Return:
///
///     Option<(SearchResult, bool)>
///     what the search had reached and whether it was pondering, or None if
///     no search was running or the worker died
///
/// stop_search
///
///   A pondering worker withholds its `bestmove`, since nobody asked it for
///   one yet, so `stop` is the moment that move is finally printed. A timed
///   search has already printed its own, and printing again here would give
///   the GUI two answers to one question.
///
///   Params:
///
///     session: &mut Session
///     session whose search is aborted
///
/// handle_ponderhit
///
///   The ponder guess was right, so the work stands but the terms change: the
///   thread is joined and relaunched on the same position with the depth and
///   node limits it was born with, and with its budget anchored from the
///   moment of the hit rather than the moment of the `go`.
///
///   Params:
///
///     session: &mut Session
///     session whose ponder search becomes a timed one
///
/// handle_setoption
///
///   Names may contain spaces, so the name is everything between `name` and
///   `value` rather than one token. An option that is unknown, or whose value
///   does not parse, changes nothing at all instead of falling back to a
///   default the GUI did not ask for.
///
///   Params:
///
///     session: &mut Session
///     session receiving the option change
///
///     tokens: &[&str]
///     the split `setoption` line
fn handle_position(session: &mut Session, tokens: &[&str]) {
    abort_search(session);

    let mut scratch = session.state.fork();
    let dict = session.translator.clone();
    let mut index = 1;

    if tokens.get(index).copied() == Some("startpos") {
        index += 1;
    } else if matches!(
        tokens.get(index).copied(),
        Some(keyword) if keyword != "moves"
    ) {
        index += 1;
        let fen_end = tokens[index..]
            .iter()
            .position(|&token| token == "moves")
            .map(|position| position + index)
            .unwrap_or(tokens.len());
        let fen = tokens[index..fen_end].join(" ");

        if fen.is_empty() {
            log_2!("position: empty FEN");
            session.position_valid = false;
            return;
        }

        scratch.reset();
        if let Err(error) = parse_fen(
            &mut scratch, &fen, dict.as_ref()
        ) {
            log_2!("position: invalid FEN: {}", error);
            session.position_valid = false;
            return;
        }
        refresh_eval_state(&mut scratch);
        index = fen_end;
    } else {
        log_2!("position: missing startpos or FEN");
        session.position_valid = false;
        return;
    }

    if index < tokens.len() {
        if tokens[index] != "moves" {
            log_2!("position: unexpected token \"{}\"", tokens[index]);
            session.position_valid = false;
            return;
        }

        let replayed = &mut scratch;

        for (ply, &token) in tokens[index + 1..].iter().enumerate() {
            let Some(mv) = parse_move(token, replayed, dict.as_ref())
            else {
                log_2!(
                    "position: ply {} \"{}\" parse failed", ply + 1, token
                );
                session.position_valid = false;
                return;
            };

            if !make_move!(replayed, mv) {
                log_2!("position: ply {} \"{}\" illegal", ply + 1, token);
                session.position_valid = false;
                return;
            }
        }
    }

    session.state = scratch;
    session.position_valid = true;
}

/// start_search
///
/// Parses a standard `go` line and launches the search thread. Dialect
/// clauses are renamed to standard tokens by the protocol before they reach
/// here, so this parse — and the engine core behind it — never sees a
/// dialect word.
///
/// Three kinds of `go` produce three kinds of budget:
///
/// - `movetime m` : spend m less overhead, and at least a millisecond
///                  of it
/// - a clock      : (remaining − overhead) / movestogo, plus the
///                  increment
/// - neither      : no deadline at all, as ponder and infinite both want
///
/// A clock search assumes 20 moves left when the GUI does not say, and never
/// budgets past the clock it actually has, since a share of the remaining
/// time plus an increment that has not been earned yet can otherwise exceed
/// what is left. The deadline is anchored against the moment the `go` was
/// read rather than the moment the thread starts, so thread startup is spent
/// out of the budget instead of extending it.
///
/// Params:
/// - session: &mut Session -> session the `go` runs in
/// - tokens : &[&str]      -> normalized `go` limit tokens
pub fn start_search(session: &mut Session, tokens: &[&str]) {
    abort_search(session);

    if !session.position_valid {
        log_2!("go: position invalid, refusing search");
        emit(EngineEvent::BestMove {
            best: "(none)".to_string(),
            ponder: None,
        });
        return;
    }

    let go_time = ENGINE_START.elapsed().as_nanos();
    let is_ponder = tokens.contains(&"ponder");

    let mut depth = 0usize;
    let mut nodes = 0u128;
    let mut movetime_ms = 0u128;
    let mut wtime_ms = 0u128;
    let mut btime_ms = 0u128;
    let mut winc_ms = 0u128;
    let mut binc_ms = 0u128;
    let mut movestogo = 0usize;
    let mut infinite = false;

    let clamped = |token: Option<&&str>| -> u128 {
        token
            .and_then(|s| s.parse::<i64>().ok())
            .map(|v| v.max(0) as u128)
            .unwrap_or(0)
    };

    let mut index = 1;
    while index < tokens.len() {
        match tokens[index] {
            "depth" => {
                depth = clamped(tokens.get(index + 1)) as usize;
                index += 2;
            }
            "nodes" => {
                nodes = clamped(tokens.get(index + 1));
                index += 2;
            }
            "movetime" => {
                movetime_ms = clamped(tokens.get(index + 1));
                index += 2;
            }
            "wtime" => {
                wtime_ms = clamped(tokens.get(index + 1)).max(1);               /* present clock is never 0: a spent  */
                index += 2;                                                     /* or negative clock means move now,  */
            }                                                                   /* not search without any time limit  */
            "btime" => {
                btime_ms = clamped(tokens.get(index + 1)).max(1);
                index += 2;
            }
            "winc" => {
                winc_ms = clamped(tokens.get(index + 1));
                index += 2;
            }
            "binc" => {
                binc_ms = clamped(tokens.get(index + 1));
                index += 2;
            }
            "movestogo" => {
                movestogo = clamped(tokens.get(index + 1)) as usize;
                index += 2;
            }
            "infinite" => {
                infinite = true;
                index += 1;
            }
            _ => { index += 1; }
        }
    }

    let (time_ms, inc_ms) = if session.state.playing == WHITE {
        (wtime_ms, winc_ms)
    } else {
        (btime_ms, binc_ms)
    };

    let budget_ns = if movetime_ms > 0 {
        movetime_ms.saturating_sub(session.overhead_ms).max(1) * 1_000_000
    } else if time_ms == 0 {
        0
    } else {
        let remaining =
            time_ms.saturating_sub(session.overhead_ms).max(1);
        let moves = if movestogo > 0 { movestogo as u128 } else { 20 };

        (remaining / moves).saturating_add(inc_ms)
            .clamp(1, remaining) * 1_000_000
    };

    let deadline = if infinite || budget_ns == 0 {
        0
    } else {
        go_time + budget_ns
    };

    let search_depth = if depth > 0 { depth } else { MAX_DEPTH };

    spawn_search(session, SearchLimits {
        is_ponder,
        depth: search_depth,
        nodes,
        deadline,
        ponderhit_budget_ns: budget_ns,
    });
}

/// spawn_search
///
/// Launches the search thread for the given limits and records it as the
/// session's active handle, wiring in the shared tables and the interrupt
/// plumbing. The position is cloned rather than borrowed, so the session
/// stays answerable to `stop` and `isready` while the search runs.
///
/// A ponder search is launched with its limits deliberately stripped:
///
/// - depth    : the maximum, not what the `go` asked for
/// - nodes    : unlimited
/// - deadline : none
/// - bestmove : withheld, printed only if a stop turns up first
///
/// Pondering happens on the opponent's clock, so there is nothing to spend
/// and no reason to stop early; the real limits are kept on the handle and
/// applied when the hit arrives. The worker is wrapped so a panic inside the
/// search yields an empty result instead of unwinding out of the thread,
/// which keeps the join in `abort_search` from becoming the way a bug in
/// search takes the whole session down.
///
/// Params:
/// - session: &mut Session -> the session being mutated
/// - limits : SearchLimits -> launch parameters for the thread
fn spawn_search(session: &mut Session, limits: SearchLimits) {
    let state_clone = session.state.clone();
    let tt_clone = Arc::clone(&session.ttable);
    let qt_clone = Arc::clone(&session.qtable);
    let dict_clone = session.translator.clone();
    let tc = session.threads;
    let is_ponder = limits.is_ponder;

    let thread_depth = if is_ponder { MAX_DEPTH } else { limits.depth };
    let deadline = if is_ponder { 0 } else { limits.deadline };
    let set_nodes = if is_ponder { 0 } else { limits.nodes };

    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let handle = thread::Builder::new()
        .name(format!("search:{}", exe_tag()))
        .stack_size(64 * 1024 * 1024)
        .spawn(move || {
            let mut s = state_clone;
            let mut info = SearchInfo {
                set_depth: thread_depth,
                set_nodes,
                deadline,
                ..Default::default()
            };
            let table = Arc::clone(&tt_clone);
            let qtable = Arc::clone(&qt_clone);
            let result = catch_unwind(AssertUnwindSafe(|| {
                search_position(
                    &mut s, table, qtable,
                    &mut info, tc, dict_clone.as_ref(),
                )
            }))
            .unwrap_or_else(|_| SearchResult {
                best_score: 0,
                best_move: null_move(),
                ponder_move: null_move(),
                completed_depth: 0,
                total_nodes: 0,
                total_elapsed: 0,
            });

            if !is_ponder {
                print_bestmove(&result, &mut s, dict_clone.as_ref());
            }

            log_table_stats(&tt_clone, &qt_clone);

            result
        }).expect("failed to spawn search thread");

    session.active = Some(SearchHandle {
        handle,
        is_ponder,
        search_depth: limits.depth,
        search_nodes: limits.nodes,
        ponderhit_budget_ns: limits.ponderhit_budget_ns,
    });
}

fn abort_search(session: &mut Session) -> Option<(SearchResult, bool)> {
    let sh = session.active.take()?;

    SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
    let joined = sh.handle.join();
    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    match joined {
        Ok(result) => Some((result, sh.is_ponder)),
        Err(_) => {
            log_1!("search thread panicked during join");
            None
        }
    }
}

fn stop_search(session: &mut Session) {
    if let Some((result, was_ponder)) = abort_search(session)
        && was_ponder
    {
        print_bestmove(
            &result,
            &mut session.state,
            session.translator.as_ref(),
        );
    }
}

fn handle_ponderhit(session: &mut Session) {
    if !session.active.as_ref().is_some_and(|sh| sh.is_ponder) {
        return;
    }

    let hit_time = ENGINE_START.elapsed().as_nanos();

    let Some(sh) = session.active.take() else {
        return;
    };

    SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
    let _ = sh.handle.join();
    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);

    let deadline = if sh.ponderhit_budget_ns == 0 {
        0
    } else {
        hit_time + sh.ponderhit_budget_ns
    };

    spawn_search(session, SearchLimits {
        is_ponder: false,
        depth: sh.search_depth,
        nodes: sh.search_nodes,
        deadline,
        ponderhit_budget_ns: 0,
    });
}

fn handle_setoption(session: &mut Session, tokens: &[&str]) {
    let Some(name_pos) = tokens.iter().position(|&t| t == "name") else {
        return;
    };
    let value_pos = tokens.iter().position(|&t| t == "value");

    let name = tokens[(name_pos + 1)..value_pos.unwrap_or(tokens.len())]
        .join(" ");
    let value = value_pos.map(|b| tokens[(b + 1)..].join(" "));

    let variant = session.variant_option();

    match (name.as_str(), value) {
        (OPT_PROTOCOL, Some(v)) if find_protocol(&v).is_some() => {
            session.set_protocol(&v);
        }
        (n, Some(v)) if n == variant && session.variants.contains(&v) => {
            abort_search(session);
            let conf = format!("{}.conf", v);
            session.state = parse_config_file(&conf);
            session.state.scratch.pawn_table =
                PTable::with_hash_mb(session.hash_mb);

            let position = session.state.statics.startpos.clone();

            session.state.reset();
            parse_fen(&mut session.state, &position, None)
                .unwrap_or_else(|error| panic!("{}", error));

            refresh_eval_state(&mut session.state);

            session.translator = Translator::find(&v, &session.protocol);
            session.variant = v;
            session.position_valid = true;
        }
        (OPT_THREADS, Some(v)) => {
            if let Ok(n) = v.parse::<usize>() {
                session.threads = n.clamp(1, session.max_threads);
            }
        }
        (OPT_HASH, Some(v)) => {
            if let Ok(mb) = v.parse::<usize>() {
                session.hash_mb = mb.clamp(1, HASH_MAX_MB);
                (session.ttable, session.qtable) =
                    spawn_hash_tables(session.hash_mb);
                session.state.scratch.pawn_table =
                    PTable::with_hash_mb(session.hash_mb);
            }
        }
        (OPT_CLEAR_HASH, None) => {
            (session.ttable, session.qtable) =
                spawn_hash_tables(session.hash_mb);
            session.state.scratch.pawn_table =
                PTable::with_hash_mb(session.hash_mb);
        }
        (OPT_MOVE_OVERHEAD, Some(v)) => {
            if let Ok(ms) = v.parse::<u128>() {
                session.overhead_ms = ms.min(MAX_OVERHEAD_MS);
            }
        }
        _ => {}
    }
}

/*----------------------------------------------------------------------------*\
                             HANDSHAKE AND NEW GAME
\*----------------------------------------------------------------------------*/

/// print_handshake
///
/// Answers the handshake with the engine's identity, its options, and the
/// terminator the GUI is waiting for. Everything dialect-specific in the
/// reply is spelled out of the protocol name, so a new dialect adds no
/// tokens here at all.
///
/// - `id name`, `id author` : the same for every dialect
/// - `Protocol`             : a combo of every dialect, current one the
///                            default
/// - `<NAME>_Variant`       : a combo listing what this dialect serves
/// - `Threads`              : 1 to however many the hardware reports
/// - `Ponder`               : a checkbox the GUI owns; the engine reads
///                            the `go`
/// - `Hash`, `Clear Hash`   : one budget, and a button that rebuilds at
///                            that size
/// - `Move Overhead`        : the clock given up per timed search
/// - `<name>ok`             : the terminator, spelled from the protocol
///                            name
///
/// The variant combo lists only what this dialect can actually speak, so a
/// GUI is never offered a variant whose moves it would be unable to read.
/// `Protocol` is offered alongside it because a session may change dialect
/// without reconnecting.
///
/// Params:
/// - session: &Session -> the session whose variants are listed
pub fn print_handshake(session: &Session) {
    let vars_str: String = session.variants
        .iter()
        .map(|v| format!(" var {}", v))
        .collect();

    let protocols_str: String = PROTOCOLS
        .iter()
        .map(|protocol| format!(" var {}", protocol.name()))
        .collect();

    emit(EngineEvent::Print(format!(
        "id name anekamacam\n\
         id author Alden Luthfi\n\
         option name {} type combo default {}{}\n\
         option name {} type combo default {}{}\n\
         option name {} type spin default 1 min 1 max {}\n\
         option name {} type check default false\n\
         option name {} type spin default {} min 1 max {}\n\
         option name {} type button\n\
         option name {} type spin default {} min 0 max {}\n\
         {}ok\n",
        OPT_PROTOCOL, session.protocol, protocols_str,
        session.variant_option(), session.variant, vars_str,
        OPT_THREADS, session.max_threads,
        OPT_PONDER,
        OPT_HASH, HASH_DEFAULT_MB, HASH_MAX_MB,
        OPT_CLEAR_HASH,
        OPT_MOVE_OVERHEAD, TIME_OVERHEAD_MS, MAX_OVERHEAD_MS,
        session.protocol,
    )));
}

/// new_game
///
/// Resets the session to the active variant's start position, aborting any
/// search in flight. Every dialect's new-game command lands here, since
/// `ucinewgame` and `usinewgame` differ in spelling and nothing else.
///
/// The variant, the options, and the hash tables all survive: a new game is
/// a new position, not a new session, and clearing the tables is what the
/// `Clear Hash` button is for.
///
/// Params:
/// - session: &mut Session -> the session to reset
pub fn new_game(session: &mut Session) {
    abort_search(session);
    let start = session.state.statics.startpos.clone();
    session.state.reset();
    parse_fen(&mut session.state, &start, None)
        .unwrap_or_else(|error| panic!("{}", error));
    refresh_eval_state(&mut session.state);
    session.position_valid = true;
}

/*----------------------------------------------------------------------------*\
                               UNIVERSAL DISPATCH
\*----------------------------------------------------------------------------*/

/// execute_common
///
/// Serves every command that behaves identically in all three dialects, and
/// contains no dialect token anywhere. It runs first, so a dialect only ever
/// sees what is left over.
///
/// - `isready`   : `readyok`, the GUI's liveness check
/// - `position`  : set the board up, however the FEN keyword was spelled
/// - `stop`      : end the search now, releasing a withheld ponder move
/// - `ponderhit` : the guess held, put the ponder search on the clock
/// - `setoption` : change one option
/// - `d`         : print the board, the result, and the reason for it
/// - `quit`      : stop the search and end the session
///
/// `d` is not part of any protocol and every GUI ignores it, which is
/// exactly what makes it useful: a human at the same stdin can look at the
/// position mid-game without the session behaving any differently.
///
/// Params:
/// - session: &mut Session -> the session being driven
/// - tokens : &[&str]      -> one trimmed, split line of input
///
/// Return:
/// Option<bool>            -> Some(should_quit) when served, None to defer
pub fn execute_common(
    session: &mut Session,
    tokens: &[&str],
) -> Option<bool> {
    match tokens.first().copied().unwrap_or("") {
        "isready" => {
            emit(EngineEvent::Print("readyok\n".to_string()));
            Some(false)
        }
        "position" => {
            handle_position(session, tokens);
            Some(false)
        }
        "stop" => {
            stop_search(session);
            Some(false)
        }
        "ponderhit" => {
            handle_ponderhit(session);
            Some(false)
        }
        "setoption" => {
            handle_setoption(session, tokens);
            Some(false)
        }
        "d" => {
            let (result, reason) = game_outcome(&mut session.state);
            let mut output = format!(
                "{}\nResult: {}\n",
                format_game_state(&session.state),
                format_game_result(result),
            );
            if let Some(name) = reason {
                output.push_str(&format!("Reason: {}\n", name));
            }
            emit(EngineEvent::Print(output));
            verify_game_state(&session.state);
            Some(false)
        }
        "quit" => {
            abort_search(session);
            Some(true)
        }
        _ => None,
    }
}

/// run
///
/// The blocking protocol main loop: one process, every dialect. It starts on
/// UCI and lets the first thing a GUI says decide what it actually is.
///
/// - a handshake word : switch to that dialect and greet, whichever it
///                      was
/// - anything else    : `execute_common` first, the dialect's `execute`
///                      after
/// - `quit` or EOF    : stop the search, drain the printer, return
///
/// Any protocol's own name works as a handshake word at any time, so a GUI
/// speaking USI is answered in USI without the engine being launched for it;
/// the `Protocol` option reaches the same switch from `setoption`. Output
/// goes through a printer thread rather than straight to stdout, so a worker
/// finishing mid-command cannot interleave its `bestmove` into another line.
///
/// The loop ends by aborting whatever search is still running and joining the
/// printer, so the process never exits with a worker mid-search or with a
/// line still queued.
///
/// Return:
/// IoResult<()> -> Ok on clean shutdown
pub fn run() -> IoResult<()> {
    let (sender, receiver) = channel::<EngineEvent>();
    set_sink(sender);
    let printer = spawn_printer(receiver);

    emit(EngineEvent::Print(format!(
        "AnekaMacam {} by Alden Luthfi\n", env!("CARGO_PKG_VERSION"),
    )));

    let mut session = Session::new("uci");

    for line in stdin().lock().lines().map_while(Result::ok) {
        let trimmed = line.trim();
        let tokens: Vec<&str> = trimmed.split_whitespace().collect();

        log_3!("{} Command: {}", session.protocol, trimmed);

        if let Some(protocol) =
            find_protocol(tokens.first().copied().unwrap_or(""))
        {
            session.set_protocol(protocol.name());
            print_handshake(&session);
            continue;
        }

        let protocol = find_protocol(&session.protocol).unwrap_or(&Uci);
        let quit = execute_common(&mut session, &tokens)
            .unwrap_or_else(|| protocol.execute(&mut session, &tokens));

        if quit {
            break;
        }
    }

    abort_search(&mut session);

    clear_sink();
    let _ = printer.join();

    Ok(())
}
