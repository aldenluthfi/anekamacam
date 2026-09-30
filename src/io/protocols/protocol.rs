//! protocol.rs
//!
//! The shared session loop of all text protocols.
//!
//! UCI, USI and UCCI differ only in the handshake word, some `go` clauses
//! and the dictionary notation. The position, variant list, hash tables,
//! search threads and ponder logic are the same. This file has the shared
//! `Session` and the `execute_common` dispatcher. Each protocol handles
//! only the lines that are different.
//!
//! Created: 19/07/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                            SESSION TIMING CONSTANTS
\*----------------------------------------------------------------------------*/

/// Session time constants
///
/// The time that a timed search keeps back for the transfer to the GUI. A
/// move that arrives late loses on time.
///
/// - TIME_OVERHEAD_MS : 50, the default, for a local pipe
/// - MAX_OVERHEAD_MS  : 1000, the maximum that a GUI can set
///
/// Notes:
/// One second is enough for all real links. A larger value is probably a
/// wrong input.
///
const TIME_OVERHEAD_MS: u128 = 50;
const MAX_OVERHEAD_MS: u128 = 1000;

/*----------------------------------------------------------------------------*\
                               DIALECT INTERFACE
\*----------------------------------------------------------------------------*/

/// Protocol
///
/// One text protocol of the engine. The trait has only two methods, so a
/// dialect has no tables to keep in step with the code. All other logic is
/// shared.
///
/// - name    : one word, the handshake and options come from it
/// - execute : only the lines that are different in this dialect
///
pub trait Protocol {
    /// name
    ///
    /// Gives the protocol name, the only string that a dialect declares.
    /// Five strings come from this one word:
    ///
    /// - `uci`           : the handshake command from the GUI
    /// - `uciok`         : the reply that ends the handshake
    /// - `UCI_Variant`   : the combo option of the variant
    /// - `= uci fen =`   : the dictionary section for FEN
    /// - `= uci moves =` : the dictionary section for moves
    ///
    /// Return:
    /// &str -> the protocol name, e.g. "uci"
    ///
    fn name(&self) -> &str;

    /// execute
    ///
    /// Handles one input line that `execute_common` did not handle. Thus it
    /// gets only the lines that are different between dialects:
    ///
    /// - new game : the same reset with three different words
    /// - `go`     : the time clauses of each dialect
    ///
    /// Params:
    /// - session: &mut Session -> the session the line acts on
    /// - tokens : &[&str]      -> the input line, split on whitespace
    ///
    /// Return:
    /// bool                    -> true when the session must stop
    ///
    /// Notes:
    /// An unknown line is ignored. A GUI can send unknown words, and output
    /// on stdout would look like a reply.
    ///
    fn execute(
        &self,
        session: &mut Session,
        tokens: &[&str],
    ) -> bool;
}

/// PROTOCOLS
///
/// All dialects of the engine, in the handshake order. The handshake word
/// or the `Protocol` option selects the active dialect at runtime. Thus one
/// process can serve chess, shogi and xiangqi GUIs, and change mid-game.
///
/// The markers have zero size, so the table needs no allocation.
///
const PROTOCOLS: [&dyn Protocol; 3] = [&Uci, &Usi, &Ucci];

/// find_protocol
///
/// Finds the dialect with a name. It compares the name with the `name` of
/// each marker, so there is no second table of words.
///
/// Params:
/// - name: &str                  -> a protocol name, e.g. "usi"
///
/// Return:
/// Option<&'static dyn Protocol> -> the dialect, or None if unknown
///
pub fn find_protocol(name: &str) -> Option<&'static dyn Protocol> {
    PROTOCOLS.into_iter().find(|protocol| protocol.name() == name)
}

/// list_variants
///
/// Finds the variants of one protocol in the embedded dictionaries. No
/// list declares them, so a new variant needs only its two files. A variant
/// needs all three:
///
/// - `<name>.dict`   : embedded, and not the example
/// - `<name>.conf`   : embedded, with the rules
/// - `= protocols =` : a line with this protocol
///
/// Params:
/// - protocol: &str    -> the protocol name to find in `protocols`
///
/// Return:
/// Vec<String>         -> sorted variant names of `protocol`
///
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
/// Sends the `bestmove` line that ends a search. The GUI waits for this
/// line, so each path sends a move or `(none)`:
///
/// - terminal position : `(none)`
/// - null best move    : a legal move without ponder, or `(none)`
/// - real best move    : the move, and the ponder move if there is one
///
/// Params:
/// - result: &SearchResult       -> result of the search
/// - state : &mut State          -> position for the move text
/// - dict  : Option<&Translator> -> translator for the move text
///
/// Notes:
/// The terminal test reads the board, because an old result can stay after
/// no search. A null move means the search stopped before depth 1 or
/// failed. A legal move is better than no move.
///
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
/// A running search thread and the data to start it again. `ponderhit`
/// restarts a ponder search as a timed search with the kept limits.
///
/// - handle              : the thread, joined on stop, abort or quit
/// - is_ponder           : true when its bestmove is kept back
/// - search_depth        : depth limit for the restart
/// - search_nodes        : node limit for the restart
/// - ponderhit_budget_ns : time for the restart, starts at the hit
///
struct SearchHandle {
    handle: JoinHandle<SearchResult>,                                           /* the running search thread          */
    is_ponder: bool,                                                            /* true if launched as a ponder       */
    search_depth: usize,                                                        /* depth limit for restart            */
    search_nodes: u128,                                                         /* node limit for restart             */
    ponderhit_budget_ns: u128,                                                  /* time budget after ponderhit        */
}

/// SearchLimits
///
/// The start parameters of one search thread, as one argument of
/// `spawn_search`.
///
/// - deadline            : a time, nanoseconds since engine start
/// - ponderhit_budget_ns : a duration, starts at the ponder hit
///
/// A timed search knows its stop time. A ponder search does not know when
/// its clock starts, so it keeps a duration.
///
struct SearchLimits {
    is_ponder: bool,                                                            /* launch as a ponder search          */
    depth: usize,                                                               /* search depth limit                 */
    nodes: u128,                                                                /* search node limit                  */
    deadline: u128,                                                             /* deadline, ns since launch          */
    ponderhit_budget_ns: u128,                                                  /* time budget after ponderhit        */
}

/// Session
///
/// All protocol state of one GUI session, shared by all dialects. There is
/// one session for each process.
///
/// - protocol, translator  : the active dialect and its notation
/// - state, position_valid : the board, and true when it is correct
/// - variants, variant     : variants of this dialect, and the loaded one
/// - threads, hash_mb      : options that the GUI can change
/// - overhead_ms           : time kept back from a timed search
/// - ttable, qtable        : shared with the workers, new on Hash
/// - active                : the running search, if any
///
/// Notes:
/// `position_valid` is false after a failed `position` until the next good
/// load. Then `go` sends `bestmove (none)` at once, and does not search a
/// wrong board.
///
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
    /// Makes a session for one protocol, ready after the handshake.
    ///
    /// - variant    : standard if the dialect has it, else the first one
    /// - state      : the config of that variant, at its start position
    /// - translator : the dialect section of the variant dictionary
    /// - tables     : default size, two thirds for the main table
    ///
    /// Params:
    /// - protocol: &str -> the protocol name
    ///
    /// Return:
    /// Self             -> a session ready for commands
    ///
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
    /// Gives the name of the variant combo option. It comes from the
    /// protocol name, so the handshake and `setoption` use the same name.
    ///
    /// - `uci`  : `UCI_Variant`
    /// - `usi`  : `USI_Variant`
    /// - `ucci` : `UCCI_Variant`
    ///
    /// Return:
    /// String -> the `<NAME>_Variant` option name
    ///
    fn variant_option(&self) -> String {
        format!("{}_Variant", self.protocol.to_uppercase())
    }

    /// Session::set_protocol
    ///
    /// Changes the dialect of the session, from the handshake word or the
    /// `Protocol` option:
    ///
    /// 1. stop the running search
    /// 2. find the variants of the new dialect
    /// 3. load its default variant, if the current one is not available
    /// 4. load the notation of the new dialect
    ///
    /// Params:
    /// - protocol: &str -> the new dialect name
    ///
    /// Notes:
    /// A variant of the two dialects keeps its position. Else, the function
    /// loads the new config from scratch and resets the board.
    ///
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
/// Divides the `Hash` megabytes between the two shared tables. The GUI
/// sets one value for all tables.
///
/// - main search : two thirds
/// - quiescence  : one third
///
/// Params:
/// - hash_mb: usize           -> total table size in megabytes
///
/// Return:
/// (Arc<TTable>, Arc<QTable>) -> the new tables
///
/// Notes:
/// Each table has at least one megabyte. `Hash` and `Clear Hash` both use
/// this function, because a new table is a clear table.
///
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
/// The session command handlers. A command that changes the position or the
/// tables first stops the search. A worker has a copy of the state and of
/// the table handles.
///
/// - handle_position  : make the board from startpos or FEN, play moves
/// - abort_search     : interrupt and join, give the search result
/// - stop_search      : abort, and send a kept ponder move
/// - handle_ponderhit : join the ponder search, restart it with the clock
/// - handle_setoption : set Protocol, Variant, Threads, Hash or Overhead
///
/// `start_search` and `spawn_search` are the two parts of `go` and have
/// their own docs.
///
/// handle_position
///
///   Skips the FEN keyword, so `fen` and `sfen` both work. It makes the
///   board in a fork and installs it only after the full line succeeds.
///   Thus a bad move keeps the previous position.
///
///   Params:
///   - session: &mut Session -> session, gets the new position on success
///   - tokens : &[&str]      -> `startpos` or FEN, optional `moves` list
///
/// abort_search
///
///   Sets the interrupt, joins the thread and clears the interrupt. A
///   worker panic gives None, so a crash loses a move, not the session.
///
///   Params:
///   - session: &mut Session -> session with the search to stop
///
///   Return:
///   Option<(SearchResult, bool)> -> result and ponder flag, or None
///
/// stop_search
///
///   A ponder search keeps its `bestmove` back, so `stop` sends it. A
///   timed search already sent its move, so `stop` sends nothing.
///
///   Params:
///   - session: &mut Session -> session with the search to stop
///
/// handle_ponderhit
///
///   The ponder move was correct. The function joins the thread and starts
///   it again with its depth and node limits. The time starts at the hit.
///
///   Params:
///   - session: &mut Session -> session with the ponder search
///
/// handle_setoption
///
///   A name can have spaces, so the name is all words between `name` and
///   `value`. An unknown option or a bad value changes nothing.
///
///   Params:
///   - session: &mut Session -> session to change
///   - tokens : &[&str]      -> the `setoption` line, split
///
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
/// Parses a standard `go` line and starts the search thread. The dialect
/// already renamed its clauses, so no dialect word gets here.
///
/// - `movetime m` : m minus overhead, minimum one millisecond
/// - a clock      : two shares, a share is (remaining - overhead) / movestogo
/// - neither      : no deadline, for ponder and infinite
///
/// Params:
/// - session: &mut Session -> session of the `go`
/// - tokens : &[&str]      -> standard `go` tokens
///
/// Notes:
/// Without `movestogo`, the clock uses 15 moves. A share also gets the
/// increment, and the time is at most half the clock. The search starts a
/// new depth only in the first quarter of the time, so a clock move uses
/// about one share, and a depth that has started can finish. The deadline starts
/// when the `go` is read, so the thread start uses part of the time.
///
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
        let moves = if movestogo > 0 { movestogo as u128 } else { 15 };
        let share = (remaining / moves).saturating_add(inc_ms);

        (2 * share).clamp(1, (remaining / 2).max(1)) * 1_000_000                /* a new depth starts in the first    */
    };                                                                          /* quarter, see iterative_deepening   */

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
/// Starts the search thread with the limits and keeps it as the active
/// handle of the session. The thread gets a copy of the position, so the
/// session can still answer `stop` and `isready`.
///
/// A ponder search starts without limits:
///
/// - depth    : the maximum
/// - nodes    : no limit
/// - deadline : none
/// - bestmove : kept back until `stop`
///
/// Params:
/// - session: &mut Session -> the session to change
/// - limits : SearchLimits -> start parameters of the thread
///
/// Notes:
/// Ponder uses the clock of the opponent. The handle keeps the real limits
/// for the hit. A panic in the search gives an empty result, so the join in
/// `abort_search` does not stop the session.
///
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
/// Answers the handshake with the engine name, its options and the end
/// word. The dialect parts come from the protocol name.
///
/// - `id name`, `id author` : the same for all dialects
/// - `Protocol`             : combo of all dialects, current one default
/// - `<NAME>_Variant`       : combo of the variants of this dialect
/// - `Threads`              : 1 to the hardware thread count
/// - `Ponder`               : checkbox for the GUI, the engine reads `go`
/// - `Hash`, `Clear Hash`   : table size, and a button to clear
/// - `Move Overhead`        : time kept back for each timed search
/// - `<name>ok`             : the end word, from the protocol name
///
/// Params:
/// - session: &Session -> the session with the variants
///
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
/// Resets the session to the start position of the variant and stops the
/// search. The new game command of each dialect calls it. The variant, the
/// options and the tables do not change.
///
/// Params:
/// - session: &mut Session -> the session to reset
///
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
/// Handles the commands that are the same in all dialects. It has no
/// dialect word. It runs first, and the dialect gets the other lines.
///
/// - `isready`   : reply `readyok`
/// - `position`  : set the board, with any FEN keyword
/// - `stop`      : stop the search, send a kept ponder move
/// - `ponderhit` : put the ponder search on the clock
/// - `setoption` : change one option
/// - `d`         : print the board, the result and the reason
/// - `quit`      : stop the search and end the session
///
/// Params:
/// - session: &mut Session -> the session
/// - tokens : &[&str]      -> one input line, trimmed and split
///
/// Return:
/// Option<bool>            -> Some(quit) when handled, None to defer
///
/// Notes:
/// `d` is in no protocol, and GUIs ignore it. A person can use it on the
/// same stdin during a game.
///
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
/// The blocking protocol main loop for all dialects. It starts with UCI.
/// The first word from the GUI selects the dialect.
///
/// - a handshake word : change to that dialect and reply
/// - other lines      : `execute_common` first, then the dialect `execute`
/// - `quit` or EOF    : stop the search, flush the printer, return
///
/// Return:
/// IoResult<()> -> Ok on a clean stop
///
/// Notes:
/// A protocol name works as a handshake word at all times. A printer
/// thread writes all output, so a `bestmove` does not mix into another
/// line. At the end, the loop stops the search and joins the printer.
///
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
