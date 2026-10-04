//! graphics.rs
//!
//! Ratatui debug console of the engine.
//!
//! A person must see the board, the move history and the derived piece
//! data at the same time. This file is a terminal interface with tabs
//! around the normal command loop. The commands control the game, and all
//! panes stay current.
//!
//! Created: 23/04/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                LAYOUT CONSTANTS
\*----------------------------------------------------------------------------*/

/// MAX_LOGS_SCROLL
///
/// The maximum scroll offset of a log pane. A ratatui offset is a `u16`.
/// `clamp_scroll` uses `u16::MAX` for "stay at the newest line", so the
/// maximum offset is one less.
///
const MAX_LOGS_SCROLL: u16 = u16::MAX - 1;

/// Interface constants
///
/// Fixed values of the interface: the tabs, the keyboard modes and the
/// scroll keys of the panes.
///
/// - TUI_INPUT_MODE  : all keys go into the command line
/// - TUI_NORMAL_MODE : keys move the focus and scroll
/// - TAB_TITLES      : the three tabs, in display order
/// - TAB_FOCUSABLES  : the number of focus panes of each tab
/// - TAB_FOCUS_MOVES : focus index of the move history
/// - TAB_FOCUS_FEN   : focus index of the FEN line
/// - TAB_FOCUS_LOGS  : focus index of the log pane
///
/// The scroll key of a pane is (tab, focus). Two scrolling views are not
/// in a tab, so they use a tab that does not exist:
///
/// - SENTINEL_TAB      : 100, the game selection screen
/// - PICKER_SCROLL_KEY : `(SENTINEL_TAB, 100)`, the variant list
/// - HELP_SCROLL_KEY   : `(SENTINEL_TAB, 50)`, the help overlay
///
/// Notes:
/// A value past `TAB_TITLES.len()` also marks an open help overlay, so
/// `help_open!` excludes the sentinel by name.
///
const TUI_INPUT_MODE: u8 = 0;
const TUI_NORMAL_MODE: u8 = 1;

const TAB_TITLES: [&str; 3] = ["Game", "Overview", "Playground"];
const TAB_FOCUSABLES: [u8; 3] = [3, 2, 3];

const SENTINEL_TAB: usize = 100usize;

const PICKER_SCROLL_KEY: (usize, usize) = (SENTINEL_TAB, 100usize);
const HELP_SCROLL_KEY: (usize, usize) = (SENTINEL_TAB, 50usize);

const TAB_FOCUS_MOVES: usize = 0;
const TAB_FOCUS_FEN: usize = 1;
const TAB_FOCUS_LOGS: usize = 2;

/*----------------------------------------------------------------------------*\
                              CONSTRUCTION MACROS
\*----------------------------------------------------------------------------*/

/// TUI construction macros
///
/// Widget macros of the debug console, private to this file.
///
/// - `split_area!`   : divides one rect into parts
/// - `padded_block!` : the frame of a pane
/// - `guide_label!`  : one named box of the help layout guide
///
/// split_area!
///
///   Divides one rect. The `overlap` form adds a one-cell overlap, so
///   panes next to each other share a border line.
///
///   Params:
///   - direction  : ident -> `Horizontal` or `Vertical`
///   - area       : Rect  -> rect to divide
///   - constraints: expr  -> constraint array, one entry for each part
///
///   Return:
///   Rects                -> the parts, in constraint order
///
/// padded_block!
///
///   The frame of a pane: a full border and one column of padding inside.
///   The `style` form colours the border for focus. The `merge` form lets
///   frames next to each other share a border line.
///
///   Params:
///   - style: expr -> border style, `style` form only
///
///   Return:
///   Block<'_>     -> the frame for `.block()`
///
/// guide_label!
///
///   Names one pane in the help layout guide, in a padded box whose
///   borders merge with its neighbours. The `center` form is for the board
///   pane, without a box.
///
///   Params:
///   - text: expr -> the pane name
///
///   Return:
///   Paragraph<'_> -> the label, boxed or centered
///
macro_rules! split_area {
    ($direction:ident, $area:expr, $constraints:expr) => {
        Layout::default()
            .direction(Direction::$direction)
            .constraints($constraints)
            .split($area)
    };
    (overlap $direction:ident, $area:expr, $constraints:expr) => {
        Layout::default()
            .direction(Direction::$direction)
            .spacing(Spacing::Overlap(1))
            .constraints($constraints)
            .split($area)
    };
}

macro_rules! padded_block {
    () => {
        Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
    };
    (style $style:expr) => {
        padded_block!().border_style($style)
    };
    (merge) => {
        padded_block!().merge_borders(MergeStrategy::Exact)
    };
}

macro_rules! guide_label {
    ($text:expr) => {
        Paragraph::new($text).block(padded_block!(merge))
    };
    (center $text:expr) => {
        Paragraph::new($text).alignment(Alignment::Center)
    };
}

/*----------------------------------------------------------------------------*\
                                  TAB ENCODING
\*----------------------------------------------------------------------------*/

/// help_open!
///
/// Tells if the help overlay is open. One number has the tab and the help
/// page. The page adds multiples of the tab count, so a value at or above
/// that count has help open. The macro excludes the sentinel by name.
///
/// Params:
/// - tab: usize -> encoded tab value to test
///
/// Return:
/// bool         -> true when help is open on that tab
///
macro_rules! help_open {
    ($tab:expr) => {
        $tab >= TAB_TITLES.len() && $tab != SENTINEL_TAB
    };
}

/// peel_help
///
/// Splits the encoded number into the tab and the help page. There are
/// three tabs, so the step is three:
///
/// - 1 : Overview, tab 1, no help
/// - 4 : Overview with help on Keybinds, tab 1, page 0
/// - 7 : Overview with help on Commands, tab 1, page 1
///
/// Params:
/// - tab: usize   -> encoded tab value to decode
///
/// Return:
/// (usize, usize) -> the tab, and the help page
///
/// Notes:
/// The first step opens the overlay, so the page is the step count minus
/// one. Without help, the page is 0 and callers ignore it.
///
fn peel_help(mut tab: usize) -> (usize, usize) {
    let mut layers = 0;

    while help_open!(tab) {
        tab -= TAB_TITLES.len();
        layers += 1;
    }

    (tab, layers.max(1) - 1)
}

/// clamp_scroll
///
/// Fits the stored scroll offset of one pane to its current end and stores
/// the result. `u16::MAX` means "pinned to the bottom", so the pane follows
/// new content.
///
/// - current < max          : no change
/// - current == max, or MAX : pinned, stored as MAX, drawn at the end
/// - max < current < MAX    : `max - (MAX - current)`, lines up from the end
///
/// The third case keeps the distance from the end when the content gets
/// shorter.
///
/// Params:
/// - current   : u16            -> stored offset, equal to `scroll_map[key]`
/// - max       : u16            -> largest offset of the pane
/// - scroll_map: &mut HashMap   -> offsets by (tab, focus), updated
/// - key       : (usize, usize) -> the key of the pane in that map
///
/// Return:
/// u16                          -> offset for this frame
///
fn clamp_scroll(
    current: u16,
    max: u16,
    scroll_map: &mut HashMap<(usize, usize), u16>,
    key: (usize, usize)
) -> u16 {
    if current > max && current < u16::MAX {
        let clamped = max.saturating_sub(u16::MAX - current);
        scroll_map.insert(key, clamped);
        clamped
    } else if current >= max {
        scroll_map.insert(key, u16::MAX);
        max
    } else {
        current
    }
}

/// log_level
///
/// Reads the level that `push_log_message!` put at the start of a line. A
/// line without a level gets level 5, so a bad line does not stop the
/// render.
///
/// Params:
/// - line: &str -> queued log line, `[n] message`
///
/// Return:
/// u8           -> the level, or 5
///
fn log_level(line: &str) -> u8 {
    match line.as_bytes() {
        [b'[', digit @ b'1'..=b'5', b']', ..] => digit - b'0',
        _ => 5,
    }
}

/// colour_log_line
///
/// Splits a queued line into its level and its message, and colours the
/// level: red for critical, magenta for trace. The message has no colour.
///
/// Params:
/// - line: &str       -> queued log line, `[n] message`
///
/// Return:
/// Line<'static>      -> the styled row
///
fn colour_log_line(line: &str) -> Line<'static> {
    let level_color = match log_level(line) {
        1 => Color::LightRed,
        2 => Color::Yellow,
        3 => Color::LightGreen,
        4 => Color::LightBlue,
        _ => Color::Magenta,
    };

    if line.starts_with('[') && let Some(end_idx) = line.find(']') {
        let (level, rest) = line.split_at(end_idx + 1);

        return Line::from(vec![
            Span::styled(level.to_string(), Style::default().fg(level_color)),
            Span::raw(rest.to_string()),
        ]);
    }

    Line::from(Span::raw(line.to_string()))
}

/// visible_log_lines
///
/// Gives the rows of a log pane: filtered by verbosity, clamped, and cut to
/// the pane height. The caller draws them at scroll zero.
///
/// 1. filter by level first, so a quiet level does not show an empty pane
/// 2. count rows in `usize`, because taikyoku logs about 78000 lines
/// 3. make a `Line` only for the visible rows
///
/// Params:
/// - height    : u16            -> rows of the pane
/// - scroll_map: &mut HashMap   -> offsets by (tab, focus), updated
/// - key       : (usize, usize) -> key of the pane offset
///
/// Return:
/// Vec<Line<'static>>           -> the rows to draw, newest last
///
fn visible_log_lines(
    height: u16,
    scroll_map: &mut HashMap<(usize, usize), u16>,
    key: (usize, usize)
) -> Vec<Line<'static>> {
    let verbosity = configured_verbosity_level();
    let logs = LOG_MESSAGES.lock().unwrap_or_else(|e| e.into_inner());

    let shown = logs                                                            /* counted, not collected: the queue  */
        .iter()                                                                 /* runs to tens of thousands of lines */
        .filter(|line| log_level(line) <= verbosity)
        .count();

    let max_scroll = shown
        .saturating_sub(height as usize)
        .min(MAX_LOGS_SCROLL as usize) as u16;                                  /* u16::MAX is the pinned sentinel    */

    let current = scroll_map.get(&key).copied().unwrap_or(0);
    let offset = clamp_scroll(current, max_scroll, scroll_map, key) as usize;

    logs.iter()
        .filter(|line| log_level(line) <= verbosity)
        .skip(offset)
        .take(height as usize)
        .map(|line| colour_log_line(line))
        .collect()
}

/*----------------------------------------------------------------------------*\
                                INTERFACE STATE
\*----------------------------------------------------------------------------*/

/// Tui
///
/// All state of the interface. It is made at startup and used by each
/// draw and each key press.
///
/// - mode, input, locked     : the command line, its mode, and its lock
/// - tab, focus, scroll_map  : the tab, the pane and each scroll offset
/// - variant, translator     : the loaded variant and its notation
/// - game_state, board_state : the game position and its drawn form
/// - playground_state        : a second position, for that tab
/// - overview_state          : the variant config, drawn once
/// - threads                 : number of search threads
/// - receiver                : events from the engine
///
/// Notes:
/// The positions are in `Arc<Mutex<State>>`, because a search runs on a
/// worker thread during the draws. A frame reads text that was drawn once.
/// A snapshot is rebuilt only when its position changes.
///
struct Tui {
    mode: u8,                                                                   /* input mode (normal / command)      */
    threads: usize,                                                             /* worker threads for searches        */

    tab: usize,                                                                 /* active top-level tab               */
    focus: usize,                                                               /* focused pane within the tab        */
    input: String,                                                              /* current command input line         */
    locked: bool,                                                               /* input locked during long ops       */

    scroll_map: HashMap<(usize, usize), u16>,                                   /* per-pane scroll offsets            */
    translator: Option<Translator>,                                             /* active protocol translator         */

    variant: Option<String>,                                                    /* loaded variant name                */
    game_state: Option<Arc<Mutex<State>>>,                                      /* shared game position               */
    board_state: Option<BoardState>,                                            /* rendered game snapshot             */
    overview_state: Option<OverviewState>,                                      /* rendered piece overview            */
    playground_state: Option<Arc<Mutex<State>>>,                                /* shared playground position         */

    receiver: Receiver<EngineEvent>,                                            /* engine-to-TUI event channel        */
}

impl Tui {
    /// Tui construction and teardown
    ///
    /// Start and end of a loaded variant. The two give the same state: the
    /// game selection screen, nothing loaded, no scroll.
    ///
    /// new
    ///
    ///   Makes the interface with a scroll slot at the top for each pane
    ///   and the two overlays. It uses one search thread until a `threads`
    ///   command.
    ///
    ///   Params:
    ///   - receiver: Receiver<EngineEvent> -> events from the engine
    ///
    ///   Return:
    ///   Self                              -> the interface, on selection
    ///
    /// reset
    ///
    ///   Drops the loaded states and goes back to the selection screen. It
    ///   sets the interrupt flag, because a worker can still search. The
    ///   `Unlock` event of that worker clears the flag. No parameters and
    ///   no return value.
    ///
    fn new(
        receiver: Receiver<EngineEvent>
    ) -> Self {
        let mut scroll_map = HashMap::new();

        for (tab, focusables) in
            TAB_FOCUSABLES.iter().enumerate().take(TAB_TITLES.len())
        {
            for focus in 0..*focusables as usize {
                let start = match focus {
                    TAB_FOCUS_LOGS => u16::MAX,                                 /* logs open on the newest line       */
                    _ => 0,
                };

                scroll_map.insert((tab, focus), start);
            }
        }

        scroll_map.insert(PICKER_SCROLL_KEY, 0);                                /* Selection screen scroll            */
        scroll_map.insert(HELP_SCROLL_KEY, 0);                                  /* Help popup content scroll          */

        let mode = TUI_NORMAL_MODE;
        let tab = PICKER_SCROLL_KEY.0;
        let focus = PICKER_SCROLL_KEY.1;
        let input = String::new();
        let locked = false;
        let variant = None;
        let game_state = None;
        let board_state = None;
        let overview_state = None;
        let playground_state = None;
        let threads = 1;
        let translator = None;

        Self {
            mode,
            threads,

            tab,
            focus,
            scroll_map,

            input,

            locked,

            translator,

            variant,
            game_state,
            board_state,
            overview_state,
            playground_state,

            receiver,
        }
    }

    fn reset(&mut self) {
        self.mode = TUI_NORMAL_MODE;
        self.tab = PICKER_SCROLL_KEY.0;
        self.focus = PICKER_SCROLL_KEY.1;

        self.input.clear();

        self.variant = None;
        self.game_state = None;
        self.board_state = None;
        self.overview_state = None;
        self.playground_state = None;

        SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
    }

    /// Tui::run
    ///
    /// The main loop. It draws a frame, reads the engine events, and waits
    /// up to 16 milliseconds for a key, 60 frames a second.
    ///
    /// - Board            : a new game board to show
    /// - StateInit        : a variant loaded, make overview and playground
    /// - PlaygroundUpdate : a new playground position
    /// - SwitchDict       : a new notation, draw the board again
    /// - Unlock           : the worker ended, clear interrupt, unlock input
    ///
    /// Params:
    /// - terminal: &mut DefaultTerminal -> the ratatui terminal
    ///
    /// Return:
    /// IoResult<()>                     -> Ok on a clean exit, else Err
    ///
    /// Notes:
    /// The loop ignores other events. Log lines come from the logger queue.
    ///
    fn run (&mut self, terminal: &mut DefaultTerminal) -> IoResult<()> {
        loop {
            terminal.draw(|frame| render(frame, self))?;

            while let Ok(engine_event) = self.receiver.try_recv() {
                match engine_event {
                    EngineEvent::Board(state) => {
                        self.board_state = Some(state);
                    },
                    EngineEvent::StateInit(state) => {
                        self.overview_state = Some(
                            OverviewState::from_state(&state.lock().unwrap())
                        );
                        let mut pg_state =
                            State::clone(&state.lock().unwrap());
                        init_playground(&mut pg_state, 0);
                        self.playground_state =
                            Some(Arc::new(Mutex::new(pg_state)));
                        self.game_state = Some(state);
                    },
                    EngineEvent::PlaygroundUpdate(state) => {
                        if let Some(arc) = &self.playground_state {
                            *arc.lock().unwrap() = *state;
                        }
                    },
                    EngineEvent::SwitchDict(dict) => {
                        self.translator = dict;
                        if let Some(arc) = self.game_state.clone() {
                            let state = arc.lock().unwrap();
                            self.board_state = Some(BoardState::from_state(
                                &state,
                                self.translator.as_ref(),
                            ));
                        }
                    }
                    EngineEvent::Unlock => {
                        SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);
                        self.locked = false;
                    }
                    _ => {}
                }
            }

            if event::poll(Duration::from_millis(16))?
                && let Event::Key(key_event) = event::read()?
                && handle_key(self, key_event)
            {
                break;
            }
        }

        Ok(())
    }
}

/*----------------------------------------------------------------------------*\
                               OVERVIEW SNAPSHOT
\*----------------------------------------------------------------------------*/

/// Overview tab snapshot types
///
/// Snapshot types of the Overview tab. The tab shows the variant config,
/// which does not change, so the tab is drawn once at load.
///
/// - OverviewState     : the config rows and the piece list
/// - OverviewPiece     : one list row, a name and optional attributes
/// - OverviewPieceInfo : the formatted attributes of one piece
///
/// The attributes are the letters, promotions, roles, material values,
/// tables and zones. A config row has a height, because the rule list can
/// have many lines.
///
/// Notes:
/// Each piece type has one row, with the attributes of the White piece.
/// `info` is `None` for a name without a White piece.
///
struct OverviewPieceInfo {
    char_str: String,                                                           /* board letter for the piece         */
    promotions: String,                                                         /* promotion targets, formatted       */
    roles: String,                                                              /* big/major/minor role labels        */
    op_val: u16,                                                                /* opening material value             */
    eg_val: u16,                                                                /* endgame material value             */
    op_pst: String,                                                             /* opening PST, formatted grid        */
    eg_pst: String,                                                             /* endgame PST, formatted grid        */
    forbidden_zones: Option<String>,                                            /* forbidden-zone grid, if any        */
    mandatory_promotions: Option<String>,                                       /* mandatory promo zone, if any       */
    optional_promotions: Option<String>,                                        /* optional promo zone, if any        */
}

struct OverviewPiece {
    name: String,                                                               /* piece display name                 */
    info: Option<OverviewPieceInfo>,                                            /* rendered attributes, if a piece    */
}

struct OverviewState {
    configs: Vec<(String, String, u16)>,                                        /* variant config rows                */
    pieces: Vec<OverviewPiece>,                                                 /* one entry per piece type           */
}

impl OverviewState {
    /// OverviewState::from_state
    ///
    /// Draws the full tab from a loaded variant, once. First the config
    /// rows of the rules that the variant has, for example a counter limit
    /// only with a counter. Then one row for each piece name, in config
    /// order.
    ///
    /// Params:
    /// - state: &State -> variant state to draw
    ///
    /// Return:
    /// Self            -> the drawn overview
    ///
    fn from_state(state: &State) -> Self {
        let mut configs = Vec::new();
        configs.push(("Title".to_string(), state.statics.title.clone(), 1));
        configs.push((
            "Board Size".to_string(),
            format!("{}x{}", state.statics.files, state.statics.ranks),
            1
        ));
        configs.push((
            "Opening Score".to_string(),
            state.statics.opening_score.to_string(), 1
        ));
        configs.push((
            "Endgame Score".to_string(),
            state.statics.endgame_score.to_string(), 1
        ));

        if let Some(counter) = &state.termination.counter {
            configs.push((
                "Counter Limit".to_string(),
                counter.limit.to_string(), 1
            ));
        }
        if let Some(repetition) = &state.termination.repetition {
            configs.push((
                "Repetition Limit".to_string(),
                repetition.occurrences.to_string(), 1
            ));
        }

        let rules = format_special_rules(state).replace(", ", "\n");
        let rules_height = state.statics.special_rules.count_ones();

        configs.push((
            "Rules".to_string(), rules, rules_height.max(1) as u16
        ));

        let mut piece_map: HashMap<String, OverviewPiece> = HashMap::new();
        let mut piece_names = Vec::new();

        for piece in state.statics.pieces.iter() {
            let name = piece.name.clone();
            let color = p_color!(piece);
            let index = p_index!(piece) as usize;

            let char_str = if color == WHITE {
                let black_char = state.statics.pieces[
                    state.statics.piece_swap_map[index] as usize
                ].char;
                format!("{}{}", piece.char, black_char)
            } else {
                piece.char.to_string()
            };

            let roles = if p_is_royal!(piece) {
                "Royal".to_string()
            } else {
                let mut result = Vec::new();

                if p_is_big!(piece) {
                    result.push("Big");
                } else {
                    result.push("Non-Big");
                }

                if p_is_major!(piece) {
                    result.push("Major");
                } else if p_is_minor!(piece) {
                    result.push("Minor");
                }

                if result.is_empty() {
                    "-".to_string()
                } else {
                    result.join(", ")
                }
            };

            let promotions = if piece.promotions.is_empty() {
                "-".to_string()
            } else {
                piece.promotions.iter()
                    .map(|&p| state.statics.pieces[p as usize].name.clone())
                    .collect::<Vec<_>>()
                    .join(", ")
            };
            let op_val = p_ovalue!(piece);
            let eg_val = p_evalue!(piece);

            let mut op_pst_str = String::from("None");
            if let Some(table) = state.statics.pst_opening.get(index) {
                op_pst_str = format_numeric_board(
                    table, state.statics.files, state.statics.ranks
                );
            }
            let mut eg_pst_str = String::from("None");
            if let Some(table) = state.statics.pst_endgame.get(index) {
                eg_pst_str = format_numeric_board(
                    table, state.statics.files, state.statics.ranks
                );
            }

            let forbidden_zones =
            if forbidden_zones!(state)
            && !is_empty!(state.statics.forbidden_zones[index])
            {
                Some(format_board(
                    &state.statics.forbidden_zones[index], Some('X')
                ))
            } else {
                None
            };

            let mandatory_promotions =
            if promotions!(state)
            && !is_empty!(state.statics.promotion_zones_mandatory[index]) {
                Some(format_board(
                    &state.statics.promotion_zones_mandatory[index],
                    Some('X')
                ))
            } else {
                None
            };

            let optional_promotions =
            if promotions!(state)
            && !is_empty!(state.statics.promotion_zones_optional[index]) {
                Some(format_board(
                    &state.statics.promotion_zones_optional[index],
                    Some('X')
                ))
            } else {
                None
            };

            let info = OverviewPieceInfo {
                char_str,
                roles,
                promotions,
                op_val,
                eg_val,
                op_pst: op_pst_str,
                eg_pst: eg_pst_str,
                forbidden_zones,
                mandatory_promotions,
                optional_promotions,
            };

            let entry = piece_map.entry(name.clone()).or_insert_with(|| {
                piece_names.push(name.clone());
                OverviewPiece { name: name.clone(), info: None }
            });

            if color == WHITE {
                entry.info = Some(info);
            }
        }

        let pieces = piece_names
            .into_iter()
            .filter_map(|n| piece_map.remove(&n))
            .collect();

        Self {
            configs,
            pieces,
        }
    }
}

/*----------------------------------------------------------------------------*\
                                 BOARD SNAPSHOT
\*----------------------------------------------------------------------------*/

/// BoardState
///
/// The drawn Game tab. It is drawn once for each move, so a frame only
/// copies text.
///
/// - board        : the board diagram with pieces and coordinates
/// - move_history : the numbered moves of the game
/// - details      : label and value rows
/// - fen          : the position as one line
///
pub struct BoardState {
    board: String,                                                              /* composite board diagram            */
    move_history: String,                                                       /* numbered move history              */
    details: Vec<[String; 2]>,                                                  /* label/value detail rows            */
    fen: String,                                                                /* current position FEN               */
}

impl BoardState {
    /// BoardState::from_state
    ///
    /// Draws the tab from the position. The detail rows show only the rules
    /// that the variant has:
    ///
    /// - Game Phase       : always, with the phase score
    /// - Result           : after the end, with the reason
    /// - Turn             : always
    /// - Position Hash    : always
    /// - Castling Rights  : only with castling
    /// - En Passant       : only with en passant
    /// - Halfmove Clock   : only with a counter, as clock of limit
    /// - Repetition Count : only with repetition, as count of limit
    /// - Pieces in Hand   : only with drops, captured promotion or setup
    ///
    /// Params:
    /// - state: &State              -> position to draw
    /// - dict : Option<&Translator> -> translator for the move text
    ///
    /// Return:
    /// Self                         -> the drawn board state
    ///
    pub fn from_state(state: &State, dict: Option<&Translator>) -> Self {
        let board = format_game_state(state);
        let move_history = format_move_history(state, dict);
        let fen = format_fen(state, dict);
        let mut details = Vec::new();

        let fen_parts: Vec<&str> = fen.split_whitespace().collect();
        let position_hash = format_position_hash(state);
        let game_phase = format_game_phase(state);
        let phase = format!("{} ({})", game_phase, state.phase_score);

        let halfmove_clock;
        let repetition_count;
        let hand_info;

        details.push(["Game Phase".to_string(), phase]);

        let (outcome, reason) = game_outcome(&mut state.clone());               /* the same result as the `d` command */
        if outcome != ONGOING {                                                 /* a clone, because the repetition    */
            let mut result = format_game_result(outcome);                       /* walk needs a mutable position      */
            if let Some(name) = reason {
                result = format!("{} ({})", result, name);
            }

            details.push(["Result".to_string(), result]);
        }
        details.push(
            [
                "Turn".to_string(),
                if state.playing == WHITE { "White" } else { "Black" }
                .to_string()
            ]
        );
        details.push(["Position Hash".to_string(), position_hash]);
        if castling!(state) {
            details.push([
                "Castling Rights".to_string(),
                fen_parts[2].to_string(),
            ]);
        }
        if en_passant!(state) {
            details.push([
                "En Passant".to_string(),
                fen_parts[2 + castling!(state) as usize].to_string(),
            ]);
        }
        if let Some(counter) = &state.termination.counter {
            halfmove_clock = format!("{}/{}",
                counter.clock,
                counter.limit
            );
            details.push(["Halfmove Clock".to_string(), halfmove_clock]);
        }
        if let Some(repetition) = &state.termination.repetition {
            repetition_count = format!("{}/{}",
                count_repetitions(state, usize::MAX),
                repetition.occurrences
            );
            details.push(["Repetition Count".to_string(), repetition_count]);
        }
        if drops!(state)
        || promote_to_captured!(state)
        || setup_phase!(state) {
            hand_info = format!(
                "White [{}] | Black [{}]\n",
                format_hand(state, WHITE),
                format_hand(state, BLACK)
            );
            details.push(["Pieces in Hand".to_string(), hand_info]);
        }

        Self {
            board,
            move_history,
            details,
            fen,
        }
    }
}

/*----------------------------------------------------------------------------*\
                                   PLAYGROUND
\*----------------------------------------------------------------------------*/

/// Playground state helpers
///
/// Helpers of the Playground tab. The tab shows where one piece can move,
/// on an empty board.
///
/// - `init_playground`      : empties the board for a piece type
/// - `set_playground_piece` : puts that piece on a square
///
/// The empty FEN has the board size and the rule fields of the variant.
/// The side to move is the colour of the piece.
///
/// init_playground
///
///   Params:
///   - state: &mut State -> playground position, gets an empty board
///   - index: PieceIndex -> piece type of the board
///
/// set_playground_piece
///
///   Params:
///   - state : &mut State -> playground position
///   - index : PieceIndex -> piece type to place
///   - square: Square     -> square of the piece, its moves reload
///
/// Notes:
/// The two use `load_fen`, because the load derives the hashes, piece lists
/// and material. Then they set the phase to opening, else one piece reads
/// as an endgame.
///
fn init_playground(state: &mut State, index: PieceIndex) {
    let piece = &state.statics.pieces[index as usize];
    let color = p_color!(piece);

    let mut empty_fen = String::new();

    let empty_ranks = (0..state.statics.ranks)
        .map(|_| state.statics.files.to_string())
        .collect::<Vec<_>>()
        .join("/");

    empty_fen.push_str(&empty_ranks);
    empty_fen.push_str(&format!(" {}", if color == WHITE { "w" } else { "b" }));

    if castling!(state) {
        empty_fen.push_str(" KQkq");
    }

    if en_passant!(state) {
        empty_fen.push_str(" *");
    }

    if drops!(state) || promote_to_captured!(state) || setup_phase!(state) {
        empty_fen.push_str(" -/-");
    }

    state.load_fen(&empty_fen, None);

    state.game_phase = OPENING;
}

fn set_playground_piece(state: &mut State, index: PieceIndex, square: Square) {
    state.main_board[square as usize] = index;

    let fen = format_fen(state, None);
    state.load_fen(&fen, None);

    state.game_phase = OPENING;
}

/*----------------------------------------------------------------------------*\
                                    DRAWING
\*----------------------------------------------------------------------------*/

/// TUI drawing functions
///
/// Each function draws one region of the frame from the current `Tui`. They
/// read the snapshots, not the positions, and write at most a scroll
/// offset. `render` makes the layout and calls the other functions. Each
/// screen has `draw_tabs` at the top, `draw_input` at the bottom with
/// `draw_help_bar`, and `draw_help_popup` on `?`.
///
/// ```text
/// Selection screen (draw_game_selection)
/// ┌──────────┬───────────────────────────────────┐
/// │ Variants │                                   │
/// │          │                                   │
/// │          │                                   │
/// │          │                                   │
/// │          │                                   │
/// │          │           Board Preview           │
/// │          │                                   │
/// │          │                                   │
/// │          │                                   │
/// │          │                                   │
/// │          │                                   │
/// └──────────┴───────────────────────────────────┘
///
/// Game tab (draw_game_tab)
/// ┌────────────────────────────────┬─────────────┐
/// │ Tab Bar                        │ Verbosity   │
/// ├────────────────────────┬───────┴─────────────┤
/// │                        │ Details             │
/// │                        │                     │
/// │       Board View       ├─────────────────────┤
/// │                        │ FEN                 │
/// │                        │                     │
/// ├────────────────────────┴─────────────────────┤
/// │ Game Log                                     │
/// ├────────────────────────────────┬─────────────┤
/// │ Input Bar                      │ Threads     │
/// └────────────────────────────────┴─────────────┘
///
/// Overview tab (draw_overview_tab)
/// ┌────────────────────────────────┬─────────────┐
/// │ Tab Bar                        │ Verbosity   │
/// ├────────────────┬───────────────┴─────────────┤
/// │ Configs        │ Tables                      │
/// │                ├─────────────────────────────┤
/// │                │                             │
/// ├────────────────┤         Board View          │
/// │ Pieces         │                             │
/// │                ├─────────────────────────────┤
/// │                │ Piece Details               │
/// ├────────────────┴───────────────┬─────────────┤
/// │ Input Bar                      │ Threads     │
/// └────────────────────────────────┴─────────────┘
///
/// Playground tab (draw_playground_tab)
/// ┌────────────────────────────────┬─────────────┐
/// │ Tab Bar                        │ Verbosity   │
/// ├─────────────────────────────┬──┴─────────────┤
/// │                             │ Pieces         │
/// │                             │                │
/// │         Board View          │                │
/// │                             │                │
/// │                             │                │
/// ├─────────────────────────────┴────────────────┤
/// │ Game Log                                     │
/// ├────────────────────────────────┬─────────────┤
/// │ Input Bar                      │ Threads     │
/// └────────────────────────────────┴─────────────┘
/// ```
///
/// - draw_game_selection : variant list and preview, sorted, no example
/// - draw_tabs           : tab bar and the five verbosity levels
/// - draw_input          : command line and the thread count window
/// - draw_help_bar       : the current mode and its two main keys
/// - draw_help_popup     : help overlay, keys page and commands page
/// - draw_game_tab       : board, moves, details, FEN and log
/// - draw_overview_tab   : variant config and derived piece data
/// - draw_playground_tab : the moves of one piece on an empty board
/// - render              : the layout, calls the other functions
///
/// draw_game_selection .. draw_playground_tab
///
///   Params:
///
///     frame: &mut Frame<'_>
///     ratatui frame to draw into
///
///     area: Rect
///     region of the frame
///
///     app: &Tui / &mut Tui
///     interface state, mutable only to update scroll offsets or picks
///
/// render
///
///   Params:
///   - frame: &mut Frame<'_> -> ratatui frame to draw into
///   - app  : &mut Tui       -> interface state
///
/// draw_game_selection
///
///   Return:
///   Option<Arc<Mutex<State>>> -> the chosen game, or None
///
/// Notes:
/// The other functions return `()`. The selection writes the highlighted
/// name into the command line, so enter loads it. The thread window shows
/// three values, from one to the hardware count, maximum eight.
///
fn draw_game_selection(
    frame: &mut Frame<'_>, area: Rect, app: &mut Tui
) -> Option<Arc<Mutex<State>>> {

    let layout = split_area!(Horizontal, area,
        [Constraint::Percentage(25), Constraint::Fill(1)]);

    let mut config_files: Vec<String> = EMBEDDED_CONFIGS
        .files()
        .filter_map(|f| {
            let name = f.path().file_name()?.to_str()?;
            if name.starts_with("example") {
                return None;
            }
            Some(name.to_string())
        })
        .collect();
    config_files.sort();

    let previews = config_files
        .iter()
        .map(|name| parse_config_preview(name))
        .collect::<Vec<(String, String)>>();

    let mut selected = app.scroll_map.get(&PICKER_SCROLL_KEY)
        .copied()
        .unwrap_or_else(
            || {
                panic!(
                    "Scroll value missing for game selection screen"
                )
            }
        );

    let files_list = List::new(
        previews
            .iter()
            .map(|(title, _)| title.to_string())
            .collect::<Vec<_>>()
    )
    .highlight_style(
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD)
    )
    .block(padded_block!());

    if !config_files.is_empty() {
        if selected >= config_files.len() as u16 {
            selected = config_files.len() as u16 - 1;
            app.scroll_map.insert(PICKER_SCROLL_KEY, selected);
        }
    } else {
        selected = 0;
        app.scroll_map.insert(PICKER_SCROLL_KEY, 0);
    }

    let board_preview = previews.get(selected as usize)
        .map(|(_, preview)| preview)
        .unwrap_or_else(|| panic!("No embedded config files found"));
    let preview_paragraph = Paragraph::new(board_preview.clone())
        .alignment(Alignment::Center);

    app.input = config_files[selected as usize].clone();

    let mut list_state = ListState::default();
    list_state.select(Some(selected as usize));

    let width = board_preview
        .lines()
        .map(|l| l.chars().count()).max().unwrap_or(0) as u16;
    let height = board_preview.lines().count() as u16;

    frame.render_stateful_widget(files_list, layout[0], &mut list_state);
    frame.render_widget(preview_paragraph, layout[1].centered(
        Constraint::Length(width + 2),
        Constraint::Length(height + 2)
    ));

    None
}

fn draw_tabs(frame: &mut Frame<'_>, area: Rect, app: &Tui) {
    let level = configured_verbosity_level();

    let tab_layout = split_area!(Horizontal, area,
        [Constraint::Min(0), Constraint::Length(21)]);

    let tab_titles: Vec<Line> = TAB_TITLES
        .iter()
        .map(|title| {
            Line::raw(*title)
        })
        .collect();

    let tabs = Tabs::new(tab_titles)
        .block(Block::default().borders(Borders::ALL))
        .select(peel_help(app.tab).0)
        .style(Style::default().fg(Color::Gray))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );

    let log_labels: Vec<Line> = ["1", "2", "3", "4", "5"]
        .iter()
        .map(|label| Line::raw(*label))
        .collect();
    let log_tabs = Tabs::new(log_labels)
        .block(Block::default().borders(Borders::ALL))
        .select(if level == 0 { None } else { Some(level as usize - 1) })
        .style(Style::default().fg(Color::Gray))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );

    frame.render_widget(tabs, tab_layout[0]);
    frame.render_widget(log_tabs, tab_layout[1]);
}

fn draw_input(frame: &mut Frame<'_>, area: Rect, app: &Tui) {
    let input_layout = split_area!(Horizontal, area,
        [Constraint::Min(0), Constraint::Length(13)]);
    let prompt = if app.mode == TUI_INPUT_MODE {
        Line::from(vec![
            Span::styled(" $> ", Style::default().fg(Color::Yellow)),
            Span::raw(&app.input),
        ])
    } else {
        Line::from(vec![
            Span::styled(" $> ", Style::default().fg(Color::DarkGray)),
            Span::styled(&app.input, Style::default().fg(Color::DarkGray)),
        ])
    };

    let command_block = Block::default().borders(Borders::ALL);

    let command = Paragraph::new(prompt).block(command_block);

    let max_threads = thread::available_parallelism()
        .map(|n| n.get())
        .unwrap_or(1)
        .min(8);

    let thread_labels: Vec<Line> = if app.threads == 1 {
        [1, 2, 3]
    } else if app.threads == max_threads {
        [max_threads - 2, max_threads - 1, max_threads]
    } else {
        [app.threads - 1, app.threads, app.threads + 1]
    }.iter().map(|&n| Line::raw(n.to_string())).collect();
    let thread_tabs = Tabs::new(thread_labels)
        .block(Block::default().borders(Borders::ALL))
        .select(if app.threads == 1 {
            Some(0)
        } else if app.threads == max_threads {
            Some(2)
        } else {
            Some(1)
        })
        .style(Style::default().fg(Color::Gray))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );

    frame.render_widget(command, input_layout[0]);
    frame.render_widget(thread_tabs, input_layout[1]);
}

fn draw_help_bar(frame: &mut Frame<'_>, area: Rect, app: &Tui) {
    let help_line = if app.mode == TUI_INPUT_MODE {
        Line::from(vec![
            Span::styled("[INPUT] ", Style::default().fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)),
            Span::raw("<Enter> Run | <Esc> Normal")
                .style(Style::default().fg(Color::Gray)),
        ])
    } else {
        Line::from(vec![
            Span::styled("[NORMAL] ", Style::default().fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)),
            Span::raw("<?> Help | <q> Quit")
                .style(Style::default().fg(Color::Gray)),
        ])
    };

    frame.render_widget(help_line, area);
}

fn draw_help_popup(frame: &mut Frame<'_>, area: Rect, app: &mut Tui) {

    let (main_tab, help_tab) = peel_help(app.tab);
    let scroll   = *app.scroll_map.get(&HELP_SCROLL_KEY).unwrap_or(&0);

    let normal_mode_rows = [
        ("<i>", "Enter input mode"),
        ("<?>", "Toggle help popup"),
        ("<Tab>", "Switch tabs"),
        ("<q>", "Quit TUI"),
        ("<←/→>", "Change focus"),
        ("<j/k>", "Scroll up/down"),
        ("<g/G>", "Scroll to top/bottom"),
        ("<n>", "Change variant"),
        ("<{/}>", "Increase/decrease log verbosity"),
        ("<]/[>", "Increase/decrease thread count"),
    ];

    let input_mode_rows = [
        ("<Enter>", "Execute command"),
        ("<Esc>", "Enter normal mode"),
    ];

    let both_rows: &[(&str, &str)] = &[
        ("reset", "Reset the board to its initial state"),
        ("protocol [protocol]", "Change engine protocol (notation)")
    ];

    let playground_rows: &[(&str, &str)] = &[
        (
            "add <piece> <square>",
            "Add a piece to the playground board at the given square"
        ),
        (
            "del <square>",
            "Remove any piece from the playground board at the given square"
        ),
    ];

    let game_rows: &[(&str, &str)] = &[
        (
            "abort",
            "Interrupt any running operation immediately"
        ),
        (
            "undo",
            "Undo the last move played on the board"
        ),
        (
            "ls",
            "List all legal moves available in the current position"
        ),
        (
            "fen <fen>",
            "Load a specific game state from a given FEN string"
        ),
        (
            "search [depth]",
            "Search the current position up to the specified depth"
        ),
        (
            "play <depth> <time>",
            "Automatically play the game with specified depth and time limit"
        ),
        (
            "perft <depth> [branch]",
            "Run a perft benchmark to verify move generation accuracy"
        ),
        (
            "move <move>",
            "Play a valid move using Cheesy Move Notation (CMN)"
        ),
        (
            "datagen <games> <movetime>",
            "Self-play games to build a Texel tuning dataset for the variant"
        ),
        (
            "tune <epochs> [rate]",
            "Texel-tune the evaluation from the dataset by gradient descent"
        ),
        (
            "sprt <binA> <binB> <movetime | base+inc> [games] [h0] [h1]",
            "Run an SPRT match between two engine binaries to test a patch"
        ),
    ];

    let mut lines = vec![];

    let help_tabs = Tabs::new(vec!["Keybinds", "Commands"])
        .select(help_tab)
        .style(Style::default().fg(Color::Gray))
        .padding_left("")
        .divider(" ")
        .highlight_style(
            Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
        );

    let popup_area = area
        .centered(
            Constraint::Length(70),
            Constraint::Length(41)
        );

    let block = padded_block!(style Style::default().fg(Color::Yellow))
        .style(Style::default().fg(Color::White));

    frame.render_widget(Clear, popup_area);
    let popup_inner = block.inner(popup_area);
    frame.render_widget(block, popup_area);

    let layout = split_area!(Vertical, popup_inner,
        [Constraint::Length(2), Constraint::Min(0)]);

    frame.render_widget(help_tabs, layout[0]);

    let content_layout = split_area!(Vertical, layout[1],
        if help_tab == 0 {
            [Constraint::Min(0), Constraint::Min(0),
                Constraint::Length(2)]
        } else {
            [Constraint::Min(0), Constraint::Max(0),
                Constraint::Length(2)]
        });

    let content_height = content_layout[0].height as usize;
    let total_lines = if help_tab == 0 {
        1 + normal_mode_rows.len() + 1 + 1 + input_mode_rows.len() + 2
    } else {
        1 + both_rows.len() * 3 +
        1 + playground_rows.len() * 3 +
        1 + game_rows.len() * 3 + 2
    };
    let max_scroll = (total_lines.saturating_sub(content_height)) as u16;
    let scroll = scroll.min(max_scroll);
    app.scroll_map.insert(HELP_SCROLL_KEY, scroll);

    if help_tab == 0 {
        let mut keybind_rows = Vec::new();

        keybind_rows.push(Row::new(vec![
            Cell::from("Normal mode").style(
                Style::default().add_modifier(Modifier::BOLD)
            ),
            Cell::from(""),
        ]));

        keybind_rows.push(Row::new(vec![
            Cell::from("-----------"),
            Cell::from(""),
        ]));

        for (key, desc) in normal_mode_rows {
            keybind_rows.push(Row::new(vec![
                Cell::from(key).style(
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                ),
                Cell::from(desc),
            ]));
        }

        keybind_rows.push(Row::new(vec![
            Cell::from(""),
            Cell::from(""),
        ]));

        keybind_rows.push(Row::new(vec![
            Cell::from("Input mode").style(
                Style::default().add_modifier(Modifier::BOLD)
            ),
            Cell::from(""),
        ]));

        keybind_rows.push(Row::new(vec![
            Cell::from("----------"),
            Cell::from(""),
        ]));

        for (key, desc) in input_mode_rows {
            keybind_rows.push(Row::new(vec![
                Cell::from(key).style(
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                ),
                Cell::from(desc),
            ]));
        }

        let visible_rows: Vec<Row> = keybind_rows
            .into_iter()
            .skip(scroll as usize)
            .collect();

        let keybind_table = Table::new(
            visible_rows,
            [
                Constraint::Percentage(100),
                Constraint::Percentage(100)
            ]
        ).block(
            Block::default().borders(Borders::NONE)
        );

        frame.render_widget(keybind_table, content_layout[0]);
    } else {
        let section_style = Style::default().add_modifier(Modifier::BOLD);
        let cmd_style     = Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD);

        let push_section = |
            lines: &mut Vec<Line>,
            label: &'static str,
            rows: &[(&str, &str)]
        | {
            lines.push(Line::from(
                Span::from(label).style(section_style)
            ));
            lines.push(Line::from(Span::from("-".repeat(label.len()))));
            for &(cmd, desc) in rows {
                lines.push(Line::from(
                    Span::from(format!("  {cmd} ")).style(cmd_style)
                ));
                lines.push(Line::from(
                    Span::from(format!("  {desc}"))
                ));
                lines.push(Line::default());
            }
        };

        push_section(&mut lines, "General commands",       both_rows);
        push_section(&mut lines, "Playground commands", playground_rows);
        push_section(&mut lines, "Game commands",       game_rows);

        let content = Paragraph::new(lines)
            .wrap(Wrap { trim: true })
            .scroll((scroll, 0));
        frame.render_widget(content, content_layout[0]);
    }

    let footer = vec![
        Line::default(),
        Line::from(
            Span::from(
                    if help_tab == 1 {
                        "Press <j/k> to scroll, <?> to close"
                    } else {
                        "Press <?> to close"
                    }
                ).style(Style::default().fg(Color::Gray))
        ),
    ];
    let footer_paragraph = Paragraph::new(footer).alignment(Alignment::Center);

    frame.render_widget(footer_paragraph, content_layout[2]);

    if help_tab == 1 {
        return;
    }

    let guide_frame = content_layout[1]
        .centered(
            Constraint::Length(64),
            Constraint::Length(18)
        );
    let guide_frame_block = Block::bordered();
    frame.render_widget(guide_frame_block, guide_frame);

    let view_guide = content_layout[1]
        .centered(
            Constraint::Length(64),
            Constraint::Length(18)
        );

    let view_layout = split_area!(overlap Vertical, view_guide,
        if main_tab == SENTINEL_TAB {
            [Constraint::Max(0), Constraint::Fill(1),
                Constraint::Max(0)]
        } else {
            [Constraint::Length(3), Constraint::Fill(1),
                Constraint::Length(3)]
        });

    let tab_guide_layout = split_area!(overlap Horizontal, view_layout[0],
        [Constraint::Min(0), Constraint::Length(13)]);
    let tab_guide = guide_label!("Tab Bar");
    let log_guide = guide_label!("Verbosity");

    if main_tab != SENTINEL_TAB {
        frame.render_widget(tab_guide, tab_guide_layout[0]);
        frame.render_widget(log_guide, tab_guide_layout[1]);
    }

    match main_tab {
        0 => {
            let has_moves = app.board_state.as_ref()
                .map(|s| !s.move_history.trim().is_empty())
                .unwrap_or(false);

            let game_log_layout = split_area!(overlap Vertical,
                view_layout[1],
                [Constraint::Fill(1), Constraint::Length(5)]);

            let game_log_guide = guide_label!("Game Log");
            let board_guide = guide_label!(center "Board View");
            let field_guide = guide_label!("Details");
            let fen_guide = guide_label!("FEN");

            frame.render_widget(game_log_guide, game_log_layout[1]);

            if has_moves {
                let game_guide_layout = split_area!(overlap Horizontal,
                    game_log_layout[0],
                    [Constraint::Fill(4), Constraint::Fill(1),
                        Constraint::Fill(2)]);

                let board_center = game_guide_layout[0]
                    .centered_vertically(Constraint::Length(1));

                let moves_guide = guide_label!("Moves");

                let right_layout = split_area!(overlap Vertical,
                    game_guide_layout[2],
                    [Constraint::Min(0), Constraint::Length(3)]);

                frame.render_widget(board_guide, board_center);
                frame.render_widget(
                    moves_guide, game_guide_layout[1]
                );
                frame.render_widget(field_guide, right_layout[0]);
                frame.render_widget(fen_guide, right_layout[1]);
            } else {
                let game_guide_layout = split_area!(overlap Horizontal,
                    game_log_layout[0],
                    [Constraint::Fill(5), Constraint::Fill(2)]);

                let board_center = game_guide_layout[0]
                    .centered_vertically(Constraint::Length(1));

                let right_layout = split_area!(overlap Vertical,
                    game_guide_layout[1],
                    [Constraint::Min(0), Constraint::Length(3)]);

                frame.render_widget(board_guide, board_center);
                frame.render_widget(field_guide, right_layout[0]);
                frame.render_widget(fen_guide, right_layout[1]);
            }

        },
        1 => {
            let chunks_layout = split_area!(overlap Horizontal,
                view_layout[1],
                [Constraint::Percentage(25), Constraint::Fill(1)]);

            let left_layout = split_area!(overlap Vertical,
                chunks_layout[0],
                [Constraint::Percentage(40), Constraint::Fill(1)]);

            let field_guide = guide_label!("Configs");
            let piece_guide = guide_label!("Pieces");

            let right_layout = split_area!(overlap Vertical,
                chunks_layout[1],
                [Constraint::Length(3), Constraint::Min(0),
                    Constraint::Length(3)]);

            let tables_guide = guide_label!("Tables");
            let board_guide = guide_label!(center "Board View");
            let board_center = right_layout[1]
                .centered_vertically(Constraint::Length(1));

            let piece_detail_guide = guide_label!("Piece Details");

            frame.render_widget(field_guide, left_layout[0]);
            frame.render_widget(piece_guide, left_layout[1]);
            frame.render_widget(tables_guide, right_layout[0]);
            frame.render_widget(board_guide, board_center);
            frame.render_widget(piece_detail_guide, right_layout[2]);
        },
        2 => {
            let selected = *app.scroll_map.get(&(2, 0)).unwrap_or(&0);
            let piece_selected = selected > 0
                && app.playground_state.as_ref().is_some_and(|arc| {
                    arc.lock().is_ok_and(|st| {
                        st.piece_count
                            .get(selected as usize - 1)
                            .is_some_and(|&count| count > 0)
                    })
                });

            let pg_log_layout = split_area!(overlap Vertical,
                view_layout[1],
                [Constraint::Fill(1), Constraint::Length(5)]);

            let log_guide = guide_label!("Game Log");

            let chunks_layout = split_area!(overlap Horizontal,
                pg_log_layout[0],
                [Constraint::Fill(5), Constraint::Fill(2)]);

            let piece_list_guide = guide_label!("Pieces");

            let board_guide = guide_label!(center "Board View");
            let board_center = chunks_layout[0]
                .centered_vertically(Constraint::Length(1));

            frame.render_widget(log_guide, pg_log_layout[1]);
            frame.render_widget(board_guide, board_center);

            if piece_selected {
                let left_layout = split_area!(overlap Horizontal,
                    chunks_layout[1],
                    [Constraint::Percentage(50), Constraint::Fill(1)]);

                let move_list_guide = guide_label!("Moves");

                frame.render_widget(piece_list_guide, left_layout[0]);
                frame.render_widget(move_list_guide, left_layout[1]);
            } else {
                frame.render_widget(
                    piece_list_guide, chunks_layout[1]
                );
            }
        },
        SENTINEL_TAB => {
            let selection_layout = split_area!(overlap Horizontal,
                view_layout[1],
                [Constraint::Percentage(25), Constraint::Fill(1)]);

            let selection_guide = guide_label!("Variants");

            let board_guide = guide_label!(center "Board View");
            let board_center = selection_layout[1]
                .centered_vertically(Constraint::Length(1));

            frame.render_widget(selection_guide, selection_layout[0]);
            frame.render_widget(board_guide, board_center);
        },
        _ => {}
    }

    let input_guide_layout = split_area!(overlap Horizontal, view_layout[2],
        [Constraint::Min(0), Constraint::Length(11)]);

    let command_guide = guide_label!("Input Bar");
    let threads_guide = guide_label!("Threads");

    if main_tab != SENTINEL_TAB {
        frame.render_widget(command_guide, input_guide_layout[0]);
        frame.render_widget(threads_guide, input_guide_layout[1]);
    }
}

fn draw_game_tab(frame: &mut Frame<'_>, area: Rect, app: &mut Tui) {

    let (main_tab, _) = peel_help(app.tab);

    let board = app.board_state.as_ref()
        .map(|state| state.board.clone())
        .unwrap_or_else(|| "Loading...".to_string());

    let main_layout = split_area!(Vertical, area,
        [Constraint::Percentage(75), Constraint::Percentage(25)]);

    let top_rect = main_layout[0];

    let has_moves = app.board_state.as_ref()
        .map(|s| !s.move_history.trim().is_empty())
        .unwrap_or(false);

    if !has_moves && app.focus == TAB_FOCUS_MOVES {
        app.focus = TAB_FOCUS_FEN;
    }

    let board_area;
    let moves_area;
    let details_area;
    let fen_area;

    if has_moves {
        let top_layout = split_area!(Horizontal, top_rect,
            [Constraint::Fill(4), Constraint::Fill(1),
                Constraint::Fill(2)]);
        board_area = top_layout[0].centered(
            Constraint::Length(
                board
                .lines()
                .map(|l| l.chars().count()).max().unwrap_or(0) as u16
            ),
            Constraint::Length(board.lines().count() as u16)
        );
        moves_area = top_layout[1];
        let right_most_rect = split_area!(overlap Vertical, top_layout[2],
            [Constraint::Min(0), Constraint::Length(3)]);
        details_area = right_most_rect[0];
        fen_area = right_most_rect[1];
    } else {
        let top_layout = split_area!(Horizontal, top_rect,
            [Constraint::Fill(5), Constraint::Fill(2)]);
        board_area = top_layout[0].centered(
            Constraint::Length(
                board
                .lines()
                .map(|l| l.chars().count()).max().unwrap_or(0) as u16
            ),
            Constraint::Length(board.lines().count() as u16)
        );
        moves_area = Rect::default();
        let right_most_rect = split_area!(overlap Vertical, top_layout[1],
            [Constraint::Min(0), Constraint::Length(3)]);
        details_area = right_most_rect[0];
        fen_area = right_most_rect[1];
    }

    let board_block = Block::default()
        .borders(Borders::NONE);
    let mut moves_block = padded_block!();
    let details_block = padded_block!(merge);
    let mut fen_block = padded_block!(merge);
    let mut logs_block = padded_block!();

    let logs_area = main_layout[1];

    match app.focus {
        TAB_FOCUS_MOVES if app.mode == TUI_NORMAL_MODE =>
            moves_block = moves_block.border_style(
                Style::default().fg(Color::Yellow)
            ),
        TAB_FOCUS_FEN if app.mode == TUI_NORMAL_MODE =>
            fen_block = fen_block.border_style(
                Style::default().fg(Color::Yellow)
            ),
        TAB_FOCUS_LOGS if app.mode == TUI_NORMAL_MODE =>
            logs_block = logs_block.border_style(
                Style::default().fg(Color::Yellow)
            ),
        _ => {}
    }

    let detail_columns = [
        Constraint::Fill(1),
        Constraint::Fill(2),
    ];

    if app.focus == TAB_FOCUS_FEN {
        Clipboard::new().unwrap_or_else(|e| {
            panic!("Failed to initialize clipboard: {e}")
        }).set_text(app.board_state.as_ref()
            .map(|state| state.fen.clone())
            .unwrap_or_else(|| "Loading...".to_string())
        ).unwrap_or_else(|e| {
            panic!("Failed to set clipboard text: {e}")
        });
    }

    let log_lines = visible_log_lines(
        logs_block.inner(logs_area).height,                                     /* rows left once the border is drawn */
        &mut app.scroll_map, (main_tab, TAB_FOCUS_LOGS)
    );

    let board_paragraph = Paragraph::new(board)
        .alignment(Alignment::Center)
        .block(board_block);
    let mut moves_paragraph = Paragraph::new(app.board_state.as_ref()
        .map(|state| state.move_history.clone())
        .unwrap_or_else(|| "Loading...".to_string())
    ).block(moves_block);
    let details_table = Table::new(
        app.board_state
            .as_ref()
            .map(|state| state.details.clone())
            .unwrap_or_else(
                || vec![["Loading...".to_string(), "".to_string()]]
            )
            .into_iter()
            .map(|row| {
                Row::new(row.into_iter().map(Span::from).collect::<Vec<_>>())
            }).collect::<Vec<_>>(),
        detail_columns
    ).header(
        Row::new(vec!["Parameter", "Value"])
            .style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
                )
    )
    .block(details_block);
    let mut fen_paragraph = Paragraph::new(app.board_state.as_ref()
        .map(|state| state.fen.clone())
        .unwrap_or_else(|| "Loading...".to_string())
    ).block(fen_block);
    let logs_paragraph = Paragraph::new(Text::from(log_lines))                  /* already windowed, so no scrolling  */
        .block(logs_block);

    let max_moves_scroll = (moves_paragraph.line_count(moves_area.width) as u16)
        .saturating_sub(moves_area.height);
    let max_fen_scroll = (fen_paragraph.line_width() as u16)
        .saturating_sub(fen_area.width);

    let current_moves_scroll = app.scroll_map.get(&(main_tab, TAB_FOCUS_MOVES))
        .copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus {}",
                main_tab, TAB_FOCUS_MOVES
            )
        }
    );

    let current_fen_scroll = app.scroll_map.get(&(main_tab, TAB_FOCUS_FEN))
        .copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus {}",
                main_tab, TAB_FOCUS_FEN
            )
        }
    );

    let final_moves_scroll = clamp_scroll(
        current_moves_scroll, max_moves_scroll,
        &mut app.scroll_map, (main_tab, TAB_FOCUS_MOVES)
    );
    let final_fen_scroll = clamp_scroll(
        current_fen_scroll, max_fen_scroll,
        &mut app.scroll_map, (main_tab, TAB_FOCUS_FEN)
    );

    moves_paragraph = moves_paragraph.scroll((final_moves_scroll, 0));
    fen_paragraph = fen_paragraph.scroll((0, final_fen_scroll));

    frame.render_widget(board_paragraph, board_area);
    if has_moves {
        frame.render_widget(moves_paragraph, moves_area);
    }
    frame.render_widget(details_table, details_area);
    frame.render_widget(fen_paragraph, fen_area);
    frame.render_widget(logs_paragraph, logs_area);
}

fn draw_overview_tab(frame: &mut Frame<'_>, area: Rect, app: &mut Tui) {

    let default_state = OverviewState {
        configs: vec![(
            "Loading...".to_string(),
            "".to_string(),
            1
        )],
        pieces: Vec::new(),
    };

    let chunks = split_area!(Horizontal, area,
        [Constraint::Percentage(25), Constraint::Fill(1)]);

    let left_chunks = split_area!(Vertical, chunks[0],
        [Constraint::Percentage(40), Constraint::Percentage(60)]);

    let configs_table = Table::new(
        app.overview_state.as_ref().unwrap_or(
            &default_state
        ).configs.iter().map(|row| {
            let title = &row.0;
            let value = &row.1;
            let height = row.2;

            Row::new(
                vec![
                    Cell::from(title.clone()),
                    Cell::from(value.clone())
                ]
            ).height(height)
        }).collect::<Vec<_>>(),
        [Constraint::Percentage(50), Constraint::Percentage(50)]
    ).header(
        Row::new(vec!["Parameter", "Value"])
            .style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
                )
    )
    .block(
        padded_block!()
    );

    let pieces = &app.overview_state.as_ref().unwrap_or(
        &default_state
    ).pieces;
    let items: Vec<ListItem> = pieces.iter()
        .map(|p| ListItem::new(p.name.clone()))
        .collect();

    let mut selected = *app.scroll_map.get(&(1, 0)).unwrap_or(&0);
    if !pieces.is_empty() {
        if selected >= pieces.len() as u16 {
            selected = pieces.len() as u16 - 1;
            app.scroll_map.insert((1, 0), selected);
        }
    } else {
        selected = 0;
        app.scroll_map.insert((1, 0), selected);
    }

    let mut list_state = ListState::default();
    list_state.select(Some(selected as usize));

    let style = if app.focus == 0 && app.mode == TUI_NORMAL_MODE {
        Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
    } else {
        Style::default()
    };

    let list = if items.is_empty() {
            List::new(vec![
                Span::from("Loading...")
            ]).block(
                padded_block!(style style)
            )
    } else {
        List::new(items)
            .block(
                padded_block!(style style)
            )
            .highlight_style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            )
    };

    if let Some(piece) = pieces.get(selected as usize) {
        let right_chunks = split_area!(Vertical, chunks[1],
            [Constraint::Min(0), Constraint::Length(6)]);

        let info_ref = piece.info.as_ref();

        let mut info_rows = Vec::new();
        info_rows.push(Row::new(vec![
            Span::from("Char".to_string())
                .style(Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
                ),
            info_ref
                .map(|i| i.char_str.clone()).unwrap_or_else(|| "-".to_string())
                .into(),
        ]));
        info_rows.push(Row::new(vec![
            Span::from("Values (Opening / Endgame)".to_string())
                .style(Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
                ),
            info_ref.map(|i| format!("{} / {}", i.op_val, i.eg_val))
                .unwrap_or_else(|| "-".to_string()).into(),
        ]));
        info_rows.push(Row::new(vec![
            Span::from("Roles".to_string())
                .style(Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
                ),
            info_ref
                .map(|i| i.roles.clone())
                .unwrap_or_else(|| "-".to_string())
                .into(),
        ]));
        info_rows.push(Row::new(vec![
            Span::from("Promotions".to_string())
                .style(Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
                ),
            info_ref
                .map(|i| i.promotions.clone())
                .unwrap_or_else(|| "-".to_string())
                .into(),
        ]));

        let info_table = Table::new(
            info_rows,
            [
                Constraint::Percentage(20),
                Constraint::Percentage(80),
            ]
        )
        .block(
            padded_block!()
        );

        frame.render_widget(info_table, right_chunks[1]);

        let mut available_tables = Vec::new();
        if let Some(info) = info_ref {
            available_tables.push(("Opening PST", &info.op_pst));
            available_tables.push(("Endgame PST", &info.eg_pst));

            if let Some(fz) = &info.forbidden_zones {
                available_tables.push(("Forbidden Zones", fz));
            }
            if let Some(mp) = &info.mandatory_promotions {
                available_tables.push(("Mandatory Promotions", mp));
            }
            if let Some(op) = &info.optional_promotions {
                available_tables.push(("Optional Promotions", op));
            }
        }

        if !available_tables.is_empty() {
            let n_tables = available_tables.len() as u16;
            let mut select = *app.scroll_map.get(&(1, 1)).unwrap_or(&0);
            if select >= n_tables {
                select = n_tables - 1;
                app.scroll_map.insert((1, 1), select);
            }

            let tab_titles: Vec<Line> = available_tables.iter()
                .map(|(t, _)| Line::from(*t))
                .collect();

            let tabs_style = if app.focus == 1 && app.mode == TUI_NORMAL_MODE {
                Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
            } else {
                Style::default().fg(Color::Gray)
            };

            let tabs = Tabs::new(tab_titles)
                .block(
                    Block::default()
                        .borders(Borders::ALL)
                        .border_style(tabs_style)
                )
                .select((n_tables - select) as usize - 1)
                .highlight_style(
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD),
                );

            let table_view_layout = split_area!(Vertical, right_chunks[0],
                [Constraint::Length(3), Constraint::Min(0)]);

            frame.render_widget(tabs, table_view_layout[0]);

            let (_, table) = available_tables[(n_tables - select) as usize - 1];

            let table_paragraph = Paragraph::new(table.clone())
                .block(Block::default().borders(Borders::NONE));

            let width = table
                .lines()
                .map(|l| l.chars().count()).max().unwrap_or(0) as u16;
            let height = table.lines().count() as u16;

            frame.render_widget(table_paragraph, table_view_layout[1].centered(
                Constraint::Length(width), Constraint::Length(height)
            ));
        }
    }

    frame.render_stateful_widget(list, left_chunks[1], &mut list_state);
    frame.render_widget(configs_table, left_chunks[0]);
}

fn draw_playground_tab(frame: &mut Frame<'_>, area: Rect, app: &mut Tui) {

    let (main_tab, _) = peel_help(app.tab);

    let main_layout = split_area!(Vertical, area,
        [Constraint::Percentage(75), Constraint::Percentage(25)]);

    let layout = split_area!(Horizontal, main_layout[0],
        [Constraint::Fill(5), Constraint::Fill(2)]);

    let logs_area = main_layout[1];

    let state_mutex = app.playground_state.as_mut();
    let mut piece_list_state = ListState::default();
    let mut move_list_state = ListState::default();

    let piece_list;
    let board_str;
    let move_list_opt;
    let width;
    let height;

    if let Some(pg_mutex) = state_mutex {

        let mut state = pg_mutex.lock().unwrap_or_else(|e| {
            panic!("Failed to lock playground state: {e}")
        });

        let pieces: Vec<ListItem> = state.statics.pieces.iter()
            .map(
                |p| ListItem::new(
                    format!(
                        "[{}] {} {}",
                        p.char,
                        if p_color!(p) == WHITE {
                            "White"
                        } else {
                            "Black"
                        },
                        p.name
                    )
                )
                .style(
                    if state.piece_count[p_index!(p) as usize] == 0 {
                        Style::default().fg(Color::DarkGray)
                    } else {
                        Style::default()
                    }
                )
            )
            .collect();

        let mut selected = *app.scroll_map.get(&(2, 0)).unwrap_or(&0);
        if !pieces.is_empty() {
            if selected > pieces.len() as u16 {
                selected = pieces.len() as u16;
                app.scroll_map.insert((2, 0), selected);
            }

            if selected == 0 {
                piece_list_state.select(None);
            } else {
                piece_list_state.select(Some(selected as usize - 1));
            }
        } else {
            piece_list_state.select(None);
        }

        let selected_piece = if selected == 0 {
            None
        } else {
            Some(selected as usize - 1)
        };

        let active_piece = selected_piece
            .filter(|&idx| state.piece_count[idx] > 0);

        if active_piece.is_none() && app.focus == 1 {
            app.focus = 0;
        }

        let piece_style = if app.focus == 0 && app.mode == TUI_NORMAL_MODE {
            Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
        } else {
            Style::default()
        };

        piece_list = List::new(pieces)
            .block(
                padded_block!(style piece_style)
            )
            .highlight_style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            );

        match active_piece {
            Some(piece_idx) => {
                state.playing = p_color!(state.statics.pieces[piece_idx]);
                state.position_hash = hash_position(&state);

                let mut out = Vec::with_capacity(64);
                let mut scratch = Vec::with_capacity(16);

                generate_all_moves_and_drops(
                    &state, &mut out, &mut scratch
                );

                let mut legal_moves = Vec::with_capacity(out.len());

                let temp_state = &mut state.clone();
                for mv in out.iter() {
                    if make_move!(temp_state, mv.clone()) {
                        undo_move!(temp_state);
                        legal_moves.push(mv.clone());
                    }
                }

                let filtered_moves: Vec<&Move> = legal_moves.iter()
                    .filter(|mv| piece!(mv) == piece_idx as u128)
                    .collect();

                let mut selected_move =
                    *app.scroll_map.get(&(2, 1)).unwrap_or(&0);
                if selected_move > filtered_moves.len() as u16 {
                    selected_move = filtered_moves.len() as u16;
                    app.scroll_map.insert((2, 1), selected_move);
                }
                if selected_move == 0 {
                    move_list_state.select(None);
                } else {
                    move_list_state.select(
                        Some(selected_move as usize - 1)
                    );
                }

                let mut move_items =
                    Vec::with_capacity(filtered_moves.len());
                for mv in filtered_moves.iter() {
                    move_items.push(
                        ListItem::new(
                            format_move(mv, &state, app.translator.as_ref())
                        )
                    );
                }

                let move_style =
                    if app.focus == 1 && app.mode == TUI_NORMAL_MODE {
                        Style::default()
                            .fg(Color::Yellow)
                            .add_modifier(Modifier::BOLD)
                    } else {
                        Style::default()
                    };

                move_list_opt = Some(
                    List::new(move_items)
                        .block(
                            padded_block!(style move_style)
                        )
                        .highlight_style(
                            Style::default()
                                .fg(Color::Yellow)
                                .add_modifier(Modifier::BOLD)
                        )
                );

                let piece_char = state.statics.pieces[piece_idx].char;
                let active_moves: Vec<&Move> = if selected_move == 0 {
                    filtered_moves.to_vec()
                } else {
                    filtered_moves
                        .get(selected_move as usize - 1)
                        .copied()
                        .into_iter()
                        .collect()
                };

                let mut piece_board =
                    board!(state.statics.files, state.statics.ranks);
                let mut captr_board =
                    board!(state.statics.files, state.statics.ranks);
                let mut quiet_board =
                    board!(state.statics.files, state.statics.ranks);
                let empty_board =
                    board!(state.statics.files, state.statics.ranks);

                active_moves.iter().for_each(|mv| {
                    set!(piece_board, start!(mv) as u32);
                    if m_capture!(mv) {
                        if move_type!(mv) == SINGLE_CAPTURE_MOVE {
                            set!(
                                captr_board,
                                captured_square!(mv) as u32
                            );
                            set!(quiet_board, end!(mv) as u32);
                        } else if move_type!(mv) == MULTI_CAPTURE_MOVE {
                            m_captures!(mv).iter().for_each(|&cap| {
                                set!(
                                    captr_board,
                                    multi_move_captured_square!(cap) as u32
                                );
                            });
                            set!(quiet_board, end!(mv) as u32);
                        }
                    } else {
                        set!(quiet_board, end!(mv) as u32);
                    }
                });

                let piece_diagram =
                    format_board(&piece_board, Some(piece_char));
                let captr_diagram =
                    format_board(&captr_board, Some('✻'));
                let quiet_diagram =
                    format_board(&quiet_board, Some('◯'));
                let empty_diagram = format_board(&empty_board, None);

                board_str = [
                    piece_diagram,
                    captr_diagram,
                    quiet_diagram,
                ]
                .iter()
                .fold(empty_diagram, |acc, s| {
                    combine_board_strings(&acc, s)
                });
            },
            None => {
                move_list_opt = None;
                board_str = format_game_state(&state);
            },
        }

        width = board_str
            .lines()
            .map(|l| l.chars().count()).max().unwrap_or(0) as u16;
        height = board_str.lines().count() as u16;

    } else {
        piece_list = List::new(vec![
            Span::from("Loading...")
        ]).block(
            padded_block!()
        );
        board_str = "Loading...".to_string();
        move_list_opt = None;
        width = board_str.chars().count() as u16;
        height = 1;
    }

    if let Some(move_list) = move_list_opt {
        let left_layout = split_area!(Horizontal, layout[1],
            [Constraint::Percentage(50), Constraint::Percentage(50)]);
        frame.render_stateful_widget(
            piece_list, left_layout[0], &mut piece_list_state
        );
        frame.render_stateful_widget(
            move_list, left_layout[1], &mut move_list_state
        );
    } else {
        frame.render_stateful_widget(
            piece_list, layout[1], &mut piece_list_state
        );
    }

    frame.render_widget(
        Paragraph::new(board_str),
        layout[0].centered(
            Constraint::Length(width), Constraint::Length(height)
        )
    );

    let mut logs_block = padded_block!();
    if app.focus == TAB_FOCUS_LOGS && app.mode == TUI_NORMAL_MODE {
        logs_block = logs_block.border_style(
            Style::default().fg(Color::Yellow)
        );
    }

    let log_lines = visible_log_lines(
        logs_block.inner(logs_area).height,                                     /* rows left once the border is drawn */
        &mut app.scroll_map, (main_tab, TAB_FOCUS_LOGS)
    );

    let logs_paragraph = Paragraph::new(Text::from(log_lines))                  /* already windowed, so no scrolling  */
        .block(logs_block);

    frame.render_widget(logs_paragraph, logs_area);
}

fn render(frame: &mut Frame<'_>, app: &mut Tui) {
    let root = frame.area();

    if !app.locked && app.game_state.is_none() {
        let chunks = split_area!(Vertical, root,
            [Constraint::Min(0), Constraint::Length(1)]);
        app.game_state = draw_game_selection(frame, chunks[0], app);
        draw_help_bar(frame, chunks[1], app);
    } else {
        let chunks = split_area!(Vertical, root,
            [Constraint::Length(3), Constraint::Min(0),
                Constraint::Length(3), Constraint::Length(1)]);

        draw_tabs(frame, chunks[0], app);

        match peel_help(app.tab).0 {
            0 => draw_game_tab(frame, chunks[1], app),
            1 => draw_overview_tab(frame, chunks[1], app),
            2 => draw_playground_tab(frame, chunks[1], app),
            _ => {},
        }

        draw_input(frame, chunks[2], app);
        draw_help_bar(frame, chunks[3], app);
    }

    if help_open!(app.tab) {
        draw_help_popup(frame, root, app);
    }
}

/*----------------------------------------------------------------------------*\
                                    COMMANDS
\*----------------------------------------------------------------------------*/

/// switch_protocol
///
/// Reads a `protocol [name]` line and sends a `SwitchDict` event. Without a
/// name, there is no translator, so the engine notation applies. The two
/// dispatchers use it, because the interface keeps the translator.
///
/// Params:
/// - command: &str           -> the raw input line
/// - variant: Option<String> -> loaded variant name, for the dictionary
///
/// Notes:
/// The line is an error before a variant is loaded, or when no dictionary
/// has the name.
///
fn switch_protocol(command: &str, variant: Option<String>) {
    let parts: Vec<_> = command.split_whitespace().collect();

    let new_dict = if let Some(name) = parts.get(1) {
        let Some(ref loaded) = variant else {
            log_2!("No variant loaded");
            return;
        };

        let Some(translator) = Translator::find(loaded, name) else {
            log_2!("Unknown protocol: {}", name);
            return;
        };

        Some(translator)
    } else {
        None
    };

    emit(EngineEvent::SwitchDict(new_dict));
}

/// execute_playground_command
///
/// Runs one line of the playground tab.
///
/// - `reset`                : empty board, first piece
/// - `add <piece> <square>` : put a piece on the square, by its letter
/// - `del <square>`         : remove the piece on the square
/// - `protocol [name]`      : change the notation
///
/// The letter case gives the colour. Another line gets a usage error. An
/// empty line does nothing.
///
/// Params:
/// - command   : &str           -> the raw input line
/// - playground: &mut State     -> the scratch position, already locked
/// - variant   : Option<String> -> loaded variant name
///
/// Notes:
/// The playground does not read the game state. Thus it works while a
/// search, a match or a tuning run uses the game.
///
fn execute_playground_command(
    command: &str,
    playground: &mut State,
    variant: Option<String>,
) {
    let trimmed = command.trim();

    match trimmed.split_whitespace().next().unwrap_or("") {
        "reset" => {
            init_playground(playground, 0);

            emit(EngineEvent::PlaygroundUpdate(
                Box::new(playground.clone()),
            ));
        }
        "add" => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 3 {
                log_2!("Usage: add <piece> <square>");
                return;
            }

            let piece_character = parts[1].chars().next();
            let piece_index = piece_character.and_then(|character| {
                playground.statics.pieces
                    .iter()
                    .position(|piece| piece.char == character)
                    .map(|index| index as PieceIndex)
            });
            let target_square = parse_square(parts[2], playground);

            match (piece_index, target_square) {
                (Some(index), Some(square)) =>
                    set_playground_piece(playground, index, square),
                _ => {
                    log_2!("Usage: add <piece> <square>");
                    return;
                }
            }

            emit(EngineEvent::PlaygroundUpdate(
                Box::new(playground.clone()),
            ));
        }
        "del" => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: del <square>");
                return;
            }

            let Some(square) = parse_square(parts[1], playground) else {
                log_2!("Usage: del <square>");
                return;
            };

            set_playground_piece(playground, NO_PIECE, square);

            emit(EngineEvent::PlaygroundUpdate(
                Box::new(playground.clone()),
            ));
        }
        "protocol" => switch_protocol(trimmed, variant),
        "" => {}
        _ => log_2!("Invalid command: {}", trimmed),
    }
}

/// execute_command
///
/// Runs one game command line.
///
/// - `undo`                   : undo the last move
/// - `reset`                  : go back to the start position
/// - `ls`                     : list all legal moves, numbered
/// - `see <move>`             : show the exchange result of a capture
/// - `fen <fen>`              : load a position
/// - `search [depth]`         : search the loaded position
/// - `play <depth> <time>`    : self-play from this position
/// - `perft <depth> [branch]` : count the move tree, divided at the root
/// - `move <move>`            : play one move
/// - `protocol [name]`        : change the notation
/// - `datagen <games> <time>` : write self-play games for tuning
/// - `tune <epochs> [rate]`   : fit the evaluation to the data
/// - `sprt <a> <b> <control>` : match between two builds
///
/// `sprt` also accepts a game count and two Elo bounds. The defaults are
/// 2000 games, 0 and 5 Elo.
///
/// Params:
/// - command   : &str                -> the raw input line
/// - state     : &mut State          -> the game state
/// - variant   : Option<String>      -> variant name, for the long jobs
/// - dict      : Option<&Translator> -> translator for the move text
/// - ttable    : Arc<TTable>         -> shared main table
/// - qtable    : Arc<QTable>         -> shared quiescence table
/// - threads   : usize               -> number of search workers
///
/// Notes:
/// The long jobs run on the worker thread of the line and report through
/// the event sink. The interface keeps drawing, and the command line stays
/// locked until they end. Playground lines go to
/// `execute_playground_command`.
///
fn execute_command(
    command: &str,
    state: &mut State,
    variant: Option<String>,
    dict: Option<&Translator>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    threads: usize,
) {
    let trimmed = command.trim();

    if trimmed.is_empty() {
        return;
    }

    match trimmed {
        "undo" => {
            if state.ply_counter > 0 {
                undo_move!(state);
            } else {
                log_2!("No moves to undo");
            }

            let board_state = BoardState::from_state(state, dict);

            emit(EngineEvent::Board(board_state));
        }
        "reset" => {
            while state.ply_counter > 0 {
                undo_move!(state);

                let board_state = BoardState::from_state(state, dict);

                emit(EngineEvent::Board(board_state));
            }
        }
        "ls" => {
            let mut moves = Vec::with_capacity(64);
            let mut scratch = Vec::with_capacity(16);
            generate_all_moves_and_drops(state, &mut moves, &mut scratch);

            if moves.is_empty() {
                log_2!("No legal moves available");
                return;
            }

            log_2!("Legal moves:");
            for (i, mv) in moves.iter().enumerate() {
                if !make_move!(state, mv.clone()) {
                    continue;
                }

                undo_move!(state);
                log_2!("{:>3} {}", i, format_move(mv, state, dict));
            }
        }
        _ if trimmed.starts_with("see") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: see <move>");
                return;
            }

            let move_text = parts[1];
            let mv = parse_move(move_text, state, dict).unwrap_or_else(|| {
                log_2!("Invalid move: {}", move_text);
                null_move()
            });

            if mv == null_move() {
                return;
            }

            let score = see!(state, &mv);
            log_2!("SEE for {}: {}", format_move(&mv, state, dict), score);
        }
        _ if trimmed.starts_with("fen") => {
            let fen = trimmed[4..].trim();
            state.load_fen(fen, dict);
            log_2!("Loaded FEN");

            let board_state = BoardState::from_state(state, dict);

            emit(EngineEvent::Board(board_state));
        }
        _ if trimmed.starts_with("search") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            let depth = parse_number(&parts, 1, MAX_DEPTH, "depth")
                .unwrap_or_else(|diagnostic| {
                    log_2!("{}", diagnostic);
                    MAX_DEPTH
                });

            let mut info = SearchInfo {
                set_depth: depth, ..Default::default()
            };
            let result = search_position(
                state, Arc::clone(&ttable), Arc::clone(&qtable),
                &mut info, threads, dict
            );
            log_table_stats(&ttable, &qtable);

            if result.best_move == null_move() {
                log_2!("No legal move available");
            }
        }
        _ if trimmed.starts_with("play") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 3 {
                log_2!("Usage: play <depth> <time (s)>");
                return;
            }

            let Ok(depth) = parts[1].parse::<usize>() else {
                log_2!("Invalid depth: {}", parts[1]);
                return;
            };
            if depth == 0 {
                log_2!("Depth must be positive");
                return;
            }

            let Ok(time_limit) = parts[2].parse::<f64>() else {
                log_2!("Invalid time limit: {}", parts[2]);
                return;
            };
            if time_limit < 0.0 {
                log_2!("Time limit cannot be negative");
                return;
            }

            let time_limit_ns =
                (time_limit * 1_000_000_000.0) as u128;
            let result = play_search_game(
                state,
                Arc::clone(&ttable),
                Arc::clone(&qtable),
                depth,
                time_limit_ns,
                threads,
                usize::MAX,
                dict,
                |position, _| {
                    emit(EngineEvent::Board(
                        BoardState::from_state(position, dict)
                    ));
                },
            );

            emit(EngineEvent::Board(BoardState::from_state(state, dict)));
            match result {
                Ok((WHITE_WIN, _)) => log_1!("White wins!"),
                Ok((BLACK_WIN, _)) => log_1!("Black wins!"),
                Ok((DRAW, _)) => log_1!("It's a draw!"),
                Ok(_) => log_2!("Play interrupted"),
                Err(error) => log_2!("{}", error),
            }
        }
        _ if trimmed.starts_with("datagen") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 3 {
                log_2!("Usage: datagen <games> <movetime (ms)>");
                return;
            }

            let Ok(games) = parts[1].parse::<usize>() else {
                log_2!("Invalid games: {}", parts[1]);
                return;
            };

            let Ok(movetime) = parts[2].parse::<u128>() else {
                log_2!("Invalid movetime: {}", parts[2]);
                return;
            };

            let Some(ref variant_name) = variant else {
                log_2!("No variant loaded for datagen");
                return;
            };

            run_datagen(
                state, variant_name, dict,
                Arc::clone(&ttable), Arc::clone(&qtable),
                threads, games, movetime,
            );
        }
        _ if trimmed.starts_with("tune") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() < 2 {
                log_2!("Usage: tune <epochs> [learning rate = 1.0]");
                return;
            }

            let Ok(epochs) = parts[1].parse::<usize>() else {
                log_2!("Invalid epochs: {}", parts[1]);
                return;
            };

            let learning_rate = parse_number(&parts, 2, 1.0, "rate")
                .unwrap_or_else(|diagnostic| {
                    log_2!("{}", diagnostic);
                    1.0
                });

            let Some(ref variant_name) = variant else {
                log_2!("No variant loaded for tune");
                return;
            };

            run_tuning(state, variant_name, epochs, learning_rate);
        }
        _ if trimmed.starts_with("sprt") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() < 4 {
                log_2!(
                    "Usage: sprt <binA> <binB> <movetime (ms) | base+inc> \
                    [games = 2000] [h0 = 0.0] [h1 = 5.0]"
                );
                return;
            }

            let Ok(time_control) =
                parse_sprt_time_control(parts[3])
            else {
                log_2!("Invalid time control: {}", parts[3]);
                return;
            };

            let max_games = parse_number(&parts, 4, 2000usize, "games")
                .unwrap_or_else(|diagnostic| {
                    log_2!("{}", diagnostic);
                    2000
                });
            let h0 = parse_number(&parts, 5, 0.0f64, "h0")
                .unwrap_or_else(|diagnostic| {
                    log_2!("{}", diagnostic);
                    0.0
                });
            let h1 = parse_number(&parts, 6, 5.0f64, "h1")
                .unwrap_or_else(|diagnostic| {
                    log_2!("{}", diagnostic);
                    5.0
                });

            let Some(ref variant_name) = variant else {
                log_2!("No variant loaded for sprt");
                return;
            };

            let settings = SPRTMatch {
                variant: variant_name.clone(),
                engine_a: SPRTEngine {
                    binary: parts[1].to_string(),
                    options: Vec::new(),
                },
                engine_b: SPRTEngine {
                    binary: parts[2].to_string(),
                    options: Vec::new(),
                },
                time_control,
                max_games,
                h0,
                h1,
                concurrency: 1,
                book: Vec::new(),
                book_path: String::new(),
                max_plies: 0,
            };

            run_sprt(state, &settings);
        }
        _ if trimmed.starts_with("perft") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() < 2 {
                log_2!("Usage: perft <depth> [branch]");
                return;
            }

            let Ok(depth) = parts[1].parse::<u8>() else {
                log_2!("Invalid depth: {}", parts[1]);
                return;
            };

            let branch = parse_number(&parts, 2, -1i8, "branch")
                .unwrap_or_else(|diagnostic| {
                    log_2!("{}", diagnostic);
                    -1
                });

            if let Some(ref variant) = variant {
                let perft_name = format!("{}.perft", variant);
                if let Some(content) = EMBEDDED_PERFT
                    .get_file(&perft_name)
                    .and_then(|f| f.contents_utf8())
                {
                    benchmark_perft(
                        state, content, depth, branch, usize::MAX, dict
                    );
                    return;
                }
            }

            benchmark_headless_perft(state, depth, branch, dict);
        }
        _ if trimmed.starts_with("move") => {
            let mv_str = trimmed[5..].trim();
            match parse_move(mv_str, state, dict) {
                None => {
                    log_2!("Invalid move: {}", mv_str);
                }
                Some(mv) => {
                    if make_move!(state, mv) {
                        let board_state =
                            BoardState::from_state(state, dict);
                        emit(EngineEvent::Board(board_state));
                    } else {
                        log_2!("Illegal move: {}", mv_str);
                    }
                }
            }
        }
        _ if trimmed.starts_with("protocol") => {
            switch_protocol(trimmed, variant);
        }
        _ => {
            log_2!("Invalid command: {}", trimmed);
        }
    }
}

/// CommandTarget
///
/// The board of a submitted line, and thus the mutex of its worker. Only
/// the game can be busy, so the input lock applies only to game lines.
///
enum CommandTarget {
    Playground(Arc<Mutex<State>>),                                              /* never gated, never waits           */
    Game(Arc<Mutex<State>>),                                                    /* gated, holds the lock until done   */
}

/*----------------------------------------------------------------------------*\
                                     INPUT
\*----------------------------------------------------------------------------*/

/// handle_key
///
/// Handles one key press. The keyboard mode decides the action:
///
/// - input mode
///   - any key : goes into the line
///   - `Enter` : submits the line
///   - `Esc`   : back to normal mode
/// - normal mode
///   - `i`   : input mode
///   - `Tab` : next tab, or next help page
///   - `←/→` : next pane of this tab
///   - `j/k` : scroll the pane, `g`/`G` to the ends
///   - `n`   : back to the variant list
///   - `?`   : help overlay on and off
///   - `{/}` : log verbosity up and down
///   - `[/]` : search threads up and down
///   - `q`   : quit
///
/// Params:
/// - app  : &mut Tui -> the interface state to change
/// - event: KeyEvent -> the key event
///
/// Return:
/// bool              -> true when the application must exit
///
/// Notes:
/// A submitted line runs on its own thread. A playground line never waits.
/// A game line locks the command line until its worker ends. A game line
/// during a running job stays in the box. `abort` sets the interrupt flag
/// to stop the running job.
///
fn handle_key(app: &mut Tui, event: KeyEvent) -> bool {

    if event.kind != KeyEventKind::Press {
        return false;
    }

    let code = event.code;

    match (app.mode, code) {
        (TUI_INPUT_MODE, KeyCode::Enter) => {

            let (main_tab, _) = peel_help(app.tab);

            if app.input == "abort" {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);

                app.input.clear();
                return false;
            }

            let target = if main_tab == 2 {
                app.playground_state.clone().map(CommandTarget::Playground)
            } else if app.locked {
                log_2!("Command execution in progress, please wait...");
                return false;                                                   /* the line stays for a retry         */
            } else {
                app.game_state.clone().map(CommandTarget::Game)
            };

            let Some(target) = target else {
                log_2!("No board loaded");
                app.input.clear();
                return false;
            };

            if matches!(target, CommandTarget::Game(_)) {
                app.locked = true;
            }

            thread::spawn({
                let command = app.input.clone();
                let variant = app.variant.clone();
                let dict = app.translator.clone();
                let threads = app.threads;

                move || match target {
                    CommandTarget::Playground(arc) => {
                        let mut playground = arc.lock()
                            .unwrap_or_else(|_| {
                                panic!(
                                    concat!(
                                        "Failed to lock playground state ",
                                        "for command execution"
                                    )
                                )
                            });

                        execute_playground_command(
                            &command,
                            &mut playground,
                            variant,
                        );
                    }
                    CommandTarget::Game(arc) => {
                        let mut state = arc.lock()
                            .unwrap_or_else(|_| {
                                panic!(
                                    concat!(
                                        "Failed to lock game state ",
                                        "for command execution"
                                    )
                                )
                            });

                        execute_command(
                            &command,
                            &mut state,
                            variant,
                            dict.as_ref(),
                            Arc::new(TTable::default()),
                            Arc::new(QTable::default()),
                            threads,
                        );

                        emit(EngineEvent::Unlock);
                    }
                }
            });

            app.input.clear();

            false
        },
        (TUI_INPUT_MODE, KeyCode::Esc) => {
            app.mode = TUI_NORMAL_MODE;
            false
        },
        (TUI_INPUT_MODE, KeyCode::Char(c)) => {
            app.input.push(c);
            false
        },
        (TUI_INPUT_MODE, KeyCode::Backspace) => {
            app.input.pop();
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('j')) => {
            if help_open!(app.tab) {
                let s = app.scroll_map
                    .get(&HELP_SCROLL_KEY)
                    .copied()
                    .unwrap_or(0)
                    .saturating_add(1);
                app.scroll_map.insert(HELP_SCROLL_KEY, s);
            } else {
                let scroll = app.scroll_map.get(&(app.tab, app.focus))
                    .copied()
                    .unwrap_or_else(
                        || {
                            panic!(
                                "Scroll value missing for tab {}, \
                                 focus {}",
                                app.tab, app.focus
                            )
                        }
                    ).saturating_add(1);
                app.scroll_map.insert((app.tab, app.focus), scroll);
            }
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('k')) => {
            if help_open!(app.tab) {
                let s = app.scroll_map
                    .get(&HELP_SCROLL_KEY)
                    .copied()
                    .unwrap_or(0)
                    .saturating_sub(1);
                app.scroll_map.insert(HELP_SCROLL_KEY, s);
            } else {
                let scroll = app.scroll_map.get(&(app.tab, app.focus))
                    .copied()
                    .unwrap_or_else(
                        || {
                            panic!(
                                "Scroll value missing for tab {}, \
                                 focus {}",
                                app.tab, app.focus
                            )
                        }
                    ).saturating_sub(1);
                app.scroll_map.insert((app.tab, app.focus), scroll);
            }
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Enter) if app.tab == SENTINEL_TAB => {
            let input_trimmed = app.input.trim();
            let variant = input_trimmed.strip_suffix(".conf")                   /* the picker offers file names       */
                .unwrap_or(input_trimmed)
                .to_string();

            app.variant = Some(variant.clone());
            app.locked = true;
            app.focus = 0;
            app.tab = 0;

            thread::spawn({
                let dict = app.translator.clone();

                app.input.clear();

                move || {
                    match load_variant(&variant) {
                        Ok(state) => {
                            let board_state = BoardState::from_state(
                                &state, dict.as_ref()
                            );

                            emit(EngineEvent::StateInit(
                                Arc::new(Mutex::new(state)),
                            ));

                            emit(EngineEvent::Board(board_state));

                            emit(EngineEvent::Unlock);
                        }
                        Err(diagnostic) => log_2!("{}", diagnostic),
                    }
                }
            });


            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('q')) => true,
        (TUI_NORMAL_MODE, KeyCode::Char('?')) => {
            if help_open!(app.tab) {
                app.tab = peel_help(app.tab).0;
                app.scroll_map.insert(HELP_SCROLL_KEY, 0);
            } else {
                app.tab += TAB_TITLES.len();
                app.scroll_map.insert(HELP_SCROLL_KEY, 0);
            }
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Tab) if help_open!(app.tab) => {
            let (_, help_tab) = peel_help(app.tab);

            if help_tab == 1 {
                app.tab -= TAB_TITLES.len();
            } else {
                app.tab += TAB_TITLES.len();
            }

            app.scroll_map.insert(HELP_SCROLL_KEY, 0);
            false
        },
        (TUI_NORMAL_MODE, _) if help_open!(app.tab) => false,                   /* all other keys blocked in help     */
        (TUI_NORMAL_MODE, _) if app.tab == SENTINEL_TAB => false,               /* everything else in is off          */
        (TUI_NORMAL_MODE, KeyCode::Left) => {
            if app.focus == 0 {
                app.focus = (TAB_FOCUSABLES[app.tab] as usize)
                    .saturating_sub(1);
            } else {
                app.focus = app.focus.saturating_sub(1);
            }
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Right) => {
            app.focus = app.focus.saturating_add(1);

            if app.focus >= TAB_FOCUSABLES[app.tab] as usize {
                app.focus = 0;
            }
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char(']')) => {
            let max_threads = thread::available_parallelism()
                .map(|n| n.get())
                .unwrap_or(1)
                .min(8);
            app.threads = (app.threads + 1).min(max_threads);
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('[')) => {
            app.threads = app.threads.saturating_sub(1).max(1);
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('}')) => {
            inc_verbosity();
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('{')) => {
            dec_verbosity();
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('n')) => {
            app.reset();
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('g')) => {
            let scroll = app.scroll_map.entry((app.tab, app.focus))
                .or_insert(0);
            *scroll = 0;
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('G')) => {
            let scroll = app.scroll_map.entry((app.tab, app.focus))
                .or_insert(0);
            *scroll = u16::MAX;
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('i')) if app.game_state.is_some() => {
            app.mode = TUI_INPUT_MODE;
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Tab) => {
            app.tab = (app.tab + 1) % TAB_TITLES.len();
            app.focus = 0;
            false
        },
        _ => false,
    }
}

/*----------------------------------------------------------------------------*\
                                  ENTRY POINT
\*----------------------------------------------------------------------------*/

/// run_debug_graphics
///
/// Takes the terminal, runs the interface and gives the terminal back.
/// During the run, all engine events go to a channel.
///
/// 1. set raw mode and mouse capture
/// 2. set the sink, so events do not print
/// 3. run the interface loop until quit
/// 4. clear the sink, release the mouse, restore the terminal
/// 5. report any failure
///
/// Return:
/// IoResult<()> -> Ok on a clean exit, Err on a terminal I/O failure
///
/// Notes:
/// The terminal restore comes before the error report. A terminal in raw
/// mode needs a manual fix.
///
pub fn run_debug_graphics() -> IoResult<()> {
    log_3!("Starting TUI...");
    let mut terminal = ratatui::init();
    execute!(terminal.backend_mut(), EnableMouseCapture)?;

    let (sender, receiver) = channel::<EngineEvent>();
    set_sink(sender);

    let run_result = Tui::new(receiver).run(&mut terminal);
    clear_sink();
    let mouse_result = execute!(terminal.backend_mut(), DisableMouseCapture);

    ratatui::restore();

    run_result?;
    mouse_result?;

    Ok(())
}
