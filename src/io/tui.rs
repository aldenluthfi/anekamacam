//! # tui.rs
//!
//! Ratatui-based interactive interface for the engine.
//!
//! This module replaces plain CLI printing with an interactive, tabbed view.
//! It keeps command-driven gameplay while rendering board/logs/move history
//! and a detailed game-state tab.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 23/04/2026

use crate::*;

const TAB_TITLES: [&str; 3] = ["Game", "Overview", "Playground"];
const TAB_FOCUSABLES: [u8; 3] = [3, 2, 3];

const PICKER_SCROLL_KEY: (usize, usize) = (usize::MAX, usize::MAX);
const HELP_SCROLL_KEY: (usize, usize) = (usize::MAX, usize::MAX - 1);

enum TuiEvent {
    Input(KeyEvent),
    StateUpdate(BoardState),
    StateInit(Arc<Mutex<State>>),
    PlaygroundUpdate(State),
    Unlock,
}

struct Tui {
    mode: u8,
    threads: usize,

    tab: usize,
    focus: usize,
    scroll_map: HashMap<(usize, usize), u16>,

    input: String,

    help: bool,
    locked: bool,

    file_name: Option<String>,
    game_state: Option<Arc<Mutex<State>>>,
    board_state: Option<BoardState>,
    overview_state: Option<OverviewState>,
    playground_state: Option<Arc<Mutex<State>>>,

    receiver: Receiver<TuiEvent>,
    sender: Sender<TuiEvent>,
}

impl Tui {
    fn new(
        receiver: Receiver<TuiEvent>, sender: Sender<TuiEvent>
    ) -> Self {
        let mut scroll_map = HashMap::new();

        for (tab, focusables) in
            TAB_FOCUSABLES.iter().enumerate().take(TAB_TITLES.len())
        {
            for focus in 0..*focusables as usize {
                scroll_map.insert((tab, focus), 0);
            }
        }

        scroll_map.insert(PICKER_SCROLL_KEY, 0);                                /* Selection screen scroll            */
        scroll_map.insert(HELP_SCROLL_KEY, 0);                                  /* Help popup tab scroll              */

        let mode = TUI_NORMAL_MODE;
        let tab = PICKER_SCROLL_KEY.0;
        let focus = PICKER_SCROLL_KEY.1;
        let input = String::new();
        let help = false;
        let locked = false;
        let file_name = None;
        let game_state = None;
        let board_state = None;
        let overview_state = None;
        let playground_state = None;
        let threads = 1;

        Self {
            mode,
            threads,

            tab,
            focus,
            scroll_map,

            input,

            help,
            locked,

            file_name,
            game_state,
            board_state,
            overview_state,
            playground_state,

            receiver,
            sender,
        }
    }

    fn reset(&mut self) {
        self.mode = TUI_NORMAL_MODE;
        self.tab = PICKER_SCROLL_KEY.0;
        self.focus = PICKER_SCROLL_KEY.1;

        self.input.clear();

        self.file_name = None;
        self.game_state = None;
        self.board_state = None;
        self.overview_state = None;
        self.playground_state = None;

        SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
    }

    fn run (&mut self, terminal: &mut DefaultTerminal) -> IoResult<()> {
        loop {
            terminal.draw(|frame| render(frame, self))?;

            if match self.receiver.try_recv() {
                Ok(TuiEvent::Input(key_event)) => {
                    handle_key(self, key_event)
                }
                Ok(TuiEvent::StateUpdate(state)) => {
                    self.board_state = Some(state);
                    false
                },
                Ok(TuiEvent::StateInit(state)) => {
                    self.overview_state = Some(OverviewState::from_state(
                        &state.lock().unwrap()
                    ));
                    let mut pg_state = State::clone(&state.lock().unwrap());
                    init_playground(&mut pg_state, 0);
                    self.playground_state =
                        Some(Arc::new(Mutex::new(pg_state)));
                    self.game_state = Some(state);
                    false
                },
                Ok(TuiEvent::PlaygroundUpdate(state)) => {
                    if let Some(arc) = &self.playground_state {
                        *arc.lock().unwrap() = state;
                    }
                    false
                },
                Ok(TuiEvent::Unlock) => {
                    SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);
                    self.locked = false;
                    false
                }
                Err(TryRecvError::Empty) => {
                    false
                },
                Err(TryRecvError::Disconnected) => {
                    true
                }
            } {
                break;
            }
        }

        Ok(())
    }
}

struct OverviewPieceInfo {
    char_str: String,
    promotions: String,
    roles: String,
    op_val: u16,
    eg_val: u16,
    op_pst: String,
    eg_pst: String,
    forbidden_zones: Option<String>,
    mandatory_promotions: Option<String>,
    optional_promotions: Option<String>,
}

struct OverviewPiece {
    name: String,
    info: Option<OverviewPieceInfo>,
}

struct OverviewState {
    configs: Vec<(String, String, u16)>,
    pieces: Vec<OverviewPiece>,
}

impl OverviewState {
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

        if halfmove_clock!(state) {
            configs.push((
                "Halfmove Limit".to_string(),
                state.statics.halfmove_limit.to_string(), 1
            ));
        }
        if repetition_limit!(state) {
            configs.push((
                "Repetition Limit".to_string(),
                state.statics.repetition_limit.to_string(), 1
            ));
        }

        let mut rules = Vec::new();

        if castling!(state) {
            rules.push("Castling");
        }
        if en_passant!(state) {
            rules.push("En Passant");
        }
        if promotions!(state) {
            rules.push("Promotions");
        }
        if drops!(state) {
            rules.push("Drops");
        }
        if count_limits!(state) {
            rules.push("Piece Limits");
        }
        if forbidden_zones!(state) {
            rules.push("Forbidden Zones");
        }
        if promote_to_captured!(state) {
            rules.push("Promote to Captured");
        }
        if demote_upon_capture!(state) {
            rules.push("Demote upon Capture");
        }
        if stalemate_loss!(state) {
            rules.push("Stalemate Loss");
        }
        if setup_phase!(state) {
            rules.push("Setup Phase");
        }
        if stand_offs!(state) {
            rules.push("Stand Offs");
        }

        let rules_str = rules.join("\n");
        let rules_height = rules.len() as u16;
        configs.push((
            "Rules".to_string(), rules_str, rules_height.max(1)
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

struct BoardState {
    board: String,
    move_history: String,
    details: Vec<[String; 2]>,
    fen: String,
}

impl BoardState {
    fn from_state(state: &State) -> Self {
        let board = format_game_state(state);
        let move_history = format_move_history(state);
        let mut details = Vec::new();

        let position_hash = format_position_hash(state);
        let game_phase = format_game_phase(state);
        let phase = format!("{} ({})", game_phase, state.phase_score);

        let castling_rights;
        let en_passant;
        let halfmove_clock;
        let repetition_count;
        let hand_info;

        details.push(["Game Phase".to_string(), phase]);
        details.push(
            [
                "Turn".to_string(),
                if state.playing == WHITE { "White" } else { "Black" }
                .to_string()
            ]
        );
        details.push(["Position Hash".to_string(), position_hash]);
        if castling!(state) {
            castling_rights = format_castling_rights(state);
            details.push(["Castling Rights".to_string(), castling_rights]);
        }
        if en_passant!(state) {
            en_passant = format_en_passant_square(state);
            details.push(["En Passant".to_string(), en_passant]);
        }
        if halfmove_clock!(state) {
            halfmove_clock = format!("{}/{}",
                state.halfmove_clock,
                state.statics.halfmove_limit
            );
            details.push(["Halfmove Clock".to_string(), halfmove_clock]);
        }
        if repetition_limit!(state) {
            let count = state
                .position_hash_map
                .get(&state.position_hash)
                .copied()
                .unwrap_or(1);
            repetition_count = format!("{}/{}",
                count,
                state.statics.repetition_limit
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

        let fen = format_fen(state);

        Self {
            board,
            move_history,
            details,
            fen,
        }
    }
}

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

    state.load_fen(&empty_fen);

    state.game_phase = OPENING;
}

fn set_playground_piece(state: &mut State, index: PieceIndex, square: Square) {
    state.main_board[square as usize] = index;

    let fen = format_fen(state);
    state.load_fen(&fen);

    state.game_phase = OPENING;
}

fn draw_game_selection(
    frame: &mut Frame<'_>, area: Rect, app: &mut Tui
) -> Option<Arc<Mutex<State>>> {

    let layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(25),
            Constraint::Fill(1),
        ])
        .split(area);

    let mut config_files = fs::read_dir(CONFIGS_DIR).ok()?
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_file())
        .map(|entry| entry.file_name().to_string_lossy().to_string())
        .filter(|name| !name.starts_with("example"))
        .collect::<Vec<String>>();
    config_files.sort();

    let previews = config_files
        .iter()
        .map(|name| parse_config_preview(&format!("{}/{}", CONFIGS_DIR, name)))
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
    .block(
        Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
    );

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
        .unwrap_or_else(|| panic!("No config files found in {}", CONFIGS_DIR));
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

    let tab_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Min(0),
            Constraint::Length(21),
        ])
        .split(area);

    let tab_titles: Vec<Line> = TAB_TITLES
        .iter()
        .enumerate()
        .map(|(i, title)| {
            if i == 2 && app.locked {
                Line::styled(
                    *title, Style::default().fg(Color::DarkGray)
                )
            } else {
                Line::raw(*title)
            }
        })
        .collect();

    let tabs = Tabs::new(tab_titles)
        .block(Block::default().borders(Borders::ALL))
        .select(app.tab)
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
    let input_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Min(0),
            Constraint::Length(13),
        ])
        .split(area);
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

fn draw_help_popup(frame: &mut Frame<'_>, area: Rect, app: &Tui) {

    let help_tab = *app.scroll_map
        .get(&HELP_SCROLL_KEY)
        .unwrap_or(&0) as usize;

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
        ("<]/[>", "Inctrease/decrease thread count"),
    ];

    let input_mode_rows = [
        ("<Enter>", "Execute command"),
        ("<Esc>", "Enter normal mode"),
    ];

    let command_rows: &[(&str, &str)] = if app.tab == 2 { &[
        (
            "add <piece> <square>",
            "Add a piece to the playground board at the given square"
        ),
        (
            "del <square>",
            "Remove any piece from the playground board at the given square"
        ),
        ("reset", "Reset the playground board to its initial state"),
    ] } else { &[
        ("abort", "Interrupt any running search immediately"),
        ("undo", "Undo the last move played on the board"),
        (
            "reset",
            "Rewind the game all the way to the starting position"
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
            "search <depth>",
            "Search the current position up to the specified depth"
        ),
        (
            "go <depth>",
            "Calculate and play the best move up to the specified depth"
        ),
        (
            "play <depth> <time>",
            "Automatically play the game with specified depth and time limit"
        ),
        (
            "perft <depth> <branch>",
            "Run a perft benchmark to verify move generation accuracy"
        ),
        (
            "move <move>",
            "Play a valid move using Cheesy Move Notation (CMN)"
        ),
    ] };

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
            Constraint::Length(40)
        );

    let block = Block::default()
        .borders(Borders::ALL)
        .border_style(Style::default().fg(Color::Yellow))
        .padding(Padding::horizontal(1))
        .style(Style::default().fg(Color::White));

    frame.render_widget(Clear, popup_area);
    let popup_inner = block.inner(popup_area);
    frame.render_widget(block, popup_area);

    let layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(2), Constraint::Min(0)])
        .split(popup_inner);

    frame.render_widget(help_tabs, layout[0]);

    let content_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints(if help_tab == 0 {
            [
                Constraint::Min(0),
                Constraint::Min(0),
                Constraint::Length(2)
            ]
        } else {
            [
                Constraint::Min(0),
                Constraint::Max(0),
                Constraint::Length(2)
            ]
        })
        .split(layout[1]);

    if help_tab == 0 {
        let mut keybind_rows = Vec::new();

        keybind_rows.push(Row::new(vec![
            Cell::from("Normal mode").style(
                Style::default().add_modifier(Modifier::BOLD)
            ),
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

        let keybind_table = Table::new(
            keybind_rows,
            [
                Constraint::Percentage(100),
                Constraint::Percentage(100)
            ]
        ).block(
            Block::default().borders(Borders::NONE)
        );

        frame.render_widget(keybind_table, content_layout[0]);
    } else {
        for &(cmd, desc) in command_rows {
            lines.push(Line::from(vec![
                Span::from(format!("{} ", cmd))
                    .style(
                        Style::default()
                            .fg(Color::Yellow)
                            .add_modifier(Modifier::BOLD)
                    )
            ]));
            lines.push(Line::from(vec![
                Span::from(desc)
            ]));
            lines.push(Line::default());
        }

        let content = Paragraph::new(lines).wrap(Wrap { trim: true });
        frame.render_widget(content, content_layout[0]);
    }

    let footer = vec![
        Line::default(),
        Line::from(Span::from("Press <j/k> to switch tabs, <?> to close")
            .style(Style::default().fg(Color::Gray))),
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

    let view_layout = Layout::default()
        .direction(Direction::Vertical)
        .spacing(Spacing::Overlap(1))
        .constraints(if app.tab == usize::MAX {
            [
                Constraint::Max(0),
                Constraint::Fill(1),
                Constraint::Max(0)
            ]
        } else {
            [
                Constraint::Length(3),
                Constraint::Fill(1),
                Constraint::Length(3)
            ]
        })
        .split(view_guide);

    let tab_guide_layout = Layout::default()
        .direction(Direction::Horizontal)
        .spacing(Spacing::Overlap(1))
        .constraints([
            Constraint::Min(0),
            Constraint::Length(13)
        ])
        .split(view_layout[0]);
    let tab_guide = Paragraph::new(vec![
        Line::from(vec![
            Span::from("Tab Bar"),
        ]),
    ])
    .block(
        Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
            .merge_borders(MergeStrategy::Exact)
    );
    let log_guide = Paragraph::new(vec![
        Line::from(vec![
            Span::from("Verbosity"),
        ]),
    ])
    .block(
        Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
            .merge_borders(MergeStrategy::Exact)
    );

    if app.tab != usize::MAX {
        frame.render_widget(tab_guide, tab_guide_layout[0]);
        frame.render_widget(log_guide, tab_guide_layout[1]);
    }

    match app.tab {
        0 => {
            let has_moves = app.board_state.as_ref()
                .map(|s| !s.move_history.trim().is_empty())
                .unwrap_or(false);

            let game_log_layout = Layout::default()
                .direction(Direction::Vertical)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Fill(1),
                    Constraint::Length(5),
                ])
                .split(view_layout[1]);

            let game_log_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Game Log"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let board_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Board View"),
                ]),
            ])
            .alignment(Alignment::Center);

            let field_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Details"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let fen_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("FEN"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            frame.render_widget(game_log_guide, game_log_layout[1]);

            if has_moves {
                let game_guide_layout = Layout::default()
                    .direction(Direction::Horizontal)
                    .spacing(Spacing::Overlap(1))
                    .constraints([
                        Constraint::Fill(4),
                        Constraint::Fill(1),
                        Constraint::Fill(2),
                    ])
                    .split(game_log_layout[0]);

                let board_center = game_guide_layout[0]
                    .centered_vertically(Constraint::Length(1));

                let moves_guide = Paragraph::new(vec![
                    Line::from(vec![
                        Span::from("Moves"),
                    ]),
                ])
                .block(
                    Block::default()
                        .borders(Borders::ALL)
                        .padding(Padding::horizontal(1))
                        .merge_borders(MergeStrategy::Exact)
                );

                let right_layout = Layout::default()
                    .direction(Direction::Vertical)
                    .spacing(Spacing::Overlap(1))
                    .constraints([
                        Constraint::Min(0),
                        Constraint::Length(3)
                    ])
                    .split(game_guide_layout[2]);

                frame.render_widget(board_guide, board_center);
                frame.render_widget(
                    moves_guide, game_guide_layout[1]
                );
                frame.render_widget(field_guide, right_layout[0]);
                frame.render_widget(fen_guide, right_layout[1]);
            } else {
                let game_guide_layout = Layout::default()
                    .direction(Direction::Horizontal)
                    .spacing(Spacing::Overlap(1))
                    .constraints([
                        Constraint::Fill(5),
                        Constraint::Fill(2),
                    ])
                    .split(game_log_layout[0]);

                let board_center = game_guide_layout[0]
                    .centered_vertically(Constraint::Length(1));

                let right_layout = Layout::default()
                    .direction(Direction::Vertical)
                    .spacing(Spacing::Overlap(1))
                    .constraints([
                        Constraint::Min(0),
                        Constraint::Length(3)
                    ])
                    .split(game_guide_layout[1]);

                frame.render_widget(board_guide, board_center);
                frame.render_widget(field_guide, right_layout[0]);
                frame.render_widget(fen_guide, right_layout[1]);
            }

        },
        1 => {
            let chunks_layout = Layout::default()
                .direction(Direction::Horizontal)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Percentage(25),
                    Constraint::Fill(1),
                ])
                .split(view_layout[1]);

            let left_layout = Layout::default()
                .direction(Direction::Vertical)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Percentage(40),
                    Constraint::Fill(1),
                ])
                .split(chunks_layout[0]);

            let field_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Configs")
                ])
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let piece_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Pieces"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let right_layout = Layout::default()
                .direction(Direction::Vertical)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Length(3),
                    Constraint::Min(0),
                    Constraint::Length(3)
                ])
                .split(chunks_layout[1]);

            let tables_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Tables"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let board_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Board View"),
                ]),
            ])
            .alignment(Alignment::Center);
            let board_center = right_layout[1]
                .centered_vertically(Constraint::Length(1));

            let piece_detail_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Piece Details"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            frame.render_widget(field_guide, left_layout[0]);
            frame.render_widget(piece_guide, left_layout[1]);
            frame.render_widget(tables_guide, right_layout[0]);
            frame.render_widget(board_guide, board_center);
            frame.render_widget(piece_detail_guide, right_layout[2]);
        },
        2 => {
            let piece_selected =
                *app.scroll_map.get(&(2, 0)).unwrap_or(&0) > 0;

            let pg_log_layout = Layout::default()
                .direction(Direction::Vertical)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Fill(1),
                    Constraint::Length(5),
                ])
                .split(view_layout[1]);

            let log_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Logs"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let chunks_layout = Layout::default()
                .direction(Direction::Horizontal)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Fill(5),
                    Constraint::Fill(2),
                ])
                .split(pg_log_layout[0]);

            let piece_list_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Pieces")
                ])
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let board_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Board View"),
                ]),
            ])
            .alignment(Alignment::Center);
            let board_center = chunks_layout[0]
                .centered_vertically(Constraint::Length(1));

            frame.render_widget(log_guide, pg_log_layout[1]);
            frame.render_widget(board_guide, board_center);

            if piece_selected {
                let left_layout = Layout::default()
                    .direction(Direction::Horizontal)
                    .spacing(Spacing::Overlap(1))
                    .constraints([
                        Constraint::Percentage(50),
                        Constraint::Fill(1),
                    ])
                    .split(chunks_layout[1]);

                let move_list_guide = Paragraph::new(vec![
                    Line::from(vec![
                        Span::from("Moves"),
                    ]),
                ])
                .block(
                    Block::default()
                        .borders(Borders::ALL)
                        .padding(Padding::horizontal(1))
                        .merge_borders(MergeStrategy::Exact)
                );

                frame.render_widget(piece_list_guide, left_layout[0]);
                frame.render_widget(move_list_guide, left_layout[1]);
            } else {
                frame.render_widget(
                    piece_list_guide, chunks_layout[1]
                );
            }
        },
        usize::MAX => {
            let selection_layout = Layout::default()
                .direction(Direction::Horizontal)
                .spacing(Spacing::Overlap(1))
                .constraints([
                    Constraint::Percentage(25),
                    Constraint::Fill(1),
                ])
                .split(view_layout[1]);

            let selection_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Variants"),
                ]),
            ])
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .padding(Padding::horizontal(1))
                    .merge_borders(MergeStrategy::Exact)
            );

            let board_guide = Paragraph::new(vec![
                Line::from(vec![
                    Span::from("Board View"),
                ]),
            ])
            .alignment(Alignment::Center);
            let board_center = selection_layout[1]
                .centered_vertically(Constraint::Length(1));

            frame.render_widget(selection_guide, selection_layout[0]);
            frame.render_widget(board_guide, board_center);
        },
        _ => {}
    }

    let input_guide_layout = Layout::default()
        .direction(Direction::Horizontal)
        .spacing(Spacing::Overlap(1))
        .constraints([
            Constraint::Min(0),
            Constraint::Length(11)
        ])
        .split(view_layout[2]);

    let command_guide = Paragraph::new(vec![
        Line::from(vec![
            Span::from("Input Bar"),
        ]),
    ])
    .block(
        Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
            .merge_borders(MergeStrategy::Exact)
    );
    let threads_guide = Paragraph::new(vec![
        Line::from(vec![
            Span::from("Threads"),
        ]),
    ])
    .block(
        Block::default()
            .borders(Borders::ALL)
            .padding(Padding::horizontal(1))
            .merge_borders(MergeStrategy::Exact)
    );

    if app.tab != usize::MAX {
        frame.render_widget(command_guide, input_guide_layout[0]);
        frame.render_widget(threads_guide, input_guide_layout[1]);
    }
}

fn draw_game_tab(frame: &mut Frame<'_>, area: Rect, app: &mut Tui) {

    const TAB_FOCUS_MOVES: usize = 0;
    const TAB_FOCUS_FEN: usize = 1;
    const TAB_FOCUS_LOGS: usize = 2;

    let board = app.board_state.as_ref()
        .map(|state| state.board.clone())
        .unwrap_or_else(|| "Loading...".to_string());

    let main_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(75),
            Constraint::Percentage(25),
        ])
        .split(area);

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
        let top_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Fill(4),
                Constraint::Fill(1),
                Constraint::Fill(2),
            ])
            .split(top_rect);
        board_area = top_layout[0].centered_vertically(
            Constraint::Length(board.lines().count() as u16 + 1)
        );
        moves_area = top_layout[1];
        let right_most_rect = Layout::default()
            .direction(Direction::Vertical)
            .spacing(Spacing::Overlap(1))
            .constraints([
                Constraint::Min(0),
                Constraint::Length(3)
            ])
            .split(top_layout[2]);
        details_area = right_most_rect[0];
        fen_area = right_most_rect[1];
    } else {
        let top_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Fill(5),
                Constraint::Fill(2),
            ])
            .split(top_rect);
        board_area = top_layout[0].centered_vertically(
            Constraint::Length(board.lines().count() as u16 + 1)
        );
        moves_area = Rect::default();
        let right_most_rect = Layout::default()
            .direction(Direction::Vertical)
            .spacing(Spacing::Overlap(1))
            .constraints([
                Constraint::Min(0),
                Constraint::Length(3)
            ])
            .split(top_layout[1]);
        details_area = right_most_rect[0];
        fen_area = right_most_rect[1];
    }

    let board_block = Block::default()
        .borders(Borders::NONE);
    let mut moves_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let details_block = Block::default()
        .merge_borders(MergeStrategy::Exact)
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let mut fen_block = Block::default()
        .merge_borders(MergeStrategy::Exact)
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let mut logs_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);

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

    let logs = LOG_MESSAGES.lock().unwrap_or_else(|e| {
        e.into_inner()
    });
    let log_lines = logs
        .iter()
        .rev()
        .take(MAX_LOGS_LEN)
        .rev()
        .filter(|line| {
            let level = if line.starts_with("[1]") {
                1
            } else if line.starts_with("[2]") {
                2
            } else if line.starts_with("[3]") {
                3
            } else if line.starts_with("[4]") {
                4
            } else if line.starts_with("[5]") {
                5
            } else {
                unreachable!()
            };

            level <= configured_verbosity_level()
        })
        .map(|line| {
            let level_color = if line.starts_with("[1]") {
                Color::LightRed
            } else if line.starts_with("[2]") {
                Color::Yellow
            } else if line.starts_with("[3]") {
                Color::LightGreen
            } else if line.starts_with("[4]") {
                Color::LightBlue
            } else if line.starts_with("[5]") {
                Color::Magenta
            } else {
                Color::Gray
            };

            if line.starts_with('[') && let Some(end_idx) = line.find(']') {
                let (level, rest) = line.split_at(end_idx + 1);
                return Line::from(vec![
                    Span::styled(
                        level.to_string(), Style::default().fg(level_color)
                    ),
                    Span::raw(rest.to_string()),
                ]);
            }

            Line::from(Span::raw(line.clone()))
        })
        .collect::<Vec<_>>();

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
    let mut logs_paragraph = Paragraph::new(Text::from(log_lines))
        .block(logs_block);

    let max_moves_scroll = (moves_paragraph.line_count(moves_area.width) as u16)
        .saturating_sub(moves_area.height);
    let max_fen_scroll = (fen_paragraph.line_width() as u16)
        .saturating_sub(fen_area.width);
    let max_logs_scroll = (logs_paragraph.line_count(logs_area.width) as u16)
        .saturating_sub(logs_area.height);

    let current_moves_scroll = app.scroll_map.get(&(app.tab, TAB_FOCUS_MOVES))
        .copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus {}",
                app.tab, TAB_FOCUS_MOVES
            )
        }
    );

    let current_fen_scroll = app.scroll_map.get(&(app.tab, TAB_FOCUS_FEN))
        .copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus {}",
                app.tab, TAB_FOCUS_FEN
            )
        }
    );

    let current_logs_scroll = app.scroll_map.get(&(app.tab, TAB_FOCUS_LOGS))
        .copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus {}",
                app.tab, TAB_FOCUS_LOGS
            )
        }
    );

    let clamp_scroll = |current: u16, max: u16,
        scroll_map: &mut HashMap<(usize, usize), u16>,
        key: (usize, usize)| -> u16 {
        if current > max && current < u16::MAX {
            let clamped = max.saturating_sub(u16::MAX - current);
            scroll_map.insert(key, clamped);
            clamped
        } else if current > max {
            max
        } else if current == max {
            scroll_map.insert(key, u16::MAX);
            max
        } else {
            current
        }
    };
    let tab = app.tab;
    let final_moves_scroll = clamp_scroll(
        current_moves_scroll, max_moves_scroll,
        &mut app.scroll_map, (tab, TAB_FOCUS_MOVES)
    );
    let final_fen_scroll = clamp_scroll(
        current_fen_scroll, max_fen_scroll,
        &mut app.scroll_map, (tab, TAB_FOCUS_FEN)
    );
    let final_logs_scroll = clamp_scroll(
        current_logs_scroll, max_logs_scroll,
        &mut app.scroll_map, (tab, TAB_FOCUS_LOGS)
    );

    moves_paragraph = moves_paragraph.scroll((final_moves_scroll, 0));
    fen_paragraph = fen_paragraph.scroll((0, final_fen_scroll));
    logs_paragraph = logs_paragraph.scroll((final_logs_scroll, 0));

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

    let chunks = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage(25),
            Constraint::Fill(1),
        ])
        .split(area);

    let left_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(40),
            Constraint::Percentage(60),
        ])
        .split(chunks[0]);

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
        Block::default().borders(Borders::ALL).padding(Padding::horizontal(1))
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
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(style)
                    .padding(Padding::horizontal(1))
            )
    } else {
        List::new(items)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(style)
                    .padding(Padding::horizontal(1))
            )
            .highlight_style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            )
    };

    if let Some(piece) = pieces.get(selected as usize) {
        let right_chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),
                Constraint::Length(6),
            ])
            .split(chunks[1]);

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
            Block::default()
            .padding(Padding::horizontal(1))
            .borders(Borders::ALL)
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

            let table_view_layout = Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length(3),
                    Constraint::Min(0),
                ])
                .split(right_chunks[0]);

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

    const TAB_FOCUS_LOGS: usize = 2;

    let main_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage(75),
            Constraint::Percentage(25),
        ])
        .split(area);

    let layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Fill(5),
            Constraint::Fill(2)
        ])
        .split(main_layout[0]);

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

        let mut enabled = Vec::new();
        for (i, piece) in state.statics.pieces.iter().enumerate() {
            if !state.piece_list[i].is_empty() {
                enabled.push(p_index!(piece) as usize);
            }
        }

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
                    if state.piece_list[p_index!(p) as usize].is_empty() {
                        Style::default().fg(Color::DarkGray)
                    } else {
                        Style::default()
                    }
                )
            )
            .collect();

        let mut selected = *app.scroll_map.get(&(2, 0)).unwrap_or(&0);
        if !pieces.is_empty() {
            if selected > enabled.len() as u16 {
                selected = enabled.len() as u16;
                app.scroll_map.insert((2, 0), selected);
            }

            if selected == 0 {
                piece_list_state.select(None);
            } else {
                piece_list_state.select(
                    Some(enabled[selected as usize - 1])
                );
            }
        } else {
            piece_list_state.select(None);
        }

        let selected_piece = if selected == 0 {
            None
        } else {
            enabled.get(selected as usize - 1).copied()
        };

        if selected_piece.is_none() && app.focus == 1 {
            app.focus = 0;
        }

        let piece_style = if app.focus == 0 && app.mode == TUI_NORMAL_MODE {
            Style::default().fg(Color::Yellow).add_modifier(Modifier::BOLD)
        } else {
            Style::default()
        };

        piece_list = List::new(pieces)
            .block(
                Block::default()
                    .borders(Borders::ALL)
                    .border_style(piece_style)
                    .padding(Padding::horizontal(1))
            )
            .highlight_style(
                Style::default()
                    .fg(Color::Yellow)
                    .add_modifier(Modifier::BOLD)
            );

        match selected_piece {
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
                        ListItem::new(format_move(mv, &state))
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
                            Block::default()
                                .borders(Borders::ALL)
                                .border_style(move_style)
                                .padding(Padding::horizontal(1))
                        )
                        .highlight_style(
                            Style::default()
                                .fg(Color::Yellow)
                                .add_modifier(Modifier::BOLD)
                        )
                );

                let piece_char = state.statics.pieces[piece_idx].char;
                let active_moves: Vec<&Move> = if selected_move == 0 {
                    filtered_moves.iter().copied().collect()
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
                            mv.1.iter().for_each(|&cap| {
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
            Block::default()
                .borders(Borders::ALL)
                .padding(Padding::horizontal(1))
        );
        board_str = "Loading...".to_string();
        move_list_opt = None;
        width = board_str.chars().count() as u16;
        height = 1;
    }

    if let Some(move_list) = move_list_opt {
        let left_layout = Layout::default()
            .direction(Direction::Horizontal)
            .constraints([
                Constraint::Percentage(50),
                Constraint::Percentage(50),
            ])
            .split(layout[1]);
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

    let mut logs_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    if app.focus == TAB_FOCUS_LOGS && app.mode == TUI_NORMAL_MODE {
        logs_block = logs_block.border_style(
            Style::default().fg(Color::Yellow)
        );
    }

    let logs = LOG_MESSAGES.lock().unwrap_or_else(|e| {
        e.into_inner()
    });
    let log_lines = logs
        .iter()
        .rev()
        .take(MAX_LOGS_LEN)
        .rev()
        .filter(|line| {
            let level = if line.starts_with("[1]") {
                1
            } else if line.starts_with("[2]") {
                2
            } else if line.starts_with("[3]") {
                3
            } else if line.starts_with("[4]") {
                4
            } else if line.starts_with("[5]") {
                5
            } else {
                unreachable!()
            };

            level <= configured_verbosity_level()
        })
        .map(|line| {
            let level_color = if line.starts_with("[1]") {
                Color::LightRed
            } else if line.starts_with("[2]") {
                Color::Yellow
            } else if line.starts_with("[3]") {
                Color::LightGreen
            } else if line.starts_with("[4]") {
                Color::LightBlue
            } else if line.starts_with("[5]") {
                Color::Magenta
            } else {
                Color::Gray
            };

            if line.starts_with('[') && let Some(end_idx) = line.find(']') {
                let (level_str, rest) = line.split_at(end_idx + 1);
                return Line::from(vec![
                    Span::styled(
                        level_str.to_string(),
                        Style::default().fg(level_color)
                    ),
                    Span::raw(rest.to_string()),
                ]);
            }

            Line::from(Span::raw(line.clone()))
        })
        .collect::<Vec<_>>();

    let mut logs_paragraph = Paragraph::new(Text::from(log_lines))
        .block(logs_block);

    let clamp_scroll = |current: u16, max: u16,
        scroll_map: &mut HashMap<(usize, usize), u16>,
        key: (usize, usize)| -> u16 {
        if current > max && current < u16::MAX {
            let clamped = max.saturating_sub(u16::MAX - current);
            scroll_map.insert(key, clamped);
            clamped
        } else if current > max {
            max
        } else if current == max {
            scroll_map.insert(key, u16::MAX);
            max
        } else {
            current
        }
    };

    let max_logs_scroll =
        (logs_paragraph.line_count(logs_area.width) as u16)
            .saturating_sub(logs_area.height);
    let current_logs_scroll = app.scroll_map
        .get(&(0, TAB_FOCUS_LOGS))
        .copied()
        .unwrap_or_else(|| {
            panic!(
                "Scroll value missing for tab 0, focus {}",
                TAB_FOCUS_LOGS
            )
        });
    let final_logs_scroll = clamp_scroll(
        current_logs_scroll, max_logs_scroll,
        &mut app.scroll_map, (0, TAB_FOCUS_LOGS)
    );
    logs_paragraph = logs_paragraph.scroll((final_logs_scroll, 0));
    frame.render_widget(logs_paragraph, logs_area);
}

fn render(frame: &mut Frame<'_>, app: &mut Tui) {
    let root = frame.area();

    if !app.locked && app.game_state.is_none() {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Min(0),
                Constraint::Length(1),
            ])
            .split(root);
        app.game_state = draw_game_selection(frame, chunks[0], app);
        draw_help_bar(frame, chunks[1], app);
    } else {
        let chunks = Layout::default()
            .direction(Direction::Vertical)
            .constraints([
                Constraint::Length(3),
                Constraint::Min(0),
                Constraint::Length(3),
                Constraint::Length(1),
            ])
            .split(root);

        draw_tabs(frame, chunks[0], app);
        match app.tab {
            0 => draw_game_tab(frame, chunks[1], app),
            1 => draw_overview_tab(frame, chunks[1], app),
            2 => draw_playground_tab(frame, chunks[1], app),
            _ => {},
        }

        draw_input(frame, chunks[2], app);
        draw_help_bar(frame, chunks[3], app);
    }

    if app.help {
        draw_help_popup(frame, root, app);
    }
}

fn execute_command(
    command: &str,
    state: &mut State,
    playground: Option<&mut State>,
    file_name: Option<String>,
    ttable: Arc<TTable>,
    qtable: Arc<QTable>,
    threads: usize,
    sender: Sender<TuiEvent>
) {
    let trimmed = command.trim();

    if trimmed.is_empty() {
        return;
    }

    let is_playground = playground.is_some();

    if is_playground && !matches!(
        trimmed.split_whitespace().next().unwrap_or(""),
        "reset" | "add" | "del"
    ) {
        log_2!("Invalid command: {}", trimmed);
        return;
    }

    if !is_playground && matches!(
        trimmed.split_whitespace().next().unwrap_or(""),
        "add" | "del"
    ) {
        log_2!("Invalid command: {}", trimmed);
        return;
    }

    match trimmed {
        "undo" => {
            if state.ply_counter > 0 {
                undo_move!(state);
            } else {
                log_2!("No moves to undo");
            }

            let board_state = BoardState::from_state(state);

            sender.send(
                TuiEvent::StateUpdate(board_state)
            ).unwrap_or_else(
                |e| {
                    panic!("Failed to send TuiEvent::StateUpdate: {e}")
                }
            );
        }
        "reset" => {
            if let Some(pg) = playground {
                *pg = state.clone();
                init_playground(pg, 0);

                sender.send(
                    TuiEvent::PlaygroundUpdate((*pg).clone())
                ).unwrap_or_else(|e| {
                    panic!(
                        "Failed to send TuiEvent::PlaygroundUpdate: {e}"
                    )
                });
            } else {
                while state.ply_counter > 0 {
                    undo_move!(state);

                    let board_state = BoardState::from_state(state);

                    sender.send(
                        TuiEvent::StateUpdate(board_state)
                    ).unwrap_or_else(|e| {
                        panic!("Failed to send TuiEvent::StateUpdate: {e}")
                    });
                }
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
            for mv in moves {
                log_2!("- {}", format_move(&mv, state));
            }
        }
        _ if trimmed.starts_with("see") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: see [move]");
                return;
            }

            let mv_str = parts[1];
            let mv = parse_move(mv_str, state).unwrap_or_else(|| {
                log_2!("Invalid move: {}", mv_str);
                null_move()
            });

            if mv == null_move() {
                return;
            }

            let see_score = see!(state, mv.clone());

            log_2!("SEE for {}: {}", format_move(&mv, state), see_score);
        }
        _ if trimmed.starts_with("fen") => {
            let fen = trimmed[4..].trim();
            state.load_fen(fen);
            log_2!("Loaded FEN");

            let board_state = BoardState::from_state(state);

            sender.send(
                TuiEvent::StateUpdate(board_state)
            ).unwrap_or_else(
                |e| {
                    panic!("Failed to send TuiEvent::StateUpdate: {e}")
                }
            );
        }
        _ if trimmed.starts_with("search") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: search [depth]");
                return;
            }

            let depth = parts[1].parse::<usize>().unwrap_or_else(|_| {
                log_2!("Invalid depth: {}", parts[1]);
                0
            });

            let mut info = SearchInfo {
                set_depth: depth, ..Default::default()
            };
            let mut bufs = SearchBufs::default();
            let result = search_position(
                state, Arc::clone(&ttable), Arc::clone(&qtable),
                &mut info, &mut bufs, threads
            );

            if result.best_move == null_move() {
                log_2!("No legal move available");
            }
        }
        _ if trimmed.starts_with("go") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: go [depth]");
                return;
            }

            let depth = parts[1].parse::<usize>().unwrap_or_else(|_| {
                log_2!("Invalid depth: {}", parts[1]);
                0
            });

            let mut info = SearchInfo {
                set_depth: depth, ..Default::default()
            };
            let mut bufs = SearchBufs::default();
            let result = search_position(
                state,
                Arc::clone(&ttable), Arc::clone(&qtable),
                &mut info, &mut bufs, threads
            );

            if result.best_move == null_move() {
                log_2!("No legal move available");
                return;
            }

            log_1!(
                "Best Move: {} | Score: {} | Nodes: {} | Time: {}",
                format_move(&result.best_move, state),
                result.best_score,
                result.total_nodes,
                format_time(result.total_elapsed)
            );

            make_move!(state, result.best_move);

            let board_state = BoardState::from_state(state);

            sender.send(
                TuiEvent::StateUpdate(board_state)
            ).unwrap_or_else(
                |e| {
                    panic!("Failed to send TuiEvent::StateUpdate: {e}")
                }
            );
        }
        _ if trimmed.starts_with("play") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 3 {
                log_2!("Usage: play [depth] [time (s)]");
                return;
            }

            let depth = parts[1].parse::<usize>().unwrap_or_else(|_| {
                log_2!("Invalid depth: {}", parts[1]);
                0
            });

            let time_limit = parts[2].parse::<f64>().unwrap_or_else(|_| {
                log_2!("Invalid time limit: {}", parts[2]);
                0.0
            });

            let mut info = SearchInfo {
                set_depth: depth,
                set_timed: (time_limit * 1_000_000_000.0) as u128,
                ..Default::default()
            };
            let mut bufs = SearchBufs::default();

            while !state.game_over {

                if SYSTEM_INTERRUPT.load(Ordering::Relaxed) {
                    break;
                }

                let result = search_position(
                    state,
                    Arc::clone(&ttable), Arc::clone(&qtable),
                    &mut info, &mut bufs, threads
                );

                if result.best_score == -INF {
                    state.game_over = true;
                    log_1!(
                        "Checkmate! {} wins.",
                        if state.playing == WHITE { "Black" } else { "White" }
                    );
                } else if state.game_over {
                    log_1!("It's a draw!");
                } else {
                    make_move!(state, result.best_move);
                }

                let board_state = BoardState::from_state(state);

                sender.send(
                    TuiEvent::StateUpdate(board_state)
                ).unwrap_or_else(
                    |e| {
                        panic!("Failed to send TuiEvent::StateUpdate: {e}")
                    }
                );
            }
        }
        _ if trimmed.starts_with("perft") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 3 {
                log_2!("Usage: perft [depth] [branch]");
                return;
            }

            let depth = parts[1].parse::<u8>().unwrap_or_else(|_| {
                log_2!("Invalid depth: {}", parts[1]);
                0
            });

            let branch = parts[2].parse::<i8>().unwrap_or_else(|_| {
                log_2!("Invalid branch: {}", parts[2]);
                -1
            });

            if let Some(name) = file_name {
                let variant = name.strip_suffix(".conf").unwrap_or(&name);
                let perft_path = format!("{}/{}.perft", PERFT_DIR, variant);

                if Path::new(&perft_path).is_file() {
                    benchmark_perft(
                        state, &perft_path, depth, branch, usize::MAX
                    );
                }

                return;
            }

            benchmark_headless_perft(state, depth, branch);
        }
        _ if trimmed.starts_with("move") => {
            let mv_str = trimmed[5..].trim();
            if let Some(mv) = parse_move(mv_str, state) {
                make_move!(state, mv);

                let board_state = BoardState::from_state(state);

                sender.send(
                    TuiEvent::StateUpdate(board_state)
                ).unwrap_or_else(
                    |e| {
                        panic!("Failed to send TuiEvent::StateUpdate: {e}")
                    }
                );
            } else {
                log_2!("Invalid move: {}", mv_str);
            }
        }
        _ if trimmed.starts_with("add") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 3 {
                log_2!("Usage: add [piece] [square]");
                return;
            }

            let playground_state = playground.unwrap();

            let piece_character = parts[1].chars().next();
            let piece_index = piece_character.and_then(|character| {
                playground_state.statics.pieces
                    .iter()
                    .position(|piece| piece.char == character)
                    .map(|index| index as PieceIndex)
            });
            let target_square = parse_square(parts[2], playground_state);

            match (piece_index, target_square) {
                (Some(index), Some(square)) =>
                    set_playground_piece(playground_state, index, square),
                _ => {
                    log_2!("Usage: add [piece] [square]");
                    return;
                }
            }

            sender.send(
                TuiEvent::PlaygroundUpdate(playground_state.clone())
            ).unwrap_or_else(|e| {
                panic!(
                    "Failed to send TuiEvent::PlaygroundUpdate: {e}"
                )
            });
        }
        _ if trimmed.starts_with("del") => {
            let parts = trimmed.split_whitespace().collect::<Vec<_>>();

            if parts.len() != 2 {
                log_2!("Usage: del [square]");
                return;
            }

            let playground_state = playground.unwrap();

            let target_square = parse_square(parts[1], playground_state);

            if target_square.is_none() {
                log_2!("Usage: del [square]");
                return;
            }

            let square = target_square.unwrap();

            set_playground_piece(playground_state, NO_PIECE, square);

            sender.send(
                TuiEvent::PlaygroundUpdate(playground_state.clone())
            ).unwrap_or_else(|e| {
                panic!(
                    "Failed to send TuiEvent::PlaygroundUpdate: {e}"
                )
            });
        }
        _ => {
            log_2!("Invalid command: {}", trimmed);
        }
    }
}

fn handle_key(app: &mut Tui, event: KeyEvent) -> bool {

    if event.kind != KeyEventKind::Press {
        return false;
    }

    let code = event.code;

    match (app.mode, code) {
        (TUI_INPUT_MODE, KeyCode::Enter) => {

            if app.input == "abort" {
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);

                app.input.clear();
                return false;
            }

            if app.locked {
                log_2!("Command execution in progress, please wait...");
                return false;
            }

            app.locked = true;

            thread::spawn({
                let command = app.input.clone();
                let file_name = app.file_name.clone();

                let arc_state = app.game_state.as_mut().unwrap_or_else(
                    || {
                        panic!("Game state is None when executing command")
                    }
                ).clone();

                let arc_playground = app.playground_state.as_mut()
                    .unwrap_or_else(
                    || {
                        panic!(
                            "Playground state is None when executing command"
                        )
                    }
                ).clone();

                let sender = app.sender.clone();
                let threads = app.threads;
                let tab = app.tab;

                move || {
                    let table = TTable::default();
                    let qtable = QTable::default();

                    let mut state = arc_state.lock()
                        .unwrap_or_else(|_| {
                            panic!(
                                concat!(
                                    "Failed to lock game state ",
                                    "for command execution"
                                )
                            )
                        });

                    let mut playground = arc_playground.lock()
                        .unwrap_or_else(|_| {
                            panic!(
                                concat!(
                                    "Failed to lock playground state ",
                                    "for command execution"
                                )
                            )
                        });

                    execute_command(
                        &command,
                        &mut state,
                        if tab == 2 { Some(&mut playground) } else { None },
                        file_name,
                        Arc::new(table),
                        Arc::new(qtable),
                        threads,
                        sender.clone()
                    );

                    sender.send(
                        TuiEvent::Unlock
                    ).unwrap_or_else(
                        |e| {
                            panic!("Failed to send TuiEvent::Unlock: {e}")
                        }
                    );
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
            if app.help {
                let tab = app.scroll_map
                    .get(&HELP_SCROLL_KEY)
                    .copied()
                    .unwrap_or(0)
                    .saturating_sub(1);
                app.scroll_map.insert(HELP_SCROLL_KEY, tab);
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
            if app.help {
                let tab = app.scroll_map
                    .get(&HELP_SCROLL_KEY)
                    .copied()
                    .unwrap_or(0)
                    .saturating_add(1)
                    .min(1);
                app.scroll_map.insert(HELP_SCROLL_KEY, tab);
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
        (TUI_NORMAL_MODE, KeyCode::Enter) if app.tab == PICKER_SCROLL_KEY.0 => {
            app.file_name = Some(app.input.clone());
            app.locked = true;
            app.focus = 0;
            app.tab = 0;

            thread::spawn({
                let filename = app.input.trim();
                let path = Path::new(CONFIGS_DIR).join(filename);
                let sender = app.sender.clone();

                app.input.clear();

                move || {
                    if path.is_file() {
                        let path_str = path.to_string_lossy();
                        let state = parse_config_file(&path_str);

                        let board_state = BoardState::from_state(&state);

                        sender.send(
                            TuiEvent::StateInit(Arc::new(Mutex::new(state)))
                        ).unwrap_or_else(
                            |e| {
                                panic!(
                                    "Failed to send TuiEvent::StateInit: {e}"
                                )
                            }
                        );

                        sender.send(
                            TuiEvent::StateUpdate(board_state)
                        ).unwrap_or_else(
                            |e| {
                                panic!(
                                    "Failed to send TuiEvent::StateUpdate: {e}"
                                )
                            }
                        );

                        sender.send(
                            TuiEvent::Unlock
                        ).unwrap_or_else(
                            |e| {
                                panic!("Failed to send TuiEvent::Unlock: {e}")
                            }
                        );
                    } else {
                        log_2!(
                            "Config file not found at path: {}",
                            path.to_string_lossy()
                        );
                    }
                }
            });


            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('q')) => true,
        (TUI_NORMAL_MODE, KeyCode::Char('?')) => {
            app.help = !app.help;
            false
        },
        (TUI_NORMAL_MODE, _) if app.tab == usize::MAX => {                      /* everything else in is off          */
            false
        },
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
            let next = (app.tab.saturating_add(1)) % TAB_TITLES.len();
            app.tab = if next == 2 && app.locked {
                (next + 1) % TAB_TITLES.len()
            } else {
                next
            };
            app.focus = 0;
            false
        },
        _ => false,
    }
}

fn input_listener(sender: Sender<TuiEvent>) {
    loop {
        let read_event = event::read().unwrap_or_else(
            |e| {
                panic!("Failed to read input event: {e}")
            }
        );

        if let Event::Key(key_event) = read_event {
            sender.send(
                TuiEvent::Input(key_event)
            ).unwrap_or_else(
                |e| {
                    panic!("Failed to send TuiEvent::Input: {e}")
                }
            );
        }
    }
}

pub fn tui() -> IoResult<()> {
    log_3!("Starting TUI...");
    let mut terminal = ratatui::init();
    execute!(terminal.backend_mut(), EnableMouseCapture)?;

    let (sender, receiver) = channel::<TuiEvent>();

    let input_sender = sender.clone();
    thread::spawn(move || {
        input_listener(input_sender);
    });

    let run_result = Tui::new(receiver, sender).run(&mut terminal);
    let mouse_result = execute!(terminal.backend_mut(), DisableMouseCapture);

    ratatui::restore();

    run_result?;
    mouse_result?;

    Ok(())
}
