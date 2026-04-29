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

const TAB_TITLES: [&str; 2] = ["Game", "Overview"];
const TAB_FOCUSABLES: [u8; 2] = [4, 1];

enum TuiEvent {
    Input(KeyEvent),
    StateUpdate(BoardState),
    Unlock,
}

struct Tui {
    mode: u8,
    tab: usize,
    focus: usize,
    scroll_map: HashMap<(usize, usize), u16>,
    input: String,
    help: bool,
    game_state: Option<Arc<Mutex<State>>>,
    locked: bool,
    board_state: Option<BoardState>,
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

        scroll_map.insert((usize::MAX, usize::MAX), 0);                         /* Selection screen scroll            */

        let mode = TUI_NORMAL_MODE;
        let tab = usize::MAX;
        let focus = usize::MAX;
        let input = String::new();
        let help = false;
        let locked = false;
        let game_state = None;
        let board_state = None;

        Self {
            mode,
            tab,
            focus,
            scroll_map,
            input,
            help,
            game_state,
            locked,
            board_state,
            receiver,
            sender,
        }
    }

    fn reset(&mut self) {
        self.mode = TUI_NORMAL_MODE;
        self.tab = usize::MAX;
        self.focus = usize::MAX;
        self.input.clear();
        self.help = false;
        self.game_state = None;
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
                }
                Ok(TuiEvent::Unlock) => {
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

struct BoardState {
    board: String,
    move_history: String,
    details: Vec<[String; 2]>,
    fen: String,
}

#[hotpath::measure_all]
impl BoardState {
    fn from_state(state: &State) -> Self {
        let board = format_game_state(state);
        let move_history = format_move_history(state);
        let mut details = Vec::new();

        let position_hash = format_position_hash(state);
        let game_phase = format_game_phase(state);

        let castling_rights;
        let en_passant;
        let halfmove_clock;
        let repetition_count;
        let hand_info;

        details.push(["Game Phase".to_string(), game_phase]);
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
                state.halfmove_limit
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
                state.repetition_limit
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

#[derive(Debug, Default)]
struct Popup<'a> {
    content: Text<'a>,
    border_style: Style,
    padding: Padding,
    style: Style,
}

impl<'a> Popup<'a> {

    fn content(mut self, content: impl Into<Text<'a>>) -> Self {
        self.content = content.into();
        self
    }

    fn border_style(mut self, style: Style) -> Self {
        self.border_style = style;
        self
    }

    fn padding(mut self, padding: Padding) -> Self {
        self.padding = padding;
        self
    }

    fn style(mut self, style: Style) -> Self {
        self.style = style;
        self
    }
}

impl Widget for Popup<'_> {
    fn render(self, area: Rect, buf: &mut Buffer) {
        Clear.render(area, buf);
        let block = Block::new()
            .borders(Borders::ALL)
            .border_style(self.border_style)
            .padding(self.padding);
        Paragraph::new(self.content)
            .wrap(Wrap { trim: true })
            .style(self.style)
            .block(block)
            .render(area, buf);
    }
}

fn draw_game_selection(
    frame: &mut Frame<'_>, area: Rect, app: &mut Tui
) -> Option<Arc<Mutex<State>>> {

    let config_files = fs::read_dir(CONFIGS_DIR).ok()?
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.path().is_file())
        .map(|entry| entry.file_name().to_string_lossy().to_string())
        .collect::<Vec<_>>();

    let mut selected = app.scroll_map.get(&(usize::MAX, usize::MAX))
        .copied()
        .unwrap_or_else(
            || {
                panic!(
                    "Scroll value missing for game selection screen"
                )
            }
        );

    let files_list = List::new(
        config_files.clone()
    )
    .highlight_style(
        Style::default()
            .fg(Color::Yellow)
            .add_modifier(Modifier::BOLD)
    )
    .block(
        Block::default()
            .borders(Borders::ALL)
    );

    if selected >= config_files.len() as u16 {
        app.scroll_map.insert((usize::MAX, usize::MAX), 0);
        selected = 0;
    }

    app.input = config_files[selected as usize].clone();

    let mut list_state = ListState::default();
    list_state.select(Some(selected as usize));

    frame.render_stateful_widget(files_list, area, &mut list_state);

    None
}

fn draw_tabs(frame: &mut Frame<'_>, area: Rect, app: &Tui) {
    let titles = TAB_TITLES
        .iter()
        .map(|title| Line::from(*title))
        .collect::<Vec<_>>();

    let tabs = Tabs::new(titles)
        .block(Block::default().borders(Borders::ALL))
        .select(app.tab)
        .style(Style::default().fg(Color::Gray))
        .highlight_style(
            Style::default()
                .fg(Color::Yellow)
                .add_modifier(Modifier::BOLD),
        );

    frame.render_widget(tabs, area);
}

fn draw_input(frame: &mut Frame<'_>, area: Rect, app: &Tui) {
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
    frame.render_widget(command, area);
}

fn draw_help_bar(frame: &mut Frame<'_>, area: Rect, app: &Tui) {
    let help_line = if app.mode == TUI_INPUT_MODE {
        Line::from(vec![
            Span::styled("[INPUT] ", Style::default().fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)),
            Span::raw("<Enter> Run")
                .style(Style::default().fg(Color::Gray)),
        ])
    } else {
        Line::from(vec![
            Span::styled("[NORMAL] ", Style::default().fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)),
            Span::raw(
                concat![
                    "<?> Help | <←/→> Focus | ",
                    "<j/k> Scroll | <Tab> Switch tab | <q> Quit | ",
                    "<n> New game | <G> Scroll to bottom | <g> Scroll to top"
                ]
            ).style(Style::default().fg(Color::Gray)),
        ])
    };

    frame.render_widget(help_line, area);
}

fn draw_help_popup(frame: &mut Frame<'_>, area: Rect) {

    let normal_mode_rows = [
        ("<i>", "Enter input mode"),
        ("<?>", "Toggle help popup"),
        ("<Tab>", "Switch tabs"),
        ("<q>", "Quit TUI"),
    ];

    let input_mode_rows = [
        ("<Enter>", "Execute command"),
        ("<Esc>", "Enter normal mode"),
    ];

    let mut lines = vec![];

    lines.push(Line::from(vec![
        Span::from("Normal mode")
            .style(Style::default().add_modifier(Modifier::BOLD)),
    ]));

    for (key, desc) in normal_mode_rows {
        let row = Line::from(vec![
            Span::from(format!("{:<15} ", key))
                .style(
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                ),
            Span::from(desc),
        ]);

        lines.push(row);
    }

    lines.push(Line::default());

    lines.push(Line::from(vec![
        Span::from("Input mode")
            .style(Style::default().add_modifier(Modifier::BOLD)),
    ]));

    for (key, desc) in input_mode_rows {
        let row = Line::from(vec![
            Span::from(format!("{:<15} ", key))
                .style(
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                ),
            Span::from(desc),
        ]);

        lines.push(row);
    }

    lines.push(Line::default());

    lines.push(
        Line::from("Press <?> again in normal mode to close help")
            .alignment(Alignment::Center)
            .style(Style::default().fg(Color::Gray))
    );

    let longest_line = lines
        .iter()
        .map(|line| line.width()).max().unwrap_or(0) as u16 + 4;
    let lines_height = lines.len() as u16 + 2;

    let popup_area = area
        .centered(
            Constraint::Length(longest_line),
            Constraint::Length(lines_height)
        );

    let popup = Popup::default()
        .content(lines)
        .border_style(Style::default().fg(Color::Yellow))
        .padding(Padding::horizontal(1))
        .style(Style::default().fg(Color::White));

    frame.render_widget(
        popup,
        popup_area,
    );
}

/// Draw the main game tab, which includes the board, move list, and logs.
/// There are three main sections: the board on the left, and the move list
/// and logs stacked, in that order.
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
    let top_layout = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Fill(4),
            Constraint::Fill(1),
            Constraint::Fill(2),
        ])
        .split(top_rect);

    let board_area = top_layout[0]
        .centered_vertically(
            Constraint::Length(board.lines().count() as u16 + 1)
        );
    let moves_area = top_layout[1];

    let right_most_rect = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Min(0),
            Constraint::Length(3)
        ])
        .split(top_layout[2]);
    let details_area = right_most_rect[0];
    let fen_area = right_most_rect[1];

    let board_block = Block::default()
        .borders(Borders::NONE);
    let mut moves_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let mut details_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let mut fen_block = Block::default()
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

    let logs = LOG_MESSAGES.lock().unwrap_or_else(|e| {
        panic!("Failed to lock LOG_MESSAGES: {e}")
    });
    let log_lines = logs
        .iter()
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


    let mut final_moves_scroll = current_moves_scroll;
    let mut final_fen_scroll = current_fen_scroll;
    let mut final_logs_scroll = current_logs_scroll;

    if current_moves_scroll > max_moves_scroll
    && current_moves_scroll < u16::MAX {
        let offset = u16::MAX - current_moves_scroll;
        final_moves_scroll = max_moves_scroll.saturating_sub(offset);
        app.scroll_map.insert((app.tab, TAB_FOCUS_MOVES), final_moves_scroll);
    } else if current_moves_scroll > max_moves_scroll {
        final_moves_scroll = max_moves_scroll;
    } else if current_moves_scroll == max_moves_scroll {
        app.scroll_map.insert((app.tab, TAB_FOCUS_MOVES), u16::MAX);
    }

    if current_fen_scroll > max_fen_scroll
    && current_fen_scroll < u16::MAX {
        let offset = u16::MAX - current_fen_scroll;
        final_fen_scroll = max_fen_scroll.saturating_sub(offset);
        app.scroll_map.insert((app.tab, TAB_FOCUS_FEN), final_fen_scroll);
    } else if current_fen_scroll > max_fen_scroll {
        final_fen_scroll = max_fen_scroll;
    } else if current_fen_scroll == max_fen_scroll {
        app.scroll_map.insert((app.tab, TAB_FOCUS_FEN), u16::MAX);
    }

    if current_logs_scroll > max_logs_scroll
    && current_logs_scroll < u16::MAX {
        let offset = u16::MAX - current_logs_scroll;
        final_logs_scroll = max_logs_scroll.saturating_sub(offset);
        app.scroll_map.insert((app.tab, TAB_FOCUS_LOGS), final_logs_scroll);
    } else if current_logs_scroll > max_logs_scroll {
        final_logs_scroll = max_logs_scroll;
    } else if current_logs_scroll == max_logs_scroll {
        app.scroll_map.insert((app.tab, TAB_FOCUS_LOGS), u16::MAX);
    }

    moves_paragraph = moves_paragraph.scroll((final_moves_scroll, 0));
    fen_paragraph = fen_paragraph.scroll((0, final_fen_scroll));
    logs_paragraph = logs_paragraph.scroll((final_logs_scroll, 0));

    frame.render_widget(board_paragraph, board_area);
    frame.render_widget(moves_paragraph, moves_area);
    frame.render_widget(details_table, details_area);
    frame.render_widget(fen_paragraph, fen_area);
    frame.render_widget(logs_paragraph, logs_area);
}

fn render(frame: &mut Frame<'_>, app: &mut Tui) {
    let root = frame.area();

    if app.game_state.is_none() {
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
            _ => {},
        }

        draw_input(frame, chunks[2], app);
        draw_help_bar(frame, chunks[3], app);
    }

    if app.help {
        draw_help_popup(frame, root);
    }
}

fn execute_command(command: &str, state: &mut State, sender: Sender<TuiEvent>) {
    let trimmed = command.trim();

    if trimmed.is_empty() {
        return;
    }

    match trimmed {
        "u" => {
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
        "r" => {
            while state.ply_counter > 0 {
                undo_move!(state);

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
        _ if trimmed.starts_with("fen ") => {
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
        _ if trimmed.starts_with("search ") => {
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
            let result = search_position(state, &mut info);

            if result.best_move == null_move() {
                log_2!("No legal move available");
            }
        }
        _ if trimmed.starts_with("go ") => {
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
            let result = search_position(state, &mut info);

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
        _ if trimmed.starts_with("play ") => {
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

            while !state.game_over {
                let result = search_position(state, &mut info);

                if result.best_score == -INFINITY {
                    state.game_over = true;
                    log_1!(
                        "Checkmate! {} wins.",
                        if state.playing == WHITE { "Black" } else { "White" }
                    );
                    break;
                }

                if result.best_move == null_move() {
                    log_1!(
                        "Stalemate! It's a draw."
                    );
                    break;
                }
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
        }
        _ => {
            if let Some(mv) = parse_move(trimmed, state) {
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
                log_2!("Invalid command or move: {}", trimmed);
            }
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
            if app.locked {
                log_2!("Command execution in progress, please wait...");
                return false;
            }

            thread::spawn({
                let command = app.input.clone();
                let arc_state = app.game_state.as_mut().unwrap_or_else(
                    || {
                        panic!("Game state is None when executing command")
                    }
                ).clone();
                let sender = app.sender.clone();

                move || {
                    let mut state = arc_state.lock().unwrap_or_else(
                        |_| {
                            panic!(
                                concat!(
                                    "Failed to lock game state ",
                                    "for command execution"
                                )
                            )
                        }
                    );
                    execute_command(&command, &mut state, sender.clone());

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
            let scroll = app.scroll_map.get(&(app.tab, app.focus))
                .copied()
                .unwrap_or_else(
                    || {
                        panic!(
                            "Scroll value missing for tab {}, focus {}",
                            app.tab, app.focus
                        )
                    }
                ).saturating_add(1);
            app.scroll_map.insert((app.tab, app.focus), scroll);
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('k')) => {
            let scroll = app.scroll_map.get(&(app.tab, app.focus))
                .copied()
                .unwrap_or_else(
                    || {
                        panic!(
                            "Scroll value missing for tab {}, focus {}",
                            app.tab, app.focus
                        )
                    }
                ).saturating_sub(1);
            app.scroll_map.insert((app.tab, app.focus), scroll);
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Left) => {
            if app.focus == 0 {
                app.focus = (TAB_FOCUSABLES[app.tab] as usize)
                    .saturating_sub(1);
            } else {
                app.focus = app.focus
                    .saturating_sub(1)
            };
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Right) => {
            app.focus = app.focus
                .saturating_add(1);

            if app.focus >= TAB_FOCUSABLES[app.tab] as usize {
                app.focus = 0;
            }
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
        (TUI_NORMAL_MODE, KeyCode::Char('?')) => {
            app.help = !app.help;
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Tab) => {
            app.tab = (app.tab + 1) % TAB_TITLES.len();
            app.focus = 0;
            false
        },
        (TUI_NORMAL_MODE, KeyCode::Enter) if app.game_state.is_none() => {
            let filename = app.input.trim();
            let path = Path::new(CONFIGS_DIR).join(filename);

            app.input.clear();
            app.tab = 0;
            app.focus = 0;

            if path.is_file() {
                let path_str = path.to_string_lossy();
                let state = parse_config_file(&path_str);

                let board_state = BoardState::from_state(&state);

                app.board_state = Some(board_state);
                app.game_state = Some(Arc::new(Mutex::new(state)));
            } else {
                log_1!(
                    "Config file not found at path: {}", path.to_string_lossy()
                );
            }


            false
        },
        (TUI_NORMAL_MODE, KeyCode::Char('q')) => true,
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
    log_2!("Starting TUI...");
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
