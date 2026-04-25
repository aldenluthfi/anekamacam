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
const TAB_FOCUSABLES: [u8; 2] = [3, 1];

struct Tui {
    mode: u8,
    tab: usize,
    focus: usize,
    scroll_map: HashMap<(usize, usize), u16>,
    input: String,
    help: bool,
    fps: f64,
}

impl Tui {
    fn new() -> Self {
        let mut init_hashmap = HashMap::new();
        for tab in 0..TAB_TITLES.len() {
            for focus in 0..TAB_FOCUSABLES[tab] as usize {
                init_hashmap.insert((tab, focus), 0);
            }
        }
        Self {
            mode: TUI_NORMAL_MODE,
            tab: 0,
            focus: 0,
            scroll_map: init_hashmap,
            input: String::new(),
            help: false,
            fps: 0.0,
        }
    }

    fn run (
        &mut self, state: &mut State, terminal: &mut DefaultTerminal
    ) -> IoResult<()> {
        let mut frame_timer = 0;
        let mut frame_count = 0;

        loop {
            let frame_start = ENGINE_START.elapsed();

            terminal.draw(|frame| {
                render(frame, state, self);
            })?;

            if event::poll(Duration::from_millis(80))? {
                if let Event::Key(key_event) = event::read()? {
                    if key_event.kind == KeyEventKind::Press {
                        let quit = handle_key(
                            self,
                            state,
                            key_event.code,
                        );

                        if quit {
                            break;
                        }
                    }
                }
            }

            let frame_end = ENGINE_START.elapsed();
            let frame_time = frame_end.as_millis() - frame_start.as_millis();
            frame_count += 1;
            frame_timer += frame_time;

            if frame_timer >= 1000 {
                self.fps = frame_count as f64 / (frame_timer as f64 / 1000.0);
                frame_timer = 0;
                frame_count = 0;
            }
        }

        Ok(())
    }
}

#[derive(Debug, Default)]
struct Popup<'a> {
    content: Text<'a>,
    title_bottom: Line<'a>,
    border_style: Style,
    padding: Padding,
    style: Style,
}

impl<'a> Popup<'a> {

    fn content(mut self, content: impl Into<Text<'a>>) -> Self {
        self.content = content.into();
        self
    }

    fn title_bottom(mut self, title: impl Into<Line<'a>>) -> Self {
        self.title_bottom = title.into();
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
            .title_bottom(self.title_bottom)
            .border_style(self.border_style)
            .padding(self.padding);
        Paragraph::new(self.content)
            .wrap(Wrap { trim: true })
            .style(self.style)
            .block(block)
            .render(area, buf);
    }
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
            Span::raw("<Enter> Run | <Esc> Enter normal mode")
                .style(Style::default().fg(Color::Gray)),
        ])
    } else {
        Line::from(vec![
            Span::styled("[NORMAL] ", Style::default().fg(Color::Yellow)
                .add_modifier(Modifier::BOLD)),
            Span::raw(
                concat![
                    "<i> Enter input mode | <?> Help | <←/→> Focus | ",
                    "<j/k> Scroll | <Tab> Switch tab | <q> Quit",
                ]
            ).style(Style::default().fg(Color::Gray)),
        ])
    };

    frame.render_widget(help_line, area);
}

fn draw_help_popup(frame: &mut Frame<'_>, area: Rect, app: &Tui) {

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
        Line::from("Press <?> in normal mode again to close help")
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
        .title_bottom(Line::from(format!(" {:.1} FPS ", app.fps))
            .alignment(Alignment::Center)
            .style(Style::default().fg(Color::Yellow))
        )
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
fn draw_game_tab(
    frame: &mut Frame<'_>, area: Rect, state: &State, app: &mut Tui
) {

    let board = format_game_state(&state);

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
    let details_area = top_layout[2];

    let board_block = Block::default()
        .borders(Borders::NONE);
    let mut moves_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let mut details_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);
    let mut logs_block = Block::default()
        .padding(Padding::horizontal(1))
        .borders(Borders::ALL);

    let logs_area = main_layout[1];

    match app.focus {
        0 if app.mode == TUI_NORMAL_MODE =>
            moves_block = moves_block.border_style(
                Style::default().fg(Color::Yellow)
            ),
        1 if app.mode == TUI_NORMAL_MODE =>
            details_block = details_block.border_style(
                Style::default().fg(Color::Yellow)
            ),
        2 if app.mode == TUI_NORMAL_MODE =>
            logs_block = logs_block.border_style(
                Style::default().fg(Color::Yellow)
            ),
            _ => {}
    }

    let detail_columns = [
        Constraint::Fill(1),
        Constraint::Fill(2),
    ];
    let mut details = Vec::new();
    let position_hash = format_position_hash(state);
    let game_phase = format_game_phase(state);
    let search_ply = format!("{}", state.search_ply);

    let castling_rights;
    let en_passant;
    let halfmove_clock;
    let repetition_count;
    let hand_info;

    details.push(
        Row::new(
            vec!["Game Phase", &game_phase]
        )
    );
    details.push(
        Row::new(
            vec!["Turn", if state.playing == WHITE { "White" } else { "Black" }]
        )
    );
    details.push(
        Row::new(
            vec!["Position Hash", &position_hash]
        )
    );
    if castling!(state) {
        castling_rights = format_castling_rights(state);
        details.push(
            Row::new(
                vec!["Castling Rights", &castling_rights]
            )
        );
    }
    if en_passant!(state) {
        en_passant = format_en_passant_square(state);
        details.push(
            Row::new(
                vec!["En Passant", &en_passant]
            )
        );
    }
    if halfmove_clock!(state) {
        halfmove_clock = format!("{}/{}",
            state.halfmove_clock,
            state.halfmove_limit
        );
        details.push(
            Row::new(
                vec!["Halfmove Clock", &halfmove_clock]
            )
        );
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
        details.push(
            Row::new(
                vec!["Repetition Count", &repetition_count]
            )
        );
    }
    if drops!(state)
    || promote_to_captured!(state)
    || setup_phase!(state) {
        hand_info = format!(
            "White [{}] | Black [{}]\n",
            format_hand(state, WHITE),
            format_hand(state, BLACK)
        );
        details.push(
            Row::new(
                vec!["Pieces in Hand", &hand_info]
            )
        );
    }
    details.push(
        Row::new(
            vec!["Search depth", &search_ply]
        )
    );

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

            if line.starts_with('[') {
                if let Some(end_idx) = line.find(']') {
                    let (level, rest) = line.split_at(end_idx + 1);
                    return Line::from(vec![
                        Span::styled(
                            level.to_string(), Style::default().fg(level_color)
                        ),
                        Span::raw(rest.to_string()),
                    ]);
                }
            }

            Line::from(Span::raw(line.clone()))
        })
        .collect::<Vec<_>>();

    let board_paragraph = Paragraph::new(board)
        .alignment(Alignment::Center)
        .block(board_block);
    let mut moves_paragraph = Paragraph::new(format_move_history(state))
        .wrap(Wrap { trim: true })
        .block(moves_block);
    let details_table = Table::new(details.clone(), detail_columns)
        .header(
            Row::new(vec!["Parameter", "Value"])
                .style(
                    Style::default()
                        .fg(Color::Yellow)
                        .add_modifier(Modifier::BOLD)
                    )
        )
        .block(details_block);
    let mut logs_paragraph = Paragraph::new(Text::from(log_lines))
        .wrap(Wrap { trim: true })
        .block(logs_block);

    let max_moves_scroll = (moves_paragraph.line_count(moves_area.width) as u16)
        .saturating_sub(moves_area.height);
    let max_logs_scroll = (logs_paragraph.line_count(logs_area.width) as u16)
        .saturating_sub(logs_area.height);
    let max_details_scroll = (details.len() as u16)
        .saturating_sub(details_area.height);
    let current_moves_scroll = app.scroll_map.get(&(app.tab, 0)).copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus 0",
                app.tab
            )
        }
    );
    let current_details_scroll = app.scroll_map.get(&(app.tab, 1)).copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus 1",
                app.tab
            )
        }
    );
    let current_logs_scroll = app.scroll_map.get(&(app.tab, 2)).copied()
        .unwrap_or_else(
        || {
            panic!(
                "Scroll value missing for tab {}, focus 2",
                app.tab
            )
        }
    );

    let mut final_moves_scroll = current_moves_scroll;
    let mut final_logs_scroll = current_logs_scroll;

    if current_moves_scroll > max_moves_scroll
    && current_moves_scroll < u16::MAX {
        let offset = u16::MAX - current_moves_scroll;
        final_moves_scroll = max_moves_scroll - offset;
        app.scroll_map.insert((app.tab, 0), final_moves_scroll);
    } else if current_moves_scroll > max_moves_scroll {
        final_moves_scroll = max_moves_scroll;
    } else if current_moves_scroll == max_moves_scroll {
        app.scroll_map.insert((app.tab, 0), u16::MAX);
    }

    if current_logs_scroll > max_logs_scroll
    && current_logs_scroll < u16::MAX {
        let offset = u16::MAX - current_logs_scroll;
        final_logs_scroll = max_logs_scroll - offset;
        app.scroll_map.insert((app.tab, 2), final_logs_scroll);
    } else if current_logs_scroll > max_logs_scroll {
        final_logs_scroll = max_logs_scroll;
    } else if current_logs_scroll == max_logs_scroll {
        app.scroll_map.insert((app.tab, 2), u16::MAX);
    }

    moves_paragraph = moves_paragraph.scroll((final_moves_scroll, 0));
    logs_paragraph = logs_paragraph.scroll((final_logs_scroll, 0));

    frame.render_widget(board_paragraph, board_area);
    frame.render_widget(moves_paragraph, moves_area);
    frame.render_widget(details_table, details_area);
    frame.render_widget(logs_paragraph, logs_area);
}

fn render(frame: &mut Frame<'_>, state: &State, app: &mut Tui) {
    let root = frame.area();

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
        0 => draw_game_tab(frame, chunks[1], state, app),
        _ => {},
    }

    draw_input(frame, chunks[2], app);
    draw_help_bar(frame, chunks[3], app);

    if app.help {
        draw_help_popup(frame, root, app);
    }
}

fn execute_command(state: &mut State, command: &str) {
    let trimmed = command.trim();

    if trimmed.is_empty() {
        return;
    }

       match trimmed {
        "u" => {
            if state.ply_counter > 0 {
                undo_move!(state);
            } else {
                log_2!("Nothing to undo");
            }
        }
        "r" => {
            while state.ply_counter > 0 {
                undo_move!(state);
            }
        }
        _ if trimmed.starts_with("fen ") => {
            let fen = trimmed[4..].trim();
            state.load_fen(fen);
            log_2!("Loaded FEN");
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

            let mut info = SearchInfo::default();
            info.set_depth = depth;
            search_position(state, &mut info);
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

            let mut info = SearchInfo::default();
            info.set_depth = depth;
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
        }
        _ => {
            if let Some(mv) = parse_move(trimmed, state) {
                make_move!(state, mv);
            } else {
                log_2!("Invalid command or move: {}", trimmed);
            }
        }
    }
}

fn handle_key(
    app: &mut Tui,
    state: &mut State,
    code: KeyCode,
) -> bool {

    let result = match (app.mode, code) {
        (TUI_INPUT_MODE, KeyCode::Enter) => {
            execute_command(state, &app.input);
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
        (TUI_NORMAL_MODE, KeyCode::Char('i')) => {
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
        (TUI_NORMAL_MODE, KeyCode::Char('q')) => true,
        _ => false,
    };

    result
}

pub fn tui(state: &mut State) -> IoResult<()> {
    log_2!("Starting TUI...");
    let mut terminal = ratatui::init();
    execute!(terminal.backend_mut(), EnableMouseCapture)?;

    let run_result = Tui::new().run(state, &mut terminal);
    let mouse_result = execute!(terminal.backend_mut(), DisableMouseCapture);

    ratatui::restore();

    run_result?;
    mouse_result?;

    Ok(())
}
