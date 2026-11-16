//! physical.rs
//!
//! Console frontend of the physical chess robot.
//!
//! The console module has a 2.4 inch ST7789 screen of 320 by 240 pixels, an
//! EC11 knob and one button. The screen stands upright, so with an 8 by 16
//! console font it has 30 columns and 20 rows. A knob turn arrives as a
//! left or right arrow key, and the button as enter. This file runs the
//! console pages and talks to the robot microcontroller over CEP.
//!
//! Created: 06/11/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                   CONSTANTS
\*----------------------------------------------------------------------------*/

/// Interface constants
///
/// Fixed values of the console.
///
/// - WORKER_STACK_BYTES : stack of the game thread, as a protocol search
/// - FRAME_WAIT_MS      : longest wait for a key between two frames
/// - LABEL_WIDTH        : columns of the label cell of a settings row
/// - MOVE_NUMBER_WIDTH  : columns of the number cell of a move list row
/// - LAYOUT_FILE        : the panel layout guide in `EMBEDDED_LAYOUTS`
/// - PANEL_COLORS       : colour of each panel kind, in the set order
/// - FOCUS_COLOR        : the focused control, and the focused piece squares
/// - WRONG_COLOR        : a square that the last board check found wrong
/// - DARK_COLOR         : a dark square of the picker and the pieces part
/// - CHECK_INTERVAL_MS  : time between two board checks of the guide
///
/// Notes:
/// The panel colours follow the order of the panel set in the guide header,
/// so a new set needs no code change. They run loosely through the rainbow,
/// red to violet, from the smallest panel to the largest. Only the light
/// squares show a colour, the dark squares stay as the screen background.
/// The panel colours leave out yellow and red, so a focused or a wrong
/// square differs from each panel.
///
const WORKER_STACK_BYTES: usize = 64 * 1024 * 1024;
const FRAME_WAIT_MS: u64 = 100;

const LABEL_WIDTH: u16 = 10;
const MOVE_NUMBER_WIDTH: u16 = 4;                                               /* three digits and a dot             */

const LAYOUT_FILE: &str = "layouts_16x16.txt";
const PANEL_COLORS: [Color; 10] = [
    Color::LightRed,                                                            /* 1x1 in the current set, red        */
    Color::LightYellow,                                                         /* 1x2, yellow                        */
    Color::LightGreen,                                                          /* 1x6, green                         */
    Color::Green,                                                               /* 2x2, green                         */
    Color::LightCyan,                                                           /* 2x5, green to blue                 */
    Color::Cyan,                                                                /* 2x8, green to blue                 */
    Color::LightBlue,                                                           /* 3x4, blue                          */
    Color::Blue,                                                                /* 4x7, indigo                        */
    Color::LightMagenta,                                                        /* 5x10, violet                       */
    Color::Magenta,                                                             /* 10x11, violet                      */
];
const FOCUS_COLOR: Color = Color::Yellow;
const WRONG_COLOR: Color = Color::Red;
const DARK_COLOR: Color = Color::DarkGray;

const CHECK_INTERVAL_MS: u64 = 10_000;

/// Settings constants
///
/// The values that the settings page offers, and the rows of the page. A
/// knob turn in edit mode moves a value one place along its list.
///
/// - THINK_TIMES_MS        : search time of each move
/// - HASH_SIZES_MB         : size of the two hash tables together
/// - THINK_ROW .. HASH_ROW : the rows, in display order
/// - SETTING_LABELS        : the label of each row
///
const THINK_TIMES_MS: [u128; 11] = [
    100, 250, 500, 1_000, 2_000, 3_000, 5_000, 10_000, 15_000, 30_000, 60_000,
];
const HASH_SIZES_MB: [usize; 7] = [16, 32, 64, 128, 256, 512, 1_024];

const THINK_ROW: usize = 0;
const THREADS_ROW: usize = 1;
const HASH_ROW: usize = 2;

const SETTING_LABELS: [&str; 3] = ["Think time", "Threads", "Hash"];

/*----------------------------------------------------------------------------*\
                                 INDEX STEPS
\*----------------------------------------------------------------------------*/

/// Index steps
///
/// Move an index by one knob turn. A list that the knob runs around wraps
/// at its two ends. A list with a first and a last place stops there.
///
/// step_wrapped
///
///   Params:
///   - index: usize -> current place
///   - step : isize -> the knob turn, -1 left or 1 right
///   - count: usize -> length of the list
///
///   Return:
///   usize          -> new place, wrapped into the list
///
/// step_clamped
///
///   Params:
///   - index: usize -> current place
///   - step : isize -> the knob turn, -1 left or 1 right
///   - count: usize -> length of the list
///
///   Return:
///   usize          -> new place, held inside the list
///
fn step_wrapped(index: usize, step: isize, count: usize) -> usize {
    (index as isize + step).rem_euclid(count as isize) as usize
}

fn step_clamped(index: usize, step: isize, count: usize) -> usize {
    (index as isize + step).clamp(0, count as isize - 1) as usize
}

/// max_threads
///
/// Gives the number of processors, the most search workers of a game.
///
/// Return:
/// usize -> processor count, or 1 if the system does not say
///
fn max_threads() -> usize {
    thread::available_parallelism()
        .map(|count| count.get())
        .unwrap_or(1)
}

/*----------------------------------------------------------------------------*\
                                    SETTINGS
\*----------------------------------------------------------------------------*/

/// Settings
///
/// The game settings that the settings page changes. The game thread gets a
/// copy at its start, so a change applies to the next game.
///
#[derive(Clone, Copy)]
struct Settings {
    think_index: usize,                                                         /* index into THINK_TIMES_MS          */
    threads: usize,                                                             /* search workers                     */
    hash_index: usize,                                                          /* index into HASH_SIZES_MB           */
}

impl Settings {
    /// Settings methods
    ///
    /// The start values, the text of a row and a change of a row.
    ///
    /// new
    ///
    ///   Gives one second for each move, all processors and 256 MB.
    ///
    ///   Return:
    ///   Self -> the start settings
    ///
    /// value
    ///
    ///   Writes the value of one row.
    ///
    ///   Params:
    ///   - row: usize -> the settings row
    ///
    ///   Return:
    ///   String       -> the value text
    ///
    /// adjust
    ///
    ///   Moves the value of one row one place along its list. The value
    ///   stops at the two ends.
    ///
    ///   Params:
    ///   - row : usize -> the settings row
    ///   - step: isize -> the knob turn, -1 left or 1 right
    ///
    fn new() -> Self {
        Self {
            think_index: 3,                                                     /* 1_000 ms                           */
            threads: max_threads(),
            hash_index: 4,                                                      /* 256 MB                             */
        }
    }

    fn value(&self, row: usize) -> String {
        match row {
            THINK_ROW => {
                let milliseconds = THINK_TIMES_MS[self.think_index] as f64;
                let seconds = milliseconds / 1000.0;                            /* milliseconds in a second           */
                format!("{} s", seconds)
            }
            THREADS_ROW => self.threads.to_string(),
            HASH_ROW => format!("{} MB", HASH_SIZES_MB[self.hash_index]),
            _ => String::new(),
        }
    }

    fn adjust(&mut self, row: usize, step: isize) {
        match row {
            THINK_ROW => {
                self.think_index =
                    step_clamped(self.think_index, step, THINK_TIMES_MS.len());
            }
            THREADS_ROW => {
                self.threads =
                    step_clamped(self.threads - 1, step, max_threads()) + 1;
            }
            HASH_ROW => {
                self.hash_index =
                    step_clamped(self.hash_index, step, HASH_SIZES_MB.len());
            }
            _ => {}
        }
    }
}

/*----------------------------------------------------------------------------*\
                                 PANEL LAYOUTS
\*----------------------------------------------------------------------------*/

/// PanelKind
///
/// One kind of panel of the set: its size and its colour tag. The squares
/// of a panel are printed in the board colours, so a tagged panel fits only
/// where the board has its colours:
///
/// - `B`, `W` : both sides odd, the corner squares are black or white
/// - `b`, `w` : both sides even, not square, top left colour when flat
/// - no tag   : a turn of the panel fits it to either colouring
///
/// A flat panel has its short side as height. A panel that stands has the
/// other colour at its top left, so one tag holds for the two ways.
///
#[derive(Clone, Copy)]
struct PanelKind {
    short: u8,                                                                  /* short side in squares              */
    long: u8,                                                                   /* long side in squares               */
    tag: Option<char>,                                                          /* colour tag, None for no tag        */
}

/// RoomPlacement
///
/// One way to lay a layout of the guide on a board: with its rows as ranks
/// or as files, and mirrored along the files, the ranks, or both.
///
#[derive(Clone, Copy)]
struct RoomPlacement {
    transposed: bool,                                                           /* the layout rows are the files      */
    flip_files: bool,                                                           /* mirrored from left to right        */
    flip_ranks: bool,                                                           /* mirrored from top to bottom        */
}

/// parse_panel_kinds
///
/// Reads the panel set from the guide header: one kind for each token, in
/// the order of the header.
///
/// - `Piece set: 1*1x1[B] 2*1x2 ...` : count, size and an optional tag
///
/// Params:
/// - text: &str     -> the guide file
///
/// Return:
/// Vec<PanelKind>   -> the panel kinds, in header order
///
fn parse_panel_kinds(text: &str) -> Vec<PanelKind> {
    let Some(set) = text
        .lines()
        .find_map(|line| line.strip_prefix("Piece set:"))
    else {
        return Vec::new();
    };

    set.split_whitespace()
        .filter_map(|token| {
            let (_, kind) = token.split_once('*')?;
            let (size, tag) = match kind.split_once('[') {
                Some((size, tag)) => (size, tag.chars().next()),
                None => (kind, None),
            };
            let (short, long) = size.split_once('x')?;

            Some(PanelKind {
                short: short.parse().ok()?,
                long: long.parse().ok()?,
                tag,
            })
        })
        .collect()
}

/// panel_fits
///
/// Tells if a panel at its place shows the board colours. A size with more
/// than one kind fits when one of its kinds fits. A size outside the set
/// fails.
///
/// Params:
/// - panel      : &Panel       -> the panel and its place
/// - kinds      : &[PanelKind] -> the panel set
/// - first_light: bool         -> true when a1 is a light square
///
/// Return:
/// bool                        -> true when the panel colours fit
///
fn panel_fits(panel: &Panel, kinds: &[PanelKind], first_light: bool) -> bool {
    let is_light = |file: u8, rank: u8| {
        ((file + rank) % 2 == 1) != first_light                                 /* an odd sum is light with dark a1   */
    };

    let short = panel.files.min(panel.ranks);
    let long = panel.files.max(panel.ranks);
    let lies_flat = panel.ranks <= panel.files;
    let corner_light = is_light(panel.file, panel.rank);
    let top_left_light = is_light(panel.file, panel.rank + panel.ranks - 1);

    kinds
        .iter()
        .filter(|kind| kind.short == short && kind.long == long)
        .any(|kind| match kind.tag {
            Some('B') => !corner_light,
            Some('W') => corner_light,
            Some('b') => top_left_light != lies_flat,
            Some('w') => top_left_light == lies_flat,
            _ => true,
        })
}

/// parse_panel_layouts
///
/// Reads the panel layout guide: one way to build each board size from the
/// panel set. The file has a layout for every size up to 16 by 16, so the
/// console offers exactly the variants of that size.
///
/// - `Room <a>x<b>: ...` : a layout of a rows and b columns, a <= b
/// - `   ABB`            : one row of that layout, one letter for each panel
///
/// Params:
/// - text: &str                      -> the guide file
///
/// Return:
/// HashMap<(u8, u8), Vec<Vec<char>>> -> layout rows by (rows, columns)
///
fn parse_panel_layouts(text: &str) -> HashMap<(u8, u8), Vec<Vec<char>>> {
    let mut layouts: HashMap<(u8, u8), Vec<Vec<char>>> = HashMap::new();
    let mut room = None;

    for line in text.lines() {
        if let Some(header) = line.strip_prefix("Room ") {
            room = header
                .split(':')
                .next()
                .and_then(|size| size.split_once('x'))
                .and_then(|(rows, columns)| {
                    Some((rows.parse().ok()?, columns.parse().ok()?))
                });
        } else if let Some(size) = room && line.starts_with("   ") {
            let row: Vec<char> = line.trim().chars().collect();
            layouts.entry(size).or_default().push(row);
        }
    }

    layouts
}

/// panel_grid
///
/// Gives the panel letter of each square of a board, in board order, for
/// one placement of a layout. A placement whose size is not the board size
/// gives None.
///
/// Params:
/// - rows     : &[Vec<char>]  -> the layout rows, from `parse_panel_layouts`
/// - files    : u8            -> board width
/// - ranks    : u8            -> board height
/// - placement: RoomPlacement -> how the layout lies on the board
///
/// Return:
/// Option<Vec<char>>          -> a letter for each square, or None
///
fn panel_grid(
    rows: &[Vec<char>],
    files: u8,
    ranks: u8,
    placement: RoomPlacement,
) -> Option<Vec<char>> {
    let (width, height) = (files as usize, ranks as usize);
    let mut grid = Vec::with_capacity(width * height);

    for rank in 0..height {
        for file in 0..width {
            let room_file = match placement.flip_files {
                true => width - 1 - file,
                false => file,
            };
            let room_rank = match placement.flip_ranks {
                true => height - 1 - rank,
                false => rank,
            };
            let letter = match placement.transposed {
                true => rows.get(room_file)?.get(room_rank)?,
                false => rows.get(height - 1 - room_rank)?.get(room_file)?,
            };

            grid.push(*letter);
        }
    }

    Some(grid)
}

/// choose_layout
///
/// Finds a placement of a layout whose tagged panels show the board
/// colours. A dark a1 comes first, as on a chess board. Some layouts fit
/// only a board with a light a1, and then the board has that colouring.
///
/// Params:
/// - rows : &[Vec<char>]  -> the layout of the board size
/// - files: u8            -> board width
/// - ranks: u8            -> board height
/// - kinds: &[PanelKind]  -> the panel set
///
/// Return:
///
///     Option<(Vec<(Panel, usize)>, bool)>
///     panels with kinds and a light a1 flag, None without a fit
///
fn choose_layout(
    rows: &[Vec<char>],
    files: u8,
    ranks: u8,
    kinds: &[PanelKind],
) -> Option<(Vec<(Panel, usize)>, bool)> {
    let choices = [false, true];

    for first_light in choices {
        for transposed in choices {
            for flip_files in choices {
                for flip_ranks in choices {
                    let placement = RoomPlacement {
                        transposed, flip_files, flip_ranks
                    };
                    let Some(grid) = panel_grid(rows, files, ranks, placement)
                    else {
                        continue;
                    };
                    let panels = grid_panels(&grid, files, kinds);
                    let all_fit = panels.iter().all(|(panel, _)| {
                        panel_fits(panel, kinds, first_light)
                    });

                    if all_fit {
                        return Some((panels, first_light));
                    }
                }
            }
        }
    }

    None
}

/// grid_panels
///
/// Gives the panels of a letter grid, one for each letter, and the kind of
/// each panel: its place in the panel set, found by its size.
///
/// Params:
/// - grid : &[char]      -> a letter for each square, from `panel_grid`
/// - files: u8           -> board width
/// - kinds: &[PanelKind] -> the panel set, from `parse_panel_kinds`
///
/// Return:
/// Vec<(Panel, usize)>   -> corner, size and kind of each panel
///
fn grid_panels(
    grid: &[char],
    files: u8,
    kinds: &[PanelKind],
) -> Vec<(Panel, usize)> {
    let width = files as usize;
    let mut letters = grid.to_vec();
    letters.sort_unstable();
    letters.dedup();

    letters
        .iter()
        .map(|letter| {
            let squares: Vec<usize> = (0..grid.len())
                .filter(|square| grid[*square] == *letter)
                .collect();

            let first_file = squares.iter().map(|square| square % width).min();
            let last_file = squares.iter().map(|square| square % width).max();
            let first_rank = squares.iter().map(|square| square / width).min();
            let last_rank = squares.iter().map(|square| square / width).max();

            let first_file = first_file.unwrap_or(0);
            let first_rank = first_rank.unwrap_or(0);

            let panel = Panel {
                file: first_file as u8,
                rank: first_rank as u8,
                files: (last_file.unwrap_or(0) + 1 - first_file) as u8,
                ranks: (last_rank.unwrap_or(0) + 1 - first_rank) as u8,
            };
            let short = panel.files.min(panel.ranks);
            let long = panel.files.max(panel.ranks);
            let kind = kinds
                .iter()
                .position(|kind| kind.short == short && kind.long == long);

            (panel, kind.unwrap_or(0))
        })
        .collect()
}

/*----------------------------------------------------------------------------*\
                                INTERFACE STATE
\*----------------------------------------------------------------------------*/

/// VariantEntry
///
/// One variant that the panel set can build. It is read once at startup,
/// and each frame uses this copy.
///
struct VariantEntry {
    file: String,                                                               /* embedded config file name          */
    title: String,                                                              /* variant title of the config        */
    placement: String,                                                          /* start position, a FEN board field  */
    pieces: Vec<char>,                                                          /* start piece of each square         */
    files: u8,                                                                  /* board width                        */
    ranks: u8,                                                                  /* board height                       */
    panels: Vec<(Panel, usize)>,                                                /* layout guide, panel and its kind   */
    first_light: bool,                                                          /* the panels make a1 a light square  */
}

impl VariantEntry {
    /// Board geometry
    ///
    /// The squares of the board in the ways that the guide needs them. A
    /// square index is `rank * files + file`, with rank 0 at the bottom.
    ///
    /// VariantEntry::is_light
    ///
    ///   Params:
    ///   - square: usize -> square index
    ///
    ///   Return:
    ///   bool            -> true when the square is a light square
    ///
    /// VariantEntry::used_pieces
    ///
    ///   The piece letters of the start placement, each one once, in ASCII
    ///   order: the white letters first, then the black. The piece list
    ///   shows them in this order.
    ///
    ///   Return:
    ///   Vec<char>       -> the piece letters
    ///
    /// VariantEntry::used_kinds
    ///
    ///   The panel kinds of the layout guide, each one once, in the set
    ///   order. The legend shows them in this order.
    ///
    ///   Return:
    ///   Vec<usize>      -> indices into the panel set
    ///
    /// VariantEntry::square_kinds
    ///
    ///   Return:
    ///   Vec<usize>      -> the panel kind of the guide on each square
    ///
    fn is_light(&self, square: usize) -> bool {
        let files = self.files as usize;
        let odd_sum = (square % files + square / files) % 2 == 1;               /* an odd sum is light with dark a1   */

        odd_sum != self.first_light
    }

    fn used_pieces(&self) -> Vec<char> {
        let mut used: Vec<char> = self
            .pieces
            .iter()
            .copied()
            .filter(|piece| *piece != CEP_EMPTY)
            .collect();
        used.sort_unstable();
        used.dedup();

        used
    }

    fn used_kinds(&self) -> Vec<usize> {
        let mut used: Vec<usize> =
            self.panels.iter().map(|(_, kind)| *kind).collect();
        used.sort_unstable();
        used.dedup();

        used
    }

    fn square_kinds(&self) -> Vec<usize> {
        let files = self.files as usize;
        let mut kinds = vec![0; files * self.ranks as usize];

        for (panel, kind) in &self.panels {
            for rank in panel.rank..panel.rank + panel.ranks {
                for file in panel.file..panel.file + panel.files {
                    kinds[rank as usize * files + file as usize] = *kind;
                }
            }
        }

        kinds
    }
}

/// BoardCheck
///
/// The result of the last board check of the guide. A check that got a
/// board marks each wrong square. A check that got no board has the robot
/// problem, and no square is marked.
///
struct BoardCheck {
    wrong: Vec<bool>,                                                           /* each square, true when it is wrong */
    problem: Option<String>,                                                    /* robot problem, no board was read   */
}

impl BoardCheck {
    /// BoardCheck::passed
    ///
    /// Tells if the check found the board right.
    ///
    /// Return:
    /// bool -> true with a board read and no wrong square
    ///
    fn passed(&self) -> bool {
        self.problem.is_none() && !self.wrong.contains(&true)
    }
}

/// GuidePhase
///
/// The two parts of the setup guide. The board is checked again and again
/// in each part, and Next opens when a check passes. Next after the second
/// part starts the game.
///
/// - Panels : build the board from the panels, the check reads the panels
/// - Pieces : put the pieces, the check reads panels and occupancy
///
#[derive(Clone, Copy, PartialEq)]
enum GuidePhase {
    Panels,
    Pieces,
}

/// Page
///
/// The page on the screen, with its own state.
///
/// - Picker   : one variant at a time, its board shape
/// - Settings : the settings rows, and the edit mode of the focused row
/// - Guide    : the setup guide of one variant, and its last board check
/// - Status   : the moves of the game, and the stop dialog if it is open
/// - Help     : the help of the guide part before it, which OK goes back to
///
enum Page {
    Picker,
    Settings {
        editing: bool,                                                          /* the knob changes the focused value */
    },
    Guide {
        variant: usize,                                                         /* index into the variants            */
        phase: GuidePhase,                                                      /* panels first, then pieces          */
        check: Option<BoardCheck>,                                              /* last check of this part, if any    */
    },
    Status {
        variant: usize,                                                         /* index into the variants            */
        asking: bool,                                                           /* the stop question is open          */
    },
    Help {
        previous: Box<Page>,                                                    /* the page to go back to, as it was  */
    },
}

/// Control
///
/// One focusable control of a page. The knob moves the focus along the
/// controls of the page and wraps at the two ends. The button presses the
/// focused control.
///
/// - Exit           : leave the console, on the picker without a robot link
/// - Help           : open the help of a guide part, at the top right
/// - Previous, Next : the variant before or after, round the list
/// - Settings, Play : open the settings page, or the setup guide
/// - Setting(row)   : one settings row, a press starts or ends its edit
/// - Panel(kind)    : one legend label, its panels show whole in colour
/// - Piece(letter)  : one piece list letter, its squares show in yellow
/// - Back           : the page or the guide part before
/// - Proceed        : the next guide part or the game, after a check passes
/// - Ok             : leave the help page
/// - Stop           : open the stop dialog
/// - No, Yes        : answer the stop dialog
///
#[derive(Clone, Copy, PartialEq)]
enum Control {
    Exit,
    Help,
    Previous,
    Settings,
    Play,
    Next,
    Setting(usize),
    Panel(usize),
    Piece(char),
    Back,
    Proceed,
    Ok,
    Stop,
    No,
    Yes,
}

/// Robot
///
/// The robot end of the console. A console without a device is in debug
/// mode: each board check passes and the game runs without a robot. A
/// device that did not open fails each check, so only a working link passes
/// one.
///
/// - Debug  : no device given
/// - Broken : the device did not open, with the reason
/// - Linked : the device answered `hello`, with its name
///
enum Robot {
    Debug,
    Broken(String),
    Linked(Arc<Mutex<CepLink>>, String),
}

/// Physical
///
/// All state of the console. The game thread owns the position and sends
/// events, so this state has only drawn text.
///
/// - variants, panel_kinds      : the buildable variants and the panel set
/// - selected                   : the variant on the picker
/// - page, focus                : the shown page and its focused control
/// - settings                   : the settings of the next game
/// - robot                      : the robot link, debug mode, or a fault
/// - scan_reply, last_scan      : the `scan` in flight, the last start
/// - quit                       : the Exit control was pressed
/// - worker_running             : a game thread is alive
/// - board_state                : the drawn game, for its move list
/// - receiver                   : events from the engine
///
struct Physical {
    variants: Vec<VariantEntry>,                                                /* variants that the panels can build */
    panel_kinds: Vec<PanelKind>,                                                /* panel kinds, in the set order      */
    selected: usize,                                                            /* the variant on the picker          */

    page: Page,                                                                 /* shown page                         */
    focus: Control,                                                             /* focused control of the page        */
    settings: Settings,                                                         /* settings of the next game          */

    robot: Robot,                                                               /* robot link, debug mode, or a fault */

    scan_reply: Option<Receiver<Result<CepReply, String>>>,                     /* reply of the scan thread, if any   */
    last_scan: Instant,                                                         /* start of the last board check      */
    quit: bool,                                                                 /* the Exit control was pressed       */

    worker_running: bool,                                                       /* a game thread is alive             */
    board_state: Option<BoardState>,                                            /* rendered game snapshot             */

    receiver: Receiver<EngineEvent>,                                            /* engine-to-screen event channel     */
}

impl Physical {
    /// Physical::new
    ///
    /// Makes the console on the picker, with the first variant shown and
    /// Play focused. Only the variants that the layout guide can build,
    /// with the colour tags kept, are on the picker.
    ///
    /// Params:
    /// - receiver: Receiver<EngineEvent> -> events from the engine
    /// - robot   : Robot                 -> robot link, debug mode, or a fault
    ///
    /// Return:
    /// Self                              -> the console, on the picker
    ///
    fn new(receiver: Receiver<EngineEvent>, robot: Robot) -> Self {
        let layout_text = EMBEDDED_LAYOUTS
            .get_file(LAYOUT_FILE)
            .and_then(|layout| layout.contents_utf8())
            .unwrap_or_else(|| panic!("Missing layout file {}", LAYOUT_FILE));
        let panel_kinds = parse_panel_kinds(layout_text);
        let layouts = parse_panel_layouts(layout_text);

        let variants = variant_configs()
            .into_iter()
            .filter_map(|file| {
                let (title, placement) = parse_config_board(&file);
                let (files, ranks) = determine_board_dimensions(&placement);
                let rows = layouts.get(&(files.min(ranks), files.max(ranks)))?;

                let Some((panels, first_light)) =
                    choose_layout(rows, files, ranks, &panel_kinds)
                else {
                    log_2!("{}: no layout fits the panel colours", title);
                    return None;
                };

                if first_light {
                    log_3!("{}: the panels make a1 a light square", title);
                }

                let pieces = parse_placement(&placement, files, ranks)
                    .unwrap_or_else(|problem| panic!("{}: {}", title, problem));

                Some(VariantEntry {
                    file,
                    title,
                    placement,
                    pieces,
                    files,
                    ranks,
                    panels,
                    first_light
                })
            })
            .collect();

        Self {
            variants,
            panel_kinds,
            selected: 0,

            page: Page::Picker,
            focus: Control::Play,
            settings: Settings::new(),

            robot,

            scan_reply: None,
            last_scan: Instant::now(),
            quit: false,

            worker_running: false,
            board_state: None,

            receiver,
        }
    }

    /// Physical::controls
    ///
    /// Gives the focusable controls of the shown page, in focus order: the
    /// title pane first, from left to right, then the body and the control
    /// bar. An open dialog has only its own answers. A page that waits for
    /// the robot or the engine has none.
    ///
    /// Return:
    /// Vec<Control> -> the controls, in the order that the knob visits them
    ///
    fn controls(&self) -> Vec<Control> {
        let stopping = SYSTEM_INTERRUPT.load(Ordering::Relaxed);

        match &self.page {
            Page::Help { .. } => vec![Control::Ok],
            Page::Picker => {
                let exit = match self.robot {
                    Robot::Linked(..) => None,
                    _ => Some(Control::Exit),
                };
                let picker = [
                    Control::Previous,
                    Control::Settings,
                    Control::Play,
                    Control::Next,
                ];

                exit.into_iter().chain(picker).collect()
            }
            Page::Settings { .. } => (0..SETTING_LABELS.len())
                .map(Control::Setting)
                .chain([Control::Back])
                .collect(),
            Page::Guide { variant, phase, check } => {
                let entry = &self.variants[*variant];
                let body: Vec<Control> = match phase {
                    GuidePhase::Panels => entry
                        .used_kinds()
                        .into_iter()
                        .map(Control::Panel)
                        .collect(),
                    GuidePhase::Pieces => entry
                        .used_pieces()
                        .into_iter()
                        .map(Control::Piece)
                        .collect(),
                };
                let proceed = check
                    .as_ref()
                    .is_some_and(BoardCheck::passed)
                    .then_some(Control::Proceed);

                [Control::Help]
                    .into_iter()
                    .chain(body)
                    .chain([Control::Back])
                    .chain(proceed)
                    .collect()
            }
            Page::Status { asking: true, .. } => {
                vec![Control::No, Control::Yes]
            }
            Page::Status { .. } if self.worker_running && stopping => {
                Vec::new()
            }
            Page::Status { .. } if self.worker_running => vec![Control::Stop],
            Page::Status { .. } => vec![Control::Back],
        }
    }

    /// Physical::focused
    ///
    /// Gives the control that has the focus. When the stored focus is not
    /// on the page, such as after a game ends, the first control of the
    /// page has it.
    ///
    /// Return:
    /// Option<Control> -> the focused control, None on a page without one
    ///
    fn focused(&self) -> Option<Control> {
        let controls = self.controls();

        match controls.contains(&self.focus) {
            true => Some(self.focus),
            false => controls.first().copied(),
        }
    }

    /// Setup control
    ///
    /// The way from the picker to a game. The guide sends `setup` when it
    /// opens. In each part, a `scan` reads the board at once and then each
    /// `CHECK_INTERVAL_MS`, and Next opens while the last check passes:
    ///
    /// ```text
    /// panels -> scan until right -> Next -> pieces -> scan until right
    ///        -> Next -> game
    /// ```
    ///
    /// enter_guide
    ///
    ///   Opens the panels part of the guide. A `setup` problem shows as the
    ///   result of the first check, and the next check comes after the
    ///   interval.
    ///
    ///   Params:
    ///   - variant: usize -> index into the variants
    ///
    /// open_part
    ///
    ///   Shows one part of the guide with the first label of its list
    ///   focused. The part has no check yet, so a check starts at once.
    ///
    ///   Params:
    ///   - phase: GuidePhase -> the part to show
    ///
    /// check_board
    ///
    ///   Runs on each frame. It takes the reply of a `scan` in flight, or
    ///   starts a `scan` when the part has no check or the interval is over.
    ///   The `scan` runs on its own thread, so the knob works while the
    ///   robot reads the board. No parameters and no return value.
    ///
    /// start_game
    ///
    ///   Opens the status page with Stop focused and starts the game thread
    ///   with a copy of the settings.
    ///
    ///   Params:
    ///   - variant: usize -> index into the variants
    ///
    /// Notes:
    /// A new game waits for the last game thread, because the two would
    /// share the interrupt flag. A reply of a part that is not on the screen
    /// any more goes nowhere, because its channel is dropped.
    ///
    fn enter_guide(&mut self, variant: usize) {
        let entry = &self.variants[variant];

        let setup = CepRequest::Setup {
            files: entry.files,
            ranks: entry.ranks,
            placement: &entry.placement,
        };
        let problem = match &self.robot {
            Robot::Debug | Robot::Broken(_) => None,
            Robot::Linked(link, _) => {
                link.lock().unwrap().request(&setup).err()
            }
        };
        let squares = entry.pieces.len();

        self.page = Page::Guide {
            variant,
            phase: GuidePhase::Panels,
            check: None,
        };
        self.open_part(GuidePhase::Panels);

        if let Some(problem) = problem
            && let Page::Guide { check, .. } = &mut self.page
        {
            log_2!("Setup problem: {}", problem);
            *check = Some(BoardCheck {
                wrong: vec![false; squares],
                problem: Some(problem),
            });
            self.last_scan = Instant::now();
        }
    }

    fn open_part(&mut self, part: GuidePhase) {
        let Page::Guide { variant, phase, check } = &mut self.page else {
            return;
        };
        let entry = &self.variants[*variant];

        *phase = part;
        *check = None;
        self.scan_reply = None;
        self.focus = match part {
            GuidePhase::Panels => Control::Panel(entry.used_kinds()[0]),
            GuidePhase::Pieces => Control::Piece(entry.used_pieces()[0]),
        };
    }

    fn check_board(&mut self) {
        let Page::Guide { variant, phase, check } = &mut self.page else {
            self.scan_reply = None;
            return;
        };
        let entry = &self.variants[*variant];
        let squares = entry.pieces.len();

        if let Some(scan_reply) = &self.scan_reply {
            let reply = match scan_reply.try_recv() {
                Ok(reply) => reply,
                Err(TryRecvError::Empty) => return,
                Err(TryRecvError::Disconnected) => {
                    Err("The scan thread failed".to_string())
                }
            };

            *check = Some(read_check(entry, *phase, reply, &self.panel_kinds));
            self.scan_reply = None;
            return;
        }

        let interval = Duration::from_millis(CHECK_INTERVAL_MS);

        if check.is_some() && self.last_scan.elapsed() < interval {
            return;
        }

        self.last_scan = Instant::now();

        match &self.robot {
            Robot::Debug => {
                *check = Some(BoardCheck {
                    wrong: vec![false; squares],
                    problem: None,
                });
            }
            Robot::Broken(problem) => {
                *check = Some(BoardCheck {
                    wrong: vec![false; squares],
                    problem: Some(problem.clone()),
                });
            }
            Robot::Linked(link, _) => {
                let link = Arc::clone(link);
                let (sender, receiver) = channel();

                thread::Builder::new()
                    .name("physical-scan".to_string())
                    .spawn(move || {
                        let reply =
                            link.lock().unwrap().request(&CepRequest::Scan);
                        sender.send(reply).ok();                                /* the part may be closed by now      */
                    })
                    .unwrap_or_else(|e| {
                        panic!("Failed to start scan thread: {e}")
                    });

                self.scan_reply = Some(receiver);
            }
        }
    }

    fn start_game(&mut self, variant: usize) {
        let link = match &self.robot {
            Robot::Debug => None,
            Robot::Broken(_) => return,
            Robot::Linked(link, _) => Some(Arc::clone(link)),
        };

        self.page = Page::Status { variant, asking: false };
        self.focus = Control::Stop;
        self.worker_running = true;
        self.board_state = None;

        let file = self.variants[variant].file.clone();
        let settings = self.settings;

        thread::Builder::new()
            .name("physical-game".to_string())
            .stack_size(WORKER_STACK_BYTES)
            .spawn(move || play_game(&file, settings, link.as_deref()))
            .unwrap_or_else(|e| panic!("Failed to start game thread: {e}"));
    }

    /// Physical::run
    ///
    /// The main loop. It draws a frame, moves the board check of the guide
    /// on, reads the events, and waits for a knob turn or a press. A frame
    /// costs time on a small board computer, so the wait is longer than in
    /// the debug console.
    ///
    /// - Board  : a new game board, for its move list
    /// - Unlock : the game thread ended, clear the interrupt flag
    ///
    /// Params:
    /// - terminal: &mut DefaultTerminal -> the ratatui terminal
    ///
    /// Return:
    /// IoResult<()>                     -> Ok on a clean exit, else Err
    ///
    fn run(&mut self, terminal: &mut DefaultTerminal) -> IoResult<()> {
        while !self.quit {
            terminal.draw(|frame| render(frame, self))?;
            self.check_board();

            while let Ok(engine_event) = self.receiver.try_recv() {
                match engine_event {
                    EngineEvent::Board(state) => {
                        self.board_state = Some(state);
                    }
                    EngineEvent::Unlock => {
                        SYSTEM_INTERRUPT.store(false, Ordering::Relaxed);
                        self.worker_running = false;
                    }
                    _ => {}
                }
            }

            if event::poll(Duration::from_millis(FRAME_WAIT_MS))?
                && let Event::Key(key_event) = event::read()?
            {
                handle_key(self, key_event);
            }
        }

        Ok(())
    }
}

/*----------------------------------------------------------------------------*\
                                  BOARD CHECKS
\*----------------------------------------------------------------------------*/

/// Board checks
///
/// Compare a `scan` result with the guide of a variant. The faults are one
/// flag for each square, in square index order, true for a wrong square.
///
/// read_check
///
///   Makes the check of one `scan` reply. The panels part checks the
///   panels. The pieces part checks the panels and the occupancy, so a
///   moved panel shows there too.
///
///   Params:
///   - entry: &VariantEntry               -> the variant on the board
///   - phase: GuidePhase                  -> the part of the guide
///   - reply: Result<CepReply, String>    -> the `scan` reply
///   - kinds: &[PanelKind]                -> the panel set
///
///   Return:
///   BoardCheck                           -> the wrong squares, or a problem
///
/// panel_faults
///
///   Each square must have one panel, and the panel must lie on the board
///   and show the board colours. Each layout that does this passes, also
///   one that differs from the guide.
///
///   Params:
///   - entry : &VariantEntry -> the variant on the board
///   - panels: &[Panel]      -> the panels that the device finds
///   - kinds : &[PanelKind]  -> the panel set
///
///   Return:
///   Vec<bool>               -> the panel faults
///
/// piece_faults
///
///   Each square of the start placement with a piece must be occupied, and
///   each other square must be empty. The device sees no piece names, so
///   the check is on occupancy only.
///
///   Params:
///   - entry    : &VariantEntry -> the variant on the board
///   - occupancy: &str          -> the occupancy that the device sees
///
///   Return:
///   Result<Vec<bool>, String>  -> the piece faults, Err on a bad occupancy
///
fn read_check(
    entry: &VariantEntry,
    phase: GuidePhase,
    reply: Result<CepReply, String>,
    kinds: &[PanelKind],
) -> BoardCheck {
    let squares = entry.pieces.len();
    let problem = |text: String| BoardCheck {
        wrong: vec![false; squares],
        problem: Some(text),
    };

    let (occupancy, panels) = match reply {
        Ok(CepReply::Board { occupancy, panels }) => (occupancy, panels),
        Ok(_) => return problem("The device sent no board".to_string()),
        Err(text) => return problem(text),
    };

    let mut wrong = panel_faults(entry, &panels, kinds);

    if phase == GuidePhase::Pieces {
        match piece_faults(entry, &occupancy) {
            Ok(faults) => {
                wrong.iter_mut().zip(faults).for_each(|(square, fault)| {
                    *square |= fault;
                });
            }
            Err(text) => return problem(text),
        }
    }

    BoardCheck { wrong, problem: None }
}

fn panel_faults(
    entry: &VariantEntry,
    panels: &[Panel],
    kinds: &[PanelKind],
) -> Vec<bool> {
    let files = entry.files as usize;
    let ranks = entry.ranks as usize;
    let mut cover = vec![0u8; files * ranks];
    let mut wrong = vec![false; files * ranks];

    for panel in panels {
        let file_end = panel.file as usize + panel.files as usize;
        let rank_end = panel.rank as usize + panel.ranks as usize;
        let on_board = file_end <= files && rank_end <= ranks;
        let fits = on_board && panel_fits(panel, kinds, entry.first_light);

        for rank in panel.rank as usize..rank_end.min(ranks) {                  /* the part of the panel on the board */
            for file in panel.file as usize..file_end.min(files) {
                let square = rank * files + file;

                cover[square] = cover[square].saturating_add(1);
                wrong[square] |= !fits;
            }
        }
    }

    wrong
        .iter()
        .zip(&cover)
        .map(|(misfit, count)| *misfit || *count != 1)
        .collect()
}

fn piece_faults(
    entry: &VariantEntry,
    occupancy: &str,
) -> Result<Vec<bool>, String> {
    let seen = parse_placement(occupancy, entry.files, entry.ranks)?;

    let faults = entry
        .pieces
        .iter()
        .zip(&seen)
        .map(|(want, saw)| match (*saw, *want != CEP_EMPTY) {
            (CEP_OCCUPIED, true) | (CEP_EMPTY, false) => false,
            _ => true,                                                          /* a wrong or an unknown mark         */
        })
        .collect();

    Ok(faults)
}

/*----------------------------------------------------------------------------*\
                                    THE GAME
\*----------------------------------------------------------------------------*/

/// play_game
///
/// The body of the game thread. It loads the variant and plays the two
/// sides until the game ends or the interrupt flag stops it. Each board goes
/// to the screen, and each ply goes to the robot as `play`. The next search
/// starts only when the robot replies `done`.
///
/// Params:
/// - file    : &str                     -> embedded config file name
/// - settings: Settings                 -> think time, threads and hash
/// - link    : Option<&Mutex<CepLink>>  -> the robot, None in debug mode
///
/// Notes:
/// A robot problem stops the game with the interrupt flag and goes to the
/// log. A game with a robot always ends with `end`, also when it stopped.
///
fn play_game(file: &str, settings: Settings, link: Option<&Mutex<CepLink>>) {
    let variant = file.strip_suffix(".conf").unwrap_or(file);
    let mut state = load_variant(variant)
        .unwrap_or_else(|diagnostic| panic!("{diagnostic}"));
    let hash_mb = HASH_SIZES_MB[settings.hash_index];

    emit(EngineEvent::Board(BoardState::from_state(&state, None)));

    let mut previous_board = state.main_board.clone();

    let result = play_search_game(
        &mut state,
        Arc::new(TTable::with_mb((hash_mb * 2 / 3).max(1))),                    /* the split of a protocol session    */
        Arc::new(QTable::with_mb((hash_mb / 3).max(1))),
        MAX_DEPTH,
        THINK_TIMES_MS[settings.think_index] * 1_000_000,                       /* milliseconds to nanoseconds        */
        settings.threads,
        usize::MAX,
        None,
        |position, _| {
            emit(EngineEvent::Board(BoardState::from_state(position, None)));

            let Some(link) = link else {
                return;
            };

            let changes = board_changes(position, &previous_board);
            previous_board.clone_from(&position.main_board);

            let play = CepRequest::Play(&changes);

            if let Err(problem) = link.lock().unwrap().request(&play) {
                log_1!("Robot problem, the game stops: {}", problem);
                SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
            }
        },
    );

    emit(EngineEvent::Board(BoardState::from_state(&state, None)));

    let result_word = match &result {
        Ok((WHITE_WIN, _)) => "white",
        Ok((BLACK_WIN, _)) => "black",
        Ok((DRAW, _)) => "draw",
        _ => "abort",
    };

    if let Err(error) = result {
        log_1!("{}", error);
    }

    if let Some(link) = link {
        let end = link.lock().unwrap().request(&CepRequest::End(result_word));

        if let Err(problem) = end {
            log_1!("Robot problem at the end: {}", problem);
        }
    }

    emit(EngineEvent::Unlock);
}

/// board_changes
///
/// Gives the squares that one ply changed, in board order, as `play`
/// changes. The engine board after the ply is compared with a copy from
/// before it, so castling, captures and drops need no special case.
///
/// Params:
/// - position      : &State          -> the position after the ply
/// - previous_board: &[PieceIndex]   -> the board before the ply
///
/// Return:
/// Vec<SquareChange>                 -> each changed square and its piece
///
fn board_changes(
    position: &State,
    previous_board: &[PieceIndex],
) -> Vec<SquareChange> {
    let files = position.statics.files as usize;

    position.main_board
        .iter()
        .zip(previous_board)
        .enumerate()
        .filter(|(_, (now, before))| now != before)
        .map(|(square, (now, _))| SquareChange {
            file: (square % files) as u8,
            rank: (square / files) as u8,
            piece: if *now == NO_PIECE {
                CEP_EMPTY
            } else {
                position.statics.pieces[*now as usize].char
            },
        })
        .collect()
}

/*----------------------------------------------------------------------------*\
                                    DRAWING
\*----------------------------------------------------------------------------*/

/// Board pictures
///
/// The board pictures of the picker and the setup guide, and the texts of
/// the guide. One text cell has two ranks, one in each half. A half cell is
/// a true square with an 8 by 16 font. A square has a fill or none, and a
/// fill has a back flag. A fill with the flag can be the background of a
/// cell:
///
/// - focused kind  : its panel colour on each square of that kind, back
/// - focused piece : `FOCUS_COLOR` on each square of that letter, back
/// - wrong         : `WRONG_COLOR`, back
/// - light         : its panel colour, or the text colour
/// - dark          : none in the panels part, or `DARK_COLOR`, back
///
/// The two halves of a cell share one glyph. A fill is the colour of the
/// glyph, or the background behind the glyph of the other half. The text
/// colour cannot be a background, so a light square is always the glyph.
/// A square with no fill, or no rank below the top square, is the screen
/// background:
///
/// ```text
///   top square    bottom square    cell
///   fill A        none             ▀ in A
///   none          fill B           ▄ in B
///   fill A        fill A           █ in A
///   back W        fill B           ▄ in B, on W
///   fill A        fill B           ▀ in A, on B
/// ```
///
/// paint_board
///
///   Params:
///   - entry: &VariantEntry            -> the variant, for its size
///   - fills: &[Option<(Color, bool)>] -> each square, its fill and flag
///
///   Return:
///   Text<'static>                     -> the picture, the top rank first
///
/// panel_fills
///
///   The panels part: each light square in the colour of its panel kind,
///   each dark square with no fill, the squares of the focused kind in
///   that colour, and the wrong squares in the wrong colour.
///
///   Params:
///   - entry  : &VariantEntry        -> the variant and its panels
///   - focused: Option<usize>        -> the focused panel kind, if any
///   - check  : Option<&BoardCheck>  -> the last check, if any
///
///   Return:
///   Vec<Option<(Color, bool)>>      -> the fill of each square
///
/// piece_fills
///
///   The pieces part: each light square in the text colour, each dark
///   square in the dark colour, the squares of the focused letter in the
///   focus colour, and the wrong squares in the wrong colour. The picker
///   uses it with no focus and no check, for the plain board.
///
///   Params:
///   - entry  : &VariantEntry        -> the variant and its start pieces
///   - focused: Option<char>         -> the focused piece letter, if any
///   - check  : Option<&BoardCheck>  -> the last check, if any
///
///   Return:
///   Vec<Option<(Color, bool)>>      -> the fill of each square
///
/// panel_legend
///
///   One label for each panel kind of the layout, in the set order, each
///   label in its colour. The focused label is black on its colour. Each
///   label is as wide as the widest of the whole set, so the pane has one
///   width for each variant. The labels fill one column, and more columns
///   when the pane is too short.
///
///   Params:
///   - entry  : &VariantEntry  -> the variant and its panels
///   - kinds  : &[PanelKind]   -> the panel set, from `parse_panel_kinds`
///   - rows   : u16            -> rows free for the legend
///   - focused: Option<usize>  -> the focused panel kind, if any
///
///   Return:
///   Vec<Line<'static>>        -> the legend rows, such as `1x2  2x2`
///
/// piece_list
///
///   One row for each piece letter of the start placement, in the order of
///   `used_pieces`. The letters are white, and the focused letter is in the
///   focus style. The pane scrolls, so the focused letter stays in sight.
///
///   Params:
///   - entry  : &VariantEntry  -> the variant and its start pieces
///   - focused: Option<char>   -> the focused piece letter, if any
///
///   Return:
///   Vec<Line<'static>>        -> the list rows, one letter each
///
/// guide_info
///
///   The texts of the info row under the picture: what the focus is on,
///   and the state of the check. The focus text is in the wrong colour
///   when its squares are wrong. A robot problem takes the whole row.
///
///   - focused kind   : `2x2`
///   - focused piece  : `white P`
///   - check          : `reading`, `all right`, or `3 wrong`
///
///   `all right` shows only with a focused size or piece. Without one, the
///   Next control shows the pass. The guide draws no info row when it has
///   no text, so the frames of the picture and the list take its rows. The
///   board is centred in the rows with no info row and the list fits the
///   rows with it, so the two stay at one place.
///
///   Params:
///   - entry  : &VariantEntry        -> the variant
///   - kinds  : &[PanelKind]         -> the panel set
///   - focused: Option<Control>      -> the focused control
///   - check  : Option<&BoardCheck>  -> the last check, if any
///
///   Return:
///   Vec<Span<'static>>              -> the texts, from left to right
///
fn paint_board(
    entry: &VariantEntry,
    fills: &[Option<(Color, bool)>],
) -> Text<'static> {
    let files = entry.files as usize;

    let lines: Vec<Line> = (0..entry.ranks as usize)
        .rev()
        .step_by(2)
        .map(|top_rank| {
            let spans: Vec<Span> = (0..files)
                .map(|file| {
                    let top = fills[top_rank * files + file];
                    let bottom = match top_rank {
                        0 => None,                                              /* the first rank of an odd count     */
                        _ => fills[(top_rank - 1) * files + file],
                    };
                    let plain = Style::default();

                    let (glyph, style) = match (top, bottom) {
                        (None, None) => (' ', plain),
                        (Some((top, _)), None) => ('▀', plain.fg(top)),
                        (None, Some((bottom, _))) => ('▄', plain.fg(bottom)),
                        (Some((top, _)), Some((bottom, _)))
                            if top == bottom =>
                        {
                            ('█', plain.fg(top))
                        }
                        (Some((top, true)), Some((bottom, _))) => {
                            ('▄', plain.fg(bottom).bg(top))
                        }
                        (Some((top, _)), Some((bottom, _))) => {
                            ('▀', plain.fg(top).bg(bottom))
                        }
                    };

                    Span::styled(glyph.to_string(), style)
                })
                .collect();

            Line::from(spans)
        })
        .collect();

    Text::from(lines)
}

fn panel_fills(
    entry: &VariantEntry,
    focused: Option<usize>,
    check: Option<&BoardCheck>,
) -> Vec<Option<(Color, bool)>> {
    entry
        .square_kinds()
        .into_iter()
        .enumerate()
        .map(|(square, kind)| {
            let color = PANEL_COLORS[kind % PANEL_COLORS.len()];

            if focused == Some(kind) {
                Some((color, true))
            } else if check.is_some_and(|check| check.wrong[square]) {
                Some((WRONG_COLOR, true))
            } else if entry.is_light(square) {
                Some((color, false))
            } else {
                None
            }
        })
        .collect()
}

fn piece_fills(
    entry: &VariantEntry,
    focused: Option<char>,
    check: Option<&BoardCheck>,
) -> Vec<Option<(Color, bool)>> {
    entry
        .pieces
        .iter()
        .enumerate()
        .map(|(square, piece)| {
            if focused == Some(*piece) {
                Some((FOCUS_COLOR, true))
            } else if check.is_some_and(|check| check.wrong[square]) {
                Some((WRONG_COLOR, true))
            } else if entry.is_light(square) {
                Some((Color::Reset, false))                                     /* the text colour of the terminal    */
            } else {
                Some((DARK_COLOR, true))
            }
        })
        .collect()
}

fn panel_legend(
    entry: &VariantEntry,
    kinds: &[PanelKind],
    rows: u16,
    focused: Option<usize>,
) -> Vec<Line<'static>> {
    let labels: Vec<(String, Style)> = entry
        .used_kinds()
        .into_iter()
        .filter_map(|kind| {
            let PanelKind { short, long, .. } = kinds.get(kind)?;
            let color = PANEL_COLORS[kind % PANEL_COLORS.len()];
            let style = match focused == Some(kind) {
                true => Style::default().fg(Color::Black).bg(color),
                false => Style::default().fg(color),
            };

            Some((format!("{}x{}", short, long), style))
        })
        .collect();

    let label_width = kinds
        .iter()
        .map(|kind| format!("{}x{}", kind.short, kind.long).len())
        .max()
        .unwrap_or(0);
    let columns = labels.len().div_ceil(rows.max(1) as usize).max(1);

    labels
        .chunks(columns)
        .map(|row| {
            let spans: Vec<Span> = row
                .iter()
                .map(|(label, style)| {
                    Span::styled(format!("{:<label_width$} ", label), *style)
                })
                .collect();

            Line::from(spans)
        })
        .collect()
}

fn piece_list(
    entry: &VariantEntry,
    focused: Option<char>,
) -> Vec<Line<'static>> {
    entry
        .used_pieces()
        .into_iter()
        .map(|piece| {
            let style = match focused == Some(piece) {
                true => focus_style(),
                false => Style::default().fg(Color::White),
            };

            Line::from(Span::styled(format!("{} ", piece), style))
        })
        .collect()
}

fn guide_info(
    entry: &VariantEntry,
    kinds: &[PanelKind],
    focused: Option<Control>,
    check: Option<&BoardCheck>,
) -> Vec<Span<'static>> {
    let wrong_style = Style::default().fg(WRONG_COLOR);

    if let Some(problem) = check.and_then(|check| check.problem.clone()) {
        return vec![Span::styled(problem, wrong_style)];
    }

    let is_wrong =
        |square: usize| check.is_some_and(|check| check.wrong[square]);

    let focus = match focused {
        Some(Control::Panel(kind)) => {
            let PanelKind { short, long, .. } = &kinds[kind];
            let wrong = entry
                .square_kinds()
                .into_iter()
                .enumerate()
                .any(|(square, of)| of == kind && is_wrong(square));

            Some((format!("{}x{}", short, long), wrong))
        }
        Some(Control::Piece(piece)) => {
            let side = match piece.is_ascii_uppercase() {
                true => "white",
                false => "black",
            };
            let wrong = (0..entry.pieces.len()).any(|square| {
                entry.pieces[square] == piece && is_wrong(square)
            });

            Some((format!("{} {}", side, piece), wrong))
        }
        _ => None,
    };

    let status = match check {
        None => Some(Span::raw("reading")),
        Some(check) if check.passed() => {
            focus.is_some().then(|| Span::raw("all right"))                     /* Next shows the pass without focus  */
        }
        Some(check) => {
            let count = check.wrong.iter().filter(|wrong| **wrong).count();
            Some(Span::styled(format!("{} wrong", count), wrong_style))
        }
    };

    let focus = focus.map(|(text, wrong)| match wrong {
        true => Span::styled(text, wrong_style),
        false => Span::raw(text),
    });

    focus.into_iter().chain(status).collect()
}

/// page_help
///
/// Gives the help of a guide part: what its picture and its list show. The
/// help page wraps the text to the screen. The other pages have no help.
///
/// Params:
/// - page: &Page   -> the page before the help page
///
/// Return:
/// &'static str    -> the help text, empty for a page with no help
///
fn page_help(page: &Page) -> &'static str {
    match page {
        Page::Guide { phase: GuidePhase::Panels, .. } => {
            "Lay the panels on the board as the picture shows. Each colour is \
             one panel size. The squares of the focused size are filled. The \
             robot reads the board each 10 seconds and shows the wrong \
             squares in red."
        }
        Page::Guide { phase: GuidePhase::Pieces, .. } => {
            "Put the pieces on the board. The uppercase letters are the white \
             pieces and the lowercase letters are the black pieces. The \
             squares of the focused letter are yellow. The robot reads the \
             board each 10 seconds and shows the wrong squares in red."
        }
        _ => "",
    }
}

/// Controls on screen
///
/// The look of a control. A focused control is yellow and bold, the others
/// are in the plain style, and a control that is shut is grey. The labels
/// are short ASCII words, so each console font shows them and a narrow
/// screen fits them. A legend label or a board square has no label here.
///
/// control_label
///
///   Params:
///   - control: Control -> the control to name
///
///   Return:
///   &'static str       -> its label, a short word
///
/// control_span
///
///   Params:
///   - control: Control         -> the control to draw
///   - focused: Option<Control> -> the focused control of the page
///
///   Return:
///   Span<'static>              -> the label with one space on each side
///
fn control_label(control: Control) -> &'static str {
    match control {
        Control::Exit => "X",
        Control::Help => "?",
        Control::Previous => "<-",
        Control::Settings => "Conf",
        Control::Play => "Play",
        Control::Next => "->",
        Control::Setting(_) | Control::Panel(_) | Control::Piece(_) => "",      /* drawn as a row or a list label     */
        Control::Back => "Back",
        Control::Proceed => "Next",
        Control::Ok => "OK",
        Control::Stop => "Stop",
        Control::No => "No",
        Control::Yes => "Yes",
    }
}

fn control_span(control: Control, focused: Option<Control>) -> Span<'static> {
    let style = match focused == Some(control) {
        true => focus_style(),
        false => Style::default(),
    };

    Span::styled(format!(" {} ", control_label(control)), style)
}

/// focus_style
///
/// Gives the style of the focused control and of the focused settings row.
///
/// Return:
/// Style -> yellow and bold
///
fn focus_style() -> Style {
    Style::default().fg(FOCUS_COLOR).add_modifier(Modifier::BOLD)
}

/// draw_spread
///
/// Draws a row of spans as `space-between` in CSS: the first at the left
/// edge, the last at the right edge, the rest spread evenly between them.
/// A lone span goes to the middle.
///
/// Params:
/// - frame: &mut Frame<'_>      -> ratatui frame to draw into
/// - area : Rect                -> the row to fill
/// - spans: Vec<Span<'static>>  -> buttons and texts, in display order
///
fn draw_spread(frame: &mut Frame<'_>, area: Rect, spans: Vec<Span<'static>>) {
    let flex = match spans.len() {
        1 => Flex::Center,
        _ => Flex::SpaceBetween,
    };
    let widths = spans
        .iter()
        .map(|span| Constraint::Length(span.width() as u16));
    let slots = Layout::horizontal(widths).flex(flex).split(area);

    for (span, slot) in spans.into_iter().zip(slots.iter()) {
        frame.render_widget(span, *slot);
    }
}

/// Screen drawing functions
///
/// Each function draws one region of the frame. The panes are stacked and
/// share their border lines. The help text wraps. The text of the other
/// panes does not wrap, so a long value is cut at the pane edge. Each page
/// has the title pane on top and the control bar at the bottom. The bar
/// spreads its controls with `draw_spread`.
///
/// The diagrams below show each page on the screen of 30 by 20.
///
/// ```text
/// Picker (draw_picker)              Help page (draw_help)
/// ┌────────────────────────────┐    ┌────────────────────────────┐
/// │ X        Variant           │    │            Help            │
/// ├────────────────────────────┤    ├────────────────────────────┤
/// │                            │    │ Help of the guide part     │
/// │                            │    │ before it, one paragraph   │
/// │                            │    │ wrapped to the screen      │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │         Board Shape        │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// ├────────────────────────────┤    ├────────────────────────────┤
/// │ <-     Conf    Play     -> │    │             OK             │
/// └────────────────────────────┘    └────────────────────────────┘
///
/// Settings (draw_settings)          Guide, panels (draw_guide)
/// ┌────────────────────────────┐    ┌────────────────────────────┐
/// │          Settings          │    │          Variant         ? │
/// ├────────────────────────────┤    ├────────────────────┬───────┤
/// │ Think time 1 s             │    │                    │ 1x2   │
/// │ Threads    4               │    │                    │ 1x6   │
/// │ Hash       256 MB          │    │                    │ 2x2   │
/// │ Robot      device name     │    │                    │ 2x8   │
/// │                            │    │                    │ 4x7   │
/// │                            │    │     Board Shape    │       │
/// │                            │    │  in panel colours, │       │
/// │                            │    │  the focused size  │       │
/// │                            │    │  filled whole, the │       │
/// │                            │    │  wrong ones in red │       │
/// │                            │    │                    │       │
/// │                            │    │                    │       │
/// │                            │    ├────────────────────┴───────┤
/// │                            │    │ 2x2              all right │
/// ├────────────────────────────┤    ├────────────────────────────┤
/// │            Back            │    │  Back                Next  │
/// └────────────────────────────┘    └────────────────────────────┘
///
/// Guide, pieces (draw_guide)        Game (draw_status)
/// ┌────────────────────────────┐    ┌────────────────────────────┐
/// │          Variant         ? │    │          Variant           │
/// ├────────────────────────┬───┤    ├────────────────────────────┤
/// │                        │ K │    │   1. e2:e4      e7:e5      │
/// │                        │ N │    │   2. g1:f3      b8:c6      │
/// │                        │ P │    │   3. f1:c4                 │
/// │                        │ R │    │                            │
/// │                        │ b │    │                            │
/// │      Board Shape,      │ k │    │                            │
/// │  the squares of the    │ n │    │                            │
/// │  focused letter in     │ p │    │                            │
/// │  yellow, the wrong     │ r │    │                            │
/// │  ones in red           │   │    │                            │
/// │                        │   │    │                            │
/// │                        │   │    │                            │
/// ├────────────────────────┴───┤    │                            │
/// │ white P          all right │    │                            │
/// ├────────────────────────────┤    ├────────────────────────────┤
/// │  Back                Next  │    │            Stop            │
/// └────────────────────────────┘    └────────────────────────────┘
///
/// Stop question (draw_dialog)       Wait (draw_dialog)
/// ┌────────────────────────────┐    ┌────────────────────────────┐
/// │          Variant           │    │          Variant           │
/// ├────────────────────────────┤    ├────────────────────────────┤
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │  ┌──────────────────────┐  │    │                            │
/// │  │    Stop the game?    │  │    │  ┌──────────────────────┐  │
/// │  ├──────────────────────┤  │    │  │      Loading...      │  │
/// │  │  No             Yes  │  │    │  └──────────────────────┘  │
/// │  └──────────────────────┘  │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// │                            │    │                            │
/// ├────────────────────────────┤    ├────────────────────────────┤
/// │                            │    │            Stop            │
/// └────────────────────────────┘    └────────────────────────────┘
///
/// Game over (draw_status)
/// ┌────────────────────────────┐
/// │          Variant           │
/// ├────────────────────────────┤
/// │   1. e2:e4      e7:e5      │
/// │   2. g1:f3      b8:c6      │
/// │   3. f1:c4                 │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// │                            │
/// ├────────────────────────────┤
/// │            Back            │
/// └────────────────────────────┘
/// ```
///
/// - draw_picker   : board shape of the shown variant
/// - draw_settings : the settings rows and the robot
/// - draw_guide    : the panels or the pieces, their list, and the info
/// - draw_status   : the newest moves, as number, White and Black columns
/// - draw_help     : the help text of the guide part before the help page
/// - draw_dialog   : a message over the page, a wait or a question
/// - draw_controls : the control bar of the page
/// - render        : the layout and the title, calls the other functions
///
/// draw_picker
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> region of the frame
///   - entry: &VariantEntry   -> the shown variant
///
/// draw_dialog
///
///   Draws the dialog of the page when it has one. The page waits for the
///   engine, or asks if the game must stop. The dialog is as wide as its
///   longest line, and stays inside the frame of the area.
///
///   - loading a game     : `Loading...`
///   - stopping a game    : `Stopping...`
///   - asking to stop     : `Stop the game?`, with No and Yes
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> region of the frame to center it in
///   - app  : &Physical       -> console state
///
/// draw_settings
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> body region of the frame
///   - app  : &Physical       -> console state
///
/// draw_guide
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> body region of the frame
///   - app  : &Physical       -> console state
///
/// draw_status
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> body region of the frame
///   - app  : &Physical       -> console state
///
/// draw_help
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> body region of the frame
///   - app  : &Physical       -> console state
///
/// draw_controls
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - area : Rect            -> control bar region of the frame
///   - app  : &Physical       -> console state
///
/// render
///
///   Params:
///   - frame: &mut Frame<'_>  -> ratatui frame to draw into
///   - app  : &Physical       -> console state
///
/// Notes:
/// The Exit control sits in the title pane, the settings rows in their
/// table, the panel sizes and the piece letters in the guide lists, and
/// all other controls in the control bar.
///
fn draw_picker(frame: &mut Frame<'_>, area: Rect, entry: &VariantEntry) {
    let preview_block = padded_block!(merge);
    let preview_area = preview_block.inner(area);

    let board = paint_board(entry, &piece_fills(entry, None, None));
    let board_area = preview_area.centered(
        Constraint::Length(entry.files as u16),
        Constraint::Length(board.lines.len() as u16)
    );

    frame.render_widget(preview_block, area);
    frame.render_widget(Paragraph::new(board), board_area);
}

fn draw_settings(frame: &mut Frame<'_>, area: Rect, app: &Physical) {
    let editing = matches!(app.page, Page::Settings { editing: true });
    let focused_row = match app.focused() {
        Some(Control::Setting(row)) => Some(row),
        _ => None,
    };

    let mut rows: Vec<Row> = (0..SETTING_LABELS.len())
        .map(|row| {
            let value = match editing && focused_row == Some(row) {
                true => format!("< {} >", app.settings.value(row)),
                false => app.settings.value(row),
            };

            Row::new([SETTING_LABELS[row].to_string(), value])
        })
        .collect();

    let robot = match &app.robot {
        Robot::Debug => "debug, no device".to_string(),
        Robot::Broken(problem) => problem.clone(),
        Robot::Linked(_, name) => name.clone(),
    };

    rows.push(Row::new(["Robot".to_string(), robot]));

    let table = Table::new(rows, [
        Constraint::Length(LABEL_WIDTH),
        Constraint::Fill(1)
    ])
    .row_highlight_style(focus_style())
    .block(padded_block!(merge));

    let mut table_state = TableState::default().with_selected(focused_row);

    frame.render_stateful_widget(table, area, &mut table_state);
}

fn draw_guide(frame: &mut Frame<'_>, area: Rect, app: &Physical) {
    let Page::Guide { variant, phase, check } = &app.page else {
        return;
    };
    let entry = &app.variants[*variant];
    let focused = app.focused();
    let check = check.as_ref();

    let rows = split_area!(overlap Vertical, area, [
        Constraint::Min(0),
        Constraint::Length(3)                                                   /* info row, one row and its frame    */
    ]);

    let info = guide_info(entry, &app.panel_kinds, focused, check);
    let body = match info.is_empty() {
        true => area,                                                           /* the frames take the info rows      */
        false => {
            let info_block = padded_block!(merge);
            let info_area = info_block.inner(rows[1]);

            frame.render_widget(info_block, rows[1]);
            draw_spread(frame, info_area, info);

            rows[0]
        }
    };

    let list_rows = rows[0].height.saturating_sub(2);                           /* rows in the frame with the info,   */
                                                                                /* so the list keeps its columns      */

    let (fills, list, scroll) = match phase {
        GuidePhase::Pieces => {
            let piece = match focused {
                Some(Control::Piece(piece)) => Some(piece),
                _ => None,
            };
            let index = entry
                .used_pieces()
                .iter()
                .position(|letter| Some(*letter) == piece)
                .unwrap_or(0);
            let scroll = (index + 1).saturating_sub(list_rows as usize);        /* the focused letter on the last row */

            (
                piece_fills(entry, piece, check),
                piece_list(entry, piece),
                scroll as u16,
            )
        }
        GuidePhase::Panels => {
            let kind = match focused {
                Some(Control::Panel(kind)) => Some(kind),
                _ => None,
            };

            (
                panel_fills(entry, kind, check),
                panel_legend(entry, &app.panel_kinds, list_rows, kind),
                0,
            )
        }
    };

    let list_width = list.iter().map(Line::width).max().unwrap_or(0) as u16;
    let panes = split_area!(overlap Horizontal, body, [
        Constraint::Min(0),
        Constraint::Length(list_width + 3)                                      /* frame, padding and the label space */
    ]);
    let list_pane = Paragraph::new(Text::from(list))
        .scroll((scroll, 0))
        .block(padded_block!(merge));

    frame.render_widget(list_pane, panes[1]);

    let picture_pane = panes[0];
    let picture_place = Rect { height: area.height, ..picture_pane };           /* the pane with no info, so the      */
                                                                                /* board stays at one place           */
    let picture_block = padded_block!(merge);
    let picture = paint_board(entry, &fills);
    let picture_area = picture_block
        .inner(picture_place)
        .centered(
            Constraint::Length(entry.files as u16),
            Constraint::Length(picture.lines.len() as u16)
        )
        .intersection(picture_block.inner(picture_pane));                       /* a tall board stays off the info    */

    frame.render_widget(picture_block, picture_pane);
    frame.render_widget(Paragraph::new(picture), picture_area);
}

fn draw_help(frame: &mut Frame<'_>, area: Rect, app: &Physical) {
    let Page::Help { previous } = &app.page else {
        return;
    };

    let help = Paragraph::new(page_help(previous))
        .wrap(Wrap { trim: true })
        .block(padded_block!(merge));

    frame.render_widget(help, area);
}

fn draw_status(frame: &mut Frame<'_>, area: Rect, app: &Physical) {
    let moves_block = padded_block!(merge);
    let moves_area = moves_block.inner(area);
    frame.render_widget(moves_block, area);

    let Some(board_state) = &app.board_state else {
        return;
    };

    let lines: Vec<&str> = board_state.move_history.lines().collect();
    let hidden = lines.len().saturating_sub(moves_area.height as usize);        /* the oldest rows that do not fit    */

    let rows: Vec<Row> = lines[hidden..]
        .iter()
        .map(|line| {
            let mut parts = line.split_whitespace();                            /* `12. e2:e4 e7:e5`, no space inside */
            let number = parts.next().unwrap_or_default();
            let white = parts.next().unwrap_or_default();
            let black = parts.next().unwrap_or_default();

            Row::new([
                format!("{:>1$}", number, MOVE_NUMBER_WIDTH as usize),
                white.to_string(),
                black.to_string(),
            ])
        })
        .collect();

    let table = Table::new(rows, [
        Constraint::Length(MOVE_NUMBER_WIDTH),
        Constraint::Fill(1),
        Constraint::Fill(1)
    ]);

    frame.render_widget(table, moves_area);
}

fn draw_dialog(frame: &mut Frame<'_>, area: Rect, app: &Physical) {
    let focused = app.focused();
    let stopping = SYSTEM_INTERRUPT.load(Ordering::Relaxed);

    let (message, answers) = match &app.page {
        Page::Status { asking: true, .. } => (
            vec!["Stop the game?".to_string()],
            vec![
                control_span(Control::No, focused),
                control_span(Control::Yes, focused),
            ],
        ),
        Page::Status { .. } if app.worker_running && stopping => {
            (vec!["Stopping...".to_string()], Vec::new())
        }
        Page::Status { .. } if app.board_state.is_none() => {
            (vec!["Loading...".to_string()], Vec::new())
        }
        _ => return,
    };

    let message_rows = message.len() as u16;
    let message_width = message
        .iter()
        .map(|line| line.chars().count())
        .max()
        .unwrap_or(0) as u16;
    let answer_rows = match answers.is_empty() {
        true => 0,
        false => 2,                                                             /* a separator and the answers        */
    };
    let dialog_width = (message_width + 8)                                      /* frame, padding and a margin        */
        .max(24)                                                                /* the width of the stop question     */
        .min(area.width.saturating_sub(2));                                     /* inside the frame of the area       */
    let dialog_area = area.centered(
        Constraint::Length(dialog_width),
        Constraint::Length(message_rows + 2 + answer_rows)                      /* two rows for the frame             */
    );
    let panes = split_area!(overlap Vertical, dialog_area, [
        Constraint::Length(message_rows + 2),                                   /* two rows for the frame             */
        Constraint::Min(0)
    ]);
    let message_lines: Vec<Line> = message
        .into_iter()
        .map(|line| Line::from(line).centered())
        .collect();
    let message_pane = Paragraph::new(Text::from(message_lines))
        .block(padded_block!(merge));

    frame.render_widget(Clear, dialog_area);
    frame.render_widget(message_pane, panes[0]);

    if !answers.is_empty() {
        let answers_block = padded_block!(merge);
        let answers_area = answers_block.inner(panes[1]);

        frame.render_widget(answers_block, panes[1]);
        draw_spread(frame, answers_area, answers);
    }
}

fn draw_controls(frame: &mut Frame<'_>, area: Rect, app: &Physical) {
    let focused = app.focused();
    let button = |control: Control| control_span(control, focused);

    let bar_block = padded_block!(merge);
    let bar_area = bar_block.inner(area);
    frame.render_widget(bar_block, area);

    let spans: Vec<Span> = match &app.page {
        Page::Picker => [
            Control::Previous,
            Control::Settings,
            Control::Play,
            Control::Next,
        ]
        .into_iter()
        .map(button)
        .collect(),
        Page::Guide { .. } => {
            let proceed = match app.controls().contains(&Control::Proceed) {
                true => button(Control::Proceed),
                false => Span::styled(
                    format!(" {} ", control_label(Control::Proceed)),
                    Style::default().fg(Color::DarkGray),                       /* shut until a check passes          */
                ),
            };

            vec![button(Control::Back), proceed]
        }
        Page::Status { asking: true, .. } => Vec::new(),
        _ => app
            .controls()
            .into_iter()
            .filter(|control| {
                !matches!(control, Control::Setting(_) | Control::Help)
            })
            .map(button)
            .collect(),
    };

    draw_spread(frame, bar_area, spans);
}

fn render(frame: &mut Frame<'_>, app: &Physical) {
    let page = split_area!(overlap Vertical, frame.area(), [
        Constraint::Length(3),                                                  /* title pane, one row and its frame  */
        Constraint::Min(0),
        Constraint::Length(3)                                                   /* control bar, one row and its frame */
    ]);

    let title = match &app.page {
        Page::Picker => app.variants[app.selected].title.as_str(),
        Page::Settings { .. } => "Settings",
        Page::Guide { variant, .. } | Page::Status { variant, .. } => {
            app.variants[*variant].title.as_str()
        }
        Page::Help { .. } => "Help",
    };

    let title_pane = Paragraph::new(title)
        .alignment(Alignment::Center)
        .block(padded_block!(merge));

    frame.render_widget(title_pane, page[0]);

    let title_controls = app.controls();
    let corner_row = Rect {
        x: page[0].x + 1,
        y: page[0].y + 1,
        width: page[0].width.saturating_sub(2),                                 /* inside the frame                   */
        height: 1,                                                              /* the title row                      */
    };
    let corners = split_area!(Horizontal, corner_row, [
        Constraint::Length(3),                                                  /* " X "                              */
        Constraint::Fill(1),
        Constraint::Length(3)                                                   /* " ? "                              */
    ]);

    if title_controls.contains(&Control::Exit) {
        frame.render_widget(
            control_span(Control::Exit, app.focused()), corners[0]
        );
    }

    if title_controls.contains(&Control::Help) {
        frame.render_widget(
            control_span(Control::Help, app.focused()), corners[2]
        );
    }

    match &app.page {
        Page::Picker => {
            draw_picker(frame, page[1], &app.variants[app.selected]);
        }
        Page::Settings { .. } => draw_settings(frame, page[1], app),
        Page::Guide { .. } => draw_guide(frame, page[1], app),
        Page::Status { .. } => draw_status(frame, page[1], app),
        Page::Help { .. } => draw_help(frame, page[1], app),
    }

    draw_dialog(frame, page[1], app);
    draw_controls(frame, page[2], app);
}

/*----------------------------------------------------------------------------*\
                                     INPUT
\*----------------------------------------------------------------------------*/

/// Console input
///
/// The console has one knob and one button. The knob turns arrive as the
/// left and right arrow keys, the button as enter. Other keys do nothing.
///
/// handle_key
///
///   Sends a key press to the knob or the button.
///
///   Params:
///   - app  : &mut Physical -> console state
///   - event: KeyEvent      -> key event from the terminal
///
/// turn_knob
///
///   Moves the focus to the control before or after, round the page. A
///   settings row in edit mode keeps the focus, and the turn changes its
///   value.
///
///   Params:
///   - app : &mut Physical -> console state
///   - step: isize         -> the knob turn, -1 left or 1 right
///
/// press_button
///
///   Presses the focused control. A page without controls ignores it.
///
///   Params:
///   - app: &mut Physical  -> console state
///
fn handle_key(app: &mut Physical, event: KeyEvent) {
    if event.kind != KeyEventKind::Press {
        return;
    }

    match event.code {
        KeyCode::Left => turn_knob(app, -1),
        KeyCode::Right => turn_knob(app, 1),
        KeyCode::Enter => press_button(app),
        _ => {}
    }
}

fn turn_knob(app: &mut Physical, step: isize) {
    if let Page::Settings { editing: true } = app.page
        && let Control::Setting(row) = app.focus
    {
        app.settings.adjust(row, step);
        return;
    }

    let controls = app.controls();

    if let Some(focused) = app.focused() {
        let index = controls
            .iter()
            .position(|control| *control == focused)
            .unwrap_or(0);

        app.focus = controls[step_wrapped(index, step, controls.len())];
    }
}

fn press_button(app: &mut Physical) {
    let Some(control) = app.focused() else {
        return;
    };
    let variant_count = app.variants.len();

    match (control, &mut app.page) {
        (Control::Exit, _) => app.quit = true,
        (Control::Help, _) => {
            let previous = mem::replace(&mut app.page, Page::Picker);
            app.page = Page::Help { previous: Box::new(previous) };
            app.focus = Control::Ok;
        }
        (Control::Ok, Page::Help { previous }) => {
            app.page = mem::replace(previous.as_mut(), Page::Picker);
            app.focus = Control::Help;
        }
        (Control::Previous, _) => {
            app.selected = step_wrapped(app.selected, -1, variant_count);
        }
        (Control::Next, _) => {
            app.selected = step_wrapped(app.selected, 1, variant_count);
        }
        (Control::Settings, _) => {
            app.page = Page::Settings { editing: false };
            app.focus = Control::Setting(THINK_ROW);
        }
        (Control::Play, _) => app.enter_guide(app.selected),
        (Control::Setting(row), Page::Settings { editing }) => {
            *editing = !*editing;
            app.focus = Control::Setting(row);
        }
        (Control::Back, Page::Settings { .. }) => {
            app.page = Page::Picker;
            app.focus = Control::Settings;
        }
        (Control::Back, Page::Guide { phase: GuidePhase::Pieces, .. }) => {
            app.open_part(GuidePhase::Panels);
        }
        (Control::Back, _) => {
            app.page = Page::Picker;
            app.focus = Control::Play;
        }
        (Control::Proceed, Page::Guide { phase: GuidePhase::Panels, .. }) => {
            app.open_part(GuidePhase::Pieces);
        }
        (Control::Proceed, Page::Guide { variant, .. }) => {
            let variant = *variant;
            app.start_game(variant);
        }
        (Control::Stop, Page::Status { asking, .. }) => {
            *asking = true;
            app.focus = Control::No;
        }
        (Control::No, Page::Status { asking, .. }) => {
            *asking = false;
            app.focus = Control::Stop;
        }
        (Control::Yes, Page::Status { asking, .. }) => {
            SYSTEM_INTERRUPT.store(true, Ordering::Relaxed);
            *asking = false;
            app.focus = Control::Back;
        }
        _ => {}
    }
}

/*----------------------------------------------------------------------------*\
                                  ENTRY POINT
\*----------------------------------------------------------------------------*/

/// run_debug_physical
///
/// Opens the robot link, takes the terminal, runs the console and gives
/// the terminal back. During the run, all engine events go to a channel.
///
/// Params:
/// - device: Option<&str> -> serial device of the robot, None for debug mode
///
/// Return:
/// IoResult<()>           -> Ok on a clean exit, Err on a terminal failure
///
/// Notes:
/// The link opens before the terminal, so `stty` writes to a normal
/// terminal. The terminal restore comes before the error report. A
/// terminal in raw mode needs a manual fix.
///
pub fn run_debug_physical(device: Option<&str>) -> IoResult<()> {
    log_3!("Starting physical frontend...");

    let robot = match device.map(CepLink::open) {
        None => Robot::Debug,
        Some(Ok(link)) => {
            let name = link.name.clone();
            Robot::Linked(Arc::new(Mutex::new(link)), name)
        }
        Some(Err(problem)) => {
            log_2!("No robot link: {}", problem);
            Robot::Broken(problem)
        }
    };

    let mut terminal = ratatui::init();
    execute!(terminal.backend_mut(), EnableMouseCapture)?;

    let (sender, receiver) = channel::<EngineEvent>();
    set_sink(sender);

    let run_result = Physical::new(receiver, robot).run(&mut terminal);
    clear_sink();
    let mouse_result = execute!(terminal.backend_mut(), DisableMouseCapture);

    ratatui::restore();

    run_result?;
    mouse_result?;

    Ok(())
}
