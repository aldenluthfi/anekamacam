//! cep.rs
//!
//! Cheesy Embedded Protocol (CEP), the link to the robot microcontroller.
//!
//! The engine runs on the Raspberry Pi and decides the moves. The
//! microcontroller drives the robot and reads the physical board. CEP is the
//! line protocol between the two, over one serial device. The engine asks
//! and the microcontroller answers, one request at a time.
//!
//! Created: 15/11/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                                 LINK CONSTANTS
\*----------------------------------------------------------------------------*/

/// Link constants
///
/// Fixed values of the link and of its square contents.
///
/// - CEP_VERSION          : protocol version that `hello` sends
/// - CEP_BAUD             : serial speed that the host sets on the device
/// - CEP_REPLY_TIMEOUT_MS : longest wait for a reply, but not for `play`
/// - CEP_EMPTY            : a square without a piece
/// - CEP_OCCUPIED         : a square with a piece, in an occupancy
///
pub const CEP_VERSION: u32 = 1;
pub const CEP_BAUD: u32 = 115_200;
pub const CEP_REPLY_TIMEOUT_MS: u64 = 5_000;

pub const CEP_EMPTY: char = '.';
pub const CEP_OCCUPIED: char = '*';

/*----------------------------------------------------------------------------*\
                                    MESSAGES
\*----------------------------------------------------------------------------*/

/// SquareChange
///
/// One square of a `play` request and its new content, as the token
/// `<file>,<rank>=<piece>`. File and rank are decimal numbers from zero:
///
/// - `0,0`        : a1, the bottom left square from White's side
/// - file         : counts to the right, from the a-file
/// - rank         : counts up to Black, from White's first rank
/// - piece        : the variant letter of the piece, or `CEP_EMPTY`
///
/// Notes:
/// The case of a letter gives its side, as in the variant config.
///
pub struct SquareChange {
    pub file: u8,                                                               /* file, zero is the a-file           */
    pub rank: u8,                                                               /* rank, zero is White's first rank   */
    pub piece: char,                                                            /* new content, or CEP_EMPTY          */
}

/// Panel
///
/// One board panel, as the token `<file>,<rank>,<files>,<ranks>`: the square
/// at its bottom left corner and its size in squares. A `board` reply has
/// one token for each panel that the device finds.
///
/// ```text
///   token     corner   size      squares
///   2,1,3,2   c2       3 x 2     c2 to e3, files 2 to 4, ranks 1 and 2
///   0,0,1,1   a1       1 x 1     a1 only
/// ```
///
/// Notes:
/// The layout guide of the console uses the same type for its panels.
///
#[derive(Clone, Copy)]
pub struct Panel {
    pub file: u8,                                                               /* file of the bottom left square     */
    pub rank: u8,                                                               /* rank of the bottom left square     */
    pub files: u8,                                                              /* width in squares                   */
    pub ranks: u8,                                                              /* height in squares                  */
}

/// CepRequest
///
/// A host line after its request number. Each request has one reply, and
/// any request can get `error <text>` as its reply.
///
/// - `hello <version>`                   : open the link, reply `ready`
/// - `setup <files> <ranks> <placement>` : announce a game, reply `ok`
/// - `scan`                              : read the board, reply `board`
/// - `play <change> ...`                 : carry out one ply, reply `done`
/// - `end <result>`                      : the game ended, reply `ok`
///
/// hello
///
///   The version is `CEP_VERSION`. A device that does not know it replies
///   `error`. The host sends `hello` first, once on each link.
///
/// setup
///
///   The board size and the start placement of the next game. The sensors
///   of the device see only which squares hold a piece, so the device keeps
///   the placement in its memory to know each piece. A placement is the
///   board field of a FEN:
///
///   - ranks    : from the top rank down, split by `/`
///   - a number : that many empty squares
///   - a letter : a piece, with the variant letter, its case gives the side
///
/// scan
///
///   The device reads the board: the panels that it finds and the squares
///   that hold a piece. It does not name the pieces. The host compares the
///   two with the setup.
///
/// play
///
///   One ply, as the squares that it changes, rank by rank from `0,0`. Each
///   change is a `SquareChange` token. The device moves pieces until each
///   square has its new content:
///
///   - a square that gets a piece takes it from a square that loses it
///   - a piece without such a square comes from the store
///   - a piece that loses its square and goes nowhere goes to the store
///
///   ```text
///   ply                 request
///   e2 to e4            play 4,1=. 4,3=P
///   e5 takes d6         play 4,4=. 3,5=P
///   White castles short play 4,0=. 5,0=R 6,0=K 7,0=.
///   e7 promotes on e8   play 4,6=. 4,7=Q
///   a pawn drop on e4   play 4,3=P
///   ```
///
///   The letters keep the device memory right, also for a promotion or a
///   drop. The device replies `done` only when the board is in the new
///   state. A `play` has no time limit.
///
/// end
///
///   The result is `white`, `black` or `draw` for a game that reached its
///   end, and `abort` for a game that stopped before its end.
///
pub enum CepRequest<'a> {
    Hello,                                                                      /* hello <version>                    */
    Setup { files: u8, ranks: u8, placement: &'a str },                         /* setup <files> <ranks> <placement>  */
    Scan,                                                                       /* scan                               */
    Play(&'a [SquareChange]),                                                   /* play <change> ...                  */
    End(&'a str),                                                               /* end <result>                       */
}

impl Display for CepRequest<'_> {
    /// CepRequest::fmt
    ///
    /// Writes the request as its line, without the request number and
    /// without the line end.
    ///
    /// Params:
    /// - formatter: &mut FmtFormatter<'_> -> output of the line
    ///
    /// Return:
    /// FmtResult                          -> the result of the write
    ///
    fn fmt(&self, formatter: &mut FmtFormatter<'_>) -> FmtResult {
        match self {
            CepRequest::Hello => write!(formatter, "hello {}", CEP_VERSION),
            CepRequest::Setup { files, ranks, placement } => {
                write!(formatter, "setup {} {} {}", files, ranks, placement)
            }
            CepRequest::Scan => write!(formatter, "scan"),
            CepRequest::Play(changes) => {
                write!(formatter, "play")?;

                for change in changes.iter() {
                    write!(
                        formatter,
                        " {},{}={}",
                        change.file, change.rank, change.piece
                    )?;
                }

                Ok(())
            }
            CepRequest::End(result) => write!(formatter, "end {}", result),
        }
    }
}

/// CepReply
///
/// A device line that answers a request, after the request number.
///
/// - `ready <name>`                 : `hello` worked, the name is free text
/// - `ok`                           : `setup` or `end` worked
/// - `board <occupancy> <panel> ...`: the result of `scan`
/// - `done`                         : the ply of `play` is on the board
/// - `error <text>`                 : the request failed, the text says why
///
/// An occupancy has the form of a placement, but without piece letters:
///
/// - ranks          : from the top rank down, split by `/`
/// - a number       : that many empty squares
/// - `CEP_OCCUPIED` : a square that holds a piece
///
/// ```text
/// start placement   rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR
/// its occupancy     ********/********/8/8/8/8/********/********
/// ```
///
/// Each panel of `board` is a `Panel` token. A device that cannot find its
/// panels sends no panel tokens, and the board check then fails.
///
pub enum CepReply {
    Ready(String),                                                              /* ready <name>                       */
    Accepted,                                                                   /* ok                                 */
    Board { occupancy: String, panels: Vec<Panel> },                            /* board <occupancy> <panel> ...      */
    Done,                                                                       /* done                               */
    Error(String),                                                              /* error <text>                       */
}

/*----------------------------------------------------------------------------*\
                                    PARSING
\*----------------------------------------------------------------------------*/

/// parse_reply
///
/// Reads one device line as a request number and a reply. The text of
/// `ready` and `error` keeps its words and loses the extra spaces.
///
/// Params:
/// - line: &str                    -> a device line, not a `log` line
///
/// Return:
/// Result<(u32, CepReply), String> -> the number and the reply, or why not
///
fn parse_reply(line: &str) -> Result<(u32, CepReply), String> {
    let mut tokens = line.split_whitespace();

    let id = tokens.next()
        .and_then(|token| token.parse::<u32>().ok())
        .ok_or_else(|| format!("No request number: {}", line))?;
    let word = tokens.next().unwrap_or_default();
    let text = tokens.clone().collect::<Vec<_>>().join(" ");

    let reply = match word {
        "ready" => CepReply::Ready(text),
        "ok" => CepReply::Accepted,
        "done" => CepReply::Done,
        "error" => CepReply::Error(text),
        "board" => {
            let occupancy = tokens.next()
                .ok_or("A board reply needs an occupancy")?
                .to_string();
            let panels = tokens
                .map(parse_panel)
                .collect::<Result<Vec<_>, _>>()?;

            CepReply::Board { occupancy, panels }
        }
        _ => return Err(format!("Unknown reply: {}", line)),
    };

    Ok((id, reply))
}

/// parse_panel
///
/// Reads one `Panel` token, four decimal numbers split by commas.
///
/// Params:
/// - token: &str          -> the token, such as `0,0,2,8`
///
/// Return:
/// Result<Panel, String>  -> the panel, or why the token is bad
///
fn parse_panel(token: &str) -> Result<Panel, String> {
    let numbers: Vec<u8> = token
        .split(',')
        .map(|number| number.parse::<u8>())
        .collect::<Result<_, _>>()
        .map_err(|_| format!("Bad panel: {}", token))?;

    match numbers[..] {
        [file, rank, files, ranks] => Ok(Panel { file, rank, files, ranks }),
        _ => Err(format!("Bad panel: {}", token)),
    }
}

/// parse_placement
///
/// Reads a placement or an occupancy into one character for each square,
/// in board order: the square of file f and rank r is at `r * files + f`.
/// An empty square is `CEP_EMPTY`, the other characters stay as they are.
///
/// Params:
/// - placement: &str           -> placement or occupancy, as in the docs
/// - files    : u8             -> board width
/// - ranks    : u8             -> board height
///
/// Return:
/// Result<Vec<char>, String>   -> the squares, or why the size is wrong
///
pub fn parse_placement(
    placement: &str,
    files: u8,
    ranks: u8,
) -> Result<Vec<char>, String> {
    let rows: Vec<&str> = placement.split('/').collect();
    let width = files as usize;

    if rows.len() != ranks as usize {
        return Err(format!(
            "Placement has {} ranks, not {}", rows.len(), ranks
        ));
    }

    let mut squares = vec![CEP_EMPTY; width * ranks as usize];

    for (row_index, row) in rows.iter().enumerate() {
        let rank = ranks as usize - 1 - row_index;
        let mut file = 0;
        let mut empty_run = 0;

        for character in row.chars() {
            if let Some(digit) = character.to_digit(10) {
                empty_run = empty_run * 10 + digit as usize;
                continue;
            }

            file += empty_run;
            empty_run = 0;

            if file >= width {
                return Err(format!("Rank {} is too wide", rank + 1));
            }

            squares[rank * width + file] = character;
            file += 1;
        }

        file += empty_run;

        if file != width {
            return Err(format!(
                "Rank {} has {} files, not {}", rank + 1, file, files
            ));
        }
    }

    Ok(squares)
}

/*----------------------------------------------------------------------------*\
                                    THE LINK
\*----------------------------------------------------------------------------*/

/// CepLink
///
/// The host end of a CEP link. The engine is the host, the microcontroller
/// is the device. A reader thread takes the device lines, so a wait for a
/// reply can have a time limit.
///
/// Transport:
///
/// - serial, 8 data bits, no parity, 1 stop bit, at `CEP_BAUD`
/// - UTF-8 text, one message on each line, a line ends with `\n`
/// - a `\r` before the `\n` is allowed, the host removes it
/// - tokens are split by one or more spaces
///
/// Flow:
///
/// - each host line is a request number, then a `CepRequest`
/// - each device reply is the same number, then a `CepReply`
/// - the host sends a new request only after the reply or a time out
/// - a reply with another number is late, and the host drops it
/// - a device line that starts with `log` is no reply, the host logs it
///
/// The request number counts up from 1 on each link and wraps after
/// `u32::MAX`. A session of chess, with `>` for host lines and `<` for
/// device lines. The `board` reply is one line, split here to fit:
///
/// ```text
/// > 1 hello 1
/// < 1 ready cheesy-arm
/// > 2 setup 8 8 rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR
/// < 2 ok
/// > 3 scan
/// < log camera on
/// < 3 board ********/********/8/8/8/8/********/********
///           0,7,2,1 2,7,2,1 4,2,1,6 5,2,1,6 6,0,2,8 0,0,4,7 4,0,2,2
/// > 4 play 4,1=. 4,3=P
/// < 4 done
/// > 5 play 4,4=p 4,6=.
/// < 5 done
/// > 6 end abort
/// < 6 ok
/// ```
///
/// The seven panels of that `board` reply, one letter for each panel:
///
/// ```text
///   8  A A B B C D E E      A  0,7,2,1   a8 to b8
///   7  F F F F C D E E      B  2,7,2,1   c8 to d8
///   6  F F F F C D E E      C  4,2,1,6   e3 to e8
///   5  F F F F C D E E      D  5,2,1,6   f3 to f8
///   4  F F F F C D E E      E  6,0,2,8   g1 to h8
///   3  F F F F C D E E      F  0,0,4,7   a1 to d7
///   2  F F F F G G E E      G  4,0,2,2   e1 to f2
///   1  F F F F G G E E
///      a b c d e f g h
/// ```
///
/// Notes:
/// Each request but `play` has a time out of `CEP_REPLY_TIMEOUT_MS`. A ply
/// that started always ends, also when the person stops the game, so the
/// physical board always holds a whole position.
///
pub struct CepLink {
    pub name: String,                                                           /* device name from `ready`           */
    writer: fs::File,                                                           /* write end of the device            */
    replies: Receiver<String>,                                                  /* device lines, but not `log`        */
    last_id: u32,                                                               /* number of the last request         */
}

impl CepLink {
    /// CepLink::open
    ///
    /// Opens the device and greets it. `stty` puts the device in raw mode
    /// first, so the kernel passes each line through as it is:
    ///
    /// 1. set raw mode and `CEP_BAUD` with `stty`
    /// 2. open the device for read and write
    /// 3. start the reader thread
    /// 4. send `hello` and wait for `ready`
    ///
    /// Params:
    /// - path: &str         -> the serial device, such as /dev/ttyACM0
    ///
    /// Return:
    /// Result<Self, String> -> the ready link, or why it failed
    ///
    pub fn open(path: &str) -> Result<Self, String> {
        let device_flag = if cfg!(target_os = "macos") { "-f" } else { "-F" };  /* BSD and GNU stty name it apart     */

        let raw_mode = Command::new("stty")
            .args([device_flag, path, "raw", "-echo", &CEP_BAUD.to_string()])
            .status()
            .map_err(|error| format!("Cannot run stty: {}", error))?;

        if !raw_mode.success() {
            return Err(format!("stty failed on {}", path));
        }

        let writer = OpenOptions::new()
            .read(true)
            .write(true)
            .open(path)
            .map_err(|error| format!("Cannot open {}: {}", path, error))?;
        let reader = writer
            .try_clone()
            .map_err(|error| format!("Cannot read {}: {}", path, error))?;

        let (sender, replies) = channel::<String>();

        thread::Builder::new()
            .name("cep-reader".to_string())
            .spawn(move || read_device_lines(reader, sender))
            .map_err(|error| format!("Cannot start the reader: {}", error))?;

        let mut link = Self {
            name: String::new(),
            writer,
            replies,
            last_id: 0,
        };

        match link.request(&CepRequest::Hello)? {
            CepReply::Ready(name) => link.name = name,
            _ => return Err("The device did not reply `ready`".to_string()),
        }

        log_3!("CEP link on {} to {}", path, link.name);

        Ok(link)
    }

    /// CepLink::request
    ///
    /// Sends one request and waits for its reply. The wait ends at the
    /// reply, at the time out, or when the device closes the link.
    ///
    /// - reply with this number : `Ok` with the reply
    /// - `error` with the number: `Err` with the device text
    /// - other device line      : logged and dropped, late or bad
    ///
    /// Params:
    /// - request: &CepRequest   -> the request to send
    ///
    /// Return:
    /// Result<CepReply, String> -> the reply, or why there is none
    ///
    pub fn request(
        &mut self,
        request: &CepRequest,
    ) -> Result<CepReply, String> {
        self.last_id = self.last_id.wrapping_add(1);

        let sent = format!("{} {}\n", self.last_id, request);

        (&self.writer)
            .write_all(sent.as_bytes())
            .and_then(|_| (&self.writer).flush())
            .map_err(|error| format!("Link write failed: {}", error))?;
        log_4!("CEP host: {}", sent.trim_end());

        let reply_limit = Duration::from_millis(CEP_REPLY_TIMEOUT_MS);
        let deadline = match request {
            CepRequest::Play(_) => None,
            _ => Some(Instant::now() + reply_limit),
        };

        loop {
            let received = match deadline {
                None => self.replies
                    .recv()
                    .map_err(|_| RecvTimeoutError::Disconnected),
                Some(limit) => self.replies.recv_timeout(
                    limit.saturating_duration_since(Instant::now())
                ),
            };

            let line = match received {
                Ok(line) => line,
                Err(RecvTimeoutError::Timeout) => {
                    return Err(format!("No reply to: {}", sent.trim_end()));
                }
                Err(RecvTimeoutError::Disconnected) => {
                    return Err("The device closed the link".to_string());
                }
            };

            match parse_reply(&line) {
                Ok((id, reply)) if id == self.last_id => {
                    log_4!("CEP device: {}", line);

                    return match reply {
                        CepReply::Error(text) => Err(text),
                        reply => Ok(reply),
                    };
                }
                _ => log_2!("CEP line dropped: {}", line),
            }
        }
    }
}

/// read_device_lines
///
/// The body of the reader thread. It gives each device line to the link,
/// but a `log` line goes to the engine log. The thread ends when the device
/// closes or sends bytes that are not UTF-8.
///
/// Params:
/// - reader: fs::File       -> read end of the device
/// - sender: Sender<String> -> lines for `CepLink::request`
///
fn read_device_lines(reader: fs::File, sender: Sender<String>) {
    for line in BufReader::new(reader).lines() {
        let Ok(line) = line else {
            return;
        };

        let line = line.trim_end_matches('\r').to_string();
        let mut tokens = line.split_whitespace();

        match tokens.next() {
            None => {}
            Some("log") => {
                let text: Vec<&str> = tokens.collect();
                log_3!("CEP device log: {}", text.join(" "));
            }
            Some(_) => {
                if sender.send(line).is_err() {
                    return;
                }
            }
        }
    }
}
