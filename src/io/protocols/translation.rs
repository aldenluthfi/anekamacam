//! translation.rs
//!
//! Handles translation between internal representations and external
//! protocols, such as UCI or custom formats.
//!
//! The engine names squares, pieces, and moves in its own internal terms, but
//! every protocol it speaks to uses a different dialect. This file is the seam
//! between the two, so the quirks of any one protocol never leak inward: it
//! maps the two things that actually cross the boundary — board states as
//! FEN, and moves as protocol notation — in both directions.
//!
//! Created: 23/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             TRANSLATION RULE SETS
\*----------------------------------------------------------------------------*/

/// TranslatorGroup
///
/// Every protocol a variant speaks, compiled from that variant's single
/// dictionary file. One `.dict` describes all of them, so the group is what
/// a whole dictionary becomes once it has been read.
pub struct TranslatorGroup {
    pub list: Vec<Translator>,                                                  /* one translator per protocol        */
}

/// Translator
///
/// One protocol's translation rules for one variant: ordered lists of regex
/// and replacement, compiled out of that variant's `.dict` file. A GUI that
/// wants different piece letters or a different coordinate style than the
/// engine uses internally is accommodated here and nowhere else.
///
/// - fen         : internal FEN → the protocol's dialect
/// - inverse_fen : the protocol's FEN → internal, in reverse order
/// - moves       : internal move text → the protocol's notation
///
/// Rules are ordered because they are applied in sequence and an earlier
/// rewrite changes what a later pattern sees. Moves travel one way only: a
/// move coming back in is resolved by generating and rendering candidates
/// rather than by rewriting text, so no inverse list is needed for it.
#[derive(Clone)]
pub struct Translator {
    pub protocol: String,                                                       /* protocol name, e.g. uci            */
    pub fen: Vec<(Regex, String)>,                                              /* internal FEN to protocol rules     */
    pub inverse_fen: Vec<(Regex, String)>,                                      /* protocol FEN back to internal      */
    pub moves: Vec<(Regex, String)>,                                            /* move-text rewrite rules            */
}

/*----------------------------------------------------------------------------*\
                             DICTIONARY COMPILATION
\*----------------------------------------------------------------------------*/

impl Translator {
    /// Translator::find
    ///
    /// Looks up the embedded dictionary for a variant and compiles one
    /// protocol's rules out of it. Dictionaries ship inside the binary, so
    /// this is a lookup rather than a file read and a variant either has a
    /// dictionary at build time or never will.
    ///
    /// Params:
    /// - variant        : &str -> variant name, matches `<name>.dict`
    /// - target_protocol: &str -> protocol section to load, e.g. "uci"
    ///
    /// Return:
    /// Option<Self>            -> the translator, or None if no dictionary
    pub fn find(variant: &str, target_protocol: &str) -> Option<Self> {
        let filename = format!("{}.dict", variant);
        let content = EMBEDDED_DICTS
            .get_file(&filename)?
            .contents_utf8()?;
        Some(Translator::from_content(content, target_protocol))
    }

    /// Translator::from_content
    ///
    /// Compiles dictionary text into a translator. Three sections must be
    /// present for the named protocol, and every one of its rules must be a
    /// well-formed line, or the build is wrong rather than the input.
    ///
    /// - `[protocols]`    : the protocol must name itself here to be
    ///                      loadable
    /// - `[<name> fen]`   : board-state rules, in either or both directions
    /// - `[<name> moves]` : move-text rules, forward only
    ///
    /// A fen rule says which way it travels, and a two-way rule compiles
    /// into one entry in each list:
    ///
    /// - `internal -> protocol`  : outbound only, nothing reads it back
    /// - `internal <- protocol`  : inbound only, nothing writes it out
    /// - `internal <-> protocol` : both, the same pattern serving each way
    ///
    /// The two-way form is tested for first, since it contains both of the
    /// one-way forms and would otherwise be read as one of them. The inverse
    /// list is reversed once at the end: undoing an ordered sequence of
    /// rewrites means applying the undo steps back to front, and a dictionary
    /// whose rules overlap gives a different board otherwise.
    ///
    /// Params:
    /// - content        : &str -> raw `.dict` file text
    /// - target_protocol: &str -> protocol section to compile
    ///
    /// Return:
    /// Self                    -> the compiled translator
    pub fn from_content(content: &str, target_protocol: &str) -> Self {
        let sections = split_sections(content);

        let fen_section = format!("{} fen", target_protocol);
        let move_section = format!("{} moves", target_protocol);

        let mandatory_sections = [
            "protocols",
            &fen_section,
            &move_section,
        ];

        let missing: Vec<_> = mandatory_sections
            .iter()
            .filter(|s| !sections.contains_key(**s))
            .cloned()
            .collect();

        assert!(
            missing.is_empty(),
            "Missing mandatory sections: {}",
            missing.join(", ")
        );

        let protocol = sections["protocols"]
            .iter()
            .find(|line| line.trim() == target_protocol)
            .expect("Target protocol not found in protocols section")
            .trim()
            .to_string();

        let mut fen = Vec::new();
        let mut inverse_fen = Vec::new();
        let mut moves = Vec::new();

        sections[&fen_section].iter().for_each(|line| {
            if line.contains("<->") {
                let parts: Vec<_> = line.split("<->").map(str::trim).collect();
                if parts.len() == 2 {

                    fen.push((
                        Regex::new(parts[0])
                        .expect("Invalid regex in fen dictionary"),
                        parts[1].to_string(),
                    ));

                    inverse_fen.push((
                        Regex::new(parts[1])
                            .expect("Invalid regex in inverse fen dictionary"),
                        parts[0].to_string(),
                    ));

                } else {
                    panic!("Invalid line in fen section: {}", line);
                }
            } else if line.contains("->") {
                let parts: Vec<_> = line.split("->").map(str::trim).collect();
                if parts.len() == 2 {
                    fen.push((
                        Regex::new(parts[0])
                            .expect("Invalid regex in fen dictionary"),
                        parts[1].to_string(),
                    ));
                } else {
                    panic!("Invalid line in fen section: {}", line);
                }
            } else if line.contains("<-") {
                let parts: Vec<_> = line.split("<-").map(str::trim).collect();
                if parts.len() == 2 {
                    inverse_fen.push((
                        Regex::new(parts[1])
                            .expect("Invalid regex in inverse fen dictionary"),
                        parts[0].to_string(),
                    ));
                } else {
                    panic!("Invalid line in fen section: {}", line);
                }
            } else {
                panic!("Invalid line in fen section: {}", line);
            }
        });

        sections[&move_section].iter().for_each(|line| {
            if line.contains("->") {
                let parts: Vec<_> = line.split("->").map(str::trim).collect();
                if parts.len() == 2 {
                    moves.push((
                        Regex::new(parts[0])
                            .expect("Invalid regex in moves dictionary"),
                        parts[1].to_string(),
                    ));
                } else {
                    panic!("Invalid line in moves section: {}", line);
                }
            } else {
                panic!("Invalid line in moves section: {}", line);
            }
        });

        inverse_fen.reverse();

        Translator {
            protocol,
            fen,
            inverse_fen,
            moves,
        }
    }
}

