//! translation.rs
//!
//! Translates between the internal notation and the protocol notations.
//!
//! The engine has its own names for squares, pieces and moves. Each
//! protocol uses a different notation. This file compiles the `.dict`
//! rules that translate FEN in the two directions and moves outward.
//! Thus no protocol detail gets into the engine.
//!
//! Created: 23/05/2026
//! Author : Alden Luthfi

use crate::*;

/*----------------------------------------------------------------------------*\
                             TRANSLATION RULE SETS
\*----------------------------------------------------------------------------*/

/// TranslatorGroup
///
/// All protocol translators of one variant. One `.dict` file gives the
/// rules for all protocols of the variant.
///
pub struct TranslatorGroup {
    pub list: Vec<Translator>,                                                  /* one translator per protocol        */
}

/// Translator
///
/// The translation rules of one protocol for one variant. Each list has
/// (regex, replacement) pairs from the `.dict` file of the variant.
///
/// - fen         : internal FEN to protocol FEN
/// - inverse_fen : protocol FEN to internal FEN, in reverse order
/// - moves       : internal move text to protocol notation
///
/// The rules apply in sequence, so the order is important. Moves have no
/// inverse list. An input move is found by comparison with the rendered
/// legal moves.
///
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
    /// Finds the embedded dictionary of a variant and compiles the rules of
    /// one protocol. The dictionaries are in the binary, so there is no
    /// file read.
    ///
    /// Params:
    /// - variant        : &str -> variant name, matches `<name>.dict`
    /// - target_protocol: &str -> protocol section to load, e.g. "uci"
    ///
    /// Return:
    /// Option<Self>            -> the translator, or None if no dictionary
    ///
    pub fn find(variant: &str, target_protocol: &str) -> Option<Self> {
        let filename = format!("{}.dict", variant);
        let content = EMBEDDED_DICTS
            .get_file(&filename)?
            .contents_utf8()?;
        Some(Translator::from_content(content, target_protocol))
    }

    /// Translator::from_content
    ///
    /// Compiles dictionary text into a translator. The protocol must have
    /// three sections, and each rule line must be correct.
    ///
    /// - `= protocols =`    : list of protocols, must include this one
    /// - `= <name> fen =`   : FEN rules, one or two directions
    /// - `= <name> moves =` : move rules, outward only
    ///
    /// The arrow of a FEN rule gives its direction:
    ///
    /// - `internal -> protocol`  : outward only
    /// - `internal <- protocol`  : inward only
    /// - `internal <-> protocol` : both, one entry in each list
    ///
    /// The internal text is on the left and the protocol text on the right:
    ///
    /// ```text
    /// = protocols =
    /// uci
    ///
    /// = uci fen =
    /// 010018P <-> a3
    /// \* -> -
    ///
    /// = uci moves =
    /// \*[a-z][0-9]+@[a-z][0-9]+ ->
    /// ```
    ///
    /// Params:
    /// - content        : &str -> raw `.dict` file text
    /// - target_protocol: &str -> protocol section to compile
    ///
    /// Return:
    /// Self                    -> the compiled translator
    ///
    /// Notes:
    /// The parser tests `<->` first, because it contains `->` and `<-`. The
    /// inverse list is reversed at the end, because an undo of ordered
    /// rewrites must go from the last rule to the first.
    ///
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

