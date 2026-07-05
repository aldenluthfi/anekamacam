//! # translation.rs
//!
//! Handles translation between internal representations and external
//! protocols, such as UCI or custom formats.
//!
//! The engine names squares, pieces, and moves in its own internal terms, but
//! every protocol it speaks to uses a different dialect. This file is the seam
//! between the two, so the quirks of any one protocol never leak inward: it
//! maps the two things that actually cross the boundary -- board states as
//! FEN, and moves as protocol notation -- in both directions.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 23/05/2026
use crate::*;

/// TranslatorGroup
///
/// A group of translators for different protocols, loaded from a single
/// dictionary file. Each entry is one protocol's compiled rule set, so a
/// variant's whole dictionary is held as one addressable collection.
pub struct TranslatorGroup {
    pub list: Vec<Translator>,                                                  /* one translator per protocol        */
}

/// Translator
///
/// One protocol's translation rules for a variant.
/// Holds ordered regex/replacement rule lists parsed from a `.dict`
/// file: `fen` rewrites internal FENs into the protocol's dialect,
/// `inverse_fen` rewrites them back, and `moves` rewrites move text.
/// External GUIs often expect different piece letters or coordinate
/// styles than the engine's internal notation; these rules bridge that.
#[derive(Clone)]
pub struct Translator {
    pub protocol: String,                                                       /* protocol name, e.g. uci            */
    pub fen: Vec<(Regex, String)>,                                              /* internal FEN to protocol rules     */
    pub inverse_fen: Vec<(Regex, String)>,                                      /* protocol FEN back to internal      */
    pub moves: Vec<(Regex, String)>,                                            /* move-text rewrite rules            */
}

impl Translator {
    /// Translator::find
    ///
    /// Looks up the embedded dictionary for a variant and builds the
    /// translator for one protocol from it.
    ///
    /// Params:
    /// - variant: &str         -> variant name, matches `<name>.dict`
    /// - target_protocol: &str -> protocol section to load, e.g. "uci"
    ///
    /// Return:
    /// Option<Self> -> the translator, or None if no dictionary exists
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
    /// Parses dictionary text into a translator: comments are stripped,
    /// the file is split into `[section]` bodies, the protocol's fen and
    /// moves sections are compiled into ordered regex/replacement rule
    /// lists, and inverse FEN rules are derived by swapping each rule's
    /// sides. Panics on missing mandatory sections or malformed rules,
    /// since dictionaries ship embedded and must be valid.
    ///
    /// Params:
    /// - content: &str         -> raw `.dict` file text
    /// - target_protocol: &str -> protocol section to compile
    ///
    /// Return:
    /// Self -> the compiled translator
    ///
    pub fn from_content(content: &str, target_protocol: &str) -> Self {
        let uncommented_str = COMMENT_PATTERN.replace_all(content, "");
        let cleaned = uncommented_str
            .lines()
            .map(|line| line.trim())
            .filter(|line| !line.is_empty())
            .collect::<Vec<_>>()
            .join("\n");
        let section_titles = SECTION_PATTERN
            .captures_iter(&cleaned);
        let section_contents = SECTION_PATTERN
            .split(&cleaned)
            .filter(|content| !content.trim().is_empty());

        let mut sections = HashMap::new();

        for (title, content) in section_titles.zip(section_contents) {
            let section_name = title[1].trim().to_string();
            let section_body = content
                .lines()
                .map(str::to_string)
                .filter(|line| !line.trim().is_empty())
                .collect::<Vec<String>>();
            sections.insert(section_name, section_body);
        }


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

