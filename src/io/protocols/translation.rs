//! # translation.rs
//!
//! Handles translation between internal representations and external protocols,
//! such as UCI or custom formats.
//!
//! There are two main translation points, FEN and moves. FEN is used for
//! representing board states, while move translation is necessary for
//! interpreting commands from protocols like UCI.
//!
//! # Author
//! Alden Luthfi
//!
//! # Date
//! 23/05/2026
use crate::*;

/// A group of translators for different protocols, loaded from a single
/// dictionary file.
pub struct TranslatorGroup {
    pub list: Vec<Translator>,
}

#[derive(Clone)]
pub struct Translator {
    pub protocol: String,
    pub fen: Vec<(Regex, String)>,
    pub inverse_fen: Vec<(Regex, String)>,
    pub moves: Vec<(Regex, String)>,
}

impl Translator {
    pub fn find(variant: &str, target_protocol: &str) -> Option<Self> {
        let path = format!("{}/{}.dict", DICTS_DIR, variant);

        if !Path::new(&path).is_file() {
            return None;
        }

        Some(Translator::from_file(&path, target_protocol))
    }

    pub fn from_file(path: &str, target_protocol: &str) -> Self {
        let file_str =
            fs::read_to_string(path).expect("Failed to read dictionary file");

        let uncommented_str = COMMENT_PATTERN.replace_all(&file_str, "");
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

