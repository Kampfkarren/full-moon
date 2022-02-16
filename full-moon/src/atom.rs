use logos::{Lexer, Logos};

use crate::ShortString;

pub(crate) fn trim_bracket_head(slice: &str) -> (ShortString, Option<usize>) {
    match test_bracket_head(slice) {
        Some(count) => {
            let trim = &slice[count + 2..slice.len() - count - 2];

            (trim.into(), Some(count))
        }
        None => (slice.into(), None),
    }
}

fn test_bracket_head(slice: &str) -> Option<usize> {
    // starts with `[`?
    if !slice.starts_with('[') {
        return None;
    }

    // how many `=` after `[`?
    let count = slice.chars().skip(1).take_while(|&v| v == '=').count();

    // ends with `[`?
    if !matches!(slice.chars().nth(count + 1), Some('[')) {
        return None;
    }

    Some(count)
}

fn read_bracketed(lex: &mut Lexer<Atom>, skips: usize) -> bool {
    let num_eq = match lex.slice().get(skips..).and_then(test_bracket_head) {
        Some(v) => v,
        None => return false,
    };

    let mut search = false;
    let mut num = 0;

    for (i, v) in lex.remainder().char_indices() {
        match (search, v) {
            (true, '=') => num += 1,
            (true, ']') if num_eq == num => {
                lex.bump(i + 1);

                return true;
            }
            (false, ']') => {
                search = true;
                num = 0;
            }
            _ => search = false,
        }
    }

    false
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum Atom {
    #[token("and")]
    And,

    #[token("break")]
    Break,

    #[token("do")]
    Do,

    #[token("else")]
    Else,

    #[token("elseif")]
    ElseIf,

    #[token("end")]
    End,

    #[token("false")]
    False,

    #[token("for")]
    For,

    #[token("function")]
    Function,

    #[token("if")]
    If,

    #[token("in")]
    In,

    #[token("local")]
    Local,

    #[token("nil")]
    Nil,

    #[token("not")]
    Not,

    #[token("or")]
    Or,

    #[token("repeat")]
    Repeat,

    #[token("return")]
    Return,

    #[token("then")]
    Then,

    #[token("true")]
    True,

    #[token("until")]
    Until,

    #[token("while")]
    While,

    #[cfg(feature = "lua52")]
    #[token("goto")]
    Goto,

    #[token("+=")]
    PlusEqual,

    #[token("-=")]
    MinusEqual,

    #[token("*=")]
    StarEqual,

    #[token("/=")]
    SlashEqual,

    #[token("%=")]
    PercentEqual,

    #[token("^=")]
    CaretEqual,

    #[token("..=")]
    TwoDotsEqual,

    #[cfg(feature = "roblox")]
    #[token("&")]
    Ampersand,

    #[cfg(feature = "roblox")]
    #[token("->")]
    ThinArrow,

    #[cfg(any(feature = "roblox", feature = "lua52"))]
    #[token("::")]
    TwoColons,

    #[token("^")]
    Caret,

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token("...")]
    Ellipse,

    #[token("..")]
    TwoDots,

    #[token(".")]
    Dot,

    #[token("==")]
    TwoEqual,

    #[token("=")]
    Equal,

    #[token(">=")]
    GreaterThanEqual,

    #[token(">")]
    GreaterThan,

    #[token("#")]
    Hash,

    #[token("[")]
    LeftBracket,

    #[token("{")]
    LeftBrace,

    #[token("(")]
    LeftParen,

    #[token("<=")]
    LessThanEqual,

    #[token("<")]
    LessThan,

    #[token("-")]
    Minus,

    #[token("%")]
    Percent,

    #[cfg(feature = "roblox")]
    #[token("|")]
    Pipe,

    #[token("+")]
    Plus,

    #[cfg(feature = "roblox")]
    #[token("?")]
    QuestionMark,

    #[token("}")]
    RightBrace,

    #[token("]")]
    RightBracket,

    #[token(")")]
    RightParen,

    #[token(";")]
    Semicolon,

    #[token("/")]
    Slash,

    #[token("*")]
    Star,

    #[token("~=")]
    TildeEqual,

    #[regex(r"#!.*\n")]
    Shebang,

    #[token("\u{feff}")]
    Bom,

    #[regex(r"[_\p{L}][_\p{L}\p{N}]*")]
    Identifier,

    #[cfg(feature = "roblox")]
    #[regex(r"0[bB][01_]+([eE][01_]+)?(\.[01_]*)?")]
    #[regex(r"0[xX][0-9a-fA-F_]+")]
    #[regex(r"\.[0-9][0-9_]*([eE][\+\-]?[0-9_]+)?")]
    #[regex(r"[0-9][0-9_]*(\.[0-9_]*)?([eE][\+\-]?[0-9_]+)?")]
    Number,

    #[cfg(not(feature = "roblox"))]
    #[regex(r"0[xX][0-9a-fA-F]+")]
    #[regex(r"\.[0-9]+([eE][\+\-]?[0-9]+)?")]
    #[regex(r"[0-9]+(\.[0-9]*)?([eE][\+\-]?[0-9]+)?")]
    Number,

    #[regex(r"'([^']|\\[\S\s])*'")]
    ApostropheString,

    #[regex(r#""([^"]|\\[\S\s])*""#)]
    QuoteString,

    #[regex(r"\[=*\[", |x| read_bracketed(x, 0))]
    MultiLineString,

    #[regex(r"--([^\n(\[=*\[)].*)?")]
    SingleLineComment,

    #[regex(r"--\[=*\[", |x| read_bracketed(x, 2))]
    MultiLineComment,

    #[regex(r"[ \t]*(\r?\n)?")]
    Whitespace,

    #[error]
    Unknown,
}
