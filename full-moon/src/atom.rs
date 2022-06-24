use logos::{Lexer, Logos};

use crate::ShortString;

pub fn trim_bracket_head(slice: &str) -> (ShortString, Option<usize>) {
    match test_bracket_head(slice) {
        Some(count) => {
            let trim = &slice[count + 2..slice.len() - count - 2];

            (trim.into(), Some(count))
        }
        None => (slice.into(), None),
    }
}

fn test_bracket_head(slice: &str) -> Option<usize> {
    if !slice.starts_with('[') {
        return None;
    }

    let count = slice.chars().skip(1).take_while(|&v| v == '=').count();

    if !matches!(slice.chars().nth(count + 1), Some('[')) {
        return None;
    }

    Some(count)
}

fn read_string(lex: &mut Lexer<Atom>, quote: char) -> bool {
    let mut escape = false;
    for char in lex.remainder().chars() {
        match (escape, char) {
            (true, ..) => escape = false,
            (false, '\\') => escape = true,
            (false, char) if char == quote => {
                lex.bump(1);
                return true;
            }
            _ => {}
        }
        lex.bump(char.len_utf8());
    }
    false
}

fn read_bracketed(lex: &mut Lexer<Atom>, skips: usize) -> bool {
    let num_eq = match lex.slice().get(skips..).and_then(test_bracket_head) {
        Some(value) => value,
        None => return false,
    };

    let mut in_tail = false;
    let mut num = 0;

    for (pos, char) in lex.remainder().char_indices() {
        match (in_tail, char) {
            (true, '=') => num += 1,
            (true, ']') if num_eq == num => {
                lex.bump(pos + 1);

                return true;
            }
            (false, ']') => {
                in_tail = true;
                num = 0;
            }
            _ => in_tail = false,
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

    #[cfg(feature = "roblox")]
    #[token("+=")]
    PlusEqual,

    #[cfg(feature = "roblox")]
    #[token("-=")]
    MinusEqual,

    #[cfg(feature = "roblox")]
    #[token("*=")]
    StarEqual,

    #[cfg(feature = "roblox")]
    #[token("/=")]
    SlashEqual,

    #[cfg(feature = "roblox")]
    #[token("%=")]
    PercentEqual,

    #[cfg(feature = "roblox")]
    #[token("^=")]
    CaretEqual,

    #[cfg(feature = "roblox")]
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

    #[regex(r"'", |x| read_string(x, '\''))]
    ApostropheString,

    #[regex(r#"""#, |x| read_string(x, '"'))]
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
