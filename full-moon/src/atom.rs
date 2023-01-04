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
    #[cfg(feature = "lua52")]
    let mut z_escaped = false;
    for char in lex.remainder().chars() {
        match (escape, char) {
            #[cfg(feature = "lua52")]
            (true, 'z') => {
                escape = false;
                z_escaped = true
            }
            (true, ..) => escape = false,
            (false, '\\') => escape = true,
            #[cfg(feature = "lua52")]
            (false, '\n' | '\r') if z_escaped => z_escaped = false,
            (false, '\n' | '\r') => break,
            (false, ..) if char == quote => {
                lex.bump(1);
                return true;
            }
            _ => {}
        }
        lex.bump(char.len_utf8());
    }
    false
}

fn proceed_with_bracketed(lex: &mut Lexer<Atom>, block_count: usize) -> bool {
    let mut in_tail = false;
    let mut current_count = 0;

    for (pos, char) in lex.remainder().char_indices() {
        match (in_tail, char) {
            (true, '=') => current_count += 1,
            (true, ']') if block_count == current_count => {
                lex.bump(pos + 1);

                return true;
            }
            (_, ']') => {
                in_tail = true;
                current_count = 0;
            }
            _ => in_tail = false,
        }
    }

    false
}

fn read_bracketed(lex: &mut Lexer<Atom>, skips: usize) -> bool {
    let block_count = match lex.slice().get(skips..).and_then(test_bracket_head) {
        Some(value) => value,
        None => return false,
    };

    proceed_with_bracketed(lex, block_count)
}

fn read_comment(lexer: &mut Lexer<Atom>) -> bool {
    let mut remainder = lexer.remainder().char_indices().peekable();

    if matches!(remainder.peek(), Some((_, '['))) {
        remainder.next();

        let mut block_count = 0;

        loop {
            let next = remainder.next();

            match next {
                Some((_, '=')) => block_count += 1,

                Some((offset, '[')) => {
                    // Confirmed real multi-line comment
                    lexer.bump(offset + 1);
                    return proceed_with_bracketed(lexer, block_count);
                }

                // Not a multi-line comment, just --[text
                Some((offset, _)) => {
                    lexer.bump(offset);
                    break;
                }

                None => return false,
            }
        }
    }

    // Normal single line comment.
    // Reset remainder since it might've been bumped.
    for (offset, char) in lexer.remainder().char_indices() {
        if char == '\n' {
            lexer.bump(offset);
            return true;
        }
    }

    // The rest of the string is a comment
    lexer.bump(lexer.remainder().chars().count());
    true
}

#[cfg(not(feature = "roblox"))]
fn read_right_brace(_: &mut Lexer<Atom>) -> Option<Option<InterpolatedStringSection>> {
    Some(None)
}

#[derive(Clone, Debug, Default)]
pub(crate) struct TokenizerState {
    #[cfg(feature = "roblox")]
    pub brace_stack: Vec<crate::tokenizer_luau::BraceType>,
}

#[derive(Logos, Debug, Clone, Copy, PartialEq, Eq)]
#[logos(extras = TokenizerState)]
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

    #[cfg(any(feature = "roblox", feature = "lua53"))]
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
    GreaterThan, // Lua 5.3: we cannot include DoubleGreaterThan '>>' in the tokenizer as it collides with Luau generics

    #[token("#")]
    Hash,

    #[token("[")]
    LeftBracket,

    #[cfg_attr(not(feature = "roblox"), token("{"))]
    #[cfg_attr(
        feature = "roblox",
        regex(r"\{", crate::tokenizer_luau::read_left_brace)
    )]
    LeftBrace,

    #[token("(")]
    LeftParen,

    #[token("<=")]
    LessThanEqual,

    #[token("<")]
    LessThan,

    #[cfg(feature = "lua53")]
    #[token("<<")]
    DoubleLessThan,

    #[token("-")]
    Minus,

    #[token("%")]
    Percent,

    #[cfg(any(feature = "roblox", feature = "lua53"))]
    #[token("|")]
    Pipe,

    #[token("+")]
    Plus,

    #[cfg(feature = "roblox")]
    #[token("?")]
    QuestionMark,

    #[cfg_attr(not(feature = "roblox"), regex(r"\}", read_right_brace))]
    #[cfg_attr(
        feature = "roblox",
        regex(r"\}", crate::tokenizer_luau::read_interpolated_string_right_brace)
    )]
    RightBrace(Option<InterpolatedStringSection>),

    #[token("]")]
    RightBracket,

    #[token(")")]
    RightParen,

    #[token(";")]
    Semicolon,

    #[token("/")]
    Slash,

    #[cfg(feature = "lua53")]
    #[token("//")]
    DoubleSlash,

    #[token("*")]
    Star,

    #[cfg(feature = "lua53")]
    #[token("~")]
    Tilde,

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

    #[cfg(feature = "roblox")]
    #[regex(r"`", crate::tokenizer_luau::read_interpolated_string_begin)]
    InterpolatedStringBegin(InterpolatedStringBegin),

    // These don't work, even with priority set! Ideally, this would be what we use.
    // #[regex(r"--.*")]
    // SingleLineComment,

    // #[regex(r"--\[=*\[", |x| read_bracketed(x, 2))]
    // MultiLineComment,
    #[regex(r"--", read_comment)]
    Comment,

    #[regex(r"[ \t]*(\r?\n)?")]
    Whitespace,

    #[error]
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg(feature = "roblox")]
pub enum InterpolatedStringBegin {
    Formatted, // `uh {oh}`
    Simple,    // `no formatting`
}

#[cfg(feature = "roblox")]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpolatedStringSection {
    Middle, // } ... {
    End,    // }
}

// This existing, but being empty makes things much easier
#[cfg(not(feature = "roblox"))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InterpolatedStringSection {}
