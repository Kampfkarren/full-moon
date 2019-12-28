use crate::visitors::{Visit, VisitMut, Visitor, VisitorMut};
use atomic_refcell::AtomicRefCell;
use full_moon_derive::symbols;
use generational_arena::{Arena, Index};
use lazy_static::lazy_static;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_till, take_while, take_while1},
    character::complete::{anychar, line_ending, space1},
    combinator::{opt, recognize},
    multi::many_till,
    sequence::{delimited, pair, preceded, tuple},
    IResult,
};
use regex::{self, Regex};
#[cfg(feature = "serde")]
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::{
    borrow::Cow,
    cmp::Ordering,
    fmt,
    str::FromStr,
    sync::{
        atomic::{AtomicUsize, Ordering as AtomicOrdering},
        Arc,
    },
};

symbols!(
    And => "and",
    Break => "break",
    Do => "do",
    ElseIf => "elseif",
    Else => "else",
    End => "end",
    False => "false",
    For => "for",
    Function => "function",
    If => "if",
    In => "in",
    Local => "local",
    Nil => "nil",
    Not => "not",
    Or => "or",
    Repeat => "repeat",
    Return => "return",
    Then => "then",
    True => "true",
    Until => "until",
    While => "while",

    Caret => "^",
    Colon => ":",
    Comma => ",",
    Ellipse => "...",
    TwoDots => "..",
    Dot => ".",
    TwoEqual => "==",
    Equal => "=",
    GreaterThanEqual => ">=",
    GreaterThan => ">",
    Hash => "#",
    LeftBrace => "{",
    LeftBracket => "[",
    LeftParen => "(",
    LessThanEqual => "<=",
    LessThan => "<",
    Minus => "-",
    Percent => "%",
    Plus => "+",
    RightBrace => "}",
    RightBracket => "]",
    RightParen => ")",
    Semicolon => ";",
    Slash => "/",
    Star => "*",
    TildeEqual => "~=",
);

/// The possible errors that can happen while tokenizing.
#[derive(Clone, Copy, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TokenizerErrorType {
    /// An unclosed multi-line comment was found
    UnclosedComment,
    /// An unclosed string was found
    UnclosedString,
    /// An unexpected token was found
    UnexpectedToken(char),
}

/// The type of tokens in parsed code
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
pub enum TokenType<'a> {
    /// End of file, should always be the very last token
    Eof,

    /// An identifier, such as `foo`
    Identifier {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The identifier itself
        identifier: Cow<'a, str>,
    },

    /// A multi line comment in the format of --[[ comment ]]
    MultiLineComment {
        /// Number of equals signs, if any, for the multi line comment
        /// For example, `--[=[` would have a `blocks` value of `1`
        blocks: usize,
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The comment itself, ignoring opening and closing tags
        comment: Cow<'a, str>,
    },

    /// A literal number, such as `3.3`
    Number {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The text representing the number, includes details such as `0x`
        text: Cow<'a, str>,
    },

    /// A single line comment, such as `-- comment`
    SingleLineComment {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The comment, ignoring initial `--`
        comment: Cow<'a, str>,
    },

    /// A literal string, such as "Hello, world"
    StringLiteral {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The literal itself, ignoring quotation marks
        literal: Cow<'a, str>,
        #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
        /// Number of equals signs used for a multi line string, if it is one
        /// For example, `[=[string]=]` would have a `multi_line` value of Some(1)
        /// [[string]] would have a `multi_line` value of Some(0)
        /// A string such as `"string"` would have a `multi_line` value of None
        multi_line: Option<usize>,
        /// The type of quotation mark used to make the string
        quote_type: StringLiteralQuoteType,
    },

    /// A [`Symbol`](enum.Symbol.html), such as `local` or `+`
    Symbol {
        /// The symbol itself
        symbol: Symbol,
    },

    /// Whitespace, such as tabs or new lines
    Whitespace {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// Characters consisting of the whitespace
        characters: Cow<'a, str>,
    },
}

impl<'a> TokenType<'a> {
    /// Returns whether a token can be practically ignored in most cases
    /// Comments and whitespace will return `true`, everything else will return `false`
    pub fn ignore(&self) -> bool {
        match self {
            TokenType::SingleLineComment { .. }
            | TokenType::MultiLineComment { .. }
            | TokenType::Whitespace { .. } => true,
            _ => false,
        }
    }

    /// Returns the [`TokenKind`](enum.TokenKind.html) of the token type.
    ///
    /// ```rust
    /// use std::borrow::Cow;
    /// use full_moon::tokenizer::{TokenKind, TokenType};
    ///
    /// assert_eq!(
    ///     TokenType::Identifier {
    ///         identifier: Cow::from("hello")
    ///     }.kind(),
    ///     TokenKind::Identifier,
    /// );
    /// ```
    pub fn kind(&self) -> TokenKind {
        match self {
            TokenType::Eof => TokenKind::Eof,
            TokenType::Identifier { .. } => TokenKind::Identifier,
            TokenType::MultiLineComment { .. } => TokenKind::MultiLineComment,
            TokenType::Number { .. } => TokenKind::Number,
            TokenType::SingleLineComment { .. } => TokenKind::SingleLineComment,
            TokenType::StringLiteral { .. } => TokenKind::StringLiteral,
            TokenType::Symbol { .. } => TokenKind::Symbol,
            TokenType::Whitespace { .. } => TokenKind::Whitespace,
        }
    }
}

/// The kind of token. Contains no additional data.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TokenKind {
    /// End of file, should always be the very last token
    Eof,
    /// An identifier, such as `foo`
    Identifier,
    /// A multi line comment in the format of --[[ comment ]]
    MultiLineComment,
    /// A literal number, such as `3.3`
    Number,
    /// A single line comment, such as `-- comment`
    SingleLineComment,
    /// A literal string, such as "Hello, world"
    StringLiteral,
    /// A [`Symbol`](enum.Symbol.html), such as `local` or `+`
    Symbol,
    /// Whitespace, such as tabs or new lines
    Whitespace,
}

/// A token such consisting of its [`Position`](struct.Position.html) and a [`TokenType`](enum.TokenType.html)
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Token<'a> {
    pub(crate) start_position: Arc<AtomicPosition>,
    pub(crate) end_position: Arc<AtomicPosition>,
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[serde(with = "serde_arc_atomic_refcell")]
    pub(crate) token_type: Arc<AtomicRefCell<TokenType<'a>>>,
}

impl<'a> Token<'a> {
    /// The position a token begins at
    pub fn start_position(&self) -> Position {
        self.start_position.load()
    }

    /// The position a token ends at
    pub fn end_position(&self) -> Position {
        self.end_position.load()
    }

    /// The [type](enum.TokenType.html) of token as well as the data needed to represent it
    /// If you don't need any other information, use [`token_kind`](#method.token_kind) instead.
    pub fn token_type(&self) -> atomic_refcell::AtomicRef<TokenType<'a>> {
        self.token_type.borrow()
    }

    /// The [kind](enum.TokenKind.html) of token with no additional data.
    /// If you need any information such as idenitfier names, use [`token_type`](#method.token_type) instead.
    pub fn token_kind(&self) -> TokenKind {
        self.token_type().kind()
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenType::*;

        match &*self.token_type() {
            Eof => "".to_string(),
            Number { text } => text.to_string(),
            Identifier { identifier } => identifier.to_string(),
            MultiLineComment { blocks, comment } => {
                format!("--[{0}[{1}]{0}]", "=".repeat(*blocks), comment)
            }
            SingleLineComment { comment } => format!("--{}", comment),
            StringLiteral {
                literal,
                multi_line,
                quote_type,
            } => {
                if let Some(blocks) = multi_line {
                    format!("[{0}[{1}]{0}]", "=".repeat(*blocks), literal.to_string())
                } else {
                    format!("{0}{1}{0}", quote_type.to_string(), literal.to_string())
                }
            }
            Symbol { symbol } => symbol.to_string(),
            Whitespace { characters } => characters.to_string(),
        }
        .fmt(formatter)
    }
}

impl<'a> PartialEq<Self> for Token<'a> {
    fn eq(&self, rhs: &Self) -> bool {
        self.start_position() == rhs.start_position()
            && self.end_position() == rhs.end_position()
            && self.token_type == rhs.token_type
    }
}

impl<'a> Eq for Token<'a> {}

impl<'a> Ord for Token<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start_position().cmp(&other.start_position())
    }
}

impl<'a> PartialOrd for Token<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// A reference to a token used by Ast's.
/// Dereferences to a [`Token`](struct.Token.html)
#[derive(Clone)]
pub enum TokenReference<'a> {
    /// Token is borrowed from an Ast's arena
    #[doc(hidden)]
    Borrowed {
        arena: Arc<Arena<Token<'a>>>,
        index: Index,
    },

    /// Token reference was manually created, likely through deserialization
    #[doc(hidden)]
    Owned(Token<'a>),
}

impl<'a> TokenReference<'a> {
    /// Sets the type of token. Note that positions will not update after using this function.
    /// If you need them to, call [`Ast::update_positions`](../ast/struct.Ast.html#method.update_positions)
    pub fn set_token_type(&mut self, new_token_type: TokenType<'a>) {
        *self.token_type.borrow_mut() = new_token_type;
    }
}

impl<'a> std::borrow::Borrow<Token<'a>> for &TokenReference<'a> {
    fn borrow(&self) -> &Token<'a> {
        &**self
    }
}

impl<'a> std::ops::Deref for TokenReference<'a> {
    type Target = Token<'a>;

    fn deref(&self) -> &Self::Target {
        match self {
            TokenReference::Borrowed { arena, index } => {
                arena.get(*index).expect("arena doesn't have index?")
            }

            TokenReference::Owned(token) => &token,
        }
    }
}

impl<'a> fmt::Debug for TokenReference<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(formatter, "TokenReference {{ {} }}", **self)
    }
}

impl<'a> fmt::Display for TokenReference<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        (**self).fmt(formatter)
    }
}

impl<'a> PartialEq<Self> for TokenReference<'a> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(other)
    }
}

impl<'a> Eq for TokenReference<'a> {}

impl<'a> Ord for TokenReference<'a> {
    fn cmp(&self, other: &Self) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl<'a> PartialOrd for TokenReference<'a> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[cfg(feature = "serde")]
impl<'a> Serialize for TokenReference<'a> {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        (**self).serialize(serializer)
    }
}

#[cfg(feature = "serde")]
impl<'de: 'a, 'a> Deserialize<'de> for TokenReference<'a> {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(TokenReference::Owned(Token::deserialize(deserializer)?))
    }
}

impl<'ast> Visit<'ast> for TokenReference<'ast> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        visitor.visit_token(self);

        match self.token_kind() {
            TokenKind::Eof => visitor.visit_eof(self),
            TokenKind::Identifier => visitor.visit_identifier(self),
            TokenKind::MultiLineComment => visitor.visit_multi_line_comment(self),
            TokenKind::Number => visitor.visit_number(self),
            TokenKind::SingleLineComment => visitor.visit_single_line_comment(self),
            TokenKind::StringLiteral => visitor.visit_string_literal(self),
            TokenKind::Symbol => visitor.visit_symbol(self),
            TokenKind::Whitespace => visitor.visit_whitespace(self),
        }
    }
}

impl<'ast> VisitMut<'ast> for TokenReference<'ast> {
    fn visit_mut<V: VisitorMut<'ast>>(&mut self, visitor: &mut V) {
        visitor.visit_token(self);

        match self.token_kind() {
            TokenKind::Eof => visitor.visit_eof(self),
            TokenKind::Identifier => visitor.visit_identifier(self),
            TokenKind::MultiLineComment => visitor.visit_multi_line_comment(self),
            TokenKind::Number => visitor.visit_number(self),
            TokenKind::SingleLineComment => visitor.visit_single_line_comment(self),
            TokenKind::StringLiteral => visitor.visit_string_literal(self),
            TokenKind::Symbol => visitor.visit_symbol(self),
            TokenKind::Whitespace => visitor.visit_whitespace(self),
        }
    }
}

/// Used to represent exact positions of tokens in code
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Position {
    pub(crate) bytes: usize,
    pub(crate) character: usize,
    pub(crate) line: usize,
}

impl Position {
    /// How many bytes, ignoring lines, it would take to find this position
    pub fn bytes(self) -> usize {
        self.bytes
    }

    /// Index of the character on the line for this position
    pub fn character(self) -> usize {
        self.character
    }

    /// Line the position lies on
    pub fn line(self) -> usize {
        self.line
    }
}

impl Ord for Position {
    fn cmp(&self, other: &Self) -> Ordering {
        self.bytes.cmp(&other.bytes)
    }
}

impl PartialOrd for Position {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

#[derive(Debug)]
pub(crate) struct AtomicPosition {
    bytes: AtomicUsize,
    character: AtomicUsize,
    line: AtomicUsize,
}

impl AtomicPosition {
    fn new(position: Position) -> Self {
        AtomicPosition {
            bytes: AtomicUsize::new(position.bytes()),
            character: AtomicUsize::new(position.character()),
            line: AtomicUsize::new(position.line()),
        }
    }

    fn load(&self) -> Position {
        Position {
            bytes: self.bytes.load(AtomicOrdering::Acquire),
            character: self.character.load(AtomicOrdering::Acquire),
            line: self.line.load(AtomicOrdering::Acquire),
        }
    }

    pub fn store(&self, position: Position) {
        self.bytes.store(position.bytes(), AtomicOrdering::Release);
        self.character
            .store(position.character(), AtomicOrdering::Release);
        self.line.store(position.line(), AtomicOrdering::Release);
    }
}

impl<'de> Deserialize<'de> for AtomicPosition {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        Ok(AtomicPosition::new(Position::deserialize(deserializer)?))
    }
}

impl Serialize for AtomicPosition {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.load().serialize(serializer)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct TokenAdvancement<'a> {
    pub advance: usize,
    pub token_type: TokenType<'a>,
}

/// The types of quotes used in a Lua string
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum StringLiteralQuoteType {
    /// Strings formatted \[\[with brackets\]\]
    Brackets,
    /// Strings formatted "with double quotes"
    Double,
    /// Strings formatted 'with single quotes'
    Single,
}

impl<'a> fmt::Display for StringLiteralQuoteType {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StringLiteralQuoteType::Brackets => unreachable!(),
            StringLiteralQuoteType::Double => "\"",
            StringLiteralQuoteType::Single => "'",
        }
        .fmt(formatter)
    }
}

lazy_static! {
    static ref PATTERN_NUMBER: Regex = {
        let mut set = Vec::new();

        // Hexadecimal
        set.push(r"(0x[A-Fa-f\d]+)");

        if cfg!(feature = "roblox") {
            // Binary literals
            set.push(r"(0b[01]+)");
        }

        // Basic numbers (123456.346e123)
        set.push(r"(((\d*\.\d+)|(\d+))([eE]-?\d+)?)");

        Regex::new(&format!("^({})", set.join("|"))).unwrap()
    };

    static ref PATTERN_STRING_MULTI_LINE_BEGIN: Regex = Regex::new(r"\[(=*)\[").unwrap();
}

type Advancement<'a> = Result<Option<TokenAdvancement<'a>>, TokenizerErrorType>;

macro_rules! advance_regex {
    ($code:expr, $regex:ident, $token_type:ident($find:ident) $block:tt) => {
        if let Some($find) = $regex.find($code) {
            if $find.start() != 0 {
                Ok(None)
            } else {
                Ok(Some(TokenAdvancement {
                    advance: $find.as_str().chars().count(),
                    token_type: TokenType::$token_type $block,
                }))
            }
        } else {
            Ok(None)
        }
    };
}

#[inline]
fn parse_single_line_comment(code: &str) -> IResult<&str, &str> {
    preceded(tag("--"), take_till(|x: char| x.is_ascii_control()))(code)
}

#[inline]
fn parse_multi_line_comment_start(code: &str) -> IResult<&str, &str> {
    delimited(tag("--["), take_while(|x: char| x == '='), tag("["))(code)
}

#[inline]
fn parse_multi_line_comment_body<'a>(
    code: &'a str,
    block_count: &'a str,
) -> IResult<&'a str, &'a str> {
    recognize(many_till(
        anychar,
        recognize(tuple((tag("]"), tag(block_count), tag("]")))),
    ))(code)
}

fn advance_comment(code: &str) -> Advancement {
    if let Ok((code, block_count)) = parse_multi_line_comment_start(code) {
        return match parse_multi_line_comment_body(code, block_count) {
            Ok((_, comment)) => {
                let blocks = block_count.len();
                // Get the comment without the ending "]]"
                let comment = &comment[..(comment.len() - "]]".len() - blocks)];
                Ok(Some(TokenAdvancement {
                    advance: comment.len() + blocks * 2 + "--[[]]".len(),
                    token_type: TokenType::MultiLineComment {
                        blocks,
                        comment: Cow::from(comment),
                    },
                }))
            }
            Err(_) => Err(TokenizerErrorType::UnclosedComment),
        };
    }

    match parse_single_line_comment(code) {
        Ok((_, comment)) => Ok(Some(TokenAdvancement {
            advance: 2 + comment.chars().count(),
            token_type: TokenType::SingleLineComment {
                comment: Cow::from(comment),
            },
        })),
        Err(_) => Ok(None),
    }
}

fn advance_number(code: &str) -> Advancement {
    advance_regex!(code, PATTERN_NUMBER, Number(find) {
        text: Cow::from(find.as_str()),
    })
}

#[inline]
fn parse_identifier(code: &str) -> IResult<&str, &str> {
    recognize(pair(
        // Identifiers must start with at least 1 alphabetic character
        take_while1(|x: char| x.is_ascii_alphabetic() || x == '_'),
        // And then they must be followed by 0 or more alphanumeric (or '_') characters
        take_while(|x: char| x.is_ascii_alphanumeric() || x == '_'),
    ))(code)
}

fn advance_identifier(code: &str) -> Advancement {
    match parse_identifier(code) {
        Ok((_, identifier)) => Ok(Some(TokenAdvancement {
            advance: identifier.chars().count(),
            token_type: TokenType::Identifier {
                identifier: Cow::from(identifier),
            },
        })),
        Err(_) => Ok(None),
    }
}

fn advance_quote(code: &str) -> Advancement {
    if let Some(beginning) = PATTERN_STRING_MULTI_LINE_BEGIN.find(code) {
        if beginning.start() == 0 {
            let block_count = beginning.end() - beginning.start() - "[[".len();

            let end_regex = Regex::new(&format!(r"\]{}\]", "=".repeat(block_count))).unwrap();

            let end_find = match end_regex.find(code) {
                Some(find) => find,
                None => return Err(TokenizerErrorType::UnclosedString),
            };

            return Ok(Some(TokenAdvancement {
                advance: code[beginning.start()..end_find.end()].chars().count(),
                token_type: TokenType::StringLiteral {
                    multi_line: Some(block_count),
                    literal: Cow::from(&code[beginning.end()..end_find.start()]),
                    quote_type: StringLiteralQuoteType::Brackets,
                },
            }));
        }
    }

    let quote = if code.starts_with('"') {
        '"'
    } else if code.starts_with('\'') {
        '\''
    } else {
        return Ok(None);
    };

    let mut end = None;
    let mut escape = false;

    for (char_index, (byte_index, character)) in code.char_indices().enumerate().skip(1) {
        if character == '\\' {
            escape = !escape;
        } else if character == quote {
            if escape {
                escape = false;
            } else {
                end = Some((char_index, byte_index));
                break;
            }
        } else if (character == '\r' || character == '\n') && !escape {
            return Err(TokenizerErrorType::UnclosedString);
        } else {
            escape = false;
        }
    }

    if let Some((char_index, byte_index)) = end {
        Ok(Some(TokenAdvancement {
            advance: char_index + 1,
            token_type: TokenType::StringLiteral {
                literal: Cow::from(&code[1..byte_index]),
                multi_line: None,
                quote_type: match quote {
                    '"' => StringLiteralQuoteType::Double,
                    '\'' => StringLiteralQuoteType::Single,
                    _ => unreachable!(),
                },
            },
        }))
    } else {
        Err(TokenizerErrorType::UnclosedString)
    }
}

fn advance_symbol(code: &str) -> Advancement {
    match parse_symbol(code) {
        Ok((_, string)) => Ok(Some(TokenAdvancement {
            advance: string.chars().count(),
            token_type: TokenType::Symbol {
                symbol: Symbol::from_str(string).unwrap(),
            },
        })),

        Err(_) => Ok(None),
    }
}

#[inline]
fn parse_whitespace(code: &str) -> IResult<&str, &str> {
    // From regex "^[^\S\n]+\n?|\n"
    alt((recognize(pair(space1, opt(line_ending))), line_ending))(code)
}

// Keep finding whitespace until the line ends
fn advance_whitespace(code: &str) -> Advancement {
    match parse_whitespace(code) {
        Ok((_, whitespace)) => Ok(Some(TokenAdvancement {
            advance: whitespace.chars().count(),
            token_type: TokenType::Whitespace {
                characters: Cow::from(whitespace),
            },
        })),
        Err(_) => Ok(None),
    }
}

/// Information about an error that occurs while tokenizing
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TokenizerError {
    /// The type of error
    error: TokenizerErrorType,
    /// The position of the token that caused the error
    position: Position,
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{} at line {}, column {}",
            match self.error {
                TokenizerErrorType::UnclosedComment => "unclosed comment".to_string(),
                TokenizerErrorType::UnclosedString => "unclosed string".to_string(),
                TokenizerErrorType::UnexpectedToken(character) => {
                    format!("unexpected character {}", character)
                }
            },
            self.position.line,
            self.position.character,
        )
    }
}

impl std::error::Error for TokenizerError {}

/// Returns a list of [`Token`](struct.Token.html) structs.
/// You probably want [`parse`](../fn.parse.html) instead.
///
/// # Errors
///
/// If the code passed is malformed from normal Lua expectations,
/// a [`TokenizerError`](struct.TokenizerError.html) will be returned.
///
/// ```rust
/// # use full_moon::tokenizer::tokens;
/// assert!(tokens("local x = 1").is_ok());
/// assert!(tokens("local 4 = end").is_ok()); // tokens does *not* check validity of code, only tokenizing
/// assert!(tokens("--[[ Unclosed comment!").is_err());
/// ```
pub fn tokens<'a>(code: &'a str) -> Result<Vec<Token<'a>>, TokenizerError> {
    let mut tokens = Vec::new();
    let mut position = Position {
        bytes: 0,
        character: 1,
        line: 1,
    };

    let mut next_is_new_line = false;

    macro_rules! advance {
        ($function:ident) => {
            match $function(&code[position.bytes..]) {
                Ok(Some(advancement)) => {
                    let start_position = position;

                    for character in code[position.bytes..].chars().take(advancement.advance) {
                        if next_is_new_line {
                            next_is_new_line = false;
                            position.line += 1;
                            position.character = 1;
                        }

                        if character == '\n' {
                            next_is_new_line = true;
                        } else {
                            position.character += 1;
                        }

                        position.bytes += character.len_utf8();
                    }

                    tokens.push(Token {
                        start_position: Arc::new(AtomicPosition::new(start_position)),
                        end_position: Arc::new(AtomicPosition::new(position)),
                        token_type: Arc::new(AtomicRefCell::new(advancement.token_type)),
                    });

                    continue;
                }

                Ok(None) => {}

                Err(error) => {
                    return Err(TokenizerError { error, position });
                }
            };
        };
    }

    while code.bytes().count() > position.bytes {
        advance!(advance_whitespace);
        advance!(advance_comment);
        advance!(advance_number);
        advance!(advance_quote);
        advance!(advance_symbol);
        advance!(advance_identifier);

        return Err(TokenizerError {
            error: TokenizerErrorType::UnexpectedToken(
                code.chars()
                    .nth(position.character - 1)
                    .expect("text overflow while giving unexpected token error"),
            ),
            position,
        });
    }

    tokens.push(Token {
        start_position: Arc::new(AtomicPosition::new(position)),
        end_position: Arc::new(AtomicPosition::new(position)),
        token_type: Arc::new(AtomicRefCell::new(TokenType::Eof)),
    });

    Ok(tokens)
}

#[cfg(feature = "serde")]
mod serde_arc_atomic_refcell {
    use super::*;

    pub fn serialize<S: Serializer, T: Serialize>(
        this: &Arc<AtomicRefCell<T>>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        this.borrow().serialize(serializer)
    }

    pub fn deserialize<'de, D: Deserializer<'de>, T: Deserialize<'de>>(
        deserializer: D,
    ) -> Result<Arc<AtomicRefCell<T>>, D::Error> {
        Ok(Arc::new(AtomicRefCell::new(T::deserialize(deserializer)?)))
    }
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::*;
    use pretty_assertions::assert_eq;

    macro_rules! test_advancer {
        ($advancer:ident($code:tt), $result:expr) => {
            assert_eq!($advancer($code), $result);

            let result: Advancement = $result;
            match result {
                Ok(Some(token)) => {
                    let tokens = tokens($code).expect("couldn't tokenize");
                    let first_token = &tokens.get(0).expect("tokenized response is empty");
                    assert_eq!(*first_token.token_type(), token.token_type);
                }

                Err(advancement_error) => {
                    if let Err(TokenizerError { error, .. }) = tokens($code) {
                        assert_eq!(error, advancement_error);
                    }
                }

                _ => {}
            };
        };
    }

    #[test]
    fn test_advance_comment() {
        test_advancer!(
            advance_comment("-- hello world"),
            Ok(Some(TokenAdvancement {
                advance: 14,
                token_type: TokenType::SingleLineComment {
                    comment: Cow::from(" hello world"),
                },
            }))
        );

        test_advancer!(
            advance_comment("--[[ hello world ]]"),
            Ok(Some(TokenAdvancement {
                advance: 19,
                token_type: TokenType::MultiLineComment {
                    blocks: 0,
                    comment: Cow::from(" hello world "),
                },
            }))
        );

        test_advancer!(
            advance_comment("--[=[ hello world ]=]"),
            Ok(Some(TokenAdvancement {
                advance: 21,
                token_type: TokenType::MultiLineComment {
                    blocks: 1,
                    comment: Cow::from(" hello world "),
                },
            }))
        );
    }

    #[test]
    fn test_advance_numbers() {
        test_advancer!(
            advance_number("213"),
            Ok(Some(TokenAdvancement {
                advance: 3,
                token_type: TokenType::Number {
                    text: Cow::from("213"),
                },
            }))
        );

        test_advancer!(
            advance_number("123.45"),
            Ok(Some(TokenAdvancement {
                advance: 6,
                token_type: TokenType::Number {
                    text: Cow::from("123.45"),
                },
            }))
        );
    }

    #[test]
    #[cfg_attr(not(feature = "roblox"), ignore)]
    fn test_advance_binary_literals() {
        test_advancer!(
            advance_number("0b101"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Number {
                    text: Cow::from("0b101"),
                },
            }))
        );
    }

    #[test]
    fn test_advance_identifier() {
        test_advancer!(
            advance_identifier("hello"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Identifier {
                    identifier: Cow::from("hello"),
                },
            }))
        );

        test_advancer!(
            advance_identifier("hello world"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Identifier {
                    identifier: Cow::from("hello"),
                },
            }))
        );

        test_advancer!(
            advance_identifier("hello___"),
            Ok(Some(TokenAdvancement {
                advance: 8,
                token_type: TokenType::Identifier {
                    identifier: Cow::from("hello___"),
                },
            }))
        );

        test_advancer!(advance_identifier("123"), Ok(None));
    }

    #[test]
    fn test_advance_symbols() {
        test_advancer!(
            advance_symbol("local"),
            Ok(Some(TokenAdvancement {
                advance: 5,
                token_type: TokenType::Symbol {
                    symbol: Symbol::Local
                },
            }))
        );
    }

    #[test]
    fn test_advance_whitespace() {
        test_advancer!(
            advance_whitespace("\t  \n"),
            Ok(Some(TokenAdvancement {
                advance: 4,
                token_type: TokenType::Whitespace {
                    characters: Cow::from("\t  \n"),
                },
            }))
        );

        test_advancer!(
            advance_whitespace("\thello"),
            Ok(Some(TokenAdvancement {
                advance: 1,
                token_type: TokenType::Whitespace {
                    characters: Cow::from("\t"),
                },
            }))
        );

        test_advancer!(
            advance_whitespace("\t\t\nhello"),
            Ok(Some(TokenAdvancement {
                advance: 3,
                token_type: TokenType::Whitespace {
                    characters: Cow::from("\t\t\n"),
                },
            }))
        );
    }

    #[test]
    fn test_advance_quote() {
        test_advancer!(
            advance_quote("\"hello\""),
            Ok(Some(TokenAdvancement {
                advance: 7,
                token_type: TokenType::StringLiteral {
                    literal: Cow::from("hello"),
                    multi_line: None,
                    quote_type: StringLiteralQuoteType::Double,
                },
            }))
        );

        test_advancer!(
            advance_quote("\"hello"),
            Err(TokenizerErrorType::UnclosedString)
        );
    }

    #[test]
    fn test_symbols_within_symbols() {
        // "index" should not return "in"
        test_advancer!(advance_symbol("index"), Ok(None));

        // "<=" should not return "<"
        test_advancer!(
            advance_symbol("<="),
            Ok(Some(TokenAdvancement {
                advance: 2,
                token_type: TokenType::Symbol {
                    symbol: Symbol::LessThanEqual,
                },
            }))
        );
    }

    #[test]
    fn test_new_line_on_same_line() {
        assert_eq!(
            tokens("\n").unwrap()[0],
            Token {
                start_position: Arc::new(AtomicPosition::new(Position {
                    bytes: 0,
                    character: 1,
                    line: 1,
                })),

                end_position: Arc::new(AtomicPosition::new(Position {
                    bytes: 1,
                    character: 1,
                    line: 1,
                })),

                token_type: Arc::new(AtomicRefCell::new(TokenType::Whitespace {
                    characters: Cow::from("\n")
                })),
            }
        );
    }

    #[test]
    fn test_fuzzer() {
        let _ = tokens("*ա");
        let _ = tokens("̹(");
        let _ = tokens("¹;");
    }
}
