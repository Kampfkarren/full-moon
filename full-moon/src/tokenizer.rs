use crate::{
    atom::{trim_bracket_head, Atom},
    visitors::{Visit, VisitMut, Visitor, VisitorMut},
    ShortString,
};

use logos::{Lexer, Logos, Span};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    convert::{TryFrom, TryInto},
    fmt::{self, Display},
};

#[cfg(feature = "roblox")]
use crate::atom::{InterpolatedStringBegin, InterpolatedStringSection};

#[cfg(feature = "roblox")]
pub use crate::tokenizer_luau::InterpolatedStringKind;

#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(rename_all = "lowercase"))]
#[non_exhaustive]
#[allow(missing_docs)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// A literal symbol, used for both words important to syntax (like while) and operators (like +)
pub enum Symbol {
    And,
    Break,
    Do,
    Else,
    ElseIf,
    End,
    False,
    For,
    Function,
    If,
    In,
    Local,
    Nil,
    Not,
    Or,
    Repeat,
    Return,
    Then,
    True,
    Until,
    While,

    #[cfg(feature = "lua52")]
    Goto,

    #[cfg_attr(feature = "serde", serde(rename = "+="))]
    PlusEqual,

    #[cfg_attr(feature = "serde", serde(rename = "-="))]
    MinusEqual,

    #[cfg_attr(feature = "serde", serde(rename = "*="))]
    StarEqual,

    #[cfg_attr(feature = "serde", serde(rename = "/="))]
    SlashEqual,

    #[cfg_attr(feature = "serde", serde(rename = "%="))]
    PercentEqual,

    #[cfg_attr(feature = "serde", serde(rename = "^="))]
    CaretEqual,

    #[cfg_attr(feature = "serde", serde(rename = "..="))]
    TwoDotsEqual,

    #[cfg(any(feature = "roblox", feature = "lua53"))]
    #[cfg_attr(feature = "serde", serde(rename = "&"))]
    Ampersand,

    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(rename = "->"))]
    ThinArrow,

    #[cfg(any(feature = "roblox", feature = "lua52"))]
    #[cfg_attr(feature = "serde", serde(rename = "::"))]
    TwoColons,

    #[cfg_attr(feature = "serde", serde(rename = "^"))]
    Caret,

    #[cfg_attr(feature = "serde", serde(rename = ":"))]
    Colon,

    #[cfg_attr(feature = "serde", serde(rename = ","))]
    Comma,

    #[cfg_attr(feature = "serde", serde(rename = "..."))]
    Ellipse,

    #[cfg_attr(feature = "serde", serde(rename = ".."))]
    TwoDots,

    #[cfg_attr(feature = "serde", serde(rename = "."))]
    Dot,

    #[cfg_attr(feature = "serde", serde(rename = "=="))]
    TwoEqual,

    #[cfg_attr(feature = "serde", serde(rename = "="))]
    Equal,

    #[cfg_attr(feature = "serde", serde(rename = ">="))]
    GreaterThanEqual,

    #[cfg_attr(feature = "serde", serde(rename = ">"))]
    GreaterThan,

    #[cfg(feature = "lua53")]
    #[cfg_attr(feature = "serde", serde(rename = ">>"))]
    DoubleGreaterThan,

    #[cfg_attr(feature = "serde", serde(rename = "#"))]
    Hash,

    #[cfg_attr(feature = "serde", serde(rename = "["))]
    LeftBracket,

    #[cfg_attr(feature = "serde", serde(rename = "{"))]
    LeftBrace,

    #[cfg_attr(feature = "serde", serde(rename = "("))]
    LeftParen,

    #[cfg_attr(feature = "serde", serde(rename = "<="))]
    LessThanEqual,

    #[cfg_attr(feature = "serde", serde(rename = "<"))]
    LessThan,

    #[cfg(feature = "lua53")]
    #[cfg_attr(feature = "serde", serde(rename = "<<"))]
    DoubleLessThan,

    #[cfg_attr(feature = "serde", serde(rename = "-"))]
    Minus,

    #[cfg_attr(feature = "serde", serde(rename = "%"))]
    Percent,

    #[cfg(any(feature = "roblox", feature = "lua53"))]
    #[cfg_attr(feature = "serde", serde(rename = "|"))]
    Pipe,

    #[cfg_attr(feature = "serde", serde(rename = "+"))]
    Plus,

    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(rename = "?"))]
    QuestionMark,

    #[cfg_attr(feature = "serde", serde(rename = "}"))]
    RightBrace,

    #[cfg_attr(feature = "serde", serde(rename = "]"))]
    RightBracket,

    #[cfg_attr(feature = "serde", serde(rename = ")"))]
    RightParen,

    #[cfg_attr(feature = "serde", serde(rename = ";"))]
    Semicolon,

    #[cfg_attr(feature = "serde", serde(rename = "/"))]
    Slash,

    #[cfg(feature = "lua53")]
    #[cfg_attr(feature = "serde", serde(rename = "//"))]
    DoubleSlash,

    #[cfg_attr(feature = "serde", serde(rename = "*"))]
    Star,

    #[cfg(feature = "lua53")]
    #[cfg_attr(feature = "serde", serde(rename = "~"))]
    Tilde,

    #[cfg_attr(feature = "serde", serde(rename = "~="))]
    TildeEqual,
}

impl TryFrom<Atom> for Symbol {
    type Error = ();

    fn try_from(value: Atom) -> Result<Self, Self::Error> {
        Ok(match value {
            Atom::And => Symbol::And,
            Atom::Break => Symbol::Break,
            Atom::Do => Symbol::Do,
            Atom::Else => Symbol::Else,
            Atom::ElseIf => Symbol::ElseIf,
            Atom::End => Symbol::End,
            Atom::False => Symbol::False,
            Atom::For => Symbol::For,
            Atom::Function => Symbol::Function,
            Atom::If => Symbol::If,
            Atom::In => Symbol::In,
            Atom::Local => Symbol::Local,
            Atom::Nil => Symbol::Nil,
            Atom::Not => Symbol::Not,
            Atom::Or => Symbol::Or,
            Atom::Repeat => Symbol::Repeat,
            Atom::Return => Symbol::Return,
            Atom::Then => Symbol::Then,
            Atom::True => Symbol::True,
            Atom::Until => Symbol::Until,
            Atom::While => Symbol::While,
            #[cfg(feature = "lua52")]
            Atom::Goto => Symbol::Goto,
            #[cfg(feature = "roblox")]
            Atom::PlusEqual => Symbol::PlusEqual,
            #[cfg(feature = "roblox")]
            Atom::MinusEqual => Symbol::MinusEqual,
            #[cfg(feature = "roblox")]
            Atom::StarEqual => Symbol::StarEqual,
            #[cfg(feature = "roblox")]
            Atom::SlashEqual => Symbol::SlashEqual,
            #[cfg(feature = "roblox")]
            Atom::PercentEqual => Symbol::PercentEqual,
            #[cfg(feature = "roblox")]
            Atom::CaretEqual => Symbol::CaretEqual,
            #[cfg(feature = "roblox")]
            Atom::TwoDotsEqual => Symbol::TwoDotsEqual,
            Atom::Caret => Symbol::Caret,
            Atom::Colon => Symbol::Colon,
            Atom::Comma => Symbol::Comma,
            Atom::Ellipse => Symbol::Ellipse,
            Atom::TwoDots => Symbol::TwoDots,
            #[cfg(any(feature = "roblox", feature = "lua53"))]
            Atom::Ampersand => Symbol::Ampersand,
            #[cfg(feature = "roblox")]
            Atom::ThinArrow => Symbol::ThinArrow,
            #[cfg(any(feature = "roblox", feature = "lua52"))]
            Atom::TwoColons => Symbol::TwoColons,
            Atom::Dot => Symbol::Dot,
            Atom::TwoEqual => Symbol::TwoEqual,
            Atom::Equal => Symbol::Equal,
            Atom::GreaterThanEqual => Symbol::GreaterThanEqual,
            Atom::GreaterThan => Symbol::GreaterThan,
            Atom::Hash => Symbol::Hash,
            Atom::LeftBracket => Symbol::LeftBracket,
            Atom::LeftBrace => Symbol::LeftBrace,
            Atom::LeftParen => Symbol::LeftParen,
            Atom::LessThanEqual => Symbol::LessThanEqual,
            Atom::LessThan => Symbol::LessThan,
            #[cfg(feature = "lua53")]
            Atom::DoubleLessThan => Symbol::DoubleLessThan,
            Atom::Minus => Symbol::Minus,
            Atom::Percent => Symbol::Percent,
            #[cfg(any(feature = "roblox", feature = "lua53"))]
            Atom::Pipe => Symbol::Pipe,
            #[cfg(feature = "roblox")]
            Atom::QuestionMark => Symbol::QuestionMark,
            Atom::Plus => Symbol::Plus,
            Atom::RightBrace(None) => Symbol::RightBrace,
            Atom::RightBracket => Symbol::RightBracket,
            Atom::RightParen => Symbol::RightParen,
            Atom::Semicolon => Symbol::Semicolon,
            Atom::Slash => Symbol::Slash,
            #[cfg(feature = "lua53")]
            Atom::DoubleSlash => Symbol::DoubleSlash,
            Atom::Star => Symbol::Star,
            #[cfg(feature = "lua53")]
            Atom::Tilde => Symbol::Tilde,
            Atom::TildeEqual => Symbol::TildeEqual,
            _ => {
                return Err(());
            }
        })
    }
}

impl Display for Symbol {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        let value = match self {
            Symbol::And => "and",
            Symbol::Break => "break",
            Symbol::Do => "do",
            Symbol::Else => "else",
            Symbol::ElseIf => "elseif",
            Symbol::End => "end",
            Symbol::False => "false",
            Symbol::For => "for",
            Symbol::Function => "function",
            Symbol::If => "if",
            Symbol::In => "in",
            Symbol::Local => "local",
            Symbol::Nil => "nil",
            Symbol::Not => "not",
            Symbol::Or => "or",
            Symbol::Repeat => "repeat",
            Symbol::Return => "return",
            Symbol::Then => "then",
            Symbol::True => "true",
            Symbol::Until => "until",
            Symbol::While => "while",
            #[cfg(feature = "lua52")]
            Symbol::Goto => "goto",
            Symbol::PlusEqual => "+=",
            Symbol::MinusEqual => "-=",
            Symbol::StarEqual => "*=",
            Symbol::SlashEqual => "/=",
            Symbol::PercentEqual => "%=",
            Symbol::CaretEqual => "^=",
            Symbol::TwoDotsEqual => "..=",
            #[cfg(any(feature = "roblox", feature = "lua53"))]
            Symbol::Ampersand => "&",
            #[cfg(feature = "roblox")]
            Symbol::ThinArrow => "->",
            #[cfg(any(feature = "roblox", feature = "lua52"))]
            Symbol::TwoColons => "::",
            Symbol::Caret => "^",
            Symbol::Colon => ":",
            Symbol::Comma => ",",
            Symbol::Ellipse => "...",
            Symbol::TwoDots => "..",
            Symbol::Dot => ".",
            Symbol::TwoEqual => "==",
            Symbol::Equal => "=",
            Symbol::GreaterThanEqual => ">=",
            Symbol::GreaterThan => ">",
            #[cfg(feature = "lua53")]
            Symbol::DoubleGreaterThan => ">>",
            Symbol::Hash => "#",
            Symbol::LeftBracket => "[",
            Symbol::LeftBrace => "{",
            Symbol::LeftParen => "(",
            Symbol::LessThanEqual => "<=",
            Symbol::LessThan => "<",
            #[cfg(feature = "lua53")]
            Symbol::DoubleLessThan => "<<",
            Symbol::Minus => "-",
            Symbol::Percent => "%",
            #[cfg(any(feature = "roblox", feature = "lua53"))]
            Symbol::Pipe => "|",
            Symbol::Plus => "+",
            #[cfg(feature = "roblox")]
            Symbol::QuestionMark => "?",
            Symbol::RightBrace => "}",
            Symbol::RightBracket => "]",
            Symbol::RightParen => ")",
            Symbol::Semicolon => ";",
            Symbol::Slash => "/",
            #[cfg(feature = "lua53")]
            Symbol::DoubleSlash => "//",
            Symbol::Star => "*",
            #[cfg(feature = "lua53")]
            Symbol::Tilde => "~",
            Symbol::TildeEqual => "~=",
        };

        value.fmt(formatter)
    }
}

/// The possible errors that can happen while tokenizing.
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum TokenizerErrorType {
    /// An unclosed multi-line comment was found
    UnclosedComment,
    /// An unclosed string was found
    UnclosedString,
    /// An unexpected #! was found
    UnexpectedShebang,
    /// An unexpected token was found
    UnexpectedToken(char),
    /// Symbol passed is not valid
    /// Returned from [`TokenReference::symbol`]
    InvalidSymbol(String),
}

impl fmt::Display for TokenizerErrorType {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenizerErrorType::UnclosedComment => "unclosed comment".fmt(formatter),
            TokenizerErrorType::UnclosedString => "unclosed string".fmt(formatter),
            TokenizerErrorType::UnexpectedShebang => "unexpected shebang".fmt(formatter),
            TokenizerErrorType::UnexpectedToken(character) => {
                write!(formatter, "unexpected character {character}")
            }
            TokenizerErrorType::InvalidSymbol(symbol) => {
                write!(formatter, "invalid symbol {symbol}")
            }
        }
    }
}

/// The type of tokens in parsed code
#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[non_exhaustive]
pub enum TokenType {
    /// End of file, should always be the very last token
    Eof,

    /// An identifier, such as `foo`
    Identifier {
        /// The identifier itself
        identifier: ShortString,
    },

    /// A multi line comment in the format of `--[[ comment ]]`
    MultiLineComment {
        /// Number of equals signs, if any, for the multi line comment
        /// For example, `--[=[` would have a `blocks` value of `1`
        blocks: usize,
        /// The comment itself, ignoring opening and closing tags
        comment: ShortString,
    },

    /// A literal number, such as `3.3`
    Number {
        /// The text representing the number, includes details such as `0x`
        text: ShortString,
    },

    /// A shebang line
    Shebang {
        /// The shebang line itself
        line: ShortString,
    },

    /// A single line comment, such as `-- comment`
    SingleLineComment {
        /// The comment, ignoring initial `--`
        comment: ShortString,
    },

    /// A literal string, such as "Hello, world"
    StringLiteral {
        /// The literal itself, ignoring quotation marks
        literal: ShortString,
        #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
        /// Number of equals signs used for a multi line string, if it is one
        /// For example, `[=[string]=]` would have a `multi_line` value of Some(1)
        /// `[[string]]` would have a `multi_line` value of Some(0)
        /// A string such as `"string"` would have a `multi_line` value of None
        multi_line: Option<usize>,
        /// The type of quotation mark used to make the string
        quote_type: StringLiteralQuoteType,
    },

    /// A [`Symbol`], such as `local` or `+`
    Symbol {
        /// The symbol itself
        symbol: Symbol,
    },

    /// Whitespace, such as tabs or new lines
    Whitespace {
        /// Characters consisting of the whitespace
        characters: ShortString,
    },

    /// Some form of interpolated string
    #[cfg(feature = "roblox")]
    InterpolatedString {
        /// The literal itself, ignoring backticks
        literal: ShortString,

        /// The kind of interpolated string.
        /// If it is the beginning, middle, end, or a standalone string.
        kind: InterpolatedStringKind,
    },
}

impl TokenType {
    fn new_string(literal: &str, quote_type: StringLiteralQuoteType) -> Self {
        let literal = literal.into();

        Self::StringLiteral {
            literal,
            quote_type,
            multi_line: None,
        }
    }

    /// Can this token type contain new lines?
    fn is_extensive(&self) -> bool {
        #[cfg(feature = "roblox")]
        let is_interpolated_string = matches!(self, TokenType::InterpolatedString { .. });

        #[cfg(not(feature = "roblox"))]
        let is_interpolated_string = false;

        matches!(
            self,
            TokenType::MultiLineComment { .. }
                | TokenType::Shebang { .. }
                | TokenType::StringLiteral { .. }
                | TokenType::Whitespace { .. }
        ) || is_interpolated_string
    }

    /// Returns whether a token can be practically ignored in most cases
    /// Comments and whitespace will return `true`, everything else will return `false`
    pub fn is_trivia(&self) -> bool {
        matches!(
            self,
            TokenType::Shebang { .. }
                | TokenType::SingleLineComment { .. }
                | TokenType::MultiLineComment { .. }
                | TokenType::Whitespace { .. }
        )
    }

    /// Returns the kind of the token type.
    ///
    /// ```rust
    /// use full_moon::{ShortString, tokenizer::{TokenKind, TokenType}};
    ///
    /// assert_eq!(
    ///     TokenType::Identifier {
    ///         identifier: ShortString::new("hello")
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
            TokenType::Shebang { .. } => TokenKind::Shebang,
            TokenType::SingleLineComment { .. } => TokenKind::SingleLineComment,
            TokenType::StringLiteral { .. } => TokenKind::StringLiteral,
            TokenType::Symbol { .. } => TokenKind::Symbol,
            TokenType::Whitespace { .. } => TokenKind::Whitespace,

            #[cfg(feature = "roblox")]
            TokenType::InterpolatedString { .. } => TokenKind::InterpolatedString,
        }
    }

    /// Returns a whitespace `TokenType` consisting of spaces
    pub fn spaces(spaces: usize) -> Self {
        TokenType::Whitespace {
            characters: " ".repeat(spaces).into(),
        }
    }

    /// Returns a whitespace `TokenType` consisting of tabs
    pub fn tabs(tabs: usize) -> Self {
        TokenType::Whitespace {
            characters: "\t".repeat(tabs).into(),
        }
    }
}

/// The kind of token. Contains no additional data.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum TokenKind {
    /// End of file, should always be the very last token
    Eof,
    /// An identifier, such as `foo`
    Identifier,
    /// A multi line comment in the format of `--[[ comment ]]`
    MultiLineComment,
    /// A literal number, such as `3.3`
    Number,
    /// The shebang line
    Shebang,
    /// A single line comment, such as `-- comment`
    SingleLineComment,
    /// A literal string, such as "Hello, world"
    StringLiteral,
    /// A [`Symbol`], such as `local` or `+`
    Symbol,
    /// Whitespace, such as tabs or new lines
    Whitespace,

    #[cfg(feature = "roblox")]
    /// Some form of interpolated string
    InterpolatedString,
}

/// A token such consisting of its [`Position`] and a [`TokenType`]
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Token {
    pub(crate) start_position: Position,
    pub(crate) end_position: Position,
    pub(crate) token_type: TokenType,
}

impl Token {
    /// Creates a token with a zero position
    pub fn new(token_type: TokenType) -> Token {
        Token {
            start_position: Position::default(),
            end_position: Position::default(),
            token_type,
        }
    }

    /// The position a token begins at
    pub fn start_position(&self) -> Position {
        self.start_position
    }

    /// The position a token ends at
    pub fn end_position(&self) -> Position {
        self.end_position
    }

    /// The type of token as well as the data needed to represent it
    /// If you don't need any other information, use [`token_kind`](Token::token_kind) instead.
    pub fn token_type(&self) -> &TokenType {
        &self.token_type
    }

    /// The kind of token with no additional data.
    /// If you need any information such as idenitfier names, use [`token_type`](Token::token_type) instead.
    pub fn token_kind(&self) -> TokenKind {
        self.token_type().kind()
    }
}

impl fmt::Display for Token {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenType::*;

        match self.token_type() {
            Eof => Ok(()),
            Number { text } => text.fmt(formatter),
            Identifier { identifier } => identifier.fmt(formatter),
            MultiLineComment { blocks, comment } => {
                write!(formatter, "--[{0}[{1}]{0}]", "=".repeat(*blocks), comment)
            }
            Shebang { line } => line.fmt(formatter),
            SingleLineComment { comment } => write!(formatter, "--{comment}"),
            StringLiteral {
                literal,
                multi_line,
                quote_type,
            } => {
                if let Some(blocks) = multi_line {
                    write!(formatter, "[{0}[{1}]{0}]", "=".repeat(*blocks), literal)
                } else {
                    write!(formatter, "{quote_type}{literal}{quote_type}")
                }
            }
            Symbol { symbol } => symbol.fmt(formatter),
            Whitespace { characters } => characters.fmt(formatter),

            #[cfg(feature = "roblox")]
            InterpolatedString { literal, kind } => match kind {
                InterpolatedStringKind::Begin => {
                    write!(formatter, "`{literal}{{")
                }

                InterpolatedStringKind::Middle => {
                    write!(formatter, "}}{literal}{{")
                }

                InterpolatedStringKind::End => {
                    write!(formatter, "}}{literal}`")
                }

                InterpolatedStringKind::Simple => {
                    write!(formatter, "`{literal}`")
                }
            },
        }
    }
}

impl PartialEq<Self> for Token {
    fn eq(&self, rhs: &Self) -> bool {
        self.start_position() == rhs.start_position()
            && self.end_position() == rhs.end_position()
            && self.token_type == rhs.token_type
    }
}

impl Eq for Token {}

impl Ord for Token {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start_position().cmp(&other.start_position())
    }
}

impl PartialOrd for Token {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Visit for Token {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_token(self);

        match self.token_kind() {
            TokenKind::Eof => {}
            TokenKind::Identifier => visitor.visit_identifier(self),
            TokenKind::MultiLineComment => visitor.visit_multi_line_comment(self),
            TokenKind::Number => visitor.visit_number(self),
            TokenKind::Shebang => {}
            TokenKind::SingleLineComment => visitor.visit_single_line_comment(self),
            TokenKind::StringLiteral => visitor.visit_string_literal(self),
            TokenKind::Symbol => visitor.visit_symbol(self),
            TokenKind::Whitespace => visitor.visit_whitespace(self),

            #[cfg(feature = "roblox")]
            TokenKind::InterpolatedString => visitor.visit_interpolated_string_segment(self),
        }
    }
}

impl VisitMut for Token {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        let token = visitor.visit_token(self);

        match token.token_kind() {
            TokenKind::Eof => token,
            TokenKind::Identifier => visitor.visit_identifier(token),
            TokenKind::MultiLineComment => visitor.visit_multi_line_comment(token),
            TokenKind::Number => visitor.visit_number(token),
            TokenKind::Shebang => token,
            TokenKind::SingleLineComment => visitor.visit_single_line_comment(token),
            TokenKind::StringLiteral => visitor.visit_string_literal(token),
            TokenKind::Symbol => visitor.visit_symbol(token),
            TokenKind::Whitespace => visitor.visit_whitespace(token),

            #[cfg(feature = "roblox")]
            TokenKind::InterpolatedString => visitor.visit_interpolated_string_segment(token),
        }
    }
}

/// A reference to a token used by Ast's.
/// Dereferences to a [`Token`]
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TokenReference {
    pub(crate) leading_trivia: Vec<Token>,
    pub(crate) token: Token,
    pub(crate) trailing_trivia: Vec<Token>,
}

impl TokenReference {
    /// Creates a TokenReference from leading/trailing trivia as well as the leading token
    pub fn new(leading_trivia: Vec<Token>, token: Token, trailing_trivia: Vec<Token>) -> Self {
        Self {
            leading_trivia,
            token,
            trailing_trivia,
        }
    }

    /// Returns a symbol with the leading and trailing whitespace
    /// Only whitespace is supported
    /// ```rust
    /// # use full_moon::tokenizer::{Symbol, TokenReference, TokenType, TokenizerErrorType};
    /// # fn main() -> Result<(), Box<TokenizerErrorType>> {
    /// let symbol = TokenReference::symbol("\nreturn ")?;
    /// assert_eq!(symbol.leading_trivia().next().unwrap().to_string(), "\n");
    /// assert_eq!(symbol.token().token_type(), &TokenType::Symbol {
    ///     symbol: Symbol::Return,
    /// });
    /// assert_eq!(symbol.trailing_trivia().next().unwrap().to_string(), " ");
    /// assert!(TokenReference::symbol("isnt whitespace").is_err());
    /// assert!(TokenReference::symbol(" notasymbol ").is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn symbol(text: &str) -> Result<Self, TokenizerErrorType> {
        let mut lexer = Atom::lexer(text).spanned().peekable();

        let leading_trivia = lexer
            .next_if(|v| v.0 == Atom::Whitespace)
            .map(|v| text[v.1].into())
            .unwrap_or_default();

        let (atom, span) = lexer.next().unwrap();
        let symbol = atom.try_into().map_err(|_| {
            let text = text[span].to_string();

            TokenizerErrorType::InvalidSymbol(text)
        })?;

        let trailing_trivia = lexer
            .next_if(|v| v.0 == Atom::Whitespace)
            .map(|v| text[v.1].into())
            .unwrap_or_default();

        if let Some(v) = lexer.next() {
            let ch = text[v.1].chars().next().unwrap();

            return Err(TokenizerErrorType::UnexpectedToken(ch));
        }

        Ok(Self {
            leading_trivia: vec![Token::new(TokenType::Whitespace {
                characters: leading_trivia,
            })],
            token: Token::new(TokenType::Symbol { symbol }),
            trailing_trivia: vec![Token::new(TokenType::Whitespace {
                characters: trailing_trivia,
            })],
        })
    }

    /// Returns the inner token.
    pub fn token(&self) -> &Token {
        &self.token
    }

    /// Returns the leading trivia
    pub fn leading_trivia(&self) -> impl Iterator<Item = &Token> {
        self.leading_trivia.iter()
    }

    /// Returns the trailing trivia
    pub fn trailing_trivia(&self) -> impl Iterator<Item = &Token> {
        self.trailing_trivia.iter()
    }

    /// Creates a clone of the current TokenReference with the new inner token, preserving trivia.
    pub fn with_token(&self, token: Token) -> Self {
        Self {
            token,
            leading_trivia: self.leading_trivia.clone(),
            trailing_trivia: self.trailing_trivia.clone(),
        }
    }
}

impl std::borrow::Borrow<Token> for &TokenReference {
    fn borrow(&self) -> &Token {
        self
    }
}

impl std::ops::Deref for TokenReference {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl fmt::Display for TokenReference {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        for trivia in &self.leading_trivia {
            trivia.fmt(formatter)?;
        }

        self.token.fmt(formatter)?;

        for trivia in &self.trailing_trivia {
            trivia.fmt(formatter)?;
        }

        Ok(())
    }
}

impl PartialEq<Self> for TokenReference {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(other)
            && self.leading_trivia == other.leading_trivia
            && self.trailing_trivia == other.trailing_trivia
    }
}

impl Eq for TokenReference {}

impl Ord for TokenReference {
    fn cmp(&self, other: &Self) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl PartialOrd for TokenReference {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Visit for TokenReference {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        visitor.visit_token(self);

        if matches!(self.token().token_kind(), TokenKind::Eof) {
            visitor.visit_eof(self);
        }

        self.leading_trivia.visit(visitor);
        self.token.visit(visitor);
        self.trailing_trivia.visit(visitor);
    }
}

impl VisitMut for TokenReference {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        let mut token_reference = visitor.visit_token_reference(self);

        if matches!(token_reference.token().token_kind(), TokenKind::Eof) {
            token_reference = visitor.visit_eof(token_reference);
        }

        token_reference.leading_trivia = token_reference.leading_trivia.visit_mut(visitor);
        token_reference.token = token_reference.token.visit_mut(visitor);
        token_reference.trailing_trivia = token_reference.trailing_trivia.visit_mut(visitor);
        token_reference
    }
}

/// Used to represent exact positions of tokens in code
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Position {
    pub(crate) bytes: usize,
    pub(crate) line: usize,
    pub(crate) character: usize,
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

/// The types of quotes used in a Lua string
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum StringLiteralQuoteType {
    /// Strings formatted \[\[with brackets\]\]
    Brackets,
    /// Strings formatted "with double quotes"
    Double,
    /// Strings formatted 'with single quotes'
    Single,
}

impl fmt::Display for StringLiteralQuoteType {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            StringLiteralQuoteType::Brackets => unreachable!(),
            StringLiteralQuoteType::Double => "\"".fmt(formatter),
            StringLiteralQuoteType::Single => "'".fmt(formatter),
        }
    }
}

type RawToken = Result<TokenType, TokenizerErrorType>;

impl From<TokenType> for RawToken {
    fn from(token_type: TokenType) -> RawToken {
        Ok(token_type)
    }
}

impl From<TokenizerErrorType> for RawToken {
    fn from(error: TokenizerErrorType) -> RawToken {
        Err(error)
    }
}

fn tokenize(token: Atom, slice: &str) -> RawToken {
    match token {
        Atom::Identifier => {
            let identifier = slice.into();

            Ok(TokenType::Identifier { identifier })
        }

        Atom::Comment => {
            let (comment, blocks) = trim_bracket_head(&slice[2..]);

            match blocks {
                Some(blocks) => Ok(TokenType::MultiLineComment { comment, blocks }),
                None => Ok(TokenType::SingleLineComment { comment }),
            }
        }

        Atom::Number => {
            let text = slice.into();

            Ok(TokenType::Number { text })
        }

        Atom::MultiLineString => {
            let (literal, multi_line) = trim_bracket_head(slice);

            Ok(TokenType::StringLiteral {
                literal,
                multi_line,
                quote_type: StringLiteralQuoteType::Brackets,
            })
        }

        Atom::ApostropheString => Ok(TokenType::new_string(
            &slice[1..slice.len() - 1],
            StringLiteralQuoteType::Single,
        )),

        Atom::QuoteString => Ok(TokenType::new_string(
            &slice[1..slice.len() - 1],
            StringLiteralQuoteType::Double,
        )),

        Atom::Whitespace => {
            let characters = slice.into();

            Ok(TokenType::Whitespace { characters })
        }

        Atom::Bom => Err(TokenizerErrorType::UnexpectedToken('\u{feff}')),

        Atom::Shebang => Err(TokenizerErrorType::UnexpectedShebang),

        #[cfg(feature = "roblox")]
        Atom::InterpolatedStringBegin(InterpolatedStringBegin::Simple)
        | Atom::InterpolatedStringBegin(InterpolatedStringBegin::Formatted)
        | Atom::RightBrace(Some(InterpolatedStringSection::Middle))
        | Atom::RightBrace(Some(InterpolatedStringSection::End)) => {
            Ok(TokenType::InterpolatedString {
                literal: ShortString::new(&slice[1..slice.len() - 1]),
                kind: match token {
                    Atom::InterpolatedStringBegin(InterpolatedStringBegin::Simple) => {
                        InterpolatedStringKind::Simple
                    }

                    Atom::InterpolatedStringBegin(InterpolatedStringBegin::Formatted) => {
                        InterpolatedStringKind::Begin
                    }

                    Atom::RightBrace(Some(InterpolatedStringSection::Middle)) => {
                        InterpolatedStringKind::Middle
                    }

                    Atom::RightBrace(Some(InterpolatedStringSection::End)) => {
                        InterpolatedStringKind::End
                    }

                    _ => unreachable!(),
                },
            })
        }

        Atom::Unknown => {
            let first = slice.chars().next().unwrap();
            let what = match first {
                '`' if cfg!(feature = "roblox") => TokenizerErrorType::UnclosedString,
                '\'' | '"' | '[' => TokenizerErrorType::UnclosedString,
                '-' => TokenizerErrorType::UnclosedComment,
                other => TokenizerErrorType::UnexpectedToken(other),
            };

            Err(what)
        }

        token => Ok(TokenType::Symbol {
            symbol: token.try_into().unwrap(),
        }),
    }
}

fn next_if(lexer: &mut Lexer<Atom>, atom: Atom) -> Option<ShortString> {
    if lexer.clone().next() == Some(atom) {
        lexer.next();

        Some(lexer.slice().into())
    } else {
        None
    }
}

fn tokenize_code(code: &str) -> Vec<(RawToken, Span)> {
    let mut lexer = Atom::lexer(code);
    let mut list = Vec::new();

    if let Some(characters) = next_if(&mut lexer, Atom::Bom) {
        let raw = (Ok(TokenType::Whitespace { characters }), lexer.span());

        list.push(raw);
    }

    if let Some(line) = next_if(&mut lexer, Atom::Shebang) {
        let raw = (Ok(TokenType::Shebang { line }), lexer.span());

        list.push(raw);
    }

    while let Some(atom) = lexer.next() {
        let raw = (tokenize(atom, lexer.slice()), lexer.span());

        list.push(raw);
    }

    list
}

/// Information about an error that occurs while tokenizing
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TokenizerError {
    /// The type of error
    error: TokenizerErrorType,
    /// The position of the token that caused the error
    position: Position,
}

impl TokenizerError {
    /// The type of error
    pub fn error(&self) -> &TokenizerErrorType {
        &self.error
    }

    /// The position of the token that caused the error
    pub fn position(&self) -> Position {
        self.position
    }
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{} at line {}, column {}",
            self.error, self.position.line, self.position.character,
        )
    }
}

impl std::error::Error for TokenizerError {}

struct TokenCollector {
    result: Vec<Token>,
}

// Collector
impl TokenCollector {
    fn new() -> Self {
        Self { result: Vec::new() }
    }
    fn push(
        &mut self,
        start_position: Position,
        raw_token: RawToken,
        end_position: Position,
    ) -> Result<(), TokenizerError> {
        match raw_token {
            Ok(token_type) => {
                self.result.push(Token {
                    start_position,
                    end_position,
                    token_type,
                });
                Ok(())
            }
            Err(error) => Err(TokenizerError {
                error,
                position: start_position,
            }),
        }
    }
    fn finish(mut self, eof_position: Position) -> Vec<Token> {
        self.result.push(Token {
            start_position: eof_position,
            end_position: eof_position,
            token_type: TokenType::Eof,
        });
        self.result
    }
}

fn read_position(code: &str, position: &mut Position) -> bool {
    let mut has_newline = false;

    for c in code.chars() {
        if c == '\n' {
            has_newline = true;
        } else {
            if has_newline {
                position.line += 1;
                position.character = 1;

                has_newline = false;
            }

            position.character += 1;
        }
    }

    has_newline
}

/// Returns a list of tokens.
/// You probably want [`parse`](crate::parse) instead.
///
/// # Errors
///
/// If the code passed is malformed from normal Lua expectations,
/// a [`TokenizerError`] will be returned.
///
/// ```rust
/// # use full_moon::tokenizer::tokens;
/// assert!(tokens("local x = 1").is_ok());
/// assert!(tokens("local 4 = end").is_ok()); // tokens does *not* check validity of code, only tokenizing
/// assert!(tokens("--[[ Unclosed comment!").is_err());
/// ```
pub fn tokens(code: &str) -> Result<Vec<Token>, TokenizerError> {
    let mut tokens = TokenCollector::new();

    // Logos provides token start and end information,
    // but not the line or column. We iterate over the
    // characters to retrieve this.
    let mut position = Position {
        bytes: 0,
        line: 1,
        character: 1,
    };

    for (token, span) in tokenize_code(code) {
        let start_position = position;

        position.bytes = span.end;

        if token
            .as_ref()
            .map(TokenType::is_extensive)
            .unwrap_or_default()
        {
            let has_newline = read_position(&code[span.clone()], &mut position);

            tokens.push(start_position, token, position)?;

            if has_newline {
                position.line += 1;
                position.character = 1;
            }
        } else {
            position.character += span.len();

            tokens.push(start_position, token, position)?;
        }
    }

    Ok(tokens.finish(position))
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::*;
    use pretty_assertions::assert_eq;

    macro_rules! test_rule {
        ($code:expr, $result:expr) => {
            let code: &str = $code;
            let result: RawToken = $result.into();

            match result {
                Ok(token) => {
                    let tokens = tokens(code).expect("couldn't tokenize");
                    let first_token = &tokens.get(0).expect("tokenized response is empty");
                    assert_eq!(*first_token.token_type(), token);
                }

                Err(expected) => {
                    if let Err(TokenizerError { error, .. }) = tokens($code) {
                        assert_eq!(error, expected);
                    } else {
                        panic!("tokenization should fail");
                    }
                }
            };
        };
    }

    #[test]
    fn test_rule_comment() {
        test_rule!(
            "-- hello world",
            TokenType::SingleLineComment {
                comment: " hello world".into()
            }
        );

        test_rule!(
            "--[[ hello world ]]",
            TokenType::MultiLineComment {
                blocks: 0,
                comment: " hello world ".into()
            }
        );

        test_rule!(
            "--[=[ hello world ]=]",
            TokenType::MultiLineComment {
                blocks: 1,
                comment: " hello world ".into()
            }
        );
        test_rule!("--", TokenType::SingleLineComment { comment: "".into() });
    }

    #[test]
    fn test_rule_numbers() {
        test_rule!("213", TokenType::Number { text: "213".into() });

        test_rule!("1", TokenType::Number { text: "1".into() });

        test_rule!(
            "123.45",
            TokenType::Number {
                text: "123.45".into(),
            }
        );
    }

    #[test]
    #[cfg_attr(not(feature = "roblox"), ignore)]
    fn test_rule_binary_literals() {
        test_rule!(
            "0b101",
            TokenType::Number {
                text: "0b101".into(),
            }
        );
    }

    #[test]
    fn test_rule_identifier() {
        test_rule!(
            "hello",
            TokenType::Identifier {
                identifier: "hello".into(),
            }
        );

        test_rule!(
            "hello world",
            TokenType::Identifier {
                identifier: "hello".into(),
            }
        );

        test_rule!(
            "hello___",
            TokenType::Identifier {
                identifier: "hello___".into(),
            }
        );
    }

    #[test]
    fn test_rule_symbols() {
        test_rule!(
            "local",
            TokenType::Symbol {
                symbol: Symbol::Local
            }
        );
    }

    #[test]
    fn test_rule_whitespace() {
        test_rule!(
            "\t  \n\t",
            TokenType::Whitespace {
                characters: "\t  \n".into(),
            }
        );

        test_rule!(
            "\thello",
            TokenType::Whitespace {
                characters: "\t".into(),
            }
        );

        test_rule!(
            "\t\t\nhello",
            TokenType::Whitespace {
                characters: "\t\t\n".into(),
            }
        );

        test_rule!(
            "\n\thello",
            TokenType::Whitespace {
                characters: "\n".into(),
            }
        );
    }

    #[test]
    fn test_rule_string_literal() {
        test_rule!(
            "\"hello\"",
            TokenType::StringLiteral {
                literal: "hello".into(),
                multi_line: None,
                quote_type: StringLiteralQuoteType::Double,
            }
        );

        test_rule!(
            "\"hello\\\nworld\"",
            TokenType::StringLiteral {
                literal: "hello\\\nworld".into(),
                multi_line: None,
                quote_type: StringLiteralQuoteType::Double,
            }
        );

        test_rule!(
            "'hello world \\'goodbye\\''",
            TokenType::StringLiteral {
                literal: "hello world \\'goodbye\\'".into(),
                multi_line: None,
                quote_type: StringLiteralQuoteType::Single,
            }
        );

        test_rule!("\"hello", TokenizerErrorType::UnclosedString);
    }

    #[test]
    #[cfg(feature = "lua52")]
    fn test_string_z_escape() {
        test_rule!(
            "'hello \\z\nworld'",
            TokenType::StringLiteral {
                literal: "hello \\z\nworld".into(),
                multi_line: None,
                quote_type: StringLiteralQuoteType::Single,
            }
        );
    }

    #[test]
    fn test_symbols_within_symbols() {
        // "index" should not return "in"
        test_rule!(
            "index",
            TokenType::Identifier {
                identifier: "index".into()
            }
        );

        // "<=" should not return "<"
        test_rule!(
            "<=",
            TokenType::Symbol {
                symbol: Symbol::LessThanEqual,
            }
        );
    }

    #[test]
    fn test_rule_shebang() {
        test_rule!(
            "#!/usr/bin/env lua\n",
            TokenType::Shebang {
                line: "#!/usr/bin/env lua\n".into()
            }
        );
        // Don't recognize with a whitespace.
        test_rule!(
            " #!/usr/bin/env lua\n",
            TokenizerErrorType::UnexpectedShebang
        );
    }

    #[test]
    fn test_rule_bom() {
        let bom = String::from_utf8(b"\xEF\xBB\xBF".to_vec()).unwrap();
        test_rule!(
            &bom,
            TokenType::Whitespace {
                characters: ShortString::new(&bom),
            }
        );
        // Don't recognize if not in the beggining.
        test_rule!(
            &format!("#!/usr/bin/env lua\n {bom}"),
            TokenizerErrorType::UnexpectedToken('\u{feff}')
        );
    }

    #[test]
    fn test_new_line_on_same_line() {
        assert_eq!(
            tokens("\n").unwrap()[0],
            Token {
                start_position: Position {
                    bytes: 0,
                    character: 1,
                    line: 1,
                },

                end_position: Position {
                    bytes: 1,
                    character: 1,
                    line: 1,
                },

                token_type: TokenType::Whitespace {
                    characters: "\n".into()
                },
            }
        );
    }

    #[cfg(feature = "roblox")]
    #[test]
    fn test_string_interpolation_multi_line() {
        let tokens = tokens("`Welcome to \\\n{name}!`").unwrap();
        assert_eq!(tokens[0].to_string(), "`Welcome to \\\n{");
    }

    #[test]
    fn test_fuzzer() {
        let _ = tokens("*ա");
        let _ = tokens("̹(");
        let _ = tokens("¹;");
    }
}
