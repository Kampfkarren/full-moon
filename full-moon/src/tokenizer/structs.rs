use crate::{
    visitors::{Visit, VisitMut, Visitor, VisitorMut},
    ShortString,
};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{
    cmp::Ordering,
    convert::TryFrom,
    fmt::{self, Display},
};

#[cfg(feature = "roblox")]
pub use crate::tokenizer_luau::InterpolatedStringKind;

use super::{Lexer, LexerResult};

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

// rewrite todo: bring back macro?
impl TryFrom<&str> for Symbol {
    type Error = ();

    fn try_from(value: &str) -> Result<Self, ()> {
        match value {
            "and" => Ok(Symbol::And),
            "break" => Ok(Symbol::Break),
            "do" => Ok(Symbol::Do),
            "else" => Ok(Symbol::Else),
            "elseif" => Ok(Symbol::ElseIf),
            "end" => Ok(Symbol::End),
            "false" => Ok(Symbol::False),
            "for" => Ok(Symbol::For),
            "function" => Ok(Symbol::Function),
            "if" => Ok(Symbol::If),
            "in" => Ok(Symbol::In),
            "local" => Ok(Symbol::Local),
            "nil" => Ok(Symbol::Nil),
            "not" => Ok(Symbol::Not),
            "or" => Ok(Symbol::Or),
            "repeat" => Ok(Symbol::Repeat),
            "return" => Ok(Symbol::Return),
            "then" => Ok(Symbol::Then),
            "true" => Ok(Symbol::True),
            "until" => Ok(Symbol::Until),
            "while" => Ok(Symbol::While),
            #[cfg(feature = "lua52")]
            "goto" => Ok(Symbol::Goto),
            "+=" => Ok(Symbol::PlusEqual),
            "-=" => Ok(Symbol::MinusEqual),
            "*=" => Ok(Symbol::StarEqual),
            "/=" => Ok(Symbol::SlashEqual),
            "%=" => Ok(Symbol::PercentEqual),
            "^=" => Ok(Symbol::CaretEqual),
            "..=" => Ok(Symbol::TwoDotsEqual),
            #[cfg(any(feature = "roblox", feature = "lua53"))]
            "&" => Ok(Symbol::Ampersand),
            #[cfg(feature = "roblox")]
            "->" => Ok(Symbol::ThinArrow),
            #[cfg(any(feature = "roblox", feature = "lua52"))]
            "::" => Ok(Symbol::TwoColons),
            "^" => Ok(Symbol::Caret),
            ":" => Ok(Symbol::Colon),
            "," => Ok(Symbol::Comma),
            "..." => Ok(Symbol::Ellipse),
            ".." => Ok(Symbol::TwoDots),
            "." => Ok(Symbol::Dot),
            "==" => Ok(Symbol::TwoEqual),
            "=" => Ok(Symbol::Equal),
            ">=" => Ok(Symbol::GreaterThanEqual),
            ">" => Ok(Symbol::GreaterThan),
            #[cfg(feature = "lua53")]
            ">>" => Ok(Symbol::DoubleGreaterThan),
            "#" => Ok(Symbol::Hash),
            "[" => Ok(Symbol::LeftBracket),
            "{" => Ok(Symbol::LeftBrace),
            "(" => Ok(Symbol::LeftParen),
            "<=" => Ok(Symbol::LessThanEqual),
            "<" => Ok(Symbol::LessThan),
            #[cfg(feature = "lua53")]
            "<<" => Ok(Symbol::DoubleLessThan),
            "-" => Ok(Symbol::Minus),
            "%" => Ok(Symbol::Percent),
            #[cfg(any(feature = "roblox", feature = "lua53"))]
            "|" => Ok(Symbol::Pipe),
            "+" => Ok(Symbol::Plus),
            #[cfg(feature = "roblox")]
            "?" => Ok(Symbol::QuestionMark),
            "}" => Ok(Symbol::RightBrace),
            "]" => Ok(Symbol::RightBracket),
            ")" => Ok(Symbol::RightParen),
            ";" => Ok(Symbol::Semicolon),
            "/" => Ok(Symbol::Slash),
            #[cfg(feature = "lua53")]
            "//" => Ok(Symbol::DoubleSlash),
            "*" => Ok(Symbol::Star),
            #[cfg(feature = "lua53")]
            "~" => Ok(Symbol::Tilde),
            "~=" => Ok(Symbol::TildeEqual),
            _ => Err(()),
        }
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
    // rewrite todo: remove
    UnexpectedShebang,
    // rewrite todo: changelog
    // rewrite todo: this only makes sense with RANGE
    InvalidNumber,
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
            TokenizerErrorType::InvalidNumber => "invalid number".fmt(formatter),
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
#[derive(Clone, Debug, Eq, PartialEq)]
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
        let mut lexer = Lexer::new_lazy(text);

        let mut leading_trivia = Vec::new();
        let symbol;

        loop {
            match lexer.process_next() {
                Some(LexerResult::Ok(
                    token @ Token {
                        token_type: TokenType::Whitespace { .. },
                        ..
                    },
                )) => {
                    leading_trivia.push(token);
                }

                Some(LexerResult::Ok(
                    token @ Token {
                        token_type: TokenType::Symbol { .. },
                        ..
                    },
                )) => {
                    symbol = token;
                    break;
                }

                Some(LexerResult::Ok(Token {
                    token_type: TokenType::Eof,
                    ..
                })) => {
                    return Err(TokenizerErrorType::InvalidSymbol(text.to_owned()));
                }

                Some(LexerResult::Ok(token)) => {
                    // rewrite todo: I think buffing the lexer is going to result in
                    // this actually receiving a Token and not just a char (or at least a variant for it)
                    return Err(TokenizerErrorType::UnexpectedToken(
                        token.to_string().chars().next().unwrap(),
                    ));
                }

                Some(LexerResult::Fatal(mut errors) | LexerResult::Recovered(_, mut errors)) => {
                    return Err(errors.remove(0).error);
                }

                None => unreachable!("we shouldn't have hit eof"),
            }
        }

        let mut trailing_trivia = Vec::new();

        loop {
            match lexer.process_next() {
                Some(LexerResult::Ok(
                    token @ Token {
                        token_type: TokenType::Whitespace { .. },
                        ..
                    },
                )) => {
                    trailing_trivia.push(token);
                }

                Some(LexerResult::Ok(Token {
                    token_type: TokenType::Eof,
                    ..
                })) => {
                    break;
                }

                Some(LexerResult::Ok(token)) => {
                    return Err(TokenizerErrorType::UnexpectedToken(
                        token.to_string().chars().next().unwrap(),
                    ));
                }

                Some(LexerResult::Fatal(mut errors) | LexerResult::Recovered(_, mut errors)) => {
                    return Err(errors.remove(0).error);
                }

                None => {
                    unreachable!("we shouldn't have hit eof");
                }
            }
        }

        Ok(TokenReference {
            leading_trivia,
            token: symbol,
            trailing_trivia,
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

    /// Checks if the token is the given symbol
    pub fn is_symbol(&self, symbol: Symbol) -> bool {
        self.token.token_type() == &TokenType::Symbol { symbol }
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

/// Information about an error that occurs while tokenizing
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TokenizerError {
    /// The type of error
    pub(crate) error: TokenizerErrorType,
    /// The position of the token that caused the error
    pub(crate) position: Position,
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

// #[cfg(test)]
#[cfg(feature = "rewrite todo: tokenizer tests")]
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
