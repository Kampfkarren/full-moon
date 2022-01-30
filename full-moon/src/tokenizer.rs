use crate::{
    visitors::{Visit, VisitMut, Visitor, VisitorMut},
    ShortString,
};

use full_moon_derive::symbols;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{cmp::Ordering, fmt, str::FromStr};

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
    #[cfg(feature = "lua52")]
    Goto => "goto",

    #[cfg(feature = "roblox")]
    PlusEqual => "+=",
    MinusEqual => "-=",
    StarEqual => "*=",
    SlashEqual => "/=",
    PercentEqual => "%=",
    CaretEqual => "^=",
    TwoDotsEqual => "..=",
    #[cfg(feature = "roblox")]
    Ampersand => "&",
    #[cfg(feature = "roblox")]
    ThinArrow => "->",
    #[cfg(any(feature = "roblox", feature = "lua52"))]
    TwoColons => "::",
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
    #[cfg(feature = "roblox")]
    Pipe => "|",
    Plus => "+",
    #[cfg(feature = "roblox")]
    QuestionMark => "?",
    RightBrace => "}",
    RightBracket => "]",
    RightParen => ")",
    Semicolon => ";",
    Slash => "/",
    Star => "*",
    TildeEqual => "~=",
);

/// The possible errors that can happen while tokenizing.
#[derive(Clone, Debug, PartialEq)]
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

        match &*self.token_type() {
            Eof => "".to_string(),
            Number { text } => text.to_string(),
            Identifier { identifier } => identifier.to_string(),
            MultiLineComment { blocks, comment } => {
                format!("--[{0}[{1}]{0}]", "=".repeat(*blocks), comment)
            }
            Shebang { line } => line.to_string(),
            SingleLineComment { comment } => format!("--{}", comment),
            StringLiteral {
                literal,
                multi_line,
                quote_type,
            } => {
                if let Some(blocks) = multi_line {
                    format!("[{0}[{1}]{0}]", "=".repeat(*blocks), literal)
                } else {
                    format!("{0}{1}{0}", quote_type.to_string(), literal)
                }
            }
            Symbol { symbol } => symbol.to_string(),
            Whitespace { characters } => characters.to_string(),
        }
        .fmt(formatter)
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
        let mut chars = text.chars().peekable();

        let mut leading_trivia = String::new();
        while let Some(character) = chars.peek() {
            if character.is_ascii_whitespace() {
                leading_trivia.push(chars.next().unwrap());
            } else {
                break;
            }
        }

        let mut symbol_text = String::new();
        while let Some(character) = chars.peek() {
            if character.is_ascii_whitespace() {
                break;
            }

            symbol_text.push(chars.next().unwrap());
        }

        let symbol = Symbol::from_str(&symbol_text)
            .map_err(|_| TokenizerErrorType::InvalidSymbol(symbol_text))?;

        let mut trailing_trivia = String::new();
        while let Some(character) = chars.peek() {
            if character.is_ascii_whitespace() {
                trailing_trivia.push(chars.next().unwrap());
            } else {
                return Err(TokenizerErrorType::UnexpectedToken(*character));
            }
        }

        Ok(Self {
            leading_trivia: vec![Token::new(TokenType::Whitespace {
                characters: leading_trivia.into(),
            })],
            token: Token::new(TokenType::Symbol { symbol }),
            trailing_trivia: vec![Token::new(TokenType::Whitespace {
                characters: trailing_trivia.into(),
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
        &**self
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
            formatter.write_str(&trivia.to_string())?;
        }

        formatter.write_str(&self.token.to_string())?;

        for trivia in &self.trailing_trivia {
            formatter.write_str(&trivia.to_string())?;
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

#[derive(Clone, Debug, PartialEq)]
struct TokenAdvancement {
    pub advance: usize,
    pub token_type: TokenType,
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
            StringLiteralQuoteType::Double => "\"",
            StringLiteralQuoteType::Single => "'",
        }
        .fmt(formatter)
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

peg::parser! {
    grammar tokens() for str {
        use super::ParseSymbol;
        use peg::ParseLiteral;
        use super::StringLiteralQuoteType as QuoteType;

        rule line_ending()
            = "\n" / "\r\n"
        rule space()
            = [' '|'\t']

        pub(super) rule whitespace() -> RawToken
            = chars:$( space()+ line_ending()? / line_ending() )
              { TokenType::Whitespace { characters:chars.into() }.into() }

        rule multi_line_start() -> &'input str
            = "[" block:$("="*) "[" {block}

        rule multi_line_end(block: &'input str)
            = "]" ##parse_string_literal(block) "]"

        rule multi_line_block() -> (usize, &'input str)
            = block:multi_line_start()
              content:$((!multi_line_end(block) [_])*)
              multi_line_end(block)
              { (block.len(), content) }

        rule multi_line_quote() -> RawToken
            = v:multi_line_block() { TokenType::StringLiteral {
                multi_line: Some(v.0),
                literal:v.1.into(),
                quote_type: QuoteType::Brackets,
            }.into()}
            / &multi_line_start() [_]+ { TokenizerErrorType::UnclosedString.into() }

        rule escape()
            = "\\" [_]

        rule quote_char(quote: &str)
            = !(##parse_string_literal(quote) / ['\r'|'\n'|'\\']) [_]

        rule quoted(quote: &str, quote_type: QuoteType) -> RawToken
            = ##parse_string_literal(quote)
              literal:$((quote_char(quote) / escape())+ / )
              ##parse_string_literal(quote)
              { TokenType::StringLiteral { multi_line: None, literal:literal.into(), quote_type }.into() }
            / ##parse_string_literal(quote) [_]* {TokenizerErrorType::UnclosedString.into() }

        rule single_line_quote() -> RawToken
            = quoted("\"", (QuoteType::Double))
            / quoted("\'", (QuoteType::Single))

        pub(super) rule string_literal() -> RawToken
            = multi_line_quote()
            / single_line_quote()

        pub(super) rule shebang() -> RawToken
            = line:$("#!" (!line_ending() [_])* line_ending())
              {TokenType::Shebang{line:line.into()}.into()}

        pub(super) rule utf8_bom() -> RawToken
            = chars:$("\u{feff}") // A BOM sequence in UTF8 "\xEF\xBB\xBF"
            { TokenType::Whitespace { characters:chars.into() }.into() }

        pub(super) rule identifier() -> RawToken
            = id:$(['_'|'a'..='z'|'A'..='Z'] ['_'|'a'..='z'|'A'..='Z'|'0'..='9']*)
              { match parse_keyword(id) {
                    Some(symbol) => TokenType::Symbol { symbol }.into(),
                    None => TokenType::Identifier { identifier: id.into() }.into(),
              }}
            / expected!("identifier")

        pub(super) rule comment() -> RawToken
            = "--" v:multi_line_block()
              { TokenType::MultiLineComment { blocks: v.0, comment: v.1.into() }.into() }
            / "--" multi_line_start() [_]* { TokenizerErrorType::UnclosedComment.into() }
            / "--" comment:$(([^ '\r'|'\n'])*)
              { TokenType::SingleLineComment { comment: comment.into() }.into() }

        rule roblox()
            = {? if cfg!(feature = "roblox") {
                Ok(())
            } else {
                Err("roblox not enabled")
            }}

        rule roblox_number() -> &'input str
            = roblox() n:$(("0b"/"0B") ['0'|'1'|'_']+) {n}

        rule hex_number() -> &'input str
            = roblox() n:$(("0x"/"0X") ['0'..='9'|'a'..='f'|'A'..='F'|'_']+) {n}
            / !roblox() n:$(("0x"/"0X") ['0'..='9'|'a'..='f'|'A'..='F']+) {n}

        rule digit_with_separator() -> &'input str
            = roblox() n:$(['0'..='9'] ['0'..='9'|'_']*) {n}
            / !roblox() n:$(['0'..='9']+) {n}

        rule basic_number() -> &'input str
            = $(
                digit_with_separator()
                ("." digit_with_separator()?)?
                (['e'|'E'] ['-'|'+']? digit_with_separator())?
            )

        rule no_int_fractional_number() -> &'input str
            = $(
                "." digit_with_separator()
                (['e'|'E'] ['-'|'+']? digit_with_separator())?
            )

        pub(super) rule number() -> RawToken
            = n:(
                roblox_number()
              / hex_number()
              / basic_number()
              / no_int_fractional_number()
            ) { TokenType::Number { text:n.into() }.into() }

        pub(super) rule symbol() -> RawToken = symbol:##parse_symbol() { TokenType::Symbol{symbol}.into() }

        rule token() -> RawToken
            = whitespace()
            / comment()
            / number()
            / string_literal()
            / "#!" { TokenizerErrorType::UnexpectedShebang.into() }
            / symbol()
            / identifier()

        pub(crate) rule tokens() -> Vec<(RawToken, usize)>
            = bom:(bom:utf8_bom() pos:position!() {(bom,pos)})?
              shebang:(shebang:shebang() pos:position!() {(shebang,pos)})?
              body:( token:token() pos:position!() {(token,pos)})*
              {
                  let mut body = body;
                  if let Some(shebang) = shebang {
                      body.insert(0, shebang);
                  }
                  if let Some(bom) = bom {
                      body.insert(0, bom);
                  }
                  body
              }
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
            match &self.error {
                TokenizerErrorType::UnclosedComment => "unclosed comment".to_string(),
                TokenizerErrorType::UnclosedString => "unclosed string".to_string(),
                TokenizerErrorType::UnexpectedShebang => "unexpected shebang".to_string(),
                TokenizerErrorType::UnexpectedToken(character) => {
                    format!("unexpected character {}", character)
                }
                TokenizerErrorType::InvalidSymbol(symbol) => {
                    format!("invalid symbol {}", symbol)
                }
            },
            self.position.line,
            self.position.character,
        )
    }
}

impl std::error::Error for TokenizerError {}

impl From<peg::str::LineCol> for Position {
    fn from(location: peg::str::LineCol) -> Position {
        Position {
            bytes: location.offset,
            line: location.line,
            character: location.column,
        }
    }
}

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

fn from_parser_error(
    code: &'_ str,
) -> impl Fn(peg::error::ParseError<peg::str::LineCol>) -> TokenizerError + '_ {
    move |err| TokenizerError {
        error: TokenizerErrorType::UnexpectedToken(
            code[err.location.offset..].chars().next().expect(
                "(internal full-moon error) Text overflow while giving unexpected token error",
            ),
        ),
        position: err.location.into(),
    }
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

    let mut raw_tokens = tokens::tokens(code).map_err(from_parser_error(code))?;

    // rust-peg lets us easily get the offset associated with
    // (the end of) each token, but not the line or column
    // information. We iterate over the characters to match
    // up the tokens with the row/column information.
    let mut raw_tokens = raw_tokens.drain(..);

    let mut position = Position {
        bytes: 0,
        character: 1,
        line: 1,
    };
    let mut next_is_new_line = false;
    let mut start_position = position;
    if let Some((mut token_type, mut token_offset)) = raw_tokens.next() {
        for character in code.chars() {
            if character == '\n' {
                next_is_new_line = true;
            } else {
                position.character += 1;
            }

            position.bytes += character.len_utf8();

            let end_position = position;

            if next_is_new_line {
                next_is_new_line = false;
                position.line += 1;
                position.character = 1;
            }

            if token_offset == end_position.bytes {
                tokens.push(start_position, token_type, end_position)?;
                start_position = position;
                if let Some((next_token_type, next_token_offset)) = raw_tokens.next() {
                    token_type = next_token_type;
                    token_offset = next_token_offset;
                } else {
                    break;
                }
            }
        }
    }

    if let Some((token_type, token_offset)) = raw_tokens.next() {
        panic!("(internal full-moon error) Found token {:?} with offset {:?} which is past the end of source", token_type, token_offset);
    }

    Ok(tokens.finish(position))
}

#[cfg(test)]
mod tests {
    use crate::tokenizer::*;
    use pretty_assertions::assert_eq;

    macro_rules! test_rule {
        ($rule:ident($code:expr), $result:expr) => {
            let code: &str = $code;
            let result: RawToken = $result.into();

            assert_eq!(
                tokens::$rule(code)
                    .map_err(|err| from_parser_error(code)(err).error)
                    .and_then(|v| v),
                result,
            );
            test_rule!(code, result)
        };
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
                    }
                }
            };
        };
    }

    #[test]
    fn test_rule_comment() {
        test_rule!(
            comment("-- hello world"),
            TokenType::SingleLineComment {
                comment: " hello world".into()
            }
        );

        test_rule!(
            comment("--[[ hello world ]]"),
            TokenType::MultiLineComment {
                blocks: 0,
                comment: " hello world ".into()
            }
        );

        test_rule!(
            comment("--[=[ hello world ]=]"),
            TokenType::MultiLineComment {
                blocks: 1,
                comment: " hello world ".into()
            }
        );
        test_rule!(
            comment("--"),
            TokenType::SingleLineComment { comment: "".into() }
        );
    }

    #[test]
    fn test_rule_numbers() {
        test_rule!(number("213"), TokenType::Number { text: "213".into() });

        test_rule!(number("1"), TokenType::Number { text: "1".into() });

        test_rule!(
            number("123.45"),
            TokenType::Number {
                text: "123.45".into(),
            }
        );
    }

    #[test]
    #[cfg_attr(not(feature = "roblox"), ignore)]
    fn test_rule_binary_literals() {
        test_rule!(
            number("0b101"),
            TokenType::Number {
                text: "0b101".into(),
            }
        );
    }

    #[test]
    fn test_rule_identifier() {
        test_rule!(
            identifier("hello"),
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
            identifier("hello___"),
            TokenType::Identifier {
                identifier: "hello___".into(),
            }
        );

        test_rule!(identifier("123"), TokenizerErrorType::UnexpectedToken('1'));
    }

    #[test]
    fn test_rule_symbols() {
        test_rule!(
            identifier("local"),
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
            string_literal("\"hello\""),
            TokenType::StringLiteral {
                literal: "hello".into(),
                multi_line: None,
                quote_type: StringLiteralQuoteType::Double,
            }
        );

        test_rule!(
            string_literal("\"hello\\\nworld\""),
            TokenType::StringLiteral {
                literal: "hello\\\nworld".into(),
                multi_line: None,
                quote_type: StringLiteralQuoteType::Double,
            }
        );

        test_rule!(
            string_literal("\"hello"),
            TokenizerErrorType::UnclosedString
        );
    }

    #[test]
    fn test_symbols_within_symbols() {
        // "index" should not return "in"
        test_rule!(
            identifier("index"),
            TokenType::Identifier {
                identifier: "index".into()
            }
        );

        // "<=" should not return "<"
        test_rule!(
            symbol("<="),
            TokenType::Symbol {
                symbol: Symbol::LessThanEqual,
            }
        );
    }

    #[test]
    fn test_rule_shebang() {
        test_rule!(
            shebang("#!/usr/bin/env lua\n"),
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
            utf8_bom(&bom),
            TokenType::Whitespace {
                characters: ShortString::new(&bom),
            }
        );
        // Don't recognize if not in the beggining.
        test_rule!(
            &format!("#!/usr/bin/env lua\n {}", bom),
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

    #[test]
    fn test_fuzzer() {
        let _ = tokens("*ա");
        let _ = tokens("̹(");
        let _ = tokens("¹;");
    }
}
