use crate::visitors::{Visit, VisitMut, Visitor, VisitorMut};

use full_moon_derive::symbols;
use peg;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, cmp::Ordering, fmt, str::FromStr};

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
    // TODO: This only is valid in Lua 5.2
    Goto => "goto",

    // TODO: This only is valid in Roblox
    PlusEqual => "+=",
    MinusEqual => "-=",
    StarEqual => "*=",
    SlashEqual => "/=",
    PercentEqual => "%=",
    CaretEqual => "^=",
    TwoDotsEqual => "..=",
    // TODO: This only is valid in Roblox
    Ampersand => "&",
    // TODO: This only is valid in Roblox
    ThinArrow => "->",
    // TODO: This only is valid in Roblox and Lua 5.2
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
    // TODO: This only is valid in Roblox
    Pipe => "|",
    Plus => "+",
    // TODO: This only is valid in Roblox
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
pub enum TokenType<'a> {
    /// End of file, should always be the very last token
    Eof,

    /// An identifier, such as `foo`
    Identifier {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The identifier itself
        identifier: Cow<'a, str>,
    },

    /// A literal number, such as `3.3`
    Number {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The text representing the number, includes details such as `0x`
        text: Cow<'a, str>,
    },

    /// A literal string, such as "Hello, world"
    StringLiteral {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The literal itself, ignoring quotation marks
        literal: Cow<'a, str>,
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
}

impl<'a> TokenType<'a> {
    /// Returns the kind of the token type.
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
            TokenType::Number { .. } => TokenKind::Number,
            TokenType::StringLiteral { .. } => TokenKind::StringLiteral,
            TokenType::Symbol { .. } => TokenKind::Symbol,
        }
    }
}

impl<'a> TriviaType<'a> {
    /// Returns a whitespace `TokenType` consisting of spaces
    pub fn spaces(spaces: usize) -> Self {
        TriviaType::Whitespace {
            characters: Cow::from(" ".repeat(spaces)),
        }
    }

    /// Returns a whitespace `TokenType` consisting of tabs
    pub fn tabs(tabs: usize) -> Self {
        TriviaType::Whitespace {
            characters: Cow::from("\t".repeat(tabs)),
        }
    }

    /// Returns the kind of the triva type.
    ///
    /// ```rust
    /// use std::borrow::Cow;
    /// use full_moon::tokenizer::{TriviaTriviaType};
    ///
    /// assert_eq!(
    ///     TriviaType::Whitespace {
    ///         characters: Cow::from("   ")
    ///     }.kind(),
    ///     TriviaKind::Whitespace,
    /// );
    /// ```
    pub fn kind(&self) -> TriviaKind {
        match self {
            TriviaType::Shebang { .. } => TriviaKind::Shebang,
            TriviaType::SingleLineComment { .. } => TriviaKind::SingleLineComment,
            TriviaType::MultiLineComment { .. } => TriviaKind::MultiLineComment,
            TriviaType::Whitespace { .. } => TriviaKind::Whitespace,
        }
    }

    /// Returns whether this should be the terminal trivia of a trailing trivia cluster.
    fn is_terminal(&self) -> bool {
        if let TriviaType::Whitespace { ref characters } = self {
            // Use contains in order to tolerate \r\n line endings and mixed whitespace tokens
            characters.contains('\n')
        } else {
            false
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
    /// A literal number, such as `3.3`
    Number,
    /// A literal string, such as "Hello, world"
    StringLiteral,
    /// A [`Symbol`], such as `local` or `+`
    Symbol,
}

/// The kind of trivia. Contains no additional data.
#[derive(Clone, Copy, Debug, Eq, PartialEq)]
#[non_exhaustive]
pub enum TriviaKind {
    /// The shebang line
    Shebang,
    /// A single line comment, such as `-- comment`
    SingleLineComment,
    /// A multi line comment in the format of `--[[ comment ]]`
    MultiLineComment,
    /// Whitespace, such as tabs or new lines
    Whitespace,
}

#[derive(Clone, Debug, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(tag = "type"))]
#[non_exhaustive]
pub enum TriviaType<'a> {
    /// A shebang line
    Shebang {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The shebang line itself
        line: Cow<'a, str>,
    },

    /// A single line comment, such as `-- comment`
    SingleLineComment {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The comment, ignoring initial `--`
        comment: Cow<'a, str>,
    },

    /// A multi line comment in the format of `--[[ comment ]]`
    MultiLineComment {
        /// Number of equals signs, if any, for the multi line comment
        /// For example, `--[=[` would have a `blocks` value of `1`
        blocks: usize,
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The comment itself, ignoring opening and closing tags
        comment: Cow<'a, str>,
    },

    /// Whitespace, such as tabs or new lines
    Whitespace {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// Characters consisting of the whitespace
        characters: Cow<'a, str>,
    },
}

/// A token such consisting of its [`Position`] and a [`TokenType`]
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Spanned<T> {
    pub(crate) start_position: Position,
    pub(crate) end_position: Position,
    pub(crate) token_type: T,
}

impl<T> Spanned<T> {
    /// Creates a token with a zero position
    pub fn new(token_type: T) -> Spanned<T> {
        Spanned {
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
}

impl<T> std::ops::Deref for Spanned<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.token_type
    }
}

pub type Token<'a> = Spanned<TokenType<'a>>;
pub type Trivia<'a> = Spanned<TriviaType<'a>>;

impl<'a> Token<'a> {
    /// The type of token as well as the data needed to represent it
    /// If you don't need any other information, use [`token_kind`](Token::token_kind) instead.
    pub fn token_type(&self) -> &TokenType<'a> {
        &self
    }

    /// The kind of token with no additional data.
    /// If you need any information such as idenitfier names, use [`token_type`](Token::token_type) instead.
    pub fn token_kind(&self) -> TokenKind {
        self.kind()
    }
}

impl<'a> Trivia<'a> {
    /// The type of trivia as well as the data needed to represent it
    /// If you don't need any other information, use [`trivia_kind`](Trivia::trivia_kind) instead.
    pub fn trivia_type(&self) -> &TriviaType<'a> {
        &self
    }

    /// The kind of token with no additional data.
    /// If you need any information such as idenitfier names, use [`trivia_type`](Trivia::trivia_type) instead.
    pub fn trivia_kind(&self) -> TriviaKind {
        self.kind()
    }
}

impl<'a> fmt::Display for Token<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use self::TokenType::*;

        match &*self.token_type() {
            Eof => "".to_string(),
            Number { text } => text.to_string(),
            Identifier { identifier } => identifier.to_string(),
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
        }
        .fmt(formatter)
    }
}

impl<'a> fmt::Display for Trivia<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        use self::TriviaType::*;

        match &*self.trivia_type() {
            Shebang { line } => line.to_string(),
            SingleLineComment { comment } => format!("--{}", comment),
            MultiLineComment { blocks, comment } => {
                format!("--[{0}[{1}]{0}]", "=".repeat(*blocks), comment)
            }
            Whitespace { characters } => characters.to_string(),
        }
        .fmt(formatter)
    }
}

impl<T: PartialEq> PartialEq<Self> for Spanned<T> {
    fn eq(&self, rhs: &Self) -> bool {
        self.start_position == rhs.start_position
            && self.end_position == rhs.end_position
            && self.token_type == rhs.token_type
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: Eq> Ord for Spanned<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.start_position.cmp(&other.start_position)
    }
}

impl<T: Eq> PartialOrd for Spanned<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'ast> Visit<'ast> for Token<'ast> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        visitor.visit_token(self);

        match self.token_kind() {
            TokenKind::Eof => {}
            TokenKind::Identifier => visitor.visit_identifier(self),
            TokenKind::Number => visitor.visit_number(self),
            TokenKind::StringLiteral => visitor.visit_string_literal(self),
            TokenKind::Symbol => visitor.visit_symbol(self),
        }
    }
}

impl<'ast> Visit<'ast> for Trivia<'ast> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        visitor.visit_trivia(self);

        match self.trivia_kind() {
            TriviaKind::Shebang => {}
            TriviaKind::SingleLineComment => visitor.visit_single_line_comment(self),
            TriviaKind::MultiLineComment => visitor.visit_multi_line_comment(self),
            TriviaKind::Whitespace => visitor.visit_whitespace(self),
        }
    }
}

impl<'ast> VisitMut<'ast> for Token<'ast> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        let token = visitor.visit_token(self);

        match token.token_kind() {
            TokenKind::Eof => token,
            TokenKind::Identifier => visitor.visit_identifier(token),
            TokenKind::Number => visitor.visit_number(token),
            TokenKind::StringLiteral => visitor.visit_string_literal(token),
            TokenKind::Symbol => visitor.visit_symbol(token),
        }
    }
}

impl<'ast> VisitMut<'ast> for Trivia<'ast> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
        let trivia = visitor.visit_trivia(self);

        match trivia.trivia_kind() {
            TriviaKind::Shebang => trivia,
            TriviaKind::SingleLineComment => visitor.visit_single_line_comment(trivia),
            TriviaKind::MultiLineComment => visitor.visit_multi_line_comment(trivia),
            TriviaKind::Whitespace => visitor.visit_whitespace(trivia),
        }
    }
}

/// A reference to a token used by Ast's.
/// Dereferences to a [`Token`]
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct WithTrivia<'input, T> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub(crate) leading_trivia: Vec<Trivia<'input>>,
    pub(crate) token: T,
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub(crate) trailing_trivia: Vec<Trivia<'input>>,
}

impl<'input, T> WithTrivia<'input, T> {
    /// Creates a WithTrivia from leading/trailing trivia as well as the leading token
    pub fn new(
        leading_trivia: Vec<Trivia<'input>>,
        token: T,
        trailing_trivia: Vec<Trivia<'input>>,
    ) -> Self {
        Self {
            leading_trivia,
            token,
            trailing_trivia,
        }
    }

    /// Returns the inner token.
    pub fn token(&self) -> &T {
        &self.token
    }

    /// Returns the leading trivia
    pub fn leading_trivia(&self) -> impl Iterator<Item = &Trivia<'input>> {
        self.leading_trivia.iter()
    }

    /// Returns the trailing trivia
    pub fn trailing_trivia(&self) -> impl Iterator<Item = &Trivia<'input>> {
        self.trailing_trivia.iter()
    }

    /// Creates a clone of the current WithTrivia with the new inner token, preserving trivia.
    pub fn with_token<U>(&self, token: U) -> WithTrivia<'input, U> {
        WithTrivia {
            token,
            leading_trivia: self.leading_trivia.clone(),
            trailing_trivia: self.trailing_trivia.clone(),
        }
    }
}

impl<'input> WithTrivia<'input, Token<'input>> {
    /// Returns a symbol with the leading and trailing whitespace
    /// Only whitespace is supported
    /// ```rust
    /// # use full_moon::tokenizer::{Symbol, WithTrivia, TokenType, TokenizerErrorType};
    /// # fn main() -> Result<(), Box<TokenizerErrorType>> {
    /// let symbol = WithTrivia::symbol("\nreturn ")?;
    /// assert_eq!(symbol.leading_trivia().next().unwrap().to_string(), "\n");
    /// assert_eq!(symbol.token().token_type(), &TokenType::Symbol {
    ///     symbol: Symbol::Return,
    /// });
    /// assert_eq!(symbol.trailing_trivia().next().unwrap().to_string(), " ");
    /// assert!(WithTrivia::symbol("isnt whitespace").is_err());
    /// assert!(WithTrivia::symbol(" notasymbol ").is_err());
    /// # Ok(())
    /// # }
    /// ```
    pub fn symbol(text: &'input str) -> Result<Self, TokenizerErrorType> {
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
            if !character.is_ascii_whitespace() {
                symbol_text.push(chars.next().unwrap());
            } else {
                break;
            }
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
            leading_trivia: vec![Trivia::new(TriviaType::Whitespace {
                characters: Cow::Owned(leading_trivia),
            })],
            token: Token::new(TokenType::Symbol { symbol }),
            trailing_trivia: vec![Trivia::new(TriviaType::Whitespace {
                characters: Cow::Owned(trailing_trivia),
            })],
        })
    }
}

impl<'input, T> std::borrow::Borrow<T> for &WithTrivia<'input, T> {
    fn borrow(&self) -> &T {
        &**self
    }
}

impl<'input> WithTrivia<'input, Token<'input>> {}

impl<'input, T> std::ops::Deref for WithTrivia<'input, T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.token
    }
}

impl<'input, T: fmt::Display> fmt::Display for WithTrivia<'input, T> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        for trivia in &self.leading_trivia {
            formatter.write_str(&trivia.to_string())?;
        }

        self.token.fmt(formatter)?;

        for trivia in &self.trailing_trivia {
            formatter.write_str(&trivia.to_string())?;
        }

        Ok(())
    }
}

impl<'input, T: PartialEq> PartialEq<Self> for WithTrivia<'input, T> {
    fn eq(&self, other: &Self) -> bool {
        (**self).eq(other)
            && self.leading_trivia == other.leading_trivia
            && self.trailing_trivia == other.trailing_trivia
    }
}

impl<'input, T: Eq> Eq for WithTrivia<'input, T> {}

impl<'input, T: Ord> Ord for WithTrivia<'input, T> {
    fn cmp(&self, other: &Self) -> Ordering {
        (**self).cmp(&**other)
    }
}

impl<'input, T: Ord> PartialOrd for WithTrivia<'input, T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'ast> Visit<'ast> for WithTrivia<'ast, Token<'ast>> {
    fn visit<V: Visitor<'ast>>(&self, visitor: &mut V) {
        visitor.visit_token(self);

        if matches!(self.token().token_kind(), TokenKind::Eof) {
            visitor.visit_eof(self);
        }

        self.leading_trivia.visit(visitor);
        self.token.visit(visitor);
        self.trailing_trivia.visit(visitor);
    }
}

impl<'ast> VisitMut<'ast> for WithTrivia<'ast, Token<'ast>> {
    fn visit_mut<V: VisitorMut<'ast>>(self, visitor: &mut V) -> Self {
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
enum RawTokenType<'input> {
    Token(TokenType<'input>),
    Trivia(TriviaType<'input>),
    Error(TokenizerErrorType),
}

#[derive(Clone, Debug, PartialEq)]
enum RawToken<'input> {
    Token(Token<'input>),
    Trivia(Trivia<'input>),
    Error(TokenizerError),
}

impl<'input> From<Spanned<RawTokenType<'input>>> for RawToken<'input> {
    fn from(raw_token: Spanned<RawTokenType<'input>>) -> RawToken<'input> {
        match raw_token.token_type {
            RawTokenType::Token(token_type) => RawToken::Token(Token {
                start_position: raw_token.start_position,
                end_position: raw_token.end_position,
                token_type,
            }),
            RawTokenType::Trivia(token_type) => RawToken::Trivia(Trivia {
                start_position: raw_token.start_position,
                end_position: raw_token.end_position,
                token_type,
            }),
            RawTokenType::Error(error) => RawToken::Error(TokenizerError {
                error,
                position: raw_token.start_position,
            }),
        }
    }
}

impl<'input> From<TokenType<'input>> for RawTokenType<'input> {
    fn from(token_type: TokenType<'input>) -> RawTokenType<'input> {
        RawTokenType::Token(token_type)
    }
}
impl<'input> From<TriviaType<'input>> for RawTokenType<'input> {
    fn from(trivia_type: TriviaType<'input>) -> RawTokenType<'input> {
        RawTokenType::Trivia(trivia_type)
    }
}
impl<'input> From<TokenizerErrorType> for RawTokenType<'input> {
    fn from(error: TokenizerErrorType) -> RawTokenType<'input> {
        RawTokenType::Error(error)
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

peg::parser! {
    grammar tokens() for str {
        use super::ParseSymbol;
        use peg::ParseLiteral;
        use super::StringLiteralQuoteType as QuoteType;
        rule line_ending()
            = "\n" / "\r\n"
        rule space()
            = [' '|'\t']

        pub(super) rule whitespace() -> RawTokenType<'input>
            = chars:$( space()+ line_ending()? / line_ending() )
              { TriviaType::Whitespace { characters:chars.into() }.into() }

        rule multi_line_start() -> &'input str
            = "[" block:$("="*) "[" {block}

        rule multi_line_end(block: &'input str)
            = "]" ##parse_string_literal(block) "]"

        rule multi_line_block() -> (usize, &'input str)
            = block:multi_line_start()
              content:$((!multi_line_end(block) [_])*)
              multi_line_end(block)
              { (block.len(), content) }

        rule multi_line_quote() -> RawTokenType<'input>
            = v:multi_line_block() { TokenType::StringLiteral {
                multi_line: Some(v.0),
                literal:v.1.into(),
                quote_type: QuoteType::Brackets,
            }.into()}
            / &multi_line_start() [_]* { TokenizerErrorType::UnclosedString.into() }

        rule escape()
            = "\\" [_]

        rule quote_char(quote: &str)
            = !(##parse_string_literal(quote) / ['\r'|'\n'|'\\']) [_]

        rule quoted(quote: &str, quote_type: QuoteType) -> RawTokenType<'input>
            = ##parse_string_literal(quote)
              literal:$((quote_char(quote) / escape())+ / )
              ##parse_string_literal(quote)
              { TokenType::StringLiteral { multi_line: None, literal:literal.into(), quote_type }.into() }
            / ##parse_string_literal(quote) [_]* {TokenizerErrorType::UnclosedString.into() }

        rule single_line_quote() -> RawTokenType<'input>
            = quoted("\"", (QuoteType::Double))
            / quoted("\'", (QuoteType::Single))

        pub(super) rule string_literal() -> RawTokenType<'input>
            = multi_line_quote() / single_line_quote()

        pub(super) rule shebang() -> RawTokenType<'input>
            = line:$("#!" (!line_ending() [_])* line_ending())
              {TriviaType::Shebang{line:line.into()}.into()}

        pub(super) rule identifier() -> RawTokenType<'input>
            = id:$(['_'|'a'..='z'|'A'..='Z'] ['_'|'a'..='z'|'A'..='Z'|'0'..='9']*)
              { match parse_keyword(id) {
                    Some(symbol) => TokenType::Symbol { symbol }.into(),
                    None => TokenType::Identifier { identifier: id.into() }.into(),
              }}
            / expected!("identifier")

        pub(super) rule comment() -> RawTokenType<'input>
            = "--" v:multi_line_block()
              { TriviaType::MultiLineComment { blocks: v.0, comment: v.1.into() }.into() }
            / "--" multi_line_start() [_]* { TokenizerErrorType::UnclosedComment.into() }
            / "--" comment:$((!['\r'|'\n'] [_])*)
              { TriviaType::SingleLineComment { comment: comment.into() }.into() }

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
        pub(super) rule number() -> RawTokenType<'input>
            = n:(
                roblox_number()
              / hex_number()
              / basic_number()
              / no_int_fractional_number()
            ) { TokenType::Number { text:n.into() }.into() }

        pub(super) rule symbol() -> RawTokenType<'input> = symbol:##parse_symbol() { TokenType::Symbol{symbol}.into() }

        rule token() -> RawTokenType<'input>
            = whitespace()
            / comment()
            / number()
            / string_literal()
            / "#!" { TokenizerErrorType::UnexpectedShebang.into() }
            / symbol()
            / identifier()

        pub(super) rule tokens() -> Vec<(RawTokenType<'input>, usize)>
            = shebang:(shebang:shebang() pos:position!() {(shebang,pos)})?
              body:( token:token() pos:position!() {(token,pos)})*
              {
                  let mut body = body;
                  if let Some(shebang) = shebang {
                      body.insert(0, shebang)
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

struct TokenCollector<'input> {
    result: Vec<WithTrivia<'input, Token<'input>>>,
    leading_trivia: Vec<Trivia<'input>>,
    trailing_trivia: Vec<Trivia<'input>>,
    current_token: Option<Token<'input>>,
}

// Collector
impl<'input> TokenCollector<'input> {
    fn new() -> Self {
        Self {
            result: Vec::new(),
            leading_trivia: Vec::new(),
            trailing_trivia: Vec::new(),
            current_token: None,
        }
    }
    fn push(&mut self, raw_token: RawToken<'input>) -> Result<(), TokenizerError> {
        Ok(match raw_token {
            RawToken::Trivia(trivia) => match self.current_token {
                Some(_) => {
                    if trivia.is_terminal() {
                        self.trailing_trivia.push(trivia);
                        self.result.push(WithTrivia {
                            leading_trivia: self.leading_trivia.drain(..).collect(),
                            trailing_trivia: self.trailing_trivia.drain(..).collect(),
                            token: self.current_token.take().unwrap(), // FIXME: unwrap
                        });
                        self.current_token = None;
                    } else {
                        self.trailing_trivia.push(trivia);
                    }
                }
                None => {
                    self.leading_trivia.push(trivia);
                }
            },
            RawToken::Token(token) => {
                if let Some(current_token) = self.current_token.take() {
                    self.result.push(WithTrivia {
                        leading_trivia: self.leading_trivia.drain(..).collect(),
                        trailing_trivia: self.trailing_trivia.drain(..).collect(),
                        token: current_token,
                    });
                }
                self.current_token = Some(token);
            }
            RawToken::Error(error) => Err(error)?,
        })
    }
    fn finish(mut self, eof_position: Position) -> Vec<WithTrivia<'input, Token<'input>>> {
        if let Some(token) = self.current_token {
            self.result.push(WithTrivia {
                leading_trivia: self.leading_trivia.drain(..).collect(),
                trailing_trivia: self.trailing_trivia.drain(..).collect(),
                token,
            });
        }
        assert_eq!(
            self.trailing_trivia.len(),
            0,
            "(internal full-moon error) trailing trivia left when adding eof to token stream)"
        );
        self.result.push(WithTrivia {
            leading_trivia: self.leading_trivia,
            trailing_trivia: self.trailing_trivia,
            token: Token {
                start_position: eof_position,
                end_position: eof_position,
                token_type: TokenType::Eof,
            },
        });
        self.result
    }
}

fn from_parser_error<'input>(
    code: &'input str,
) -> impl Fn(peg::error::ParseError<peg::str::LineCol>) -> TokenizerError + 'input {
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
pub fn tokens<'a>(code: &'a str) -> Result<Vec<WithTrivia<'a, Token<'a>>>, TokenizerError> {
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
                tokens.push(
                    Spanned {
                        start_position,
                        token_type: token_type,
                        end_position,
                    }
                    .into(),
                )?;
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
                    if token.is_trivia() {
                        let first_trivia = &first_token
                            .leading_trivia
                            .get(0)
                            .expect("leading trivia is empty");
                        assert_eq!(*first_trivia.token_type(), token);
                    } else {
                        assert_eq!(*first_token.token_type(), token);
                    }
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
            TriviaType::Whitespace {
                characters: "\t  \n".into(),
            }
        );

        test_rule!(
            "\thello",
            TriviaType::Whitespace {
                characters: "\t".into(),
            }
        );

        test_rule!(
            "\t\t\nhello",
            TriviaType::Whitespace {
                characters: "\t\t\n".into(),
            }
        );

        test_rule!(
            "\n\thello",
            TriviaType::Whitespace {
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
    fn test_new_line_on_same_line() {
        assert_eq!(
            tokens("\n").unwrap()[0].leading_trivia[0],
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

                token_type: TriviaType::Whitespace {
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

    #[test]
    fn test_extract_token_references() {
        let references =
            tokens("print(1)\n-- hello world\nlocal foo -- this is the word foo").unwrap();

        assert_eq!(references.len(), 7);

        assert!(references[0].trailing_trivia.is_empty());
        assert_eq!(references[0].token.to_string(), "print");
        assert!(references[0].leading_trivia.is_empty());

        assert!(references[1].trailing_trivia.is_empty());
        assert_eq!(references[1].token.to_string(), "(");
        assert!(references[1].leading_trivia.is_empty());

        assert!(references[2].trailing_trivia.is_empty());
        assert_eq!(references[2].token.to_string(), "1");
        assert!(references[2].leading_trivia.is_empty());

        assert_eq!(references[3].trailing_trivia[0].to_string(), "\n");
        assert_eq!(references[3].token.to_string(), ")");
        assert!(references[3].leading_trivia.is_empty());

        assert_eq!(
            references[4].leading_trivia[0].to_string(),
            "-- hello world",
        );

        assert_eq!(references[4].leading_trivia[1].to_string(), "\n");
        assert_eq!(references[4].token.to_string(), "local");
        assert_eq!(references[4].trailing_trivia[0].to_string(), " ");
    }
}
