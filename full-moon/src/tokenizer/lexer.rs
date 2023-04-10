use crate::{tokenizer::StringLiteralQuoteType, ShortString};

use super::{
    Position, Symbol, Token, TokenReference, TokenType, TokenizerError, TokenizerErrorType,
};

pub struct Lexer {
    pub source: LexerSource,
    sent_eof: bool,

    // rewrite todo: maybe an array if we need more lookahead
    next_token: Option<LexerResult<TokenReference>>,
    peek_token: Option<LexerResult<TokenReference>>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut lexer = Self {
            source: LexerSource::new(source),
            sent_eof: false,

            next_token: None,
            peek_token: None,
        };

        lexer.next_token = lexer.process_first_with_trivia();
        lexer.peek_token = lexer.process_next_with_trivia();

        lexer
    }

    pub fn new_lazy(source: &str) -> Self {
        Self {
            source: LexerSource::new(source),
            sent_eof: false,

            next_token: None,
            peek_token: None,
        }
    }

    pub fn current(&self) -> Option<&LexerResult<TokenReference>> {
        self.next_token.as_ref()
    }

    pub fn peek(&self) -> Option<&LexerResult<TokenReference>> {
        self.peek_token.as_ref()
    }

    pub fn consume(&mut self) -> Option<LexerResult<TokenReference>> {
        let next = self.next_token.take()?;
        self.next_token = self.peek_token.take();
        self.peek_token = self.process_next_with_trivia();
        Some(next)
    }

    pub fn collect(self) -> LexerResult<Vec<Token>> {
        let mut tokens = Vec::new();
        let mut lexer = self;
        let mut errors = Vec::new();

        while let Some(token_reference) = lexer.consume() {
            let mut token_reference = match token_reference {
                LexerResult::Ok(token_reference) => token_reference,

                LexerResult::Recovered(token_reference, mut new_errors) => {
                    errors.append(&mut new_errors);
                    token_reference
                }

                LexerResult::Fatal(mut new_errors) => {
                    errors.append(&mut new_errors);
                    continue;
                }
            };

            tokens.append(&mut token_reference.leading_trivia);
            tokens.push(token_reference.token);
            tokens.append(&mut token_reference.trailing_trivia);
        }

        LexerResult::new(tokens, errors)
    }

    fn create(
        &self,
        start_position: Position,
        token_type: TokenType,
    ) -> Option<LexerResult<Token>> {
        Some(LexerResult::Ok(Token {
            token_type,
            start_position,
            end_position: self.source.position(),
        }))
    }

    fn process_next_with_trivia(&mut self) -> Option<LexerResult<TokenReference>> {
        let mut leading_trivia = Vec::new();
        let mut errors: Option<Vec<TokenizerError>> = None;

        let nontrivial_token = loop {
            match self.process_next()? {
                LexerResult::Ok(token) if token.token_type().is_trivia() => {
                    leading_trivia.push(token);
                }

                LexerResult::Ok(token) => {
                    break token;
                }

                LexerResult::Fatal(error) => {
                    return Some(LexerResult::Fatal(error));
                }

                LexerResult::Recovered(token, mut new_errors) => {
                    if let Some(errors) = errors.as_mut() {
                        errors.append(&mut new_errors);
                    } else {
                        errors = Some(new_errors);
                    }

                    if token.token_type().is_trivia() {
                        leading_trivia.push(token);
                    } else {
                        break token;
                    }
                }
            }
        };

        let trailing_trivia = self.collect_trailing_trivia();
        let token = TokenReference {
            token: nontrivial_token,
            leading_trivia,
            trailing_trivia,
        };

        if let Some(errors) = errors {
            Some(LexerResult::Recovered(token, errors))
        } else {
            Some(LexerResult::Ok(token))
        }
    }

    fn process_first_with_trivia(&mut self) -> Option<LexerResult<TokenReference>> {
        if self.source.current() == Some('#') && self.source.peek() == Some('!') {
            // rewrite todo: this changes shebang behavior to be more in line with the rest of full-moon. document
            let start_position = self.source.position();
            let mut line = "#!".to_string();

            self.source.next();
            self.source.next();

            while let Some(next) = self.source.current() {
                if next == '\n' {
                    break;
                }

                self.source.next();
                line.push(next);
            }

            let end_position = self.source.position();

            let shebang = Token {
                token_type: TokenType::Shebang { line: line.into() },
                start_position,
                end_position,
            };

            // rewrite todo: handle LexerResult better here
            if let Some(LexerResult::Ok(mut token_reference)) = self.process_next_with_trivia() {
                token_reference.leading_trivia.insert(0, shebang);
                return Some(LexerResult::Ok(token_reference));
            }
        }

        self.process_next_with_trivia()
    }

    fn collect_trailing_trivia(&mut self) -> Vec<Token> {
        let mut trailing_trivia = Vec::new();

        loop {
            let sent_eof = self.sent_eof;
            let start_position = self.source.lexer_position;

            match self.process_next() {
                Some(LexerResult::Ok(token)) if token.token_type().is_trivia() => {
                    // Take all trivia up to and including the newline character. If we see a newline character
                    // we should break once we have taken it in.
                    let should_break =
                        if let TokenType::Whitespace { ref characters } = token.token_type() {
                            // Use contains in order to tolerate \r\n line endings and mixed whitespace tokens
                            characters.contains('\n')
                        } else {
                            false
                        };

                    trailing_trivia.push(token);

                    if should_break {
                        break;
                    }
                }

                _ => {
                    self.source.lexer_position = start_position;
                    self.sent_eof = sent_eof;
                    break;
                }
            }
        }

        trailing_trivia
    }

    pub fn process_next(&mut self) -> Option<LexerResult<Token>> {
        let start_position = self.source.position();

        let Some(next) = self.source.next() else {
            if self.sent_eof {
                return None;
            } else {
                self.sent_eof = true;
                return self.create(start_position, TokenType::Eof);
            }
        };

        match next {
            initial if is_identifier_start(initial) => {
                let mut identifier = String::new();
                identifier.push(initial);

                while let Some(next) = self.source.current() {
                    if is_identifier_start(next) || matches!(next, '0'..='9') {
                        identifier.push(self.source.next().expect("peeked, but no next"));
                    } else {
                        break;
                    }
                }

                self.create(
                    start_position,
                    if let Ok(symbol) = Symbol::try_from(identifier.as_str()) {
                        TokenType::Symbol { symbol }
                    } else {
                        TokenType::Identifier {
                            identifier: ShortString::from(identifier),
                        }
                    },
                )
            }

            initial @ (' ' | '\t' | '\r') => {
                let mut whitespace = String::new();
                whitespace.push(initial);

                while let Some(next) = self.source.current() {
                    if next == ' ' || next == '\t' {
                        whitespace.push(self.source.next().expect("peeked, but no next"));
                    } else if next == '\n' {
                        whitespace.push(self.source.next().expect("peeked, but no next"));
                        break;
                    } else if next == '\r' && self.source.peek() == Some('\n') {
                        whitespace.push(self.source.next().expect("peeked, but no next"));
                        whitespace.push(self.source.next().expect("peeked, but no next"));
                        break;
                    } else {
                        break;
                    }
                }

                self.create(
                    start_position,
                    TokenType::Whitespace {
                        characters: ShortString::from(whitespace),
                    },
                )
            }

            initial @ '0' => {
                if self.source.current() == Some('x') {
                    self.source.next();
                    let number = self.read_hex_number();
                    self.create(start_position, number)
                } else {
                    let number = self.read_number(initial.to_string());
                    self.create(start_position, number)
                }
            }

            initial @ ('1'..='9') => {
                let number = self.read_number(initial.to_string());
                self.create(start_position, number)
            }

            quote @ ('"' | '\'') => {
                let string = self.read_string(quote);
                self.create(start_position, string)
            }

            '=' => {
                if self.source.consume('=') {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::TwoEqual,
                        },
                    )
                } else {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::Equal,
                        },
                    )
                }
            }

            '~' => {
                if self.source.consume('=') {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::TildeEqual,
                        },
                    )
                } else {
                    Some(LexerResult::Fatal(vec![TokenizerError {
                        error: TokenizerErrorType::InvalidSymbol("~".to_owned()),
                        position: start_position,
                    }]))
                }
            }

            '\n' => Some(LexerResult::Ok(Token {
                token_type: TokenType::Whitespace {
                    characters: ShortString::from("\n"),
                },
                start_position,
                end_position: Position {
                    bytes: start_position.bytes() + 1,
                    ..start_position
                },
            })),

            '(' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::LeftParen,
                },
            ),

            ')' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::RightParen,
                },
            ),

            '[' => {
                let start_lexer_position = self.source.lexer_position;

                if self.source.current() == Some('[') || self.source.current() == Some('=') {
                    match self.read_multi_line_body() {
                        Ok((blocks, string)) => self.create(
                            start_position,
                            TokenType::StringLiteral {
                                literal: string.into(),
                                multi_line: Some(blocks),
                                quote_type: StringLiteralQuoteType::Brackets,
                            },
                        ),

                        Err(_) => {
                            // Reset back, parse the one `[`, and let the rest be parsed again
                            self.source.lexer_position = start_lexer_position;

                            self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::LeftBracket,
                                },
                            )
                        }
                    }
                } else {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::LeftBracket,
                        },
                    )
                }
            }

            ']' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::RightBracket,
                },
            ),

            ':' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Colon,
                },
            ),

            ',' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Comma,
                },
            ),

            '+' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Plus,
                },
            ),

            '*' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Star,
                },
            ),

            '/' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Slash,
                },
            ),

            '%' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Percent,
                },
            ),

            '^' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Caret,
                },
            ),

            '#' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Hash,
                },
            ),

            '<' => {
                if self.source.consume('=') {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::LessThanEqual,
                        },
                    )
                } else {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::LessThan,
                        },
                    )
                }
            }

            '>' => {
                if self.source.consume('=') {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::GreaterThanEqual,
                        },
                    )
                } else {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::GreaterThan,
                        },
                    )
                }
            }

            '{' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::LeftBrace,
                },
            ),

            '}' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::RightBrace,
                },
            ),

            '.' => {
                if self.source.consume('.') {
                    if self.source.consume('.') {
                        self.create(
                            start_position,
                            TokenType::Symbol {
                                symbol: Symbol::Ellipse,
                            },
                        )
                    } else {
                        self.create(
                            start_position,
                            TokenType::Symbol {
                                symbol: Symbol::TwoDots,
                            },
                        )
                    }
                } else if matches!(self.source.current(), Some('0'..='9')) {
                    let number = self.read_number(".".to_string());
                    self.create(start_position, number)
                } else {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::Dot,
                        },
                    )
                }
            }

            '-' => {
                if self.source.consume('-') {
                    let comment = self.read_comment();
                    self.create(start_position, comment)
                } else {
                    self.create(
                        start_position,
                        TokenType::Symbol {
                            symbol: Symbol::Minus,
                        },
                    )
                }
            }

            ';' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Semicolon,
                },
            ),

            unknown_char => Some(LexerResult::Fatal(vec![TokenizerError {
                error: TokenizerErrorType::UnexpectedToken(unknown_char),
                position: self.source.position(),
            }])),
        }
    }

    fn read_number(&mut self, mut number: String) -> TokenType {
        let mut hit_decimal = false;

        while let Some(next) = self.source.current() {
            if matches!(next, '0'..='9') {
                number.push(self.source.next().expect("peeked, but no next"));
            } else if matches!(next, '.') {
                if hit_decimal {
                    todo!("error: multiple decimal points in number")
                }

                hit_decimal = true;
                number.push(self.source.next().expect("peeked, but no next"));
            } else if matches!(next, 'e' | 'E') {
                number.push(self.source.next().expect("peeked, but no next"));

                let next = self.source.current();
                if matches!(next, Some('+') | Some('-')) {
                    number.push(self.source.next().expect("peeked, but no next"));
                }

                if !matches!(self.source.current(), Some('0'..='9')) {
                    todo!("error: expected digit after exponent (e.g. 2e+3), need to be able to put an error in and then give a fallible number-ish")
                }

                while let Some(next) = self.source.current() {
                    if matches!(next, '0'..='9') {
                        number.push(self.source.next().expect("peeked, but no next"));
                    } else {
                        break;
                    }
                }

                break;
            } else {
                break;
            }
        }

        TokenType::Number {
            text: ShortString::from(number),
        }
    }

    fn read_hex_number(&mut self) -> TokenType {
        let mut number = "0x".to_string();

        while let Some(next) = self.source.current() {
            if matches!(next, '0'..='9' | 'a'..='f' | 'A'..='F') {
                number.push(self.source.next().expect("peeked, but no next"));
            } else {
                break;
            }
        }

        if number.len() == 2 {
            todo!("error: expected hex digit after 0x")
        }

        TokenType::Number {
            text: ShortString::from(number),
        }
    }

    fn read_string(&mut self, quote: char) -> TokenType {
        let quote_type = match quote {
            '"' => StringLiteralQuoteType::Double,
            '\'' => StringLiteralQuoteType::Single,
            _ => unreachable!(),
        };

        let mut literal = String::new();

        let mut escape = false;
        loop {
            let next = match self.source.next() {
                Some(next) => next,
                None => {
                    // return TokenType::StringLiteral {
                    //     literal: literal.into(),
                    //     multi_line: None,
                    //     quote_type,
                    // }
                    todo!("this should return the string literal, and error")
                }
            };

            match (escape, next) {
                (true, ..) => {
                    escape = false;
                    literal.push(next);
                }

                (false, '\\') => {
                    escape = true;
                    literal.push('\\');
                }

                (false, '\n' | '\r') => {
                    // return TokenType::StringLiteral {
                    //     literal: literal.into(),
                    //     multi_line: None,
                    //     quote_type,
                    // }
                    todo!("this should return the string literal, and error")
                }

                (false, ..) if next == quote => {
                    return TokenType::StringLiteral {
                        literal: literal.into(),
                        multi_line: None,
                        quote_type,
                    };
                }

                (false, ..) => {
                    literal.push(next);
                }
            }
        }
    }

    // rewrite todo: single-line-comment-6 had its tokens changed
    fn read_comment(&mut self) -> TokenType {
        let mut comment = String::new();

        if self.source.consume('[') {
            match self.read_multi_line_body() {
                Ok((blocks, body)) => {
                    return TokenType::MultiLineComment {
                        blocks,
                        comment: body.into(),
                    }
                }

                Err(blocks) => {
                    comment.push('[');

                    for _ in 0..blocks {
                        comment.push('=');
                    }
                }
            }
        }

        let mut position_before_new_line = self.source.lexer_position;

        while let Some(next) = self.source.next() {
            if next == '\n' {
                break;
            }

            comment.push(next);
            position_before_new_line = self.source.lexer_position;
        }

        self.source.lexer_position = position_before_new_line;

        TokenType::SingleLineComment {
            comment: comment.into(),
        }
    }

    fn read_multi_line_body(&mut self) -> Result<(usize, String), usize> {
        let mut blocks = 0;
        while self.source.consume('=') {
            blocks += 1;
        }

        if !self.source.consume('[') {
            return Err(blocks);
        }

        let mut body = String::new();

        'read_comment_char: loop {
            let next = match self.source.next() {
                Some(next) => next,
                None => {
                    todo!("error: expected ']' to end multi-line comment")
                }
            };

            if next == ']' {
                let mut equal_signs = 0;

                while equal_signs < blocks {
                    if !self.source.consume('=') {
                        body.push(']');

                        for _ in 0..equal_signs {
                            body.push('=');
                        }

                        continue 'read_comment_char;
                    }

                    equal_signs += 1;
                }

                if self.source.consume(']') {
                    break;
                }

                body.push(']');
                for _ in 0..equal_signs {
                    body.push('=');
                }

                continue;
            }

            body.push(next);
        }

        Ok((blocks, body))
    }
}

fn is_identifier_start(character: char) -> bool {
    matches!(character, 'a'..='z' | 'A'..='Z' | '_')
}

pub struct LexerSource {
    source: Vec<char>,
    pub lexer_position: LexerPosition,
}

impl LexerSource {
    fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            lexer_position: LexerPosition::new(),
        }
    }

    fn current(&self) -> Option<char> {
        self.source.get(self.lexer_position.index).copied()
    }

    fn next(&mut self) -> Option<char> {
        let next = self.current()?;

        if next == '\n' {
            self.lexer_position.position.line += 1;
            self.lexer_position.position.character = 1;
        } else {
            self.lexer_position.position.character += 1;
        }

        self.lexer_position.position.bytes += next.len_utf8();
        self.lexer_position.index += 1;

        Some(next)
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.lexer_position.index + 1).copied()
    }

    fn consume(&mut self, character: char) -> bool {
        if self.current() == Some(character) {
            self.next();
            true
        } else {
            false
        }
    }

    fn position(&self) -> Position {
        self.lexer_position.position
    }
}

#[derive(Clone, Copy)]
pub struct LexerPosition {
    position: Position,
    pub index: usize,
}

impl LexerPosition {
    fn new() -> Self {
        Self {
            position: Position {
                line: 1,
                character: 1,
                bytes: 0,
            },
            index: 0,
        }
    }
}

#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub enum LexerResult<T> {
    Ok(T),
    Fatal(Vec<TokenizerError>),
    Recovered(T, Vec<TokenizerError>),
}

impl<T: std::fmt::Debug> LexerResult<T> {
    pub fn new(value: T, errors: Vec<TokenizerError>) -> Self {
        if errors.is_empty() {
            Self::Ok(value)
        } else {
            Self::Recovered(value, errors)
        }
    }

    pub fn unwrap(self) -> T {
        match self {
            Self::Ok(value) => value,
            _ => panic!("expected ok, got {self:#?}"),
        }
    }

    pub fn unwrap_errors(self) -> Vec<TokenizerError> {
        match self {
            Self::Fatal(errors) | Self::Recovered(_, errors) => errors,
            _ => panic!("expected fatal error, got {self:#?}"),
        }
    }

    pub fn errors(self) -> Vec<TokenizerError> {
        match self {
            Self::Recovered(_, errors) => errors,
            _ => Vec::new(),
        }
    }
}
