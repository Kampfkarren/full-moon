use crate::{tokenizer::StringLiteralQuoteType, ShortString};

use super::{
    Position, Symbol, Token, TokenReference, TokenType, TokenizerError, TokenizerErrorType,
};

pub struct Lexer {
    source: LexerSource,
    sent_eof: bool,

    // rewrite todo: maybe an array if we need more lookahead
    next_token: Option<Result<TokenReference, TokenizerError>>,
    peek_token: Option<Result<TokenReference, TokenizerError>>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut lexer = Self {
            source: LexerSource::new(source),
            sent_eof: false,

            next_token: None,
            peek_token: None,
        };

        lexer.next_token = lexer.process_next_with_trivia();
        lexer.peek_token = lexer.process_next_with_trivia();

        lexer
    }

    pub fn current(&self) -> Option<Result<&TokenReference, &TokenizerError>> {
        self.next_token.as_ref().map(Result::as_ref)
    }

    pub fn peek(&self) -> Option<Result<&TokenReference, &TokenizerError>> {
        self.peek_token.as_ref().map(Result::as_ref)
    }

    pub fn consume(&mut self) -> Option<Result<TokenReference, TokenizerError>> {
        let next = self.next_token.take()?;
        self.next_token = self.peek_token.take();
        self.peek_token = self.process_next_with_trivia();
        Some(next)
    }

    pub fn collect(self) -> Result<Vec<Token>, TokenizerError> {
        let mut tokens = Vec::new();
        let mut lexer = self;

        while let Some(token_reference) = lexer.consume() {
            let mut token_reference = token_reference?;

            tokens.append(&mut token_reference.leading_trivia);
            tokens.push(token_reference.token);
            tokens.append(&mut token_reference.trailing_trivia);
        }

        Ok(tokens)
    }

    fn create(
        &self,
        start_position: Position,
        token_type: TokenType,
    ) -> Option<Result<Token, TokenizerError>> {
        Some(Ok(Token {
            token_type,
            start_position,
            end_position: self.source.position(),
        }))
    }

    fn process_next_with_trivia(&mut self) -> Option<Result<TokenReference, TokenizerError>> {
        let mut leading_trivia = Vec::new();

        let nontrivial_token = loop {
            match self.process_next()? {
                Ok(token) if token.token_type().is_trivia() => {
                    leading_trivia.push(token);
                }

                Ok(token) => {
                    break token;
                }

                Err(error) => {
                    return Some(Err(error));
                }
            }
        };

        let mut trailing_trivia = Vec::new();

        loop {
            let sent_eof = self.sent_eof;
            let start_position = self.source.lexer_position;

            match self.process_next() {
                Some(Ok(token)) if token.token_type().is_trivia() => {
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

        Some(Ok(TokenReference {
            token: nontrivial_token,
            leading_trivia,
            trailing_trivia,
        }))
    }

    fn process_next(&mut self) -> Option<Result<Token, TokenizerError>> {
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

            initial @ (' ' | '\t') => {
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

            initial @ ('0'..='9') => {
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

            '\n' => Some(Ok(Token {
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
                if self.source.consume('[') {
                    todo!("multi-line string")
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

            unknown_char => Some(Err(TokenizerError {
                error: TokenizerErrorType::UnexpectedToken(unknown_char),
                position: self.source.position(),
            })),
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

    fn read_comment(&mut self) -> TokenType {
        if self.source.consume('[') {
            let (blocks, body) = self.read_multi_line_body();
            return TokenType::MultiLineComment {
                blocks,
                comment: body.into(),
            };
        }

        let mut comment = String::new();

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

    fn read_multi_line_body(&mut self) -> (usize, String) {
        let mut blocks = 0;
        while self.source.consume('=') {
            blocks += 1;
        }

        if !self.source.consume('[') {
            todo!("error: expected '[' after multi-line comment blocks")
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

        (blocks, body)
    }
}

fn is_identifier_start(character: char) -> bool {
    matches!(character, 'a'..='z' | 'A'..='Z' | '_')
}

struct LexerSource {
    source: Vec<char>,
    lexer_position: LexerPosition,
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
struct LexerPosition {
    position: Position,
    index: usize,
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
