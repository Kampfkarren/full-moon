use crate::ShortString;

use super::{Position, Symbol, Token, TokenType, TokenizerError, TokenizerErrorType};

pub struct Lexer {
    source: LexerSource,
    sent_eof: bool,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        Self {
            source: LexerSource::new(source),
            sent_eof: false,
        }
    }

    fn create(
        &self,
        start_position: Position,
        token_type: TokenType,
    ) -> Option<Result<Token, TokenizerError>> {
        Some(Ok(Token {
            token_type,
            start_position,
            end_position: self.source.position,
        }))
    }
}

fn is_identifier_start(character: char) -> bool {
    matches!(character, 'a'..='z' | 'A'..='Z' | '_')
}

impl Iterator for Lexer {
    type Item = Result<Token, TokenizerError>;

    fn next(&mut self) -> Option<Self::Item> {
        let start_position = self.source.position;

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
                let mut number = String::new();
                number.push(initial);

                while let Some(next) = self.source.current() {
                    if matches!(next, '0'..='9') {
                        number.push(self.source.next().expect("peeked, but no next"));
                    } else {
                        break;
                    }
                }

                self.create(
                    start_position,
                    TokenType::Number {
                        text: ShortString::from(number),
                    },
                )
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

            unknown_char => Some(Err(TokenizerError {
                error: TokenizerErrorType::UnexpectedToken(unknown_char),
                position: self.source.position,
            })),
        }
    }
}

struct LexerSource {
    source: Vec<char>,
    position: Position,
}

impl LexerSource {
    fn new(source: &str) -> Self {
        Self {
            source: source.chars().collect(),
            position: Position {
                line: 1,
                character: 1,
                bytes: 0,
            },
        }
    }

    fn current(&self) -> Option<char> {
        self.source.get(self.position.bytes as usize).copied()
    }

    fn next(&mut self) -> Option<char> {
        let next = self.current()?;

        if next == '\n' {
            self.position.line += 1;
            self.position.character = 1;
        } else {
            self.position.character += 1;
        }

        self.position.bytes += 1;

        Some(next)
    }

    fn peek(&self) -> Option<char> {
        self.source.get(self.position.bytes as usize + 1).copied()
    }

    fn consume(&mut self, character: char) -> bool {
        if self.peek() == Some(character) {
            self.next();
            true
        } else {
            false
        }
    }
}
