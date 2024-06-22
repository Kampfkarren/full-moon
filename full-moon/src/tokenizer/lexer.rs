use crate::{ast::LuaVersion, tokenizer::StringLiteralQuoteType, version_switch, ShortString};

use super::{
    Position, Symbol, Token, TokenReference, TokenType, TokenizerError, TokenizerErrorType,
};

#[cfg(feature = "luau")]
use super::{interpolated_strings, InterpolatedStringKind};

/// A lexer, which will produce a stream of tokens from a source string.
/// If you just want to create an [`Ast`](crate::ast::Ast) from a string, you want to use
/// [`parse`](crate::parse) instead.
pub struct Lexer {
    pub(crate) source: LexerSource,
    sent_eof: bool,

    next_token: Option<LexerResult<TokenReference>>,
    peek_token: Option<LexerResult<TokenReference>>,

    #[cfg(feature = "luau")]
    pub(crate) brace_stack: Vec<interpolated_strings::BraceType>,

    /// The Lua version(s) to parse for.
    pub lua_version: LuaVersion,
}

impl Lexer {
    /// Creates a new Lexer from the given source string and Lua version(s).
    pub fn new(source: &str, lua_version: LuaVersion) -> Self {
        let mut lexer = Self::new_lazy(source, lua_version);

        lexer.next_token = lexer.process_first_with_trivia();
        lexer.peek_token = lexer.process_next_with_trivia();

        lexer
    }

    /// Creates a new Lexer from the given source string and Lua version(s), but does not process
    /// the first token.
    pub fn new_lazy(source: &str, lua_version: LuaVersion) -> Self {
        Self {
            source: LexerSource::new(source),
            sent_eof: false,

            next_token: None,
            peek_token: None,

            #[cfg(feature = "luau")]
            brace_stack: Vec::new(),

            lua_version,
        }
    }

    /// Returns the current token.
    pub fn current(&self) -> Option<&LexerResult<TokenReference>> {
        self.next_token.as_ref()
    }

    /// Returns the next token.
    pub fn peek(&self) -> Option<&LexerResult<TokenReference>> {
        self.peek_token.as_ref()
    }

    /// Consumes the current token and returns the next token.
    pub fn consume(&mut self) -> Option<LexerResult<TokenReference>> {
        let next = self.next_token.take()?;
        self.next_token = self.peek_token.take();
        self.peek_token = self.process_next_with_trivia();
        Some(next)
    }

    /// Returns a vector of all tokens left in the source string.
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

    fn create_recovered(
        &self,
        start_position: Position,
        token_type: TokenType,
        errors: Vec<TokenizerError>,
    ) -> Option<LexerResult<Token>> {
        Some(LexerResult::new(
            Token {
                token_type,
                start_position,
                end_position: self.source.position(),
            },
            errors,
        ))
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

    /// Processes and returns the next token in the source string, ignoring trivia.
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
                    if is_identifier_start(next) || next.is_ascii_digit() {
                        identifier.push(self.source.next().expect("peeked, but no next"));
                    } else {
                        break;
                    }
                }

                self.create(
                    start_position,
                    if let Some(symbol) = Symbol::from_str(&identifier, self.lua_version) {
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

                // Handle end_position appropriately: for a newline, we increment the bytes, but
                // do not increment line/char
                let mut end_position = Position {
                    bytes: start_position.bytes() + initial.len_utf8(),
                    line: start_position.line,
                    character: start_position.character + 1,
                };

                while let Some(next) = self.source.current() {
                    if next == ' ' || next == '\t' {
                        end_position.bytes += next.len_utf8();
                        end_position.character += 1;
                        whitespace.push(self.source.next().expect("peeked, but no next"));
                    } else if next == '\n' {
                        end_position.bytes += next.len_utf8();
                        whitespace.push(self.source.next().expect("peeked, but no next"));
                        break;
                    } else if next == '\r' && self.source.peek() == Some('\n') {
                        let carriage_return = self.source.next().expect("peeked, but no next");
                        let new_line = self.source.next().expect("peeked, but no next");
                        end_position.bytes += carriage_return.len_utf8() + new_line.len_utf8();
                        end_position.character += 1;
                        whitespace.push(carriage_return);
                        whitespace.push(new_line);
                        break;
                    } else {
                        break;
                    }
                }

                Some(LexerResult::Ok(Token {
                    token_type: TokenType::Whitespace {
                        characters: ShortString::from(whitespace.as_str()),
                    },
                    start_position,
                    end_position,
                }))
            }

            initial @ '0' => {
                if matches!(self.source.current(), Some('x' | 'X')) {
                    let hex_character = self.source.next().unwrap();
                    self.read_hex_number(hex_character, start_position)
                } else if (self.lua_version.has_luau() || self.lua_version.has_luajit())
                    && matches!(self.source.current(), Some('b' | 'B'))
                {
                    let binary_character = self.source.next().unwrap();
                    self.read_binary_number(binary_character, start_position)
                } else {
                    self.read_number(start_position, initial.to_string())
                }
            }

            initial @ ('1'..='9') => self.read_number(start_position, initial.to_string()),

            quote @ ('"' | '\'') => {
                let (string, recovered) = self.read_string(quote);
                self.create_recovered(
                    start_position,
                    string,
                    if recovered {
                        vec![TokenizerError {
                            error: TokenizerErrorType::UnclosedString,
                            range: (start_position, self.source.position()),
                        }]
                    } else {
                        Vec::new()
                    },
                )
            }

            #[cfg(feature = "luau")]
            '`' if self.lua_version.has_luau() => {
                Some(interpolated_strings::read_interpolated_string_section(
                    self,
                    start_position,
                    InterpolatedStringKind::Begin,
                    InterpolatedStringKind::Simple,
                ))
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
                    version_switch!(self.lua_version, {
                        lua53 => {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::Tilde,
                                },
                            )
                        }
                    });

                    Some(LexerResult::Fatal(vec![TokenizerError {
                        error: TokenizerErrorType::InvalidSymbol("~".to_owned()),
                        range: (start_position, self.source.position()),
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
                        MultiLineBodyResult::Ok { blocks, body } => self.create(
                            start_position,
                            TokenType::StringLiteral {
                                literal: body.into(),
                                multi_line_depth: blocks,
                                quote_type: StringLiteralQuoteType::Brackets,
                            },
                        ),

                        MultiLineBodyResult::Unclosed { blocks, body } => self.create_recovered(
                            start_position,
                            TokenType::StringLiteral {
                                literal: body.into(),
                                multi_line_depth: blocks,
                                quote_type: StringLiteralQuoteType::Brackets,
                            },
                            vec![TokenizerError {
                                error: TokenizerErrorType::UnclosedString,
                                range: (start_position, self.source.position()),
                            }],
                        ),

                        MultiLineBodyResult::NotMultiLine { .. } => {
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

            ':' => {
                version_switch!(self.lua_version, {
                    lua52 | luau | luajit => {
                        if self.source.consume(':') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::TwoColons,
                                },
                            );
                        }
                    }
                });

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::Colon,
                    },
                )
            }

            ',' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Comma,
                },
            ),

            '+' => {
                version_switch!(self.lua_version, {
                    luau => {
                        if self.source.consume('=') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::PlusEqual,
                                },
                            );
                        }
                    }
                });

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::Plus,
                    },
                )
            }

            '*' => {
                version_switch!(self.lua_version, {
                    luau => {
                        if self.source.consume('=') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::StarEqual,
                                },
                            );
                        }
                    }
                });

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::Star,
                    },
                )
            }

            '/' => {
                version_switch!(self.lua_version, {
                    lua53 => {
                        if self.source.current() == Some('/') {
                            self.source.next();
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::DoubleSlash,
                                },
                            );
                        }
                    }
                    luau => {
                        if self.source.consume('/') {
                            if self.source.consume('=') {
                                return self.create(start_position, TokenType::Symbol { symbol: Symbol::DoubleSlashEqual })
                            } else {
                                return self.create(start_position, TokenType::Symbol { symbol: Symbol::DoubleSlash} )
                            }
                        } else if self.source.consume('=') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::SlashEqual,
                                },
                            );
                        }
                    }
                });

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::Slash,
                    },
                )
            }

            '%' => {
                version_switch!(self.lua_version, {
                    luau => {
                        if self.source.consume('=') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::PercentEqual,
                                },
                            );
                        }
                    }
                });

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::Percent,
                    },
                )
            }

            '^' => {
                version_switch!(self.lua_version, {
                    luau => {
                        if self.source.consume('=') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::CaretEqual,
                                },
                            );
                        }
                    }
                });

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::Caret,
                    },
                )
            }

            '#' => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Hash,
                },
            ),

            '<' => {
                version_switch!(self.lua_version, {
                    lua53 => {
                        if self.source.consume('<') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::DoubleLessThan,
                                },
                            );
                        }
                    }
                });

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
                version_switch!(self.lua_version, {
                    lua53 => {
                        if self.source.consume('>') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::DoubleGreaterThan,
                                },
                            );
                        }
                    }
                });

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

            '{' => {
                #[cfg(feature = "luau")]
                if self.lua_version.has_luau() {
                    self.brace_stack
                        .push(interpolated_strings::BraceType::Normal);
                }

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::LeftBrace,
                    },
                )
            }

            '}' => {
                #[cfg(feature = "luau")]
                if self.lua_version.has_luau()
                    && self.brace_stack.pop()
                        == Some(interpolated_strings::BraceType::InterpolatedString)
                {
                    return Some(interpolated_strings::read_interpolated_string_section(
                        self,
                        start_position,
                        InterpolatedStringKind::Middle,
                        InterpolatedStringKind::End,
                    ));
                }

                self.create(
                    start_position,
                    TokenType::Symbol {
                        symbol: Symbol::RightBrace,
                    },
                )
            }

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
                        version_switch!(self.lua_version, {
                            luau => {
                                if self.source.consume('=') {
                                    return self.create(
                                        start_position,
                                        TokenType::Symbol {
                                            symbol: Symbol::TwoDotsEqual,
                                        },
                                    );
                                }
                            }
                        });

                        self.create(
                            start_position,
                            TokenType::Symbol {
                                symbol: Symbol::TwoDots,
                            },
                        )
                    }
                } else if matches!(self.source.current(), Some('0'..='9')) {
                    self.read_number(start_position, ".".to_string())
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
                version_switch!(self.lua_version, {
                    luau => {
                        if self.source.consume('=') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::MinusEqual,
                                },
                            );
                        } else if self.source.consume('>') {
                            return self.create(
                                start_position,
                                TokenType::Symbol {
                                    symbol: Symbol::ThinArrow
                                }
                            );
                        }
                    }
                });

                if self.source.consume('-') {
                    let (token, recovered) = self.read_comment();

                    self.create_recovered(
                        start_position,
                        token,
                        if recovered {
                            vec![TokenizerError {
                                error: TokenizerErrorType::UnclosedComment,
                                range: (start_position, self.source.position()),
                            }]
                        } else {
                            Vec::new()
                        },
                    )
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

            #[cfg(any(feature = "lua53", feature = "luau"))]
            '&' if self.lua_version.has_lua53() || self.lua_version.has_luau() => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Ampersand,
                },
            ),

            #[cfg(any(feature = "lua53", feature = "luau"))]
            '|' if self.lua_version.has_lua53() || self.lua_version.has_luau() => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::Pipe,
                },
            ),

            #[cfg(feature = "luau")]
            '?' if self.lua_version.has_luau() => self.create(
                start_position,
                TokenType::Symbol {
                    symbol: Symbol::QuestionMark,
                },
            ),

            unknown_char => Some(LexerResult::Fatal(vec![TokenizerError {
                error: TokenizerErrorType::UnexpectedToken(unknown_char),
                range: (start_position, self.source.position()),
            }])),
        }
    }

    fn read_number(
        &mut self,
        start_position: Position,
        mut number: String,
    ) -> Option<LexerResult<Token>> {
        let mut hit_decimal = false;

        while let Some(next) = self.source.current() {
            if next.is_ascii_digit() || (self.lua_version.has_luau() && matches!(next, '_')) {
                number.push(self.source.next().expect("peeked, but no next"));
            } else if matches!(next, '.') {
                if hit_decimal {
                    return Some(self.eat_invalid_number(start_position, number));
                }

                hit_decimal = true;
                number.push(self.source.next().expect("peeked, but no next"));
            } else if matches!(next, 'e' | 'E') {
                return self.read_exponent_part(start_position, number);
            } else if self.lua_version.has_luajit()
                && matches!(next, 'u' | 'U' | 'l' | 'L' | 'i' | 'I')
            {
                return self.read_luajit_number_suffix(start_position, number);
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

    fn eat_invalid_number(
        &mut self,
        start_position: Position,
        mut number: String,
    ) -> LexerResult<Token> {
        loop {
            if matches!(self.source.current(), Some(token) if token.is_ascii_whitespace())
                || self.source.current().is_none()
            {
                return LexerResult::new(
                    Token {
                        token_type: TokenType::Number {
                            text: number.into(),
                        },
                        start_position,
                        end_position: self.source.position(),
                    },
                    vec![TokenizerError {
                        error: TokenizerErrorType::InvalidNumber,
                        range: (start_position, self.source.position()),
                    }],
                );
            }

            number.push(self.source.next().expect("peeked, but no next"));
        }
    }

    // Starts from the exponent marker (like 'e')
    fn read_exponent_part(
        &mut self,
        start_position: Position,
        mut number: String,
    ) -> Option<LexerResult<Token>> {
        number.push(self.source.next().expect("peeked, but no next"));

        let next = self.source.current();
        if matches!(next, Some('+') | Some('-')) {
            number.push(self.source.next().expect("peeked, but no next"));
        }

        if !matches!(self.source.current(), Some('0'..='9')) {
            return Some(self.eat_invalid_number(start_position, number));
        }

        while let Some(next) = self.source.current() {
            if next.is_ascii_digit() || (self.lua_version.has_luau() && matches!(next, '_')) {
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

    fn read_hex_number(
        &mut self,
        hex_character: char,
        start_position: Position,
    ) -> Option<LexerResult<Token>> {
        let mut number = String::from_iter(['0', hex_character]);
        let mut hit_decimal = false;

        while let Some(next) = self.source.current() {
            match next {
                '0'..='9' | 'a'..='f' | 'A'..='F' => {
                    number.push(self.source.next().expect("peeked, but no next"));
                }

                '_' if self.lua_version.has_luau() => {
                    number.push(self.source.next().expect("peeked, but no next"));
                }

                '.' if self.lua_version.has_lua52() => {
                    if hit_decimal {
                        return Some(self.eat_invalid_number(start_position, number));
                    }

                    hit_decimal = true;
                    number.push(self.source.next().expect("peeked, but no next"));
                }

                'p' | 'P' if self.lua_version.has_lua52() => {
                    if number.len() == 2 {
                        return Some(self.eat_invalid_number(start_position, number));
                    }

                    return self.read_exponent_part(start_position, number);
                }

                'u' | 'U' | 'l' | 'L' | 'i' | 'I' if self.lua_version.has_luajit() => {
                    return self.read_luajit_number_suffix(start_position, number);
                }

                _ => break,
            }
        }

        if number.len() == 2 {
            return Some(self.eat_invalid_number(start_position, number));
        }

        self.create(
            start_position,
            TokenType::Number {
                text: ShortString::from(number),
            },
        )
    }

    fn read_binary_number(
        &mut self,
        binary_character: char,
        start_position: Position,
    ) -> Option<LexerResult<Token>> {
        debug_assert!(self.lua_version.has_luau() || self.lua_version.has_luajit());

        let mut number = String::from_iter(['0', binary_character]);

        while let Some(next) = self.source.current() {
            match next {
                '0' | '1' | '_' => {
                    number.push(self.source.next().expect("peeked, but no next"));
                }

                'u' | 'U' | 'l' | 'L' | 'i' | 'I' if self.lua_version.has_luajit() => {
                    return self.read_luajit_number_suffix(start_position, number);
                }

                _ => break,
            }
        }

        if number.len() == 2 {
            return Some(self.eat_invalid_number(start_position, number));
        }

        self.create(
            start_position,
            TokenType::Number {
                text: ShortString::from(number),
            },
        )
    }

    fn read_luajit_number_suffix(
        &mut self,
        start_position: Position,
        mut number: String,
    ) -> Option<LexerResult<Token>> {
        debug_assert!(self.lua_version.has_luajit());

        while let Some(next) = self.source.current() {
            if matches!(next, 'u' | 'U') {
                number.push(self.source.next().expect("peeked, but no next"));
                if !matches!(self.source.current(), Some('l' | 'L')) {
                    return Some(self.eat_invalid_number(start_position, number));
                }
            } else if matches!(next, 'l' | 'L') {
                number.push(self.source.next().expect("peeked, but no next"));
                if matches!(self.source.current(), Some('l' | 'L')) {
                    number.push(self.source.next().expect("peeked, but no next"));
                    break;
                } else {
                    return Some(self.eat_invalid_number(start_position, number));
                }
            } else if matches!(next, 'i' | 'I') {
                number.push(self.source.next().expect("peeked, but no next"));
                break;
            } else {
                return Some(self.eat_invalid_number(start_position, number));
            }
        }

        self.create(
            start_position,
            TokenType::Number {
                text: ShortString::from(number),
            },
        )
    }

    // (string, had to be recovered?)
    fn read_string(&mut self, quote: char) -> (TokenType, bool) {
        let quote_type = match quote {
            '"' => StringLiteralQuoteType::Double,
            '\'' => StringLiteralQuoteType::Single,
            _ => unreachable!(),
        };

        let mut literal = String::new();

        let mut escape = false;
        let mut z_escaped = false;

        loop {
            let next = match self.source.next() {
                Some(next) => next,
                None => {
                    return (
                        TokenType::StringLiteral {
                            literal: literal.into(),
                            multi_line_depth: 0,
                            quote_type,
                        },
                        true,
                    )
                }
            };

            match (escape, next) {
                (true, 'z')
                    if self.lua_version.has_lua52()
                        || self.lua_version.has_luau()
                        || self.lua_version.has_luajit() =>
                {
                    escape = false;
                    z_escaped = true;
                    literal.push('z');
                }

                (true, ..) => {
                    escape = false;

                    if self.lua_version.has_lua52() || self.lua_version.has_luau() {
                        z_escaped = true; // support for '\' followed by a new line
                    }

                    literal.push(next);
                }

                (false, '\\') => {
                    escape = true;
                    literal.push('\\');
                }

                (false, '\n' | '\r') if z_escaped => {
                    z_escaped = false;
                    literal.push(next);
                }

                (false, '\n' | '\r') => {
                    return (
                        TokenType::StringLiteral {
                            literal: literal.into(),
                            multi_line_depth: 0,
                            quote_type,
                        },
                        true,
                    )
                }

                (false, ..) if next == quote => {
                    return (
                        TokenType::StringLiteral {
                            literal: literal.into(),
                            multi_line_depth: 0,
                            quote_type,
                        },
                        false,
                    );
                }

                (false, ..) => {
                    literal.push(next);
                }
            }
        }
    }

    // (comment, had to be recovered?)
    fn read_comment(&mut self) -> (TokenType, bool) {
        let mut comment = String::new();

        if self.source.consume('[') {
            match self.read_multi_line_body() {
                MultiLineBodyResult::Ok { blocks, body } => {
                    return (
                        TokenType::MultiLineComment {
                            blocks,
                            comment: body.into(),
                        },
                        false,
                    );
                }

                MultiLineBodyResult::Unclosed { blocks, body } => {
                    return (
                        TokenType::MultiLineComment {
                            blocks,
                            comment: body.into(),
                        },
                        true,
                    );
                }

                MultiLineBodyResult::NotMultiLine { blocks } => {
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

        (
            TokenType::SingleLineComment {
                comment: comment.into(),
            },
            false,
        )
    }

    fn read_multi_line_body(&mut self) -> MultiLineBodyResult {
        let mut blocks = 0;
        while self.source.consume('=') {
            blocks += 1;
        }

        if !self.source.consume('[') {
            return MultiLineBodyResult::NotMultiLine { blocks };
        }

        let mut body = String::new();

        'read_comment_char: loop {
            let next = match self.source.next() {
                Some(next) => next,
                None => return MultiLineBodyResult::Unclosed { blocks, body },
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

        MultiLineBodyResult::Ok { blocks, body }
    }
}

fn is_identifier_start(character: char) -> bool {
    matches!(character, 'a'..='z' | 'A'..='Z' | '_')
}

pub(crate) struct LexerSource {
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

    pub(crate) fn current(&self) -> Option<char> {
        self.source.get(self.lexer_position.index).copied()
    }

    pub(crate) fn next(&mut self) -> Option<char> {
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

    pub(crate) fn peek(&self) -> Option<char> {
        self.source.get(self.lexer_position.index + 1).copied()
    }

    pub(crate) fn consume(&mut self, character: char) -> bool {
        if self.current() == Some(character) {
            self.next();
            true
        } else {
            false
        }
    }

    pub(crate) fn position(&self) -> Position {
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

/// The result of a lexer operation.
#[derive(Debug)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub enum LexerResult<T> {
    /// The lexer operation was successful.
    Ok(T),
    /// The lexer operation was unsuccessful, and could not be recovered.
    Fatal(Vec<TokenizerError>),
    /// The lexer operation was unsuccessful, but some result can be extracted.
    Recovered(T, Vec<TokenizerError>),
}

impl<T: std::fmt::Debug> LexerResult<T> {
    fn new(value: T, errors: Vec<TokenizerError>) -> Self {
        if errors.is_empty() {
            Self::Ok(value)
        } else {
            Self::Recovered(value, errors)
        }
    }

    /// Unwraps the result, panicking if it is not [`LexerResult::Ok`].
    pub fn unwrap(self) -> T {
        match self {
            Self::Ok(value) => value,
            _ => panic!("expected ok, got {self:#?}"),
        }
    }

    /// Unwraps the errors, panicking if it is [`LexerResult::Ok`].
    pub fn unwrap_errors(self) -> Vec<TokenizerError> {
        match self {
            Self::Fatal(errors) | Self::Recovered(_, errors) => errors,
            _ => panic!("expected fatal error, got {self:#?}"),
        }
    }

    /// Returns the errors, if there was any.
    pub fn errors(self) -> Vec<TokenizerError> {
        match self {
            Self::Recovered(_, errors) => errors,
            _ => Vec::new(),
        }
    }
}

enum MultiLineBodyResult {
    Ok { blocks: usize, body: String },
    NotMultiLine { blocks: usize },
    Unclosed { blocks: usize, body: String },
}
