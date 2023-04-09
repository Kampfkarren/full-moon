use std::borrow::Cow;

use crate::tokenizer::{
    Lexer, Symbol, Token, TokenKind, TokenReference, TokenType, TokenizerError,
};

use super::{parsers::parse_block, Ast, Block};

pub struct ParserState {
    errors: Vec<crate::Error>,
    lexer: Lexer,
}

impl ParserState {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            errors: Vec::new(),
            lexer,
        }
    }

    pub fn current(&self) -> Result<&TokenReference, ()> {
        match self.lexer.current() {
            Some(Ok(token)) => Ok(token),
            Some(Err(error)) => Err(()),
            None => unreachable!("current() called past EOF"),
        }
    }

    pub fn peek(&self) -> Result<&TokenReference, ()> {
        match self.lexer.peek() {
            Some(Ok(token)) => Ok(token),
            Some(Err(error)) => Err(()),
            None => unreachable!("peek() called past EOF"),
        }
    }

    pub fn consume(&mut self) -> ParserResult<TokenReference> {
        let token = self.lexer.consume();

        match token {
            Some(Ok(token)) => ParserResult::Value(token),
            Some(Err(error)) => {
                self.errors.push(crate::Error::TokenizerError(error));
                ParserResult::LexerMoved
            }
            None => ParserResult::NotFound,
        }
    }

    pub fn consume_if(&mut self, symbol: Symbol) -> Option<TokenReference> {
        match self.current() {
            Ok(token) => {
                if token.is_symbol(symbol) {
                    Some(self.consume().unwrap())
                } else {
                    None
                }
            }

            Err(()) => None,
        }
    }

    pub fn require(&mut self, symbol: Symbol, error: &'static str) -> Option<TokenReference> {
        match self.current() {
            Ok(token) => {
                if token.is_symbol(symbol) {
                    Some(self.consume().unwrap())
                } else {
                    self.token_error(token.clone(), error);
                    None
                }
            }

            Err(()) => None,
        }
    }

    pub fn require_with_reference_token(
        &mut self,
        symbol: Symbol,
        error: &'static str,
        reference_token: &TokenReference,
    ) -> Option<TokenReference> {
        match self.current() {
            Ok(token) => {
                if token.is_symbol(symbol) {
                    Some(self.consume().unwrap())
                } else {
                    self.token_error(reference_token.clone(), error);
                    None
                }
            }

            Err(()) => None,
        }
    }

    pub fn token_error<S: Into<Cow<'static, str>>>(
        &mut self,
        token_reference: TokenReference,
        error: S,
    ) {
        self.errors.push(crate::Error::AstError(
            crate::ast::AstError::UnexpectedToken {
                token: token_reference.token,
                additional: Some(error.into()),
            },
        ));
    }

    pub fn current_offset(&self) -> usize {
        self.lexer.source.lexer_position.index
    }
}

#[derive(Debug)]
pub enum ParserResult<T> {
    // This doesn't necessarily mean that there were no errors,
    // because this can sometimes be a recovered value.
    Value(T),

    // Couldn't get any sort of value, but the lexer has moved.
    // This should always come with an error.
    LexerMoved,

    NotFound,
}

impl<T> ParserResult<T> {
    pub fn unwrap(self) -> T {
        match self {
            ParserResult::Value(value) => value,
            ParserResult::LexerMoved => panic!("unwrap() called when value was LexerMoved"),
            ParserResult::NotFound => panic!("unwrap() called when value was NotFound"),
        }
    }
}

pub struct AstResult {
    // rewrite todo: not pub
    pub ast: Ast,
    pub errors: Vec<crate::Error>,
}

impl AstResult {
    pub(crate) fn parse_fallible(code: &str) -> Self {
        const UNEXPECTED_TOKEN_ERROR: &str = "unexpected token, this needs to be a statement";

        let lexer = Lexer::new(code);
        let mut parser_state = ParserState::new(lexer);

        let mut last_lexer_position = parser_state.current_offset();

        let mut block = match parse_block(&mut parser_state) {
            ParserResult::Value(block) => block,
            _ => Block::new(),
        };

        // rewrite todo: try to keep parsing??
        loop {
            match parser_state.lexer.current() {
                Some(Ok(token)) if token.token_kind() == TokenKind::Eof => {
                    break;
                }

                Some(Ok(_)) => {
                    if parser_state.current_offset() == last_lexer_position {
                        match parser_state.consume() {
                            ParserResult::Value(token) => {
                                parser_state.token_error(token, UNEXPECTED_TOKEN_ERROR);
                            }

                            ParserResult::LexerMoved => {}

                            ParserResult::NotFound => unreachable!(),
                        };

                        continue;
                    }

                    assert!(!parser_state.errors.is_empty());

                    if let ParserResult::Value(new_block) = parse_block(&mut parser_state) {
                        if new_block.stmts.is_empty() {
                            match parser_state.consume() {
                                ParserResult::Value(token) => {
                                    parser_state.token_error(token, UNEXPECTED_TOKEN_ERROR);
                                }

                                ParserResult::LexerMoved => {}

                                ParserResult::NotFound => unreachable!(),
                            }

                            continue;
                        }

                        block.merge_blocks(new_block);
                    }
                }

                Some(Err(_)) => {
                    parser_state.errors.push(crate::Error::TokenizerError(
                        parser_state.lexer.consume().unwrap().unwrap_err(),
                    ));
                }

                None => break,
            }
        }

        Self {
            ast: Ast {
                nodes: block,
                eof: parser_state.lexer.consume().unwrap().unwrap(),
            },
            errors: parser_state.errors,
        }
    }
}
