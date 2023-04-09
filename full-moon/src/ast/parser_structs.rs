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
    pub fn todo_errors(&self) -> &[crate::Error] {
        &self.errors
    }

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
        let mut lexer = Lexer::new(code);
        let mut parser_state = ParserState::new(lexer);

        let block = match parse_block(&mut parser_state) {
            ParserResult::Value(block) => block,
            _ => Block::new(),
        };

        // rewrite todo: remove
        if !parser_state.errors.is_empty() {
            panic!("{:#?}", parser_state.errors);
        }

        // rewrite todo: try to keep parsing??
        let last_token = parser_state.lexer.current().unwrap().unwrap();
        if last_token.token_kind() != TokenKind::Eof {
            todo!(
                "didn't finish, last token: {last_token:#?}, errors: {:#?}",
                parser_state.todo_errors()
            );
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
