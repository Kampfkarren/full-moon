use std::borrow::Cow;

use crate::tokenizer::{Lexer, Token, TokenKind, TokenReference, TokenType, TokenizerError};

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

    pub fn current(&self) -> ParserResult<&TokenReference> {
        match self.lexer.current() {
            Some(Ok(token)) => ParserResult::Value(token),
            Some(Err(error)) => ParserResult::LexerMoved,
            None => ParserResult::NotFound,
        }
    }

    pub fn peek(&self) -> ParserResult<&TokenReference> {
        match self.lexer.peek() {
            Some(Ok(token)) => ParserResult::Value(token),
            Some(Err(error)) => ParserResult::LexerMoved,
            None => ParserResult::NotFound,
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
        assert_eq!(
            parser_state.lexer.current().unwrap().unwrap().token_kind(),
            TokenKind::Eof
        );

        Self {
            ast: Ast {
                nodes: block,
                eof: parser_state.lexer.consume().unwrap().unwrap(),
            },
            errors: parser_state.errors,
        }
    }
}
