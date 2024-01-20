use std::borrow::Cow;

use crate::tokenizer::{Lexer, LexerResult, Symbol, TokenKind, TokenReference};

use super::{parsers::parse_block, Ast, Block, LuaVersion};

pub struct ParserState {
    errors: Vec<crate::Error>,
    lexer: Lexer,
    // Unused with no features enabled
    #[allow(unused)]
    lua_version: LuaVersion,
}

impl ParserState {
    pub fn new(lexer: Lexer) -> Self {
        Self {
            errors: Vec::new(),
            lua_version: lexer.lua_version,
            lexer,
        }
    }

    // Unused with no features enabled
    #[allow(unused)]
    pub fn lua_version(&self) -> LuaVersion {
        self.lua_version
    }

    pub fn current(&self) -> Result<&TokenReference, ()> {
        match self.lexer.current() {
            Some(LexerResult::Ok(token) | LexerResult::Recovered(token, _)) => Ok(token),
            Some(LexerResult::Fatal(_)) => Err(()),
            None => unreachable!("current() called past EOF"),
        }
    }

    pub fn peek(&self) -> Result<&TokenReference, ()> {
        match self.lexer.peek() {
            Some(LexerResult::Ok(token) | LexerResult::Recovered(token, _)) => Ok(token),
            Some(LexerResult::Fatal(_)) => Err(()),
            None => unreachable!("peek() called past EOF"),
        }
    }

    pub fn consume(&mut self) -> ParserResult<TokenReference> {
        let token = self.lexer.consume();

        match token {
            Some(LexerResult::Ok(token)) => ParserResult::Value(token),

            Some(LexerResult::Recovered(token, errors)) => {
                for error in errors {
                    self.errors.push(crate::Error::TokenizerError(error));
                }

                ParserResult::Value(token)
            }

            Some(LexerResult::Fatal(errors)) => {
                for error in errors {
                    self.errors.push(crate::Error::TokenizerError(error));
                }

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

    pub fn require_with_reference_range(
        &mut self,
        symbol: Symbol,
        error: impl MaybeLazyString,
        start_token: &TokenReference,
        end_token: &TokenReference,
    ) -> Option<TokenReference> {
        match self.current() {
            Ok(token) => {
                if token.is_symbol(symbol) {
                    Some(self.consume().unwrap())
                } else {
                    self.token_error_ranged(token.clone(), error.to_str(), start_token, end_token);
                    None
                }
            }

            Err(()) => None,
        }
    }

    pub fn require_with_reference_range_callback(
        &mut self,
        symbol: Symbol,
        error: impl MaybeLazyString,
        tokens: impl FnOnce() -> (TokenReference, TokenReference),
    ) -> Option<TokenReference> {
        match self.current() {
            Ok(token) => {
                if token.is_symbol(symbol) {
                    Some(self.consume().unwrap())
                } else {
                    let (start_token, end_token) = tokens();

                    self.token_error_ranged(
                        token.clone(),
                        error.to_str(),
                        &start_token,
                        &end_token,
                    );

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
        self.errors
            .push(crate::Error::AstError(crate::ast::AstError {
                token: token_reference.token,
                additional: error.into(),
                range: None,
            }));
    }

    // This takes start_token and end_token as owned references because otherwise, we tend to stack an immutable over mutable borrow.
    pub fn token_error_ranged<S: Into<Cow<'static, str>>>(
        &mut self,
        token_reference: TokenReference,
        error: S,
        start_token: &TokenReference,
        end_token: &TokenReference,
    ) {
        self.errors
            .push(crate::Error::AstError(crate::ast::AstError {
                token: token_reference.token,
                additional: error.into(),
                range: Some((start_token.start_position(), end_token.end_position())),
            }));
    }
}

pub trait MaybeLazyString {
    fn to_str(self) -> Cow<'static, str>;
}

impl MaybeLazyString for &'static str {
    fn to_str(self) -> Cow<'static, str> {
        Cow::Borrowed(self)
    }
}

impl<F: FnOnce() -> String> MaybeLazyString for F {
    fn to_str(self) -> Cow<'static, str> {
        Cow::Owned(self())
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

/// A produced [`Ast`](crate::ast::Ast), along with any errors found during parsing.
/// This Ast may not be exactly the same as the input code, as reconstruction may have occurred.
/// For more information, read the documentation for [`parse_fallible`](crate::parse_fallible).
pub struct AstResult {
    ast: Ast,
    errors: Vec<crate::Error>,
}

impl AstResult {
    /// Returns a reference to the [`Ast`](crate::ast::Ast) that was parsed.
    /// If there were any errors, this will not be exactly the same,
    /// as reconstruction will have occurred.
    /// For more information, read the documentation for [`parse_fallible`](crate::parse_fallible).
    pub fn ast(&self) -> &Ast {
        &self.ast
    }

    /// Consumes the [`Ast`](crate::ast::Ast) that was parsed.
    /// If there were any errors, this will not be exactly the same,
    /// as reconstruction will have occurred.
    /// For more information, read the documentation for [`parse_fallible`](crate::parse_fallible).
    pub fn into_ast(self) -> Ast {
        self.ast
    }

    /// Returns all errors that occurred during parsing.
    pub fn errors(&self) -> &[crate::Error] {
        &self.errors
    }

    pub(crate) fn parse_fallible(code: &str, lua_version: LuaVersion) -> Self {
        const UNEXPECTED_TOKEN_ERROR: &str = "unexpected token, this needs to be a statement";

        let lexer = Lexer::new(code, lua_version);
        let mut parser_state = ParserState::new(lexer);

        let mut block = match parse_block(&mut parser_state) {
            ParserResult::Value(block) => block,
            _ => Block::new(),
        };

        loop {
            match parser_state.lexer.current() {
                Some(LexerResult::Ok(token)) if token.token_kind() == TokenKind::Eof => {
                    break;
                }

                Some(LexerResult::Ok(_) | LexerResult::Recovered(_, _)) => {
                    if let ParserResult::Value(new_block) = parse_block(&mut parser_state) {
                        if new_block.stmts.is_empty() {
                            if let Ok(token) = parser_state.current() {
                                if token.token_kind() == TokenKind::Eof {
                                    break;
                                }
                            }

                            match parser_state.consume() {
                                ParserResult::Value(token) => {
                                    if let Some(crate::Error::AstError(crate::ast::AstError {
                                        additional,
                                        ..
                                    })) = parser_state.errors.last()
                                    {
                                        if additional == UNEXPECTED_TOKEN_ERROR {
                                            continue;
                                        }
                                    }

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

                Some(LexerResult::Fatal(_)) => {
                    for error in parser_state.lexer.consume().unwrap().unwrap_errors() {
                        parser_state
                            .errors
                            .push(crate::Error::TokenizerError(error));
                    }
                }

                None => break,
            }
        }

        let eof = match parser_state.lexer.consume().unwrap() {
            LexerResult::Ok(token) => token,

            LexerResult::Recovered(token, errors) => {
                for error in errors {
                    parser_state
                        .errors
                        .push(crate::Error::TokenizerError(error));
                }

                token
            }

            LexerResult::Fatal(error) => unreachable!("error: {error:?}"),
        };

        debug_assert_eq!(eof.token_kind(), TokenKind::Eof);

        Self {
            ast: Ast { nodes: block, eof },
            errors: parser_state.errors,
        }
    }

    /// Consumes this AstResult, returning the [`Ast`](crate::ast::Ast) that was parsed.
    pub fn into_result(self) -> Result<Ast, Vec<crate::Error>> {
        self.into()
    }
}

impl From<AstResult> for Result<Ast, Vec<crate::Error>> {
    fn from(ast_result: AstResult) -> Self {
        if ast_result.errors.is_empty() {
            Ok(ast_result.ast)
        } else {
            Err(ast_result.errors)
        }
    }
}
