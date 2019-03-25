use crate::tokenizer::{Symbol, Token, TokenType};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

// const FILTER_WHITESPACE: fn(&&Token) -> bool = |token: &&Token| !token.token_type.ignore();

#[derive(Clone, Copy, Debug, PartialEq)]
struct ParserState<'a> {
    index: usize,
    tokens: &'a [Token<'a>],
}

impl<'a> ParserState<'a> {
    fn expect_symbol(self, symbol: Symbol) -> Option<ParserState<'a>> {
        let expectation = TokenType::Symbol { symbol };

        if self.peek()?.token_type == expectation {
            self.advance()
        } else {
            None
        }
    }

    fn advance(self) -> Option<ParserState<'a>> {
        let mut state = self;

        loop {
            state = ParserState {
                index: state.index + 1,
                tokens: self.tokens,
            };

            if !state.tokens.get(state.index)?.token_type.ignore() {
                return Some(state);
            }
        }
    }

    fn peek<'b>(self) -> Option<&'b Token<'a>> {
        self.tokens.get(self.index)
    }
}

trait Parser<'a>: Sized {
    fn parse(state: ParserState<'a>) -> Option<(ParserState<'a>, Self)>;
}

macro_rules! define_parser {
    ($ident:ident |$state:ident| $block:block) => {
        impl<'a> Parser<'a> for $ident<'a> {
            fn parse($state: ParserState<'a>) -> Option<(ParserState<'a>, Self)> $block
        }
    };
}

macro_rules! zero_or_more {
    ($state:ident, $parser:ident) => {{
        let mut state = $state;
        let mut nodes = Vec::new();
        while let Some((new_state, node)) = $parser::parse(state) {
            state = new_state;
            nodes.push(node);
        }
        (state, nodes)
    }};
}

macro_rules! one_or_more {
    ($state:ident, $parser:ident) => {{
        let (state, nodes) = zero_or_more!($state, $parser);

        if nodes.is_empty() {
            return None;
        } else {
            (state, nodes)
        }
    }};

    ($state:ident, $parser:ident, $delimiter:ident) => {{
        let mut state = $state;
        let mut nodes = Vec::new();
        while let Some((new_state, node)) = $parser::parse(state) {
            state = new_state;
            nodes.push(node);
            if state.expect_symbol(Symbol::$delimiter).is_none() {
                break;
            }
        }

        if nodes.is_empty() {
            return None;
        }

        (state, nodes)
    }};

    ($state:ident, Token($token:ident), $delimiter:ident) => {{
        let mut state = $state;
        let mut nodes = Vec::new();

        while let Some(next) = state.peek() {
            match next.token_type {
                TokenType::$token { .. } => {
                    nodes.push(next.clone()); // TODO: remove clone()
                    state = state.advance().unwrap();
                    if let Some(new_state) = state.expect_symbol(Symbol::$delimiter) {
                        state = new_state;
                    }
                }

                _ => break,
            };
        }

        if nodes.is_empty() {
            return None;
        }

        (state, nodes)
    }};
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Block<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    stmts: Vec<Stmt<'a>>,
}

define_parser!(
    Block | state | {
        let (state, stmts) = one_or_more!(state, Stmt);
        Some((state, Block { stmts }))
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    LocalAssignment(LocalAssignment<'a>),
}

define_parser!(
    Stmt | state | {
        macro_rules! try_stmt {
            ($name:ident) => {
                if let Some((state, stmt)) = $name::parse(state) {
                    return Some((state, Stmt::$name(stmt)));
                }
            };
        }

        try_stmt!(LocalAssignment);

        None
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name_list: Vec<Token<'a>>,
}

define_parser!(
    LocalAssignment | state | {
        let state = state.expect_symbol(Symbol::Local)?;
        let (state, name_list) = one_or_more!(state, Token(Identifier), Comma);

        Some((state, LocalAssignment { name_list }))
    }
);

#[derive(Clone, Debug, PartialEq)]
pub enum AstError<'a> {
    UnknownToken(&'a Token<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'a> {
    pub nodes: Block<'a>,
    pub tokens: &'a [Token<'a>],
}

pub fn nodes<'a>(tokens: &'a Vec<Token<'a>>) -> Result<Block<'a>, AstError<'a>> {
    if tokens.is_empty() {
        Ok(Block { stmts: Vec::new() })
    } else {
        let state = ParserState { index: 0, tokens };

        if let Some((state, block)) = Block::parse(state) {
            if state.index == tokens.len() - 1 {
                Ok(block)
            } else {
                Err(AstError::UnknownToken(&tokens[state.index]))
            }
        } else {
            Err(AstError::UnknownToken(&tokens[0]))
        }
    }
}
