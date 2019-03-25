use crate::tokenizer::{Symbol, Token, TokenType};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

// const FILTER_WHITESPACE: fn(&&Token) -> bool = |token: &&Token| !token.token_type.ignore();

#[derive(Clone, Copy, Debug, PartialEq)]
struct ParserState<'a> {
    index: usize,
    tokens: &'a [Token<'a>],
}

impl<'a> ParserState<'a> {
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
    type Item;

    fn parse(&self, state: ParserState<'a>) -> Option<(ParserState<'a>, Self::Item)>;

    fn parse_new(state: ParserState<'a>) -> Option<(ParserState<'a>, Self::Item)>
    where
        Self: Default,
    {
        Self::default().parse(state)
    }
}

macro_rules! define_parser {
    ($parser:ident, $node:ty, $body:expr) => {
        impl<'a> Parser<'a> for $parser {
            type Item = $node;

            fn parse(&self, state: ParserState<'a>) -> Option<(ParserState<'a>, $node)> {
                $body(self, state)
            }
        }
    };
}

#[derive(Clone, Debug, PartialEq)]
struct ZeroOrMore<P>(P);

impl<'a, P, T> Parser<'a> for ZeroOrMore<P>
where
    P: Parser<'a, Item = T>,
{
    type Item = Vec<T>;

    fn parse(&self, mut state: ParserState<'a>) -> Option<(ParserState<'a>, Vec<T>)> {
        let mut nodes = Vec::new();
        while let Some((new_state, node)) = self.0.parse(state) {
            state = new_state;
            nodes.push(node);
        }
        Some((state, nodes))
    }
}

#[derive(Clone, Debug, PartialEq)]
struct ZeroOrMoreDelimited<ItemParser, Delimiter>(
    ItemParser, // What items to parse, what is actually returned in a vec
    Delimiter,  // Delimiter parser between one item and another
    bool,       // Allow trailing delimiter?
);

impl<'a, ItemParser, Delimiter, T> Parser<'a> for ZeroOrMoreDelimited<ItemParser, Delimiter>
where
    ItemParser: Parser<'a, Item = T>,
    Delimiter: Parser<'a>,
{
    type Item = Vec<T>;

    fn parse(&self, mut state: ParserState<'a>) -> Option<(ParserState<'a>, Vec<T>)> {
        let mut nodes = Vec::new();

        if let Some((new_state, node)) = self.0.parse(state) {
            state = new_state;
            nodes.push(node);
        } else {
            return Some((state, Vec::new()));
        }

        while let Some((new_state, _)) = self.1.parse(state) {
            state = new_state;

            if let Some((new_state, node)) = self.0.parse(state) {
                state = new_state;
                nodes.push(node);
            } else {
                if self.2 {
                    break;
                } else {
                    return None;
                }
            }
        }

        Some((state, nodes))
    }
}

#[derive(Clone, Debug, PartialEq)]
struct OneOrMore<ItemParser, Delimiter>(
    ItemParser, // What items to parse, what is actually returned in a vec
    Delimiter,  // Delimiter parser between one item and another
    bool,       // Allow trailing delimiter?
);

impl<'a, ItemParser: Parser<'a>, Delimiter: Parser<'a>> Parser<'a>
    for OneOrMore<ItemParser, Delimiter>
{
    type Item = Vec<ItemParser::Item>;

    fn parse(&self, state: ParserState<'a>) -> Option<(ParserState<'a>, Vec<ItemParser::Item>)> {
        let mut nodes = Vec::new();
        let (mut state, node) = self.0.parse(state)?;
        nodes.push(node);

        while let Some((new_state, _)) = self.1.parse(state) {
            state = new_state;

            if let Some((new_state, node)) = self.0.parse(state) {
                state = new_state;
                nodes.push(node);
            } else {
                if self.2 {
                    break;
                } else {
                    return None;
                }
            }
        }

        Some((state, nodes))
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct NoDelimiter;

define_parser!(NoDelimiter, (), |_, state: ParserState<'a>| Some((
    state,
    ()
)));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParseSymbol(Symbol);

define_parser!(
    ParseSymbol,
    (),
    |this: &ParseSymbol, state: ParserState<'a>| {
        let expecting = TokenType::Symbol { symbol: this.0 };
        if state.peek()?.token_type == expecting {
            Some((state.advance()?, ()))
        } else {
            None
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Block<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    stmts: Vec<Stmt<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseBlock;
define_parser!(ParseBlock, Block<'a>, |_, state| {
    let (state, stmts) = OneOrMore(ParseStmt, NoDelimiter, true).parse(state)?;
    Some((state, Block { stmts }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Expression<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Number(Token<'a>),
    Token(Token<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(
    ParseExpression,
    Expression<'a>,
    |_, state: ParserState<'a>| {
        let next_token = state.peek()?;
        match &next_token.token_type {
            // TODO: remove clone
            TokenType::Number { .. } => {
                Some((state.advance()?, Expression::Number(next_token.clone())))
            }
            TokenType::Symbol { symbol } => match symbol {
                Symbol::Ellipse | Symbol::False | Symbol::True | Symbol::Nil => {
                    Some((state.advance()?, Expression::Number(next_token.clone())))
                }
                _ => None,
            },
            _ => None,
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    LocalAssignment(LocalAssignment<'a>),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseStmt;
define_parser!(ParseStmt, Stmt<'a>, |_, state| {
    macro_rules! try_stmt {
        ($parser:ident, $type:ident) => {
            if let Some((state, stmt)) = $parser::parse_new(state) {
                return Some((state, Stmt::$type(stmt)));
            }
        };
    };

    try_stmt!(ParseLocalAssignment, LocalAssignment);
    None
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name_list: Vec<Token<'a>>,
    expr_list: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalAssignment;
define_parser!(ParseLocalAssignment, LocalAssignment<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Local).parse(state)?;
    let (state, name_list) =
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, expr_list) = if let Some((state, _)) = ParseSymbol(Symbol::Equal).parse(state) {
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state)?
    } else {
        (state, Vec::new())
    };

    Some((
        state,
        LocalAssignment {
            name_list,
            expr_list,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseIdentifier;
define_parser!(ParseIdentifier, Token<'a>, |_, state: ParserState<'a>| {
    let next_token = state.peek()?;
    match &next_token.token_type {
        // TODO: remove clone
        TokenType::Identifier { .. } => Some((state.advance()?, next_token.clone())),
        _ => None,
    }
});

#[derive(Clone, Debug, PartialEq)]
pub enum AstError<'a> {
    NoEof,
    Empty,
    UnknownToken(&'a Token<'a>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'a> {
    pub nodes: Block<'a>,
    pub tokens: &'a [Token<'a>],
}

pub fn nodes<'a>(tokens: &'a Vec<Token<'a>>) -> Result<Block<'a>, AstError<'a>> {
    if tokens.last().ok_or(AstError::Empty)?.token_type != TokenType::Eof {
        Err(AstError::NoEof)
    } else {
        if tokens.iter().filter(|token| !token.token_type.ignore()).count() == 1 {
            // Entirely comments/whitespace
            return Ok(Block { stmts: Vec::new() });
        }

        let mut state = ParserState { index: 0, tokens };
        // ParserState has to have at least 2 tokens, the last being an EOF, thus unwrap() can't fail
        if state.peek().unwrap().token_type.ignore() {
            state = state.advance().unwrap();
        }

        if let Some((state, block)) = ParseBlock::parse_new(state) {
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::tokenizer::tokens;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_zero_or_more_empty() {
        let tokens = tokens("local x").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            tokens: &tokens,
        };

        let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma)).parse(state).unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 0,
                tokens: &tokens,
            },
        );

        assert_eq!(commas.len(), 0);
    }

    #[test]
    fn test_zero_or_more_exists() {
        let tokens = tokens(",,, , ,\t ,local x").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            tokens: &tokens,
        };

        let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma)).parse(state).unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 9,
                tokens: &tokens,
            },
        );

        assert_eq!(commas.len(), 6);
    }

    #[test]
    fn test_one_or_more_empty() {
        let tokens = tokens("local x").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            tokens: &tokens,
        };

        assert!(
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
                .parse(state)
                .is_none()
        );
    }

    #[test]
    fn test_one_or_more_exists_no_delimiter() {
        let tokens = tokens("end,end, end,\t\tend local").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            tokens: &tokens,
        };

        let (state, commas) =
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
                .parse(state)
                .unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 10,
                tokens: &tokens,
            },
        );

        assert_eq!(commas.len(), 4);
    }

    #[test]
    fn test_one_or_more_exists_with_delimiter() {
        let tokens = tokens("end,end, end,\t\tend, local").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            tokens: &tokens,
        };

        let (state, commas) = OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
            .parse(state)
            .unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 11,
                tokens: &tokens,
            },
        );

        assert_eq!(commas.len(), 4);
    }

    #[test]
    fn test_one_or_more_exists_with_nothing() {
        let tokens = tokens("local").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            tokens: &tokens,
        };

        assert!(
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
                .parse(state)
                .is_none()
        );
    }
}
