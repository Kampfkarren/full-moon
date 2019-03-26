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

macro_rules! parse_first_of {
    ($state:ident, {$($parser:expr => $constructor:expr,)+}) => ({
        $(
            if let Some((state, node)) = $parser.parse($state) {
                return Some((state, $constructor(node.into())));
            }
        )+

        None
    });
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
            } else if self.2 {
                break;
            } else {
                return None;
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
            } else if self.2 {
                break;
            } else {
                return None;
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
    Symbol(Token<'a>),
    Var(Box<Var<'a>>),
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
                    Some((state.advance()?, Expression::Symbol(next_token.clone())))
                }
                _ => None,
            },
            _ => parse_first_of!(state, {
                ParseVar => Expression::Var,
            }),
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Assignment(Assignment<'a>),
    LocalAssignment(LocalAssignment<'a>),
    FunctionCall(FunctionCall<'a>),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseStmt;
define_parser!(ParseStmt, Stmt<'a>, |_, state| parse_first_of!(state, {
    ParseAssignment => Stmt::Assignment,
    ParseFunctionCall => Stmt::FunctionCall,
    ParseLocalAssignment => Stmt::LocalAssignment,
}));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Prefix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Expression(Expression<'a>),
    Name(Token<'a>),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParsePrefix;
define_parser!(
    ParsePrefix,
    Prefix<'a>,
    |_, state| if let Some((state, _)) = ParseSymbol(Symbol::LeftParen).parse(state) {
        let (state, expression) = ParseExpression.parse(state)?;
        let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;
        Some((state, Prefix::Expression(expression)))
    } else if let Some((state, name)) = ParseIdentifier.parse(state) {
        Some((state, Prefix::Name(name)))
    } else {
        None
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Index<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Brackets(Expression<'a>),
    Dot(Token<'a>),
}

struct ParseIndex;
define_parser!(ParseIndex, Index<'a>, |_, state| if let Some((state, _)) =
    ParseSymbol(Symbol::LeftBracket).parse(state)
{
    let (state, expression) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightBracket).parse(state)?;
    Some((state, Index::Brackets(expression)))
} else if let Some((state, _)) = ParseSymbol(Symbol::Dot).parse(state) {
    let (state, name) = ParseIdentifier.parse(state)?;
    Some((state, Index::Dot(name)))
} else {
    None
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum FunctionArgs<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Parentheses(Vec<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionArgs;
define_parser!(
    ParseFunctionArgs,
    FunctionArgs<'a>,
    |_, state| if let Some((state, _)) = ParseSymbol(Symbol::LeftParen).parse(state) {
        let (state, expr_list) =
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state)?;
        let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;
        Some((state, FunctionArgs::Parentheses(expr_list)))
    } else {
        None
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct MethodCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub name: Token<'a>,
    pub args: FunctionArgs<'a>,
}

struct ParseMethodCall;
define_parser!(ParseMethodCall, MethodCall<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Colon).parse(state)?;
    let (state, name) = ParseIdentifier.parse(state)?;
    let (state, args) = ParseFunctionArgs.parse(state)?;
    Some((state, MethodCall { name, args }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Call<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    AnonymousCall(FunctionArgs<'a>),
    MethodCall(MethodCall<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseCall;
define_parser!(ParseCall, Call<'a>, |_, state| parse_first_of!(state, {
    ParseFunctionArgs => Call::AnonymousCall,
    ParseMethodCall => Call::MethodCall,
}));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Suffix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Call(Call<'a>),
    Index(Index<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseSuffix;
define_parser!(ParseSuffix, Suffix<'a>, |_, state| parse_first_of!(state, {
    ParseCall => Suffix::Call,
    ParseIndex => Suffix::Index,
}));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct VarExpression<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    prefix: Prefix<'a>,
    suffixes: Vec<Suffix<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseVarExpression;
define_parser!(ParseVarExpression, VarExpression<'a>, |_, state| {
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

    if let Some(Suffix::Index(_)) = suffixes.last() {
        Some((state, VarExpression { prefix, suffixes }))
    } else {
        None
    }
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Var<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Expression(VarExpression<'a>),
    Name(Token<'a>),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseVar;
define_parser!(ParseVar, Var<'a>, |_, state| parse_first_of!(state, {
    ParseVarExpression => Var::Expression,
    ParseIdentifier => Var::Name,
}));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Assignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    var_list: Vec<Var<'a>>,
    expr_list: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseAssignment;
define_parser!(ParseAssignment, Assignment<'a>, |_, state| {
    let (state, var_list) = OneOrMore(ParseVar, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, expr_list) =
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state)?;

    Some((
        state,
        Assignment {
            var_list,
            expr_list,
        },
    ))
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
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub prefix: Prefix<'a>,
    pub suffixes: Vec<Suffix<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionCall;
define_parser!(ParseFunctionCall, FunctionCall<'a>, |_, state| {
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

    if let Some(Suffix::Call(_)) = suffixes.last() {
        Some((state, FunctionCall { prefix, suffixes }))
    } else {
        None
    }
});

#[derive(Clone, Debug, Default, PartialEq)]
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

pub fn nodes<'a>(tokens: &'a [Token<'a>]) -> Result<Block<'a>, AstError<'a>> {
    if tokens.last().ok_or(AstError::Empty)?.token_type != TokenType::Eof {
        Err(AstError::NoEof)
    } else {
        if tokens
            .iter()
            .filter(|token| !token.token_type.ignore())
            .count()
            == 1
        {
            // Entirely comments/whitespace
            return Ok(Block { stmts: Vec::new() });
        }

        let mut state = ParserState { index: 0, tokens };
        // ParserState has to have at least 2 tokens, the last being an EOF, thus unwrap() can't fail
        if state.peek().unwrap().token_type.ignore() {
            state = state.advance().unwrap();
        }

        if let Some((state, block)) = ParseBlock.parse(state) {
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
