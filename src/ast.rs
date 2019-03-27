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
            if let Some((new_state, node)) = self.0.parse(new_state) {
                state = new_state;
                nodes.push(node);
            } else {
                if self.2 {
                    state = new_state;
                    break;
                }

                break;
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
    Token<'a>,
    |this: &ParseSymbol, state: ParserState<'a>| {
        let expecting = TokenType::Symbol { symbol: this.0 };
        let token = state.peek()?;

        if token.token_type == expecting {
            // TODO: remove clone
            Some((state.advance()?, token.clone()))
        } else {
            None
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParseNumber;

define_parser!(ParseNumber, Token<'a>, |_, state: ParserState<'a>| {
    let token = state.peek()?;
    if let TokenType::Number { .. } = token.token_type {
        // TODO: remove clone
        Some((state.advance()?, token.clone()))
    } else {
        None
    }
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParseStringLiteral;

define_parser!(
    ParseStringLiteral,
    Token<'a>,
    |_, state: ParserState<'a>| {
        let token = state.peek()?;
        if let TokenType::StringLiteral { .. } = token.token_type {
            // TODO: remove clone
            Some((state.advance()?, token.clone()))
        } else {
            None
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Block<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub stmts: Vec<Stmt<'a>>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub last_stmt: Option<LastStmt<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseBlock;
define_parser!(ParseBlock, Block<'a>, |_, state| {
    let (state, stmts) = ZeroOrMore(ParseStmt).parse(state)?;

    if let Some((state, last_stmt)) = ParseLastStmt.parse(state) {
        Some((
            state,
            Block {
                stmts,
                last_stmt: Some(last_stmt),
            },
        ))
    } else {
        Some((
            state,
            Block {
                stmts,
                last_stmt: None,
            },
        ))
    }
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LastStmt<'a> {
    Break,
    #[cfg_attr(feature = "serde", serde(borrow))]
    Return(Vec<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseLastStmt;
define_parser!(
    ParseLastStmt,
    LastStmt<'a>,
    |_, state| if let Some((state, _)) = ParseSymbol(Symbol::Return).parse(state) {
        let (state, returns) =
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state)?;
        Some((state, LastStmt::Return(returns)))
    } else if let Some((state, _)) = ParseSymbol(Symbol::Break).parse(state) {
        Some((state, LastStmt::Break))
    } else {
        None
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Field<'a> {
    ExpressionKey {
        #[cfg_attr(feature = "serde", serde(borrow))]
        key: Box<Expression<'a>>,
        value: Box<Expression<'a>>,
    },

    NameKey {
        #[cfg_attr(feature = "serde", serde(borrow))]
        key: Box<Token<'a>>,
        value: Box<Expression<'a>>,
    },

    #[cfg_attr(feature = "serde", serde(borrow))]
    NoKey(Expression<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseField;
define_parser!(ParseField, Field<'a>, |_, state| if let Some((state, _)) =
    ParseSymbol(Symbol::LeftBracket).parse(state)
{
    let (state, key) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::RightBracket).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, value) = ParseExpression.parse(state)?;
    let (key, value) = (Box::new(key), Box::new(value));
    Some((state, Field::ExpressionKey { key, value }))
} else if let Some((state, key)) = ParseIdentifier.parse(state) {
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, value) = ParseExpression.parse(state)?;
    let (key, value) = (Box::new(key), Box::new(value));
    Some((state, Field::NameKey { key, value }))
} else if let Some((state, expr)) = ParseExpression.parse(state) {
    Some((state, Field::NoKey(expr)))
} else {
    None
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TableConstructor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    fields: Vec<(Field<'a>, Option<Token<'a>>)>,
}

struct ParseTableConstructor;
define_parser!(ParseTableConstructor, TableConstructor<'a>, |_, state| {
    let (mut state, _) = ParseSymbol(Symbol::LeftBrace).parse(state)?;
    let mut fields = Vec::new();

    // TODO: remove clone
    while let Some((new_state, field)) = ParseField.parse(state) {
        let field_sep = if let Some((new_state, _)) = ParseSymbol(Symbol::Comma).parse(new_state) {
            state = new_state;
            Some(state.peek()?.clone())
        } else if let Some((new_state, _)) = ParseSymbol(Symbol::Semicolon).parse(state) {
            state = new_state;
            Some(state.peek()?.clone())
        } else {
            state = new_state;
            None
        };

        let is_none = field_sep.is_none();
        fields.push((field, field_sep));
        if is_none {
            break;
        }
    }

    let (state, _) = ParseSymbol(Symbol::RightBrace).parse(state)?;
    Some((state, TableConstructor { fields }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Expression<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub value: Value<'a>,
    pub binop: Option<(BinOp<'a>, Box<Expression<'a>>)>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(ParseExpression, Expression<'a>, |_, state| {
    let (state, value) = ParseValue.parse(state)?;

    let (state, binop) = if let Some((state, binop)) = ParseBinOp.parse(state) {
        let (state, expression) = ParseExpression.parse(state)?;
        (state, Some((binop, Box::new(expression))))
    } else {
        (state, None)
    };

    Some((state, Expression { value, binop }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Value<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Function(FuncBody<'a>),
    FunctionCall(Box<FunctionCall<'a>>),
    TableConstructor(Box<TableConstructor<'a>>),
    Number(Token<'a>),
    String(Token<'a>),
    Symbol(Token<'a>),
    Var(Box<Var<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseValue;
define_parser!(
    ParseValue,
    Value<'a>,
    |_, state: ParserState<'a>| parse_first_of!(state, {
        ParseSymbol(Symbol::Nil) => Value::Symbol,
        ParseSymbol(Symbol::False) => Value::Symbol,
        ParseSymbol(Symbol::True) => Value::Symbol,
        ParseNumber => Value::Number,
        ParseStringLiteral => Value::String,
        ParseSymbol(Symbol::Ellipse) => Value::Symbol,
        ParseFunction => Value::Function,
        ParseTableConstructor => Value::TableConstructor,
        ParseFunctionCall => Value::FunctionCall,
        ParseVar => Value::Var,
    })
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Assignment(Assignment<'a>),
    Do(Block<'a>),
    LocalAssignment(LocalAssignment<'a>),
    LocalFunction(LocalFunction<'a>),
    FunctionCall(Box<FunctionCall<'a>>),
    NumericFor(Box<NumericFor<'a>>),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseStmt;
define_parser!(ParseStmt, Stmt<'a>, |_, state| parse_first_of!(state, {
    ParseAssignment => Stmt::Assignment,
    ParseFunctionCall => Stmt::FunctionCall,
    ParseDo => Stmt::Do,
    ParseNumericFor => Stmt::NumericFor,
    ParseLocalFunction => Stmt::LocalFunction,
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
pub struct NumericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    index_variable: Token<'a>,
    start: Expression<'a>,
    end: Expression<'a>,
    limit: Option<Expression<'a>>,
    block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::For).parse(state)?;
    let (state, index_variable) = ParseIdentifier.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, start) = ParseExpression.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Comma).parse(state)?;
    let (state, end) = ParseExpression.parse(state)?;
    let (state, limit) = if let Some((state, _)) = ParseSymbol(Symbol::Comma).parse(state) {
        let (state, expression) = ParseExpression.parse(state)?;
        (state, Some(expression))
    } else {
        (state, None)
    };
    let (state, _) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, block) = ParseBlock.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

    Some((
        state,
        NumericFor {
            index_variable,
            start,
            end,
            limit,
            block,
        },
    ))
});

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
pub struct FuncBody<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub parameters: Vec<Parameter<'a>>,
    pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseFuncBody;
define_parser!(ParseFuncBody, FuncBody<'a>, |_, state| {
    let (mut state, _) = ParseSymbol(Symbol::LeftParen).parse(state)?;
    let mut parameters = Vec::new();

    if let Some((new_state, names)) = OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state) {
        state = new_state;
        parameters.extend(names.into_iter().map(Parameter::Name));

        if let Some((new_state, _)) = ParseSymbol(Symbol::Comma).parse(state) {
            if let Some((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(new_state) {
                state = new_state;
                parameters.push(Parameter::Ellipse(ellipse));
            }
        }
    } else if let Some((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
        state = new_state;
        parameters.push(Parameter::Ellipse(ellipse));
    }

    let (state, _) = ParseSymbol(Symbol::RightParen).parse(state)?;
    let (state, block) = ParseBlock.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;
    Some((state, FuncBody {
        parameters,
        block,
    }))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunction;
define_parser!(ParseFunction, FuncBody<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Function).parse(state)?;
    ParseFuncBody.parse(state)
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Parameter<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Ellipse(Token<'a>),
    Name(Token<'a>),
}

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
pub struct LocalFunction<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: Token<'a>,
    func_body: FuncBody<'a>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalFunction;
define_parser!(ParseLocalFunction, LocalFunction<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Local).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Function).parse(state)?;
    let (state, name) = ParseIdentifier.parse(state)?;
    let (state, func_body) = ParseFuncBody.parse(state)?;
    Some((state, LocalFunction {
        name,
        func_body,
    }))
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
struct ParseDo;
define_parser!(ParseDo, Block<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Do).parse(state)?;
    let (state, block) = ParseBlock.parse(state)?;
    let (state, _) = ParseSymbol(Symbol::End).parse(state)?;

    Some((state, block))
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

macro_rules! make_bin_op {
    ($($binop:ident,)+) => {
        #[derive(Clone, Debug, PartialEq)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        pub enum BinOp<'a> {
            #[cfg_attr(feature = "serde", serde(borrow))]
            $(
                $binop(Token<'a>),
            )+
        }

        #[derive(Clone, Debug, PartialEq)]
        struct ParseBinOp;
        define_parser!(ParseBinOp, BinOp<'a>, |_, state| {
            $(
                if let Some((state, _)) = ParseSymbol(Symbol::$binop).parse(state) {
                    // TODO: remove clone()
                    return Some((state, BinOp::$binop(state.peek()?.clone())))
                }
            )+

            None
        });
    };
}

make_bin_op!(
    And,
    Caret,
    GreaterThan,
    GreaterThanEqual,
    LessThan,
    LessThanEqual,
    Minus,
    Or,
    Percent,
    Plus,
    Slash,
    Star,
    TildeEqual,
    TwoDots,
    TwoEqual,
);

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
            return Ok(Block {
                stmts: Vec::new(),
                last_stmt: None,
            });
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
                .expect("OneOrMore failed");

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
