use crate::tokenizer::{Symbol, Token, TokenType};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Clone, Copy, PartialEq)]
pub struct ParserState<'a> {
    index: usize,
    len: usize,
    tokens: *const Token<'a>,
}

impl<'a> ParserState<'a> {
    fn advance(self) -> Option<ParserState<'a>> {
        let mut state = self;

        loop {
            state = ParserState {
                index: state.index + 1,
                len: self.len,
                tokens: self.tokens,
            };

            if !state.peek().token_type.ignore() {
                return Some(state);
            }
        }
    }

    pub fn peek<'b>(self) -> Cow<'b, Token<'a>> {
        if self.index >= self.len {
            panic!("peek failed, when there should always be an eof");
        }

        let result = unsafe {
            &*self
                .tokens
                .add(self.index)
                .as_ref()
                .expect("couldn't peek, no eof?")
        };

        Cow::Borrowed(result)
    }
}

impl<'a> std::fmt::Debug for ParserState<'a> {
    fn fmt(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            formatter,
            "ParserState {{ index: {}, current: {:?} }}",
            self.index,
            self.peek()
        )
    }
}

trait Parser<'a>: Sized {
    type Item;

    fn parse(
        &self,
        state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Self::Item), InternalAstError<'a>>;
}

macro_rules! define_parser {
    ($parser:ident, $node:ty, $body:expr) => {
        impl<'a> Parser<'a> for $parser {
            type Item = $node;

            fn parse(
                &self,
                state: ParserState<'a>,
            ) -> Result<(ParserState<'a>, $node), InternalAstError<'a>> {
                $body(self, state)
            }
        }
    };
}

macro_rules! parse_first_of {
    ($state:ident, {$($parser:expr => $constructor:expr,)+}) => ({
        $(
            match $parser.parse($state) {
                Ok((state, node)) => return Ok((state, $constructor(node.into()))),
                Err(InternalAstError::NoMatch) => {},
                Err(other) => return Err(other),
            };
        )+

        Err(InternalAstError::NoMatch)
    });
}

macro_rules! expect {
    ($state:ident, $parsed:expr) => {
        match $parsed {
            Ok((state, node)) => (state, node),
            Err(InternalAstError::NoMatch) => {
                return Err(InternalAstError::UnexpectedToken {
                    token: $state.peek(),
                    additional: None,
                });
            }
            Err(other) => return Err(other),
        };
    };

    ($state:ident, $parsed:expr, $error:tt) => {
        match $parsed {
            Ok((state, node)) => (state, node),
            Err(InternalAstError::NoMatch) => {
                return Err(InternalAstError::UnexpectedToken {
                    token: $state.peek(),
                    additional: Some($error),
                });
            }
            Err(other) => return Err(other),
        };
    };
}

// This name is bad
macro_rules! keep_going {
    ($parsed:expr) => {
        match $parsed {
            Ok((state, node)) => Ok((state, node)),
            Err(InternalAstError::NoMatch) => Err(InternalAstError::NoMatch),
            Err(other) => return Err(other),
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

    fn parse(
        &self,
        mut state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Vec<T>), InternalAstError<'a>> {
        let mut nodes = Vec::new();
        loop {
            match self.0.parse(state) {
                Ok((new_state, node)) => {
                    state = new_state;
                    nodes.push(node);
                }
                Err(InternalAstError::NoMatch) => break,
                Err(other) => return Err(other),
            };
        }
        Ok((state, nodes))
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

    fn parse(
        &self,
        mut state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Vec<T>), InternalAstError<'a>> {
        let mut nodes = Vec::new();

        if let Ok((new_state, node)) = keep_going!(self.0.parse(state)) {
            state = new_state;
            nodes.push(node);
        } else {
            return Ok((state, Vec::new()));
        }

        while let Ok((new_state, _)) = keep_going!(self.1.parse(state)) {
            state = new_state;

            match self.0.parse(state) {
                Ok((new_state, node)) => {
                    state = new_state;
                    nodes.push(node);
                }

                Err(InternalAstError::NoMatch) => {
                    if self.2 {
                        break;
                    } else {
                        return Err(InternalAstError::UnexpectedToken {
                            token: state.peek(),
                            additional: Some("trailing character"),
                        });
                    }
                }

                Err(other) => {
                    return Err(other);
                }
            }
        }

        Ok((state, nodes))
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

    fn parse(
        &self,
        state: ParserState<'a>,
    ) -> Result<(ParserState<'a>, Vec<ItemParser::Item>), InternalAstError<'a>> {
        let mut nodes = Vec::new();
        let (mut state, node) = self.0.parse(state)?;
        nodes.push(node);

        while let Ok((new_state, _)) = self.1.parse(state) {
            match self.0.parse(new_state) {
                Ok((new_state, node)) => {
                    state = new_state;
                    nodes.push(node);
                }

                Err(InternalAstError::NoMatch) => {
                    if self.2 {
                        state = new_state;
                    }

                    break;
                }

                Err(other) => {
                    return Err(other);
                }
            }
        }

        Ok((state, nodes))
    }
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct NoDelimiter;

define_parser!(NoDelimiter, (), |_, state: ParserState<'a>| Ok((state, ())));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParseSymbol(Symbol);

define_parser!(
    ParseSymbol,
    Cow<'a, Token<'a>>,
    |this: &ParseSymbol, state: ParserState<'a>| {
        let expecting = TokenType::Symbol { symbol: this.0 };
        let token = state.peek();

        if token.token_type == expecting {
            Ok((state.advance().ok_or(InternalAstError::NoMatch)?, token))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParseNumber;

define_parser!(
    ParseNumber,
    Cow<'a, Token<'a>>,
    |_, state: ParserState<'a>| {
        let token = state.peek();
        if let TokenType::Number { .. } = token.token_type {
            Ok((state.advance().ok_or(InternalAstError::NoMatch)?, token))
        } else {
            Err(InternalAstError::NoMatch)
        }
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParseStringLiteral;

define_parser!(
    ParseStringLiteral,
    Cow<'a, Token<'a>>,
    |_, state: ParserState<'a>| {
        let token = state.peek();
        if let TokenType::StringLiteral { .. } = token.token_type {
            Ok((state.advance().ok_or(InternalAstError::NoMatch)?, token))
        } else {
            Err(InternalAstError::NoMatch)
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
define_parser!(ParseBlock, Block<'a>, |_, mut state| {
    let mut stmts = Vec::new();
    while let Ok((new_state, stmt)) = keep_going!(ParseStmt.parse(state)) {
        state = new_state;
        if let Ok((new_state, _)) = ParseSymbol(Symbol::Semicolon).parse(state) {
            state = new_state;
        }
        stmts.push(stmt);
    }

    if let Ok((mut state, last_stmt)) = keep_going!(ParseLastStmt.parse(state)) {
        if let Ok((new_state, _)) = ParseSymbol(Symbol::Semicolon).parse(state) {
            state = new_state;
        }

        Ok((
            state,
            Block {
                stmts,
                last_stmt: Some(last_stmt),
            },
        ))
    } else {
        Ok((
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
    |_, state| if let Ok((state, _)) = ParseSymbol(Symbol::Return).parse(state) {
        let (state, returns) = expect!(
            state,
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
            "return values"
        );
        Ok((state, LastStmt::Return(returns)))
    } else if let Ok((state, _)) = ParseSymbol(Symbol::Break).parse(state) {
        Ok((state, LastStmt::Break))
    } else {
        Err(InternalAstError::NoMatch)
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
        key: Box<Cow<'a, Token<'a>>>,
        value: Box<Expression<'a>>,
    },

    #[cfg_attr(feature = "serde", serde(borrow))]
    NoKey(Box<Expression<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseField;
define_parser!(ParseField, Field<'a>, |_, state| {
    if let Ok((state, _)) = ParseSymbol(Symbol::LeftBracket).parse(state) {
        let (state, key) = expect!(state, ParseExpression.parse(state), "expected key");
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::RightBracket).parse(state),
            "expected ']'"
        );
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::Equal).parse(state),
            "expected '='"
        );
        let (state, value) = expect!(state, ParseExpression.parse(state), "expected value");
        let (key, value) = (Box::new(key), Box::new(value));
        return Ok((state, Field::ExpressionKey { key, value }));
    } else if let Ok((state, key)) = keep_going!(ParseIdentifier.parse(state)) {
        if let Ok((state, _)) = ParseSymbol(Symbol::Equal).parse(state) {
            let (state, value) = expect!(state, ParseExpression.parse(state), "expected value");
            let (key, value) = (Box::new(key), Box::new(value));
            return Ok((state, Field::NameKey { key, value }));
        }
    }

    if let Ok((state, expr)) = keep_going!(ParseExpression.parse(state)) {
        let expr = Box::new(expr);
        return Ok((state, Field::NoKey(expr)));
    }

    Err(InternalAstError::NoMatch)
});

pub type TableConstructorField<'a> = (Field<'a>, Option<Cow<'a, Token<'a>>>);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TableConstructor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub fields: Vec<TableConstructorField<'a>>,
}

struct ParseTableConstructor;
define_parser!(ParseTableConstructor, TableConstructor<'a>, |_, state| {
    let (mut state, _) = ParseSymbol(Symbol::LeftBrace).parse(state)?;
    let mut fields = Vec::new();

    while let Ok((new_state, field)) = keep_going!(ParseField.parse(state)) {
        let field_sep = if let Ok((new_state, _)) = ParseSymbol(Symbol::Comma).parse(new_state) {
            state = new_state;
            Some(state.peek())
        } else if let Ok((new_state, _)) = ParseSymbol(Symbol::Semicolon).parse(new_state) {
            state = new_state;
            Some(state.peek())
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

    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::RightBrace).parse(state),
        "expected '}'"
    );

    Ok((state, TableConstructor { fields }))
});

pub type BinOpRhs<'a> = (BinOp<'a>, Box<Expression<'a>>);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum Expression<'a> {
    UnaryOperator {
        #[cfg_attr(feature = "serde", serde(borrow))]
        unop: UnOp<'a>,
        expression: Box<Expression<'a>>,
    },

    Value {
        #[cfg_attr(feature = "serde", serde(borrow))]
        value: Value<'a>,
        binop: Option<BinOpRhs<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(
    ParseExpression,
    Expression<'a>,
    |_, state| if let Ok((state, value)) = keep_going!(ParseValue.parse(state)) {
        let (state, binop) = if let Ok((state, binop)) = ParseBinOp.parse(state) {
            let (state, expression) =
                expect!(state, ParseExpression.parse(state), "expected expression");
            (state, Some((binop, Box::new(expression))))
        } else {
            (state, None)
        };

        Ok((state, Expression::Value { value, binop }))
    } else if let Ok((state, unop)) = keep_going!(ParseUnOp.parse(state)) {
        let (state, expression) =
            expect!(state, ParseExpression.parse(state), "expected expression");
        Ok((
            state,
            Expression::UnaryOperator {
                unop,
                expression: Box::new(expression),
            },
        ))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
struct ParseParenExpression;
define_parser!(
    ParseParenExpression,
    Expression<'a>,
    |_, state| if let Ok((state, _)) = ParseSymbol(Symbol::LeftParen).parse(state) {
        let (state, expression) =
            expect!(state, ParseExpression.parse(state), "expected expression");
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::RightParen).parse(state),
            "expected ')'"
        );
        Ok((state, expression))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Value<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Function(FunctionBody<'a>),
    FunctionCall(Box<FunctionCall<'a>>),
    TableConstructor(Box<TableConstructor<'a>>),
    Number(Cow<'a, Token<'a>>),
    ParseExpression(Box<Expression<'a>>),
    String(Cow<'a, Token<'a>>),
    Symbol(Cow<'a, Token<'a>>),
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
        ParseParenExpression => Value::ParseExpression,
    })
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Assignment(Assignment<'a>),
    Do(Block<'a>),
    FunctionCall(Box<FunctionCall<'a>>),
    FunctionDeclaration(Box<FunctionDeclaration<'a>>),
    GenericFor(Box<GenericFor<'a>>),
    If(Box<If<'a>>),
    LocalAssignment(LocalAssignment<'a>),
    LocalFunction(LocalFunction<'a>),
    NumericFor(Box<NumericFor<'a>>),
    Repeat(Box<Repeat<'a>>),
    While(Box<While<'a>>),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseStmt;
define_parser!(ParseStmt, Stmt<'a>, |_, state| parse_first_of!(state, {
    ParseAssignment => Stmt::Assignment,
    ParseFunctionCall => Stmt::FunctionCall,
    ParseDo => Stmt::Do,
    ParseWhile => Stmt::While,
    ParseRepeat => Stmt::Repeat,
    ParseIf => Stmt::If,
    ParseNumericFor => Stmt::NumericFor,
    ParseGenericFor => Stmt::GenericFor,
    ParseFunctionDeclaration => Stmt::FunctionDeclaration,
    ParseLocalFunction => Stmt::LocalFunction,
    ParseLocalAssignment => Stmt::LocalAssignment,
}));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Prefix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Expression(Expression<'a>),
    Name(Cow<'a, Token<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
struct ParsePrefix;
define_parser!(ParsePrefix, Prefix<'a>, |_, state| parse_first_of!(state, {
    ParseParenExpression => Prefix::Expression,
    ParseIdentifier => Prefix::Name,
}));

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Index<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Brackets(Expression<'a>),
    Dot(Cow<'a, Token<'a>>),
}

struct ParseIndex;
define_parser!(ParseIndex, Index<'a>, |_, state| if let Ok((state, _)) =
    ParseSymbol(Symbol::LeftBracket).parse(state)
{
    let (state, expression) = expect!(state, ParseExpression.parse(state), "expected expression");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::RightBracket).parse(state),
        "expected ']'"
    );
    Ok((state, Index::Brackets(expression)))
} else if let Ok((state, _)) = ParseSymbol(Symbol::Dot).parse(state) {
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected name");
    Ok((state, Index::Dot(name)))
} else {
    Err(InternalAstError::NoMatch)
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum FunctionArgs<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Parentheses(Vec<Expression<'a>>),
    String(Cow<'a, Token<'a>>),
    TableConstructor(Box<TableConstructor<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionArgs;
define_parser!(
    ParseFunctionArgs,
    FunctionArgs<'a>,
    |_, state| if let Ok((state, _)) = keep_going!(ParseSymbol(Symbol::LeftParen).parse(state)) {
        let (state, expr_list) = expect!(
            state,
            ZeroOrMoreDelimited(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
            "expected arguments"
        );
        let (state, _) = expect!(
            state,
            ParseSymbol(Symbol::RightParen).parse(state),
            "expected ')'"
        );
        Ok((state, FunctionArgs::Parentheses(expr_list)))
    } else if let Ok((state, table_constructor)) = keep_going!(ParseTableConstructor.parse(state)) {
        Ok((
            state,
            FunctionArgs::TableConstructor(Box::new(table_constructor)),
        ))
    } else if let Ok((state, string)) = keep_going!(ParseStringLiteral.parse(state)) {
        Ok((state, FunctionArgs::String(string)))
    } else {
        Err(InternalAstError::NoMatch)
    }
);

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct NumericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub index_variable: Cow<'a, Token<'a>>,
    pub start: Expression<'a>,
    pub end: Expression<'a>,
    pub step: Option<Expression<'a>>,
    pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseNumericFor;
define_parser!(ParseNumericFor, NumericFor<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::For).parse(state)?;
    let (state, index_variable) = expect!(state, ParseIdentifier.parse(state), "expected names");
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?; // Numeric fors run before generic fors, so we can't guarantee this
    let (state, start) = expect!(
        state,
        ParseExpression.parse(state),
        "expected start expression"
    );
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::Comma).parse(state),
        "expected comma"
    );
    let (state, end) = expect!(
        state,
        ParseExpression.parse(state),
        "expected end expression"
    );
    let (state, step) = if let Ok((state, _)) = ParseSymbol(Symbol::Comma).parse(state) {
        let (state, expression) = expect!(
            state,
            ParseExpression.parse(state),
            "expected limit expression"
        );
        (state, Some(expression))
    } else {
        (state, None)
    };
    let (state, _) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        NumericFor {
            index_variable,
            start,
            end,
            step,
            block,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub names: Vec<Cow<'a, Token<'a>>>,
    pub expr_list: Vec<Expression<'a>>,
    pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseGenericFor;
define_parser!(ParseGenericFor, GenericFor<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::For).parse(state)?;
    let (state, names) = expect!(
        state,
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected names"
    );
    let (state, _) = expect!(state, ParseSymbol(Symbol::In).parse(state), "expected 'in'"); // Numeric fors run before here, so there has to be an in
    let (state, expr_list) = expect!(
        state,
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected expression"
    );
    let (state, _) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((
        state,
        GenericFor {
            names,
            expr_list,
            block,
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct If<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub condition: Expression<'a>,
    pub block: Block<'a>,
    pub else_if: Option<Vec<(Expression<'a>, Block<'a>)>>,
    #[cfg_attr(feature = "serde", serde(rename = "else"))]
    pub r#else: Option<Block<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseIf;
define_parser!(ParseIf, If<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::If).parse(state)?;
    let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::Then).parse(state),
        "expected 'then'"
    );
    let (mut state, block) = expect!(state, ParseBlock.parse(state), "expected block");

    let mut else_ifs = Vec::new();
    while let Ok((new_state, _)) = ParseSymbol(Symbol::ElseIf).parse(state) {
        let (new_state, condition) = expect!(
            state,
            ParseExpression.parse(new_state),
            "expected condition"
        );
        let (new_state, _) = expect!(
            state,
            ParseSymbol(Symbol::Then).parse(new_state),
            "expected 'then'"
        );
        let (new_state, block) = expect!(state, ParseBlock.parse(new_state), "expected block");
        state = new_state;
        else_ifs.push((condition, block));
    }

    let (state, r#else) = if let Ok((state, _)) = ParseSymbol(Symbol::Else).parse(state) {
        let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
        (state, Some(block))
    } else {
        (state, None)
    };

    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((
        state,
        If {
            condition,
            block,
            r#else,
            else_if: if else_ifs.is_empty() {
                None
            } else {
                Some(else_ifs)
            },
        },
    ))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct While<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub condition: Expression<'a>,
    pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseWhile;
define_parser!(ParseWhile, While<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::While).parse(state)?;
    let (state, condition) = expect!(state, ParseExpression.parse(state), "expected condition");
    let (state, _) = expect!(state, ParseSymbol(Symbol::Do).parse(state), "expected 'do'");
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((state, While { condition, block }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Repeat<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub block: Block<'a>,
    pub until: Expression<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseRepeat;
define_parser!(ParseRepeat, Repeat<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Repeat).parse(state)?;
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::Until).parse(state),
        "expected 'until'"
    );
    let (state, until) = expect!(state, ParseExpression.parse(state), "expected condition");
    Ok((state, Repeat { until, block }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct MethodCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub name: Cow<'a, Token<'a>>,
    pub args: FunctionArgs<'a>,
}

struct ParseMethodCall;
define_parser!(ParseMethodCall, MethodCall<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Colon).parse(state)?;
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected method");
    let (state, args) = expect!(state, ParseFunctionArgs.parse(state), "expected args");
    Ok((state, MethodCall { name, args }))
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
pub struct FunctionBody<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub parameters: Vec<Parameter<'a>>,
    pub block: Block<'a>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionBody;
define_parser!(ParseFunctionBody, FunctionBody<'a>, |_, state| {
    let (mut state, _) = expect!(
        state,
        ParseSymbol(Symbol::LeftParen).parse(state),
        "expected '('"
    );
    let mut parameters = Vec::new();

    if let Ok((new_state, names)) =
        keep_going!(OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state))
    {
        state = new_state;
        parameters.extend(names.into_iter().map(Parameter::Name));

        if let Ok((new_state, _)) = ParseSymbol(Symbol::Comma).parse(state) {
            if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(new_state) {
                state = new_state;
                parameters.push(Parameter::Ellipse(ellipse));
            }
        }
    } else if let Ok((new_state, ellipse)) = ParseSymbol(Symbol::Ellipse).parse(state) {
        state = new_state;
        parameters.push(Parameter::Ellipse(ellipse));
    }

    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::RightParen).parse(state),
        "expected ')'"
    );
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );
    Ok((state, FunctionBody { parameters, block }))
});

#[derive(Clone, Debug, PartialEq)]
struct ParseFunction;
define_parser!(ParseFunction, FunctionBody<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Function).parse(state)?;
    ParseFunctionBody.parse(state)
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Parameter<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Ellipse(Cow<'a, Token<'a>>),
    Name(Cow<'a, Token<'a>>),
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
    pub prefix: Prefix<'a>,
    pub suffixes: Vec<Suffix<'a>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseVarExpression;
define_parser!(ParseVarExpression, VarExpression<'a>, |_, state| {
    let (state, prefix) = ParsePrefix.parse(state)?;
    let (state, suffixes) = ZeroOrMore(ParseSuffix).parse(state)?;

    if let Some(Suffix::Index(_)) = suffixes.last() {
        Ok((state, VarExpression { prefix, suffixes }))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Var<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    Expression(VarExpression<'a>),
    Name(Cow<'a, Token<'a>>),
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
    pub var_list: Vec<Var<'a>>,
    pub expr_list: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseAssignment;
define_parser!(ParseAssignment, Assignment<'a>, |_, state| {
    let (state, var_list) = OneOrMore(ParseVar, ParseSymbol(Symbol::Comma), false).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Equal).parse(state)?;
    let (state, expr_list) = expect!(
        state,
        OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected values"
    );

    Ok((
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
    pub name: Cow<'a, Token<'a>>,
    pub func_body: FunctionBody<'a>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalFunction;
define_parser!(ParseLocalFunction, LocalFunction<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Local).parse(state)?;
    let (state, _) = ParseSymbol(Symbol::Function).parse(state)?;
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected name");
    let (state, func_body) = ParseFunctionBody.parse(state)?;
    Ok((state, LocalFunction { name, func_body }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub name_list: Vec<Cow<'a, Token<'a>>>,
    pub expr_list: Vec<Expression<'a>>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseLocalAssignment;
define_parser!(ParseLocalAssignment, LocalAssignment<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Local).parse(state)?;
    let (state, name_list) = expect!(
        state,
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Comma), false).parse(state),
        "expected name"
    );
    let (state, expr_list) = match ParseSymbol(Symbol::Equal).parse(state) {
        Ok((state, _)) => OneOrMore(ParseExpression, ParseSymbol(Symbol::Comma), false)
            .parse(state)
            .or_else(|_| {
                Err(InternalAstError::UnexpectedToken {
                    token: state.peek(),
                    additional: Some("expected expression"),
                })
            })?,
        Err(InternalAstError::NoMatch) => (state, Vec::new()),
        Err(other) => return Err(other),
    };

    Ok((
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
    let (state, block) = expect!(state, ParseBlock.parse(state), "expected block");
    let (state, _) = expect!(
        state,
        ParseSymbol(Symbol::End).parse(state),
        "expected 'end'"
    );

    Ok((state, block))
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
        Ok((state, FunctionCall { prefix, suffixes }))
    } else {
        Err(InternalAstError::NoMatch)
    }
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionName<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub names: Vec<Cow<'a, Token<'a>>>,
    pub colon_name: Option<Cow<'a, Token<'a>>>,
}

#[derive(Clone, Debug, PartialEq)]
struct ParseFunctionName;
define_parser!(ParseFunctionName, FunctionName<'a>, |_, state| {
    let (state, names) =
        OneOrMore(ParseIdentifier, ParseSymbol(Symbol::Dot), false).parse(state)?;
    let (state, colon_name) = if let Ok((state, _)) = ParseSymbol(Symbol::Colon).parse(state) {
        let (state, colon_name) =
            expect!(state, ParseIdentifier.parse(state), "expected method name");
        (state, Some(colon_name))
    } else {
        (state, None)
    };

    Ok((state, FunctionName { names, colon_name }))
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionDeclaration<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub name: FunctionName<'a>,
    pub body: FunctionBody<'a>,
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseFunctionDeclaration;
define_parser!(
    ParseFunctionDeclaration,
    FunctionDeclaration<'a>,
    |_, state| {
        let (state, _) = ParseSymbol(Symbol::Function).parse(state)?;
        let (state, name) = expect!(
            state,
            ParseFunctionName.parse(state),
            "expected function name"
        );
        let (state, body) = expect!(
            state,
            ParseFunctionBody.parse(state),
            "expected function body"
        );
        Ok((state, FunctionDeclaration { name, body }))
    }
);

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseIdentifier;
#[rustfmt::skip]
define_parser!(ParseIdentifier, Cow<'a, Token<'a>>, |_, state: ParserState<'a>| {
    let next_token = state.peek();
    match &next_token.token_type {
        TokenType::Identifier { .. } => Ok((
            state.advance().ok_or(InternalAstError::NoMatch)?,
            next_token,
        )),
        _ => Err(InternalAstError::NoMatch),
    }
});

macro_rules! make_op {
    ($enum:ident, $parser:ident, { $($operator:ident,)+ }) => {
        #[derive(Clone, Debug, PartialEq)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        pub enum $enum<'a> {
            #[cfg_attr(feature = "serde", serde(borrow))]
            $(
                $operator(Cow<'a, Token<'a>>),
            )+
        }

        #[derive(Clone, Debug, PartialEq)]
        struct $parser;
        define_parser!($parser, $enum<'a>, |_, state| {
            $(
                if let Ok((state, _)) = ParseSymbol(Symbol::$operator).parse(state) {
                    return Ok((state, $enum::$operator(state.peek())));
                }
            )+

            Err(InternalAstError::NoMatch)
        });
    };
}

make_op!(BinOp, ParseBinOp, {
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
});

make_op!(UnOp, ParseUnOp, {
    Minus,
    Not,
    Hash,
});

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum AstError<'a> {
    Empty,
    NoEof,
    NoMatch,
    UnexpectedToken {
        token: Token<'a>,
        additional: Option<&'a str>,
    },
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
enum InternalAstError<'a> {
    NoMatch,
    UnexpectedToken {
        #[cfg_attr(feature = "serde", serde(borrow))]
        token: Cow<'a, Token<'a>>,
        additional: Option<&'a str>,
    },
}

#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'a> {
    pub nodes: Block<'a>,
    pub tokens: Vec<Token<'a>>,
}

impl<'a> Ast<'a> {
    pub fn from_tokens(tokens: Vec<Token<'a>>) -> Result<Ast<'a>, AstError<'a>> {
        if tokens.last().ok_or(AstError::Empty)?.token_type != TokenType::Eof {
            Err(AstError::NoEof)
        } else {
            let mut state = ParserState {
                index: 0,
                tokens: tokens.as_ptr(),
                len: tokens.len(),
            };

            if tokens
                .iter()
                .filter(|token| !token.token_type.ignore())
                .count()
                == 1
            {
                // Entirely comments/whitespace
                return Ok(Ast {
                    nodes: Block {
                        stmts: Vec::new(),
                        last_stmt: None,
                    },
                    tokens,
                });
            }

            // ParserState has to have at least 2 tokens, the last being an EOF, thus unwrap() can't fail
            if state.peek().token_type.ignore() {
                state = state.advance().unwrap();
            }

            match ParseBlock.parse(state) {
                Ok((state, block)) => {
                    if state.index == tokens.len() - 1 {
                        Ok(Ast {
                            tokens,
                            nodes: block,
                        })
                    } else {
                        Err(AstError::UnexpectedToken {
                            token: state.peek().into_owned(),
                            additional: Some("leftover token"),
                        })
                    }
                }

                Err(InternalAstError::NoMatch) => Err(AstError::UnexpectedToken {
                    token: state.peek().into_owned(),
                    additional: None,
                }),

                Err(InternalAstError::UnexpectedToken { token, additional }) => {
                    Err(AstError::UnexpectedToken {
                        token: token.into_owned(),
                        additional,
                    })
                }
            }
        }
    }
}

#[cfg(all(test, not(feature = "only-source-tests")))]
mod tests {
    use super::*;
    use crate::tokenizer::tokens;
    use pretty_assertions::assert_eq;

    #[test]
    fn test_zero_or_more_empty() {
        let tokens = tokens("local x").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        };

        let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma)).parse(state).unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 0,
                len: tokens.len(),
                tokens: tokens.as_ptr(),
            },
        );

        assert_eq!(commas.len(), 0);
    }

    #[test]
    fn test_zero_or_more_exists() {
        let tokens = tokens(",,, , ,\t ,local x").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        };

        let (state, commas) = ZeroOrMore(ParseSymbol(Symbol::Comma)).parse(state).unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 9,
                len: tokens.len(),
                tokens: tokens.as_ptr(),
            },
        );

        assert_eq!(commas.len(), 6);
    }

    #[test]
    fn test_one_or_more_empty() {
        let tokens = tokens("local x").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        };

        assert!(
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
                .parse(state)
                .is_err()
        );
    }

    #[test]
    fn test_one_or_more_exists_no_delimiter() {
        let tokens = tokens("end,end, end,\t\tend local").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        };

        let (state, commas) =
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), false)
                .parse(state)
                .expect("OneOrMore failed");

        assert_eq!(
            state,
            ParserState {
                index: 10,
                len: tokens.len(),
                tokens: tokens.as_ptr(),
            },
        );

        assert_eq!(commas.len(), 4);
    }

    #[test]
    fn test_one_or_more_exists_with_delimiter() {
        let tokens = tokens("end,end, end,\t\tend, local").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        };

        let (state, commas) = OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
            .parse(state)
            .unwrap();

        assert_eq!(
            state,
            ParserState {
                index: 11,
                len: tokens.len(),
                tokens: tokens.as_ptr(),
            },
        );

        assert_eq!(commas.len(), 4);
    }

    #[test]
    fn test_one_or_more_exists_with_nothing() {
        let tokens = tokens("local").expect("couldn't tokenize");
        let state = ParserState {
            index: 0,
            len: tokens.len(),
            tokens: tokens.as_ptr(),
        };

        assert!(
            OneOrMore(ParseSymbol(Symbol::End), ParseSymbol(Symbol::Comma), true)
                .parse(state)
                .is_err()
        );
    }
}
