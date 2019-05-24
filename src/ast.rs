use crate::tokenizer::{Symbol, Token, TokenType};
use full_moon_derive::Visit;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;
use std::fmt;

#[derive(Clone, Copy, PartialEq)]
struct ParserState<'a> {
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

impl<'a> fmt::Debug for ParserState<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
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
struct NoDelimiter;

define_parser!(NoDelimiter, (), |_, state: ParserState<'a>| Ok((state, ())));

#[derive(Clone, Debug, PartialEq)]
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

/// A block of statements, such as in if/do/etc block
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Block<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    stmts: Vec<Stmt<'a>>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    last_stmt: Option<LastStmt<'a>>,
}

impl<'a> Block<'a> {
    /// An iterator over the [statements](enum.Stmt.html) in the block, such as `local foo = 1`
    pub fn iter_stmts(&self) -> impl Iterator<Item = &Stmt<'a>> {
        self.stmts.iter()
    }

    /// The last statement of the block if one exists, such as `return foo`
    pub fn last_stmts(&self) -> Option<&LastStmt<'a>> {
        self.last_stmt.as_ref()
    }
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

/// The last statement of a [`Block`](struct.Block.html)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LastStmt<'a> {
    /// A `break` statement
    Break,
    /// A `return` statement, expression is what is being returned
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

/// Fields of a [`TableConstructor`](struct.TableConstructor.html)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Field<'a> {
    /// A key in the format of `[expression] = value`
    ExpressionKey {
        /// The `expression` part of `[expression] = value`
        #[cfg_attr(feature = "serde", serde(borrow))]
        key: Box<Expression<'a>>,
        /// The `value` part of `[expression] = value`
        value: Box<Expression<'a>>,
    },

    /// A key in the format of `name = value`
    NameKey {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `name` part of `name = value`
        key: Box<Cow<'a, Token<'a>>>,
        /// The `value` part of `name = value`
        value: Box<Expression<'a>>,
    },

    /// A field with no key, just a value (such as `"a"` in `{ "a" }`)
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

/// A [`Field`](enum.Field.html) used when creating a table
/// Second parameter is the separator used (`,` or `;`) if one exists
pub type TableConstructorField<'a> = (Field<'a>, Option<Cow<'a, Token<'a>>>);

/// A table being constructed, such as `{ 1, 2, 3 }` or `{ a = 1 }`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TableConstructor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    fields: Vec<TableConstructorField<'a>>,
}

impl<'a> TableConstructor<'a> {
    /// An iterator over the [fields](type.TableConstructorField.html) used to create the table
    pub fn iter_fields(&self) -> impl Iterator<Item = &TableConstructorField<'a>> {
        self.fields.iter()
    }
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

/// A binary operation, such as (`+ 3`)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[visit(visit_as = "bin_op")]
pub struct BinOpRhs<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    bin_op: BinOp<'a>,
    rhs: Box<Expression<'a>>,
}

impl<'a> BinOpRhs<'a> {
    /// The binary operation used, the `+` part of `+ 3`
    pub fn bin_op(&self) -> &BinOp<'a> {
        &self.bin_op
    }

    /// The right hand side of the binary operation, the `3` part of `+ 3`
    pub fn rhs(&self) -> &Expression<'a> {
        self.rhs.as_ref()
    }
}

/// An expression, mostly useful for getting values
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum Expression<'a> {
    /// A unary operation, such as `#list`
    UnaryOperator {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The unary operation, the `#` part of `#list`
        unop: UnOp<'a>,
        /// The expression the operation is being done on, the `list` part of `#list`
        expression: Box<Expression<'a>>,
    },

    /// A value, such as "strings"
    Value {
        /// The value itself
        #[cfg_attr(feature = "serde", serde(borrow))]
        value: Value<'a>,
        /// The binary operation being done, if one exists (the `+ 3` part of `2 + 3`)
        binop: Option<BinOpRhs<'a>>,
    },
}

#[derive(Clone, Debug, PartialEq)]
struct ParseExpression;
define_parser!(
    ParseExpression,
    Expression<'a>,
    |_, state| if let Ok((state, value)) = keep_going!(ParseValue.parse(state)) {
        let (state, binop) = if let Ok((state, bin_op)) = ParseBinOp.parse(state) {
            let (state, expression) =
                expect!(state, ParseExpression.parse(state), "expected expression");
            (
                state,
                Some(BinOpRhs {
                    bin_op,
                    rhs: Box::new(expression),
                }),
            )
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

/// Values that cannot be used standalone, but as part of things such as [statements](enum.Stmt.html)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Value<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An anonymous function, such as `function() end)`
    Function(FunctionBody<'a>),
    /// A call of a function, such as `call()`
    FunctionCall(Box<FunctionCall<'a>>),
    /// A table constructor, such as `{ 1, 2, 3 }`
    TableConstructor(Box<TableConstructor<'a>>),
    /// A number token, such as `3.3`
    Number(Cow<'a, Token<'a>>),
    /// An expression between parentheses, such as `(3 + 2)`
    ParseExpression(Box<Expression<'a>>),
    /// A string token, such as `"hello"`
    String(Cow<'a, Token<'a>>),
    /// A symbol, such as `true`
    Symbol(Cow<'a, Token<'a>>),
    /// A more complex value, such as `call().x`
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

/// A statement that stands alone
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An assignment, such as `x = 1`
    Assignment(Assignment<'a>),
    /// A do block, `do end`
    Do(Block<'a>),
    /// A function call on its own, such as `call()`
    FunctionCall(Box<FunctionCall<'a>>),
    /// A function declaration, such as `function x() end`
    FunctionDeclaration(Box<FunctionDeclaration<'a>>),
    /// A generic for loop, such as `for index, value in pairs(list) do end`
    GenericFor(Box<GenericFor<'a>>),
    /// An if statement
    If(Box<If<'a>>),
    /// A local assignment, such as `local x = 1`
    LocalAssignment(LocalAssignment<'a>),
    /// A local function declaration, such as `local function x() end`
    LocalFunction(LocalFunction<'a>),
    /// A numeric for loop, such as `for index = 1, 10 do end`
    NumericFor(Box<NumericFor<'a>>),
    /// A repeat loop
    Repeat(Box<Repeat<'a>>),
    /// A while loop
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

/// A node used before another in cases such as function calling
/// The `("foo")` part of `("foo"):upper()`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Prefix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A complicated expression, such as `("foo")`
    Expression(Expression<'a>),
    /// Just a name, such as `foo`
    Name(Cow<'a, Token<'a>>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParsePrefix;
define_parser!(ParsePrefix, Prefix<'a>, |_, state| parse_first_of!(state, {
    ParseParenExpression => Prefix::Expression,
    ParseIdentifier => Prefix::Name,
}));

/// The indexing of something, such as `x.y` or `x["y"]`
/// Values of variants are the keys, such as `"y"`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Index<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// Indexing in the form of `x["y"]`
    Brackets(Expression<'a>),
    /// Indexing in the form of `x.y`
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

/// Arguments used for a function
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum FunctionArgs<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// Used when a function is called in the form of `call(1, 2, 3)`
    Parentheses(Vec<Expression<'a>>),
    /// Used when a function is called in the form of `call "foobar"`
    String(Cow<'a, Token<'a>>),
    /// Used when a function is called in the form of `call { 1, 2, 3 }`
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

/// A numeric for loop, such as `for index = 1, 10 do end`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct NumericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    index_variable: Cow<'a, Token<'a>>,
    start: Expression<'a>,
    end: Expression<'a>,
    step: Option<Expression<'a>>,
    block: Block<'a>,
}

impl<'a> NumericFor<'a> {
    /// The index identity, `index` in the initial example
    pub fn index_variable(&self) -> &Token<'a> {
        self.index_variable.as_ref()
    }

    /// The starting point, `1` in the initial example
    pub fn start(&self) -> &Expression<'a> {
        &self.start
    }

    /// The ending point, `10` in the initial example
    pub fn end(&self) -> &Expression<'a> {
        &self.end
    }

    /// The step if one exists, `2` in `for index = 0, 10, 2 do end`
    pub fn step(&self) -> Option<&Expression<'a>> {
        self.step.as_ref()
    }

    /// The code inside the for loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }
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

/// A generic for loop, such as `for index, value in pairs(list) do end`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    names: Vec<Cow<'a, Token<'a>>>,
    expr_list: Vec<Expression<'a>>,
    block: Block<'a>,
}

impl<'a> GenericFor<'a> {
    /// An iterator over the names used in a for loop
    /// In `for index, value in pairs(list) do`, iterates over `index` and `value`
    pub fn iter_names(&self) -> impl Iterator<Item = &Cow<'a, Token<'a>>> {
        self.names.iter()
    }

    /// An iterator over the expression used in a for loop
    /// In `for index, value in pairs(list) do`, iterates over `pairs(list)`
    pub fn iter_expr_list(&self) -> impl Iterator<Item = &Expression<'a>> {
        self.expr_list.iter()
    }

    /// The code inside the for loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }
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

/// An if statement
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct If<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    condition: Expression<'a>,
    block: Block<'a>,
    else_if: Option<Vec<(Expression<'a>, Block<'a>)>>,
    #[cfg_attr(feature = "serde", serde(rename = "else"))]
    r#else: Option<Block<'a>>,
}

impl<'a> If<'a> {
    /// The condition of the if statement, `condition` in `if condition then`
    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    /// The block inside the initial if statement
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// If there are `elseif` conditions, returns a vector of them
    /// Expression is the condition, block is the code if the condition is true
    // TODO: Make this return an iterator, and remove Option part entirely?
    pub fn else_if(&self) -> Option<&Vec<(Expression<'a>, Block<'a>)>> {
        self.else_if.as_ref()
    }

    /// The code inside an `else` block if one exists
    pub fn else_block(&self) -> Option<&Block<'a>> {
        self.r#else.as_ref()
    }
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

/// A while loop
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct While<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    condition: Expression<'a>,
    block: Block<'a>,
}

impl<'a> While<'a> {
    /// The `condition` part of `while condition do`
    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    /// The code inside the while loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }
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

/// A repeat loop
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Repeat<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    block: Block<'a>,
    until: Expression<'a>,
}

impl<'a> Repeat<'a> {
    /// The code inside the `repeat` block
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The condition for the `until` part
    pub fn until(&self) -> &Expression<'a> {
        &self.until
    }
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

/// A method call, such as `x:y()`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct MethodCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: Cow<'a, Token<'a>>,
    args: FunctionArgs<'a>,
}

impl<'a> MethodCall<'a> {
    /// The arguments of a method call, the `x, y, z` part of `method:call(x, y, z)`
    pub fn args(&self) -> &FunctionArgs<'a> {
        &self.args
    }

    /// The method being called, the `call` part of `method:call()`
    pub fn name(&self) -> &Token<'a> {
        self.name.as_ref()
    }
}

struct ParseMethodCall;
define_parser!(ParseMethodCall, MethodCall<'a>, |_, state| {
    let (state, _) = ParseSymbol(Symbol::Colon).parse(state)?;
    let (state, name) = expect!(state, ParseIdentifier.parse(state), "expected method");
    let (state, args) = expect!(state, ParseFunctionArgs.parse(state), "expected args");
    Ok((state, MethodCall { name, args }))
});

/// Something being called
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Call<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A function being called directly, such as `x(1)`
    AnonymousCall(FunctionArgs<'a>),
    /// A method call, such as `x:y()`
    MethodCall(MethodCall<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseCall;
define_parser!(ParseCall, Call<'a>, |_, state| parse_first_of!(state, {
    ParseFunctionArgs => Call::AnonymousCall,
    ParseMethodCall => Call::MethodCall,
}));

/// A function body, everything except `function x` in `function x(a, b, c) call() end`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionBody<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    parameters: Vec<Parameter<'a>>,
    block: Block<'a>,
}

impl<'a> FunctionBody<'a> {
    /// The code of a function body
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// An iterator over the parameters for the function declaration
    pub fn iter_parameters(&self) -> impl Iterator<Item = &Parameter<'a>> {
        self.parameters.iter()
    }
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

/// A parameter in a function declaration
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Parameter<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// The `...` vararg syntax, such as `function x(...)`
    Ellipse(Cow<'a, Token<'a>>),
    /// A name parameter, such as `function x(a, b, c)`
    Name(Cow<'a, Token<'a>>),
}

/// A suffix in certain cases, such as `:y()` in `x:y()`
/// Can be stacked on top of each other, such as in `x()()()`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Suffix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A call, including method calls and direct calls
    Call(Call<'a>),
    /// An index, such as `x.y`
    Index(Index<'a>),
}

#[derive(Clone, Debug, PartialEq)]
struct ParseSuffix;
define_parser!(ParseSuffix, Suffix<'a>, |_, state| parse_first_of!(state, {
    ParseCall => Suffix::Call,
    ParseIndex => Suffix::Index,
}));

/// A complex expression used by [`Var`](enum.Var.html), consisting of both a prefix and suffixes
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct VarExpression<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    prefix: Prefix<'a>,
    suffixes: Vec<Suffix<'a>>,
}

impl<'a> VarExpression<'a> {
    /// The prefix of the expression, such as a name
    pub fn prefix(&self) -> &Prefix<'a> {
        &self.prefix
    }

    /// An iter over the suffixes, such as indexing or calling
    pub fn iter_suffixes(&self) -> impl Iterator<Item = &Suffix<'a>> {
        self.suffixes.iter()
    }
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

/// Used in [`Assignment`s](struct.Assignment.html) and [`Value`s](enum.Value.html)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Var<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An expression, such as `x.y.z` or `x()`
    Expression(VarExpression<'a>),
    /// A literal identifier, such as `x`
    Name(Cow<'a, Token<'a>>),
}

#[derive(Clone, Debug, Default, PartialEq)]
struct ParseVar;
define_parser!(ParseVar, Var<'a>, |_, state| parse_first_of!(state, {
    ParseVarExpression => Var::Expression,
    ParseIdentifier => Var::Name,
}));

/// An assignment, such as `x = y`. Not used for [`LocalAssignment`s](struct.LocalAssignment.html)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Assignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    var_list: Vec<Var<'a>>,
    expr_list: Vec<Expression<'a>>,
}

impl<'a> Assignment<'a> {
    /// An iterator over the expressions being assigned, the `1, 2` part of `x, y["a"] = 1, 2`
    pub fn iter_expr_list(&self) -> impl Iterator<Item = &Expression<'a>> {
        self.expr_list.iter()
    }

    /// An iterator over the variables being assigned to, the `x, y["a"]` part of `x, y["a"] = 1, 2`
    pub fn iter_var_list(&self) -> impl Iterator<Item = &Var<'a>> {
        self.var_list.iter()
    }
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

/// A declaration of a local function, such as `local function x() end`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalFunction<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: Cow<'a, Token<'a>>,
    func_body: FunctionBody<'a>,
}

impl<'a> LocalFunction<'a> {
    /// The function body, everything except `local function x` in `local function x(a, b, c) call() end`
    pub fn func_body(&self) -> &FunctionBody<'a> {
        &self.func_body
    }

    /// The name of the function, the `x` part of `local function x() end`
    pub fn name(&self) -> &Token<'a> {
        self.name.as_ref()
    }
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

/// An assignment to a local variable, such as `local x = 1`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name_list: Vec<Cow<'a, Token<'a>>>,
    expr_list: Vec<Expression<'a>>,
}

impl<'a> LocalAssignment<'a> {
    /// An iterator over the expressions being assigned, the `1, 2` part of `local x, y = 1, 2`
    pub fn iter_expr_list(&self) -> impl Iterator<Item = &Expression<'a>> {
        self.expr_list.iter()
    }

    /// An iterator over the names being assigned to, the `x, y` part of `local x, y = 1, 2`
    pub fn iter_name_list(&self) -> impl Iterator<Item = &Token<'a>> {
        self.name_list.iter().map(Cow::as_ref)
    }
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

/// A function being called, such as `call()`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    prefix: Prefix<'a>,
    suffixes: Vec<Suffix<'a>>,
}

impl<'a> FunctionCall<'a> {
    /// The prefix of a function call, the `call` part of `call()`
    pub fn prefix(&self) -> &Prefix<'a> {
        &self.prefix
    }

    /// The suffix of a function call, the `()` part of `call()`
    pub fn iter_suffixes(&self) -> impl Iterator<Item = &Suffix<'a>> {
        self.suffixes.iter()
    }
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

/// A function name when being [declared](struct.FunctionDeclaration.html)
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionName<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    names: Vec<Cow<'a, Token<'a>>>,
    colon_name: Option<Cow<'a, Token<'a>>>,
}

impl<'a> FunctionName<'a> {
    /// A method name if one exists, the `y` part of `function x:y() end`
    pub fn method_name(&self) -> Option<&Token<'a>> {
        self.colon_name.as_ref().map(Cow::as_ref)
    }

    /// An iterator over the names used when defining the function, the `x.y.z` part of `function x.y.z() end`
    pub fn iter_names(&self) -> impl Iterator<Item = &Token<'a>> {
        self.names.iter().map(Cow::as_ref)
    }
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

/// A normal function declaration, supports simple declarations like `function x() end`
/// as well as complicated declarations such as `function x.y.z:a() end`
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionDeclaration<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    name: FunctionName<'a>,
    body: FunctionBody<'a>,
}

impl<'a> FunctionDeclaration<'a> {
    /// The body of the function
    pub fn body(&self) -> &FunctionBody<'a> {
        &self.body
    }

    /// The name of the function
    pub fn name(&self) -> &FunctionName<'a> {
        &self.name
    }
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
    ($enum:ident, $parser:ident, $(#[$outer:meta])* { $($operator:ident,)+ }) => {
        #[derive(Clone, Debug, PartialEq, Visit)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        #[visit(skip_visit_self)]
        $(#[$outer])*
        pub enum $enum<'a> {
            #[cfg_attr(feature = "serde", serde(borrow))]
            $(
                #[allow(missing_docs)]
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

make_op!(BinOp, ParseBinOp,
    #[doc = "Operators that require two operands, such as X + Y or X - Y"]
    {
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
    }
);

make_op!(UnOp, ParseUnOp,
    #[doc = "Operators that require just one operand, such as #X"]
    {
        Minus,
        Not,
        Hash,
    }
);

/// An error that occurs when creating the ast *after* tokenizing
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum AstError<'a> {
    /// There were no tokens passed, which shouldn't happen normally
    Empty,
    /// Tokens passed had no end of file token, which shouldn't happen normally
    NoEof,
    /// An unexpected token, the most likely scenario when getting an AstError
    UnexpectedToken {
        /// The token that caused the error
        token: Token<'a>,
        /// Any additional information that could be provided for debugging
        additional: Option<&'a str>,
    },
}

impl<'a> fmt::Display for AstError<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstError::Empty => write!(formatter, "tokens passed was empty, which shouldn't happen normally"),
            AstError::NoEof => write!(formatter, "tokens passed had no eof token, which shouldn't happen normally"),
            AstError::UnexpectedToken { token, additional } => write!(
                formatter,
                "unexpected token `{}`. (starting from line {}, character {} and ending on line {}, character {}){}",
                token,
                token.start_position.line,
                token.start_position.character,
                token.end_position.line,
                token.end_position.character,
                match additional {
                    Some(additional) => format!("\nadditional information: {}", additional),
                    None => String::new(),
                }
            )
        }
    }
}

impl<'a> std::error::Error for AstError<'a> {}

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

/// An abstract syntax tree, contains all the nodes used in the code
#[derive(Clone, Debug, PartialEq)]
pub struct Ast<'a> {
    nodes: Block<'a>,
    tokens: Vec<Token<'a>>,
}

impl<'a> Ast<'a> {
    /// Create an Ast from the passed tokens. You probably want [`parse`](../fn.parse.html)
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

    /// The entire code of the function
    pub fn nodes(&self) -> &Block<'a> {
        &self.nodes
    }

    /// The entire code of the function, but mutable
    pub fn nodes_mut(&mut self) -> &mut Block<'a> {
        &mut self.nodes
    }

    /// An iterator over the tokens used to create the Ast
    pub fn iter_tokens(&self) -> impl Iterator<Item = &Token<'a>> {
        self.tokens.iter()
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
