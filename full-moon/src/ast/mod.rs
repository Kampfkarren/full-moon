pub mod owned;
#[macro_use]
mod parser_util;
mod parsers;
pub mod punctuated;
pub mod span;
mod update_positions;
mod visitors;

use crate::{
    tokenizer::{Symbol, Token, TokenReference, TokenType},
    util::*,
};
use derive_more::Display;
use full_moon_derive::{Node, Owned, Visit};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::{borrow::Cow, fmt};

use parser_util::{
    InternalAstError, OneOrMore, Parser, ParserState, ZeroOrMore, ZeroOrMoreDelimited,
};

use punctuated::{Pair, Punctuated};
use span::ContainedSpan;

#[cfg(feature = "roblox")]
pub mod types;
#[cfg(feature = "roblox")]
use types::*;

#[cfg(feature = "roblox")]
mod type_visitors;

/// A block of statements, such as in if/do/etc block
#[derive(Clone, Debug, Default, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}",
    "display_optional_punctuated_vec(stmts)",
    "display_option(&last_stmt.as_ref().map(display_optional_punctuated))"
)]
pub struct Block<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    stmts: Vec<(Stmt<'a>, Option<Cow<'a, TokenReference<'a>>>)>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    last_stmt: Option<(LastStmt<'a>, Option<Cow<'a, TokenReference<'a>>>)>,
}

impl<'a> Block<'a> {
    /// Creates an empty block
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            last_stmt: None,
        }
    }

    /// An iterator over the statements in the block, such as `local foo = 1`
    pub fn iter_stmts(&self) -> impl Iterator<Item = &Stmt<'a>> {
        self.stmts.iter().map(|(stmt, _)| stmt)
    }

    /// An iterator over the statements in the block, including any optional
    /// semicolon token reference present
    pub fn iter_stmts_with_semicolon(
        &self,
    ) -> impl Iterator<Item = &(Stmt<'a>, Option<Cow<'a, TokenReference<'a>>>)> {
        self.stmts.iter()
    }

    /// The last statement of the block if one exists, such as `return foo`
    /// Deprecated in favor of [`Block::last_stmt`],
    /// the plural in `last_stmts` was a typo
    #[deprecated(since = "0.5.0", note = "Use last_stmt instead")]
    pub fn last_stmts(&self) -> Option<&LastStmt<'a>> {
        self.last_stmt()
    }

    /// The last statement of the block if one exists, such as `return foo`
    pub fn last_stmt(&self) -> Option<&LastStmt<'a>> {
        Some(&self.last_stmt.as_ref()?.0)
    }

    /// Returns a new block with the given statements
    /// Takes a vector of statements, followed by an optional semicolon token reference
    pub fn with_stmts(self, stmts: Vec<(Stmt<'a>, Option<Cow<'a, TokenReference<'a>>>)>) -> Self {
        Self { stmts, ..self }
    }

    /// Returns a new block with the given last statement, if one is given
    /// Takes an optional last statement, with an optional semicolon
    pub fn with_last_stmt(
        self,
        last_stmt: Option<(LastStmt<'a>, Option<Cow<'a, TokenReference<'a>>>)>,
    ) -> Self {
        Self { last_stmt, ..self }
    }
}

/// The last statement of a [`Block`]
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LastStmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A `break` statement
    Break(Cow<'a, TokenReference<'a>>),
    /// A continue statement
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    Continue(Cow<'a, TokenReference<'a>>),
    /// A `return` statement
    Return(Return<'a>),
}

/// A `return` statement
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", token, returns)]
pub struct Return<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    token: Cow<'a, TokenReference<'a>>,
    returns: Punctuated<'a, Expression<'a>>,
}

impl<'a> Return<'a> {
    /// Creates a new empty Return
    /// Default return token is followed by a single space
    pub fn new() -> Self {
        Self {
            token: Cow::Owned(TokenReference::symbol("return ").unwrap()),
            returns: Punctuated::new(),
        }
    }

    /// The `return` token
    pub fn token(&self) -> &TokenReference<'a> {
        &self.token
    }

    /// The values being returned
    pub fn returns(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.returns
    }

    /// Returns a new Return with the given `return` token
    pub fn with_token(self, token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { token, ..self }
    }

    /// Returns a new Return with the given punctuated sequence
    pub fn with_returns(self, returns: Punctuated<'a, Expression<'a>>) -> Self {
        Self { returns, ..self }
    }
}

impl Default for Return<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// Fields of a [`TableConstructor`]
#[derive(Clone, Debug, Display, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Field<'a> {
    /// A key in the format of `[expression] = value`
    #[display(
        fmt = "{}{}{}{}{}",
        "brackets.tokens().0",
        "key",
        "brackets.tokens().1",
        "equal",
        "value"
    )]
    ExpressionKey {
        /// The `[...]` part of `[expression] = value`
        #[cfg_attr(feature = "serde", serde(borrow))]
        brackets: ContainedSpan<'a>,
        /// The `expression` part of `[expression] = value`
        key: Expression<'a>,
        /// The `=` part of `[expression] = value`
        equal: Cow<'a, TokenReference<'a>>,
        /// The `value` part of `[expression] = value`
        value: Expression<'a>,
    },

    /// A key in the format of `name = value`
    #[display(fmt = "{}{}{}", "key", "equal", "value")]
    NameKey {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `name` part of `name = value`
        key: Cow<'a, TokenReference<'a>>,
        /// The `=` part of `name = value`
        equal: Cow<'a, TokenReference<'a>>,
        /// The `value` part of `name = value`
        value: Expression<'a>,
    },

    /// A field with no key, just a value (such as `"a"` in `{ "a" }`)
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[display(fmt = "{}", "_0")]
    NoKey(Expression<'a>),
}

/// A table being constructed, such as `{ 1, 2, 3 }` or `{ a = 1 }`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "braces.tokens().0", "fields", "braces.tokens().1")]
pub struct TableConstructor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[node(full_range)]
    #[visit(contains = "fields")]
    braces: ContainedSpan<'a>,
    fields: Punctuated<'a, Field<'a>>,
}

impl<'a> TableConstructor<'a> {
    /// Creates a new empty TableConstructor
    /// Brace tokens are followed by spaces, such that { `fields` }
    pub fn new() -> Self {
        Self {
            braces: ContainedSpan::new(
                Cow::Owned(TokenReference::symbol("{ ").unwrap()),
                Cow::Owned(TokenReference::symbol(" }").unwrap()),
            ),
            fields: Punctuated::new(),
        }
    }

    /// The braces of the constructor
    pub fn braces(&self) -> &ContainedSpan<'a> {
        &self.braces
    }

    /// An iterator over the fields used to create the table
    #[deprecated(note = "Please use fields().iter instead")]
    pub fn iter_fields(&self) -> impl Iterator<Item = &Field<'a>> {
        self.fields.iter()
    }

    /// Returns the [`Punctuated`] sequence of the fields used to create the table
    pub fn fields(&self) -> &Punctuated<'a, Field<'a>> {
        &self.fields
    }

    /// Returns a new TableConstructor with the given braces
    pub fn with_braces(self, braces: ContainedSpan<'a>) -> Self {
        Self { braces, ..self }
    }

    /// Returns a new TableConstructor with the given fields
    pub fn with_fields(self, fields: Punctuated<'a, Field<'a>>) -> Self {
        Self { fields, ..self }
    }
}

impl Default for TableConstructor<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// A binary operation, such as (`+ 3`)
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", bin_op, rhs)]
#[visit(visit_as = "bin_op")]
pub struct BinOpRhs<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    bin_op: BinOp<'a>,
    rhs: Box<Expression<'a>>,
}

impl<'a> BinOpRhs<'a> {
    /// Creates a new BinOpRhs from the given binary operator and right hand side
    pub fn new(bin_op: BinOp<'a>, rhs: Box<Expression<'a>>) -> Self {
        Self { bin_op, rhs }
    }

    /// The binary operation used, the `+` part of `+ 3`
    pub fn bin_op(&self) -> &BinOp<'a> {
        &self.bin_op
    }

    /// The right hand side of the binary operation, the `3` part of `+ 3`
    pub fn rhs(&self) -> &Expression<'a> {
        self.rhs.as_ref()
    }

    /// Returns a new BinOpRhs with the given binary operator token
    pub fn with_bin_op(self, bin_op: BinOp<'a>) -> Self {
        Self { bin_op, ..self }
    }

    /// Returns a new BinOpRhs with the given right hand side
    pub fn with_rhs(self, rhs: Box<Expression<'a>>) -> Self {
        Self { rhs, ..self }
    }
}

/// An expression, mostly useful for getting values
#[derive(Clone, Debug, Display, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum Expression<'a> {
    /// A statement in parentheses, such as `(#list)`
    #[display(
        fmt = "{}{}{}",
        "contained.tokens().0",
        "expression",
        "contained.tokens().1"
    )]
    Parentheses {
        /// The parentheses of the `ParenExpression`
        #[cfg_attr(feature = "serde", serde(borrow))]
        #[node(full_range)]
        contained: ContainedSpan<'a>,
        /// The expression inside the parentheses
        expression: Box<Expression<'a>>,
    },

    /// A unary operation, such as `#list`
    #[display(fmt = "{}{}", "unop", "expression")]
    UnaryOperator {
        /// The unary operation, the `#` part of `#list`
        #[cfg_attr(feature = "serde", serde(borrow))]
        unop: UnOp<'a>,
        /// The expression the operation is being done on, the `list` part of `#list`
        expression: Box<Expression<'a>>,
    },

    /// A value, such as "strings"
    #[cfg_attr(
        not(feature = "roblox"),
        display(fmt = "{}{}", value, "display_option(binop)")
    )]
    #[cfg_attr(
        feature = "roblox",
        display(
            fmt = "{}{}{}",
            value,
            "display_option(binop)",
            "display_option(as_assertion)"
        )
    )]
    Value {
        /// The value itself
        #[cfg_attr(feature = "serde", serde(borrow))]
        value: Box<Value<'a>>,
        /// The binary operation being done, if one exists (the `+ 3` part of `2 + 3`)
        binop: Option<BinOpRhs<'a>>,
        /// What the value is being asserted as using `as`.
        /// Only available when the "roblox" feature flag is enabled.
        #[cfg(feature = "roblox")]
        #[cfg_attr(feature = "serde", serde(borrow))]
        #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
        as_assertion: Option<AsAssertion<'a>>,
    },
}

/// Values that cannot be used standalone, but as part of things such as [`Stmt`]
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Value<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An anonymous function, such as `function() end)`
    #[display(fmt = "{}{}", "_0.0", "_0.1")]
    Function((Cow<'a, TokenReference<'a>>, FunctionBody<'a>)),
    /// A call of a function, such as `call()`
    #[display(fmt = "{}", "_0")]
    FunctionCall(FunctionCall<'a>),
    /// A table constructor, such as `{ 1, 2, 3 }`
    #[display(fmt = "{}", "_0")]
    TableConstructor(TableConstructor<'a>),
    /// A number token, such as `3.3`
    #[display(fmt = "{}", "_0")]
    Number(Cow<'a, TokenReference<'a>>),
    /// An expression between parentheses, such as `(3 + 2)`
    #[display(fmt = "{}", "_0")]
    ParenthesesExpression(Expression<'a>),
    /// A string token, such as `"hello"`
    #[display(fmt = "{}", "_0")]
    String(Cow<'a, TokenReference<'a>>),
    /// A symbol, such as `true`
    #[display(fmt = "{}", "_0")]
    Symbol(Cow<'a, TokenReference<'a>>),
    /// A more complex value, such as `call().x`
    #[display(fmt = "{}", "_0")]
    Var(Var<'a>),
}

/// A statement that stands alone
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An assignment, such as `x = 1`
    #[display(fmt = "{}", _0)]
    Assignment(Assignment<'a>),
    /// A do block, `do end`
    #[display(fmt = "{}", _0)]
    Do(Do<'a>),
    /// A function call on its own, such as `call()`
    #[display(fmt = "{}", _0)]
    FunctionCall(FunctionCall<'a>),
    /// A function declaration, such as `function x() end`
    #[display(fmt = "{}", _0)]
    FunctionDeclaration(FunctionDeclaration<'a>),
    /// A generic for loop, such as `for index, value in pairs(list) do end`
    #[display(fmt = "{}", _0)]
    GenericFor(GenericFor<'a>),
    /// An if statement
    #[display(fmt = "{}", _0)]
    If(If<'a>),
    /// A local assignment, such as `local x = 1`
    #[display(fmt = "{}", _0)]
    LocalAssignment(LocalAssignment<'a>),
    /// A local function declaration, such as `local function x() end`
    #[display(fmt = "{}", _0)]
    LocalFunction(LocalFunction<'a>),
    /// A numeric for loop, such as `for index = 1, 10 do end`
    #[display(fmt = "{}", _0)]
    NumericFor(NumericFor<'a>),
    /// A repeat loop
    #[display(fmt = "{}", _0)]
    Repeat(Repeat<'a>),
    /// A while loop
    #[display(fmt = "{}", _0)]
    While(While<'a>),

    /// A compound assignment, such as `+=`
    /// Only available when the "roblox" feature flag is enabled
    #[cfg(feature = "roblox")]
    #[display(fmt = "{}", _0)]
    CompoundAssignment(CompoundAssignment<'a>),
    /// An exported type declaration, such as `export type Meters = number`
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    ExportedTypeDeclaration(ExportedTypeDeclaration<'a>),
    /// A type declaration, such as `type Meters = number`
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    TypeDeclaration(TypeDeclaration<'a>),
}

/// A node used before another in cases such as function calling
/// The `("foo")` part of `("foo"):upper()`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Prefix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[display(fmt = "{}", _0)]
    /// A complicated expression, such as `("foo")`
    Expression(Expression<'a>),
    #[display(fmt = "{}", _0)]
    /// Just a name, such as `foo`
    Name(Cow<'a, TokenReference<'a>>),
}

/// The indexing of something, such as `x.y` or `x["y"]`
/// Values of variants are the keys, such as `"y"`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Index<'a> {
    /// Indexing in the form of `x["y"]`
    #[display(
        fmt = "{}{}{}",
        "brackets.tokens().0",
        "expression",
        "brackets.tokens().1"
    )]
    Brackets {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `[...]` part of `["y"]`
        brackets: ContainedSpan<'a>,
        /// The `"y"` part of `["y"]`
        expression: Expression<'a>,
    },

    /// Indexing in the form of `x.y`
    #[display(fmt = "{}{}", "dot", "name")]
    Dot {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `.` part of `.y`
        dot: Cow<'a, TokenReference<'a>>,
        /// The `y` part of `.y`
        name: Cow<'a, TokenReference<'a>>,
    },
}

/// Arguments used for a function
#[derive(Clone, Debug, Display, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum FunctionArgs<'a> {
    /// Used when a function is called in the form of `call(1, 2, 3)`
    #[display(
        fmt = "{}{}{}",
        "parentheses.tokens().0",
        "arguments",
        "parentheses.tokens().1"
    )]
    Parentheses {
        /// The `(...) part of (1, 2, 3)`
        #[node(full_range)]
        parentheses: ContainedSpan<'a>,
        /// The `1, 2, 3` part of `1, 2, 3`
        #[cfg_attr(feature = "serde", serde(borrow))]
        arguments: Punctuated<'a, Expression<'a>>,
    },
    /// Used when a function is called in the form of `call "foobar"`
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[display(fmt = "{}", "_0")]
    String(Cow<'a, TokenReference<'a>>),
    /// Used when a function is called in the form of `call { 1, 2, 3 }`
    #[display(fmt = "{}", "_0")]
    TableConstructor(TableConstructor<'a>),
}

/// A numeric for loop, such as `for index = 1, 10 do end`
#[derive(Clone, Debug, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct NumericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    for_token: Cow<'a, TokenReference<'a>>,
    index_variable: Cow<'a, TokenReference<'a>>,
    equal_token: Cow<'a, TokenReference<'a>>,
    start: Expression<'a>,
    start_end_comma: Cow<'a, TokenReference<'a>>,
    end: Expression<'a>,
    end_step_comma: Option<Cow<'a, TokenReference<'a>>>,
    step: Option<Expression<'a>>,
    do_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
    end_token: Cow<'a, TokenReference<'a>>,
    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(borrow))]
    type_specifier: Option<TypeSpecifier<'a>>,
}

impl<'a> NumericFor<'a> {
    /// Creates a new NumericFor from the given index variable, start, and end expressions
    pub fn new(
        index_variable: Cow<'a, TokenReference<'a>>,
        start: Expression<'a>,
        end: Expression<'a>,
    ) -> Self {
        Self {
            for_token: Cow::Owned(TokenReference::symbol("for ").unwrap()),
            index_variable,
            equal_token: Cow::Owned(TokenReference::symbol(" = ").unwrap()),
            start,
            start_end_comma: Cow::Owned(TokenReference::symbol(", ").unwrap()),
            end,
            end_step_comma: None,
            step: None,
            do_token: Cow::Owned(TokenReference::symbol(" do\n").unwrap()),
            block: Block::new(),
            end_token: Cow::Owned(TokenReference::symbol("\nend").unwrap()),
            #[cfg(feature = "roblox")]
            type_specifier: None,
        }
    }

    /// The `for` token
    pub fn for_token(&self) -> &TokenReference<'a> {
        &self.for_token
    }

    /// The index identity, `index` in the initial example
    pub fn index_variable(&self) -> &TokenReference<'a> {
        &self.index_variable
    }

    /// The `=` token
    pub fn equal_token(&self) -> &TokenReference<'a> {
        &self.equal_token
    }

    /// The starting point, `1` in the initial example
    pub fn start(&self) -> &Expression<'a> {
        &self.start
    }

    /// The comma in between the starting point and end point
    /// for _ = 1, 10 do
    ///          ^
    pub fn start_end_comma(&self) -> &TokenReference<'a> {
        &self.start_end_comma
    }

    /// The ending point, `10` in the initial example
    pub fn end(&self) -> &Expression<'a> {
        &self.end
    }

    /// The comma in between the ending point and limit, if one exists
    /// for _ = 0, 10, 2 do
    ///              ^
    pub fn end_step_comma(&self) -> Option<&TokenReference<'a>> {
        self.end_step_comma.as_deref()
    }

    /// The step if one exists, `2` in `for index = 0, 10, 2 do end`
    pub fn step(&self) -> Option<&Expression<'a>> {
        self.step.as_ref()
    }

    /// The `do` token
    pub fn do_token(&self) -> &TokenReference<'a> {
        &self.do_token
    }

    /// The code inside the for loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `end` token
    pub fn end_token(&self) -> &TokenReference<'a> {
        &self.end_token
    }

    /// The type specifiers of the index variable
    /// `for i: number = 1, 10 do` returns:
    /// `Some(TypeSpecifier(number))`
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn type_specifier(&self) -> Option<&TypeSpecifier<'a>> {
        self.type_specifier.as_ref()
    }

    /// Returns a new NumericFor with the given for token
    pub fn with_for_token(self, for_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { for_token, ..self }
    }

    /// Returns a new NumericFor with the given index variable
    pub fn with_index_variable(self, index_variable: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            index_variable,
            ..self
        }
    }

    /// Returns a new NumericFor with the given `=` token
    pub fn with_equal_token(self, equal_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            equal_token,
            ..self
        }
    }

    /// Returns a new NumericFor with the given start expression
    pub fn with_start(self, start: Expression<'a>) -> Self {
        Self { start, ..self }
    }

    /// Returns a new NumericFor with the given comma between the start and end expressions
    pub fn with_start_end_comma(self, start_end_comma: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            start_end_comma,
            ..self
        }
    }

    /// Returns a new NumericFor with the given end expression
    pub fn with_end(self, end: Expression<'a>) -> Self {
        Self { end, ..self }
    }

    /// Returns a new NumericFor with the given comma between the end and the step expressions
    pub fn with_end_step_comma(self, end_step_comma: Option<Cow<'a, TokenReference<'a>>>) -> Self {
        Self {
            end_step_comma,
            ..self
        }
    }

    /// Returns a new NumericFor with the given step expression
    pub fn with_step(self, step: Option<Expression<'a>>) -> Self {
        Self { step, ..self }
    }

    /// Returns a new NumericFor with the given `do` token
    pub fn with_do_token(self, do_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { do_token, ..self }
    }

    /// Returns a new NumericFor with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new NumericFor with the given `end` token
    pub fn with_end_token(self, end_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { end_token, ..self }
    }

    /// Returns a new NumericFor with the given type specifiers
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn with_type_specifier(self, type_specifier: Option<TypeSpecifier<'a>>) -> Self {
        Self {
            type_specifier,
            ..self
        }
    }
}

impl fmt::Display for NumericFor<'_> {
    #[cfg(feature = "roblox")]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}{}{}{}{}{}{}{}{}",
            self.for_token,
            self.index_variable,
            display_option(self.type_specifier()),
            self.equal_token,
            self.start,
            self.start_end_comma,
            self.end,
            display_option(self.end_step_comma()),
            display_option(self.step()),
            self.do_token,
            self.block,
            self.end_token,
        )
    }

    #[cfg(not(feature = "roblox"))]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}{}{}{}{}{}{}{}",
            self.for_token,
            self.index_variable,
            self.equal_token,
            self.start,
            self.start_end_comma,
            self.end,
            display_option(self.end_step_comma()),
            display_option(self.step()),
            self.do_token,
            self.block,
            self.end_token,
        )
    }
}

/// A generic for loop, such as `for index, value in pairs(list) do end`
#[derive(Clone, Debug, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    for_token: Cow<'a, TokenReference<'a>>,
    names: Punctuated<'a, Cow<'a, TokenReference<'a>>>,
    in_token: Cow<'a, TokenReference<'a>>,
    expr_list: Punctuated<'a, Expression<'a>>,
    do_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
    end_token: Cow<'a, TokenReference<'a>>,
    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(borrow))]
    type_specifiers: Vec<Option<TypeSpecifier<'a>>>,
}

impl<'a> GenericFor<'a> {
    /// Creates a new GenericFor from the given names and expressions
    pub fn new(
        names: Punctuated<'a, Cow<'a, TokenReference<'a>>>,
        expr_list: Punctuated<'a, Expression<'a>>,
    ) -> Self {
        Self {
            for_token: Cow::Owned(TokenReference::symbol("for ").unwrap()),
            names,
            in_token: Cow::Owned(TokenReference::symbol(" in ").unwrap()),
            expr_list,
            do_token: Cow::Owned(TokenReference::symbol(" do\n").unwrap()),
            block: Block::new(),
            end_token: Cow::Owned(TokenReference::symbol("\nend").unwrap()),
            #[cfg(feature = "roblox")]
            type_specifiers: Vec::new(),
        }
    }

    /// The `for` token
    pub fn for_token(&self) -> &TokenReference<'a> {
        &self.for_token
    }

    /// Returns the punctuated sequence of names
    /// In `for index, value in pairs(list) do`, iterates over `index` and `value`
    pub fn names(&self) -> &Punctuated<'a, Cow<'a, TokenReference<'a>>> {
        &self.names
    }

    /// The `in` token
    pub fn in_token(&self) -> &TokenReference<'a> {
        &self.in_token
    }

    /// Returns the punctuated sequence of the expressions looped over
    /// In `for index, value in pairs(list) do`, iterates over `pairs(list)`
    pub fn expr_list(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.expr_list
    }

    /// The `do` token
    pub fn do_token(&self) -> &TokenReference<'a> {
        &self.do_token
    }

    /// The code inside the for loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `end` token
    pub fn end_token(&self) -> &TokenReference<'a> {
        &self.end_token
    }

    /// The type specifiers of the named variables, in the order that they were assigned.
    /// `for i, v: string in pairs() do` returns an iterator containing:
    /// `None, Some(TypeSpecifier(string))`
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn type_specifiers(&self) -> impl Iterator<Item = Option<&TypeSpecifier<'a>>> {
        self.type_specifiers.iter().map(Option::as_ref)
    }

    /// Returns a new GenericFor with the given `for` token
    pub fn with_for_token(self, for_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { for_token, ..self }
    }

    /// Returns a new GenericFor with the given names
    pub fn with_names(self, names: Punctuated<'a, Cow<'a, TokenReference<'a>>>) -> Self {
        Self { names, ..self }
    }

    /// Returns a new GenericFor with the given `in` token
    pub fn with_in_token(self, in_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { in_token, ..self }
    }

    /// Returns a new GenericFor with the given expression list
    pub fn with_expr_list(self, expr_list: Punctuated<'a, Expression<'a>>) -> Self {
        Self { expr_list, ..self }
    }

    /// Returns a new GenericFor with the given `do` token
    pub fn with_do_token(self, do_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { do_token, ..self }
    }

    /// Returns a new GenericFor with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new GenericFor with the given `end` token
    pub fn with_end_token(self, end_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { end_token, ..self }
    }

    /// Returns a new GenericFor with the given type specifiers
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn with_type_specifiers(self, type_specifiers: Vec<Option<TypeSpecifier<'a>>>) -> Self {
        Self {
            type_specifiers,
            ..self
        }
    }
}

impl fmt::Display for GenericFor<'_> {
    #[cfg(feature = "roblox")]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}{}{}{}",
            self.for_token,
            join_type_specifiers(&self.names, self.type_specifiers()),
            self.in_token,
            self.expr_list,
            self.do_token,
            self.block,
            self.end_token
        )
    }

    #[cfg(not(feature = "roblox"))]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}{}{}{}",
            self.for_token,
            self.names,
            self.in_token,
            self.expr_list,
            self.do_token,
            self.block,
            self.end_token
        )
    }
}

/// An if statement
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}{}{}{}{}{}",
    "if_token",
    "condition",
    "then_token",
    "block",
    "display_option(else_if.as_ref().map(join_vec))",
    "display_option(else_token)",
    "display_option(r#else)",
    "end_token"
)]
pub struct If<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    if_token: Cow<'a, TokenReference<'a>>,
    condition: Expression<'a>,
    then_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
    else_if: Option<Vec<ElseIf<'a>>>,
    else_token: Option<Cow<'a, TokenReference<'a>>>,
    #[cfg_attr(feature = "serde", serde(rename = "else"))]
    r#else: Option<Block<'a>>,
    end_token: Cow<'a, TokenReference<'a>>,
}

impl<'a> If<'a> {
    /// Creates a new If from the given condition
    pub fn new(condition: Expression<'a>) -> Self {
        Self {
            if_token: Cow::Owned(TokenReference::symbol("if ").unwrap()),
            condition,
            then_token: Cow::Owned(TokenReference::symbol(" then").unwrap()),
            block: Block::new(),
            else_if: None,
            else_token: None,
            r#else: None,
            end_token: Cow::Owned(TokenReference::symbol("\nend").unwrap()),
        }
    }

    /// The `if` token
    pub fn if_token(&self) -> &TokenReference<'a> {
        &self.if_token
    }

    /// The condition of the if statement, `condition` in `if condition then`
    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    /// The `then` token
    pub fn then_token(&self) -> &TokenReference<'a> {
        &self.then_token
    }

    /// The block inside the initial if statement
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `else` token if one exists
    pub fn else_token(&self) -> Option<&TokenReference<'a>> {
        self.else_token.as_deref()
    }

    /// If there are `elseif` conditions, returns a vector of them
    /// Expression is the condition, block is the code if the condition is true
    // TODO: Make this return an iterator, and remove Option part entirely?
    pub fn else_if(&self) -> Option<&Vec<ElseIf<'a>>> {
        self.else_if.as_ref()
    }

    /// The code inside an `else` block if one exists
    pub fn else_block(&self) -> Option<&Block<'a>> {
        self.r#else.as_ref()
    }

    /// The `end` token
    pub fn end_token(&self) -> &TokenReference<'a> {
        &self.end_token
    }

    /// Returns a new If with the given `if` token
    pub fn with_if_token(self, if_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { if_token, ..self }
    }

    /// Returns a new If with the given condition
    pub fn with_condition(self, condition: Expression<'a>) -> Self {
        Self { condition, ..self }
    }

    /// Returns a new If with the given `then` token
    pub fn with_then_token(self, then_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { then_token, ..self }
    }

    /// Returns a new If with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new If with the given list of `elseif` blocks
    pub fn with_else_if(self, else_if: Option<Vec<ElseIf<'a>>>) -> Self {
        Self { else_if, ..self }
    }

    /// Returns a new If with the given `else` token
    pub fn with_else_token(self, else_token: Option<Cow<'a, TokenReference<'a>>>) -> Self {
        Self { else_token, ..self }
    }

    /// Returns a new If with the given `else` body
    pub fn with_else(self, r#else: Option<Block<'a>>) -> Self {
        Self { r#else, ..self }
    }

    /// Returns a new If with the given `end` token
    pub fn with_end_token(self, end_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { end_token, ..self }
    }
}

/// An elseif block in a bigger [`If`] statement
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}{}", "else_if_token", "condition", "then_token", "block")]
pub struct ElseIf<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    else_if_token: Cow<'a, TokenReference<'a>>,
    condition: Expression<'a>,
    then_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
}

impl<'a> ElseIf<'a> {
    /// Creates a new ElseIf from the given condition
    pub fn new(condition: Expression<'a>) -> Self {
        Self {
            else_if_token: Cow::Owned(TokenReference::symbol("elseif ").unwrap()),
            condition,
            then_token: Cow::Owned(TokenReference::symbol(" then\n").unwrap()),
            block: Block::new(),
        }
    }

    /// The `elseif` token
    pub fn else_if_token(&self) -> &TokenReference<'a> {
        &self.else_if_token
    }

    /// The condition of the `elseif`, `condition` in `elseif condition then`
    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    /// The `then` token
    pub fn then_token(&self) -> &TokenReference<'a> {
        &self.then_token
    }

    /// The body of the `elseif`
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// Returns a new ElseIf with the given `elseif` token
    pub fn with_else_if_token(self, else_if_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            else_if_token,
            ..self
        }
    }

    /// Returns a new ElseIf with the given condition
    pub fn with_condition(self, condition: Expression<'a>) -> Self {
        Self { condition, ..self }
    }

    /// Returns a new ElseIf with the given `then` token
    pub fn with_then_token(self, then_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { then_token, ..self }
    }

    /// Returns a new ElseIf with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }
}

/// A while loop
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}{}{}",
    "while_token",
    "condition",
    "do_token",
    "block",
    "end_token"
)]
pub struct While<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    while_token: Cow<'a, TokenReference<'a>>,
    condition: Expression<'a>,
    do_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
    end_token: Cow<'a, TokenReference<'a>>,
}

impl<'a> While<'a> {
    /// Creates a new While from the given condition
    pub fn new(condition: Expression<'a>) -> Self {
        Self {
            while_token: Cow::Owned(TokenReference::symbol("while ").unwrap()),
            condition,
            do_token: Cow::Owned(TokenReference::symbol(" do\n").unwrap()),
            block: Block::new(),
            end_token: Cow::Owned(TokenReference::symbol("end\n").unwrap()),
        }
    }

    /// The `while` token
    pub fn while_token(&self) -> &TokenReference<'a> {
        &self.while_token
    }

    /// The `condition` part of `while condition do`
    pub fn condition(&self) -> &Expression<'a> {
        &self.condition
    }

    /// The `do` token
    pub fn do_token(&self) -> &TokenReference<'a> {
        &self.do_token
    }

    /// The code inside the while loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `end` token
    pub fn end_token(&self) -> &TokenReference<'a> {
        &self.end_token
    }

    /// Returns a new While with the given `while` token
    pub fn with_while_token(self, while_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            while_token,
            ..self
        }
    }

    /// Returns a new While with the given condition
    pub fn with_condition(self, condition: Expression<'a>) -> Self {
        Self { condition, ..self }
    }

    /// Returns a new While with the given `do` token
    pub fn with_do_token(self, do_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { do_token, ..self }
    }

    /// Returns a new While with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new While with the given `end` token
    pub fn with_end_token(self, end_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { end_token, ..self }
    }
}

/// A repeat loop
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}{}", "repeat_token", "block", "until_token", "until")]
pub struct Repeat<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    repeat_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
    until_token: Cow<'a, TokenReference<'a>>,
    until: Expression<'a>,
}

impl<'a> Repeat<'a> {
    /// Creates a new Repeat from the given expression to repeat until
    pub fn new(until: Expression<'a>) -> Self {
        Self {
            repeat_token: Cow::Owned(TokenReference::symbol("repeat\n").unwrap()),
            block: Block::new(),
            until_token: Cow::Owned(TokenReference::symbol("\nuntil ").unwrap()),
            until,
        }
    }

    /// The `repeat` token
    pub fn repeat_token(&self) -> &TokenReference<'a> {
        &self.repeat_token
    }

    /// The code inside the `repeat` block
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `until` token
    pub fn until_token(&self) -> &TokenReference<'a> {
        &self.until_token
    }

    /// The condition for the `until` part
    pub fn until(&self) -> &Expression<'a> {
        &self.until
    }

    /// Returns a new Repeat with the given `repeat` token
    pub fn with_repeat_token(self, repeat_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            repeat_token,
            ..self
        }
    }

    /// Returns a new Repeat with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new Repeat with the given `until` token
    pub fn with_until_token(self, until_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            until_token,
            ..self
        }
    }

    /// Returns a new Repeat with the given `until` block
    pub fn with_until(self, until: Expression<'a>) -> Self {
        Self { until, ..self }
    }
}

/// A method call, such as `x:y()`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "colon_token", "name", "args")]
pub struct MethodCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    colon_token: Cow<'a, TokenReference<'a>>,
    name: Cow<'a, TokenReference<'a>>,
    args: FunctionArgs<'a>,
}

impl<'a> MethodCall<'a> {
    /// Returns a new MethodCall from the given name and args
    pub fn new(name: Cow<'a, TokenReference<'a>>, args: FunctionArgs<'a>) -> Self {
        Self {
            colon_token: Cow::Owned(TokenReference::symbol(":").unwrap()),
            name,
            args,
        }
    }

    /// The `:` in `x:y()`
    pub fn colon_token(&self) -> &TokenReference<'a> {
        &self.colon_token
    }

    /// The arguments of a method call, the `x, y, z` part of `method:call(x, y, z)`
    pub fn args(&self) -> &FunctionArgs<'a> {
        &self.args
    }

    /// The method being called, the `call` part of `method:call()`
    pub fn name(&self) -> &TokenReference<'a> {
        &self.name
    }

    /// Returns a new MethodCall with the given `:` token
    pub fn with_colon_token(self, colon_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            colon_token,
            ..self
        }
    }

    /// Returns a new MethodCall with the given name
    pub fn with_name(self, name: Cow<'a, TokenReference<'a>>) -> Self {
        Self { name, ..self }
    }

    /// Returns a new MethodCall with the given args
    pub fn with_args(self, args: FunctionArgs<'a>) -> Self {
        Self { args, ..self }
    }
}

/// Something being called
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Call<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[display(fmt = "{}", "_0")]
    /// A function being called directly, such as `x(1)`
    AnonymousCall(FunctionArgs<'a>),
    #[display(fmt = "{}", "_0")]
    /// A method call, such as `x:y()`
    MethodCall(MethodCall<'a>),
}

/// A function body, everything except `function x` in `function x(a, b, c) call() end`
#[derive(Clone, Debug, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionBody<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    parameters_parentheses: ContainedSpan<'a>,
    parameters: Punctuated<'a, Parameter<'a>>,

    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(borrow))]
    type_specifiers: Vec<Option<TypeSpecifier<'a>>>,

    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    return_type: Option<TypeSpecifier<'a>>,

    block: Block<'a>,
    end_token: Cow<'a, TokenReference<'a>>,
}

impl<'a> FunctionBody<'a> {
    /// Returns a new empty FunctionBody
    pub fn new() -> Self {
        Self {
            parameters_parentheses: ContainedSpan::new(
                Cow::Owned(TokenReference::symbol("(").unwrap()),
                Cow::Owned(TokenReference::symbol(")").unwrap()),
            ),
            parameters: Punctuated::new(),

            #[cfg(feature = "roblox")]
            type_specifiers: Vec::new(),

            #[cfg(feature = "roblox")]
            return_type: None,

            block: Block::new(),
            end_token: Cow::Owned(TokenReference::symbol("\nend").unwrap()),
        }
    }

    /// The parentheses of the parameters
    pub fn parameters_parentheses(&self) -> &ContainedSpan<'a> {
        &self.parameters_parentheses
    }

    /// An iterator over the parameters for the function declaration
    /// Deprecated in favor of [`Punctuated::iter`], which supports retrieving punctuation too
    #[deprecated(note = "Please use parameters().iter instead")]
    pub fn iter_parameters(&self) -> impl Iterator<Item = &Parameter<'a>> {
        self.parameters.iter()
    }

    /// Returns the [`Punctuated`] sequence of the parameters for the function declaration
    pub fn parameters(&self) -> &Punctuated<'a, Parameter<'a>> {
        &self.parameters
    }

    /// The code of a function body
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `end` token
    pub fn end_token(&self) -> &TokenReference<'a> {
        &self.end_token
    }

    /// The type specifiers of the variables, in the order that they were assigned.
    /// `(foo: number, bar, baz: boolean)` returns an iterator containing:
    /// `Some(TypeSpecifier(number)), None, Some(TypeSpecifier(boolean))`
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn type_specifiers(&self) -> impl Iterator<Item = Option<&TypeSpecifier<'a>>> {
        self.type_specifiers.iter().map(Option::as_ref)
    }

    /// The return type of the function, if one exists.
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn return_type(&self) -> Option<&TypeSpecifier<'a>> {
        self.return_type.as_ref()
    }

    /// Returns a new FunctionBody with the given parentheses for the parameters
    pub fn with_parameters_parentheses(self, parameters_parentheses: ContainedSpan<'a>) -> Self {
        Self {
            parameters_parentheses,
            ..self
        }
    }

    /// Returns a new FunctionBody with the given parameters
    pub fn with_parameters(self, parameters: Punctuated<'a, Parameter<'a>>) -> Self {
        Self { parameters, ..self }
    }

    /// Returns a new FunctionBody with the given type specifiers
    #[cfg(feature = "roblox")]
    pub fn with_type_specifiers(self, type_specifiers: Vec<Option<TypeSpecifier<'a>>>) -> Self {
        Self {
            type_specifiers,
            ..self
        }
    }

    /// Returns a new FunctionBody with the given return type
    #[cfg(feature = "roblox")]
    pub fn with_return_type(self, return_type: Option<TypeSpecifier<'a>>) -> Self {
        Self {
            return_type,
            ..self
        }
    }

    /// Returns a new FunctionBody with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new FunctionBody with the given `end` token
    pub fn with_end_token(self, end_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { end_token, ..self }
    }
}

impl Default for FunctionBody<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl fmt::Display for FunctionBody<'_> {
    #[cfg(feature = "roblox")]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}{}{}",
            self.parameters_parentheses.tokens().0,
            join_type_specifiers(&self.parameters, self.type_specifiers()),
            self.parameters_parentheses.tokens().1,
            display_option(self.return_type.as_ref()),
            self.block,
            self.end_token
        )
    }

    #[cfg(not(feature = "roblox"))]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}{}",
            self.parameters_parentheses.tokens().0,
            self.parameters,
            self.parameters_parentheses.tokens().1,
            self.block,
            self.end_token
        )
    }
}

/// A parameter in a function declaration
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Parameter<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// The `...` vararg syntax, such as `function x(...)`
    Ellipse(Cow<'a, TokenReference<'a>>),
    /// A name parameter, such as `function x(a, b, c)`
    Name(Cow<'a, TokenReference<'a>>),
}

/// A suffix in certain cases, such as `:y()` in `x:y()`
/// Can be stacked on top of each other, such as in `x()()()`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Suffix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[display(fmt = "{}", "_0")]
    /// A call, including method calls and direct calls
    Call(Call<'a>),
    #[display(fmt = "{}", "_0")]
    /// An index, such as `x.y`
    Index(Index<'a>),
}

/// A complex expression used by [`Var`], consisting of both a prefix and suffixes
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", "prefix", "join_vec(suffixes)")]
pub struct VarExpression<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    prefix: Prefix<'a>,
    suffixes: Vec<Suffix<'a>>,
}

impl<'a> VarExpression<'a> {
    /// Returns a new VarExpression from the given prefix
    pub fn new(prefix: Prefix<'a>) -> Self {
        Self {
            prefix,
            suffixes: Vec::new(),
        }
    }

    /// The prefix of the expression, such as a name
    pub fn prefix(&self) -> &Prefix<'a> {
        &self.prefix
    }

    /// An iter over the suffixes, such as indexing or calling
    pub fn iter_suffixes(&self) -> impl Iterator<Item = &Suffix<'a>> {
        self.suffixes.iter()
    }

    /// Returns a new VarExpression with the given prefix
    pub fn with_prefix(self, prefix: Prefix<'a>) -> Self {
        Self { prefix, ..self }
    }

    /// Returns a new VarExpression with the given suffixes
    pub fn with_suffixes(self, suffixes: Vec<Suffix<'a>>) -> Self {
        Self { suffixes, ..self }
    }
}

/// Used in [`Assignment`s](Assignment) and [`Value`s](Value)
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Var<'a> {
    /// An expression, such as `x.y.z` or `x()`
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[display(fmt = "{}", "_0")]
    Expression(VarExpression<'a>),
    /// A literal identifier, such as `x`
    #[display(fmt = "{}", "_0")]
    Name(Cow<'a, TokenReference<'a>>),
}

/// An assignment, such as `x = y`. Not used for [`LocalAssignment`s](LocalAssignment)
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "var_list", "equal_token", "expr_list")]
pub struct Assignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    var_list: Punctuated<'a, Var<'a>>,
    equal_token: Cow<'a, TokenReference<'a>>,
    expr_list: Punctuated<'a, Expression<'a>>,
}

impl<'a> Assignment<'a> {
    /// Returns a new Assignment from the given variable and expression list
    pub fn new(
        var_list: Punctuated<'a, Var<'a>>,
        expr_list: Punctuated<'a, Expression<'a>>,
    ) -> Self {
        Self {
            var_list,
            equal_token: Cow::Owned(TokenReference::symbol(" = ").unwrap()),
            expr_list,
        }
    }

    /// Returns the punctuated sequence over the expressions being assigned.
    /// This is the the `1, 2` part of `x, y["a"] = 1, 2`
    pub fn expr_list(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.expr_list
    }

    /// The `=` token in between `x = y`
    pub fn equal_token(&self) -> &TokenReference<'a> {
        &self.equal_token
    }

    /// Returns the punctuated sequence over the variables being assigned to.
    /// This is the `x, y["a"]` part of `x, y["a"] = 1, 2`
    pub fn var_list(&self) -> &Punctuated<'a, Var<'a>> {
        &self.var_list
    }

    /// Returns a new Assignment with the given var list
    pub fn with_var_list(self, var_list: Punctuated<'a, Var<'a>>) -> Self {
        Self { var_list, ..self }
    }

    /// Returns a new Assignment with the given `=` token
    pub fn with_equal_token(self, equal_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            equal_token,
            ..self
        }
    }

    /// Returns a new Assignment with the given expressions
    pub fn with_expr_list(self, expr_list: Punctuated<'a, Expression<'a>>) -> Self {
        Self { expr_list, ..self }
    }
}

/// A declaration of a local function, such as `local function x() end`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}{}", "local_token", "function_token", "name", "func_body")]
pub struct LocalFunction<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    local_token: Cow<'a, TokenReference<'a>>,
    function_token: Cow<'a, TokenReference<'a>>,
    name: Cow<'a, TokenReference<'a>>,
    func_body: FunctionBody<'a>,
}

impl<'a> LocalFunction<'a> {
    /// Returns a new LocalFunction from the given name
    pub fn new(name: Cow<'a, TokenReference<'a>>) -> Self {
        LocalFunction {
            local_token: Cow::Owned(TokenReference::symbol("local ").unwrap()),
            function_token: Cow::Owned(TokenReference::symbol("function ").unwrap()),
            name,
            func_body: FunctionBody::new(),
        }
    }

    /// The `local` token
    pub fn local_token(&self) -> &TokenReference<'a> {
        &self.local_token
    }

    /// The `function` token
    pub fn function_token(&self) -> &TokenReference<'a> {
        &self.function_token
    }

    /// The function body, everything except `local function x` in `local function x(a, b, c) call() end`
    pub fn func_body(&self) -> &FunctionBody<'a> {
        &self.func_body
    }

    /// The name of the function, the `x` part of `local function x() end`
    pub fn name(&self) -> &TokenReference<'a> {
        &self.name
    }

    /// Returns a new LocalFunction with the given `local` token
    pub fn with_local_token(self, local_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            local_token,
            ..self
        }
    }

    /// Returns a new LocalFunction with the given `function` token
    pub fn with_function_token(self, function_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            function_token,
            ..self
        }
    }

    /// Returns a new LocalFunction with the given name
    pub fn with_name(self, name: Cow<'a, TokenReference<'a>>) -> Self {
        Self { name, ..self }
    }

    /// Returns a new LocalFunction with the given function body
    pub fn with_func_body(self, func_body: FunctionBody<'a>) -> Self {
        Self { func_body, ..self }
    }
}

/// An assignment to a local variable, such as `local x = 1`
#[derive(Clone, Debug, PartialEq, Owned, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    local_token: Cow<'a, TokenReference<'a>>,
    #[cfg(feature = "roblox")]
    #[cfg_attr(feature = "serde", serde(borrow))]
    type_specifiers: Vec<Option<TypeSpecifier<'a>>>,
    name_list: Punctuated<'a, Cow<'a, TokenReference<'a>>>,
    equal_token: Option<Cow<'a, TokenReference<'a>>>,
    expr_list: Punctuated<'a, Expression<'a>>,
}

impl<'a> LocalAssignment<'a> {
    /// Returns a new LocalAssignment from the given name list
    pub fn new(name_list: Punctuated<'a, Cow<'a, TokenReference<'a>>>) -> Self {
        Self {
            local_token: Cow::Owned(TokenReference::symbol("local ").unwrap()),
            #[cfg(feature = "roblox")]
            type_specifiers: Vec::new(),
            name_list,
            equal_token: None,
            expr_list: Punctuated::new(),
        }
    }

    /// The `local` token
    pub fn local_token(&self) -> &TokenReference<'a> {
        &self.local_token
    }

    /// The `=` token in between `local x = y`, if one exists
    pub fn equal_token(&self) -> Option<&TokenReference<'a>> {
        self.equal_token.as_deref()
    }

    /// Returns the punctuated sequence of the expressions being assigned.
    /// This is the `1, 2` part of `local x, y = 1, 2`
    pub fn expr_list(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.expr_list
    }

    /// Returns the punctuated sequence of names being assigned to.
    /// This is the `x, y` part of `local x, y = 1, 2`
    pub fn name_list(&self) -> &Punctuated<'a, Cow<'a, TokenReference<'a>>> {
        &self.name_list
    }

    /// The type specifiers of the variables, in the order that they were assigned.
    /// `local foo: number, bar, baz: boolean` returns an iterator containing:
    /// `Some(TypeSpecifier(number)), None, Some(TypeSpecifier(boolean))`
    /// Only available when the "roblox" feature flag is enabled.
    #[cfg(feature = "roblox")]
    pub fn type_specifiers(&self) -> impl Iterator<Item = Option<&TypeSpecifier<'a>>> {
        self.type_specifiers.iter().map(Option::as_ref)
    }

    /// Returns a new LocalAssignment with the given `local` token
    pub fn with_local_token(self, local_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            local_token,
            ..self
        }
    }

    /// Returns a new LocalAssignment with the given type specifiers
    #[cfg(feature = "roblox")]
    pub fn with_type_specifiers(self, type_specifiers: Vec<Option<TypeSpecifier<'a>>>) -> Self {
        Self {
            type_specifiers,
            ..self
        }
    }

    /// Returns a new LocalAssignment with the given name list
    pub fn with_name_list(self, name_list: Punctuated<'a, Cow<'a, TokenReference<'a>>>) -> Self {
        Self { name_list, ..self }
    }

    /// Returns a new LocalAssignment with the given `=` token
    pub fn with_equal_token(self, equal_token: Option<Cow<'a, TokenReference<'a>>>) -> Self {
        Self {
            equal_token,
            ..self
        }
    }

    /// Returns a new LocalAssignment with the given expression list
    pub fn with_expr_list(self, expr_list: Punctuated<'a, Expression<'a>>) -> Self {
        Self { expr_list, ..self }
    }
}

impl fmt::Display for LocalAssignment<'_> {
    #[cfg(feature = "roblox")]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}",
            self.local_token,
            join_type_specifiers(&self.name_list, self.type_specifiers()),
            display_option(&self.equal_token),
            self.expr_list
        )
    }

    #[cfg(not(feature = "roblox"))]
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        write!(
            formatter,
            "{}{}{}{}",
            self.local_token,
            self.name_list,
            display_option(&self.equal_token),
            self.expr_list
        )
    }
}

/// A `do` block, such as `do ... end`
/// This is not used for things like `while true do end`, only those on their own
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "do_token", "block", "end_token")]
pub struct Do<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    do_token: Cow<'a, TokenReference<'a>>,
    block: Block<'a>,
    end_token: Cow<'a, TokenReference<'a>>,
}

impl<'a> Do<'a> {
    /// Creates an empty Do
    pub fn new() -> Self {
        Self {
            do_token: Cow::Owned(TokenReference::symbol("do\n").unwrap()),
            block: Block::new(),
            end_token: Cow::Owned(TokenReference::symbol("\nend").unwrap()),
        }
    }

    /// The `do` token
    pub fn do_token(&self) -> &TokenReference<'a> {
        &self.do_token
    }

    /// The code inside the `do ... end`
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }

    /// The `end` token
    pub fn end_token(&self) -> &TokenReference<'a> {
        &self.end_token
    }

    /// Returns a new Do with the given `do` token
    pub fn with_do_token(self, do_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { do_token, ..self }
    }

    /// Returns a new Do with the given block
    pub fn with_block(self, block: Block<'a>) -> Self {
        Self { block, ..self }
    }

    /// Returns a new Do with the given `end` token
    pub fn with_end_token(self, end_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self { end_token, ..self }
    }
}

impl Default for Do<'_> {
    fn default() -> Self {
        Self::new()
    }
}

/// A function being called, such as `call()`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", "prefix", "join_vec(suffixes)")]
pub struct FunctionCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    prefix: Prefix<'a>,
    suffixes: Vec<Suffix<'a>>,
}

impl<'a> FunctionCall<'a> {
    /// Creates a new FunctionCall from the given prefix
    /// Sets the suffixes such that the return is `prefixes()`
    pub fn new(prefix: Prefix<'a>) -> Self {
        FunctionCall {
            prefix,
            suffixes: vec![Suffix::Call(Call::AnonymousCall(
                FunctionArgs::Parentheses {
                    arguments: Punctuated::new(),
                    parentheses: ContainedSpan::new(
                        Cow::Owned(TokenReference::symbol("(").unwrap()),
                        Cow::Owned(TokenReference::symbol(")").unwrap()),
                    ),
                },
            ))],
        }
    }

    /// The prefix of a function call, the `call` part of `call()`
    pub fn prefix(&self) -> &Prefix<'a> {
        &self.prefix
    }

    /// The suffix of a function call, the `()` part of `call()`
    pub fn iter_suffixes(&self) -> impl Iterator<Item = &Suffix<'a>> {
        self.suffixes.iter()
    }

    /// Returns a new FunctionCall with the given prefix
    pub fn with_prefix(self, prefix: Prefix<'a>) -> Self {
        Self { prefix, ..self }
    }

    /// Returns a new FunctionCall with the given suffixes
    pub fn with_suffixes(self, suffixes: Vec<Suffix<'a>>) -> Self {
        Self { suffixes, ..self }
    }
}

/// A function name when being declared as [`FunctionDeclaration`]
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(
    fmt = "{}{}{}",
    "names",
    "display_option(self.method_colon())",
    "display_option(self.method_name())"
)]
pub struct FunctionName<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    names: Punctuated<'a, Cow<'a, TokenReference<'a>>>,
    colon_name: Option<(Cow<'a, TokenReference<'a>>, Cow<'a, TokenReference<'a>>)>,
}

impl<'a> FunctionName<'a> {
    /// Creates a new FunctionName from the given list of names
    pub fn new(names: Punctuated<'a, Cow<'a, TokenReference<'a>>>) -> Self {
        Self {
            names,
            colon_name: None,
        }
    }

    /// The colon between the name and the method, the `:` part of `function x:y() end`
    pub fn method_colon(&self) -> Option<&TokenReference<'a>> {
        Some(&self.colon_name.as_ref()?.0)
    }

    /// A method name if one exists, the `y` part of `function x:y() end`
    pub fn method_name(&self) -> Option<&TokenReference<'a>> {
        Some(&self.colon_name.as_ref()?.1)
    }

    /// Returns the punctuated sequence over the names used when defining the function.
    /// This is the `x.y.z` part of `function x.y.z() end`
    pub fn names(&self) -> &Punctuated<'a, Cow<'a, TokenReference<'a>>> {
        &self.names
    }

    /// Returns a new FunctionName with the given names
    pub fn with_names(self, names: Punctuated<'a, Cow<'a, TokenReference<'a>>>) -> Self {
        Self { names, ..self }
    }

    /// Returns a new FunctionName with the given method name
    /// The first token is the colon, and the second token is the method name itself
    pub fn with_method(
        self,
        method: Option<(Cow<'a, TokenReference<'a>>, Cow<'a, TokenReference<'a>>)>,
    ) -> Self {
        Self {
            colon_name: method,
            ..self
        }
    }
}

/// A normal function declaration, supports simple declarations like `function x() end`
/// as well as complicated declarations such as `function x.y.z:a() end`
#[derive(Clone, Debug, Display, PartialEq, Owned, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "function_token", "name", "body")]
pub struct FunctionDeclaration<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    function_token: Cow<'a, TokenReference<'a>>,
    name: FunctionName<'a>,
    body: FunctionBody<'a>,
}

impl<'a> FunctionDeclaration<'a> {
    /// Creates a new FunctionDeclaration from the given name
    pub fn new(name: FunctionName<'a>) -> Self {
        Self {
            function_token: Cow::Owned(TokenReference::symbol("function ").unwrap()),
            name,
            body: FunctionBody::new(),
        }
    }

    /// The `function` token
    pub fn function_token(&self) -> &TokenReference<'a> {
        &self.function_token
    }

    /// The body of the function
    pub fn body(&self) -> &FunctionBody<'a> {
        &self.body
    }

    /// The name of the function
    pub fn name(&self) -> &FunctionName<'a> {
        &self.name
    }

    /// Returns a new FunctionDeclaration with the given `function` token
    pub fn with_function_token(self, function_token: Cow<'a, TokenReference<'a>>) -> Self {
        Self {
            function_token,
            ..self
        }
    }

    /// Returns a new FunctionDeclaration with the given function name
    pub fn with_name(self, name: FunctionName<'a>) -> Self {
        Self { name, ..self }
    }

    /// Returns a new FunctionDeclaration with the given function body
    pub fn with_body(self, body: FunctionBody<'a>) -> Self {
        Self { body, ..self }
    }
}

make_op!(BinOp,
    #[doc = "Operators that require two operands, such as X + Y or X - Y"]
    #[visit(skip_visit_self)]
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

make_op!(UnOp,
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
        #[cfg_attr(feature = "serde", serde(borrow))]
        token: Token<'a>,
        /// Any additional information that could be provided for debugging
        additional: Option<Cow<'a, str>>,
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
                token.start_position().line(),
                token.start_position().character(),
                token.end_position().line(),
                token.end_position().character(),
                match additional {
                    Some(additional) => format!("\nadditional information: {}", additional),
                    None => String::new(),
                }
            )
        }
    }
}

impl<'a> std::error::Error for AstError<'a> {}

/// An abstract syntax tree, contains all the nodes used in the code
#[derive(Clone, Debug, Owned)]
pub struct Ast<'a> {
    pub(crate) nodes: Block<'a>,
    pub(crate) tokens: Vec<TokenReference<'a>>,
}

impl<'a> Ast<'a> {
    /// Create an Ast from the passed tokens. You probably want [`parse`](crate::parse)
    ///
    /// # Errors
    ///
    /// If the tokens passed are impossible to get through normal tokenization,
    /// an error of Empty (if the vector is empty) or NoEof (if there is no eof token)
    /// will be returned.
    ///
    /// More likely, if the tokens pass are invalid Lua 5.1 code, an
    /// UnexpectedToken error will be returned.
    pub fn from_tokens(tokens: Vec<Token<'a>>) -> Result<Ast<'a>, AstError<'a>> {
        if *tokens.last().ok_or(AstError::Empty)?.token_type() != TokenType::Eof {
            Err(AstError::NoEof)
        } else {
            let tokens = extract_token_references(tokens);
            let mut state = ParserState::new(&tokens);

            if tokens
                .iter()
                .filter(|token| !token.token_type().is_trivia())
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
            if state.peek().token_type().is_trivia() {
                state = state.advance().unwrap();
            }

            match parsers::ParseBlock.parse(state) {
                Ok((state, block)) => {
                    if state.index == tokens.len() - 1 {
                        Ok(Ast {
                            nodes: block,
                            tokens,
                        })
                    } else {
                        Err(AstError::UnexpectedToken {
                            token: (*state.peek()).to_owned().token,
                            additional: Some(Cow::Borrowed("leftover token")),
                        })
                    }
                }

                Err(InternalAstError::NoMatch) => Err(AstError::UnexpectedToken {
                    token: (*state.peek()).to_owned().token,
                    additional: None,
                }),

                Err(InternalAstError::UnexpectedToken { token, additional }) => {
                    Err(AstError::UnexpectedToken {
                        token: (*token).to_owned(),
                        additional: additional.map(Cow::Borrowed),
                    })
                }
            }
        }
    }

    /// Returns a new Ast with the given nodes
    pub fn with_nodes(self, nodes: Block<'a>) -> Self {
        Self { nodes, ..self }
    }

    /// Returns a new Ast with the given EOF token
    pub fn with_eof(mut self, eof: TokenReference<'a>) -> Self {
        self.tokens.pop();
        self.tokens.push(eof);
        Self {
            tokens: self.tokens,
            ..self
        }
    }

    /// The entire code of the function
    ///
    /// ```rust
    /// # fn main() -> Result<(), Box<std::error::Error>> {
    /// assert_eq!(full_moon::parse("local x = 1; local y = 2")?.nodes().iter_stmts().count(), 2);
    /// # Ok(())
    /// # }
    /// ```
    pub fn nodes(&self) -> &Block<'a> {
        &self.nodes
    }

    /// The entire code of the function, but mutable
    pub fn nodes_mut(&mut self) -> &mut Block<'a> {
        &mut self.nodes
    }

    /// The EOF token at the end of every Ast
    pub fn eof(&self) -> &TokenReference<'a> {
        self.tokens.last().expect("no eof token, somehow?")
    }
}

/// Extracts leading and trailing trivia from tokens
pub(crate) fn extract_token_references<'a>(mut tokens: Vec<Token<'a>>) -> Vec<TokenReference<'a>> {
    let mut references = Vec::new();
    let (mut leading_trivia, mut trailing_trivia) = (Vec::new(), Vec::new());
    let mut tokens = tokens.drain(..).peekable();

    while let Some(token) = tokens.next() {
        if token.token_type().is_trivia() {
            leading_trivia.push(token);
        } else {
            while let Some(token) = tokens.peek() {
                if token.token_type().is_trivia() {
                    if let TokenType::Whitespace { ref characters } = &*token.token_type() {
                        // Use contains in order to tolerate \r\n line endings and mixed whitespace tokens
                        if characters.contains('\n') {
                            break;
                        }
                    }

                    trailing_trivia.push(tokens.next().unwrap());
                } else {
                    break;
                }
            }

            references.push(TokenReference {
                leading_trivia: leading_trivia.drain(..).collect(),
                trailing_trivia: trailing_trivia.drain(..).collect(),
                token,
            });
        }
    }

    references
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{parse, print, tokenizer::tokens, visitors::VisitorMut};

    #[test]
    fn test_extract_token_references() {
        let tokens = tokens("print(1)\n-- hello world\nlocal foo -- this is the word foo").unwrap();

        let references = extract_token_references(tokens);
        assert_eq!(references.len(), 7);

        assert!(references[0].trailing_trivia.is_empty());
        assert_eq!(references[0].token.to_string(), "print");
        assert!(references[0].leading_trivia.is_empty());

        assert!(references[1].trailing_trivia.is_empty());
        assert_eq!(references[1].token.to_string(), "(");
        assert!(references[1].leading_trivia.is_empty());

        assert!(references[2].trailing_trivia.is_empty());
        assert_eq!(references[2].token.to_string(), "1");
        assert!(references[2].leading_trivia.is_empty());

        assert_eq!(references[4].leading_trivia[0].to_string(), "\n");

        assert_eq!(
            references[4].leading_trivia[1].to_string(),
            "-- hello world",
        );

        assert_eq!(references[4].leading_trivia[2].to_string(), "\n");
        assert_eq!(references[4].token.to_string(), "local");
        assert_eq!(references[4].trailing_trivia[0].to_string(), " ");
    }

    #[test]
    fn test_with_eof_safety() {
        let new_ast = {
            let ast = parse("local foo = 1").unwrap();
            let eof = ast.eof().clone();
            ast.with_eof(eof)
        };

        print(&new_ast);
    }

    #[test]
    fn test_with_nodes_safety() {
        let new_ast = {
            let ast = parse("local foo = 1").unwrap();
            let nodes = ast.nodes().clone();
            ast.with_nodes(nodes)
        };

        print(&new_ast);
    }

    #[test]
    fn test_with_visitor_safety() {
        let new_ast = {
            let ast = parse("local foo = 1").unwrap();

            struct SyntaxRewriter;
            impl<'ast> VisitorMut<'ast> for SyntaxRewriter {
                fn visit_token(&mut self, token: Token<'ast>) -> Token<'ast> {
                    token
                }
            }

            SyntaxRewriter.visit_ast(ast)
        };

        print(&new_ast);
    }

    // Tests AST nodes with new methods that call unwrap
    #[test]
    fn test_new_validity() {
        let token: Cow<TokenReference> = Cow::Owned(TokenReference::new(
            Vec::new(),
            Token::new(TokenType::Identifier {
                identifier: "foo".into(),
            }),
            Vec::new(),
        ));

        let expression = Expression::Value {
            value: Box::new(Value::Var(Var::Name(token.clone()))),
            binop: None,
            #[cfg(feature = "roblox")]
            as_assertion: None,
        };

        Assignment::new(Punctuated::new(), Punctuated::new());
        Do::new();
        ElseIf::new(expression.clone());
        FunctionBody::new();
        FunctionCall::new(Prefix::Name(token.clone()));
        FunctionDeclaration::new(FunctionName::new(Punctuated::new()));
        GenericFor::new(Punctuated::new(), Punctuated::new());
        If::new(expression.clone());
        LocalAssignment::new(Punctuated::new());
        LocalFunction::new(token.clone());
        MethodCall::new(
            token.clone(),
            FunctionArgs::Parentheses {
                arguments: Punctuated::new(),
                parentheses: ContainedSpan::new(token.clone(), token.clone()),
            },
        );
        NumericFor::new(token.clone(), expression.clone(), expression.clone());
        Repeat::new(expression.clone());
        Return::new();
        TableConstructor::new();
        While::new(expression.clone());
    }
}
