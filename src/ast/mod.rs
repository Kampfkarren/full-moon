#[macro_use]
mod parser_util;
mod parsers;
pub mod punctuated;
pub mod span;

use crate::tokenizer::{Symbol, Token, TokenKind, TokenReference, TokenType};
use full_moon_derive::{Node, Visit};
use generational_arena::Arena;
use itertools::Itertools;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::fmt;
use std::iter::FromIterator;
use std::sync::Arc;

use parser_util::{
    InternalAstError, OneOrMore, Parser, ParserState, ZeroOrMore, ZeroOrMoreDelimited,
};

use punctuated::{Pair, Punctuated};
use span::ContainedSpan;

/// A block of statements, such as in if/do/etc block
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Block<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    stmts: Vec<(Stmt<'a>, Option<TokenReference<'a>>)>,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    last_stmt: Option<(LastStmt<'a>, Option<TokenReference<'a>>)>,
}

impl<'a> Block<'a> {
    /// An iterator over the [statements](enum.Stmt.html) in the block, such as `local foo = 1`
    pub fn iter_stmts(&self) -> impl Iterator<Item = &Stmt<'a>> {
        self.stmts.iter().map(|(stmt, _)| stmt)
    }

    /// The last statement of the block if one exists, such as `return foo`
    pub fn last_stmts(&self) -> Option<&LastStmt<'a>> {
        Some(&self.last_stmt.as_ref()?.0)
    }
}

/// The last statement of a [`Block`](struct.Block.html)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum LastStmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A `break` statement
    Break(TokenReference<'a>),
    /// A `return` statement
    Return(Return<'a>),
}

/// A `return` statement
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Return<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    token: TokenReference<'a>,
    returns: Punctuated<'a, Expression<'a>>,
}

impl<'a> Return<'a> {
    /// The `return` token
    pub fn token(&self) -> &TokenReference<'a> {
        &self.token
    }

    /// The values being returned
    pub fn returns(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.returns
    }
}

/// Fields of a [`TableConstructor`](struct.TableConstructor.html)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Field<'a> {
    /// A key in the format of `[expression] = value`
    ExpressionKey {
        /// The `[...]` part of `[expression] = value`
        #[cfg_attr(feature = "serde", serde(borrow))]
        brackets: ContainedSpan<'a>,
        /// The `expression` part of `[expression] = value`
        key: Expression<'a>,
        /// The `=` part of `[expression] = value`
        equal: TokenReference<'a>,
        /// The `value` part of `[expression] = value`
        value: Expression<'a>,
    },

    /// A key in the format of `name = value`
    NameKey {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `name` part of `name = value`
        key: TokenReference<'a>,
        /// The `=` part of `name = value`
        equal: TokenReference<'a>,
        /// The `value` part of `name = value`
        value: Expression<'a>,
    },

    /// A field with no key, just a value (such as `"a"` in `{ "a" }`)
    #[cfg_attr(feature = "serde", serde(borrow))]
    NoKey(Expression<'a>),
}

/// A [`Field`](enum.Field.html) used when creating a table
/// Second parameter is the separator used (`,` or `;`) if one exists
pub type TableConstructorField<'a> = (Field<'a>, Option<TokenReference<'a>>);

/// A table being constructed, such as `{ 1, 2, 3 }` or `{ a = 1 }`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct TableConstructor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    braces: ContainedSpan<'a>,
    fields: Vec<TableConstructorField<'a>>,
}

impl<'a> TableConstructor<'a> {
    /// An iterator over the [fields](type.TableConstructorField.html) used to create the table
    pub fn iter_fields(&self) -> impl Iterator<Item = &TableConstructorField<'a>> {
        self.fields.iter()
    }
}

/// A binary operation, such as (`+ 3`)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
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
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(untagged))]
pub enum Expression<'a> {
    /// A statement in parentheses, such as `(#list)`
    Parentheses {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The parentheses of the `ParenExpression`
        contained: ContainedSpan<'a>,
        /// The expression inside the parentheses
        expression: Box<Expression<'a>>,
    },

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
        value: Box<Value<'a>>,
        /// The binary operation being done, if one exists (the `+ 3` part of `2 + 3`)
        binop: Option<BinOpRhs<'a>>,
    },
}

/// Values that cannot be used standalone, but as part of things such as [statements](enum.Stmt.html)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Value<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An anonymous function, such as `function() end)`
    Function(FunctionBody<'a>),
    /// A call of a function, such as `call()`
    FunctionCall(FunctionCall<'a>),
    /// A table constructor, such as `{ 1, 2, 3 }`
    TableConstructor(TableConstructor<'a>),
    /// A number token, such as `3.3`
    Number(TokenReference<'a>),
    /// An expression between parentheses, such as `(3 + 2)`
    ParseExpression(Expression<'a>),
    /// A string token, such as `"hello"`
    String(TokenReference<'a>),
    /// A symbol, such as `true`
    Symbol(TokenReference<'a>),
    /// A more complex value, such as `call().x`
    Var(Var<'a>),
}

/// A statement that stands alone
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Stmt<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An assignment, such as `x = 1`
    Assignment(Assignment<'a>),
    /// A do block, `do end`
    Do(Do<'a>),
    /// A function call on its own, such as `call()`
    FunctionCall(FunctionCall<'a>),
    /// A function declaration, such as `function x() end`
    FunctionDeclaration(FunctionDeclaration<'a>),
    /// A generic for loop, such as `for index, value in pairs(list) do end`
    GenericFor(GenericFor<'a>),
    /// An if statement
    If(If<'a>),
    /// A local assignment, such as `local x = 1`
    LocalAssignment(LocalAssignment<'a>),
    /// A local function declaration, such as `local function x() end`
    LocalFunction(LocalFunction<'a>),
    /// A numeric for loop, such as `for index = 1, 10 do end`
    NumericFor(NumericFor<'a>),
    /// A repeat loop
    Repeat(Repeat<'a>),
    /// A while loop
    While(While<'a>),
}

/// A node used before another in cases such as function calling
/// The `("foo")` part of `("foo"):upper()`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Prefix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A complicated expression, such as `("foo")`
    Expression(Expression<'a>),
    /// Just a name, such as `foo`
    Name(TokenReference<'a>),
}

/// The indexing of something, such as `x.y` or `x["y"]`
/// Values of variants are the keys, such as `"y"`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Index<'a> {
    /// Indexing in the form of `x["y"]`
    Brackets {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `[...]` part of `["y"]`
        brackets: ContainedSpan<'a>,
        /// The `"y"` part of `["y"]`
        expression: Expression<'a>,
    },

    /// Indexing in the form of `x.y`
    Dot {
        #[cfg_attr(feature = "serde", serde(borrow))]
        /// The `.` part of `.y`
        dot: TokenReference<'a>,
        /// The `y` part of `.y`
        name: TokenReference<'a>,
    },
}

/// Arguments used for a function
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum FunctionArgs<'a> {
    /// Used when a function is called in the form of `call(1, 2, 3)`
    Parentheses {
        /// The `1, 2, 3` part of `1, 2, 3`
        #[cfg_attr(feature = "serde", serde(borrow))]
        arguments: Punctuated<'a, Expression<'a>>,
        /// The `(...) part of (1, 2, 3)`
        parentheses: ContainedSpan<'a>,
    },
    /// Used when a function is called in the form of `call "foobar"`
    #[cfg_attr(feature = "serde", serde(borrow))]
    String(TokenReference<'a>),
    /// Used when a function is called in the form of `call { 1, 2, 3 }`
    TableConstructor(TableConstructor<'a>),
}

/// A numeric for loop, such as `for index = 1, 10 do end`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct NumericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    for_token: TokenReference<'a>,
    index_variable: TokenReference<'a>,
    equal_token: TokenReference<'a>,
    start: Expression<'a>,
    start_end_comma: TokenReference<'a>,
    end: Expression<'a>,
    end_step_comma: Option<TokenReference<'a>>,
    step: Option<Expression<'a>>,
    do_token: TokenReference<'a>,
    block: Block<'a>,
    end_token: TokenReference<'a>,
}

impl<'a> NumericFor<'a> {
    /// The index identity, `index` in the initial example
    pub fn index_variable(&self) -> &Token<'a> {
        &self.index_variable
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

/// A generic for loop, such as `for index, value in pairs(list) do end`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GenericFor<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    for_token: TokenReference<'a>,
    names: Punctuated<'a, TokenReference<'a>>,
    in_token: TokenReference<'a>,
    expr_list: Punctuated<'a, Expression<'a>>,
    do_token: TokenReference<'a>,
    block: Block<'a>,
    end_token: TokenReference<'a>,
}

impl<'a> GenericFor<'a> {
    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence of names
    /// In `for index, value in pairs(list) do`, iterates over `index` and `value`
    pub fn names(&self) -> &Punctuated<'a, TokenReference<'a>> {
        &self.names
    }

    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence of the expressions looped over
    /// In `for index, value in pairs(list) do`, iterates over `pairs(list)`
    pub fn expr_list(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.expr_list
    }

    /// The code inside the for loop
    pub fn block(&self) -> &Block<'a> {
        &self.block
    }
}

/// An if statement
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct If<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    if_token: TokenReference<'a>,
    condition: Expression<'a>,
    then_token: TokenReference<'a>,
    block: Block<'a>,
    else_if: Option<Vec<ElseIf<'a>>>,
    else_token: Option<TokenReference<'a>>,
    #[cfg_attr(feature = "serde", serde(rename = "else"))]
    r#else: Option<Block<'a>>,
    end_token: TokenReference<'a>,
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

    /// The `else` token if one exists
    pub fn else_token(&self) -> Option<&TokenReference<'a>> {
        self.else_token.as_ref()
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
}

/// An elseif block in a bigger [`If`](struct.If.html) statement
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ElseIf<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    else_if_token: TokenReference<'a>,
    condition: Expression<'a>,
    then_token: TokenReference<'a>,
    block: Block<'a>,
}

/// A while loop
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct While<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    while_token: TokenReference<'a>,
    condition: Expression<'a>,
    do_token: TokenReference<'a>,
    block: Block<'a>,
    end_token: TokenReference<'a>,
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

/// A repeat loop
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Repeat<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    repeat_token: TokenReference<'a>,
    block: Block<'a>,
    until_token: TokenReference<'a>,
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

/// A method call, such as `x:y()`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct MethodCall<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    colon_token: TokenReference<'a>,
    name: TokenReference<'a>,
    args: FunctionArgs<'a>,
}

impl<'a> MethodCall<'a> {
    /// The arguments of a method call, the `x, y, z` part of `method:call(x, y, z)`
    pub fn args(&self) -> &FunctionArgs<'a> {
        &self.args
    }

    /// The method being called, the `call` part of `method:call()`
    pub fn name(&self) -> &Token<'a> {
        &self.name
    }
}

/// Something being called
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Call<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A function being called directly, such as `x(1)`
    AnonymousCall(FunctionArgs<'a>),
    /// A method call, such as `x:y()`
    MethodCall(MethodCall<'a>),
}

/// A function body, everything except `function x` in `function x(a, b, c) call() end`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionBody<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    parameters_parantheses: ContainedSpan<'a>,
    parameters: Punctuated<'a, Parameter<'a>>,
    block: Block<'a>,
    end_token: TokenReference<'a>,
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

/// A parameter in a function declaration
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Parameter<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// The `...` vararg syntax, such as `function x(...)`
    Ellipse(TokenReference<'a>),
    /// A name parameter, such as `function x(a, b, c)`
    Name(TokenReference<'a>),
}

/// A suffix in certain cases, such as `:y()` in `x:y()`
/// Can be stacked on top of each other, such as in `x()()()`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Suffix<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// A call, including method calls and direct calls
    Call(Call<'a>),
    /// An index, such as `x.y`
    Index(Index<'a>),
}

/// A complex expression used by [`Var`](enum.Var.html), consisting of both a prefix and suffixes
#[derive(Clone, Debug, PartialEq, Node, Visit)]
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

/// Used in [`Assignment`s](struct.Assignment.html) and [`Value`s](enum.Value.html)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Var<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    /// An expression, such as `x.y.z` or `x()`
    Expression(VarExpression<'a>),
    /// A literal identifier, such as `x`
    Name(TokenReference<'a>),
}

/// An assignment, such as `x = y`. Not used for [`LocalAssignment`s](struct.LocalAssignment.html)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Assignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    var_list: Punctuated<'a, Var<'a>>,
    equal_token: TokenReference<'a>,
    expr_list: Punctuated<'a, Expression<'a>>,
}

impl<'a> Assignment<'a> {
    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence over the expressions being assigned.
    /// This is the the `1, 2` part of `x, y["a"] = 1, 2`
    pub fn expr_list(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.expr_list
    }

    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence over the variables being assigned to.
    /// This is the `x, y["a"]` part of `x, y["a"] = 1, 2`
    pub fn var_list(&self) -> &Punctuated<'a, Var<'a>> {
        &self.var_list
    }
}

/// A declaration of a local function, such as `local function x() end`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalFunction<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    local_token: TokenReference<'a>,
    function_token: TokenReference<'a>,
    name: TokenReference<'a>,
    func_body: FunctionBody<'a>,
}

impl<'a> LocalFunction<'a> {
    /// The function body, everything except `local function x` in `local function x(a, b, c) call() end`
    pub fn func_body(&self) -> &FunctionBody<'a> {
        &self.func_body
    }

    /// The name of the function, the `x` part of `local function x() end`
    pub fn name(&self) -> &Token<'a> {
        &self.name
    }
}

/// An assignment to a local variable, such as `local x = 1`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct LocalAssignment<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    local_token: TokenReference<'a>,
    name_list: Punctuated<'a, TokenReference<'a>>,
    expr_list: Punctuated<'a, Expression<'a>>,
}

impl<'a> LocalAssignment<'a> {
    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence of the expressions being assigned.
    /// This is the `1, 2` part of `local x, y = 1, 2`
    pub fn expr_list(&self) -> &Punctuated<'a, Expression<'a>> {
        &self.expr_list
    }

    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence of names being assigned to.
    /// This is the `x, y` part of `local x, y = 1, 2`
    pub fn name_list(&self) -> &Punctuated<'a, TokenReference<'a>> {
        &self.name_list
    }

    /// Returns a mutable [`Punctuated`](punctuated/struct.Punctuated.html) sequence of names being assigned to.
    /// This is the `x, y` part of `local x, y = 1, 2`
    pub fn name_list_mut(&mut self) -> &mut Punctuated<'a, TokenReference<'a>> {
        &mut self.name_list
    }
}

/// A `do` block, such as `do ... end`
/// This is not used for things like `while true do end`, only those on their own
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Do<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    do_token: TokenReference<'a>,
    block: Block<'a>,
    end_token: TokenReference<'a>,
}

/// A function being called, such as `call()`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
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

/// A function name when being [declared](struct.FunctionDeclaration.html)
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionName<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    names: Punctuated<'a, TokenReference<'a>>,
    colon_name: Option<(TokenReference<'a>, TokenReference<'a>)>,
}

impl<'a> FunctionName<'a> {
    /// A method name if one exists, the `y` part of `function x:y() end`
    pub fn method_name(&self) -> Option<&TokenReference<'a>> {
        Some(&self.colon_name.as_ref()?.1)
    }

    /// Returns the [`Punctuated`](punctuated/struct.Punctuated.html) sequence over the names used when defining the function.
    /// This is the `x.y.z` part of `function x.y.z() end`
    pub fn names(&self) -> &Punctuated<'a, TokenReference<'a>> {
        &self.names
    }
}

/// A normal function declaration, supports simple declarations like `function x() end`
/// as well as complicated declarations such as `function x.y.z:a() end`
#[derive(Clone, Debug, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct FunctionDeclaration<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    function_token: TokenReference<'a>,
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

macro_rules! make_op {
    ($enum:ident, $(#[$outer:meta])* { $($operator:ident,)+ }) => {
        #[derive(Clone, Debug, PartialEq, Node, Visit)]
        #[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
        #[visit(skip_visit_self)]
        $(#[$outer])*
        pub enum $enum<'a> {
            #[cfg_attr(feature = "serde", serde(borrow))]
            $(
                #[allow(missing_docs)]
                $operator(TokenReference<'a>),
            )+
        }
    };
}

make_op!(BinOp,
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
#[derive(Clone, Debug)]
pub struct Ast<'a> {
    nodes: Block<'a>,
    tokens: Arc<Arena<Token<'a>>>,
}

impl<'a> Ast<'a> {
    /// Create an Ast from the passed tokens. You probably want [`parse`](../fn.parse.html)
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
            let tokens = Arc::new(Arena::from_iter(tokens));

            let mut state = ParserState::new(Arc::clone(&tokens));

            if tokens
                .iter()
                .filter(|token| !token.1.token_type().ignore())
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
            if state.peek().token_type().ignore() {
                state = state.advance().unwrap();
            }

            match parsers::ParseBlock.parse(state.clone()) {
                Ok((state, block)) => {
                    if state.index == tokens.len() - 1 {
                        Ok(Ast {
                            tokens,
                            nodes: block,
                        })
                    } else {
                        Err(AstError::UnexpectedToken {
                            token: (*state.peek()).to_owned(),
                            additional: Some("leftover token"),
                        })
                    }
                }

                Err(InternalAstError::NoMatch) => Err(AstError::UnexpectedToken {
                    token: (*state.peek()).to_owned(),
                    additional: None,
                }),

                Err(InternalAstError::UnexpectedToken { token, additional }) => {
                    Err(AstError::UnexpectedToken {
                        token: (*token).to_owned(),
                        additional,
                    })
                }
            }
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

    /// An iterator over the tokens used to create the Ast
    pub fn iter_tokens(&self) -> impl Iterator<Item = &Token<'a>> {
        self.tokens.iter().map(|(_, token)| token).sorted()
    }

    /// Will update the positions of all the tokens in the tree
    /// Necessary if you are both mutating the tree and need the positions of the tokens
    pub fn update_positions(&mut self) {
        use crate::tokenizer::Position;

        let mut start_position = Position {
            bytes: 0,
            character: 1,
            line: 1,
        };

        let mut next_is_new_line = false;

        for (_, token) in self.tokens.iter() {
            let display = token.to_string();

            let new_lines = match bytecount::count(display.as_bytes(), b'\n') {
                0 | 1 => 0,
                n => n,
            };

            let end_position = if token.token_kind() == TokenKind::Eof {
                start_position
            } else {
                let mut end_position = Position {
                    bytes: start_position.bytes() + display.len(),
                    line: start_position.line() + new_lines,
                    character: {
                        let offset = display.lines().last().unwrap_or("").len();
                        if new_lines > 0 || next_is_new_line {
                            offset + 1
                        } else {
                            start_position.character() + offset
                        }
                    },
                };

                if next_is_new_line {
                    end_position.line += 1;
                    next_is_new_line = false;
                }

                end_position
            };

            if display.ends_with('\n') {
                next_is_new_line = true;
            }

            token.start_position.store(start_position);
            token.end_position.store(end_position);
            start_position = end_position;
        }
    }
}
