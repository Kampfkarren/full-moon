use std::sync::{Arc, RwLock};

use full_moon::{ast, node::Node, tokenizer};
use full_moon_lua_types_derive::LuaUserData;
use mlua::{Table, UserData};

use crate::{
    create_ast_node::CreateAstNode,
    mlua_util::{
        add_core_meta_methods, add_newindex_block, add_print, add_to_string_display, ArcLocked,
    },
    prepare_for_lua::PrepareForLua,
    shared::*,
};

fn l<T>(t: T) -> ArcLocked<T> {
    Arc::new(RwLock::new(t))
}

pub struct Ast {
    nodes: ArcLocked<Block>,
    eof: ArcLocked<TokenReference>,
}

impl From<ast::Ast> for Ast {
    fn from(ast: ast::Ast) -> Self {
        Ast {
            nodes: l(Block::new(ast.nodes())),
            eof: l(TokenReference::new(ast.eof())),
        }
    }
}

impl UserData for Ast {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("nodes", |_, Ast { nodes, .. }| Ok(nodes.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Ast", methods);
    }
}

pub struct Assignment {
    var_list: Punctuated<Var>,
    equal_token: TokenReference,
    expr_list: Punctuated<Expression>,
}

impl Assignment {
    fn new(assignment: &ast::Assignment) -> Self {
        Assignment {
            var_list: Punctuated::map_from_punctuated(assignment.variables(), Var::new),
            equal_token: TokenReference::new(assignment.equal_token()),
            expr_list: Punctuated::map_from_punctuated(assignment.expressions(), Expression::new),
        }
    }
}

impl UserData for Assignment {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Assignment", methods);
        add_print(methods);
    }
}

impl CreateAstNode for Assignment {
    type Node = ast::Assignment;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::Assignment::new(
                self.var_list.create_ast_node()?,
                self.expr_list.create_ast_node()?,
            )
            .with_equal_token(self.equal_token.create_ast_node()?),
        )
    }
}

#[derive(LuaUserData)]
pub enum BinOp {
    And(ArcLocked<TokenReference>),
    Caret(ArcLocked<TokenReference>),
    GreaterThan(ArcLocked<TokenReference>),
    GreaterThanEqual(ArcLocked<TokenReference>),
    LessThan(ArcLocked<TokenReference>),
    LessThanEqual(ArcLocked<TokenReference>),
    Minus(ArcLocked<TokenReference>),
    Or(ArcLocked<TokenReference>),
    Percent(ArcLocked<TokenReference>),
    Plus(ArcLocked<TokenReference>),
    Slash(ArcLocked<TokenReference>),
    Star(ArcLocked<TokenReference>),
    TildeEqual(ArcLocked<TokenReference>),
    TwoDots(ArcLocked<TokenReference>),
    TwoEqual(ArcLocked<TokenReference>),
}

impl BinOp {
    fn new(bin_op: &ast::BinOp) -> Self {
        match bin_op {
            ast::BinOp::And(token) => BinOp::And(l(TokenReference::new(&token))),
            ast::BinOp::Caret(token) => BinOp::Caret(l(TokenReference::new(&token))),
            ast::BinOp::GreaterThan(token) => BinOp::GreaterThan(l(TokenReference::new(&token))),
            ast::BinOp::GreaterThanEqual(token) => {
                BinOp::GreaterThanEqual(l(TokenReference::new(&token)))
            }
            ast::BinOp::LessThan(token) => BinOp::LessThan(l(TokenReference::new(&token))),
            ast::BinOp::LessThanEqual(token) => {
                BinOp::LessThanEqual(l(TokenReference::new(&token)))
            }
            ast::BinOp::Minus(token) => BinOp::Minus(l(TokenReference::new(&token))),
            ast::BinOp::Or(token) => BinOp::Or(l(TokenReference::new(&token))),
            ast::BinOp::Percent(token) => BinOp::Percent(l(TokenReference::new(&token))),
            ast::BinOp::Plus(token) => BinOp::Plus(l(TokenReference::new(&token))),
            ast::BinOp::Slash(token) => BinOp::Slash(l(TokenReference::new(&token))),
            ast::BinOp::Star(token) => BinOp::Star(l(TokenReference::new(&token))),
            ast::BinOp::TildeEqual(token) => BinOp::TildeEqual(l(TokenReference::new(&token))),
            ast::BinOp::TwoDots(token) => BinOp::TwoDots(l(TokenReference::new(&token))),
            ast::BinOp::TwoEqual(token) => BinOp::TwoEqual(l(TokenReference::new(&token))),
            other => panic!("unimplemented BinOp: {other:?}"),
        }
    }
}

pub struct Block {
    stmts: Vec<(ArcLocked<Stmt>, Option<ArcLocked<TokenReference>>)>,
    last_stmt: Option<(ArcLocked<LastStmt>, Option<ArcLocked<TokenReference>>)>,
}

impl Block {
    fn new(block: &ast::Block) -> Self {
        Block {
            stmts: block
                .stmts_with_semicolon()
                .map(|(stmt, token)| {
                    (
                        l(Stmt::new(stmt)),
                        token.as_ref().map(TokenReference::new).map(l),
                    )
                })
                .collect(),

            last_stmt: block
                .last_stmt_with_semicolon()
                .as_ref()
                .map(|(last_stmt, token)| {
                    (
                        l(LastStmt::new(last_stmt)),
                        token.as_ref().map(TokenReference::new).map(l),
                    )
                }),
        }
    }
}

impl UserData for Block {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("stmts", |_, Block { stmts, .. }| {
            Ok(stmts
                .iter()
                .map(|(stmt, _)| stmt.clone())
                .collect::<Vec<_>>())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Block", methods);
    }
}

impl CreateAstNode for Block {
    type Node = ast::Block;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::Block::new()
                .with_stmts(
                    self.stmts
                        .iter()
                        .map(|(stmt, token)| {
                            Some((stmt.create_ast_node()?, token.create_ast_node()))
                        })
                        .collect::<Option<Vec<_>>>()?,
                )
                .with_last_stmt(self.last_stmt.as_ref().map(|(last_stmt, token)| {
                    Some((last_stmt.create_ast_node()?, token.create_ast_node()))
                })?),
        )
    }
}

#[derive(LuaUserData)]
pub enum Call {
    AnonymousCall(ArcLocked<FunctionArgs>),
    MethodCall(ArcLocked<MethodCall>),
}

impl Call {
    fn new(call: &ast::Call) -> Self {
        match call {
            ast::Call::AnonymousCall(function_args) => {
                Call::AnonymousCall(l(FunctionArgs::new(function_args)))
            }
            ast::Call::MethodCall(method_call) => Call::MethodCall(l(MethodCall::new(method_call))),
            other => panic!("unimplemented Call: {other:?}"),
        }
    }
}

pub struct Do {
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl Do {
    fn new(do_: &ast::Do) -> Self {
        Do {
            do_token: TokenReference::new(do_.do_token()),
            block: Block::new(do_.block()),
            end_token: TokenReference::new(do_.end_token()),
        }
    }
}

impl UserData for Do {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Do", methods);
    }
}

impl CreateAstNode for Do {
    type Node = ast::Do;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::Do::new()
                .with_block(self.block.create_ast_node()?)
                .with_do_token(self.do_token.create_ast_node()?)
                .with_end_token(self.end_token.create_ast_node()?),
        )
    }
}

pub struct ElseIf {
    else_if_token: TokenReference,
    condition: Expression,
    then_token: TokenReference,
    block: Block,
}

impl ElseIf {
    fn new(else_if: &ast::ElseIf) -> Self {
        ElseIf {
            else_if_token: TokenReference::new(else_if.else_if_token()),
            condition: Expression::new(else_if.condition()),
            then_token: TokenReference::new(else_if.then_token()),
            block: Block::new(else_if.block()),
        }
    }
}

impl UserData for ElseIf {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("ElseIf", methods);
    }
}

impl CreateAstNode for ElseIf {
    type Node = ast::ElseIf;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::ElseIf::new(self.condition.create_ast_node()?)
                .with_block(self.block.create_ast_node()?)
                .with_else_if_token(self.else_if_token.create_ast_node()?)
                .with_then_token(self.then_token.create_ast_node()?),
        )
    }
}

#[derive(Clone, LuaUserData)]
pub enum Expression {
    BinaryOperator {
        lhs: Box<ArcLocked<Expression>>,
        binop: ArcLocked<BinOp>,
        rhs: Box<ArcLocked<Expression>>,
    },

    Parentheses {
        contained: ArcLocked<ContainedSpan>,
        expression: Box<ArcLocked<Expression>>,
    },

    UnaryOperator {
        unop: ArcLocked<UnOp>,
        expression: Box<ArcLocked<Expression>>,
    },

    Value {
        value: Box<ArcLocked<Value>>,
    },
}

impl Expression {
    fn new(expression: &ast::Expression) -> Self {
        match expression {
            ast::Expression::BinaryOperator { lhs, binop, rhs } => Expression::BinaryOperator {
                lhs: Box::new(l(Expression::new(lhs))),
                binop: l(BinOp::new(binop)),
                rhs: Box::new(l(Expression::new(rhs))),
            },

            ast::Expression::Parentheses {
                contained,
                expression,
            } => Expression::Parentheses {
                contained: l(ContainedSpan::new(contained)),
                expression: Box::new(l(Expression::new(expression))),
            },

            ast::Expression::UnaryOperator { unop, expression } => Expression::UnaryOperator {
                unop: l(UnOp::new(unop)),
                expression: Box::new(l(Expression::new(expression))),
            },

            ast::Expression::Value { value } => Expression::Value {
                value: Box::new(l(Value::new(value))),
            },

            other => panic!("unimplemented Expression: {other:?}"),
        }
    }
}

#[derive(LuaUserData)]
pub enum FunctionArgs {
    Parentheses {
        parentheses: ArcLocked<ContainedSpan>,
        arguments: ArcLocked<Punctuated<Expression>>,
    },

    String(ArcLocked<TokenReference>),

    TableConstructor(ArcLocked<TableConstructor>),
}

impl FunctionArgs {
    fn new(function_args: &ast::FunctionArgs) -> Self {
        match function_args {
            ast::FunctionArgs::Parentheses {
                parentheses,
                arguments,
            } => FunctionArgs::Parentheses {
                parentheses: l(ContainedSpan::new(parentheses)),
                arguments: l(Punctuated::map_from_punctuated(arguments, Expression::new)),
            },

            ast::FunctionArgs::String(token) => FunctionArgs::String(l(TokenReference::new(token))),

            ast::FunctionArgs::TableConstructor(table_constructor) => {
                FunctionArgs::TableConstructor(l(TableConstructor::new(table_constructor)))
            }

            other => panic!("unimplemented FunctionArgs: {other:?}"),
        }
    }
}

pub struct FunctionBody {
    parameters_parentheses: ContainedSpan,
    parameters: Punctuated<Parameter>,
    block: Block,
    end_token: TokenReference,
}

impl FunctionBody {
    fn new(function_body: &ast::FunctionBody) -> Self {
        FunctionBody {
            parameters_parentheses: ContainedSpan::new(function_body.parameters_parentheses()),

            parameters: Punctuated::map_from_punctuated(function_body.parameters(), Parameter::new),

            block: Block::new(function_body.block()),
            end_token: TokenReference::new(function_body.end_token()),
        }
    }
}

impl UserData for FunctionBody {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionBody", methods);
    }
}

impl CreateAstNode for FunctionBody {
    type Node = ast::FunctionBody;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::FunctionBody::new()
                .with_block(self.block.create_ast_node()?)
                .with_end_token(self.end_token.create_ast_node()?)
                .with_parameters(self.parameters.create_ast_node()?)
                .with_parameters_parentheses(self.parameters_parentheses.create_ast_node()?),
        )
    }
}

#[derive(LuaUserData)]
pub enum LastStmt {
    Break(ArcLocked<TokenReference>),
    #[cfg(feature = "luau")]
    Continue(ArcLocked<TokenReference>),
    Return(ArcLocked<Return>),
}

impl LastStmt {
    fn new(last_stmt: &ast::LastStmt) -> Self {
        match last_stmt {
            ast::LastStmt::Break(break_token) => {
                LastStmt::Break(l(TokenReference::new(break_token)))
            }

            #[cfg(feature = "luau")]
            ast::LastStmt::Continue(continue_token) => {
                LastStmt::Continue(l(TokenReference::new(continue_token)))
            }

            ast::LastStmt::Return(return_token) => LastStmt::Return(l(Return::new(return_token))),

            _ => unimplemented!("unexpected LastStmt variant: {last_stmt:#?}"),
        }
    }
}

#[derive(LuaUserData)]
pub enum Field {
    ExpressionKey {
        brackets: ArcLocked<ContainedSpan>,
        key: ArcLocked<Expression>,
        equal: ArcLocked<TokenReference>,
        value: ArcLocked<Expression>,
    },

    NameKey {
        key: ArcLocked<TokenReference>,
        equal: ArcLocked<TokenReference>,
        value: ArcLocked<Expression>,
    },

    NoKey(ArcLocked<Expression>),
}

impl Field {
    fn new(field: &ast::Field) -> Self {
        match field {
            ast::Field::ExpressionKey {
                brackets,
                key,
                equal,
                value,
            } => Field::ExpressionKey {
                brackets: l(ContainedSpan::new(brackets)),
                key: l(Expression::new(key)),
                equal: l(TokenReference::new(equal)),
                value: l(Expression::new(value)),
            },

            ast::Field::NameKey { key, equal, value } => Field::NameKey {
                key: l(TokenReference::new(key)),
                equal: l(TokenReference::new(equal)),
                value: l(Expression::new(value)),
            },

            ast::Field::NoKey(expression) => Field::NoKey(l(Expression::new(expression))),

            other => panic!("unimplemented Field: {other:?}"),
        }
    }
}

pub struct FunctionCall {
    prefix: Prefix,
    suffixes: Vec<Suffix>,
}

impl FunctionCall {
    fn new(function_call: &ast::FunctionCall) -> Self {
        FunctionCall {
            prefix: Prefix::new(function_call.prefix()),
            suffixes: function_call.suffixes().map(Suffix::new).collect(),
        }
    }
}

impl UserData for FunctionCall {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionCall", methods);
    }
}

impl CreateAstNode for FunctionCall {
    type Node = ast::FunctionCall;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::FunctionCall::new(self.prefix.create_ast_node()?).with_suffixes(
                self.suffixes
                    .iter()
                    .map(Suffix::create_ast_node)
                    .collect::<Option<Vec<_>>>()?,
            ),
        )
    }
}

pub struct FunctionDeclaration {
    function_token: TokenReference,
    name: FunctionName,
    body: FunctionBody,
}

impl FunctionDeclaration {
    fn new(function_declaration: &ast::FunctionDeclaration) -> Self {
        FunctionDeclaration {
            function_token: TokenReference::new(function_declaration.function_token()),
            name: FunctionName::new(function_declaration.name()),
            body: FunctionBody::new(function_declaration.body()),
        }
    }
}

impl UserData for FunctionDeclaration {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionDeclaration", methods);
    }
}

impl CreateAstNode for FunctionDeclaration {
    type Node = ast::FunctionDeclaration;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::FunctionDeclaration::new(self.name.create_ast_node()?)
                .with_body(self.body.create_ast_node()?)
                .with_function_token(self.function_token.create_ast_node()?),
        )
    }
}

pub struct FunctionName {
    names: Punctuated<TokenReference>,
    colon_name: Option<(TokenReference, TokenReference)>,
}

impl FunctionName {
    fn new(function_name: &ast::FunctionName) -> Self {
        FunctionName {
            names: Punctuated::map_from_punctuated(function_name.names(), TokenReference::new),

            colon_name: match (function_name.method_colon(), function_name.method_name()) {
                (Some(colon), Some(name)) => {
                    Some((TokenReference::new(colon), TokenReference::new(name)))
                }

                _ => None,
            },
        }
    }
}

impl UserData for FunctionName {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("FunctionName", methods);
    }
}

impl CreateAstNode for FunctionName {
    type Node = ast::FunctionName;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::FunctionName::new(self.names.create_ast_node()?).with_method(
                self.colon_name
                    .as_ref()
                    .map(|(colon, name)| Some((colon.create_ast_node()?, name.create_ast_node()?)))
                    .flatten(),
            ),
        )
    }
}

pub struct GenericFor {
    for_token: TokenReference,
    names: Punctuated<TokenReference>,
    in_token: TokenReference,
    expr_list: Punctuated<Expression>,
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl GenericFor {
    fn new(generic_for: &ast::GenericFor) -> Self {
        GenericFor {
            for_token: TokenReference::new(generic_for.for_token()),
            names: Punctuated::map_from_punctuated(generic_for.names(), TokenReference::new),
            in_token: TokenReference::new(generic_for.in_token()),
            expr_list: Punctuated::map_from_punctuated(generic_for.expressions(), Expression::new),
            do_token: TokenReference::new(generic_for.do_token()),
            block: Block::new(generic_for.block()),
            end_token: TokenReference::new(generic_for.end_token()),
        }
    }
}

impl UserData for GenericFor {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("GenericFor", methods);
    }
}

impl CreateAstNode for GenericFor {
    type Node = ast::GenericFor;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::GenericFor::new(
                self.names.create_ast_node()?,
                self.expr_list.create_ast_node()?,
            )
            .with_for_token(self.for_token.create_ast_node()?)
            .with_in_token(self.in_token.create_ast_node()?)
            .with_do_token(self.do_token.create_ast_node()?)
            .with_block(self.block.create_ast_node()?)
            .with_end_token(self.end_token.create_ast_node()?),
        )
    }
}

pub struct If {
    if_token: TokenReference,
    condition: Expression,
    then_token: TokenReference,
    block: Block,
    else_if: Option<Vec<ElseIf>>,
    else_token: Option<TokenReference>,
    else_block: Option<Block>,
    end_token: TokenReference,
}

impl If {
    fn new(if_node: &ast::If) -> Self {
        If {
            if_token: TokenReference::new(if_node.if_token()),
            condition: Expression::new(if_node.condition()),
            then_token: TokenReference::new(if_node.then_token()),
            block: Block::new(if_node.block()),
            else_if: if_node
                .else_if()
                .map(|else_if| else_if.iter().map(ElseIf::new).collect()),
            else_token: if_node.else_token().map(TokenReference::new),
            else_block: if_node.else_block().map(Block::new),
            end_token: TokenReference::new(if_node.end_token()),
        }
    }
}

impl UserData for If {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("If", methods);
    }
}

impl CreateAstNode for If {
    type Node = ast::If;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::If::new(self.condition.create_ast_node()?)
                .with_if_token(self.if_token.create_ast_node()?)
                .with_then_token(self.then_token.create_ast_node()?)
                .with_block(self.block.create_ast_node()?)
                .with_else_if(
                    self.else_if
                        .as_ref()
                        .map(|else_if| {
                            else_if
                                .iter()
                                .map(ElseIf::create_ast_node)
                                .collect::<Option<Vec<_>>>()
                        })
                        .flatten(),
                )
                .with_else_token(self.else_token.create_ast_node())
                .with_else(self.else_block.create_ast_node())
                .with_end_token(self.end_token.create_ast_node()?),
        )
    }
}

#[derive(LuaUserData)]
pub enum Index {
    Brackets {
        brackets: ArcLocked<ContainedSpan>,
        expression: ArcLocked<Expression>,
    },

    Dot {
        dot: ArcLocked<TokenReference>,
        name: ArcLocked<TokenReference>,
    },
}

impl Index {
    fn new(index: &ast::Index) -> Self {
        match index {
            ast::Index::Brackets {
                brackets,
                expression,
            } => Index::Brackets {
                brackets: l(ContainedSpan::new(brackets)),
                expression: l(Expression::new(expression)),
            },

            ast::Index::Dot { dot, name } => Index::Dot {
                dot: l(TokenReference::new(dot)),
                name: l(TokenReference::new(name)),
            },

            other => panic!("unimplemented Index: {other:?}"),
        }
    }
}

pub struct LocalAssignment {
    local_token: TokenReference,
    name_list: Punctuated<TokenReference>,
    equal_token: Option<TokenReference>,
    expr_list: Punctuated<Expression>,
}

impl LocalAssignment {
    fn new(local_assignment: &ast::LocalAssignment) -> Self {
        LocalAssignment {
            local_token: TokenReference::new(local_assignment.local_token()),
            name_list: Punctuated::map_from_punctuated(
                local_assignment.names(),
                TokenReference::new,
            ),
            equal_token: local_assignment.equal_token().map(TokenReference::new),
            expr_list: Punctuated::map_from_punctuated(
                local_assignment.expressions(),
                Expression::new,
            ),
        }
    }
}

impl UserData for LocalAssignment {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("LocalAssignment", methods);
    }
}

impl CreateAstNode for LocalAssignment {
    type Node = ast::LocalAssignment;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::LocalAssignment::new(self.name_list.create_ast_node()?)
                .with_expressions(self.expr_list.create_ast_node()?)
                .with_local_token(self.local_token.create_ast_node()?)
                .with_equal_token(self.equal_token.create_ast_node()),
        )
    }
}

pub struct LocalFunction {
    local_token: TokenReference,
    function_token: TokenReference,
    name: TokenReference,
    body: FunctionBody,
}

impl LocalFunction {
    fn new(local_function: &ast::LocalFunction) -> Self {
        LocalFunction {
            local_token: TokenReference::new(local_function.local_token()),
            function_token: TokenReference::new(local_function.function_token()),
            name: TokenReference::new(local_function.name()),
            body: FunctionBody::new(local_function.body()),
        }
    }
}

impl UserData for LocalFunction {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("LocalFunction", methods);
    }
}

impl CreateAstNode for LocalFunction {
    type Node = ast::LocalFunction;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::LocalFunction::new(self.name.create_ast_node()?)
                .with_body(self.body.create_ast_node()?)
                .with_local_token(self.local_token.create_ast_node()?)
                .with_function_token(self.function_token.create_ast_node()?),
        )
    }
}

pub struct MethodCall {
    colon_token: TokenReference,
    name: TokenReference,
    args: FunctionArgs,
}

impl MethodCall {
    fn new(method_call: &ast::MethodCall) -> Self {
        MethodCall {
            colon_token: TokenReference::new(method_call.colon_token()),
            name: TokenReference::new(method_call.name()),
            args: FunctionArgs::new(method_call.args()),
        }
    }
}

impl UserData for MethodCall {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("MethodCall", methods);
    }
}

impl CreateAstNode for MethodCall {
    type Node = ast::MethodCall;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::MethodCall::new(self.name.create_ast_node()?, self.args.create_ast_node()?)
                .with_colon_token(self.colon_token.create_ast_node()?),
        )
    }
}

pub struct NumericFor {
    for_token: TokenReference,
    index_variable: TokenReference,
    equal_token: TokenReference,
    start: Expression,
    start_end_comma: TokenReference,
    end: Expression,
    end_step_comma: Option<TokenReference>,
    step: Option<Expression>,
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl NumericFor {
    fn new(numeric_for: &ast::NumericFor) -> Self {
        NumericFor {
            for_token: TokenReference::new(numeric_for.for_token()),
            index_variable: TokenReference::new(numeric_for.index_variable()),
            equal_token: TokenReference::new(numeric_for.equal_token()),
            start: Expression::new(numeric_for.start()),
            start_end_comma: TokenReference::new(numeric_for.start_end_comma()),
            end: Expression::new(numeric_for.end()),
            end_step_comma: numeric_for.end_step_comma().map(TokenReference::new),
            step: numeric_for.step().map(Expression::new),
            do_token: TokenReference::new(numeric_for.do_token()),
            block: Block::new(numeric_for.block()),
            end_token: TokenReference::new(numeric_for.end_token()),
        }
    }
}

impl UserData for NumericFor {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("NumericFor", methods);
    }
}

impl CreateAstNode for NumericFor {
    type Node = ast::NumericFor;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::NumericFor::new(
                self.index_variable.create_ast_node()?,
                self.start.create_ast_node()?,
                self.end.create_ast_node()?,
            )
            .with_step(self.step.create_ast_node())
            .with_block(self.block.create_ast_node()?)
            .with_end_token(self.end_token.create_ast_node()?)
            .with_start_end_comma(self.start_end_comma.create_ast_node()?)
            .with_end_step_comma(self.end_step_comma.create_ast_node())
            .with_for_token(self.for_token.create_ast_node()?)
            .with_equal_token(self.equal_token.create_ast_node()?)
            .with_do_token(self.do_token.create_ast_node()?),
        )
    }
}

#[derive(LuaUserData)]
pub enum Parameter {
    Ellipse(ArcLocked<TokenReference>),
    Name(ArcLocked<TokenReference>),
}

impl Parameter {
    fn new(parameter: &ast::Parameter) -> Self {
        match parameter {
            ast::Parameter::Ellipse(ellipse_token) => {
                Parameter::Ellipse(l(TokenReference::new(ellipse_token)))
            }

            ast::Parameter::Name(name_token) => Parameter::Name(l(TokenReference::new(name_token))),

            _ => unimplemented!("unexpected Parameter variant: {parameter:#?}"),
        }
    }
}

#[derive(LuaUserData)]
pub enum Prefix {
    Expression(ArcLocked<Expression>),
    Name(ArcLocked<TokenReference>),
}

impl Prefix {
    fn new(prefix: &ast::Prefix) -> Self {
        match prefix {
            ast::Prefix::Expression(expr) => Prefix::Expression(l(Expression::new(expr))),
            ast::Prefix::Name(name) => Prefix::Name(l(TokenReference::new(name))),
            other => unimplemented!("unexpected Prefix variant: {other:?}"),
        }
    }
}

pub struct Return {
    token: TokenReference,
    returns: Punctuated<Expression>,
}

impl Return {
    fn new(return_token: &ast::Return) -> Self {
        Return {
            token: TokenReference::new(return_token.token()),
            returns: Punctuated::map_from_punctuated(return_token.returns(), Expression::new),
        }
    }
}

impl UserData for Return {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Return", methods);
    }
}

impl CreateAstNode for Return {
    type Node = ast::Return;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::Return::new()
                .with_token(self.token.create_ast_node()?)
                .with_returns(self.returns.create_ast_node()?),
        )
    }
}

pub struct Repeat {
    repeat_token: TokenReference,
    block: Block,
    until_token: TokenReference,
    until: Expression,
}

impl Repeat {
    fn new(repeat: &ast::Repeat) -> Self {
        Repeat {
            repeat_token: TokenReference::new(repeat.repeat_token()),
            block: Block::new(repeat.block()),
            until_token: TokenReference::new(repeat.until_token()),
            until: Expression::new(repeat.until()),
        }
    }
}

impl UserData for Repeat {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("Repeat", methods);
    }
}

impl CreateAstNode for Repeat {
    type Node = ast::Repeat;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::Repeat::new(self.until.create_ast_node()?)
                .with_repeat_token(self.repeat_token.create_ast_node()?)
                .with_block(self.block.create_ast_node()?)
                .with_until_token(self.until_token.create_ast_node()?),
        )
    }
}

#[derive(LuaUserData)]
pub enum Stmt {
    Assignment(ArcLocked<Assignment>),
    Do(ArcLocked<Do>),
    FunctionCall(ArcLocked<FunctionCall>),
    FunctionDeclaration(ArcLocked<FunctionDeclaration>),
    GenericFor(ArcLocked<GenericFor>),
    If(ArcLocked<If>),
    LocalAssignment(ArcLocked<LocalAssignment>),
    LocalFunction(ArcLocked<LocalFunction>),
    NumericFor(ArcLocked<NumericFor>),
    Repeat(ArcLocked<Repeat>),
    While(ArcLocked<While>),

    NonStandard(Vec<ArcLocked<TokenReference>>),
}

impl Stmt {
    fn new(stmt: &ast::Stmt) -> Self {
        match stmt {
            ast::Stmt::Assignment(assignment) => Stmt::Assignment(l(Assignment::new(assignment))),
            ast::Stmt::Do(do_token) => Stmt::Do(l(Do::new(do_token))),
            ast::Stmt::FunctionCall(function_call) => {
                Stmt::FunctionCall(l(FunctionCall::new(function_call)))
            }
            ast::Stmt::FunctionDeclaration(function_declaration) => {
                Stmt::FunctionDeclaration(l(FunctionDeclaration::new(function_declaration)))
            }
            ast::Stmt::GenericFor(generic_for) => Stmt::GenericFor(l(GenericFor::new(generic_for))),
            ast::Stmt::If(if_token) => Stmt::If(l(If::new(if_token))),
            ast::Stmt::LocalAssignment(local_assignment) => {
                Stmt::LocalAssignment(l(LocalAssignment::new(local_assignment)))
            }
            ast::Stmt::LocalFunction(local_function) => {
                Stmt::LocalFunction(l(LocalFunction::new(local_function)))
            }
            ast::Stmt::NumericFor(numeric_for) => Stmt::NumericFor(l(NumericFor::new(numeric_for))),
            ast::Stmt::Repeat(repeat_token) => Stmt::Repeat(l(Repeat::new(repeat_token))),
            ast::Stmt::While(while_token) => Stmt::While(l(While::new(while_token))),

            // TODO: Support everything, then make this `unimplemented!`
            _ => Stmt::NonStandard(stmt.tokens().map(TokenReference::new).map(l).collect()),
        }
    }
}

#[derive(LuaUserData)]
pub enum Suffix {
    Call(ArcLocked<Call>),
    Index(ArcLocked<Index>),
}

impl Suffix {
    fn new(suffix: &ast::Suffix) -> Self {
        match suffix {
            ast::Suffix::Call(call) => Suffix::Call(l(Call::new(call))),
            ast::Suffix::Index(index) => Suffix::Index(l(Index::new(index))),
            other => unimplemented!("unexpected Suffix variant: {other:#?}"),
        }
    }
}

pub struct TableConstructor {
    braces: ContainedSpan,
    fields: Punctuated<Field>,
}

impl TableConstructor {
    fn new(table_constructor: &ast::TableConstructor) -> Self {
        TableConstructor {
            braces: ContainedSpan::new(table_constructor.braces()),
            fields: Punctuated::map_from_punctuated(table_constructor.fields(), Field::new),
        }
    }
}

impl UserData for TableConstructor {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("TableConstructor", methods);
    }
}

impl CreateAstNode for TableConstructor {
    type Node = ast::TableConstructor;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::TableConstructor::new()
                .with_braces(self.braces.create_ast_node()?)
                .with_fields(self.fields.create_ast_node()?),
        )
    }
}

#[derive(LuaUserData)]
pub enum UnOp {
    Minus(ArcLocked<TokenReference>),
    Not(ArcLocked<TokenReference>),
    Hash(ArcLocked<TokenReference>),
}

impl UnOp {
    fn new(unop: &ast::UnOp) -> Self {
        match unop {
            ast::UnOp::Minus(token) => UnOp::Minus(l(TokenReference::new(token))),
            ast::UnOp::Not(token) => UnOp::Not(l(TokenReference::new(token))),
            ast::UnOp::Hash(token) => UnOp::Hash(l(TokenReference::new(token))),
            other => panic!("unimplemented UnOp: {other:?}"),
        }
    }
}

#[derive(LuaUserData)]
pub enum Value {
    #[lua(
        create_ast_node = "ast::Value::Function((_0.create_ast_node()?, _1.create_ast_node()?))"
    )]
    Function(ArcLocked<TokenReference>, ArcLocked<FunctionBody>),
    FunctionCall(ArcLocked<FunctionCall>),
    TableConstructor(ArcLocked<TableConstructor>),
    Number(ArcLocked<TokenReference>),
    ParenthesesExpression(ArcLocked<Expression>),
    String(ArcLocked<TokenReference>),
    Symbol(ArcLocked<TokenReference>),
    Var(ArcLocked<Var>),

    NonStandard(Vec<ArcLocked<TokenReference>>),
}

impl Value {
    pub fn new(value: &ast::Value) -> Self {
        match value {
            ast::Value::Function((token_reference, function_body)) => Value::Function(
                l(TokenReference::new(token_reference)),
                l(FunctionBody::new(function_body)),
            ),

            ast::Value::FunctionCall(function_call) => {
                Value::FunctionCall(l(FunctionCall::new(function_call)))
            }

            ast::Value::TableConstructor(table_constructor) => {
                Value::TableConstructor(l(TableConstructor::new(table_constructor)))
            }

            ast::Value::Number(number) => Value::Number(l(TokenReference::new(number))),
            ast::Value::ParenthesesExpression(expression) => {
                Value::ParenthesesExpression(l(Expression::new(expression)))
            }
            ast::Value::String(string) => Value::String(l(TokenReference::new(string))),
            ast::Value::Symbol(symbol) => Value::Symbol(l(TokenReference::new(symbol))),
            ast::Value::Var(var) => Value::Var(l(Var::new(var))),

            // TODO: implement everything, then `unimplemented!`
            other => Value::NonStandard(other.tokens().map(TokenReference::new).map(l).collect()),
        }
    }
}

#[derive(LuaUserData)]
pub enum Var {
    Expression(ArcLocked<VarExpression>),
    Name(ArcLocked<TokenReference>),
}

impl Var {
    fn new(var: &ast::Var) -> Self {
        match var {
            ast::Var::Expression(expression) => Var::Expression(l(VarExpression::new(expression))),
            ast::Var::Name(name_token) => Var::Name(l(TokenReference::new(name_token))),
            other => unimplemented!("unexpected Var variant: {var:#?}"),
        }
    }
}

pub struct VarExpression {
    prefix: Prefix,
    suffixes: Vec<Suffix>,
}

impl VarExpression {
    fn new(var_expression: &ast::VarExpression) -> Self {
        VarExpression {
            prefix: Prefix::new(var_expression.prefix()),
            suffixes: var_expression
                .suffixes()
                .map(|suffix| Suffix::new(suffix))
                .collect(),
        }
    }
}

impl UserData for VarExpression {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("VarExpression", methods);
    }
}

impl CreateAstNode for VarExpression {
    type Node = ast::VarExpression;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::VarExpression::new(self.prefix.create_ast_node()?).with_suffixes(
                self.suffixes
                    .iter()
                    .map(|suffix| suffix.create_ast_node())
                    .collect::<Option<Vec<_>>>()?,
            ),
        )
    }
}

pub struct While {
    while_token: TokenReference,
    condition: Expression,
    do_token: TokenReference,
    block: Block,
    end_token: TokenReference,
}

impl While {
    fn new(while_token: &ast::While) -> Self {
        While {
            while_token: TokenReference::new(while_token.while_token()),
            condition: Expression::new(while_token.condition()),
            do_token: TokenReference::new(while_token.do_token()),
            block: Block::new(while_token.block()),
            end_token: TokenReference::new(while_token.end_token()),
        }
    }
}

impl UserData for While {
    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_to_string_display("While", methods);
    }
}

impl CreateAstNode for While {
    type Node = ast::While;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::While::new(self.condition.create_ast_node()?)
                .with_block(self.block.create_ast_node()?)
                .with_do_token(self.do_token.create_ast_node()?)
                .with_end_token(self.end_token.create_ast_node()?)
                .with_while_token(self.while_token.create_ast_node()?),
        )
    }
}
