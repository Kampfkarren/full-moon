use std::sync::{Arc, RwLock};

use full_moon::{ast, node::Node, tokenizer};
use full_moon_lua_types_derive::LuaUserData;
use mlua::{Table, ToLua, UserData};

use crate::{
    create_ast_node::CreateAstNode,
    mlua_util::{
        add_core_meta_methods, add_create_ast_node_methods, add_newindex_block, add_print,
        add_visit, ArcLocked,
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

impl From<&ast::Ast> for Ast {
    fn from(ast: &ast::Ast) -> Self {
        Ast {
            nodes: l(Block::new(ast.nodes())),
            eof: l(TokenReference::new(ast.eof())),
        }
    }
}

impl UserData for Ast {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("eof", |_, Ast { eof, .. }| Ok(eof.clone()));
        fields.add_field_method_get("nodes", |_, Ast { nodes, .. }| Ok(nodes.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Ast", methods);

        crate::visitor::add_visit_with_visitor(methods, |ast, visitor| {
            use full_moon::visitors::Visitor;
            visitor.visit_ast(&ast);
        });
    }
}

impl CreateAstNode for Ast {
    type Node = ast::Ast;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::Ast::from_tokens(vec![tokenizer::Token::new(tokenizer::TokenType::Eof)])
                .unwrap()
                .with_eof(self.eof.create_ast_node()?)
                .with_nodes(self.nodes.create_ast_node()?),
        )
    }
}

pub struct Assignment {
    var_list: ArcLocked<Punctuated<Var>>,
    equal_token: ArcLocked<TokenReference>,
    expr_list: ArcLocked<Punctuated<Expression>>,
}

impl Assignment {
    pub fn new(assignment: &ast::Assignment) -> Self {
        Assignment {
            var_list: l(Punctuated::map_from_punctuated(
                assignment.variables(),
                Var::new,
            )),
            equal_token: l(TokenReference::new(assignment.equal_token())),
            expr_list: l(Punctuated::map_from_punctuated(
                assignment.expressions(),
                Expression::new,
            )),
        }
    }
}

impl UserData for Assignment {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("variables", |_, Assignment { var_list, .. }| {
            Ok(var_list.clone())
        });

        fields.add_field_method_get("equal_token", |_, Assignment { equal_token, .. }| {
            Ok(equal_token.clone())
        });

        fields.add_field_method_get("expressions", |_, Assignment { expr_list, .. }| {
            Ok(expr_list.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Assignment", methods);
        add_create_ast_node_methods(methods);
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

#[derive(Clone, LuaUserData)]
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
    pub fn new(bin_op: &ast::BinOp) -> Self {
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

#[derive(Clone)]
pub struct Block {
    stmts: Vec<(ArcLocked<Stmt>, Option<ArcLocked<TokenReference>>)>,
    last_stmt: Option<(ArcLocked<LastStmt>, Option<ArcLocked<TokenReference>>)>,
}

impl Block {
    pub fn new(block: &ast::Block) -> Self {
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

        fields.add_field_method_get("last_stmt", |_, Block { last_stmt, .. }| {
            Ok(last_stmt.as_ref().map(|(last_stmt, _)| last_stmt.clone()))
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Block", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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
                .with_last_stmt(self.last_stmt.as_ref().and_then(|(last_stmt, token)| {
                    Some((last_stmt.create_ast_node()?, token.create_ast_node()))
                })),
        )
    }
}

#[derive(Clone, LuaUserData)]
pub enum Call {
    AnonymousCall(ArcLocked<FunctionArgs>),
    MethodCall(ArcLocked<MethodCall>),
}

impl Call {
    pub fn new(call: &ast::Call) -> Self {
        match call {
            ast::Call::AnonymousCall(function_args) => {
                Call::AnonymousCall(l(FunctionArgs::new(function_args)))
            }
            ast::Call::MethodCall(method_call) => Call::MethodCall(l(MethodCall::new(method_call))),
            other => panic!("unimplemented Call: {other:?}"),
        }
    }
}

#[derive(Clone)]
pub struct Do {
    do_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
    end_token: ArcLocked<TokenReference>,
}

impl Do {
    pub fn new(do_: &ast::Do) -> Self {
        Do {
            do_token: l(TokenReference::new(do_.do_token())),
            block: l(Block::new(do_.block())),
            end_token: l(TokenReference::new(do_.end_token())),
        }
    }
}

impl UserData for Do {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("do_token", |_, Do { do_token, .. }| Ok(do_token.clone()));

        fields.add_field_method_get("block", |_, Do { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("end_token", |_, Do { end_token, .. }| Ok(end_token.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Do", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct ElseIf {
    else_if_token: ArcLocked<TokenReference>,
    condition: ArcLocked<Expression>,
    then_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
}

impl ElseIf {
    pub fn new(else_if: &ast::ElseIf) -> Self {
        ElseIf {
            else_if_token: l(TokenReference::new(else_if.else_if_token())),
            condition: l(Expression::new(else_if.condition())),
            then_token: l(TokenReference::new(else_if.then_token())),
            block: l(Block::new(else_if.block())),
        }
    }
}

impl UserData for ElseIf {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("else_if_token", |_, ElseIf { else_if_token, .. }| {
            Ok(else_if_token.clone())
        });

        fields.add_field_method_get("condition", |_, ElseIf { condition, .. }| {
            Ok(condition.clone())
        });

        fields.add_field_method_get("then_token", |_, ElseIf { then_token, .. }| {
            Ok(then_token.clone())
        });

        fields.add_field_method_get("block", |_, ElseIf { block, .. }| Ok(block.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("ElseIf", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

    #[cfg_attr(feature = "luau", lua(add_field = "type_assertion: None"))]
    Value { value: Box<ArcLocked<Value>> },
}

impl Expression {
    pub fn new(expression: &ast::Expression) -> Self {
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

            ast::Expression::Value { value, .. } => Expression::Value {
                value: Box::new(l(Value::new(value))),
            },

            other => panic!("unimplemented Expression: {other:?}"),
        }
    }
}

#[derive(Clone, LuaUserData)]
pub enum FunctionArgs {
    Parentheses {
        parentheses: ArcLocked<ContainedSpan>,
        arguments: ArcLocked<Punctuated<Expression>>,
    },

    String(ArcLocked<TokenReference>),

    TableConstructor(ArcLocked<TableConstructor>),
}

impl FunctionArgs {
    pub fn new(function_args: &ast::FunctionArgs) -> Self {
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

#[derive(Clone)]
pub struct FunctionBody {
    parameters_parentheses: ArcLocked<ContainedSpan>,
    parameters: ArcLocked<Punctuated<Parameter>>,
    block: ArcLocked<Block>,
    end_token: ArcLocked<TokenReference>,
}

impl FunctionBody {
    pub fn new(function_body: &ast::FunctionBody) -> Self {
        FunctionBody {
            parameters_parentheses: l(ContainedSpan::new(function_body.parameters_parentheses())),

            parameters: l(Punctuated::map_from_punctuated(
                function_body.parameters(),
                Parameter::new,
            )),

            block: l(Block::new(function_body.block())),
            end_token: l(TokenReference::new(function_body.end_token())),
        }
    }
}

impl UserData for FunctionBody {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get(
            "parameters_parentheses",
            |_,
             FunctionBody {
                 parameters_parentheses,
                 ..
             }| { Ok(parameters_parentheses.clone()) },
        );

        fields.add_field_method_get("parameters", |_, FunctionBody { parameters, .. }| {
            Ok(parameters.clone())
        });

        fields.add_field_method_get("block", |_, FunctionBody { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("end_token", |_, FunctionBody { end_token, .. }| {
            Ok(end_token.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("FunctionBody", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone, LuaUserData)]
pub enum LastStmt {
    Break(ArcLocked<TokenReference>),
    #[cfg(feature = "luau")]
    Continue(ArcLocked<TokenReference>),
    Return(ArcLocked<Return>),
}

impl LastStmt {
    pub fn new(last_stmt: &ast::LastStmt) -> Self {
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

#[derive(Clone, LuaUserData)]
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
    pub fn new(field: &ast::Field) -> Self {
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

#[derive(Clone)]
pub struct FunctionCall {
    prefix: ArcLocked<Prefix>,
    suffixes: Vec<ArcLocked<Suffix>>,
}

impl FunctionCall {
    pub fn new(function_call: &ast::FunctionCall) -> Self {
        FunctionCall {
            prefix: l(Prefix::new(function_call.prefix())),
            suffixes: function_call.suffixes().map(Suffix::new).map(l).collect(),
        }
    }
}

impl UserData for FunctionCall {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get(
            "prefix",
            |_, FunctionCall { prefix, .. }| Ok(prefix.clone()),
        );

        fields.add_field_method_get("suffixes", |_, FunctionCall { suffixes, .. }| {
            Ok(suffixes.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("FunctionCall", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
    }
}

impl CreateAstNode for FunctionCall {
    type Node = ast::FunctionCall;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::FunctionCall::new(self.prefix.create_ast_node()?).with_suffixes(
                self.suffixes
                    .iter()
                    .map(|suffix| suffix.read().unwrap().create_ast_node())
                    .collect::<Option<Vec<_>>>()?,
            ),
        )
    }
}

#[derive(Clone)]
pub struct FunctionDeclaration {
    function_token: ArcLocked<TokenReference>,
    name: ArcLocked<FunctionName>,
    body: ArcLocked<FunctionBody>,
}

impl FunctionDeclaration {
    pub fn new(function_declaration: &ast::FunctionDeclaration) -> Self {
        FunctionDeclaration {
            function_token: l(TokenReference::new(function_declaration.function_token())),
            name: l(FunctionName::new(function_declaration.name())),
            body: l(FunctionBody::new(function_declaration.body())),
        }
    }
}

impl UserData for FunctionDeclaration {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get(
            "function_token",
            |_, FunctionDeclaration { function_token, .. }| Ok(function_token.clone()),
        );

        fields.add_field_method_get("name", |_, FunctionDeclaration { name, .. }| {
            Ok(name.clone())
        });

        fields.add_field_method_get("body", |_, FunctionDeclaration { body, .. }| {
            Ok(body.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("FunctionDeclaration", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct FunctionName {
    names: ArcLocked<Punctuated<TokenReference>>,
    colon_name: Option<ArcLocked<(TokenReference, TokenReference)>>,
}

impl FunctionName {
    pub fn new(function_name: &ast::FunctionName) -> Self {
        FunctionName {
            names: l(Punctuated::map_from_punctuated(
                function_name.names(),
                TokenReference::new,
            )),

            colon_name: match (function_name.method_colon(), function_name.method_name()) {
                (Some(colon), Some(name)) => {
                    Some(l((TokenReference::new(colon), TokenReference::new(name))))
                }

                _ => None,
            },
        }
    }
}

impl UserData for FunctionName {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("names", |_, FunctionName { names, .. }| Ok(names.clone()));

        fields.add_field_method_get("method_colon", |_, FunctionName { colon_name, .. }| {
            Ok(colon_name
                .as_ref()
                .map(|lock| Arc::clone(lock).read().unwrap().0.clone()))
        });

        fields.add_field_method_get("method_name", |_, FunctionName { colon_name, .. }| {
            Ok(colon_name
                .as_ref()
                .map(|lock| Arc::clone(lock).read().unwrap().1.clone()))
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("FunctionName", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
    }
}

impl CreateAstNode for FunctionName {
    type Node = ast::FunctionName;

    fn create_ast_node(&self) -> Option<Self::Node> {
        Some(
            ast::FunctionName::new(self.names.create_ast_node()?).with_method(
                self.colon_name.as_ref().and_then(|colon_name_arc| {
                    let colon_name_arc = Arc::clone(colon_name_arc);
                    let lock = colon_name_arc.read().unwrap();
                    Some((lock.0.create_ast_node()?, lock.1.create_ast_node()?))
                }),
            ),
        )
    }
}

#[derive(Clone)]
pub struct GenericFor {
    for_token: ArcLocked<TokenReference>,
    names: ArcLocked<Punctuated<TokenReference>>,
    in_token: ArcLocked<TokenReference>,
    expr_list: ArcLocked<Punctuated<Expression>>,
    do_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
    end_token: ArcLocked<TokenReference>,
}

impl GenericFor {
    pub fn new(generic_for: &ast::GenericFor) -> Self {
        GenericFor {
            for_token: l(TokenReference::new(generic_for.for_token())),
            names: l(Punctuated::map_from_punctuated(
                generic_for.names(),
                TokenReference::new,
            )),
            in_token: l(TokenReference::new(generic_for.in_token())),
            expr_list: l(Punctuated::map_from_punctuated(
                generic_for.expressions(),
                Expression::new,
            )),
            do_token: l(TokenReference::new(generic_for.do_token())),
            block: l(Block::new(generic_for.block())),
            end_token: l(TokenReference::new(generic_for.end_token())),
        }
    }
}

impl UserData for GenericFor {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("for_token", |_, GenericFor { for_token, .. }| {
            Ok(for_token.clone())
        });

        fields.add_field_method_get("names", |_, GenericFor { names, .. }| Ok(names.clone()));

        fields.add_field_method_get("in_token", |_, GenericFor { in_token, .. }| {
            Ok(in_token.clone())
        });

        fields.add_field_method_get("expressions", |_, GenericFor { expr_list, .. }| {
            Ok(expr_list.clone())
        });

        fields.add_field_method_get("do_token", |_, GenericFor { do_token, .. }| {
            Ok(do_token.clone())
        });

        fields.add_field_method_get("block", |_, GenericFor { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("end_token", |_, GenericFor { end_token, .. }| {
            Ok(end_token.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("GenericFor", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct If {
    if_token: ArcLocked<TokenReference>,
    condition: ArcLocked<Expression>,
    then_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
    else_if: Option<Vec<ArcLocked<ElseIf>>>,
    else_token: Option<ArcLocked<TokenReference>>,
    else_block: Option<ArcLocked<Block>>,
    end_token: ArcLocked<TokenReference>,
}

impl If {
    pub fn new(if_node: &ast::If) -> Self {
        If {
            if_token: l(TokenReference::new(if_node.if_token())),
            condition: l(Expression::new(if_node.condition())),
            then_token: l(TokenReference::new(if_node.then_token())),
            block: l(Block::new(if_node.block())),
            else_if: if_node
                .else_if()
                .map(|else_if| else_if.iter().map(ElseIf::new).map(l).collect()),
            else_token: if_node.else_token().map(TokenReference::new).map(l),
            else_block: if_node.else_block().map(Block::new).map(l),
            end_token: l(TokenReference::new(if_node.end_token())),
        }
    }
}

impl UserData for If {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("if_token", |_, If { if_token, .. }| Ok(if_token.clone()));

        fields.add_field_method_get("condition", |_, If { condition, .. }| Ok(condition.clone()));

        fields.add_field_method_get("then_token", |_, If { then_token, .. }| {
            Ok(then_token.clone())
        });

        fields.add_field_method_get("block", |_, If { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("else_if", |lua, If { else_if, .. }| {
            Ok(else_if
                .as_ref()
                .map(|else_if| else_if.iter().map(Arc::clone).collect::<Vec<_>>()))
        });

        fields.add_field_method_get("else_token", |_, If { else_token, .. }| {
            Ok(else_token.clone())
        });

        fields.add_field_method_get("else_block", |_, If { else_block, .. }| {
            Ok(else_block.clone())
        });

        fields.add_field_method_get("end_token", |_, If { end_token, .. }| Ok(end_token.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("If", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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
                .with_else_if(self.else_if.as_ref().and_then(|else_if| {
                    else_if
                        .iter()
                        .map(|else_if| else_if.read().unwrap().create_ast_node())
                        .collect::<Option<Vec<_>>>()
                }))
                .with_else_token(self.else_token.create_ast_node())
                .with_else(self.else_block.create_ast_node())
                .with_end_token(self.end_token.create_ast_node()?),
        )
    }
}

#[derive(Clone, LuaUserData)]
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
    pub fn new(index: &ast::Index) -> Self {
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

#[derive(Clone)]
pub struct LocalAssignment {
    local_token: ArcLocked<TokenReference>,
    name_list: ArcLocked<Punctuated<TokenReference>>,
    equal_token: Option<ArcLocked<TokenReference>>,
    expr_list: ArcLocked<Punctuated<Expression>>,
}

impl LocalAssignment {
    pub fn new(local_assignment: &ast::LocalAssignment) -> Self {
        LocalAssignment {
            local_token: l(TokenReference::new(local_assignment.local_token())),
            name_list: l(Punctuated::map_from_punctuated(
                local_assignment.names(),
                TokenReference::new,
            )),
            equal_token: local_assignment
                .equal_token()
                .map(TokenReference::new)
                .map(l),
            expr_list: l(Punctuated::map_from_punctuated(
                local_assignment.expressions(),
                Expression::new,
            )),
        }
    }
}

impl UserData for LocalAssignment {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("local_token", |_, LocalAssignment { local_token, .. }| {
            Ok(local_token.clone())
        });

        fields.add_field_method_get("names", |_, LocalAssignment { name_list, .. }| {
            Ok(name_list.clone())
        });

        fields.add_field_method_get("equal_token", |_, LocalAssignment { equal_token, .. }| {
            Ok(equal_token.clone())
        });

        fields.add_field_method_get("expressions", |_, LocalAssignment { expr_list, .. }| {
            Ok(expr_list.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("LocalAssignment", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct LocalFunction {
    local_token: ArcLocked<TokenReference>,
    function_token: ArcLocked<TokenReference>,
    name: ArcLocked<TokenReference>,
    body: ArcLocked<FunctionBody>,
}

impl LocalFunction {
    pub fn new(local_function: &ast::LocalFunction) -> Self {
        LocalFunction {
            local_token: l(TokenReference::new(local_function.local_token())),
            function_token: l(TokenReference::new(local_function.function_token())),
            name: l(TokenReference::new(local_function.name())),
            body: l(FunctionBody::new(local_function.body())),
        }
    }
}

impl UserData for LocalFunction {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("local_token", |_, LocalFunction { local_token, .. }| {
            Ok(local_token.clone())
        });

        fields.add_field_method_get(
            "function_token",
            |_, LocalFunction { function_token, .. }| Ok(function_token.clone()),
        );

        fields.add_field_method_get("name", |_, LocalFunction { name, .. }| Ok(name.clone()));

        fields.add_field_method_get("body", |_, LocalFunction { body, .. }| Ok(body.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("LocalFunction", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct MethodCall {
    colon_token: ArcLocked<TokenReference>,
    name: ArcLocked<TokenReference>,
    args: ArcLocked<FunctionArgs>,
}

impl MethodCall {
    pub fn new(method_call: &ast::MethodCall) -> Self {
        MethodCall {
            colon_token: l(TokenReference::new(method_call.colon_token())),
            name: l(TokenReference::new(method_call.name())),
            args: l(FunctionArgs::new(method_call.args())),
        }
    }
}

impl UserData for MethodCall {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("colon_token", |_, MethodCall { colon_token, .. }| {
            Ok(colon_token.clone())
        });

        fields.add_field_method_get("name", |_, MethodCall { name, .. }| Ok(name.clone()));

        fields.add_field_method_get("args", |_, MethodCall { args, .. }| Ok(args.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("MethodCall", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct NumericFor {
    for_token: ArcLocked<TokenReference>,
    index_variable: ArcLocked<TokenReference>,
    equal_token: ArcLocked<TokenReference>,
    start: ArcLocked<Expression>,
    start_end_comma: ArcLocked<TokenReference>,
    end: ArcLocked<Expression>,
    end_step_comma: Option<ArcLocked<TokenReference>>,
    step: Option<ArcLocked<Expression>>,
    do_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
    end_token: ArcLocked<TokenReference>,
}

impl NumericFor {
    pub fn new(numeric_for: &ast::NumericFor) -> Self {
        NumericFor {
            for_token: l(TokenReference::new(numeric_for.for_token())),
            index_variable: l(TokenReference::new(numeric_for.index_variable())),
            equal_token: l(TokenReference::new(numeric_for.equal_token())),
            start: l(Expression::new(numeric_for.start())),
            start_end_comma: l(TokenReference::new(numeric_for.start_end_comma())),
            end: l(Expression::new(numeric_for.end())),
            end_step_comma: numeric_for.end_step_comma().map(TokenReference::new).map(l),
            step: numeric_for.step().map(Expression::new).map(l),
            do_token: l(TokenReference::new(numeric_for.do_token())),
            block: l(Block::new(numeric_for.block())),
            end_token: l(TokenReference::new(numeric_for.end_token())),
        }
    }
}

impl UserData for NumericFor {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("for_token", |_, NumericFor { for_token, .. }| {
            Ok(for_token.clone())
        });

        fields.add_field_method_get("index_variable", |_, NumericFor { index_variable, .. }| {
            Ok(index_variable.clone())
        });

        fields.add_field_method_get("equal_token", |_, NumericFor { equal_token, .. }| {
            Ok(equal_token.clone())
        });

        fields.add_field_method_get("start", |_, NumericFor { start, .. }| Ok(start.clone()));

        fields.add_field_method_get(
            "start_end_comma",
            |_,
             NumericFor {
                 start_end_comma, ..
             }| Ok(start_end_comma.clone()),
        );

        fields.add_field_method_get("end", |_, NumericFor { end, .. }| Ok(end.clone()));

        fields.add_field_method_get("end_step_comma", |_, NumericFor { end_step_comma, .. }| {
            Ok(end_step_comma.clone())
        });

        fields.add_field_method_get("step", |_, NumericFor { step, .. }| Ok(step.clone()));

        fields.add_field_method_get("do_token", |_, NumericFor { do_token, .. }| {
            Ok(do_token.clone())
        });

        fields.add_field_method_get("block", |_, NumericFor { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("end_token", |_, NumericFor { end_token, .. }| {
            Ok(end_token.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("NumericFor", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone, LuaUserData)]
pub enum Parameter {
    Ellipse(ArcLocked<TokenReference>),
    Name(ArcLocked<TokenReference>),
}

impl Parameter {
    pub fn new(parameter: &ast::Parameter) -> Self {
        match parameter {
            ast::Parameter::Ellipse(ellipse_token) => {
                Parameter::Ellipse(l(TokenReference::new(ellipse_token)))
            }

            ast::Parameter::Name(name_token) => Parameter::Name(l(TokenReference::new(name_token))),

            _ => unimplemented!("unexpected Parameter variant: {parameter:#?}"),
        }
    }
}

#[derive(Clone, LuaUserData)]
pub enum Prefix {
    Expression(ArcLocked<Expression>),
    Name(ArcLocked<TokenReference>),
}

impl Prefix {
    pub fn new(prefix: &ast::Prefix) -> Self {
        match prefix {
            ast::Prefix::Expression(expr) => Prefix::Expression(l(Expression::new(expr))),
            ast::Prefix::Name(name) => Prefix::Name(l(TokenReference::new(name))),
            other => unimplemented!("unexpected Prefix variant: {other:?}"),
        }
    }
}

#[derive(Clone)]
pub struct Return {
    token: ArcLocked<TokenReference>,
    returns: ArcLocked<Punctuated<Expression>>,
}

impl Return {
    pub fn new(return_token: &ast::Return) -> Self {
        Return {
            token: l(TokenReference::new(return_token.token())),
            returns: l(Punctuated::map_from_punctuated(
                return_token.returns(),
                Expression::new,
            )),
        }
    }
}

impl UserData for Return {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("token", |_, Return { token, .. }| Ok(token.clone()));

        fields.add_field_method_get("returns", |_, Return { returns, .. }| Ok(returns.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Return", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct Repeat {
    repeat_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
    until_token: ArcLocked<TokenReference>,
    until: ArcLocked<Expression>,
}

impl Repeat {
    pub fn new(repeat: &ast::Repeat) -> Self {
        Repeat {
            repeat_token: l(TokenReference::new(repeat.repeat_token())),
            block: l(Block::new(repeat.block())),
            until_token: l(TokenReference::new(repeat.until_token())),
            until: l(Expression::new(repeat.until())),
        }
    }
}

impl UserData for Repeat {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("repeat_token", |_, Repeat { repeat_token, .. }| {
            Ok(repeat_token.clone())
        });

        fields.add_field_method_get("block", |_, Repeat { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("until_token", |_, Repeat { until_token, .. }| {
            Ok(until_token.clone())
        });

        fields.add_field_method_get("until", |_, Repeat { until, .. }| Ok(until.clone()));
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("Repeat", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone, LuaUserData)]
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
    pub fn new(stmt: &ast::Stmt) -> Self {
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

#[derive(Clone, LuaUserData)]
pub enum Suffix {
    Call(ArcLocked<Call>),
    Index(ArcLocked<Index>),
}

impl Suffix {
    pub fn new(suffix: &ast::Suffix) -> Self {
        match suffix {
            ast::Suffix::Call(call) => Suffix::Call(l(Call::new(call))),
            ast::Suffix::Index(index) => Suffix::Index(l(Index::new(index))),
            other => unimplemented!("unexpected Suffix variant: {other:#?}"),
        }
    }
}

#[derive(Clone)]
pub struct TableConstructor {
    braces: ArcLocked<ContainedSpan>,
    fields: ArcLocked<Punctuated<Field>>,
}

impl TableConstructor {
    pub fn new(table_constructor: &ast::TableConstructor) -> Self {
        TableConstructor {
            braces: l(ContainedSpan::new(table_constructor.braces())),
            fields: l(Punctuated::map_from_punctuated(
                table_constructor.fields(),
                Field::new,
            )),
        }
    }
}

impl UserData for TableConstructor {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("braces", |_, TableConstructor { braces, .. }| {
            Ok(braces.clone())
        });

        fields.add_field_method_get("fields", |_, TableConstructor { fields, .. }| {
            Ok(fields.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("TableConstructor", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone, LuaUserData)]
pub enum UnOp {
    Minus(ArcLocked<TokenReference>),
    Not(ArcLocked<TokenReference>),
    Hash(ArcLocked<TokenReference>),
}

impl UnOp {
    pub fn new(unop: &ast::UnOp) -> Self {
        match unop {
            ast::UnOp::Minus(token) => UnOp::Minus(l(TokenReference::new(token))),
            ast::UnOp::Not(token) => UnOp::Not(l(TokenReference::new(token))),
            ast::UnOp::Hash(token) => UnOp::Hash(l(TokenReference::new(token))),
            other => panic!("unimplemented UnOp: {other:?}"),
        }
    }
}

#[derive(Clone, LuaUserData)]
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

#[derive(Clone, LuaUserData)]
pub enum Var {
    Expression(ArcLocked<VarExpression>),
    Name(ArcLocked<TokenReference>),
}

impl Var {
    pub fn new(var: &ast::Var) -> Self {
        match var {
            ast::Var::Expression(expression) => Var::Expression(l(VarExpression::new(expression))),
            ast::Var::Name(name_token) => Var::Name(l(TokenReference::new(name_token))),
            other => unimplemented!("unexpected Var variant: {var:#?}"),
        }
    }
}

#[derive(Clone)]
pub struct VarExpression {
    prefix: ArcLocked<Prefix>,
    suffixes: Vec<ArcLocked<Suffix>>,
}

impl VarExpression {
    pub fn new(var_expression: &ast::VarExpression) -> Self {
        VarExpression {
            prefix: l(Prefix::new(var_expression.prefix())),
            suffixes: var_expression.suffixes().map(Suffix::new).map(l).collect(),
        }
    }
}

impl UserData for VarExpression {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("prefix", |_, VarExpression { prefix, .. }| {
            Ok(prefix.clone())
        });

        fields.add_field_method_get("suffixes", |_, VarExpression { suffixes, .. }| {
            Ok(suffixes.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("VarExpression", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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

#[derive(Clone)]
pub struct While {
    while_token: ArcLocked<TokenReference>,
    condition: ArcLocked<Expression>,
    do_token: ArcLocked<TokenReference>,
    block: ArcLocked<Block>,
    end_token: ArcLocked<TokenReference>,
}

impl While {
    pub fn new(while_token: &ast::While) -> Self {
        While {
            while_token: l(TokenReference::new(while_token.while_token())),
            condition: l(Expression::new(while_token.condition())),
            do_token: l(TokenReference::new(while_token.do_token())),
            block: l(Block::new(while_token.block())),
            end_token: l(TokenReference::new(while_token.end_token())),
        }
    }
}

impl UserData for While {
    fn add_fields<'lua, F: mlua::UserDataFields<'lua, Self>>(fields: &mut F) {
        fields.add_field_method_get("while_token", |_, While { while_token, .. }| {
            Ok(while_token.clone())
        });

        fields.add_field_method_get("condition", |_, While { condition, .. }| {
            Ok(condition.clone())
        });

        fields.add_field_method_get("do_token", |_, While { do_token, .. }| Ok(do_token.clone()));

        fields.add_field_method_get("block", |_, While { block, .. }| Ok(block.clone()));

        fields.add_field_method_get("end_token", |_, While { end_token, .. }| {
            Ok(end_token.clone())
        });
    }

    fn add_methods<'lua, M: mlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        add_core_meta_methods("While", methods);
        add_create_ast_node_methods(methods);
        add_print(methods);
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
