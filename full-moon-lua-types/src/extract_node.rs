use crate::{core, mlua_util::ArcLocked, shared, CreateAstNode};

use full_moon::{ast, node::Node, tokenizer, visitors::Visit};
use mlua::FromLua;

macro_rules! all_nodes {
    (pub enum AnyNode {
        $(
            $name:ident: $lua_type:ty => $ast_type:ty,
        )+
    }) => {
        pub enum AnyNode {
            $(
                $name($ast_type),
            )+
        }

        impl<'lua> FromLua<'lua> for AnyNode {
            fn from_lua(value: mlua::Value, _: &mlua::Lua) -> mlua::Result<Self> {
                if let mlua::Value::UserData(user_data) = &value {
                    $(
                        if let Ok(lua_node) = user_data.borrow::<$lua_type>() {
                            return Ok(AnyNode::$name(match lua_node.create_ast_node() {
                                Some(ast_node) => ast_node,
                                None => return Err(mlua::Error::external(format!("could not convert {} to an AST node", stringify!($name)))),
                            }));
                        }

                        if let Ok(lua_node) = user_data.borrow::<ArcLocked<$lua_type>>() {
                            return Ok(AnyNode::$name(match lua_node.create_ast_node() {
                                Some(ast_node) => ast_node,
                                None => return Err(mlua::Error::external(format!("could not convert {} to an AST node", stringify!($name)))),
                            }));
                        }
                    )+
                }

                Err(mlua::Error::external(format!("Expected a node, received {}", value.type_name())))
            }
        }
    };
}

all_nodes!(pub enum AnyNode {
    Ast: core::Ast => ast::Ast,
    Assignment: core::Assignment => ast::Assignment,
    BinOp: core::BinOp => ast::BinOp,
    Block: core::Block => ast::Block,
    Call: core::Call => ast::Call,
    Do: core::Do => ast::Do,
    ElseIf: core::ElseIf => ast::ElseIf,
    Expression: core::Expression => ast::Expression,
    Field: core::Field => ast::Field,
    FunctionArgs: core::FunctionArgs => ast::FunctionArgs,
    FunctionBody: core::FunctionBody => ast::FunctionBody,
    FunctionCall: core::FunctionCall => ast::FunctionCall,
    FunctionDeclaration: core::FunctionDeclaration => ast::FunctionDeclaration,
    FunctionName: core::FunctionName => ast::FunctionName,
    GenericFor: core::GenericFor => ast::GenericFor,
    If: core::If => ast::If,
    Index: core::Index => ast::Index,
    LastStmt: core::LastStmt => ast::LastStmt,
    LocalAssignment: core::LocalAssignment => ast::LocalAssignment,
    LocalFunction: core::LocalFunction => ast::LocalFunction,
    MethodCall: core::MethodCall => ast::MethodCall,
    NumericFor: core::NumericFor => ast::NumericFor,
    Parameter: core::Parameter => ast::Parameter,
    Prefix: core::Prefix => ast::Prefix,
    Repeat: core::Repeat => ast::Repeat,
    Return: core::Return => ast::Return,
    Stmt: core::Stmt => ast::Stmt,
    Suffix: core::Suffix => ast::Suffix,
    TableConstructor: core::TableConstructor => ast::TableConstructor,
    UnOp: core::UnOp => ast::UnOp,
    Value: core::Value => ast::Value,
    Var: core::Var => ast::Var,
    VarExpression: core::VarExpression => ast::VarExpression,
    While: core::While => ast::While,

    ContainedSpan: shared::ContainedSpan => ast::span::ContainedSpan,
    TokenReference: shared::TokenReference => tokenizer::TokenReference,
});
