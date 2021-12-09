use crate::{ast::*, tokenizer::TokenReference};

use super::{Never, PluginMod};

use derive_more::Display;
use full_moon_derive::Node;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq)]
pub struct TestPlugin;

pub struct LastStmtMod;

#[derive(Clone, Debug, Display, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct CustomLastStmt {
    token: TokenReference,
}

impl Default for CustomLastStmt {
    fn default() -> Self {
        unreachable!("Default should never be called on an enum variant")
    }
}

impl PluginMod<LastStmt<TestPlugin>> for LastStmtMod {
    type NodeInfo = CustomLastStmt;

    fn display(node: &LastStmt<TestPlugin>) -> String {
        match node {
            LastStmt::Plugin(node) => node.to_string(),
            _ => unreachable!("Enum nodes should only ever be Plugin"),
        }
    }
}

crate::create_plugin!(TestPlugin, {
    type LastStmtMod = LastStmtMod;
}, {
    Assignment: (),
    Block: (),
    Call: Never,
    Do: (),
    ElseIf: (),
    Expression: Never,
    Field: Never,
    FunctionArgs: Never,
    FunctionBody: (),
    FunctionCall: (),
    FunctionDeclaration: (),
    FunctionName: (),
    GenericFor: (),
    If: (),
    Index: Never,
    LocalAssignment: (),
    LocalFunction: (),
    MethodCall: (),
    NumericFor: (),
    Parameter: Never,
    Prefix: Never,
    Repeat: (),
    Return: (),
    Stmt: Never,
    Suffix: Never,
    Value: Never,
    Var: Never,
    VarExpression: (),
    While: (),
});
