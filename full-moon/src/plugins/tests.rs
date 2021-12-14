use crate::{
    ast::*,
    define_parser, keep_going,
    parsers::{self, InternalAstError, Parser, ParserResult, ParserState},
    tokenizer::TokenReference,
};

use std::borrow::Cow;

use super::{Never, PluginMod};

use derive_more::Display;
use full_moon_derive::Node;
use insta::assert_yaml_snapshot;
use pretty_assertions::assert_eq;
use serde::{Deserialize, Serialize};

// PLUGIN TODO: Remove Deserialize, Serialize, and lift serde bounds
#[derive(Clone, Debug, PartialEq, Deserialize, Serialize)]
pub struct TestPlugin;

pub struct LastStmtMod;

#[derive(Clone, Debug, Display, PartialEq, Node, Deserialize, Serialize)]
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

#[derive(Clone, Debug, Default)]
pub struct ParseLastStmt;

impl Parser<TestPlugin> for ParseLastStmt {
    type Item = LastStmt<TestPlugin>;

    fn parse<'a>(
        &self,
        state: ParserState<'a, TestPlugin>,
    ) -> ParserResult<'a, TestPlugin, Self::Item> {
        if let Ok((state, last_stmt)) = keep_going!(parsers::ParseLastStmt.parse(state)) {
            return Ok((state, last_stmt));
        }

        let (state, identifier) = parsers::ParseIdentifier.parse(state)?;

        if identifier.token().to_string() == "imdone" {
            Ok((
                state,
                LastStmt::Plugin(CustomLastStmt { token: identifier }),
            ))
        } else {
            Err(parsers::InternalAstError::UnexpectedToken {
                token: identifier,
                additional: Some(Cow::from("Expected identifier to be imdone")),
            })
        }
    }
}

crate::create_plugin!(TestPlugin, {
    type LastStmtMod = LastStmtMod;
    type LastStmtParser = ParseLastStmt;
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

#[test]
fn test_custom_parser_last_stmt() {
    let ast = crate::parse_with_plugin::<TestPlugin>("imdone -- we're done").expect("parse failed");

    assert_yaml_snapshot!("ast", ast.nodes());
    assert_eq!(crate::print(&ast), "imdone -- we're done");
}
