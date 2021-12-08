use crate::{ast::*, node::Node, private};

use super::{Plugin, PluginInfo, PluginMod};

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct DefaultPlugin;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Never {}

impl private::Sealed for Never {}

impl Default for Never {
    fn default() -> Self {
        unreachable!()
    }
}

impl Node for Never {
    fn start_position(&self) -> Option<crate::tokenizer::Position> {
        None
    }

    fn end_position(&self) -> Option<crate::tokenizer::Position> {
        None
    }

    fn similar(&self, other: &Self) -> bool
    where
        Self: Sized,
    {
        true
    }

    fn tokens(&self) -> crate::node::Tokens {
        crate::node::Tokens { items: Vec::new() }
    }
}

macro_rules! plugin_infos {
    ({$(
        $type:ty: $node_info:ty,
    )+}) => {
        paste::item! {
            $(
                pub struct [<Default $type Info>];

                impl PluginMod<$type<DefaultPlugin>> for [<Default $type Info>] {
                    type NodeInfo = $node_info;

                    fn display(_: &$node_info) -> String {
                        unreachable!("DefaultPlugin info structs should not be being displayed");
                    }
                }
            )+

            impl Plugin for DefaultPlugin {
                $(
                    type [<$type Mod>] = [<Default $type Info>];
                )+
            }
        }
    };
}

// Structs get (), enums get Never
plugin_infos!({
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
    LastStmt: Never,
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
