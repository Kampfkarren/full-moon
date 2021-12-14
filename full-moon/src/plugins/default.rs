use crate::ast::*;

use super::Never;

use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct DefaultPlugin;

#[macro_export]
macro_rules! create_plugin {
    ($plugin:ty, {$(
        $arbitrary_stmt:stmt
    )*}, {$(
        $default_type:ty: $default_node_info:ty,
    )*}) => {
        paste::item! {
            $(
                pub struct [<Default $default_type Info>];

                impl $crate::plugins::PluginMod<$default_type<$plugin>> for [<Default $default_type Info>] {
                    type NodeInfo = $default_node_info;

                    fn display(_: &$default_type<$plugin>) -> String {
                        unreachable!("DefaultPlugin info structs should not be being displayed");
                    }
                }
            )*

            impl $crate::plugins::Plugin for $plugin {
                $(
                    $arbitrary_stmt
                )*

                $(
                    type [<$default_type Mod>] = [<Default $default_type Info>];
                    type [<$default_type Parser>] = $crate::parsers::[<Parse $default_type>];
                )*
            }
        }
    };
}

// Structs get (), enums get Never
create_plugin!(DefaultPlugin, {}, {
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
