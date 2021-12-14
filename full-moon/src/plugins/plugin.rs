use std::fmt::Debug;

use crate::{
    ast::*,
    node::Node,
    parsers::{Parser, ParserResult},
};

#[cfg(feature = "serde")]
pub trait ConditionalSerialize: serde::Serialize {}

#[cfg(feature = "serde")]
impl<T> ConditionalSerialize for T where T: serde::Serialize {}

#[cfg(feature = "serde")]
pub trait ConditionalDeserialize: serde::de::DeserializeOwned {}

#[cfg(feature = "serde")]
impl<T> ConditionalDeserialize for T where T: serde::de::DeserializeOwned {}

#[cfg(not(feature = "serde"))]
pub trait ConditionalSerialize {}

#[cfg(not(feature = "serde"))]
impl<T> ConditionalSerialize for T {}

#[cfg(not(feature = "serde"))]
pub trait ConditionalDeserialize {}

#[cfg(not(feature = "serde"))]
impl<T> ConditionalDeserialize for T {}

macro_rules! create_base_plugin_trait {
    ({
        $($type:ty,)+
    }) => {
        // None of these traits are actually required, but because of how trait bounds are derived, Plugin has to have them
        pub trait Plugin: Clone + Debug + PartialEq {
            paste::item! {
                $(
                    type [<$type Mod>]: PluginMod<$type<Self>>;
                    type [<$type Parser>]: Parser<Self, Item = $type<Self>> + Default;
                )+
            }
        }
    };
}

create_base_plugin_trait!({
    Assignment,
    Block,
    Call,
    Do,
    ElseIf,
    Expression,
    Field,
    FunctionArgs,
    FunctionBody,
    FunctionCall,
    FunctionDeclaration,
    FunctionName,
    GenericFor,
    If,
    Index,
    LastStmt,
    LocalAssignment,
    LocalFunction,
    MethodCall,
    NumericFor,
    Parameter,
    Prefix,
    Repeat,
    Return,
    Stmt,
    Suffix,
    While,
    Value,
    Var,
    VarExpression,
});

// PLUGIN TODO: Relax Node restriction, and instead have them be functions on PluginMod itself
pub trait PluginInfo:
    Clone + Debug + Default + PartialEq + Node + ConditionalDeserialize + ConditionalSerialize
{
}

impl<T> PluginInfo for T where
    T: Clone + Debug + Default + PartialEq + Node + ConditionalDeserialize + ConditionalSerialize
{
}

pub trait PluginMod<T> {
    type NodeInfo: PluginInfo;

    fn display(node: &T) -> String;
}
