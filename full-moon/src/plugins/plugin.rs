use std::fmt::{Debug, Display};

use crate::{ast::*, node::Node};

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

macro_rules! create_plugin {
    ({
        $($type:ty,)+
    }) => {
        // None of these traits are actually required, but because of how trait bounds are derived, Plugin has to have them
        pub trait Plugin: Clone + Debug + PartialEq {
            paste::item! {
                $(
                    type [<$type Mod>]: PluginMod<$type<Self>>;
                )+
            }
        }
    };
}

create_plugin!({
    Block,
    Call,
    ElseIf,
    Expression,
    Field,
    FunctionArgs,
    Index,
    LastStmt,
    Parameter,
    Prefix,
    Return,
    Stmt,
    Suffix,
    Value,
    Var,
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

    fn display(node_info: Self::NodeInfo) -> String;
}
