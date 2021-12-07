use crate::{ast::*, node::Node, private};

use super::{Plugin, PluginMod};

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

impl Plugin for DefaultPlugin {
    type BlockMod = DefaultBlockInfo;
    type CallMod = DefaultCallInfo;
    type ElseIfMod = DefaultElseIfInfo;
    type ExpressionMod = DefaultExpressionInfo;
    type FieldMod = DefaultFieldInfo;
    type FunctionArgsMod = DefaultFunctionArgsInfo;
    type IndexMod = DefaultIndexInfo;
    type LastStmtMod = DefaultLastStmtInfo;
    type ParameterMod = DefaultParameterInfo;
    type PrefixMod = DefaultPrefixInfo;
    type ReturnMod = DefaultReturnInfo;
    type StmtMod = DefaultStmtInfo;
    type SuffixMod = DefaultSuffixInfo;
    type ValueMod = DefaultValueInfo;
    type VarMod = DefaultVarInfo;
}

pub struct DefaultBlockInfo;

impl PluginMod<Block<DefaultPlugin>> for DefaultBlockInfo {
    type NodeInfo = ();
}

pub struct DefaultCallInfo;

impl PluginMod<Call<DefaultPlugin>> for DefaultCallInfo {
    type NodeInfo = Never;
}

pub struct DefaultElseIfInfo;

impl PluginMod<ElseIf<DefaultPlugin>> for DefaultElseIfInfo {
    type NodeInfo = ();
}

pub struct DefaultExpressionInfo;

impl PluginMod<Expression<DefaultPlugin>> for DefaultExpressionInfo {
    type NodeInfo = Never;
}

pub struct DefaultFieldInfo;

impl PluginMod<Field<DefaultPlugin>> for DefaultFieldInfo {
    type NodeInfo = Never;
}

pub struct DefaultFunctionArgsInfo;

impl PluginMod<FunctionArgs<DefaultPlugin>> for DefaultFunctionArgsInfo {
    type NodeInfo = Never;
}

pub struct DefaultIndexInfo;

impl PluginMod<Index<DefaultPlugin>> for DefaultIndexInfo {
    type NodeInfo = Never;
}

pub struct DefaultLastStmtInfo;

impl PluginMod<LastStmt<DefaultPlugin>> for DefaultLastStmtInfo {
    type NodeInfo = Never;
}

pub struct DefaultParameterInfo;

impl PluginMod<Parameter<DefaultPlugin>> for DefaultParameterInfo {
    type NodeInfo = Never;
}

pub struct DefaultPrefixInfo;

impl PluginMod<Prefix<DefaultPlugin>> for DefaultPrefixInfo {
    type NodeInfo = Never;
}

pub struct DefaultReturnInfo;

impl PluginMod<Return<DefaultPlugin>> for DefaultReturnInfo {
    type NodeInfo = ();
}

pub struct DefaultStmtInfo;

impl PluginMod<Stmt<DefaultPlugin>> for DefaultStmtInfo {
    type NodeInfo = Never;
}

pub struct DefaultSuffixInfo;

impl PluginMod<Suffix<DefaultPlugin>> for DefaultSuffixInfo {
    type NodeInfo = Never;
}

pub struct DefaultValueInfo;

impl PluginMod<Value<DefaultPlugin>> for DefaultValueInfo {
    type NodeInfo = Never;
}

pub struct DefaultVarInfo;

impl PluginMod<Var<DefaultPlugin>> for DefaultVarInfo {
    type NodeInfo = Never;
}
