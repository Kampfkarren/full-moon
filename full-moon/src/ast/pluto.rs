//! Contains the types necessary to parse [Pluto](https://pluto-lang.org/).
use super::*;
use crate::{
    tokenizer::{TokenReference},
};
use derive_more::Display;

/// An assignment **expression**, i.e. `val := getval()` within an if or while condition.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{var_list}{walrus_token}{expr}")]
pub struct AssignmentExpression {
    pub(crate) var_list: Punctuated<Var>,
    pub(crate) walrus_token: TokenReference,
    pub(crate) expr: Box<Expression>,
}
