use crate::tokenizer::Token;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use std::borrow::Cow;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum AstError {}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Ast<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pub tokens: Vec<Token<'a>>,
}
