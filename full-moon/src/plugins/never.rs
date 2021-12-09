use crate::{ast::*, node::Node, private};

use serde::{Deserialize, Serialize};

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
