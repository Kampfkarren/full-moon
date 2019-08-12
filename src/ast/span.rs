use crate::{
    node::Node,
    private::Sealed,
    tokenizer::{Position, Token, TokenReference},
};

use full_moon_derive::Visit;
use serde::{Deserialize, Serialize};

#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ContainedSpan<'a> {
    #[serde(borrow)]
    #[visit(skip)]
    tokens: (TokenReference<'a>, TokenReference<'a>),
}

impl<'a> ContainedSpan<'a> {
    pub fn new(
        start: TokenReference<'a>,
        end: TokenReference<'a>,
    ) -> Self {
        Self { tokens: (start, end) }
    }

    pub fn tokens(&self) -> (&Token<'a>, &Token<'a>) {
        (&self.tokens.0, &self.tokens.1)
    }
}

impl<'a> Node for ContainedSpan<'a> {
    fn start_position(&self) -> Option<Position> {
        self.tokens.0.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.tokens.1.end_position()
    }
}

impl<'a> Sealed for ContainedSpan<'a> {}
