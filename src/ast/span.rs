use crate::{
    node::Node,
    private::Sealed,
    tokenizer::{Position, Token, TokenReference},
};

use full_moon_derive::Visit;

#[derive(Clone, Debug, PartialEq, Visit)]
pub struct ContainedSpan<'a> {
    #[visit(skip)]
    range: (Position, Position),
    tokens: (TokenReference<'a>, TokenReference<'a>),
}

impl<'a> ContainedSpan<'a> {
    pub fn new(
        range: (Position, Position),
        tokens: (TokenReference<'a>, TokenReference<'a>),
    ) -> Self {
        Self { range, tokens }
    }

    pub fn range(&self) -> (Position, Position) {
        (self.range.0, self.range.1)
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
