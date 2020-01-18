//! A representation of a "contained span", or a span within specific bounds.
//!
//! Examples of contained spans include:
//! - Arguments in a function call use parentheses `(...)`
//! - Indexing a table uses brackets `[...]`
//! - Creating a table uses braces `{...}`
//!
//! Contained spans don't contain the inner data, just the start and end bounds.
//! ```
use crate::{
    node::Node,
    private::Sealed,
    tokenizer::{Position, TokenReference},
};

use full_moon_derive::{Owned, Visit};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A contained span with the beginning and ending bounds.
/// Refer to the [module documentation](index.html) for more details.
#[derive(Clone, Debug, PartialEq, Owned, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ContainedSpan<'a> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    #[visit(skip)]
    tokens: (TokenReference<'a>, TokenReference<'a>),
}

impl<'a> ContainedSpan<'a> {
    /// Creates a contained span from the start and end bounds
    pub fn new(start: TokenReference<'a>, end: TokenReference<'a>) -> Self {
        Self {
            tokens: (start, end),
        }
    }

    /// Returns the start and end bounds in a tuple as references
    pub fn tokens(&self) -> (&TokenReference<'a>, &TokenReference<'a>) {
        (&self.tokens.0, &self.tokens.1)
    }

    /// Returns the start and end bounds in a tuple as mutable references
    pub fn tokens_mut(&mut self) -> (&mut TokenReference<'a>, &mut TokenReference<'a>) {
        (&mut self.tokens.0, &mut self.tokens.1)
    }
}

impl<'a> Node for ContainedSpan<'a> {
    fn start_position(&self) -> Option<Position> {
        self.tokens.0.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.tokens.1.end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        self.tokens.0.similar(&other.tokens.0) && self.tokens.1.similar(&other.tokens.1)
    }
}

impl<'a> Sealed for ContainedSpan<'a> {}
