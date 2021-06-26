//! A representation of a "contained span", or a span within specific bounds.
//!
//! Examples of contained spans include:
//! - Arguments in a function call use parentheses `(...)`
//! - Indexing a table uses brackets `[...]`
//! - Creating a table uses braces `{...}`
//!
//! Contained spans don't contain the inner data, just the start and end bounds.
use crate::{
    node::{Node, Tokens},
    private::Sealed,
    tokenizer::{Position, TokenReference},
};

use full_moon_derive::Visit;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A contained span with the beginning and ending bounds.
/// Refer to the [module documentation](index.html) for more details.
#[derive(Clone, Debug, PartialEq, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct ContainedSpan {
    pub(crate) tokens: (TokenReference, TokenReference),
}

impl ContainedSpan {
    /// Creates a contained span from the start and end bounds
    pub fn new(start: TokenReference, end: TokenReference) -> Self {
        Self {
            tokens: (start, end),
        }
    }

    /// Returns the start and end bounds in a tuple as references
    pub fn tokens(&self) -> (&TokenReference, &TokenReference) {
        (&self.tokens.0, &self.tokens.1)
    }
}

impl Node for ContainedSpan {
    fn start_position(&self) -> Option<Position> {
        self.tokens.0.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.tokens.1.end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        self.tokens.0.similar(&other.tokens.0) && self.tokens.1.similar(&other.tokens.1)
    }

    fn tokens(&self) -> Tokens {
        self.tokens.tokens()
    }
}

impl Sealed for ContainedSpan {}
