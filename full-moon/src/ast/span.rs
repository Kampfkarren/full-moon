//! A representation of a "contained span", or a span within specific bounds.
//!
//! Examples of contained spans include:
//! - Arguments in a function call use parentheses `(...)`
//! - Indexing a table uses brackets `[...]`
//! - Creating a table uses braces `{...}`
//!
//! Contained spans don't contain the inner data, just the start and end bounds.
use crate::{
    node::{Node, TokenItem, Tokens},
    private::Sealed,
    tokenizer::{Position, TokenReference},
    visitors::{Visit, VisitMut, Visitor, VisitorMut},
};

use derive_more::Display;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A contained span with the beginning and ending bounds.
/// Refer to the [module documentation](index.html) for more details.
#[derive(Clone, Debug, Display, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "start", "inner", "end")]
pub struct ContainedSpan<T> {
    pub(crate) start: TokenReference,
    pub(crate) inner: T,
    pub(crate) end: TokenReference,
}

impl<T> ContainedSpan<T> {
    /// Creates a contained span from the start and end bounds
    pub fn new(start: TokenReference, inner: T, end: TokenReference) -> Self {
        Self { start, inner, end }
    }

    /// Returns the start and end bounds in a tuple as references
    pub fn tokens(&self) -> (&TokenReference, &TokenReference) {
        (&self.start, &self.end)
    }

    /// Returns the node contained within the ContainedSpan
    pub fn inner(&self) -> &T {
        &self.inner
    }

    /// Creates a new ContainedSpan with the given containing tokens
    pub fn with_tokens(self, tokens: (TokenReference, TokenReference)) -> Self {
        Self {
            start: tokens.0,
            end: tokens.1,
            ..self
        }
    }

    /// Creates a new ContainedSpan with the given node contained within it
    pub fn with_inner(self, inner: T) -> Self {
        Self { inner, ..self }
    }
}

impl<T: Node> Node for ContainedSpan<T> {
    fn start_position(&self) -> Option<Position> {
        self.start.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.end.end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        self.start.similar(&other.start)
            && self.end.similar(&other.end)
            && self.inner.similar(&other.inner)
    }

    fn tokens(&self) -> Tokens {
        let mut items = self.start.tokens().items;
        items.append(&mut self.inner.tokens().items);
        items.push(TokenItem::TokenReference(&self.end));

        Tokens { items }
    }
}

impl<T> Sealed for ContainedSpan<T> {}

impl<T: Visit> Visit for ContainedSpan<T> {
    fn visit<V: Visitor>(&self, visitor: &mut V) {
        self.start.visit(visitor);
        self.inner.visit(visitor);
        self.end.visit(visitor);
    }
}

impl<T: VisitMut> VisitMut for ContainedSpan<T> {
    fn visit_mut<V: VisitorMut>(self, visitor: &mut V) -> Self {
        ContainedSpan {
            start: self.start.visit_mut(visitor),
            inner: self.inner.visit_mut(visitor),
            end: self.end.visit_mut(visitor),
        }
    }
}
