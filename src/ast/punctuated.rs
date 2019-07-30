//! A punctuated sequence of syntax tree nodes separated by punctuation (tokens).
//!
//! Examples of punctuated sequences include:
//! - Arguments in a function call are `Punctuated<Expression>`
//! - Names and definitions in a local assignment are `Punctuated<TokenReference>` and `Punctuated<Expression>` respectively
//! - The values of a return statement are `Punctuated<Expression>`
//!
//! Everything with punctuation uses the [`Punctuated<T>`](struct.Punctuated.html) type with the following logic.
//! ```rust
//! # use full_moon::parse;
//! # fn main() -> Result<(), Box<std::error::Error>> {
//! let ast = parse("call(arg1, arg2, arg3)")?;
//! //                   ^^^^^ ~~~~~ ^^^^^
//! # Ok(())
//! # }
//! ```
use crate::{
    node::Node,
    private::Sealed,
    tokenizer::{Position, TokenReference},
    visitors::{Visit, Visitor, VisitMut, VisitorMut},
};
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A punctuated sequence of node `T` separated by [`TokenReference`](../tokenizer/enum.TokenReference.html).
/// Refer to the [module documentation](index.html) for more details.
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct Punctuated<'a, T> {
    #[cfg_attr(feature = "serde", serde(borrow))]
    pairs: Vec<Pair<'a, T>>,
}

impl<'a, T> Punctuated<'a, T> {
    /// Creates an empty punctuated sequence
    pub fn new() -> Self {
        Self {
            pairs: Vec::new(),
        }
    }

    /// Returns whether there's any nodes in the punctuated sequence
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns the number of pairs in the punctuated sequence
    pub fn len(&self) -> usize {
        self.pairs.len()
    }

    /// Returns an iterator over the [`Pair`](enum.Pair.html) sequences
    pub fn into_pairs(self) -> impl Iterator<Item = Pair<'a, T>> {
        self.pairs.into_iter()
    }

    /// Returns an iterator over references of the sequence values, ignoring punctuation
    pub fn iter(&self) -> Iter<'a, '_, T> {
        self.into_iter()
    }

    /// Returns an iterator over mutable references of the sequence values, ignoring punctuation
    pub fn iter_mut(&'a mut self) -> impl Iterator<Item = &'a mut T> {
        self.pairs.iter_mut().map(Pair::value_mut)
    }

    /// Returns an iterator over the [`Pair`](enum.Pair.html) sequences as references
    pub fn pairs(&self) -> impl Iterator<Item = &Pair<'a, T>> {
        self.pairs.iter()
    }

    /// Returns an iterator over the [`Pair`](enum.Pair.html) sequences as mutable references
    pub fn pairs_mut(&mut self) -> impl Iterator<Item = &mut Pair<'a, T>> {
        self.pairs.iter_mut()
    }

    /// Pops off the last [`Pair`](enum.Pair.html), if it isn't empty
    pub fn pop(&mut self) -> Option<Pair<'a, T>> {
        self.pairs.pop()
    }

    /// Pushes a new [`Pair`](enum.Pair.html) onto the sequence
    pub fn push(&mut self, pair: Pair<'a, T>) {
        self.pairs.push(pair);
    }
}

impl<'a, T> Sealed for Punctuated<'a, T> {}

impl<'a, T: Node> Node for Punctuated<'a, T> {
    fn start_position(&self) -> Option<Position> {
        self.pairs.first()?.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.pairs.last()?.end_position()
    }
}

impl<'a, T: Visit<'a>> Visit<'a> for Punctuated<'a, T> {
    fn visit<V: Visitor<'a>>(&self, visitor: &mut V) {
        self.pairs.visit(visitor);
    }
}

impl<'a, T: VisitMut<'a>> VisitMut<'a> for Punctuated<'a, T> {
    fn visit_mut<V: VisitorMut<'a>>(&mut self, visitor: &mut V) {
        self.pairs.visit_mut(visitor);
    }
}

impl<'a, T> std::iter::Extend<Pair<'a, T>> for Punctuated<'a, T> {
    fn extend<I: IntoIterator<Item = Pair<'a, T>>>(&mut self, iter: I) {
        self.pairs.extend(iter);
    }
}

impl<'a, T> IntoIterator for Punctuated<'a, T> {
    type Item = T;
    type IntoIter = IntoIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        IntoIter {
            inner: self.pairs.into_iter()
        }
    }
}

impl<'a: 'b, 'b, T> IntoIterator for &'b Punctuated<'a, T> {
    type Item = &'b T;
    type IntoIter = Iter<'a, 'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        Iter {
            inner: self.pairs.iter()
        }
    }
}

impl<'a: 'b, 'b, T> IntoIterator for &'b mut Punctuated<'a, T> {
    type Item = &'b mut T;
    type IntoIter = IterMut<'a, 'b, T>;

    fn into_iter(self) -> Self::IntoIter {
        IterMut {
            inner: self.pairs.iter_mut()
        }
    }
}

/// An iterator over owned values of type `T`.
/// Refer to the [module documentation](index.html) for more details.
pub struct IntoIter<'a, T> {
    inner: std::vec::IntoIter<Pair<'a, T>>,
}

impl<'a, T> Iterator for IntoIter<'a, T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.inner.next()?.into_value())
    }
}

/// An iterator over borrowed values of type `&T`.
/// Refer to the [module documentation](index.html) for more details.
pub struct Iter<'a, 'b, T> {
    inner: std::slice::Iter<'b, Pair<'a, T>>,
}

impl<'a, 'b, T> Iterator for Iter<'a, 'b, T> {
    type Item = &'b T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.inner.next()?.value())
    }
}

/// An iterator over borrowed values of type `&mut T`.
/// Refer to the [module documentation](index.html) for more details.
pub struct IterMut<'a, 'b, T> {
    inner: std::slice::IterMut<'b, Pair<'a, T>>,
}

impl<'a, 'b, T> Iterator for IterMut<'a, 'b, T> {
    type Item = &'b mut T;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.inner.next()?.value_mut())
    }
}

/// A node `T` followed by the possible trailing [`TokenReference`](../tokenizer/enum.TokenReference.html).
/// Refer to the [module documentation](index.html) for more details.
#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub enum Pair<'a, T> {
    /// A node `T` with no trailing punctuation
    End(T),

    /// A node `T` followed by punctuation (in the form of a [`TokenReference`](../tokenizer/enum.TokenReference.html))
    Punctuated(
        T,
        #[cfg_attr(feature = "serde", serde(borrow))] TokenReference<'a>,
    ),
}

impl<'a, T> Pair<'a, T> {
    /// Creates a `Pair` with node `T` and optional punctuation
    pub fn new(value: T, punctuation: Option<TokenReference<'a>>) -> Self {
        match punctuation {
            None => Pair::End(value),
            Some(punctuation) => Pair::Punctuated(value, punctuation),
        }
    }

    /// Takes the `Pair` and returns the node `T` and the punctuation, if it exists as a tuple
    pub fn into_tuple(self) -> (T, Option<TokenReference<'a>>) {
        match self {
            Pair::End(value) => (value, None),
            Pair::Punctuated(value, punctuation) => (value, Some(punctuation)),
        }
    }

    /// Takes the `Pair` and returns the node `T`
    pub fn into_value(self) -> T {
        self.into_tuple().0
    }

    /// Returns a reference to the node `T`
    pub fn value(&self) -> &T {
        match self {
            Pair::End(value) => value,
            Pair::Punctuated(value, _) => value,
        }
    }

    /// Returns a mutable reference to the node `T`
    pub fn value_mut(&mut self) -> &mut T {
        match self {
            Pair::End(value) => value,
            Pair::Punctuated(value, _) => value,
        }
    }

    /// Returns the trailing punctuation, if it exists
    pub fn punctuation(&self) -> Option<&TokenReference<'a>> {
        match self {
            Pair::End(_) => None,
            Pair::Punctuated(_, punctuation) => Some(punctuation),
        }
    }
}

impl<'a, T> Sealed for Pair<'a, T> {}

impl<'a, T: Node> Node for Pair<'a, T> {
    fn start_position(&self) -> Option<Position> {
        self.value().start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.punctuation().and_then(Node::end_position)
            .or_else(|| self.value().end_position())
    }
}

impl<'a, T: Visit<'a>> Visit<'a> for Pair<'a, T> {
    fn visit<V: Visitor<'a>>(&self, visitor: &mut V) {
        match self {
            Pair::End(value) => value.visit(visitor),
            Pair::Punctuated(value, punctuation) => {
                value.visit(visitor);
                punctuation.visit(visitor);
            }
        }
    }
}

impl<'a, T: VisitMut<'a>> VisitMut<'a> for Pair<'a, T> {
    fn visit_mut<V: VisitorMut<'a>>(&mut self, visitor: &mut V) {
        match self {
            Pair::End(value) => value.visit_mut(visitor),
            Pair::Punctuated(value, punctuation) => {
                value.visit_mut(visitor);
                punctuation.visit_mut(visitor);
            }
        }
    }
}
