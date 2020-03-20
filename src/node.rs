use crate::{
    ast::Ast,
    private,
    tokenizer::{Position, Token, TokenReference},
};
use std::{borrow::Cow, fmt};

/// Used to represent nodes such as tokens or function definitions
///
/// This trait is sealed and cannot be implemented for types outside of `full-moon`
pub trait Node<'ast>: private::Sealed {
    /// The start position of a node. None if can't be determined
    fn start_position(&self) -> Option<Position>;

    /// The end position of a node. None if it can't be determined
    fn end_position(&self) -> Option<Position>;

    /// Whether another node of the same type is the same as this one semantically, ignoring position
    fn similar(&self, other: &Self) -> bool
    where
        Self: Sized;

    /// The token references that comprise a node
    fn tokens<'b>(&'b self) -> Tokens<'ast, 'b>;

    /// The full range of a node, if it has both start and end positions
    fn range(&self) -> Option<(Position, Position)> {
        Some((self.start_position()?, self.end_position()?))
    }

    /// The tokens surrounding a node that are ignored and not accessible through the node's own accessors.
    /// Use this if you want to get surrounding comments or whitespace.
    /// Returns a tuple of the leading and trailing trivia.
    fn surrounding_trivia<'b>(&'b self) -> (Vec<Cow<'b, Token<'ast>>>, Vec<Cow<'b, Token<'ast>>>) {
        let mut tokens = self.tokens();
        let leading = tokens.next();
        let trailing = tokens.next_back();

        (
            match leading {
                Some(Cow::Owned(token)) => {
                    token.leading_trivia.into_iter().map(Cow::Owned).collect()
                }
                Some(Cow::Borrowed(token)) => token.leading_trivia().map(Cow::Borrowed).collect(),
                None => Vec::new(),
            },
            match trailing {
                Some(Cow::Owned(token)) => {
                    token.trailing_trivia.into_iter().map(Cow::Owned).collect()
                }
                Some(Cow::Borrowed(token)) => token.trailing_trivia().map(Cow::Borrowed).collect(),
                None => Vec::new(),
            },
        )
    }
}

pub(crate) enum TokenItem<'ast, 'b> {
    MoreTokens(&'b dyn Node<'ast>),
    TokenReference(Cow<'b, TokenReference<'ast>>),
}

impl fmt::Debug for TokenItem<'_, '_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenItem::MoreTokens(_) => write!(f, "TokenItem::MoreTokens"),
            TokenItem::TokenReference(token) => write!(f, "TokenItem::TokenReference({})", token),
        }
    }
}

#[derive(Default)]
pub struct Tokens<'ast, 'b> {
    pub(crate) items: Vec<TokenItem<'ast, 'b>>,
}

impl<'ast, 'b> Iterator for Tokens<'ast, 'b> {
    type Item = Cow<'b, TokenReference<'ast>>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.items.is_empty() {
            return None;
        }

        match self.items.remove(0) {
            TokenItem::TokenReference(reference) => Some(reference),
            TokenItem::MoreTokens(node) => {
                let mut tokens = node.tokens();
                tokens.items.extend(self.items.drain(..));
                self.items = tokens.items;
                self.next()
            }
        }
    }
}

impl<'ast, 'b> DoubleEndedIterator for Tokens<'ast, 'b> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.items.is_empty() {
            return None;
        }

        match self.items.pop()? {
            TokenItem::TokenReference(reference) => Some(reference),
            TokenItem::MoreTokens(node) => {
                let mut tokens = node.tokens();
                self.items.extend(tokens.items.drain(..));
                self.next_back()
            }
        }
    }
}

impl<'a> Node<'a> for Ast<'a> {
    fn start_position(&self) -> Option<Position> {
        self.nodes().start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.nodes().end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        self.nodes().similar(other.nodes())
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        self.nodes().tokens()
    }
}

impl<'a, T: Node<'a>> Node<'a> for Box<T> {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        (**self).tokens()
    }
}

impl<'a, T: Node<'a>> Node<'a> for &T {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        (**self).tokens()
    }
}

impl<'a, T: Node<'a>> Node<'a> for &mut T {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        (**self).tokens()
    }
}

impl<'a, T: Node<'a> + ToOwned> Node<'a> for Cow<'_, T> {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        (**self).tokens()
    }
}

impl<'a> Node<'a> for Token<'a> {
    fn start_position(&self) -> Option<Position> {
        Some(self.start_position())
    }

    fn end_position(&self) -> Option<Position> {
        Some(self.end_position())
    }

    fn similar(&self, other: &Self) -> bool {
        *self.token_type() == *other.token_type()
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        Tokens {
            items: vec![TokenItem::TokenReference(Cow::Owned(TokenReference::new(
                Vec::new(),
                self.to_owned(),
                Vec::new(),
            )))],
        }
    }
}

impl<'a> Node<'a> for TokenReference<'a> {
    fn start_position(&self) -> Option<Position> {
        Some((**self).start_position())
    }

    fn end_position(&self) -> Option<Position> {
        Some((**self).end_position())
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        Tokens {
            items: vec![TokenItem::TokenReference(Cow::Borrowed(self))],
        }
    }
}

impl<'a, T: Node<'a>> Node<'a> for Option<T> {
    fn start_position(&self) -> Option<Position> {
        self.as_ref().and_then(Node::start_position)
    }

    fn end_position(&self) -> Option<Position> {
        self.as_ref().and_then(Node::end_position)
    }

    fn similar(&self, other: &Self) -> bool {
        match (self.as_ref(), other.as_ref()) {
            (Some(x), Some(y)) => x.similar(y),
            (None, None) => true,
            _ => false,
        }
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        match self {
            Some(node) => node.tokens(),
            None => Tokens::default(),
        }
    }
}

impl<'a, T: Node<'a>> Node<'a> for Vec<T> {
    fn start_position(&self) -> Option<Position> {
        self.first()?.start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.last()?.end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        if self.len() == other.len() {
            self.iter().zip(other.iter()).all(|(x, y)| x.similar(y))
        } else {
            false
        }
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        Tokens {
            items: self
                .iter()
                .map(|node| node.tokens().items)
                .flatten()
                .collect(),
        }
    }
}

impl<'a, A: Node<'a>, B: Node<'a>> Node<'a> for (A, B) {
    fn start_position(&self) -> Option<Position> {
        match (self.0.start_position(), self.1.start_position()) {
            (Some(x), Some(y)) => Some(std::cmp::min(x, y)),
            (Some(x), None) => Some(x),
            (None, Some(y)) => Some(y),
            (None, None) => None,
        }
    }

    fn end_position(&self) -> Option<Position> {
        match (self.0.end_position(), self.1.end_position()) {
            (Some(x), Some(y)) => Some(std::cmp::max(x, y)),
            (Some(x), None) => Some(x),
            (None, Some(y)) => Some(y),
            (None, None) => None,
        }
    }

    fn similar(&self, other: &Self) -> bool {
        self.0.similar(&other.0) && self.1.similar(&other.1)
    }

    fn tokens<'b>(&'b self) -> Tokens<'a, 'b> {
        let mut items = self.0.tokens().items;
        items.extend(self.1.tokens().items.drain(..));

        Tokens { items }
    }
}
