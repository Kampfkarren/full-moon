use crate::{
    ast::Ast,
    private,
    tokenizer::{Position, Token, TokenReference},
};

/// Used to represent nodes such as tokens or function definitions
///
/// This trait is sealed and cannot be implemented for types outside of `full-moon`
pub trait Node: private::Sealed {
    /// The start position of a node. None if can't be determined
    fn start_position(&self) -> Option<Position>;

    /// The end position of a node. None if it can't be determined
    fn end_position(&self) -> Option<Position>;

    /// Whether another node of the same type is the same as this one semantically, ignoring position
    fn similar(&self, other: &Self) -> bool;

    /// The full range of a node, if it has both start and end positions
    fn range(&self) -> Option<(Position, Position)> {
        Some((self.start_position()?, self.end_position()?))
    }

    /// The tokens surrounding a node that are ignored and not accessible through the node's own accessors.
    /// Use this if you want to get surrounding comments or whitespace.
    /// Return value is None if a token doesn't have both a start and end position. Otherwise, it is a tuple
    /// of two token vectors, first being the preceding and the second being the following.
    fn surrounding_ignore_tokens<'ast, 'b>(
        &self,
        ast: &'b Ast<'ast>,
    ) -> Option<(Vec<&'b Token<'ast>>, Vec<&'b Token<'ast>>)> {
        let (start, end) = self.range()?;
        let (mut previous, mut following) = (Vec::new(), Vec::new());

        let mut tokens = ast.iter_tokens();

        while let Some(token) = tokens.next() {
            let this_end = token.end_position();

            if start < this_end {
                break;
            }

            if token.token_type().ignore() {
                previous.push(token);
            } else {
                previous = Vec::new();
            }
        }

        // Skip all tokens within range
        while let Some(token) = tokens.next() {
            let (this_start, this_end) = token.range()?;

            if start >= this_start || end <= this_end {
                break;
            }
        }

        for token in tokens {
            if token.token_type().ignore() {
                following.push(token);
            } else {
                break;
            }
        }

        Some((previous, following))
    }
}

impl<T: Node> Node for &T {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }
}

impl<T: Node> Node for &mut T {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }
}

impl<'a> Node for Token<'a> {
    fn start_position(&self) -> Option<Position> {
        Some(self.start_position())
    }

    fn end_position(&self) -> Option<Position> {
        Some(self.end_position())
    }

    fn similar(&self, other: &Self) -> bool {
        *self.token_type() == *other.token_type()
    }
}

impl<'a> Node for TokenReference<'a> {
    fn start_position(&self) -> Option<Position> {
        Some((**self).start_position())
    }

    fn end_position(&self) -> Option<Position> {
        Some((**self).end_position())
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }
}

impl<T: Node> Node for Option<T> {
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
}

impl<T: Node> Node for Vec<T> {
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
}

impl<A: Node, B: Node> Node for (A, B) {
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
}
