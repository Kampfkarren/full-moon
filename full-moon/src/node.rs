use crate::{
    ast::{span::ContainedSpan, Ast},
    private,
    tokenizer::{Position, Token, TokenReference},
};
use std::fmt;

/// Used to represent nodes such as tokens or function definitions
///
/// This trait is sealed and cannot be implemented for types outside of `full-moon`
pub trait Node: private::Sealed {
    /// The start position of a node. None if can't be determined
    fn start_position(&self) -> Option<Position>;

    /// The end position of a node. None if it can't be determined
    fn end_position(&self) -> Option<Position>;

    /// Whether another node of the same type is the same as this one semantically, ignoring position
    fn similar(&self, other: &Self) -> bool
    where
        Self: Sized;

    /// The token references that comprise a node, in source order.
    ///
    /// A node's tokens reproduce its [`Display`](fmt::Display) output, so joining them
    /// gives back the source.
    fn tokens(&self) -> Tokens<'_>;

    /// The full range of a node, if it has both start and end positions
    fn range(&self) -> Option<(Position, Position)> {
        Some((self.start_position()?, self.end_position()?))
    }

    /// The tokens surrounding a node that are ignored and not accessible through the node's own accessors.
    /// Use this if you want to get surrounding comments or whitespace.
    /// Returns a tuple of the leading and trailing trivia.
    fn surrounding_trivia(&self) -> (Vec<&Token>, Vec<&Token>) {
        let mut tokens = self.tokens();
        let leading = tokens.next();
        let trailing = tokens.next_back();

        (
            match leading {
                Some(token) => token.leading_trivia().collect(),
                None => Vec::new(),
            },
            match trailing {
                Some(token) => token.trailing_trivia().collect(),
                None => Vec::new(),
            },
        )
    }
}

pub(crate) enum TokenItem<'a> {
    MoreTokens(&'a dyn Node),
    TokenReference(&'a TokenReference),
}

impl fmt::Debug for TokenItem<'_> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenItem::MoreTokens(_) => write!(formatter, "TokenItem::MoreTokens"),
            TokenItem::TokenReference(token) => {
                write!(formatter, "TokenItem::TokenReference({token})")
            }
        }
    }
}

/// An iterator that iterates over the tokens of a node
/// Returned by [`Node::tokens`]
#[derive(Default)]
pub struct Tokens<'a> {
    pub(crate) items: Vec<TokenItem<'a>>,
}

impl<'a> Tokens<'a> {
    /// This iterator with `token` appended after everything queued.
    pub(crate) fn with_token(mut self, token: &'a TokenReference) -> Self {
        self.items.push(TokenItem::TokenReference(token));

        self
    }
}

impl<'a> Iterator for Tokens<'a> {
    type Item = &'a TokenReference;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            if self.items.is_empty() {
                return None;
            }

            match self.items.remove(0) {
                TokenItem::TokenReference(reference) => return Some(reference),
                TokenItem::MoreTokens(node) => {
                    let mut tokens = node.tokens();
                    tokens.items.append(&mut self.items);
                    self.items = tokens.items;
                }
            }
        }
    }
}

impl DoubleEndedIterator for Tokens<'_> {
    fn next_back(&mut self) -> Option<Self::Item> {
        loop {
            if self.items.is_empty() {
                return None;
            }

            match self.items.pop()? {
                TokenItem::TokenReference(reference) => return Some(reference),
                TokenItem::MoreTokens(node) => {
                    let mut tokens = node.tokens();
                    self.items.append(&mut tokens.items);
                }
            }
        }
    }
}

impl Node for Ast {
    fn start_position(&self) -> Option<Position> {
        self.nodes().start_position()
    }

    fn end_position(&self) -> Option<Position> {
        self.nodes().end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        self.nodes().similar(other.nodes())
    }

    fn tokens(&self) -> Tokens<'_> {
        self.nodes().tokens()
    }
}

impl<T: Node> Node for Box<T> {
    fn start_position(&self) -> Option<Position> {
        (**self).start_position()
    }

    fn end_position(&self) -> Option<Position> {
        (**self).end_position()
    }

    fn similar(&self, other: &Self) -> bool {
        (**self).similar(other)
    }

    fn tokens(&self) -> Tokens<'_> {
        (**self).tokens()
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

    fn tokens(&self) -> Tokens<'_> {
        (**self).tokens()
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

    fn tokens(&self) -> Tokens<'_> {
        (**self).tokens()
    }
}

impl Node for TokenReference {
    fn start_position(&self) -> Option<Position> {
        Some((**self).start_position())
    }

    fn end_position(&self) -> Option<Position> {
        Some((**self).end_position())
    }

    fn similar(&self, other: &Self) -> bool {
        *self.token_type() == *other.token_type()
    }

    fn tokens(&self) -> Tokens<'_> {
        Tokens {
            items: vec![TokenItem::TokenReference(self)],
        }
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

    fn tokens(&self) -> Tokens<'_> {
        match self {
            Some(node) => node.tokens(),
            None => Tokens::default(),
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

    fn tokens(&self) -> Tokens<'_> {
        Tokens {
            items: self.iter().flat_map(|node| node.tokens().items).collect(),
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

    fn tokens(&self) -> Tokens<'_> {
        let mut items = self.0.tokens().items;
        items.append(&mut self.1.tokens().items);

        Tokens { items }
    }
}

/// Builds the [`TokenItem`] list for a derived `Node::tokens`.
/// It lives here, not in the derive, which would repeat it once per node.
#[derive(Default)]
pub(crate) struct TokenBuilder<'a> {
    items: Vec<TokenItem<'a>>,
    /// Closes still owed, LIFO so spans close inside-out. Leftovers go out at the end.
    pending_closes: Vec<&'a TokenReference>,
}

impl<'a> TokenBuilder<'a> {
    /// Emits `node` whole, leaving it to yield its own tokens.
    pub(crate) fn push(&mut self, node: &'a dyn Node) {
        self.items.push(TokenItem::MoreTokens(node));
    }

    pub(crate) fn push_token(&mut self, token: &'a TokenReference) {
        self.items.push(TokenItem::TokenReference(token));
    }

    /// Emits the opening delimiter of `span`, queueing the closing one for the end.
    pub(crate) fn push_open(&mut self, span: &'a ContainedSpan) {
        let (open, close) = span.tokens();

        self.push_token(open);
        self.pending_closes.push(close);
    }

    /// Emits the closing delimiter of the most recently opened span, if one is open.
    ///
    /// The derive rejects a `#[node(close)]` with no span before it,
    /// so this never runs empty from one. Doing nothing beats panicking in a user's binary.
    pub(crate) fn close(&mut self) {
        if let Some(close) = self.pending_closes.pop() {
            self.push_token(close);
        }
    }

    /// Emits each `extras` element after its value and before its comma,
    /// as in `local a: string, b, c: number`.
    #[cfg(any(feature = "luau", feature = "lua54"))]
    pub(crate) fn push_punctuated_with<T: Node, U: Node>(
        &mut self,
        punctuated: &'a crate::ast::punctuated::Punctuated<T>,
        extras: &'a [Option<U>],
    ) {
        self.push_punctuated_with_two::<T, U, U>(punctuated, extras, &[]);
    }

    /// The same with two parallel `Vec`s, as `local a <const>, b: string` needs.
    #[cfg(any(feature = "luau", feature = "lua54"))]
    pub(crate) fn push_punctuated_with_two<T: Node, U: Node, V: Node>(
        &mut self,
        punctuated: &'a crate::ast::punctuated::Punctuated<T>,
        first: &'a [Option<U>],
        second: &'a [Option<V>],
    ) {
        for (index, pair) in punctuated.pairs().enumerate() {
            self.push(pair.value());

            if let Some(extra) = first.get(index) {
                self.push(extra);
            }

            if let Some(extra) = second.get(index) {
                self.push(extra);
            }

            if let Some(punctuation) = pair.punctuation() {
                self.push_token(punctuation);
            }
        }
    }

    pub(crate) fn build(mut self) -> Tokens<'a> {
        while let Some(close) = self.pending_closes.pop() {
            self.push_token(close);
        }

        Tokens { items: self.items }
    }
}

#[cfg(test)]
mod tests {
    use super::Node;
    use crate::{
        ast::span::ContainedSpan,
        tokenizer::{Token, TokenReference, TokenType},
    };
    use full_moon_derive::Node;

    fn identifier(name: &str) -> TokenReference {
        TokenReference::new(
            vec![],
            Token::new(TokenType::Identifier {
                identifier: name.into(),
            }),
            vec![],
        )
    }

    /// `middle` carries `after`, yet owns `close` and a follower.
    /// Declared in the order `brackets, middle, tail, deep`, it still emits `(TM)D`.
    #[derive(Node)]
    struct Skipped {
        #[allow(dead_code)]
        brackets: ContainedSpan,
        #[allow(dead_code)]
        #[node(close, after = "tail")]
        middle: TokenReference,
        #[allow(dead_code)]
        tail: TokenReference,
        #[allow(dead_code)]
        #[node(after = "middle")]
        deep: TokenReference,
    }

    /// Used to lose the close, and never emit the follower at all.
    #[test]
    fn close_and_followers_survive_a_skipped_field() {
        let node = Skipped {
            brackets: ContainedSpan::new(
                TokenReference::basic_symbol("("),
                TokenReference::basic_symbol(")"),
            ),
            middle: identifier("M"),
            tail: identifier("T"),
            deep: identifier("D"),
        };

        let tokens: String = node.tokens().map(|token| token.to_string()).collect();
        assert_eq!(tokens, "(TM)D");
    }

    /// A named variant's fields become locals, so `builder` would shadow the builder.
    #[derive(Node)]
    enum Shadowed {
        Named {
            #[allow(dead_code)]
            builder: TokenReference,
            #[allow(dead_code)]
            tail: TokenReference,
        },
    }

    #[test]
    fn a_field_named_builder_does_not_shadow_the_builder() {
        let node = Shadowed::Named {
            builder: identifier("A"),
            tail: identifier("B"),
        };

        let tokens: String = node.tokens().map(|token| token.to_string()).collect();
        assert_eq!(tokens, "AB");
    }

    /// A repeated name is one field; counting it twice reports a false cycle.
    #[cfg(any(feature = "luau", feature = "lua54"))]
    #[derive(Node)]
    struct Repeated {
        #[allow(dead_code)]
        #[node(interleave = "specs, specs")]
        names: crate::ast::punctuated::Punctuated<TokenReference>,
        #[allow(dead_code)]
        specs: Vec<Option<TokenReference>>,
    }

    #[test]
    #[cfg(any(feature = "luau", feature = "lua54"))]
    fn a_repeated_interleave_name_is_not_a_cycle() {
        let mut names = crate::ast::punctuated::Punctuated::new();
        names.push(crate::ast::punctuated::Pair::End(identifier("a")));

        let node = Repeated {
            names,
            specs: vec![Some(identifier("T"))],
        };

        let tokens: String = node.tokens().map(|token| token.to_string()).collect();
        assert_eq!(tokens, "aT");
    }
}
