//! Exposes the [`Owned`](trait.Owned.html) that nodes implement to produce an owned version of themselves.
//! Owned versions are represented as the node with a lifetime of `'static`. For example, if you have
//! an [`Ast<'a>`](../struct.Ast.html), calling `ast.owned()` on it will produce an owned `Ast<'static>`.
use super::*;
use crate::tokenizer::*;

use atomic_refcell::AtomicRefCell;
use std::borrow::Cow;

/// A trait for getting an owned version of a node.
/// Refer to the [module documentation](index.html) for more details.
/// This trait is sealed and cannot be implemented for types outside of `full-moon`
pub trait Owned
where
    Self: crate::private::Sealed,
{
    /// What an owned version of the object looks like. Usually contains a `'static` lifetime.
    type Owned;
    /// Returns an owned version of the object.
    fn owned(&self) -> Self::Owned;
}

impl Owned for Ast<'_> {
    type Owned = Ast<'static>;

    fn owned(&self) -> Self::Owned {
        Ast {
            nodes: self.nodes.owned(),
            tokens: Arc::new(self.tokens.iter().map(|(_, token)| token.owned()).collect()),
        }
    }
}

impl<T> Owned for Pair<'_, T>
where
    T: Owned,
{
    type Owned = Pair<'static, <T as Owned>::Owned>;

    fn owned(&self) -> Self::Owned {
        match self {
            Pair::End(token) => Pair::End(token.owned()),
            Pair::Punctuated(token, sep) => Pair::Punctuated(token.owned(), sep.owned()),
        }
    }
}

impl<T> Owned for Punctuated<'_, T>
where
    T: Owned,
{
    type Owned = Punctuated<'static, <T as Owned>::Owned>;

    fn owned(&self) -> Self::Owned {
        let mut owned = Punctuated::new();
        owned.extend(&mut self.pairs().map(Pair::owned));
        owned
    }
}

impl Owned for Token<'_> {
    type Owned = Token<'static>;

    fn owned(&self) -> Self::Owned {
        Token {
            start_position: self.start_position.clone(),
            end_position: self.end_position.clone(),
            token_type: Arc::new(AtomicRefCell::new(self.token_type().owned())),
        }
    }
}

impl Owned for TokenReference<'_> {
    type Owned = TokenReference<'static>;

    fn owned(&self) -> TokenReference<'static> {
        TokenReference::Owned((**self).owned())
    }
}

impl Owned for TokenType<'_> {
    type Owned = TokenType<'static>;

    fn owned(&self) -> Self::Owned {
        match self {
            TokenType::Eof => TokenType::Eof,
            TokenType::Identifier { identifier } => TokenType::Identifier {
                identifier: Cow::Owned(identifier.clone().into_owned()),
            },
            TokenType::MultiLineComment { blocks, comment } => TokenType::MultiLineComment {
                blocks: *blocks,
                comment: Cow::Owned(comment.clone().into_owned()),
            },
            TokenType::Number { text } => TokenType::Number {
                text: Cow::Owned(text.clone().into_owned()),
            },
            TokenType::SingleLineComment { comment } => TokenType::SingleLineComment {
                comment: Cow::Owned(comment.clone().into_owned()),
            },
            TokenType::StringLiteral {
                literal,
                multi_line,
                quote_type,
            } => TokenType::StringLiteral {
                literal: Cow::Owned(literal.clone().into_owned()),
                multi_line: multi_line.clone(),
                quote_type: *quote_type,
            },
            TokenType::Symbol { symbol } => TokenType::Symbol { symbol: *symbol },
            TokenType::Whitespace { characters } => TokenType::Whitespace {
                characters: Cow::Owned(characters.clone().into_owned()),
            },
        }
    }
}

impl<T> Owned for Box<T>
where
    T: Owned,
{
    type Owned = Box<<T as Owned>::Owned>;

    fn owned(&self) -> Self::Owned {
        Box::new((**self).owned())
    }
}

impl<T> Owned for Option<T>
where
    T: Owned,
{
    type Owned = Option<<T as Owned>::Owned>;

    fn owned(&self) -> Self::Owned {
        Some(self.as_ref()?.owned())
    }
}

impl<T> Owned for Vec<T>
where
    T: Owned,
{
    type Owned = Vec<<T as Owned>::Owned>;

    fn owned(&self) -> Self::Owned {
        self.iter().map(Owned::owned).collect()
    }
}

impl<A, B> Owned for (A, B)
where
    A: Owned,
    B: Owned,
{
    type Owned = (<A as Owned>::Owned, <B as Owned>::Owned);

    fn owned(&self) -> Self::Owned {
        (self.0.owned(), self.1.owned())
    }
}
