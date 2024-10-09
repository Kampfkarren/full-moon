//! Contains the nodes necessary to parse [Lua 5.4](http://www.lua.org/manual/5.4/).
//! Only usable when the "lua54" feature flag is enabled.

use crate::{ast::ContainedSpan, tokenizer::TokenReference};
use derive_more::Display;
use full_moon_derive::{Node, Visit};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An attribute on a local variable, `<const>` in `local x <const>`
#[derive(Clone, Debug, Display, PartialEq, Eq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{}{}{}", brackets.tokens().0, name, brackets.tokens().1)]
pub struct Attribute {
    #[node(full_range)]
    #[visit(contains = "name")]
    pub(crate) brackets: ContainedSpan,
    pub(crate) name: TokenReference,
}

impl Attribute {
    /// Creates a new Label with the given name
    pub fn new(name: TokenReference) -> Self {
        Self {
            brackets: ContainedSpan::new(
                TokenReference::symbol("<").unwrap(),
                TokenReference::symbol(">").unwrap(),
            ),
            name,
        }
    }

    /// The name used for the attribute, the `const` part of `<const>`
    pub fn name(&self) -> &TokenReference {
        &self.name
    }

    /// The angle brackets (`<` and `>`) surrounding the attribute
    pub fn brackets(&self) -> &ContainedSpan {
        &self.brackets
    }

    /// Returns a new Attribute with the given attribute name
    pub fn with_name(self, name: TokenReference) -> Self {
        Self { name, ..self }
    }

    /// Returns a new Attribute with the given angle brackets
    pub fn with_brackets(self, brackets: ContainedSpan) -> Self {
        Self { brackets, ..self }
    }
}
