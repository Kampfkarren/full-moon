//! Contains the nodes necessary to parse [Lua 5.4](http://www.lua.org/manual/5.4/).
//! Only usable when the "lua54" feature flag is enabled.

use crate::tokenizer::TokenReference;
use derive_more::Display;
use full_moon_derive::{Node, Visit};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// An attribute on a local variable, `<const>` in `local x <const>`
#[derive(Clone, Debug, Display, PartialEq, Eq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "left_angle_bracket", "name", "right_angle_bracket")]
pub struct Attribute {
    pub(crate) left_angle_bracket: TokenReference,
    pub(crate) name: TokenReference,
    pub(crate) right_angle_bracket: TokenReference,
}

impl Attribute {
    /// Creates a new Label with the given name
    pub fn new(name: TokenReference) -> Self {
        Self {
            left_angle_bracket: TokenReference::symbol("<").unwrap(),
            name,
            right_angle_bracket: TokenReference::symbol(">").unwrap(),
        }
    }

    /// The `<` symbol on the left hand side of the attribute
    pub fn left_angle_bracket(&self) -> &TokenReference {
        &self.left_angle_bracket
    }

    /// The name used for the attribute, the `const` part of `<const>`
    pub fn name(&self) -> &TokenReference {
        &self.name
    }

    /// The `>` symbol on the right hand side of the attribute
    pub fn right_angle_bracket(&self) -> &TokenReference {
        &self.right_angle_bracket
    }

    /// Returns a new Attribute with the given `<` symbol on the left hand side
    pub fn with_left_angle_bracket(self, left_angle_bracket: TokenReference) -> Self {
        Self {
            left_angle_bracket,
            ..self
        }
    }

    /// Returns a new Attribute with the given attribute name
    pub fn with_name(self, name: TokenReference) -> Self {
        Self { name, ..self }
    }

    /// Returns a new Attribute with the given `>` symbol on the right hand side
    pub fn with_right_angle_bracket(self, right_angle_bracket: TokenReference) -> Self {
        Self {
            right_angle_bracket,
            ..self
        }
    }
}
