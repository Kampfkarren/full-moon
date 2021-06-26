//! Contains the nodes necessary to parse [Lua 5.2](http://www.lua.org/manual/5.2/).
//! Only usable when the "lua52" feature flag is enabled.

use crate::tokenizer::TokenReference;
use derive_more::Display;
use full_moon_derive::{Node, Visit};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A goto statement, such as `goto label`.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}", "goto_token", "label_name")]
pub struct Goto {
    pub(crate) goto_token: TokenReference,
    pub(crate) label_name: TokenReference,
}

impl Goto {
    /// Creates a new Goto with the given label name
    pub fn new(label_name: TokenReference) -> Self {
        Self {
            goto_token: TokenReference::symbol("goto").unwrap(),
            label_name,
        }
    }

    /// The `goto` symbol in `goto label`
    pub fn goto_token(&self) -> &TokenReference {
        &self.goto_token
    }

    /// The name corresponding to the label, the `label` part of `goto label`
    pub fn label_name(&self) -> &TokenReference {
        &self.label_name
    }

    /// Returns a new Goto with the given `goto` symbol
    pub fn with_goto_token(self, goto_token: TokenReference) -> Self {
        Self { goto_token, ..self }
    }

    /// Returns a new Goto with the given label name
    pub fn with_label_name(self, label_name: TokenReference) -> Self {
        Self { label_name, ..self }
    }
}

/// A label, such as `::label::`.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display(fmt = "{}{}{}", "left_colons", "name", "right_colons")]
pub struct Label {
    pub(crate) left_colons: TokenReference,
    pub(crate) name: TokenReference,
    pub(crate) right_colons: TokenReference,
}

impl Label {
    /// Creates a new Label with the given name
    pub fn new(name: TokenReference) -> Self {
        Self {
            left_colons: TokenReference::symbol("::").unwrap(),
            name,
            right_colons: TokenReference::symbol("::").unwrap(),
        }
    }

    /// The `::` symbol on the left hand side of the name
    pub fn left_colons(&self) -> &TokenReference {
        &self.left_colons
    }

    /// The name used for the label, the `label` part of `::label::`
    pub fn name(&self) -> &TokenReference {
        &self.name
    }

    /// The `::` symbol on the right hand side of the name
    pub fn right_colons(&self) -> &TokenReference {
        &self.right_colons
    }

    /// Returns a new Label with the given `::` symbol on the left hand side
    pub fn with_left_colons(self, left_colons: TokenReference) -> Self {
        Self {
            left_colons,
            ..self
        }
    }

    /// Returns a new Label with the given label name
    pub fn with_name(self, name: TokenReference) -> Self {
        Self { name, ..self }
    }

    /// Returns a new Label with the given `::` symbol on the right hand side
    pub fn with_right_colons(self, right_colons: TokenReference) -> Self {
        Self {
            right_colons,
            ..self
        }
    }
}
