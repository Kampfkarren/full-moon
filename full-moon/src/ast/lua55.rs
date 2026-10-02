//! Contains the nodes necessary to parse [Lua 5.5](http://www.lua.org/manual/5.5/).
//! Only usable when the "lua55" feature flag is enabled.

use crate::{
    ast::{lua54::Attribute, punctuated::Punctuated, Expression},
    tokenizer::TokenReference,
    util::{display_option, empty_optional_vector, join_iterators},
};
use derive_more::Display;
use full_moon_derive::{Node, Visit};
use std::fmt;

#[cfg(feature = "luau")]
use crate::ast::luau::TypeSpecifier;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A `global` statement, in either the named-list form (`global x, y = 1, 2`)
/// or the wildcard form (`global *`).
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[non_exhaustive]
pub enum Global {
    /// A named global declaration, such as `global x = 1` or `global x <const>, y`
    #[display("{_0}")]
    Assignment(GlobalAssignment),
    /// A collective wildcard declaration, such as `global *` or `global <const> *`
    #[display("{_0}")]
    Wildcard(GlobalWildcard),
}

/// A named global declaration, such as `global x = 1`.
///
/// Models the grammar `global [attrib] Name [attrib] {, Name [attrib]} [= explist]`,
/// where the optional leading `[attrib]` is the *prefix attribute* and applies to
/// every name in the list, while each per-name `[attrib]` applies only to that name.
// Visit/VisitMut are implemented manually in ast/visitors.rs because
// `name_list`, `attributes`, and `type_specifiers` are parallel sequences
// that need to be walked in lockstep — the same trick LocalAssignment uses.
#[derive(Clone, Debug, PartialEq, Node)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
pub struct GlobalAssignment {
    pub(crate) global_token: TokenReference,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub(crate) prefix_attribute: Option<Attribute>,
    #[cfg(feature = "luau")]
    #[cfg_attr(
        feature = "serde",
        serde(skip_serializing_if = "empty_optional_vector")
    )]
    pub(crate) type_specifiers: Vec<Option<TypeSpecifier>>,
    pub(crate) name_list: Punctuated<TokenReference>,
    #[cfg_attr(
        feature = "serde",
        serde(skip_serializing_if = "empty_optional_vector")
    )]
    pub(crate) attributes: Vec<Option<Attribute>>,
    pub(crate) equal_token: Option<TokenReference>,
    pub(crate) expr_list: Punctuated<Expression>,
}

impl GlobalAssignment {
    /// Returns a new GlobalAssignment from the given name list
    pub fn new(name_list: Punctuated<TokenReference>) -> Self {
        Self {
            global_token: TokenReference::basic_symbol("global "),
            prefix_attribute: None,
            #[cfg(feature = "luau")]
            type_specifiers: Vec::new(),
            name_list,
            attributes: Vec::new(),
            equal_token: None,
            expr_list: Punctuated::new(),
        }
    }

    /// The `global` token
    pub fn global_token(&self) -> &TokenReference {
        &self.global_token
    }

    /// The prefix attribute of the global assignment, the `<const>` part of
    /// `global <const> x, y`. A prefix attribute applies to every name in the list.
    pub fn prefix_attribute(&self) -> Option<&Attribute> {
        self.prefix_attribute.as_ref()
    }

    /// The `=` token, if one exists
    pub fn equal_token(&self) -> Option<&TokenReference> {
        self.equal_token.as_ref()
    }

    /// The punctuated sequence of expressions on the right-hand side of `=`
    pub fn expressions(&self) -> &Punctuated<Expression> {
        &self.expr_list
    }

    /// The punctuated sequence of names being declared
    pub fn names(&self) -> &Punctuated<TokenReference> {
        &self.name_list
    }

    /// The per-name postfix attributes, in the order names appear.
    /// `global x <const>, y` returns `[Some(<const>), None]`.
    pub fn attributes(&self) -> impl Iterator<Item = Option<&Attribute>> {
        self.attributes.iter().map(Option::as_ref)
    }

    /// The type specifiers of the variables, in the order they appear.
    /// Only meaningful when the "luau" feature flag is enabled and the
    /// active `LuaVersion` includes both Lua 5.5 and Luau.
    #[cfg(feature = "luau")]
    pub fn type_specifiers(&self) -> impl Iterator<Item = Option<&TypeSpecifier>> {
        self.type_specifiers.iter().map(Option::as_ref)
    }

    /// Returns a new GlobalAssignment with the given `global` token
    pub fn with_global_token(self, global_token: TokenReference) -> Self {
        Self {
            global_token,
            ..self
        }
    }

    /// Returns a new GlobalAssignment with the given prefix attribute
    pub fn with_prefix_attribute(self, prefix_attribute: Option<Attribute>) -> Self {
        Self {
            prefix_attribute,
            ..self
        }
    }

    /// Returns a new GlobalAssignment with the given type specifiers
    #[cfg(feature = "luau")]
    pub fn with_type_specifiers(self, type_specifiers: Vec<Option<TypeSpecifier>>) -> Self {
        Self {
            type_specifiers,
            ..self
        }
    }

    /// Returns a new GlobalAssignment with the given name list
    pub fn with_names(self, name_list: Punctuated<TokenReference>) -> Self {
        Self { name_list, ..self }
    }

    /// Returns a new GlobalAssignment with the given attributes
    pub fn with_attributes(self, attributes: Vec<Option<Attribute>>) -> Self {
        Self { attributes, ..self }
    }

    /// Returns a new GlobalAssignment with the given `=` token
    pub fn with_equal_token(self, equal_token: Option<TokenReference>) -> Self {
        Self {
            equal_token,
            ..self
        }
    }

    /// Returns a new GlobalAssignment with the given expression list
    pub fn with_expressions(self, expr_list: Punctuated<Expression>) -> Self {
        Self { expr_list, ..self }
    }
}

impl fmt::Display for GlobalAssignment {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        let attributes = self.attributes().chain(std::iter::repeat(None));
        #[cfg(feature = "luau")]
        let type_specifiers = self.type_specifiers().chain(std::iter::repeat(None));
        #[cfg(not(feature = "luau"))]
        let type_specifiers = std::iter::repeat_with(|| None::<TokenReference>);

        write!(
            formatter,
            "{}{}{}{}{}",
            self.global_token,
            display_option(&self.prefix_attribute),
            join_iterators(&self.name_list, attributes, type_specifiers),
            display_option(&self.equal_token),
            self.expr_list,
        )
    }
}

/// The wildcard form of `global`, such as `global *` or `global <const> *`.
///
/// The optional `[attrib]` between `global` and `*` declares all subsequently
/// undeclared free names as globals, with that attribute applied.
#[derive(Clone, Debug, Display, PartialEq, Node, Visit)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[display("{}{}{}", global_token, display_option(prefix_attribute), star_token)]
pub struct GlobalWildcard {
    pub(crate) global_token: TokenReference,
    #[cfg_attr(feature = "serde", serde(skip_serializing_if = "Option::is_none"))]
    pub(crate) prefix_attribute: Option<Attribute>,
    pub(crate) star_token: TokenReference,
}

impl GlobalWildcard {
    /// Creates a new GlobalWildcard with the given `*` token and no prefix attribute
    pub fn new(star_token: TokenReference) -> Self {
        Self {
            global_token: TokenReference::basic_symbol("global "),
            prefix_attribute: None,
            star_token,
        }
    }

    /// The `global` token
    pub fn global_token(&self) -> &TokenReference {
        &self.global_token
    }

    /// The prefix attribute, the `<const>` part of `global <const> *`
    pub fn prefix_attribute(&self) -> Option<&Attribute> {
        self.prefix_attribute.as_ref()
    }

    /// The `*` token
    pub fn star_token(&self) -> &TokenReference {
        &self.star_token
    }

    /// Returns a new GlobalWildcard with the given `global` token
    pub fn with_global_token(self, global_token: TokenReference) -> Self {
        Self {
            global_token,
            ..self
        }
    }

    /// Returns a new GlobalWildcard with the given prefix attribute
    pub fn with_prefix_attribute(self, prefix_attribute: Option<Attribute>) -> Self {
        Self {
            prefix_attribute,
            ..self
        }
    }

    /// Returns a new GlobalWildcard with the given `*` token
    pub fn with_star_token(self, star_token: TokenReference) -> Self {
        Self { star_token, ..self }
    }
}
