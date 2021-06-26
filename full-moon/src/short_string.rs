use std::{fmt::Display, ops::Deref};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use smol_str::SmolStr;

/// A string as used in `TokenType`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
#[cfg_attr(feature = "serde", derive(Deserialize, Serialize))]
#[cfg_attr(feature = "serde", serde(transparent))]
pub struct ShortString(SmolStr);

impl ShortString {
    /// Creates a new ShortString from the given text.
    pub fn new<T: Into<String> + AsRef<str>>(text: T) -> Self {
        ShortString(SmolStr::from(text))
    }

    /// Returns a `&str` representation of the ShortString.
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    /// Returns the length of the ShortString.
    pub fn len(&self) -> usize {
        self.0.len()
    }

    /// Returns whether or not the ShortString is empty.
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

impl Display for ShortString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for ShortString {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T: Into<String> + AsRef<str>> From<T> for ShortString {
    fn from(value: T) -> Self {
        ShortString(SmolStr::from(value))
    }
}
