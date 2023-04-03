#![warn(missing_docs)]
#![allow(clippy::large_enum_variant)]
#![cfg_attr(doc_cfg, feature(doc_auto_cfg))]
#![allow(unused)] // rewrite todo: remove
//! # Full Moon
//!
//! `full_moon` is a lossless parser for Lua, supporting Lua 5.1, 5.2, 5.3, 5.4 and Luau
//! Learn more by going to [the repository](https://github.com/Kampfkarren/full-moon)

/// Utilities for ASTs (Abstract Syntax Trees). Contains all nodes used by Full Moon (such as blocks).
pub mod ast;

/// Contains the `Node` trait, implemented on all nodes
pub mod node;

/// Used for tokenizing, the process of converting the code to individual tokens.
/// Useful for getting symbols and manually tokenizing without going using an AST.
pub mod tokenizer;

/// Used to create visitors that recurse through [`Ast`](ast::Ast) nodes.
pub mod visitors;

mod private;
mod short_string;
mod util;

#[cfg(feature = "roblox")]
mod tokenizer_luau;

pub use short_string::ShortString;

use std::fmt;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

/// An error type that consists of both [`AstError`](ast::AstError) and [`TokenizerError`](tokenizer::TokenizerError)
/// Used by [`parse`]
#[derive(Clone, Debug, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Deserialize, serde::Serialize))]
pub enum Error {
    /// Triggered if there's an issue creating an AST, but tokenizing must have succeeded
    AstError(ast::AstError),
    /// Triggered if there's an issue when tokenizing, and an AST can't be made
    TokenizerError(tokenizer::TokenizerError),
}

impl fmt::Display for Error {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::AstError(error) => {
                write!(formatter, "error occurred while creating ast: {error}")
            }
            Error::TokenizerError(error) => {
                write!(formatter, "error occurred while tokenizing: {error}")
            }
        }
    }
}

impl std::error::Error for Error {}

/// Creates an [`Ast`](ast::Ast) from Lua code
///
/// # Errors
/// If the code passed cannot be tokenized, a TokenizerError will be returned.
/// If the code passed is not valid Lua 5.1 code, an AstError will be returned,
/// specifically AstError::UnexpectedToken.
///
/// ```rust
/// assert!(full_moon::parse("local x = 1").is_ok());
/// assert!(full_moon::parse("local x = ").is_err());
/// ```
#[allow(clippy::result_large_err)]
// rewrite todo: deprecate?
pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let result = parse_fallible(code);
    if let Some(error) = result.errors.into_iter().next() {
        Err(error)
    } else {
        Ok(result.ast)
    }
}

pub fn parse_fallible(code: &str) -> ast::AstResult {
    todo!()
}

/// Prints back Lua code from an [`Ast`](ast::Ast)
pub fn print(ast: &ast::Ast) -> String {
    format!("{}{}", ast.nodes(), ast.eof())
}
