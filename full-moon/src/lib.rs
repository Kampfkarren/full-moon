#![warn(missing_docs)]
#![allow(clippy::large_enum_variant)]
#![cfg_attr(doc_cfg, feature(doc_auto_cfg))]
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

pub use ast::LuaVersion;
pub use short_string::ShortString;
use tokenizer::Position;

use std::{borrow::Cow, fmt};

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

impl Error {
    /// Returns a human readable error message
    pub fn error_message(&self) -> Cow<'static, str> {
        match self {
            Error::AstError(error) => error.error_message(),
            Error::TokenizerError(error) => error.to_string().into(),
        }
    }

    /// Returns the range of the error
    pub fn range(&self) -> (Position, Position) {
        match self {
            Error::AstError(error) => error.range(),
            Error::TokenizerError(error) => error.range(),
        }
    }
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

/// Creates an [`Ast`](ast::Ast) from Lua code.
/// Will use the most complete set of Lua versions enabled in your feature set.
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
pub fn parse(code: &str) -> Result<ast::Ast, Vec<Error>> {
    parse_fallible(code, LuaVersion::new()).into_result()
}

/// Given code and a pinned Lua version, will produce an [`ast::AstResult`].
/// This AstResult always produces some [`Ast`](ast::Ast), regardless of errors.
/// If a partial Ast is produced (i.e. if there are any errors), a few guarantees are lost.
/// 1. Tokens may be produced that aren't in the code itself. For example, `if x == 2 code()`
/// will produce a phantom `then` token in order to produce a usable [`If`](ast::If) struct.
/// These phantom tokens will have a null position. If you need accurate positions from the
/// phantom tokens, you can call [`Ast::update_positions`](ast::Ast::update_positions).
/// 2. The code, when printed, is not guaranteed to be valid Lua.
/// This can happen in the case of something like `local x = if`, which will produce a
/// [`LocalAssignment`](ast::LocalAssignment) that would print to `local x =`.
/// 3. There are no stability guarantees for partial Ast results, but they are consistent
/// within the same exact version of full-moon.
pub fn parse_fallible(code: &str, lua_version: LuaVersion) -> ast::AstResult {
    ast::AstResult::parse_fallible(code, lua_version)
}

/// Prints back Lua code from an [`Ast`](ast::Ast)
pub fn print(ast: &ast::Ast) -> String {
    format!("{}{}", ast.nodes(), ast.eof())
}
