#![deny(missing_docs)]

//! # Full Moon
//!
//! `full_moon` is a lossless parser for Lua 5.1
//! Learn more by going to [the repository](https://github.com/Kampfkarren/full-moon)

/// Utilities for ASTs (Abstract Syntax Trees). Contains all nodes used by Full Moon (such as blocks).
pub mod ast;

/// Used for tokenizing, the process of converting the code to individual tokens.
/// Useful for getting symbols and manually tokenizing without going using an AST.
pub mod tokenizer;

/// Used to create visitors that recurse through [`Ast`](ast/struct.Ast.html) nodes.
pub mod visitors;

use std::fmt;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

/// An error type that consists of both [`AstError`](ast/enum.AstError.html) and [`TokenizerError`](tokenizer/enum.TokenizerError.html)
/// Used by [`parse`](fn.parse)
#[derive(Clone, Debug, PartialEq)]
pub enum Error<'a> {
    /// Triggered if there's an issue creating an AST, but tokenizing must have succeeded
    AstError(ast::AstError<'a>),
    /// Triggered if there's an issue when tokenizing, and an AST can't be made
    TokenizerError(tokenizer::TokenizerError),
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::AstError(error) => {
                write!(formatter, "error occurred while creating ast: {}", error)
            }
            Error::TokenizerError(error) => {
                write!(formatter, "error occurred while tokenizing: {}", error)
            }
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

/// Creates an [`Ast`](ast/struct.Ast.html) from Lua code
///
/// # Errors
/// If the code passed cannot be tokenized, a TokenizerError will be returned.
/// If the code passed is not valid Lua 5.1 code, an AstError will be returned,
/// specifically AstError::UnexpectedToken.
///
/// ```rust
/// assert!(full_moon::parse("local x = 1").is_ok());
/// ```
pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let tokens = tokenizer::tokens(code).map_err(Error::TokenizerError)?;
    ast::Ast::from_tokens(tokens).map_err(Error::AstError)
}

/// Prints back Lua code from an [Ast](ast/struct.Ast.html)
pub fn print<'a>(ast: &'a ast::Ast<'a>) -> String {
    ast.iter_tokens()
        .fold(String::new(), |acc, token| acc + &token.to_string())
}
