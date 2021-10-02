#![warn(missing_docs)]
#![allow(clippy::large_enum_variant)]
//! # Full Moon
//!
//! `full_moon` is a lossless parser for Lua 5.1
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

pub use short_string::ShortString;

use std::fmt;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

/// An error type that consists of both [`AstError`](ast::AstError) and [`TokenizerError`](tokenizer::TokenizerError)
/// Used by [`parse`]
#[derive(Clone, Debug, PartialEq)]
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
                write!(formatter, "error occurred while creating ast: {}", error)
            }
            Error::TokenizerError(error) => {
                write!(formatter, "error occurred while tokenizing: {}", error)
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
pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let code = skip_utf8_BOM(code);
    let tokens = tokenizer::tokens(code).map_err(Error::TokenizerError)?;
    ast::Ast::from_tokens(tokens).map_err(Error::AstError)
}

/// Prints back Lua code from an [`Ast`](ast::Ast)
pub fn print(ast: &ast::Ast) -> String {
    format!("{}{}", ast.nodes(), ast.eof())
}

/// Clean UTF8 BOM(byte order mark https://en.wikipedia.org/wiki/Byte_order_mark).
///
/// IMPORTANT: It doesn't handle UTF16 BOM see wiki.
///
/// Original LUA interpretator does this as well.
/// https://github.com/lua/lua/blob/439e45a2f69549b674d6a6e2023e8debfa00a2b8/lauxlib.c#L742-L753
#[allow(non_snake_case)]
fn skip_utf8_BOM(code: &str) -> &str {
  const BOM: &[u8; 3] = b"\xEF\xBB\xBF";
  if code.as_bytes().starts_with(BOM) {
      &code[3..]
  } else {
      code
  }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[allow(non_snake_case)]
    fn test_parse_code_with_utf8_BOM() {
        assert!(parse(str_with_BOM("local x = 1").as_str()).is_ok());
        assert!(parse(str_with_BOM("\nlocal x = 1").as_str()).is_ok());
    }

    #[test]
    #[allow(non_snake_case)]
    fn test_skip_utf8_BOM() {
        assert_eq!(skip_utf8_BOM(str_with_BOM("123").as_str()), "123");
        assert_eq!(skip_utf8_BOM(str_with_BOM("").as_str()), "");
    }

    #[allow(non_snake_case)]
    fn str_with_BOM(s: &str) -> String {
        format!("{}{}", String::from_utf8(b"\xEF\xBB\xBF".to_vec()).unwrap(), s)
    }
}