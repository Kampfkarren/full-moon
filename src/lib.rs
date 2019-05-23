pub mod ast;
pub mod tokenizer;
pub mod visitors;

use std::fmt;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

#[derive(Clone, Debug, PartialEq)]
pub enum Error<'a> {
    AstError(ast::AstError<'a>),
    TokenizerError(tokenizer::TokenizerError),
}

impl<'a> fmt::Display for Error<'a> {
    fn fmt(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Error::AstError(error) => write!(formatter, "error occurred while creating ast: {}", error),
            Error::TokenizerError(error) => write!(formatter, "error occurred while tokenizing: {}", error),
        }
    }
}

impl<'a> std::error::Error for Error<'a> {}

/// Creates an [Ast](ast/struct.Ast.html) from Lua code
pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let tokens = tokenizer::tokens(code).map_err(Error::TokenizerError)?;
    ast::Ast::from_tokens(tokens).map_err(Error::AstError)
}

/// Prints back Lua code from an [Ast](ast/struct.Ast.html)
pub fn print<'a>(ast: &'a ast::Ast<'a>) -> String {
    ast.iter_tokens()
        .fold(String::new(), |acc, token| acc + &token.to_string())
}
