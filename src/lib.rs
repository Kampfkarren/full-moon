pub mod ast;
pub mod tokenizer;

#[cfg(feature = "visitors")]
pub mod visitors;
// #[cfg(feature = "searching")]
// pub use searching::*;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

#[derive(Clone, Debug, PartialEq)]
pub enum Error<'a> {
    AstError(ast::AstError<'a>),
    TokenizerError(tokenizer::TokenizerError),
}

pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let tokens = tokenizer::tokens(code).map_err(Error::TokenizerError)?;
    ast::Ast::from_tokens(tokens).map_err(Error::AstError)
}

pub fn print<'a>(ast: &'a ast::Ast<'a>) -> String {
    ast.tokens
        .iter()
        .fold(String::new(), |acc, token| acc + &token.to_string())
}
