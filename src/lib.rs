pub mod ast;
pub mod tokenizer;
pub mod visitors;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

#[derive(Clone, Debug, PartialEq)]
pub enum Error<'a> {
    AstError(ast::AstError<'a>),
    TokenizerError(tokenizer::TokenizerError),
}

/// Creates an [Ast](ast/struct.Ast.html) from Lua code
pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let tokens = tokenizer::tokens(code).map_err(Error::TokenizerError)?;
    ast::Ast::from_tokens(tokens).map_err(Error::AstError)
}

/// Prints back Lua code from an [Ast](ast/struct.Ast.html)
pub fn print<'a>(ast: &'a ast::Ast<'a>) -> String {
    ast.tokens
        .iter()
        .fold(String::new(), |acc, token| acc + &token.to_string())
}
