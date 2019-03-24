pub mod ast;
pub mod tokenizer;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

#[derive(Clone, Debug, PartialEq)]
pub enum Error {
    AstError(ast::AstError),
    TokenizerError(tokenizer::TokenizerError),
}

pub fn parse(code: &str) -> Result<ast::Ast, Error> {
    let tokens = tokenizer::tokens(code).map_err(Error::TokenizerError)?;
    Ok(ast::Ast { tokens })
}

pub fn print(ast: &ast::Ast) -> String {
    ast.tokens
        .iter()
        .fold(String::new(), |acc, token| acc + &token.to_string())
}
