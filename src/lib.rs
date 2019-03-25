pub mod ast;
pub mod tokenizer;

#[cfg(all(test, not(feature = "serde")))]
compile_error!("Serde feature must be enabled for tests");

#[derive(Clone, Debug, PartialEq)]
pub enum Error<'a> {
    AstError(ast::AstError<'a>),
    TokenizerError(tokenizer::TokenizerError),
}

pub fn print(ast: &ast::Ast) -> String {
    ast.tokens
        .iter()
        .fold(String::new(), |acc, token| acc + &token.to_string())
}
