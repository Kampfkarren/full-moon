// This is code from a real life usage of full-moon

use full_moon::{self, ast::*, node::Node, tokenizer::TokenKind, visitors::Visitor};
use owned::Owned;
use std::error::Error;

const CODE: &str = r#"

local Module = {}
Module.__index = Module

--[[
    Creates a new instance of Module.

    ```lua
        Module.new()
    ```

    @static
    @param name string - This is the name for this Module.
    @returns Module - Returns the new Module!
]]
function Module.new(name)

end

"#;

struct MemberVisitor<'a> {
    comments: Vec<String>,
    ast: &'a Ast<'static>,
}

impl Visitor<'static> for MemberVisitor<'_> {
    fn visit_function_declaration(&mut self, function: &FunctionDeclaration<'static>) {
        if let Some((tokens, _)) = function.surrounding_ignore_tokens(&self.ast) {
            let mut tokens = tokens.clone();
            tokens.retain(|&t| t.token_kind() == TokenKind::MultiLineComment);
            self.comments.extend(
                tokens
                    .into_iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>(),
            )
        }
    }
}

fn generate() -> Result<(), Box<dyn Error>> {
    let ast = full_moon::parse(CODE)?.owned();

    let mut visitor = MemberVisitor {
        comments: Vec::new(),
        ast: &ast,
    };

    visitor.visit_ast(&ast);

    println!("{:?}", visitor.comments);

    Ok(())
}

fn main() {
    generate().expect("Oh");
}
