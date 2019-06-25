use full_moon::{ast, parse, print, tokenizer, visitors::{Visitor, VisitorMut}};
use std::borrow::Cow;

#[test]
fn test_visitor() {
    struct FunctionCallVisitor {
        called: Vec<String>,
    };

    impl<'ast> Visitor<'ast> for FunctionCallVisitor {
        fn visit_function_call(&mut self, call: &ast::FunctionCall<'ast>) {
            match call.prefix() {
                ast::Prefix::Name(token) => {
                    self.called.push(token.to_string());
                }

                _ => unreachable!(),
            }
        }
    }

    let code = parse("foo(bar())").unwrap();
    let mut visitor = FunctionCallVisitor { called: Vec::new() };

    visitor.visit_ast(&code);

    assert_eq!(visitor.called, vec!["foo", "bar"]);
}

#[test]
fn test_visitor_mut() {
    struct SnakeNamer;

    impl<'ast> VisitorMut<'ast> for SnakeNamer {
        fn visit_local_assignment(&mut self, assignment: &mut ast::LocalAssignment<'ast>) {
            for name in assignment.iter_name_list_mut() {
                let identifier;

                match *name.token_type() {
                    tokenizer::TokenType::Identifier { identifier: ref identifier_tmp } => {
                        identifier = identifier_tmp.replace("s", "sss");
                    },

                    _ => unreachable!(),
                }

                name.set_token_type(tokenizer::TokenType::Identifier {
                    identifier: Cow::from(identifier),
                });
            }
        }
    }

    let mut code = parse("local dogs, snakes = 1").unwrap();
    SnakeNamer.visit_ast(&mut code);
    assert_eq!(print(&code), "local dogsss, sssnakesss = 1");
}
