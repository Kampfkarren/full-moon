use full_moon::{
    ast, parse, print, tokenizer,
    visitors::{Visitor, VisitorMut},
};
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
            for name in assignment.name_list_mut().pairs_mut() {
                let identifier;

                match *name.value_mut().token_type() {
                    tokenizer::TokenType::Identifier {
                        identifier: ref identifier_tmp,
                    } => {
                        identifier = identifier_tmp.replace("s", "sss");
                    }

                    _ => unreachable!(),
                }

                name.value_mut()
                    .set_token_type(tokenizer::TokenType::Identifier {
                        identifier: Cow::from(identifier),
                    });
            }
        }
    }

    let mut code = parse("local dogs, snakes = 1").unwrap();
    SnakeNamer.visit_ast(&mut code);
    assert_eq!(print(&code), "local dogsss, sssnakesss = 1");

    struct PositionValidator;

    impl<'ast> Visitor<'ast> for PositionValidator {
        fn visit_local_assignment(&mut self, assignment: &ast::LocalAssignment<'ast>) {
            for name in assignment.name_list() {
                assert_eq!(
                    name.end_position().bytes() - name.start_position().bytes(),
                    name.to_string().len()
                );
            }
        }
    }

    code.update_positions();
    PositionValidator.visit_ast(&code);
}

#[test]
fn test_visit_token() {
    #[derive(Default)]
    struct CommentVisitor {
        comments: Vec<String>,
    };

    impl Visitor<'_> for CommentVisitor {
        fn visit_single_line_comment(&mut self, token: &tokenizer::TokenReference<'_>) {
            self.comments.push(token.to_string());
        }
    }

    let mut visitor = CommentVisitor::default();

    let code = parse(
        r#"
    -- bla bla bla
    --[[
        multi line comment
    ]]

    -- comment here
    local x = 1
    -- and here
    "#,
    )
    .unwrap();

    visitor.visit_ast(&code);
    assert_eq!(
        visitor.comments,
        vec!["-- bla bla bla", "-- comment here", "-- and here"]
    );
}
