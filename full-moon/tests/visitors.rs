use full_moon::{
    ast, parse, print,
    tokenizer::*,
    visitors::{Visitor, VisitorMut},
};

#[test]
fn test_visitor() {
    struct FunctionCallVisitor {
        called: Vec<String>,
    }

    impl Visitor for FunctionCallVisitor {
        fn visit_function_call(&mut self, call: &ast::FunctionCall) {
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

    impl VisitorMut for SnakeNamer {
        fn visit_local_assignment(
            &mut self,
            assignment: ast::LocalAssignment,
        ) -> ast::LocalAssignment {
            let name_list = assignment
                .names()
                .pairs()
                .map(|name| {
                    name.to_owned().map(|value| {
                        value.with_token(Token::new(TokenType::Identifier {
                            identifier: value.token().to_string().replace("s", "sss").into(),
                        }))
                    })
                })
                .collect();

            assignment.with_names(name_list)
        }
    }

    let code = parse("local dogs, snakes = 1").unwrap();
    let code = SnakeNamer.visit_ast(code);
    assert_eq!(print(&code), "local dogsss, sssnakesss = 1");

    struct PositionValidator;

    impl Visitor for PositionValidator {
        fn visit_local_assignment(&mut self, assignment: &ast::LocalAssignment) {
            for name in assignment.names() {
                assert_eq!(
                    name.end_position().bytes() - name.start_position().bytes(),
                    name.token().to_string().len()
                );
            }
        }
    }

    let code = code.update_positions();
    PositionValidator.visit_ast(&code);
}

#[test]
fn test_visit_token() {
    #[derive(Default)]
    struct CommentVisitor {
        comments: Vec<String>,
    }

    impl Visitor for CommentVisitor {
        fn visit_single_line_comment(&mut self, token: &Token) {
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

#[test]
fn test_end_visit() {
    #[derive(Default)]
    struct LogVisitor {
        instructions: usize,
        if_start_at: usize,
        if_end_at: usize,
        called_at: usize,
    }

    impl Visitor for LogVisitor {
        fn visit_if(&mut self, _: &ast::If) {
            self.instructions += 1;
            self.if_start_at = self.instructions
        }

        fn visit_if_end(&mut self, _: &ast::If) {
            self.instructions += 1;
            self.if_end_at = self.instructions;
        }

        fn visit_call(&mut self, _: &ast::Call) {
            self.instructions += 1;
            self.called_at = self.instructions;
        }
    }

    let mut visitor = LogVisitor::default();
    visitor.visit_ast(
        &parse(
            r#"
    if true then
        call()
    end
    "#,
        )
        .unwrap(),
    );

    assert_eq!(visitor.if_start_at, 1);
    assert_eq!(visitor.called_at, 2);
    assert_eq!(visitor.if_end_at, 3);
}

#[test]
fn test_unary_visitor_regression() {
    struct TestVisitor(bool);

    impl Visitor for TestVisitor {
        fn visit_un_op(&mut self, _: &ast::UnOp) {
            self.0 = true;
        }
    }

    let mut visitor = TestVisitor(false);
    visitor.visit_ast(&parse("local x = #{}").unwrap());
    assert!(visitor.0, "Unary operation was not visited");
}
