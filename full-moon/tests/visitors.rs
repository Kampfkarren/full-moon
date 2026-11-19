use full_moon::{
    ast, parse,
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
                            identifier: value.token().to_string().replace('s', "sss").into(),
                        }))
                    })
                })
                .collect();

            assignment.with_names(name_list)
        }
    }

    let code = parse("local dogs, snakes = 1").unwrap();
    let code = SnakeNamer.visit_ast(code);
    assert_eq!(code.to_string(), "local dogsss, sssnakesss = 1");

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
fn test_visitor_mut_replace() {
    use full_moon::ast::punctuated::Pair;

    #[derive(Default)]
    struct Replacer {
        replace_calls: usize,
        visit_calls: usize,
        visit_end_calls: usize,
        numbers_seen: Vec<String>,
    }

    impl VisitorMut for Replacer {
        // Swaps the value of `local x = ...` for `999`, leaving other
        // local assignments untouched.
        fn replace_local_assignment(
            &mut self,
            assignment: &ast::LocalAssignment,
        ) -> Option<ast::LocalAssignment> {
            self.replace_calls += 1;

            let is_x = assignment
                .names()
                .iter()
                .next()
                .is_some_and(|name| name.token().to_string() == "x");

            if !is_x {
                return None;
            }

            let original_token = match assignment.expressions().iter().next() {
                Some(ast::Expression::Number(token)) => token,
                _ => unreachable!(),
            };

            let replacement_token = original_token
                .to_owned()
                .with_token(Token::new(TokenType::Number { text: "999".into() }));

            Some(assignment.to_owned().with_expressions(
                std::iter::once(Pair::End(ast::Expression::Number(replacement_token))).collect(),
            ))
        }

        fn visit_local_assignment(
            &mut self,
            assignment: ast::LocalAssignment,
        ) -> ast::LocalAssignment {
            self.visit_calls += 1;
            assignment
        }

        fn visit_local_assignment_end(
            &mut self,
            assignment: ast::LocalAssignment,
        ) -> ast::LocalAssignment {
            self.visit_end_calls += 1;
            assignment
        }

        fn visit_number(&mut self, token: Token) -> Token {
            self.numbers_seen.push(token.to_string());
            token
        }
    }

    let code = parse("local x = 1\nlocal y = 2").unwrap();
    let mut replacer = Replacer::default();
    let code = replacer.visit_ast(code);

    assert_eq!(code.to_string(), "local x = 999\nlocal y = 2");

    // `replace_local_assignment` is consulted for every local assignment...
    assert_eq!(replacer.replace_calls, 2);
    // ...but `visit_local_assignment`/`_end` only fire for the one that
    // wasn't replaced - a replaced node bypasses its own visit hooks.
    assert_eq!(replacer.visit_calls, 1);
    assert_eq!(replacer.visit_end_calls, 1);
    // The replacement's own subtree (`999`) is never descended into -
    // only the untouched assignment's `2` is seen.
    assert_eq!(replacer.numbers_seen, vec!["2"]);
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
