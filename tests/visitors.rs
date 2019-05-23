use full_moon::{ast, parse, visitors::Visitor};

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
