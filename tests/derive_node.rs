use full_moon::{ast, node::Node, parse, visitors::Visitor};

const MIN_MAX_CODE: &str = "local x = { 1, 2, 3 }";

#[test]
fn test_position_min_max() {
    struct TestVisitor(bool);

    impl<'ast> Visitor<'ast> for TestVisitor {
        fn visit_table_constructor(&mut self, constructor: &ast::TableConstructor<'ast>) {
            self.0 = true;
            assert_eq!(
                MIN_MAX_CODE
                    .bytes()
                    .nth(constructor.start_position().unwrap().bytes()),
                Some(b'{')
            );
            assert_eq!(
                MIN_MAX_CODE
                    .bytes()
                    .nth(constructor.end_position().unwrap().bytes() - 1),
                Some(b'}')
            );
        }
    }

    let ast = parse(MIN_MAX_CODE).unwrap();

    let mut visitor = TestVisitor(false);
    visitor.visit_ast(&ast);
    assert!(visitor.0, "TableConstructor was never found");
}
