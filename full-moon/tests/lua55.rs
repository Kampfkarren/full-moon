#![cfg(feature = "lua55")]

use full_moon::{
    ast::{
        lua55::{GlobalAssignment, GlobalFunction, GlobalWildcard, NamedVararg},
        LuaVersion,
    },
    node::Node,
    tokenizer::{Lexer, TokenReference, TokenType},
    visitors::{Visitor, VisitorMut},
};

#[test]
fn version_isolation() {
    for version in [
        LuaVersion::lua51(),
        LuaVersion::lua52(),
        LuaVersion::lua53(),
        LuaVersion::lua54(),
    ] {
        let source = "local global = 1; global = global + 1";
        assert_eq!(
            full_moon::parse_fallible(source, version)
                .into_result()
                .unwrap()
                .to_string(),
            source
        );
        for source in [
            "global x",
            "global *",
            "global function f() end",
            "local <const> x = 1",
            "function f(...args) end",
        ] {
            assert!(
                full_moon::parse_fallible(source, version)
                    .into_result()
                    .is_err(),
                "{source} under {version:?}"
            );
        }
    }
    assert!(LuaVersion::lua55().has_lua54());
    assert!(full_moon::parse("global function f(...args) end").is_ok());
}

#[test]
fn visitors_and_positions() {
    #[derive(Default)]
    struct Names(Vec<String>);
    impl Visitor for Names {
        fn visit_global_function(&mut self, node: &GlobalFunction) {
            self.0.push(node.name().to_string());
        }
        fn visit_named_vararg(&mut self, node: &NamedVararg) {
            self.0.push(node.name().to_string());
        }
    }
    struct Rename;
    impl VisitorMut for Rename {
        fn visit_named_vararg(&mut self, node: NamedVararg) -> NamedVararg {
            let token = node
                .name()
                .clone()
                .with_token(full_moon::tokenizer::Token::new(TokenType::Identifier {
                    identifier: "arguments".into(),
                }));
            node.with_name(token)
        }
        fn visit_global_function(&mut self, node: GlobalFunction) -> GlobalFunction {
            let token = node
                .name()
                .clone()
                .with_token(full_moon::tokenizer::Token::new(TokenType::Identifier {
                    identifier: "collect".into(),
                }));
            node.with_name(token)
        }
    }
    let source = "global function f(...args) end";
    let ast = full_moon::parse_fallible(source, LuaVersion::lua55())
        .into_result()
        .unwrap();
    let mut names = Names::default();
    names.visit_ast(&ast);
    assert_eq!(names.0, ["f", "args"]);
    let ast = Rename.visit_ast(ast).update_positions();
    let output = ast.to_string();
    assert_eq!(output, "global function collect(...arguments) end");
    let mut actual: Vec<_> = ast
        .tokens()
        .flat_map(|r| {
            r.leading_trivia()
                .chain(std::iter::once(r.token()))
                .chain(r.trailing_trivia())
        })
        .cloned()
        .collect();
    let mut expected = Lexer::new(&output, LuaVersion::lua55()).collect().unwrap();
    actual.sort_by_key(|token| token.start_position().bytes());
    expected.retain(|token| token.token_kind() != full_moon::tokenizer::TokenKind::Eof);
    // Node traversal includes contained spans before their contents.
    assert_eq!(actual, expected);
    #[cfg(feature = "serde")]
    {
        let encoded = serde_json::to_string(&ast).unwrap();
        let decoded: full_moon::ast::Ast = serde_json::from_str(&encoded).unwrap();
        assert_eq!(ast.nodes(), decoded.nodes());
        assert_eq!(ast.to_string(), decoded.to_string());
    }
}

#[test]
fn constructors_preserve_syntax() {
    let name = || {
        TokenReference::new(
            vec![],
            full_moon::tokenizer::Token::new(TokenType::Identifier {
                identifier: "f".into(),
            }),
            vec![],
        )
    };
    assert_eq!(
        GlobalFunction::new(name()).to_string(),
        "global function f()\nend"
    );
    assert_eq!(NamedVararg::new(name()).to_string(), "...f");
    let names = std::iter::once(full_moon::ast::punctuated::Pair::End(name())).collect();
    assert_eq!(GlobalAssignment::new(names).to_string(), "global f");
    assert_eq!(
        GlobalWildcard::new(TokenReference::symbol("*").unwrap()).to_string(),
        "global *"
    );
}
