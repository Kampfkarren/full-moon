//! `Node::tokens()` must reproduce `Display`, so these walk every node of every pass case.
//!
//! The corpus tests read `tests/*_cases`. `no-source-tests` marks them `#[ignore]`, which
//! skips them at run time but still compiles the helpers below, so none of them go unused.
use full_moon::{ast::LuaVersion, node::Node, tokenizer::TokenKind, visitors::Visitor};
use std::{fmt::Display, fs, path::Path};

/// Compares the tokens of every node it visits with `Display`.
struct TokensAgree {
    failures: Vec<String>,
    checked: usize,
}

impl TokensAgree {
    fn new() -> Self {
        Self {
            failures: Vec::new(),
            checked: 0,
        }
    }

    fn check<T: Display + Node>(&mut self, kind: &str, node: &T) {
        let display = node.to_string();
        let tokens: String = node.tokens().map(|token| token.to_string()).collect();

        self.checked += 1;

        if display != tokens {
            self.failures
                .push(format!("{kind}: display={display:?} tokens={tokens:?}"));
        }
    }
}

/// Implements [`Visitor`] for every node type in the AST.
///
/// An entry's `#[cfg]` must match the one on the `Visitor` method it overrides.
///
/// Left out: `visit_contained_span`, which has no `Display`,
/// and `visit_token_reference` with `visit_eof`, whose tokens are the token itself.
macro_rules! check_nodes {
    ($($(#[$meta:meta])* $visit:ident: $ty:path),* $(,)?) => {
        impl Visitor for TokensAgree {
            $(
                $(#[$meta])*
                fn $visit(&mut self, node: &$ty) {
                    self.check(stringify!($visit), node);
                }
            )*
        }
    };
}

check_nodes! {
    visit_anonymous_function: full_moon::ast::AnonymousFunction,
    visit_assignment: full_moon::ast::Assignment,
    visit_block: full_moon::ast::Block,
    visit_call: full_moon::ast::Call,
    visit_do: full_moon::ast::Do,
    visit_else_if: full_moon::ast::ElseIf,
    visit_expression: full_moon::ast::Expression,
    visit_field: full_moon::ast::Field,
    visit_function_args: full_moon::ast::FunctionArgs,
    visit_function_body: full_moon::ast::FunctionBody,
    visit_function_call: full_moon::ast::FunctionCall,
    visit_function_declaration: full_moon::ast::FunctionDeclaration,
    visit_function_name: full_moon::ast::FunctionName,
    visit_generic_for: full_moon::ast::GenericFor,
    visit_if: full_moon::ast::If,
    visit_index: full_moon::ast::Index,
    visit_local_assignment: full_moon::ast::LocalAssignment,
    visit_local_function: full_moon::ast::LocalFunction,
    visit_last_stmt: full_moon::ast::LastStmt,
    visit_method_call: full_moon::ast::MethodCall,
    visit_numeric_for: full_moon::ast::NumericFor,
    visit_parameter: full_moon::ast::Parameter,
    visit_prefix: full_moon::ast::Prefix,
    visit_repeat: full_moon::ast::Repeat,
    visit_return: full_moon::ast::Return,
    visit_stmt: full_moon::ast::Stmt,
    visit_suffix: full_moon::ast::Suffix,
    visit_table_constructor: full_moon::ast::TableConstructor,
    visit_un_op: full_moon::ast::UnOp,
    visit_var: full_moon::ast::Var,
    visit_var_expression: full_moon::ast::VarExpression,
    visit_while: full_moon::ast::While,

    #[cfg(feature = "luau")]
    visit_luau_attribute: full_moon::ast::luau::LuauAttribute,
    #[cfg(feature = "luau")]
    visit_const_assignment: full_moon::ast::luau::ConstAssignment,
    #[cfg(feature = "luau")]
    visit_const_function: full_moon::ast::luau::ConstFunction,
    #[cfg(feature = "luau")]
    visit_else_if_expression: full_moon::ast::luau::ElseIfExpression,
    #[cfg(feature = "luau")]
    visit_exported_type_declaration: full_moon::ast::luau::ExportedTypeDeclaration,
    #[cfg(feature = "luau")]
    visit_exported_type_function: full_moon::ast::luau::ExportedTypeFunction,
    #[cfg(feature = "luau")]
    visit_generic_declaration: full_moon::ast::luau::GenericDeclaration,
    #[cfg(feature = "luau")]
    visit_generic_declaration_parameter: full_moon::ast::luau::GenericDeclarationParameter,
    #[cfg(feature = "luau")]
    visit_generic_parameter_info: full_moon::ast::luau::GenericParameterInfo,
    #[cfg(feature = "luau")]
    visit_if_condition_binding: full_moon::ast::luau::IfConditionBinding,
    #[cfg(feature = "luau")]
    visit_if_expression: full_moon::ast::luau::IfExpression,
    #[cfg(feature = "luau")]
    visit_indexed_type_info: full_moon::ast::luau::IndexedTypeInfo,
    #[cfg(feature = "luau")]
    visit_interpolated_string: full_moon::ast::luau::InterpolatedString,
    #[cfg(feature = "luau")]
    visit_type_argument: full_moon::ast::luau::TypeArgument,
    #[cfg(feature = "luau")]
    visit_type_assertion: full_moon::ast::luau::TypeAssertion,
    #[cfg(feature = "luau")]
    visit_type_declaration: full_moon::ast::luau::TypeDeclaration,
    #[cfg(feature = "luau")]
    visit_type_field: full_moon::ast::luau::TypeField,
    #[cfg(feature = "luau")]
    visit_type_field_key: full_moon::ast::luau::TypeFieldKey,
    #[cfg(feature = "luau")]
    visit_type_function: full_moon::ast::luau::TypeFunction,
    #[cfg(feature = "luau")]
    visit_type_info: full_moon::ast::luau::TypeInfo,
    #[cfg(feature = "luau")]
    visit_type_instantiation: full_moon::ast::luau::TypeInstantiation,
    #[cfg(feature = "luau")]
    visit_type_intersection: full_moon::ast::luau::TypeIntersection,
    #[cfg(feature = "luau")]
    visit_type_specifier: full_moon::ast::luau::TypeSpecifier,
    #[cfg(feature = "luau")]
    visit_type_union: full_moon::ast::luau::TypeUnion,

    #[cfg(any(feature = "lua52", feature = "luajit"))]
    visit_goto: full_moon::ast::lua52::Goto,
    #[cfg(any(feature = "lua52", feature = "luajit"))]
    visit_label: full_moon::ast::lua52::Label,
    #[cfg(feature = "lua54")]
    visit_attribute: full_moon::ast::lua54::Attribute,
    #[cfg(any(feature = "cfxlua", feature = "luau"))]
    visit_compound_assignment: full_moon::ast::CompoundAssignment,
    #[cfg(any(feature = "cfxlua", feature = "luau"))]
    visit_compound_op: full_moon::ast::CompoundOp,
}

/// Every `source.lua` under `folder`, which holds one directory per case.
fn sources(folder: &Path) -> Vec<std::path::PathBuf> {
    let mut found = Vec::new();
    let mut pending = vec![folder.to_owned()];

    while let Some(folder) = pending.pop() {
        for entry in fs::read_dir(&folder)
            .unwrap_or_else(|error| panic!("couldn't read {folder:?}: {error}"))
        {
            let path = entry
                .unwrap_or_else(|error| panic!("couldn't read an entry of {folder:?}: {error}"))
                .path();

            if path.is_dir() {
                pending.push(path);
            } else if path.file_name().is_some_and(|name| name == "source.lua") {
                found.push(path);
            }
        }
    }

    found.sort();
    found
}

/// Checks every node of every file in `folder`, plus the `Ast`, and returns how many.
fn check_folder(folder: &Path, version: LuaVersion) -> usize {
    let mut failures = Vec::new();
    let mut checked = 0;
    let mut files = 0;
    let mut asts = 0;

    for path in sources(folder) {
        let source = fs::read_to_string(&path).unwrap();
        let ast = full_moon::parse_fallible(&source, version)
            .into_result()
            .unwrap_or_else(|error| panic!("couldn't parse {path:?}: {error:#?}"));

        files += 1;

        let mut checker = TokensAgree::new();
        checker.visit_ast(&ast);
        checked += checker.checked;
        asts += 1;

        failures.extend(
            checker
                .failures
                .into_iter()
                .map(|failure| format!("{path:?}: {failure}")),
        );

        let display = ast.to_string();
        let tokens: String = ast.tokens().map(|token| token.to_string()).collect();
        if display != tokens {
            failures.push(format!(
                "{path:?}: Ast: display={display:?} tokens={tokens:?}"
            ));
        }
    }

    assert!(files > 0, "no files found in {folder:?}");
    assert!(
        failures.is_empty(),
        "{} failures among {checked} nodes and {asts} trees in these files had tokens \
         that disagree with Display:\n{}",
        failures.len(),
        failures.join("\n"),
    );

    checked
}

/// Reuses the parser's own test cases.
fn check(version: LuaVersion, folder: &str) -> usize {
    check_folder(&Path::new(env!("CARGO_MANIFEST_DIR")).join(folder), version)
}

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua51() {
    assert!(check(LuaVersion::lua51(), "tests/cases/pass") > 0);
}

#[test]
#[cfg(feature = "lua52")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua52() {
    assert!(check(LuaVersion::lua52(), "tests/lua52_cases/pass") > 0);
}

#[test]
#[cfg(feature = "lua53")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua53() {
    assert!(check(LuaVersion::lua53(), "tests/lua53_cases/pass") > 0);
}

#[test]
#[cfg(feature = "lua54")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua54() {
    assert!(check(LuaVersion::lua54(), "tests/lua54_cases/pass") > 0);
}

#[test]
#[cfg(feature = "luau")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_luau() {
    assert!(check(LuaVersion::luau(), "tests/roblox_cases/pass") > 0);
}

#[test]
#[cfg(feature = "luajit")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_luajit() {
    assert!(check(LuaVersion::luajit(), "tests/luajit_cases/pass") > 0);
}

#[test]
#[cfg(feature = "cfxlua")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_cfxlua() {
    assert!(check(LuaVersion::cfxlua(), "tests/cfxlua_cases/pass") > 0);
}

/// The minimal examples from the bug report.
#[test]
fn test_reported_cases() {
    for source in [
        "f(a, b)\n",
        "local t = {1, 2}\n",
        "local x = (foo)\n",
        "local x = t[1]\n",
        "function f(a) end\n",
        "local a, b = 1, 2\n",
        "return 1, 2\n",
        "local x = t.a\n",
    ] {
        let ast = full_moon::parse(source).unwrap();
        let tokens: String = ast.tokens().map(|token| token.to_string()).collect();

        assert_eq!(ast.to_string(), tokens, "for source {source:?}");
    }
}

/// Nesting exposes a wrong close order worst: `f(g(h(1)))` used to come back as
/// `f()g()h()1`, which reads like three empty calls followed by a stray `1`.
///
/// The corpus reaches a bracket depth of four, but never with a call inside a
/// call, so this pins the case the reported issue was about.
#[test]
fn test_nested_calls_keep_their_order() {
    let source = "\
local packed = table.pack(
    outer(inner(1), inner(2, deep(3, 4))),
    wrap({ key = nest({ 1, { 2, 3 } }) })
)
";
    let ast = full_moon::parse(source).unwrap();
    let tokens: String = ast.tokens().map(|token| token.to_string()).collect();

    assert_eq!(ast.to_string(), tokens);
}

/// `surrounding_trivia` takes the first and last token of `tokens()`,
/// so a node whose tokens were out of order reported the wrong trivia.
/// Leads and trails now match `Display` as a consequence.
#[test]
fn test_surrounding_trivia() {
    let source = "-- leading\nlocal x = { 1, 2 } -- trailing\n";
    let ast = full_moon::parse(source).unwrap();
    let statement = ast.nodes().stmts().next().unwrap();

    let (leading, trailing) = statement.surrounding_trivia();
    let leading = leading
        .into_iter()
        .map(|t| t.to_string())
        .collect::<String>();
    let trailing = trailing
        .into_iter()
        .map(|t| t.to_string())
        .collect::<String>();

    assert_eq!(leading, "-- leading\n");
    assert_eq!(trailing, " -- trailing\n");
}

/// `Ast::tokens()` ends with the EOF token, which has no trailing trivia of its own,
/// so taking the last token naively reported an empty trail for a whole file.
#[test]
fn test_ast_surrounding_trivia_skips_eof() {
    let source = "-- leading\nlocal x = 1 -- trailing\n";
    let ast = full_moon::parse(source).unwrap();

    let (leading, trailing) = ast.surrounding_trivia();
    let leading = leading
        .into_iter()
        .map(|t| t.to_string())
        .collect::<String>();
    let trailing = trailing
        .into_iter()
        .map(|t| t.to_string())
        .collect::<String>();

    assert_eq!(leading, "-- leading\n");
    assert_eq!(trailing, " -- trailing\n");
}

/// `Ast::tokens()` now ends with the EOF token, so it had better sit at the end of the file.
/// `update_positions` rebuilds it through `visit_eof`; this pins the position as parsed.
#[test]
fn test_eof_is_last_and_sits_at_the_end() {
    for source in [
        "local x = { a = 1, b = f(2) }\n",
        "local x = 1\nlocal y = 2\n",
        "-- comment only\n",
        "",
    ] {
        let ast = full_moon::parse(source).unwrap();

        let last = ast.tokens().next_back().expect("tokens() is never empty");
        assert_eq!(last.token_kind(), TokenKind::Eof, "for source {source:?}");

        let position = ast.eof().start_position().expect("eof has a position");
        assert_eq!(
            position.bytes(),
            source.len(),
            "eof should sit at the end of {source:?}",
        );
    }
}

/// `Ast::tokens()` includes the EOF token, which holds the end of the file as *leading*
/// trivia, so nothing between the last statement and the end of the source is lost.
#[test]
fn test_trailing_trivia_is_included() {
    for source in ["local x = 1\n\n", "-- only a comment\n", "\n", ""] {
        let ast = full_moon::parse(source).unwrap();
        let tokens: String = ast.tokens().map(|token| token.to_string()).collect();

        assert_eq!(tokens, source, "for source {source:?}");
    }
}

/// Pins a known disagreement between `tokens()` and `end_position()`.
///
/// `StructRangeGenerator` walks fields in declaration order, while `tokens()` follows
/// `Display`. For a node whose `Display` order differs from its declaration order, the two
/// can name different last fields: `NumericFor` declares `type_specifier` last but writes
/// it after `index_variable`, so `end_position()` lands on the type specifier and
/// `tokens()` on `end_token`.
///
/// This is how the parser behaved before the token fix and is deliberately not changed
/// here: `Node::range` is public API and out of scope. The test records the behaviour rather
/// than leaving it to be discovered, so a future fix is visible.
#[test]
#[cfg(feature = "luau")]
fn test_end_position_uses_declaration_order() {
    let source = "for i: number = 1, 10 do end";
    let ast = full_moon::parse_fallible(source, LuaVersion::luau())
        .into_result()
        .unwrap();

    let numeric_for = ast
        .nodes()
        .stmts()
        .find_map(|statement| match statement {
            full_moon::ast::Stmt::NumericFor(node) => Some(node),
            _ => None,
        })
        .expect("expected a numeric for");

    let end = numeric_for
        .end_position()
        .expect("numeric for has a position")
        .bytes();
    let last_token = numeric_for
        .tokens()
        .next_back()
        .expect("tokens() is never empty")
        .end_position()
        .expect("token has a position")
        .bytes();

    // `end_position` stops at the type specifier, which is declared last...
    assert_eq!(&source[..end], "for i: number");
    // ...while the tokens end at `end`, which is what `Display` writes last.
    assert_eq!(last_token, source.len());
}
