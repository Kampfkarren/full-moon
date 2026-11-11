//! One test per `#[node(...)]` hint. `tokens.rs` covers these corpus-wide,
//! so these name the behaviour and a failure points at the hint that broke.
use full_moon::{ast, node::Node};

fn tokens(node: &impl Node) -> String {
    node.tokens().map(|token| token.to_string()).collect()
}

/// Without hints: the opening delimiter goes in place, the closing one at the end.
#[test]
fn test_span_halves_surround_the_content() {
    let ast = full_moon::parse("local t = { 1 }").unwrap();
    let statement = ast.nodes().stmts().next().unwrap();

    assert_eq!(tokens(statement), "local t = { 1 }");
}

/// `Field::ExpressionKey` declares `brackets, key, equal, value` but writes `[key] = value`,
/// so without `close` the bracket would land after `value`.
#[test]
fn test_close_hint_places_the_close_early() {
    let ast = full_moon::parse("local t = { [k] = v }").unwrap();

    let expression = ast
        .nodes()
        .stmts()
        .find_map(|statement| match statement {
            ast::Stmt::LocalAssignment(assignment) => {
                assignment.expressions().iter().next().cloned()
            }
            _ => None,
        })
        .expect("expected a local assignment");

    assert_eq!(tokens(&expression), "{ [k] = v }");
}

/// Each name is followed by its own specifier, before the comma, from a parallel `Vec`.
#[test]
#[cfg(feature = "luau")]
fn test_interleave_hint_zips_type_specifiers() {
    let ast = full_moon::parse_fallible(
        "local a: number, b, c: string = 1, 2, 3",
        ast::LuaVersion::luau(),
    )
    .into_result()
    .unwrap();

    let statement = ast.nodes().stmts().next().unwrap();

    assert_eq!(tokens(statement), "local a: number, b, c: string = 1, 2, 3");
}

/// `NumericFor` declares `type_specifier` last but writes it after the index variable.
#[test]
#[cfg(feature = "luau")]
fn test_after_hint_moves_the_specifier() {
    let ast = full_moon::parse_fallible("for i: number = 1, 10 do end", ast::LuaVersion::luau())
        .into_result()
        .unwrap();

    let statement = ast.nodes().stmts().next().unwrap();

    assert_eq!(tokens(statement), "for i: number = 1, 10 do end");
}

/// `<<T>>` closes inside-out.
#[test]
#[cfg(feature = "luau")]
fn test_nested_spans_close_inside_out() {
    let ast = full_moon::parse_fallible("f<<T>>()", ast::LuaVersion::luau())
        .into_result()
        .unwrap();

    let statement = ast.nodes().stmts().next().unwrap();

    assert_eq!(tokens(statement), "f<<T>>()");
}
