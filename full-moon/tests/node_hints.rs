//! One test per `#[node(...)]` hint, so a failure points at the hint that broke.
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

/// The examples from the two reports, each of which used to come back reordered.
#[test]
fn test_reported_cases() {
    for source in [
        "local x = (foo)\n",
        "f(a, b)\n",
        "local t = { [k] = v }\n",
        "math.random(3, 12)\n",
        "local a, b = f(1), g(2, 3)\n",
    ] {
        let ast = full_moon::parse(source).unwrap();
        let tokens: String = ast.tokens().map(|token| token.to_string()).collect();

        assert_eq!(tokens, source, "tokens for {source:?}");
    }
}

/// Nesting exposes a wrong close order worst: `f(g(h(1)))` used to come back as
/// `f()g()h()1`, which reads like three empty calls followed by a stray `1`.
#[test]
fn test_nested_calls_keep_their_order() {
    let source = "local packed = table.pack(outer(inner(1), inner(2, deep(3, 4))))\n";
    let ast = full_moon::parse(source).unwrap();
    let tokens: String = ast.tokens().map(|token| token.to_string()).collect();

    assert_eq!(tokens, source);
}

/// `surrounding_trivia` takes the first and last token of `tokens()`, so a node whose
/// tokens were out of order reported the wrong trivia.
#[test]
fn test_surrounding_trivia_follows_token_order() {
    let source = "-- leading\nlocal x = { 1, 2 } -- trailing\n";
    let ast = full_moon::parse(source).unwrap();
    let statement = ast.nodes().stmts().next().unwrap();

    let (leading, trailing) = statement.surrounding_trivia();
    let leading: String = leading.into_iter().map(|t| t.to_string()).collect();
    let trailing: String = trailing.into_iter().map(|t| t.to_string()).collect();

    assert_eq!(leading, "-- leading\n");
    assert_eq!(trailing, " -- trailing\n");
}
