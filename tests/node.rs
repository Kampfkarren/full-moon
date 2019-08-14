use full_moon::{node::Node, parse};

#[test]
fn surrounding_ignore_tokens() {
    let ast = parse(include_str!("cases/pass/local-assignment-5/source.lua")).unwrap();
    let stmt = ast.nodes().iter_stmts().nth(1);

    let (prev, next) = stmt.surrounding_ignore_tokens(&ast).unwrap();

    let mut prev = prev.into_iter();
    assert_eq!(prev.next().unwrap().to_string(), "\n");
    assert_eq!(prev.next().unwrap().to_string(), "-- Then a comment");
    assert_eq!(prev.next().unwrap().to_string(), "\n");
    assert_eq!(prev.next(), None);

    let mut next = next.into_iter();
    assert_eq!(next.next().unwrap().to_string(), "\n");
    assert_eq!(next.next(), None);
}
