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

#[test]
fn test_similar() {
    let ast = parse("local x = 1; --[[ uh oh, filler ]] local x = 1; local x = 2;").unwrap();
    let stmts = ast.nodes().iter_stmts().collect::<Vec<_>>();

    assert!(stmts[0].similar(stmts[1]));
    assert!(stmts[1].similar(stmts[0]));
    assert!(!stmts[0].similar(stmts[2]));
}
