use full_moon::{node::Node, parse};

#[test]
fn surrounding_trivia() {
    let ast = parse(include_str!("cases/pass/local-assignment-5/source.lua")).unwrap();
    let stmt = ast.nodes().stmts().nth(1);

    let (prev, _) = stmt.surrounding_trivia();

    let mut prev = prev.into_iter();
    assert_eq!(prev.next().unwrap().to_string(), "-- Then a comment");
    assert_eq!(prev.next().unwrap().to_string(), "\n");
    assert_eq!(prev.next(), None);
}

#[test]
fn test_similar() {
    let ast = parse("local x = 1; --[[ uh oh, filler ]] local x = 1; local x = 2;").unwrap();
    let stmts = ast.nodes().stmts().collect::<Vec<_>>();

    assert!(stmts[0].similar(stmts[1]));
    assert!(stmts[1].similar(stmts[0]));
    assert!(!stmts[0].similar(stmts[2]));
}

#[test]
fn test_tokens_collect() {
    let source = parse("local abcd = 1").unwrap();
    let tokens = source.nodes().tokens();
    assert_eq!(tokens.count(), 4);
}

#[test]
fn test_tokens_back() {
    let source = parse("local abcd = 1").unwrap();
    let mut tokens = source.nodes().tokens();
    assert_eq!(tokens.next_back().unwrap().to_string(), "1");
}
