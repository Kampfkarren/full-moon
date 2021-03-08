use full_moon::{node::Node, parse};

#[test]
fn test_one_line_range() {
    let ast = parse(
        r#"

	local x = 1

	local y = 1

	local function x() print(1) end

	function x() print(1) end

	for index, value in pairs(list) do print(index, value) end

	"#,
    )
    .unwrap();

    for stmt in ast.nodes().stmts() {
        let (start, end) = stmt.range().unwrap();
        assert_eq!(
            end.line() - start.line(),
            0,
            "node {:?} does not have a range on the same line",
            stmt
        );
    }
}
