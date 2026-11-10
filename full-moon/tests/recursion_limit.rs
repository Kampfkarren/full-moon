use full_moon::LuaVersion;

fn assert_recursion_limit(code: String, lua_version: LuaVersion) {
    // Debug builds use far more stack per level of nesting than release builds
    let errors = std::thread::Builder::new()
        .stack_size(16 * 1024 * 1024)
        .spawn(move || {
            full_moon::parse_fallible(&code, lua_version)
                .errors()
                .to_vec()
        })
        .unwrap()
        .join()
        .unwrap();

    assert_eq!(
        errors.first().map(|error| error.error_message()),
        Some("exceeded allowed recursion depth".into())
    );
}

#[test]
fn test_recursion_limit() {
    for code in [
        "(".repeat(10000),
        format!("local x = {}1{}", "(".repeat(10000), ")".repeat(10000)),
        format!("local x = {}", "{".repeat(10000)),
        format!("local x = {}1", "not ".repeat(10000)),
        format!("local x = {}1", "1 .. ".repeat(10000)),
        "do ".repeat(10000),
        "if x then ".repeat(10000),
        "repeat ".repeat(10000),
        "function x() ".repeat(10000),
    ] {
        assert_recursion_limit(code, LuaVersion::new());
    }
}

#[test]
#[cfg(feature = "luau")]
fn test_recursion_limit_types() {
    for code in [
        format!("local x: {}", "(".repeat(10000)),
        format!("local x: {}", "{".repeat(10000)),
    ] {
        assert_recursion_limit(code, LuaVersion::luau());
    }
}
