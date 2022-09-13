fn test_lua_code(code: &str) {
    let lua = full_moon_lua_types::create_lua().expect("can't create lua");

    if let Err(error) = lua.load(code).exec() {
        panic!("lua error:\n{error}");
    }
}

#[test]
fn core() {
    test_lua_code(include_str!("lua/core.lua"));
}

#[test]
fn everything() {
    test_lua_code(include_str!("lua/everything.lua"));
}
