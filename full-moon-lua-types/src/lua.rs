use crate::core;

pub fn create_lua() -> mlua::Result<mlua::Lua> {
    let lua = mlua::Lua::new();

    assign_globals(&lua)?;

    Ok(lua)
}

fn assign_globals(lua: &mlua::Lua) -> mlua::Result<()> {
    let globals = lua.globals();

    globals.set(
        "parse",
        lua.create_function(|_, code: String| {
            let ast = full_moon::parse(&code).expect("NYI: Error on failure");

            Ok(core::Ast::from(ast))
        })?,
    )
}
