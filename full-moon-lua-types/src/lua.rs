use crate::core;

pub fn create_lua() -> mlua::Result<mlua::Lua> {
    let lua = mlua::Lua::new();

    assign_globals(&lua)?;

    Ok(lua)
}

pub fn full_moon_table(lua: &mlua::Lua) -> mlua::Result<mlua::Table> {
    let full_moon = lua.create_table()?;

    full_moon.set(
        "parse",
        lua.create_function(|_, code: String| {
            let ast = full_moon::parse(&code).expect("NYI: Error on failure");

            Ok(core::Ast::from(&ast))
        })?,
    )?;

    Ok(full_moon)
}

fn assign_globals(lua: &mlua::Lua) -> mlua::Result<()> {
    let globals = lua.globals();

    globals.set("full_moon", full_moon_table(lua)?)?;

    Ok(())
}
