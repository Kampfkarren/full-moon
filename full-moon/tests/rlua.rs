use std::fs;

macro_rules! lua_tests {
	({$(
		$name:ident,
	)+}) => (
		$(
			#[cfg(feature = "lua-bindings")]
			#[test]
			fn $name() {
				let source = fs::read_to_string(format!("./tests/rlua/{}.lua", stringify!($name)))
					.expect("couldn't open file");
				let lua = rlua::Lua::new();
				lua.context(|context| {
					let callback: rlua::Function = context.load(&source).eval().unwrap();
					if let Err(err) = callback.call::<_, ()>(full_moon::rlua::LuaApi) {
						panic!("{}", err);
					}
				});
			}
		)+
	)
}

lua_tests!({
	properties,
});
