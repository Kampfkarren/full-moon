use mlua::UserData;

pub fn add_to_string_display<'lua, T: UserData, M: mlua::UserDataMethods<'lua, T>>(
    name: &'static str,
    methods: &mut M,
) {
    methods.add_meta_method(mlua::MetaMethod::ToString, move |_, this, _: ()| {
        Ok(format!("{name}({:x})", this as *const _ as usize))
    });
}
