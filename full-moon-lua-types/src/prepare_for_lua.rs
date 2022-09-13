use std::sync::Arc;

use mlua::{ToLua, UserData};

use crate::mlua_util::ArcLocked;

pub trait PrepareForLua {
    fn prepare_for_lua<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>>;
}

impl<T: 'static + UserData> PrepareForLua for ArcLocked<T> {
    fn prepare_for_lua<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>> {
        Arc::clone(self).to_lua(lua)
    }
}

impl<T: 'static + UserData> PrepareForLua for Vec<ArcLocked<T>> {
    fn prepare_for_lua<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>> {
        self.iter()
            .map(Arc::clone)
            .map(|x| x.to_lua(lua))
            .collect::<mlua::Result<Vec<_>>>()?
            .to_lua(lua)
    }
}

impl<T: 'static + Clone + UserData> PrepareForLua for Box<T> {
    fn prepare_for_lua<'lua>(&self, lua: &'lua mlua::Lua) -> mlua::Result<mlua::Value<'lua>> {
        (**self).clone().to_lua(lua)
    }
}
