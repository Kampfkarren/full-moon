use std::sync::{Arc, RwLock};

use full_moon::node::Node;
use mlua::{ToLua, ToLuaMulti, UserData};

use crate::ast_traits::CreateAstNode;

pub use crate::visitor::add_visit;

pub type ArcLocked<T> = Arc<RwLock<T>>;

pub fn add_core_meta_methods<'lua, T: UserData>(
    name: &'static str,
    methods: &mut impl mlua::UserDataMethods<'lua, T>,
) {
    add_to_string_display(name, methods);
    add_newindex_block(name, methods);
}

pub fn add_core_metamethods_no_tostring<'lua, T: UserData>(
    name: &'static str,
    methods: &mut impl mlua::UserDataMethods<'lua, T>,
) {
    add_newindex_block(name, methods);
}

pub fn add_create_ast_node_methods<'lua, T, N>(methods: &mut impl mlua::UserDataMethods<'lua, T>)
where
    T: UserData + CreateAstNode<Node = N>,
    N: Node,
{
    add_range(methods);
}

pub fn add_range<'lua, T, N>(methods: &mut impl mlua::UserDataMethods<'lua, T>)
where
    T: UserData + CreateAstNode<Node = N>,
    N: Node,
{
    methods.add_method("range", |lua, this, ()| {
        let node = this.create_ast_node().unwrap();

        match node.range() {
            Some((start, end)) => (start.bytes(), end.bytes()).to_lua_multi(lua),
            None => mlua::Value::Nil.to_lua_multi(lua),
        }
    });
}

pub fn add_print<'lua, T, N>(methods: &mut impl mlua::UserDataMethods<'lua, T>)
where
    T: UserData + CreateAstNode<Node = N>,
    N: std::fmt::Display,
{
    methods.add_method("print", |lua, this, ()| match this.create_ast_node() {
        Some(node) => node.to_string().to_lua(lua),
        None => Ok(mlua::Value::Nil),
    });
}

pub fn add_to_string_display<'lua, T: UserData>(
    name: &'static str,
    methods: &mut impl mlua::UserDataMethods<'lua, T>,
) {
    methods.add_meta_method(mlua::MetaMethod::ToString, move |_, this, _: ()| {
        Ok(format!("{name}({:x})", this as *const _ as usize))
    });
}

pub fn add_newindex_block<'lua, T: UserData>(
    name: &'static str,
    methods: &mut impl mlua::UserDataMethods<'lua, T>,
) {
    methods.add_meta_method(
        mlua::MetaMethod::NewIndex,
        move |_, _, (_, _): (String, mlua::Value)| -> mlua::Result<()> {
            // TODO: Detect if withKey exists, and suggest that

            Err(mlua::Error::RuntimeError(format!(
                "can't mutate {name} directly",
            )))
        },
    );
}
