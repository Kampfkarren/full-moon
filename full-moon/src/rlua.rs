use crate::{
    ast::{owned::Owned, *},
    tokenizer::TokenReference,
};
use rlua::ToLua;

macro_rules! enum_userdata {
    ($object:path, {
		$(
			$tag: path $(=> $modify:ident)?,
		)+
	}) => {
        impl<'lua> rlua::ToLua<'lua> for $object {
            fn to_lua(self, lua: rlua::Context<'lua>) -> rlua::Result<rlua::Value<'lua>> {
                match self {
					$(
						$tag(data) => {
							$(
								let data = $modify(lua, data)?;
							)?

							data.to_owned().to_lua(lua)
						}
					)+
                }
            }
        }
    };
}

macro_rules! userdata {
    ($object:path, {
		$(
			properties: {
				$($property_name:ident: $property_getter:expr,)+
			},
		)?
	}) => {
        impl rlua::UserData for $object {
            fn add_methods<'lua, M: rlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
				methods.add_meta_method(
					rlua::MetaMethod::Index,
					|lua, this, key: String| -> rlua::Result<rlua::Value<'lua>> {
						match key.as_str() {
							$(
								$(
									stringify!($property_name) => {
										return $property_getter(lua, this)?.to_lua(lua);
									},
								)+
							)?

							key => {
								return Err(rlua::Error::external(format!("{} is not a valid property", key)));
							}
						}
					},
				);
			}
		}
    };
}

fn decow_token_reference<'lua>(
    _: rlua::Context<'lua>,
    cow: std::borrow::Cow<TokenReference<'static>>,
) -> rlua::Result<TokenReference<'static>> {
    return Ok(cow.to_owned().into_owned());
}

fn borrowed_return<'a, 'lua, T, U, F>(
    function: F,
) -> Box<dyn FnOnce(rlua::Context<'lua>, T) -> rlua::Result<rlua::Value<'lua>>>
where
    <U as ToOwned>::Owned: rlua::ToLua<'lua>,
    U: 'a + Clone,
    F: FnOnce(T) -> &'a U + 'static,
{
    return Box::new(|lua, this| function(this).to_owned().to_lua(lua));
}

fn optional<'a, 'lua, T, U, F>(
    function: F,
) -> Box<dyn FnOnce(rlua::Context<'lua>, T) -> rlua::Result<rlua::Value<'lua>>>
where
    <U as ToOwned>::Owned: rlua::ToLua<'lua>,
    U: 'a + Clone,
    F: FnOnce(T) -> Option<&'a U> + 'static,
{
    return Box::new(|lua, this| match function(this) {
        Some(value) => value.to_owned().to_lua(lua),
        None => Ok(rlua::Nil),
    });
}

userdata!(Ast<'static>, {
    properties: {
    	nodes: borrowed_return(Ast::nodes),
    },
});

userdata!(Block<'static>, {
    properties: {
    	lastStmt: optional(Block::last_stmt),
    },
});

userdata!(Return<'static>, {
    properties: {
    	token: borrowed_return(Return::token),
    },
});

userdata!(TokenReference<'static>, {});

enum_userdata!(LastStmt<'static>, {
	LastStmt::Break => decow_token_reference,
	LastStmt::Return,
});

pub struct LuaApi;

impl rlua::UserData for LuaApi {
    fn add_methods<'lua, M: rlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_function("parse", |_, source: String| {
            return crate::parse(&source)
                .map(|ast| ast.owned())
                .map_err(|error| rlua::Error::external(error.owned()));
        });
    }
}
