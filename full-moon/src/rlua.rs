use crate::{
    ast::{owned::Owned, *},
    tokenizer::*,
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

				methods.add_method("print", |_, this, _: ()| {
					Ok(this.print())
				});

				methods.add_meta_function(
					rlua::MetaMethod::ToString,
					|_, _: ()| -> rlua::Result<String> {
						Ok(stringify!($object)
							.replace("<'static>", "")
							.to_owned()
						)
					}
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

fn collect_iterator<'a, 'lua, T, U, I, F>(
    function: F,
) -> Box<dyn FnOnce(rlua::Context<'lua>, T) -> rlua::Result<rlua::Table<'lua>>>
where
    U: 'a + Clone + rlua::ToLua<'lua>,
    I: Iterator<Item = &'a U>,
    F: FnOnce(T) -> I + 'static,
{
    return Box::new(|lua, this| {
        let array = lua.create_table()?;
        for (index, item) in function(this).cloned().enumerate() {
            array.set(index + 1, item.to_lua(lua)?)?;
        }
        Ok(array)
    });
}

fn owned_return<'lua, T, U, F>(
    function: F,
) -> Box<dyn FnOnce(rlua::Context<'lua>, T) -> rlua::Result<rlua::Value<'lua>>>
where
    U: rlua::ToLua<'lua>,
    F: FnOnce(T) -> U + 'static,
{
    return Box::new(|lua, this| function(this).to_lua(lua));
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

impl rlua::UserData for Position {
    fn add_methods<'lua, M: rlua::UserDataMethods<'lua, Self>>(methods: &mut M) {
        methods.add_meta_method(
            rlua::MetaMethod::Index,
            |lua, this, key: String| -> rlua::Result<rlua::Value<'lua>> {
                match key.as_str() {
                    "bytes" => this.bytes().to_lua(lua),
                    "character" => this.character().to_lua(lua),
                    "line" => this.line().to_lua(lua),
                    key => {
                        return Err(rlua::Error::external(format!(
                            "{} is not a valid property",
                            key
                        )));
                    }
                }
            },
        );
    }
}

trait LuaPrint {
    fn print(&self) -> String;
}

impl<T: std::fmt::Display> LuaPrint for T {
    fn print(&self) -> String {
        self.to_string()
    }
}

impl LuaPrint for Ast<'static> {
    fn print(&self) -> String {
        self.nodes().to_string()
    }
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

userdata!(Token<'static>, {
	properties: {
		end_position: owned_return(Token::end_position),
		start_position: owned_return(Token::start_position),
	},
});

userdata!(TokenReference<'static>, {
	properties: {
		leading_trivia: collect_iterator(TokenReference::leading_trivia),
		token: borrowed_return(TokenReference::token),
		trailing_trivia: collect_iterator(TokenReference::trailing_trivia),
	},
});

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
