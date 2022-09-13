use proc_macro::TokenStream;

extern crate proc_macro;

mod lua_user_data;

#[proc_macro_derive(LuaUserData, attributes(lua))]
pub fn derive_lua_user_data(input: TokenStream) -> TokenStream {
    lua_user_data::derive(input)
}
