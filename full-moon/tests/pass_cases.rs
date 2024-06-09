use full_moon::{
    ast::LuaVersion,
    node::Node,
    print,
    tokenizer::{self, Token, TokenReference},
};
use insta::assert_yaml_snapshot;
use pretty_assertions::assert_eq;
use std::{fmt, fs, path::Path};

mod common;
use common::run_test_folder;

#[derive(PartialEq, Eq)]
struct PrettyString<'a>(pub &'a str);

// Make diff to display string as multi-line string
impl<'a> fmt::Debug for PrettyString<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.0)
    }
}

fn unpack_token_reference(token: &TokenReference) -> Vec<Token> {
    token
        .leading_trivia()
        .chain(std::iter::once(token.token()))
        .chain(token.trailing_trivia())
        .cloned()
        .collect()
}

fn test_pass_case(path: &Path, lua_version: LuaVersion) {
    let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

    let tokens = tokenizer::Lexer::new(&source, lua_version)
        .collect()
        .unwrap();

    assert_yaml_snapshot!("tokens", tokens);

    let ast = full_moon::parse_fallible(&source, lua_version)
        .into_result()
        .unwrap_or_else(|error| panic!("couldn't make ast for {path:?} - {error:#?}"));

    let old_positions: Vec<_> = ast.tokens().flat_map(unpack_token_reference).collect();

    assert_yaml_snapshot!("ast", ast.nodes());
    assert_eq!(PrettyString(&print(&ast)), PrettyString(&source));

    let ast = ast.update_positions();
    assert_eq!(
        old_positions,
        ast.tokens()
            .flat_map(unpack_token_reference)
            .collect::<Vec<_>>(),
    );
}

#[test]
#[cfg(not(feature = "luau"))] // exclude extra nodes added to yaml
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_pass_cases() {
    run_test_folder("./tests/cases/pass", |path| {
        test_pass_case(path, LuaVersion::lua51())
    });
}

#[test]
#[cfg(feature = "luau")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_pass_cases() {
    run_test_folder("./tests/roblox_cases/pass", |path| {
        test_pass_case(path, LuaVersion::luau())
    });
}

#[test]
#[cfg(feature = "lua52")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua52_pass_cases() {
    run_test_folder("./tests/lua52_cases/pass", |path| {
        test_pass_case(path, LuaVersion::lua52())
    });
}

#[test]
#[cfg(feature = "lua53")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua53_pass_cases() {
    run_test_folder("./tests/lua53_cases/pass", |path| {
        test_pass_case(path, LuaVersion::lua53())
    });
}

#[test]
#[cfg(feature = "lua54")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua54_pass_cases() {
    run_test_folder("./tests/lua54_cases/pass", |path| {
        test_pass_case(path, LuaVersion::lua54())
    });
}

#[test]
#[cfg(feature = "luajit")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_luajit_pass_cases() {
    run_test_folder("./tests/luajit_cases/pass", |path| {
        test_pass_case(path, LuaVersion::luajit())
    });
}
