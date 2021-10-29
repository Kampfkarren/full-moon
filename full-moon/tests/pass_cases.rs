use full_moon::{
    ast,
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

fn test_pass_case(path: &Path) {
    let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

    let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

    assert_yaml_snapshot!("tokens", tokens);

    let ast = ast::Ast::from_tokens(tokens)
        .unwrap_or_else(|error| panic!("couldn't make ast for {:?} - {:?}", path, error));

    let old_positions: Vec<_> = ast.tokens().flat_map(unpack_token_reference).collect();
    let ast = ast.update_positions();
    assert_eq!(
        old_positions,
        ast.tokens()
            .flat_map(unpack_token_reference)
            .collect::<Vec<_>>(),
    );

    assert_yaml_snapshot!("ast", ast.nodes());
    assert_eq!(PrettyString(&print(&ast)), PrettyString(&source));
}

#[test]
#[cfg_attr(feature = "roblox", ignore)] // We don't want Roblox fields in JSON
#[cfg_attr(feature = "lua52", ignore)] // Lua 5.2 collides with this implementation
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_pass_cases() {
    run_test_folder("./tests/cases/pass", test_pass_case);
}

#[test]
#[cfg(feature = "roblox")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_pass_cases() {
    run_test_folder("./tests/roblox_cases/pass", test_pass_case);
}

#[test]
#[cfg(feature = "lua52")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua52_pass_cases() {
    run_test_folder("./tests/lua52_cases/pass", test_pass_case);
}
