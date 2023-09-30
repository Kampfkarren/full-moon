use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};
use full_moon::{
    ast::{AstResult, LuaVersion},
    tokenizer::{self, LexerResult},
};
use insta::{assert_display_snapshot, assert_yaml_snapshot};
use std::{fs, path::Path};

mod common;
use common::run_test_folder;

fn process_fail_case(path: &Path, source: &str, lua_version: LuaVersion) {
    let result = full_moon::parse_fallible(source, lua_version);

    if result.errors.is_empty() {
        panic!("fail case passed for {path:?}");
    }

    assert_yaml_snapshot!("errors", result.errors);
    assert_yaml_snapshot!("ast", result.ast);

    process_codespan_display(source, &result);

    let ast = result.ast.update_positions();
    assert_yaml_snapshot!("ast_to_string", full_moon::print(&ast));
}

fn process_codespan_display(source: &str, result: &AstResult) {
    let mut files = SimpleFiles::new();

    let file_id = files.add("source.lua", source);

    let config = codespan_reporting::term::Config::default();
    let mut output = termcolor::NoColor::new(Vec::new());

    for error in &result.errors {
        let range = error.range();

        let diagnostic = Diagnostic::error()
            .with_message(error.error_message())
            .with_code(match error {
                full_moon::Error::AstError(_) => "ast",
                full_moon::Error::TokenizerError(_) => "tokenizer",
            })
            .with_labels(vec![Label::primary(
                file_id,
                (range.0.bytes())..(range.1.bytes()),
            )]);

        codespan_reporting::term::emit(&mut output, &config, &files, &diagnostic).unwrap();
    }

    assert_display_snapshot!(
        "error_display",
        String::from_utf8(output.into_inner()).unwrap()
    );
}

fn run_parser_fail_cases(folder: &str, lua_version: LuaVersion) {
    run_test_folder(folder, |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");
        let tokens = tokenizer::Lexer::new(&source, lua_version)
            .collect()
            .unwrap();
        assert_yaml_snapshot!("tokens", tokens);

        process_fail_case(path, &source, lua_version);
    });
}

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_parser_fail_cases() {
    run_parser_fail_cases("./tests/cases/fail/parser", LuaVersion::lua51());
}

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_tokenizer_fail_cases() {
    run_test_folder("./tests/cases/fail/tokenizer", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::Lexer::new(&source, LuaVersion::lua51()).collect();
        assert!(!matches!(tokens, LexerResult::Ok(_)));
        assert_yaml_snapshot!("tokens_result", tokens);

        process_fail_case(path, &source, LuaVersion::lua51());
    });
}

#[test]
#[cfg(feature = "roblox")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_parser_fail_cases() {
    run_parser_fail_cases("./tests/roblox_cases/fail/parser", LuaVersion::luau());
}

#[test]
#[cfg(feature = "roblox")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_tokenizer_fail_cases() {
    run_test_folder("./tests/roblox_cases/fail/tokenizer", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::Lexer::new(&source, LuaVersion::luau()).collect();
        assert!(!matches!(tokens, LexerResult::Ok(_)));
        assert_yaml_snapshot!("tokens_result", tokens);

        process_fail_case(path, &source, LuaVersion::luau());
    })
}

#[test]
#[cfg(feature = "lua52")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua52_parser_fail_cases() {
    run_parser_fail_cases("./tests/lua52_cases/fail/parser", LuaVersion::lua52());
}

#[test]
#[cfg(feature = "lua53")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua53_parser_fail_cases() {
    run_parser_fail_cases("./tests/lua53_cases/fail/parser", LuaVersion::lua53());
}

#[test]
#[cfg(feature = "lua54")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua54_parser_fail_cases() {
    run_parser_fail_cases("./tests/lua54_cases/fail/parser", LuaVersion::lua54());
}
