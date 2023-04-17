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

fn process_fail_case(path: &Path, source: &str) {
    let result = full_moon::parse_fallible(source);

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

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_parser_fail_cases() {
    run_test_folder("./tests/cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");
        let tokens = tokenizer::Lexer::new(&source, LuaVersion::Lua51)
            .collect()
            .unwrap();
        assert_yaml_snapshot!("tokens", tokens);

        process_fail_case(path, &source);
    });
}

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_tokenizer_fail_cases() {
    run_test_folder("./tests/cases/fail/tokenizer", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::Lexer::new(&source, LuaVersion::Lua51).collect();
        assert!(!matches!(tokens, LexerResult::Ok(_)));
        assert_yaml_snapshot!("tokens_result", tokens);

        process_fail_case(path, &source);
    });
}

#[test]
#[cfg(feature = "roblox")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_parser_fail_cases() {
    run_test_folder("./tests/roblox_cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        assert_yaml_snapshot!("tokens", tokens);

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                assert_yaml_snapshot!("error", error);
            }
        }
    })
}

#[test]
#[cfg(feature = "roblox")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_tokenizer_fail_cases() {
    run_test_folder("./tests/roblox_cases/fail/tokenizer", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        match tokenizer::tokens(&source) {
            Ok(tokens) => panic!("fail case passed for {:?}\n{tokens:#?}", path),
            Err(error) => {
                assert_yaml_snapshot!("error", error);
            }
        }
    })
}

#[test]
#[cfg(feature = "lua52")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua52_parser_fail_cases() {
    run_test_folder("./tests/lua52_cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        assert_yaml_snapshot!("tokens", tokens);

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                assert_yaml_snapshot!("error", error);
            }
        }
    })
}

#[test]
#[cfg(feature = "lua53")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua53_parser_fail_cases() {
    run_test_folder("./tests/lua53_cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        assert_yaml_snapshot!("tokens", tokens);

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                assert_yaml_snapshot!("error", error);
            }
        }
    })
}

#[test]
#[cfg(feature = "lua54")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua54_parser_fail_cases() {
    run_test_folder("./tests/lua54_cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        assert_yaml_snapshot!("tokens", tokens);

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                assert_yaml_snapshot!("error", error);
            }
        }
    })
}
