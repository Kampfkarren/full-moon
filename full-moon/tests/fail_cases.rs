use full_moon::{ast, tokenizer};
use insta::assert_yaml_snapshot;
use std::fs;

mod common;
use common::run_test_folder;

fn unpack_token_reference<'a>(token: &tokenizer::TokenReference<'a>) -> Vec<tokenizer::Token<'a>> {
    token
        .leading_trivia()
        .chain(std::iter::once(token.token()))
        .chain(token.trailing_trivia())
        .cloned()
        .collect()
}

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_parser_fail_cases() {
    run_test_folder("./tests/cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        assert_yaml_snapshot!(
            "tokens",
            tokens
                .iter()
                .flat_map(unpack_token_reference)
                .collect::<Vec<_>>()
        );

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
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_tokenizer_fail_cases() {
    run_test_folder("./tests/cases/fail/tokenizer", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        match tokenizer::tokens(&source) {
            Ok(_) => panic!("fail case passed for {:?}", path),
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

        assert_yaml_snapshot!(
            "tokens",
            tokens
                .iter()
                .flat_map(unpack_token_reference)
                .collect::<Vec<_>>()
        );

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                assert_yaml_snapshot!("error", error);
            }
        }
    })
}
