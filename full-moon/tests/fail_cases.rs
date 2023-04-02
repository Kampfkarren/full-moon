use full_moon::{ast, tokenizer};
use insta::assert_yaml_snapshot;
use std::fs;

mod common;
use common::run_test_folder;

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_parser_fail_cases() {
    run_test_folder("./tests/cases/fail/parser", |path| {
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::Lexer::new(&source)
            .collect::<Result<Vec<_>, _>>()
            .expect("couldn't tokenize");

        assert_yaml_snapshot!("tokens", tokens);

        match full_moon::parse(&source) {
            Ok(_) => panic!("fail case passed for {path:?}"),
            Err(error) => {
                println!("error {error:#?}");
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

        match tokenizer::Lexer::new(&source).collect::<Result<Vec<_>, _>>() {
            Ok(tokens) => panic!("fail case passed for {path:?}\n{tokens:#?}"),
            Err(error) => {
                assert_yaml_snapshot!("error", error);
            }
        }
    })
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
