use full_moon::{
    ast,
    tokenizer::{self, Token},
};
use pretty_assertions::assert_eq;
use std::fs::{self, File};
use std::io::Write;

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_parser_fail_cases() {
    for entry in fs::read_dir("./tests/cases/fail/parser").expect("couldn't read directory") {
        let entry = entry.unwrap();
        let path = entry.path();
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        let tokens_path = path.join("tokens.json");
        let tokens_contents;

        if let Ok(tokens_contents_tmp) = fs::read_to_string(dbg!(&tokens_path)) {
            tokens_contents = tokens_contents_tmp;
            let expected_tokens: Vec<Token> =
                serde_json::from_str(&tokens_contents).expect("couldn't deserialize tokens file");
            assert_eq!(tokens, expected_tokens);
        } else {
            let mut file = File::create(&tokens_path).expect("couldn't write tokens file");
            file.write_all(
                serde_json::to_string_pretty(&tokens)
                    .expect("couldn't serialize")
                    .as_bytes(),
            )
            .expect("couldn't write to tokens file");
        }

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                let error_path = path.join("error.json");
                if let Ok(error_contents) = fs::read_to_string(&error_path) {
                    let expected_error = serde_json::from_str(&error_contents)
                        .expect("couldn't deserialize existing error file");
                    assert_eq!(error, expected_error);
                } else {
                    let mut file = File::create(&error_path).expect("couldn't write error file");
                    file.write_all(
                        serde_json::to_string_pretty(&error)
                            .expect("couldn't serialize")
                            .as_bytes(),
                    )
                    .expect("couldn't write to ast file");
                }
            }
        }
    }
}

#[test]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_tokenizer_fail_cases() {
    for entry in fs::read_dir("./tests/cases/fail/tokenizer").expect("couldn't read directory") {
        let entry = entry.unwrap();
        let path = entry.path();
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        match tokenizer::tokens(&source) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                let error_path = path.join("error.json");
                if let Ok(error_contents) = fs::read_to_string(&error_path) {
                    let expected_error = serde_json::from_str(&error_contents)
                        .expect("couldn't deserialize existing error file");
                    assert_eq!(error, expected_error);
                } else {
                    let mut file = File::create(&error_path).expect("couldn't write error file");
                    file.write_all(
                        serde_json::to_string_pretty(&error)
                            .expect("couldn't serialize")
                            .as_bytes(),
                    )
                    .expect("couldn't write to ast file");
                }
            }
        }
    }
}

#[test]
#[cfg(feature = "lua52")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_lua52_parser_fail_cases() {
    for entry in fs::read_dir("./tests/lua52_cases/fail/parser").expect("couldn't read directory") {
        let entry = entry.unwrap();
        let path = entry.path();
        let source = fs::read_to_string(path.join("source.lua")).expect("couldn't read source.lua");

        let tokens = tokenizer::tokens(&source).expect("couldn't tokenize");

        let tokens_path = path.join("tokens.json");
        let tokens_contents;

        if let Ok(tokens_contents_tmp) = fs::read_to_string(dbg!(&tokens_path)) {
            tokens_contents = tokens_contents_tmp;
            let expected_tokens: Vec<Token> =
                serde_json::from_str(&tokens_contents).expect("couldn't deserialize tokens file");
            assert_eq!(tokens, expected_tokens);
        } else {
            let mut file = File::create(&tokens_path).expect("couldn't write tokens file");
            file.write_all(
                serde_json::to_string_pretty(&tokens)
                    .expect("couldn't serialize")
                    .as_bytes(),
            )
            .expect("couldn't write to tokens file");
        }

        match ast::Ast::from_tokens(tokens) {
            Ok(_) => panic!("fail case passed for {:?}", path),
            Err(error) => {
                println!("error {:#?}", error);
                let error_path = path.join("error.json");
                if let Ok(error_contents) = fs::read_to_string(&error_path) {
                    let expected_error = serde_json::from_str(&error_contents)
                        .expect("couldn't deserialize existing error file");
                    assert_eq!(error, expected_error);
                } else {
                    let mut file = File::create(&error_path).expect("couldn't write error file");
                    file.write_all(
                        serde_json::to_string_pretty(&error)
                            .expect("couldn't serialize")
                            .as_bytes(),
                    )
                    .expect("couldn't write to ast file");
                }
            }
        }
    }
}
