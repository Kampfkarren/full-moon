use full_moon::{
    ast, print,
    tokenizer::{self, Token},
};
use pretty_assertions::assert_eq;
use std::{
    fs::{self, File},
    io::Write,
    path::Path,
};

fn test_pass_cases_folder<P: AsRef<Path>>(folder: P) {
    for entry in fs::read_dir(folder).expect("couldn't read directory") {
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

        let mut ast = ast::Ast::from_tokens(tokens)
            .unwrap_or_else(|error| panic!("couldn't make ast for {:?} - {:?}", path, error));

        // let old_positions: Vec<_> = ast
        //     .iter_tokens()
        //     .map(|token| (token.start_position(), token.end_position()))
        //     .collect();
        // ast.update_positions();
        // assert_eq!(
        //     old_positions,
        //     ast.iter_tokens()
        //         .map(|token| (token.start_position(), token.end_position()))
        //         .collect::<Vec<_>>(),
        // );

        let ast_path = path.join("ast.json");

        if let Ok(ast_file) = fs::read_to_string(&ast_path) {
            let expected_ast =
                serde_json::from_str(&ast_file).expect("couldn't deserialize ast file");
            assert_eq!(ast.nodes(), &expected_ast);
            assert_eq!(print(&ast), source);
        } else {
            let mut file = File::create(&ast_path).expect("couldn't write ast file");
            file.write_all(
                serde_json::to_string_pretty(ast.nodes())
                    .expect("couldn't serialize")
                    .as_bytes(),
            )
            .expect("couldn't write to ast file");
            assert_eq!(print(&ast), source);
        }
    }
}

#[test]
#[cfg_attr(feature = "roblox", ignore)] // We don't want Roblox fields in JSON
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_pass_cases() {
    test_pass_cases_folder("./tests/cases/pass");
}

#[test]
#[cfg(feature = "roblox")]
#[cfg_attr(feature = "no-source-tests", ignore)]
fn test_roblox_pass_cases() {
    test_pass_cases_folder("./tests/roblox_cases/pass");
}
