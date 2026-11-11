#![no_main]

use full_moon::{node::Node, parse_fallible, LuaVersion};
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(string) = std::str::from_utf8(data) {
        // The recovered tree counts too: it is the branch most fuzz inputs reach.
        let result = parse_fallible(string, LuaVersion::new());
        let ast = result.ast();

        let tokens: String = ast.tokens().map(|token| token.to_string()).collect();
        assert_eq!(tokens, ast.to_string());
    }
});
