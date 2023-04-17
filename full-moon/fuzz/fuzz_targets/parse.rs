#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(string) = std::str::from_utf8(data) {
        full_moon::parse_fallible(string, full_moon::LuaVersion::new());
    }
});
