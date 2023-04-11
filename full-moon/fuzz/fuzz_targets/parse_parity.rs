#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    if let Ok(string) = std::str::from_utf8(data) {
        if old_full_moon::parse(string).is_ok() {
            full_moon::parse_fallible(dbg!(string));
        }
    }
});
