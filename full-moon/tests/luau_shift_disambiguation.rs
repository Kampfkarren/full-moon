//! Regression tests for Luau type-instantiation / nested-generic syntax
//! interacting with Lua 5.3+ shift operators when both feature flags are
//! active. The lexer suppresses `<<`/`>>` combining when Luau is part of
//! the runtime `LuaVersion`, so blended-dialect callers can parse
//! `f<<T>>(x)` and `Map<K, Vec<V>>` correctly.

#![cfg(all(feature = "luau", feature = "lua53"))]

use full_moon::{ast::LuaVersion, parse_fallible};

fn assert_roundtrips(label: &str, source: &str, version: LuaVersion) {
    let result = parse_fallible(source, version);
    assert!(
        result.errors().is_empty(),
        "[{label}] parse errored: {:#?}",
        result.errors()
    );
    let printed = result.ast().to_string();
    assert_eq!(printed, source, "[{label}] roundtrip mismatch");
}

#[test]
fn type_instantiation_with_lua53() {
    let src = "f<<T>>(x)\n";
    assert_roundtrips(
        "luau+lua53 type instantiation",
        src,
        LuaVersion::luau().with_lua53(),
    );
}

#[test]
#[cfg(feature = "lua54")]
fn type_instantiation_with_lua54() {
    let src = "local _ = f<<T>>(x)\n";
    assert_roundtrips(
        "luau+lua54 type instantiation",
        src,
        LuaVersion::luau().with_lua54(),
    );
}

#[test]
fn method_type_instantiation_with_lua53() {
    let src = "a:method<<T>>(1)\n";
    assert_roundtrips(
        "luau+lua53 method type instantiation",
        src,
        LuaVersion::luau().with_lua53(),
    );
}

#[test]
fn nested_generic_close_with_lua53() {
    let src = "type Foo = Map<string, Array<number>>\n";
    assert_roundtrips(
        "luau+lua53 nested generic close",
        src,
        LuaVersion::luau().with_lua53(),
    );
}

#[test]
fn triple_nested_generic_close_with_lua53() {
    let src = "type X = A<B<C<D>>>\n";
    assert_roundtrips(
        "luau+lua53 triple-nested generic close",
        src,
        LuaVersion::luau().with_lua53(),
    );
}

/// Bitwise shifts still parse correctly when only `lua53` (no luau) is
/// part of the runtime `LuaVersion`.
#[test]
fn bitwise_shifts_still_parse_under_lua53_alone() {
    let src = "local z = a << b\n";
    assert_roundtrips("lua53 shift", src, LuaVersion::lua53());
}
