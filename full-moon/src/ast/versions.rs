#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Debug)]
pub enum LuaVersion {
    Lua51,

    #[cfg(feature = "roblox")]
    Luau,
}
