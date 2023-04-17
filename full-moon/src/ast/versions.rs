// Lua version is handled as a bitfield to support parsing as many languages as possible at once.
// Any new language added does not necessarily need to (or should be) added to the default set.
const VERSION_LUAU: u8 = 1 << 0;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LuaVersion {
    bitfield: u8,
}

impl LuaVersion {
    // rewrite todo: bad name?
    pub fn new() -> Self {
        Self::default()
    }

    pub fn lua51() -> Self {
        Self { bitfield: 0 }
    }

    #[cfg(feature = "roblox")]
    pub fn with_luau(self) -> Self {
        Self {
            bitfield: self.bitfield | VERSION_LUAU,
        }
    }

    #[cfg(feature = "roblox")]
    pub fn has_luau(self) -> bool {
        self.bitfield & VERSION_LUAU != 0
    }
}

impl Default for LuaVersion {
    fn default() -> Self {
        Self {
            bitfield: VERSION_LUAU,
        }
    }
}
