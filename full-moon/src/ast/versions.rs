// Lua version is handled as a bitfield to support parsing as many languages as possible at once.
// Any new language added does not necessarily need to (or should be) added to the default set.
const VERSION_LUAU: u8 = 1 << 0;
const VERSION_LUA52: u8 = 1 << 1;
const VERSION_LUA53: u8 = 1 << 2;
const VERSION_LUA54: u8 = 1 << 3;

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
    pub fn luau() -> Self {
        Self {
            bitfield: VERSION_LUAU,
        }
    }

    #[cfg(feature = "roblox")]
    pub fn with_luau(self) -> Self {
        Self {
            bitfield: self.bitfield | VERSION_LUAU,
        }
    }

    pub fn has_luau(self) -> bool {
        cfg!(feature = "roblox") && (self.bitfield & VERSION_LUAU != 0)
    }

    #[cfg(feature = "lua52")]
    pub fn lua52() -> Self {
        Self {
            bitfield: VERSION_LUA52,
        }
    }

    #[cfg(feature = "lua52")]
    pub fn with_lua52(self) -> Self {
        self | Self::lua52()
    }

    pub fn has_lua52(self) -> bool {
        cfg!(feature = "lua52") && (self.bitfield & VERSION_LUA52 != 0)
    }

    #[cfg(feature = "lua53")]
    pub fn lua53() -> Self {
        Self {
            bitfield: VERSION_LUA52 | VERSION_LUA53,
        }
    }

    #[cfg(feature = "lua53")]
    pub fn with_lua53(self) -> Self {
        self | Self::lua53()
    }

    pub fn has_lua53(self) -> bool {
        cfg!(feature = "lua53") && (self.bitfield & VERSION_LUA53 != 0)
    }

    #[cfg(feature = "lua54")]
    pub fn lua54() -> Self {
        Self {
            bitfield: VERSION_LUA52 | VERSION_LUA53 | VERSION_LUA54,
        }
    }

    #[cfg(feature = "lua54")]
    pub fn with_lua54(self) -> Self {
        self | Self::lua54()
    }

    pub fn has_lua54(self) -> bool {
        cfg!(feature = "lua54") && (self.bitfield & VERSION_LUA54 != 0)
    }
}

impl Default for LuaVersion {
    fn default() -> Self {
        Self {
            bitfield: VERSION_LUAU | VERSION_LUA52 | VERSION_LUA53,
        }
    }
}

impl std::ops::BitOr for LuaVersion {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self::Output {
        Self {
            bitfield: self.bitfield | rhs.bitfield,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lua51_sanity() {
        assert!(!LuaVersion::lua51().has_lua52());
        assert!(!LuaVersion::lua51().has_lua53());
    }
}
