// Lua version is handled as a bitfield to support parsing as many languages as possible at once.
// Any new language added does not necessarily need to (or should be) added to the default set.
const VERSION_LUAU: u8 = 1 << 0;
const VERSION_LUA52: u8 = 1 << 1;
const VERSION_LUA53: u8 = 1 << 2;
const VERSION_LUA54: u8 = 1 << 3;
const VERSION_LUAJIT: u8 = 1 << 4;

/// Represents the Lua version(s) to parse as.
/// Lua 5.1 is always included.
/// In order to get more Lua versions, you must include their respective features.
#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct LuaVersion {
    bitfield: u8,
}

impl LuaVersion {
    /// Creates a new LuaVersion with the default features: Luau, Lua 5.2, Lua 5.3, and Lua 5.4.
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates a new LuaVersion with only Lua 5.1.
    pub fn lua51() -> Self {
        Self { bitfield: 0 }
    }

    /// Creates a new LuaVersion with only Luau.
    #[cfg(feature = "luau")]
    pub fn luau() -> Self {
        Self {
            bitfield: VERSION_LUAU,
        }
    }

    /// Adds Luau as a version to parse for.
    #[cfg(feature = "luau")]
    pub fn with_luau(self) -> Self {
        Self {
            bitfield: self.bitfield | VERSION_LUAU,
        }
    }

    /// Returns true if Luau is enabled.
    pub fn has_luau(self) -> bool {
        cfg!(feature = "luau") && (self.bitfield & VERSION_LUAU != 0)
    }

    /// Creates a new LuaVersion with only Lua 5.2.
    #[cfg(feature = "lua52")]
    pub fn lua52() -> Self {
        Self {
            bitfield: VERSION_LUA52,
        }
    }

    /// Adds Lua 5.2 as a version to parse for.
    #[cfg(feature = "lua52")]
    pub fn with_lua52(self) -> Self {
        self | Self::lua52()
    }

    /// Returns true if Lua 5.2 is enabled.
    pub fn has_lua52(self) -> bool {
        cfg!(feature = "lua52") && (self.bitfield & VERSION_LUA52 != 0)
    }

    /// Creates a new LuaVersion with only Lua 5.3.
    #[cfg(feature = "lua53")]
    pub fn lua53() -> Self {
        Self {
            bitfield: VERSION_LUA52 | VERSION_LUA53,
        }
    }

    /// Adds Lua 5.3 as a version to parse for.
    #[cfg(feature = "lua53")]
    pub fn with_lua53(self) -> Self {
        self | Self::lua53()
    }

    /// Returns true if Lua 5.3 is enabled.
    pub fn has_lua53(self) -> bool {
        cfg!(feature = "lua53") && (self.bitfield & VERSION_LUA53 != 0)
    }

    /// Creates a new LuaVersion with only Lua 5.4.
    #[cfg(feature = "lua54")]
    pub fn lua54() -> Self {
        Self {
            bitfield: VERSION_LUA52 | VERSION_LUA53 | VERSION_LUA54,
        }
    }

    /// Adds Lua 5.4 as a version to parse for.
    #[cfg(feature = "lua54")]
    pub fn with_lua54(self) -> Self {
        self | Self::lua54()
    }

    /// Returns true if Lua 5.4 is enabled.
    pub fn has_lua54(self) -> bool {
        cfg!(feature = "lua54") && (self.bitfield & VERSION_LUA54 != 0)
    }

    /// Creates a new LuaVersion with only LuaJIT.
    #[cfg(feature = "luajit")]
    pub fn luajit() -> Self {
        Self {
            bitfield: VERSION_LUAJIT,
        }
    }

    /// Adds LuaJIT as a version to parse for.
    #[cfg(feature = "luajit")]
    pub fn with_luajit(self) -> Self {
        self | Self::luajit()
    }

    /// Returns true if LuaJIT is enabled.
    pub fn has_luajit(self) -> bool {
        cfg!(feature = "luajit") && (self.bitfield & VERSION_LUAJIT != 0)
    }
}

impl Default for LuaVersion {
    fn default() -> Self {
        Self {
            bitfield: VERSION_LUAU | VERSION_LUA52 | VERSION_LUA53 | VERSION_LUA54 | VERSION_LUAJIT,
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
