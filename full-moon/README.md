<div align="center">
	<h1>
		Full Moon<br>
		<a href="https://crates.io/crates/full-moon"><img src="https://img.shields.io/crates/v/full-moon.svg"></a>
	</h1>
</div>

A lossless Lua 5.1 parser written in Rust.


## Lossless?

Full Moon preserves comments, whitespace, style choices, etc. With Full Moon, you're able to convert your Lua code into an AST and a syntax tree and convert it back to the original code exactly.

Using Full Moon, you'll be able to modify the AST directly and re-export it back to Lua, all while preserving the style in which you write.


## Why?

Full Moon is usable in projects such as:
- Static analysis (like Luacheck or [rust-clippy](https://github.com/rust-lang/rust-clippy))
- Static typing (like TypeScript)
- Automatic mass code refactoring (like [jscodeshift](https://github.com/facebook/jscodeshift))
- Automatic code formatting (like [rustfmt](https://github.com/rust-lang/rustfmt))
- An [LSP Server](https://microsoft.github.io/language-server-protocol/) for Lua

## Inspiration

Full Moon is heavily inspired by [LPGhatguy's mab](https://github.com/LPGhatguy/mab/) and by the possibilities brought on by [benjamn's recast](https://github.com/benjamn/recast).
