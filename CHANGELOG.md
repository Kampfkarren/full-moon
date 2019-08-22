# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## Unreleased
### Added
- Added `TokenKind` to get the kind of a token without any additional data
- Added mutation methods: `set_start_position`, `set_end_position`, `set_token_type` for `TokenReference` objects
- Added `Ast::update_positions` to update all the position structs if you mutate it
- Added `node::Node` which contains `start_position` and `end_position` methods to obtain the full range of a node, as well as `surrounding_ignore_tokens` to get surrounding comments/whitespace.
- Added `Punctuated` and `Pair`, replaced lots of uses of `Vec<_>` with `Punctuated<_>`
- Added `ContainedSpan`, a way to represent structures like `(...)` and `[...]`
- Added `Return` and `visit_return`
- Added `Expression::Parentheses`
- Added `Owned` trait to get a `'static` lifetime version of nodes

### Changed
- Fields of `Token` and `Position` have been made private with public accessors
- Changed signatures from `Token` to `TokenReference`, which dereference to tokens
- Changed `visit_do` to use a new `Do` struct instead of `Block`
- Changed `FunctionArgs::Parentheses` to be a struct item
- `LastStmt::Return` no longer uses an enum struct, and now uses `Return`
- Changed `If::else_if` to use a new `Vec<ElseIf>`
- Changed `Value::Function` to also include the function token

## [0.3.0] - 2019-05-24
### Added
- Added std::fmt::Display and std::error::Error implementations for Error, AstError, and TokenizerError
- Documented all public APIs

### Changed
- Properties are no longer `pub` and now have public accessor methods
- License has been changed from GPL to LGPL

### Fixed
- Fixed `TableConstructorField` parsing not correctly giving separator tokens ([#12](https://github.com/Kampfkarren/full-moon/issues/12))

## [0.2.0] - 2019-05-23
### Added
- Added VisitorMut trait which is passed mutable nodes. Mutation is not completely ready yet and may cause side effects
- Added Visit and VisitMut traits that all nodes implement, as well as some utility types such as Vec<T>

### Removed
- Removed Interpreter in favor of using the Visitor trait directly
- Removed "visitors" feature flag

### Changed
- BinOpRhs is no longer a type alias to a tuple and is instead its own struct

## [0.1.2] - 2019-04-05
### Added
- Added documentation to visitors::Visitor::Interpreter

## [0.1.1] - 2019-04-05
### Fixed
- visitors::Interpreter's constructor is no longer accidentally private

## [0.1.0] - 2019-04-04
### Added
- Initial commit
- Tokenizer and parser
- Visitors