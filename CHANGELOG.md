# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
### Added
- Added support for exporting types (`export type Foo = { bar: number }`) under the `roblox` feature flag
- Added support for using types from other modules (`local x: module.Foo`) under the `roblox` feature flag

### Fixed
- Fixed type declaration of objects not supporting trailing commas
- Fixed an issue where `continue` was not treated similar to `return` or `break`. It is now moved to `LastStmt` instead of a `Stmt` 
- Fixed long comments and long strings containing multi-byte characters.

## [0.6.2] - 2020-07-11
### Fixed
- Fixed an error related with `visit_compound_op` and the `roblox` feature flag

## [0.6.1] - 2020-07-05
### Fixed
- Fixed `visit_un_op` not being called correctly

## [0.6.0] - 2020-06-02
### Added
- Added support for `continue` under `roblox` feature flag
- Added support for compound assignments (eg. `x += 1`) under the `roblox` feature flag
- Added support for intersectional types (`string & number`) under the `roblox` feature flag
- Added support for underscores as numeric seperators (eg. `1_123_531`) under the `roblox` feature flag

### Fixed
- Fixed old function return type syntax under `roblox` feature flag
- Fixed `leading_trivia` not being populated correctly within `TokenReference`s inside `extract_token_references` due to CRLF line endings
- Fixed parse error for numbers with negative exponents
- Fixed parse error for fractional numbers with no integer part

## [0.5.0] - 2020-04-21
### Added
- Added `TokenReference::with_token`
- Added `Token::new`
- Added `Puncutated::from_iter`
- Added `TokenType::spaces` and `TokenType::tabs`
- Added `Punctuated::last`
- Display is now implemented on all nodes (Closes #26)

### Changed
- TokenReference has been completely rewritten to now be a representation of an inner token and its leading/trailing trivia.
- `ignore` is now deprecated in favor of `is_trivia`
- `last_stmts` is now deprecated in favor of `last_stmt`

### Fixed
- Fixed a bug where illogical punctuated sequences were created when function bodies had both named parameters and varargs

## [0.4.0-rc.14] - 2020-01-27
### Fixed
- Fixed serde being used even when the `serde` feature flag was not active
- Fixed tabs in comments leading to a parser error.

## [0.4.0-rc.13] - 2020-01-18
### Added
- Added support for Roblox typed Lua syntax under the `roblox` feature flag

### Changed
- Changed internal parser which should lead to better performance

## [0.4.0-rc.12] - 2019-11-13
### Added
- Added a `roblox` feature flag for Roblox specific syntax
- Added binary literals when using `roblox` feature flag

### Fixed
- Fixed strings with escaped new lines being unparseable

### Removed
- Removed leftover `dbg!` line

## [0.4.0-rc.11] - 2019-10-27
### Fixed
- Fixed a bug where subtraction without spaces and the right hand side being a number would cause a parsing error (e.g. `x-7`)

## [0.4.0-rc.10] - 2019-10-20
### Added
- Added a `similar` method to `node::Node` to check if two nodes are semantically equivalent

## [0.4.0-rc.9] - 2019-10-11
### Fixed
- Fixed performance issue with `Node::start_position` and `Node::end_position`

## [0.4.0-rc.8] - 2019-10-07
### Removed
- Removed dependency on crossbeam.

## [0.4.0-rc.7] - 2019-10-03
### Added
- Added `visit_XXX_end` methods for when completing a visit on a node

## [0.4.0-rc.6] - 2019-10-01
### Fixed
- Fixed unexpected parsing issues with UTF-8 strings

## [0.4.0-rc.5] - 2019-09-30
### Fixed
- Fixed tokens not being visited for `TokenReference`

## [0.4.0-rc.4] - 2019-09-30
### Fixed
- Fixed a massive performance bug
- Fixed `visit_ast` not following tokens

## [0.4.0-rc.3] - 2019-09-11
### Added
- Added `Owned` implementation for errors

## [0.4.0-rc.2] - 2019-09-11
### Changed
- Changed signature of `Node::surrounding_ignore_tokens` to have more lenient lifetimes

## [0.4.0-rc.1] - 2019-09-08
### Added
- Added `node::Node` which contains `start_position` and `end_position` methods to obtain the full range of a node, as well as `surrounding_ignore_tokens` to get surrounding comments/whitespace.
- Added `Punctuated` and `Pair`, replaced lots of uses of `Vec<_>` with `Punctuated<_>`
- Added `ContainedSpan`, a way to represent structures like `(...)` and `[...]`
- Added `Return` and `visit_return`
- Added `Expression::Parentheses`
- Added `Owned` trait to get a `'static` lifetime version of nodes
- Added `TokenKind` to get the kind of a token without any additional data
- Added mutation methods: `set_start_position`, `set_end_position`, `set_token_type` for `TokenReference` objects
- Added `Ast::update_positions` to update all the position structs if you mutate it

### Changed
- Fields of `Token` and `Position` have been made private with public accessors
- Changed signatures from `Token` to `TokenReference`, which dereference to tokens
- Changed `If::else_if` to use a new `Vec<ElseIf>`
- Changed `Value::Function` to also include the function token
- `LastStmt::Return` no longer uses an enum struct, and now uses `Return`
- Changed `visit_do` to use a new `Do` struct instead of `Block`
- Changed `FunctionArgs::Parentheses` to be a struct item

### Fixed
- Fixed unexpected parsing issues with UTF-8 strings

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
