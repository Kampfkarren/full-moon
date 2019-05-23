# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

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