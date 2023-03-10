# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]

### Changed

- Use newer langkit version

## [0.13.0] - 2022-09-19

### Removed

- Session aspects
- Null states
- `NullID`

## [0.12.0] - 2022-09-05

### Removed

- Support for `private` types

## [0.11.0] - 2022-06-10

### Added

- Message field assignments
- Case expressions
- Changelog

## [0.10.0] - 2021-11-23

### Added

- `Byte_Order` aspect

### Changed

- Enable parallel build
- Enable static build
- Enable Ada API

## [0.9.2] - 2021-11-24

### Changed

- Make install PEP 517 compliant to avoid fallback

## [0.9.1] - 2021-11-10

### Changed

- Generate GPR file

## [0.9.0] - 2021-10-08

### Added

- Type hints
- Document process of updating `librflxlang.gpr`

### Changed

- Rename message components to fields

## [0.8.1] - 2021-10-05

### Added

- Build of source package

### Changed

- Rename package to `RecordFlux-parser`

## [0.8.0] - 2021-09-22

### Changed

- Rename `then` keyword to `goto` in state transitions

## [0.7.0] - 2021-09-07

### Added

- Parameterized message type declarations
- Parameterized messages as field types
- Message parameters in `Reset` statement

## [0.6.0] - 2021-07-28

### Changed

- Rename `when` keyword to `if` in list comprehension
- Change position of filter expression in list comprehensions

## [0.5.1] - 2021-06-05

### Changed

- Allow reserved words as identifiers

## [0.5.0] - 2021-05-25

### Added

- `Has_Data` attribute

### Changed

- Rename list attribute to attribute statement

## [0.4.0] - 2021-05-12

### Removed

- `array` keyword

## [0.3.0] - 2021-05-10

### Added

- `sequence` keyword

### Changed

- Rename arrays to sequences

## [0.2.1] - 2021-04-30

### Changed

- Use newer langkit version
- Generate single shared lib
- Include Ada libs statically in parser shared object
- Set version in generated parser

## [0.2.0] - 2021-03-09

### Added

- Exception transitions in session states

## [0.1.0] - 2021-01-25

### Added

- Initial version of parser

[Unreleased]: https://github.com/Componolit/RecordFlux-parser/compare/v0.11.0...HEAD
[0.11.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.10.0...v0.11.0
[0.10.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.9.2...v0.10.0
[0.9.2]: https://github.com/Componolit/RecordFlux-parser/compare/v0.9.1...v0.9.2
[0.9.1]: https://github.com/Componolit/RecordFlux-parser/compare/v0.9.0...v0.9.1
[0.9.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.8.1...v0.9.0
[0.8.1]: https://github.com/Componolit/RecordFlux-parser/compare/v0.8.0...v0.8.1
[0.8.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.7.0...v0.8.0
[0.7.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.5.1...v0.6.0
[0.5.1]: https://github.com/Componolit/RecordFlux-parser/compare/v0.5.0...v0.5.1
[0.5.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.4.0...v0.5.0
[0.4.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.2.1...v0.3.0
[0.2.1]: https://github.com/Componolit/RecordFlux-parser/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/Componolit/RecordFlux-parser/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/Componolit/RecordFlux-parser/releases/tag/v0.1.0
