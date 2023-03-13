# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.9.0] - 2023-01-06

### Added

- Support for Python 3.11

### Removed

- Bindings (#724)

## [0.8.0] - 2022-12-02

### Changed

- Rename `Structural_Valid` to `Well_Formed` (#986)
- Reject statically true conditions in messages (#662)
- Reject statically false and true refinement conditions (#662)

### Removed

- Modular integer types (#727)

### Fixed

- Exception transition rejected on message assignment (#1144)
- Document where type derivations and sequence types are valid (#1235)

## [0.7.1] - 2022-11-04

### Fixed

- Exception when using a boolean value as condition (#776)

## [0.7.0] - 2022-10-04

### Added

CLI:

- `rflx setup_ide` subcommand for installing IDE integration (#795)
- `rflx` option `--unsafe` (#987)
- `rflx convert` subcommand for converting foreign specifications
- `rflx convert iana` subcommand for converting IANA "Service Name and Transport Protocol Port Number Registry" XML files (#708)

Model:

- Detection of unused parameters (#874)
- Detection of invalid use of literals in expressions (#686, #1194)

### Changed

Specification:

- Syntax for defining initial and final states of session (#700)

Model:

- Change representation of null messages (#643)

Generator:

- Style of Ada comments (#816)
- Detect when a generated file would overwrite an existing file (#993)
- Move operators and operations on types into separate child packages (#1126)

### Removed

Specification:

- Private types (#1156)

### Fixed

- Non-null state accepted as final state (#1130)
- Spurious error if providing specifications in certain order (#759)
- Handling of specification dependencies when using multiple directories

## [0.6.0] - 2022-08-31

### Added

- Parameterized messages (#609, #743)
- Endianness (#104, #914)
- Validator (#560)
- Parallelization of Z3 proofs and code generation (#625, #976)
- Simple specification style checker (#799)
- Integration files for defining buffer sizes of messages and sequences in sessions (#713)
- Memory management in sessions to avoid use of heap (#629)
- Setting of single message fields (#1067)
- Case expressions (#907)
- Optimization and support for Head attributes on list comprehensions (#1115)

Specification:

- Enable deactivation of style checks for individual files (#1079)

CLI:

- `rflx` option `--max-errors NUM` (#748)
- `rflx` option `--workers NUM` for setting the maximum number of parallel processes which are used for model verification (#755)
- `rflx generate` option `--integration-files-dir` (#713)
- `rflx generate` option `--debug {built-in,external}` (#1052)

Generator:

- Function for getting current state of session (#796)
- Support for `No_Secondary_Stack` restriction (#911)
- Possibility for externally defined debug output function in generated code (#1052)
- Compatibility of generated code to FSF GNAT 11, 12 and GNAT Pro 23 (#674, #905, #1015, #1116)
- Backward compatibility of generated code to GNAT Community 2020 and GNAT Pro 20 (#896)

Dependencies:

- Support for Python 3.10
- Python: `ruamel.yaml`

### Changed

CLI:

- Make `rflx` option `--no-verification` global (#750)

Specification / Model:

- `Model.__init__` now considers all type dependencies (#1074)
- Rename `then` to `goto` in session states (#738)
- Allow omitting the size aspect for opaque and sequence fields which are the last field of the message (#736)
- Allow use of `Message'Last` and `Message'Size` only in conditions of the last fields of the message (#736)
- Enable use of `Opaque` attribute for arguments of function calls and on sequences (#984, #1021)
- Keep multiple message versions in verification cache (#1028)
- Improve generation of specification files for model (#1009, #1022)
- Detect duplicate aspects (#714)

Generator:

- Improve binary size of generated code (#908)
- Use tagged types instead of generic packages for sessions (#768)
- Change channel interface in generated code (#766, #807)
- Improve handling of bounds in message contexts (#844)
- Optimize provability of generated code (#806, #840, #938, #975)
- Relax length precondition of `To_Context` (#1054)
- Enable comprehensions with message sequence as target (#891)
- Add precondition `Uninitialized` to procedure `Initialize` (#788)
- Add operators for `Length` and `Index` types (#1070)
- Overwrite symlinks when creating files
- Make `In_IO_State` session function public (#1155)
- Generate improved code for messages with reduced feature usage (#1114)

PyRFLX:

- Remove `__getitem__` (#783)

Graph:

- Improve layout of session graphs (#400)

### Removed

- Support for Python 3.7

### Fixed

- Installation of parser when installing RecordFlux from PyPI (#745)
- Examples in README (#879)

Model:

- Handling of `Message` attributes in message types (#729)
- Missing file location in error messages (#647)
- Bug box due to dangling field when merging messages (#1033)
- Missing type information in `Reset` statement (#1080)
- Incorrect message size calculation if size depends on variables (#1064)

Generator:

- Error when using `Boolean` as return type of function (#752)
- Error when using unqualified type as return type of function (#892)
- Bugbox when using `Reset` attribute on a sequence while running without optimization (#946)
- Generation of use clauses for sessions (#757)
- Missing type conversions in generated code (#761, #902, #965)
- Code generation for:
  - `Boolean` as function parameter (#882)
  - Message aggregates (#770)
  - Use of messages with single opaque field in sessions (#888)
  - Function calls in sessions (#763)
  - Mathematical expressions with intermediate values outside type range (#726)
  - Logical expressions in assignments (#1012)
  - Boolean relations containing global variables (#1059)
  - Minimal session (#883)
  - Message aggregates with variables as field values (#1064)
  - Message fields with a sequence type name equal to the package name
- Code generation when using non-default prefix (#897)
- Conversion between message `Structure` and `Context` (#961)
- Missing reset in assignment to comprehension (#1050)
- Message size calculation for message aggregates (#1042)
- Initialization of session context (#954)
- Unprovable VC with some user conditions on fields (#995)

PyRFLX:

- Error caused by relations between sequences, opaque fields or aggregates (#964)
- Undefined attribute in `MessageValue.Field` (#1045)
- Missing type check for arguments of parameterized message (#1104)

## [0.5.0] - 2021-08-11

### Preview Features

- Message checksums (#222, #240)
- Protocol sessions (#47, #291, #292, #675)

### General

- Achieve "Passing" level of CII Best Practices Badge Program (#660)
- Enforce 100% test coverage (#334)
- Show bug box on fatal errors (#607, #655)
- Add ping example app (#366)
- Improve language reference (#703)
- Support showing message graphs in GNAT Studio (#345)

### Specification / Model

- Improve specification parser (#547, #572)
- Change syntax of message types (#380, #432, #421), sequence types (#528) and package separators (#441)
- Enable specification of field conditions (#95, #617)
- Add modulo operation (#476)
- Enable use of size of static types in expressions (#384, #480)
- Add static type checking of expressions (#87)
- Fix message verification (#388, #389, #410, #413, #492, #497, #520, #522, #530, #579)
- Add caching of verification result of message specifications (#442)

### SPARK Code Generation

- Switch from GNAT Community 2020 to GNAT Community 2021 (#494)
- Fix code generation (#375, #479, #486, #356, #500, #536, #530, #593, #665)
- Change API of generated code (#487, #514, #548, #557, #659)

### PyRFLX

- Enable type checking in external applications (#393)
- Change API (#406, #423, #467, #529, #510)
- Fix message parsing and serialization (#407, #503, #531, #533, #525, #606, #559, #624)
- Improve performance (#344)

### New Dependencies

- Python >=3.7
- attrs
- GNAT Community 2021 (GNAT compiler and SPARK verification tools)

## [0.4.1] - 2020-07-23

### Specification / Model

- Improve error messages (#248)
- Add GNAT Studio integration (#243)
- Add more checks for invalid models (#282, #288, #298, #309, #310, #311, #313, #336, #338)
- Fix erroneously rejected specifications (#277, #347, #351)
- Improve parsing performance (#305)

### SPARK Code Generation

- Allow use of scalars up to 64 bit (#238)
- Prevent potentially failing code compilation (#312, #314, #315, #316, #319, #320, #329, #349)
- Allow setting empty sequence field (#353)
- Fix comparison of field values with aggregate (#328)
- Improve verifiability of accesses to opaque fields (#287)
- Fix handling of empty prefixes (#266)

### PyRFLX

- Improve performance (#254)
- Fix determining of predecessor field (#289)
- Fix handling of prefixed literals (#346)

## [0.4.0] - 2020-06-02

### General

- Introduce PyRFLX - a Python library for rapid-prototyping and validation
  - Based on RecordFlux message specifications
  - Allows parsing and generation of messages
  - Validates formal specification at runtime
- Introduce design-by-contract programming in Python code using icontract

### Specification / Model

New Features:

- Allow import of types of other packages
- Allow use of message types as field types
- Add built-in Boolean type
- Support aggregates and strings
- Allow comparisons of arrays to aggregates in conditions

Improvements:

- Simplify derived types by removing inheritance of refinements
- Improve detection of error cases
- Improve error messages
- Fix incorrect parsing of mathematical expressions
- Rename Payload to Opaque in specifications

### SPARK Code Generation

- Allow use of custom buffer type
- Add support for GNAT Community 2020
- Remove need for SPARK Pro for verification
- Remove support for GNAT Community 2019

### New Dependencies

- GNAT Community 2020 (GNAT compiler and SPARK verification tools)
- icontract (Python library for design by contract)

## [0.3.0] - 2020-01-24

### New Features

- Generation of message generator
- Verification of message specifications
- Generation of graph from message specification

### Fixed Bugs

- Incorrect handling of absolute file paths

### New Dependencies

- Minimum version of PyParsing increased to 2.4.0
- Minimum version of SPARK verification tools changed to Pro 20.0 (known issues will be resolved in GNAT Community 2020)
- PyDotPlus (used for generation of graphs)
- Z3 (used for verification of message specifications)

## [0.2.0] - 2019-09-16

## [0.1.0] - 2019-05-14

[unreleased]: https://github.com/AdaCore/RecordFlux/compare/v0.9.0...HEAD
[0.9.0]: https://github.com/AdaCore/RecordFlux/compare/v0.8.0...v0.9.0
[0.8.0]: https://github.com/AdaCore/RecordFlux/compare/v0.7.1...v0.8.0
[0.7.1]: https://github.com/AdaCore/RecordFlux/compare/v0.7.0...v0.7.1
[0.7.0]: https://github.com/AdaCore/RecordFlux/compare/v0.6.0...v0.7.0
[0.6.0]: https://github.com/AdaCore/RecordFlux/compare/v0.5.0...v0.6.0
[0.5.0]: https://github.com/AdaCore/RecordFlux/compare/v0.4.1...v0.5.0
[0.4.1]: https://github.com/AdaCore/RecordFlux/compare/v0.4.0...v0.4.1
[0.4.0]: https://github.com/AdaCore/RecordFlux/compare/v0.3.0...v0.4.0
[0.3.0]: https://github.com/AdaCore/RecordFlux/compare/v0.2.0...v0.3.0
[0.2.0]: https://github.com/AdaCore/RecordFlux/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/AdaCore/RecordFlux/compare/29a292a794af58d29ee0d499e74f3d86b73309fa...v0.1.0
