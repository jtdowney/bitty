# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.0.0] - 2026-02-14

### Added

- Core `Parser(a)` type with zero-copy binary parsing
- Monadic composition via `use` syntax (`do`, `return`)
- Backtracking control with `attempt`, `cut`, and `committed`
- Choice combinators: `one_of`, `optional`, `not`
- Repetition combinators: `many`, `many1`, `times`, `take_while`, `take_until`
- `within_bytes` for length-delimited sub-window parsing
- Position tracking with `byte_offset` and `bit_offset`
- `bitty/bytes` module: `take`, `skip`, `rest`, `peek`, `tag`
- `bitty/bits` module: `bit`, `uint` for bit-level parsing
- `bitty/num` module: integer types (`u8`/`i8` through `u64`/`i64`), floats (`f32`/`f64`), LEB128 (`var_u32`/`var_u64`), and raw byte integer decoding
- Endianness support for multi-byte numeric parsers
- Structured error reporting with `ParseError`, `expected`, and `label`
- Support for both BEAM and JavaScript targets

[1.0.0]: https://github.com/jtdowney/bitty/releases/tag/v1.0.0
