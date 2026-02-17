# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [1.1.0] - 2026-02-16

### Added

- `bitty/string` module: UTF-8 string parsers including character classes (`alpha`, `digit`, `alphanumeric`, `hex_digit`, `space`, `multispace`), `integer` parsing, line endings, `literal` tag matching, `grapheme`/`grapheme_if`, `take_graphemes`, `take_while`, `take_while1`, `take_until`, `utf8`, `fixed`, and `null_terminated`
- `pair` and `separated_pair` combinators for parsing two values in sequence
- `preceded`, `terminated`, `delimited` sequencing combinators
- `separated` and `separated1` separation combinators
- `fold` and `fold1` combinators for accumulating results without intermediate lists
- `verify` combinator for validating parsed values against a predicate
- `capture` combinator to return the matched `BitArray` span
- `cond` combinator for conditional parsing
- `bytes.byte` parser for matching a single specific byte
- TCP and HTTP parsing examples in the `example/` project

### Changed

- Moved `align` to `bitty/bits` module; `bitty.align` is deprecated but still available

### Fixed

- Propagate `committed` flag correctly in `not` and `capture` combinators
- Preserve partial bits in `remaining_slice` when not byte-aligned

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

[1.1.0]: https://github.com/jtdowney/bitty/releases/tag/v1.1.0
[1.0.0]: https://github.com/jtdowney/bitty/releases/tag/v1.0.0
