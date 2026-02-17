# bitty

[![Package Version](https://img.shields.io/hexpm/v/bitty)](https://hex.pm/packages/bitty)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/bitty/)

A binary and string parser combinator library for Gleam that tries to be zero-copy when it can be.

## Installation

```sh
gleam add bitty
```

## Quick Example

### Parsing a simple TLV (tag-length-value) structure

Using Gleam’s `use` syntax for monadic bind:

```gleam
import bitty as p
import bitty/bytes as b
import bitty/num

pub type Tlv {
  Tlv(tag: Int, length: Int, value: BitArray)
}

pub fn tlv() -> p.Parser(Tlv) {
  use tag <- p.then(num.u8())
  use len <- p.then(num.u8())
  use value <- p.then(b.take(len))

  p.success(Tlv(tag, len, value))
}
```

- `value` is a zero-copy slice.
- No hidden allocation.
- Fully portable.

## Core Concepts

### Parser

```gleam
pub opaque type Parser(a)
```

Parsers operate over `BitArray` and track:

- Byte offset
- Bit offset
- Commit state for backtracking control

### Running a parser

```gleam
pub fn run(parser: Parser(a), on input: BitArray)
  -> Result(a, BittyError)

pub fn run_partial(parser: Parser(a), on input: BitArray)
  -> Result(#(a, BitArray), BittyError)
```

- `run` requires full consumption.
- `run_partial` returns the zero-copy remainder.

## Length-Bounded Parsing

Critical for TLV-style formats:

```gleam
pub fn within_bytes(byte_len: Int, run inner: Parser(a))
  -> Parser(a)
```

Parse exactly `byte_len` bytes as a sub-stream.

Used for:

- ASN.1 SEQUENCE
- PNG chunks
- Length-prefixed frames
- Nested protocols

## Numeric Decoding

```gleam
import bitty/num

num.u8()
num.u16(num.BigEndian)
num.u32(num.LittleEndian)
num.u64(num.BigEndian)
```

All integer parsers return `Int`. On the JavaScript target, 64-bit values
above 2^53 - 1 (9,007,199,254,740,991) may lose precision due to IEEE 754
double-precision floating-point limitations.

## Bit-Level Parsing

```gleam
import bitty/bits

use flag <- p.then(bits.bit())
use value <- p.then(bits.uint(5))
use _ <- p.then(p.align())

p.success(#(flag, value))
```

Byte-aligned parsing is the fast path.
Bit parsing is explicit and opt-in.

## String Parsing

```gleam
import bitty as p
import bitty/string as s

pub fn request_line() -> p.Parser(#(String, String, String)) {
  use method <- p.then(s.alpha1())
  use _ <- p.then(s.literal(" "))
  use path <- p.then(s.take_while1(fn(c) { c != " " }))
  use _ <- p.then(s.literal(" "))
  use version <- p.then(s.not_line_ending())

  p.success(#(method, path, version))
}
```

## Error Reporting

Structured error type:

```gleam
pub type BittyError {
  BittyError(
    at: Location,
    expected: List(String),
    context: List(String),
    message: Option(String),
  )
}
```

Add readable labels:

```gleam
p.label(parser, named: "ASN.1 length")
p.context(parser, in: "TLS handshake")
```

You get precise error locations and meaningful context stacks.
