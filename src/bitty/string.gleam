//// String-level parsers for reading, matching, and scanning UTF-8 text.
//// These parsers decode graphemes on the fly.
//// Use `bitty/bytes` for raw byte operations and `bitty/bits` for sub-byte
//// access before returning to string parsing.

import bitty
import bitty/bytes
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option
import gleam/result
import gleam/string

/// Parse zero or more ASCII alphabetic characters (`a-zA-Z`).
/// Returns `""` when no characters match.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.alpha(), on: <<"abcXYZ123">>)
/// assert s == "abcXYZ"
/// assert rest == <<"123">>
/// ```
pub fn alpha() -> bitty.Parser(String) {
  take_while(is_alpha)
}

/// Parse one or more ASCII alphabetic characters (`a-zA-Z`).
/// Fails if the first character is not alphabetic.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.alpha1(), on: <<"abcXYZ123">>)
/// assert s == "abcXYZ"
/// assert rest == <<"123">>
/// ```
pub fn alpha1() -> bitty.Parser(String) {
  take_while1(is_alpha)
}

/// Parse zero or more ASCII alphanumeric characters (`a-zA-Z0-9`).
/// Returns `""` when no characters match.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.alphanumeric(), on: <<"abc123!!">>)
/// assert s == "abc123"
/// assert rest == <<"!!">>
/// ```
pub fn alphanumeric() -> bitty.Parser(String) {
  take_while(is_alphanumeric)
}

/// Parse one or more ASCII alphanumeric characters (`a-zA-Z0-9`).
/// Fails if the first character is not alphanumeric.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.alphanumeric1(), on: <<"abc123!!">>)
/// assert s == "abc123"
/// assert rest == <<"!!">>
/// ```
pub fn alphanumeric1() -> bitty.Parser(String) {
  take_while1(is_alphanumeric)
}

/// Parse zero or more ASCII decimal digits (`0-9`).
/// Returns `""` when no characters match.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.digit(), on: <<"42abc">>)
/// assert s == "42"
/// assert rest == <<"abc">>
/// ```
pub fn digit() -> bitty.Parser(String) {
  take_while(is_digit)
}

/// Parse one or more ASCII decimal digits (`0-9`).
/// Fails if the first character is not a digit.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.digit1(), on: <<"42abc">>)
/// assert s == "42"
/// assert rest == <<"abc">>
/// ```
pub fn digit1() -> bitty.Parser(String) {
  take_while1(is_digit)
}

/// Parse one or more ASCII digits and convert them to an `Int`.
/// Fails if no digits are found or if parsing fails.
///
/// ```gleam
/// let assert Ok(#(n, rest)) = bitty.run_partial(string.integer(), on: <<"42abc">>)
/// assert n == 42
/// assert rest == <<"abc">>
/// ```
pub fn integer() -> bitty.Parser(Int) {
  use digits <- bitty.then(digit1())
  case int.parse(digits) {
    Ok(n) -> bitty.success(n)
    Error(_) -> bitty.fail("expected valid integer")
  }
}

/// Parse zero or more ASCII hexadecimal digits (`0-9a-fA-F`).
/// Returns `""` when no characters match.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.hex_digit(), on: <<"deadBEEF!">>)
/// assert s == "deadBEEF"
/// assert rest == <<"!">>
/// ```
pub fn hex_digit() -> bitty.Parser(String) {
  take_while(is_hex_digit)
}

/// Parse one or more ASCII hexadecimal digits (`0-9a-fA-F`).
/// Fails if the first character is not a hex digit.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.hex_digit1(), on: <<"deadBEEF!">>)
/// assert s == "deadBEEF"
/// assert rest == <<"!">>
/// ```
pub fn hex_digit1() -> bitty.Parser(String) {
  take_while1(is_hex_digit)
}

/// Parse zero or more spaces and tabs.
/// Returns `""` when no characters match.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.space(), on: <<"  \thello">>)
/// assert s == "  \t"
/// assert rest == <<"hello">>
/// ```
pub fn space() -> bitty.Parser(String) {
  take_while(is_space)
}

/// Parse one or more spaces and tabs.
/// Fails if the first character is not a space or tab.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.space1(), on: <<"  \thello">>)
/// assert s == "  \t"
/// assert rest == <<"hello">>
/// ```
pub fn space1() -> bitty.Parser(String) {
  take_while1(is_space)
}

/// Parse zero or more whitespace characters (space, tab, `\r`, `\n`).
/// Returns `""` when no characters match.
///
/// ```gleam
/// let assert Ok(s) = bitty.run(string.multispace(), on: <<" \t\n">>)
/// assert s == " \t\n"
/// ```
pub fn multispace() -> bitty.Parser(String) {
  take_while(is_multispace)
}

/// Parse one or more whitespace characters (space, tab, `\r`, `\n`).
/// Fails if the first character is not whitespace.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.multispace1(), on: <<" \t\nhello">>)
/// assert s == " \t\n"
/// assert rest == <<"hello">>
/// ```
pub fn multispace1() -> bitty.Parser(String) {
  take_while1(is_multispace)
}

/// Match a newline character (`\n`).
///
/// ```gleam
/// let assert Ok(#(_, rest)) = bitty.run_partial(string.newline(), on: <<"\nhello">>)
/// assert rest == <<"hello">>
/// ```
pub fn newline() -> bitty.Parser(Nil) {
  literal("\n")
}

/// Match a tab character (`\t`).
///
/// ```gleam
/// let assert Ok(#(_, rest)) = bitty.run_partial(string.tab(), on: <<"\thello">>)
/// assert rest == <<"hello">>
/// ```
pub fn tab() -> bitty.Parser(Nil) {
  literal("\t")
}

/// Match a carriage return + line feed sequence (`\r\n`).
///
/// ```gleam
/// let assert Ok(#(_, rest)) = bitty.run_partial(string.crlf(), on: <<"\r\nhello">>)
/// assert rest == <<"hello">>
/// ```
pub fn crlf() -> bitty.Parser(Nil) {
  literal("\r\n")
}

/// Match a line ending: either `\r\n` or `\n`.
/// Tries `\r\n` first to avoid partial matches.
///
/// ```gleam
/// let assert Ok(#(_, rest)) = bitty.run_partial(string.line_ending(), on: <<"\r\nhello">>)
/// assert rest == <<"hello">>
/// ```
pub fn line_ending() -> bitty.Parser(Nil) {
  bitty.one_of([crlf(), newline()])
}

/// Consume characters until a `\r` or `\n` is found.
/// The line ending itself is not consumed.
///
/// ```gleam
/// let assert Ok(#(s, rest)) = bitty.run_partial(string.not_line_ending(), on: <<"hello\nworld">>)
/// assert s == "hello"
/// assert rest == <<"\nworld">>
/// ```
pub fn not_line_ending() -> bitty.Parser(String) {
  // Uses string.contains because "\r\n" is a single grapheme cluster (UAX #29),
  // so checking c != "\n" && c != "\r" would miss the combined CR+LF grapheme.
  take_while(fn(c) { !string.contains(c, "\n") && !string.contains(c, "\r") })
}

/// Parse exactly `byte_len` bytes and decode them as a UTF-8 string.
/// Fails if the bytes are not valid UTF-8 or if there is insufficient input.
///
/// ```gleam
/// let assert Ok(s) = bitty.run(string.utf8(5), on: <<"hello">>)
/// assert s == "hello"
/// ```
pub fn utf8(byte_len: Int) -> bitty.Parser(String) {
  use raw <- bitty.then(bytes.take(bytes: byte_len))
  case bit_array.to_string(raw) {
    Ok(s) -> bitty.success(s)
    Error(_) -> bitty.fail("expected valid utf-8")
  }
}

/// Match an exact string literal at the current position and consume it.
/// Returns `Nil` on success; fails if the bytes don't match.
///
/// ```gleam
/// let assert Ok(Nil) = bitty.run(string.literal("GET"), on: <<"GET">>)
/// ```
pub fn literal(expected: String) -> bitty.Parser(Nil) {
  bytes.tag(bit_array.from_string(expected))
}

/// Parse a null-terminated (C-style) string. Consumes graphemes until a
/// NUL byte (`0x00`) is found, then skips the NUL. Fails if no NUL is present.
///
/// ```gleam
/// let assert Ok(s) = bitty.run(string.null_terminated(), on: <<"hi", 0x00>>)
/// assert s == "hi"
/// ```
pub fn null_terminated() -> bitty.Parser(String) {
  use s <- bitty.then(take_until(fn(c) { c == "\u{0000}" }))
  use _ <- bitty.then(bytes.tag(<<0>>))
  bitty.success(s)
}

/// Parse a fixed-width string field, trimming trailing NUL bytes and spaces.
/// Useful for binary formats that pad string fields to a fixed size.
///
/// ```gleam
/// let assert Ok(s) = bitty.run(string.fixed(5), on: <<"hi", 0x00, 0x00, 0x00>>)
/// assert s == "hi"
/// ```
pub fn fixed(byte_len: Int) -> bitty.Parser(String) {
  utf8(byte_len) |> bitty.map(trim_padding)
}

fn trim_padding(s: String) -> String {
  s
  |> string.to_graphemes
  |> list.reverse
  |> list.drop_while(fn(g) { g == " " || g == "\u{0000}" })
  |> list.reverse
  |> string.join("")
}

/// Parse a single UTF-8 grapheme and return it as a one-character string.
/// Handles multi-byte and multi-codepoint grapheme clusters.
///
/// ```gleam
/// let assert Ok(c) = bitty.run(string.grapheme(), on: <<"A">>)
/// assert c == "A"
/// ```
pub fn grapheme() -> bitty.Parser(String) {
  grapheme_if(fn(_) { True })
}

/// Parse a single UTF-8 grapheme that satisfies the given predicate.
/// Fails if the grapheme doesn't match or if the input is not valid UTF-8.
///
/// ```gleam
/// let parser = string.grapheme_if(fn(c) { c == "A" })
/// let assert Ok(c) = bitty.run(parser, on: <<"A">>)
/// assert c == "A"
/// ```
pub fn grapheme_if(predicate: fn(String) -> Bool) -> bitty.Parser(String) {
  bitty.make_parser(fn(state: bitty.State) {
    use <- bitty.require_aligned(state)
    case decode_grapheme(state.input, state.byte_offset) {
      Ok(#(s, len)) ->
        case predicate(s) {
          True ->
            bitty.Continue(
              s,
              bitty.State(..state, byte_offset: state.byte_offset + len),
              True,
            )
          False -> bitty.stop_expected(state, "matching grapheme")
        }
      Error(_) -> bitty.stop_expected(state, "valid utf-8 grapheme")
    }
  })
}

fn peek_lead_byte(input: BitArray, offset: Int) -> Result(Int, Nil) {
  case bit_array.slice(input, offset, 1) {
    Ok(<<byte>>) -> Ok(byte)
    _ -> Error(Nil)
  }
}

fn utf8_byte_len(lead_byte: Int) -> Result(Int, Nil) {
  case lead_byte {
    b if b < 0x80 -> Ok(1)
    b if b >= 0xC0 && b < 0xE0 -> Ok(2)
    b if b >= 0xE0 && b < 0xF0 -> Ok(3)
    b if b >= 0xF0 && b < 0xF8 -> Ok(4)
    _ -> Error(Nil)
  }
}

fn decode_codepoint(input: BitArray, offset: Int) -> Result(#(String, Int), Nil) {
  use lead <- result.try(peek_lead_byte(input, offset))
  use len <- result.try(utf8_byte_len(lead))
  use bytes <- result.try(bit_array.slice(input, offset, len))
  use s <- result.try(bit_array.to_string(bytes))
  Ok(#(s, len))
}

fn decode_grapheme(input: BitArray, offset: Int) -> Result(#(String, Int), Nil) {
  use #(_, first_len) <- result.try(decode_codepoint(input, offset))
  extend_grapheme(input, offset, offset + first_len)
}

fn extend_grapheme(
  input: BitArray,
  start: Int,
  end: Int,
) -> Result(#(String, Int), Nil) {
  let len = end - start
  use bytes <- result.try(bit_array.slice(input, start, len))
  use current <- result.try(bit_array.to_string(bytes))

  case try_extend_grapheme(input, start, end) {
    Ok(new_end) -> extend_grapheme(input, start, new_end)
    Error(_) -> Ok(#(current, len))
  }
}

fn try_extend_grapheme(
  input: BitArray,
  start: Int,
  end: Int,
) -> Result(Int, Nil) {
  use #(_, next_len) <- result.try(decode_codepoint(input, end))
  let new_end = end + next_len
  let total_len = new_end - start
  use ext_bytes <- result.try(bit_array.slice(input, start, total_len))
  use ext_str <- result.try(bit_array.to_string(ext_bytes))
  case string.to_graphemes(ext_str) {
    [_] -> Ok(new_end)
    _ -> Error(Nil)
  }
}

/// Parse exactly `count` UTF-8 graphemes and return them as a string.
/// Unlike `utf8` which counts bytes, this counts logical characters.
///
/// ```gleam
/// let assert Ok(s) = bitty.run(string.take_graphemes(2), on: <<"café">>)
/// assert s == "ca"
/// ```
pub fn take_graphemes(count: Int) -> bitty.Parser(String) {
  bitty.make_parser(fn(state: bitty.State) {
    use <- bool.guard(
      when: count < 0,
      return: bitty.stop_expected(state, "non-negative grapheme count"),
    )
    use <- bitty.require_aligned(state)
    take_graphemes_loop(state, state.byte_offset, count)
  })
}

fn take_graphemes_loop(
  state: bitty.State,
  start: Int,
  remaining: Int,
) -> bitty.Step(String) {
  case remaining <= 0 {
    True -> finish_slice(state, start)
    False ->
      case decode_grapheme(state.input, state.byte_offset) {
        Ok(#(_s, len)) ->
          take_graphemes_loop(
            bitty.State(..state, byte_offset: state.byte_offset + len),
            start,
            remaining - 1,
          )
        Error(_) -> {
          let consumed = state.byte_offset > start
          bitty.Stop(
            bitty.BittyError(
              at: bitty.Location(byte: state.byte_offset, bit: state.bit_offset),
              expected: [int.to_string(remaining) <> " more graphemes"],
              context: [],
              message: option.None,
            ),
            consumed,
            state.committed,
          )
        }
      }
  }
}

fn finish_slice(state: bitty.State, start: Int) -> bitty.Step(String) {
  let len = state.byte_offset - start
  case len {
    0 -> bitty.Continue("", state, False)
    _ ->
      case decode_slice(state.input, start, len) {
        Ok(s) -> bitty.Continue(s, state, True)
        Error(_) -> bitty.stop_expected(state, "valid utf-8")
      }
  }
}

fn decode_slice(input: BitArray, start: Int, len: Int) -> Result(String, Nil) {
  use bytes <- result.try(bit_array.slice(input, start, len))
  bit_array.to_string(bytes)
}

/// Consume graphemes while the predicate holds, returning them as a string.
/// Returns `""` when zero graphemes match.
///
/// ```gleam
/// let parser = string.take_while(fn(c) { c != " " })
/// let assert Ok(#(s, rest)) = bitty.run_partial(parser, on: <<"hello world">>)
/// assert s == "hello"
/// assert rest == <<" world">>
/// ```
pub fn take_while(predicate: fn(String) -> Bool) -> bitty.Parser(String) {
  bitty.make_parser(fn(state: bitty.State) {
    use <- bitty.require_aligned(state)
    take_while_loop(state, state.byte_offset, predicate)
  })
}

fn take_while_loop(
  state: bitty.State,
  start: Int,
  predicate: fn(String) -> Bool,
) -> bitty.Step(String) {
  case decode_grapheme(state.input, state.byte_offset) {
    Ok(#(s, len)) ->
      case predicate(s) {
        True ->
          take_while_loop(
            bitty.State(..state, byte_offset: state.byte_offset + len),
            start,
            predicate,
          )
        False -> finish_slice(state, start)
      }
    Error(_) -> finish_slice(state, start)
  }
}

/// Like `take_while` but requires at least one matching grapheme.
/// Fails if the first grapheme doesn't satisfy the predicate.
///
/// ```gleam
/// let parser = string.take_while1(fn(c) { c == "a" })
/// let assert Ok(#(s, _rest)) = bitty.run_partial(parser, on: <<"aab">>)
/// assert s == "aa"
/// ```
pub fn take_while1(predicate: fn(String) -> Bool) -> bitty.Parser(String) {
  bitty.make_parser(fn(state: bitty.State) {
    use <- bitty.require_aligned(state)
    case decode_grapheme(state.input, state.byte_offset) {
      Ok(#(s, len)) ->
        case predicate(s) {
          True ->
            take_while_loop(
              bitty.State(..state, byte_offset: state.byte_offset + len),
              state.byte_offset,
              predicate,
            )
          False -> bitty.stop_expected(state, "matching grapheme")
        }
      Error(_) -> bitty.stop_expected(state, "valid utf-8 grapheme")
    }
  })
}

/// Consume graphemes until the predicate matches, returning everything
/// before the match. The matching grapheme is not consumed.
/// Returns `""` when the first grapheme matches.
///
/// ```gleam
/// let parser = string.take_until(fn(c) { c == " " })
/// let assert Ok(#(s, rest)) = bitty.run_partial(parser, on: <<"hello world">>)
/// assert s == "hello"
/// assert rest == <<" world">>
/// ```
pub fn take_until(predicate: fn(String) -> Bool) -> bitty.Parser(String) {
  take_while(fn(c) { !predicate(c) })
}

fn byte_of(s: String) -> Result(Int, Nil) {
  case bit_array.from_string(s) {
    <<b>> -> Ok(b)
    _ -> Error(Nil)
  }
}

fn is_alpha(c: String) -> Bool {
  case byte_of(c) {
    Ok(b) -> { b >= 0x41 && b <= 0x5A } || { b >= 0x61 && b <= 0x7A }
    Error(_) -> False
  }
}

fn is_digit(c: String) -> Bool {
  case byte_of(c) {
    Ok(b) -> b >= 0x30 && b <= 0x39
    Error(_) -> False
  }
}

fn is_alphanumeric(c: String) -> Bool {
  is_alpha(c) || is_digit(c)
}

fn is_hex_digit(c: String) -> Bool {
  case byte_of(c) {
    Ok(b) ->
      { b >= 0x30 && b <= 0x39 }
      || { b >= 0x41 && b <= 0x46 }
      || { b >= 0x61 && b <= 0x66 }
    Error(_) -> False
  }
}

fn is_space(c: String) -> Bool {
  c == " " || c == "\t"
}

fn is_multispace(c: String) -> Bool {
  // The "\r\n" branch is intentional: it forms a single grapheme cluster per
  // UAX #29, so the grapheme iterator yields it as one unit.
  c == " " || c == "\t" || c == "\n" || c == "\r" || c == "\r\n"
}
