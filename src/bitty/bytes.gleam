//// Byte-level parsers for reading, skipping, peeking, and matching raw
//// bytes. These parsers work at any bit position — when unaligned, bytes
//// are extracted bit-by-bit. When byte-aligned, zero-copy slicing is used.
//// After bit-level parsing with `bitty/bits`, call `bits.align()` before
//// resuming byte-aligned operations.

import bitty
import gleam/bit_array
import gleam/bool
import gleam/int

/// Parse a single byte and return it as a `BitArray`.
///
/// ```gleam
/// let assert Ok(b) = bitty.run(bytes.byte(), on: <<0xFF>>)
/// assert b == <<0xFF>>
/// ```
pub fn byte() -> bitty.Parser(BitArray) {
  take(bytes: 1)
}

/// Parse exactly `count` bytes as a `BitArray`.
/// When byte-aligned, returns a zero-copy slice.
pub fn take(bytes count: Int) -> bitty.Parser(BitArray) {
  bitty.make_parser(bitty.read_n_bytes(_, count))
}

/// Advance the parser position by `count` bytes, discarding the data.
pub fn skip(bytes count: Int) -> bitty.Parser(Nil) {
  bitty.make_parser(fn(state: bitty.State) {
    use <- bool.guard(
      when: count < 0,
      return: bitty.stop_expected(state, "non-negative skip count"),
    )
    let total_bits = count * 8
    let #(new_byte, new_bit) =
      bitty.advance_bits(state.byte_offset, state.bit_offset, total_bits)
    let input_size = bit_array.byte_size(state.input)
    let at_valid_position = case new_bit {
      0 -> new_byte <= input_size
      _ -> new_byte < input_size
    }
    use <- bool.guard(
      when: !at_valid_position,
      return: bitty.stop_expected(
        state,
        "at least " <> int.to_string(count) <> " bytes",
      ),
    )
    bitty.Continue(
      Nil,
      bitty.State(..state, byte_offset: new_byte, bit_offset: new_bit),
      count > 0,
    )
  })
}

/// Consume all remaining input as a `BitArray`.
/// Works at any bit position — the result may be non-byte-aligned.
pub fn rest() -> bitty.Parser(BitArray) {
  bitty.make_parser(fn(state: bitty.State) {
    let total_remaining_bits =
      { bit_array.byte_size(state.input) - state.byte_offset }
      * 8
      - state.bit_offset
    use <- bool.guard(
      when: total_remaining_bits <= 0,
      return: bitty.Continue(<<>>, state, False),
    )

    let remaining_bytes = total_remaining_bits / 8
    let trailing_bits = total_remaining_bits % 8
    rest_read(state, remaining_bytes, trailing_bits)
  })
}

fn rest_read(
  state: bitty.State,
  remaining_bytes: Int,
  trailing_bits: Int,
) -> bitty.Step(BitArray) {
  case rest_read_bytes(state, remaining_bytes) {
    bitty.Continue(bytes, mid_state, _) ->
      case rest_read_trailing(mid_state, trailing_bits) {
        bitty.Continue(trail, end_state, _) ->
          bitty.Continue(<<bytes:bits, trail:bits>>, end_state, True)
        bitty.Stop(e, c, cm) -> bitty.Stop(e, c, cm)
      }
    bitty.Stop(e, c, cm) -> bitty.Stop(e, c, cm)
  }
}

fn rest_read_bytes(state: bitty.State, count: Int) -> bitty.Step(BitArray) {
  case count > 0 {
    True -> bitty.read_n_bytes(state, count)
    False -> bitty.Continue(<<>>, state, False)
  }
}

fn rest_read_trailing(state: bitty.State, count: Int) -> bitty.Step(BitArray) {
  case count > 0 {
    True ->
      case bitty.read_uint(state, count, 0, True) {
        bitty.Continue(val, end_state, _) ->
          bitty.Continue(<<val:size(count)>>, end_state, True)
        bitty.Stop(e, c, cm) -> bitty.Stop(e, c, cm)
      }
    False -> bitty.Continue(<<>>, state, False)
  }
}

/// Look ahead at the next `count` bytes without consuming them.
pub fn peek(bytes count: Int) -> bitty.Parser(BitArray) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, count) {
      bitty.Continue(bytes, _, _) -> bitty.Continue(bytes, state, False)
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Match an exact byte sequence at the current position and consume it.
/// Fails if the bytes don't match. Useful for magic numbers and fixed headers.
pub fn tag(expected: BitArray) -> bitty.Parser(Nil) {
  let len = bit_array.byte_size(expected)
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, len) {
      bitty.Continue(actual, new_state, consumed) if actual == expected ->
        bitty.Continue(Nil, new_state, consumed)
      bitty.Continue(_, _, _) -> bitty.stop_expected(state, "tag")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse a single byte that satisfies the given predicate.
/// The predicate receives a 1-byte `BitArray`.
/// When byte-aligned, returns a zero-copy slice. When unaligned, the byte
/// is extracted individually.
///
/// ```gleam
/// let parser = bytes.byte_if(fn(b) { b == <<0xFF>> })
/// let assert Ok(value) = bitty.run(parser, on: <<0xFF, 0x00>>)
/// assert value == <<0xFF>>
/// ```
pub fn byte_if(predicate: fn(BitArray) -> Bool) -> bitty.Parser(BitArray) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 1) {
      bitty.Continue(matched, new_state, _) ->
        case predicate(matched) {
          True -> bitty.Continue(matched, new_state, True)
          False -> bitty.stop_expected(state, "matching byte")
        }
      bitty.Stop(_, _, _) -> bitty.stop_expected(state, "a byte")
    }
  })
}

/// Consume bytes while the predicate holds.
/// Returns `<<>>` when zero bytes match.
/// When byte-aligned, returns a zero-copy slice. When unaligned, bytes are
/// extracted individually.
///
/// ```gleam
/// let parser = bytes.take_while(fn(b) { b != <<0x00>> })
/// let assert Ok(value) = bitty.run(parser, on: <<1, 2, 3, 0x00>>)
/// assert value == <<1, 2, 3>>
/// ```
pub fn take_while(predicate: fn(BitArray) -> Bool) -> bitty.Parser(BitArray) {
  bitty.make_parser(fn(state: bitty.State) {
    case state.bit_offset == 0 {
      True -> take_while_loop(state, state.byte_offset, predicate)
      False -> take_while_unaligned(state, <<>>, predicate)
    }
  })
}

fn take_while_loop(
  state: bitty.State,
  start: Int,
  predicate: fn(BitArray) -> Bool,
) -> bitty.Step(BitArray) {
  case bit_array.slice(state.input, state.byte_offset, 1) {
    Ok(matched) ->
      case predicate(matched) {
        True ->
          take_while_loop(
            bitty.State(..state, byte_offset: state.byte_offset + 1),
            start,
            predicate,
          )
        False -> finish_byte_slice(state, start)
      }
    _ -> finish_byte_slice(state, start)
  }
}

fn finish_byte_slice(state: bitty.State, start: Int) -> bitty.Step(BitArray) {
  let len = state.byte_offset - start
  case len {
    0 -> bitty.Continue(<<>>, state, False)
    _ ->
      case bit_array.slice(state.input, start, len) {
        Ok(bytes) -> bitty.Continue(bytes, state, True)
        _ -> bitty.stop_expected(state, "valid slice")
      }
  }
}

fn take_while_unaligned(
  state: bitty.State,
  acc: BitArray,
  predicate: fn(BitArray) -> Bool,
) -> bitty.Step(BitArray) {
  case bitty.read_n_bytes(state, 1) {
    bitty.Continue(matched, new_state, _) ->
      case predicate(matched) {
        True ->
          take_while_unaligned(new_state, <<acc:bits, matched:bits>>, predicate)
        False -> bitty.Continue(acc, state, bit_array.byte_size(acc) > 0)
      }
    bitty.Stop(_, _, _) ->
      bitty.Continue(acc, state, bit_array.byte_size(acc) > 0)
  }
}

/// Like `take_while` but requires at least one matching byte.
/// When byte-aligned, returns a zero-copy slice. When unaligned, bytes are
/// extracted individually.
///
/// ```gleam
/// let parser = bytes.take_while1(fn(b) { b != <<0x00>> })
/// let assert Ok(value) = bitty.run(parser, on: <<1, 2, 3, 0x00>>)
/// assert value == <<1, 2, 3>>
/// ```
pub fn take_while1(predicate: fn(BitArray) -> Bool) -> bitty.Parser(BitArray) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 1) {
      bitty.Continue(matched, new_state, _) ->
        case predicate(matched) {
          True ->
            case state.bit_offset == 0 {
              True -> take_while_loop(new_state, state.byte_offset, predicate)
              False -> take_while_unaligned(new_state, matched, predicate)
            }
          False -> bitty.stop_expected(state, "at least one matching byte")
        }
      bitty.Stop(_, _, _) ->
        bitty.stop_expected(state, "at least one matching byte")
    }
  })
}

/// Consume bytes until the predicate matches, returning everything before
/// the matching byte. Returns `<<>>` when zero bytes match.
/// When byte-aligned, returns a zero-copy slice. When unaligned, bytes are
/// extracted individually.
///
/// ```gleam
/// let parser = bytes.take_until(fn(b) { b == <<0x00>> })
/// let assert Ok(value) = bitty.run(parser, on: <<1, 2, 3, 0x00>>)
/// assert value == <<1, 2, 3>>
/// ```
pub fn take_until(predicate: fn(BitArray) -> Bool) -> bitty.Parser(BitArray) {
  take_while(fn(b) { !predicate(b) })
}
