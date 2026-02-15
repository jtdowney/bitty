//// Byte-level parsers for reading, skipping, peeking, and matching raw
//// bytes. These parsers work at any bit position — when unaligned, bytes
//// are extracted bit-by-bit. When byte-aligned, zero-copy slicing is used.
//// After bit-level parsing with `bitty/bits`, call `bitty.align()` before
//// resuming byte-aligned operations.

import bitty
import gleam/bit_array
import gleam/bool
import gleam/int

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
