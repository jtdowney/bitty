//// Bit-level parsers for reading individual bits and sub-byte unsigned
//// integers. Bits are read MSB-first within each byte. Use `bitty.align` to
//// return to byte-aligned parsing after bit-level operations.

import bitty
import gleam/bit_array
import gleam/int

/// Parse a single bit as a `Bool` (`True` for 1, `False` for 0).
/// Bits are read MSB-first within each byte.
///
/// ```gleam
/// let assert Ok(#(flag, _)) =
///   bitty.run_partial(bits.bit(), on: <<0x80>>)
/// assert flag == True
/// ```
pub fn bit() -> bitty.Parser(Bool) {
  bitty.make_parser(fn(state: bitty.State) {
    case bit_array.slice(state.input, state.byte_offset, 1) {
      Ok(<<byte>>) -> {
        let bit_val =
          int.bitwise_and(
            int.bitwise_shift_right(byte, 7 - state.bit_offset),
            1,
          )
        let #(new_byte, new_bit) =
          bitty.advance_bits(state.byte_offset, state.bit_offset, 1)
        bitty.Continue(
          bit_val == 1,
          bitty.State(..state, byte_offset: new_byte, bit_offset: new_bit),
          True,
        )
      }
      _ -> bitty.stop_expected(state, "a bit")
    }
  })
}

/// Parse `count` bits as an unsigned integer. Does not require byte
/// alignment — can start mid-byte. On the JavaScript target, values
/// above 32 bits may lose precision due to IEEE 754 double-precision
/// limitations.
///
/// ```gleam
/// let assert Ok(#(value, _)) =
///   bitty.run_partial(bits.uint(bits: 4), on: <<0xAB>>)
/// assert value == 0x0A
/// ```
pub fn uint(bits count: Int) -> bitty.Parser(Int) {
  bitty.make_parser(fn(state: bitty.State) {
    case count < 0 {
      True -> bitty.stop_expected(state, "non-negative bit count")
      False -> bitty.read_uint(state, count, 0, count > 0)
    }
  })
}
