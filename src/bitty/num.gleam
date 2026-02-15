//// Numeric parsers for fixed-width integers, floats, and variable-length
//// encodings. Includes single-byte (`u8`, `i8`) and multi-byte types with
//// endianness control.

import bitty
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/option.{None}

/// Byte order for multi-byte numeric parsers.
pub type Endian {
  BigEndian
  LittleEndian
}

/// Parse a single byte as an unsigned integer (0–255).
pub fn u8() -> bitty.Parser(Int) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 1) {
      bitty.Continue(<<byte>>, new_state, consumed) ->
        bitty.Continue(byte, new_state, consumed)
      bitty.Continue(_, new_state, _) ->
        bitty.stop_expected(new_state, "a byte")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse a single byte as a signed integer (-128–127).
pub fn i8() -> bitty.Parser(Int) {
  u8()
  |> bitty.map(fn(unsigned) {
    case unsigned >= 128 {
      True -> unsigned - 256
      False -> unsigned
    }
  })
}

/// Parse a 16-bit unsigned integer with the given endianness.
///
/// ```gleam
/// let assert Ok(val) =
///   bitty.run(num.u16(num.BigEndian), on: <<0x01, 0x00>>)
/// assert val == 256
/// ```
pub fn u16(endian: Endian) -> bitty.Parser(Int) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 2) {
      bitty.Continue(<<b0, b1>>, new_state, consumed) -> {
        let value = case endian {
          BigEndian -> b0 * 256 + b1
          LittleEndian -> b1 * 256 + b0
        }
        bitty.Continue(value, new_state, consumed)
      }
      bitty.Continue(_, _, _) -> bitty.stop_expected(state, "2 bytes")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse a 32-bit unsigned integer with the given endianness.
pub fn u32(endian: Endian) -> bitty.Parser(Int) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 4) {
      bitty.Continue(<<b0, b1, b2, b3>>, new_state, consumed) -> {
        let value = case endian {
          BigEndian -> b0 * 16_777_216 + b1 * 65_536 + b2 * 256 + b3
          LittleEndian -> b3 * 16_777_216 + b2 * 65_536 + b1 * 256 + b0
        }
        bitty.Continue(value, new_state, consumed)
      }
      bitty.Continue(_, _, _) -> bitty.stop_expected(state, "4 bytes")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse a 16-bit signed integer (two's complement) with the given endianness.
pub fn i16(endian: Endian) -> bitty.Parser(Int) {
  u16(endian)
  |> bitty.map(fn(unsigned) {
    case unsigned >= 32_768 {
      True -> unsigned - 65_536
      False -> unsigned
    }
  })
}

/// Parse a 32-bit signed integer (two's complement) with the given endianness.
pub fn i32(endian: Endian) -> bitty.Parser(Int) {
  u32(endian)
  |> bitty.map(fn(unsigned) {
    case unsigned >= 2_147_483_648 {
      True -> unsigned - 4_294_967_296
      False -> unsigned
    }
  })
}

/// Parse a 64-bit unsigned integer with the given endianness.
/// On the JavaScript target, values above 2^53 - 1 (9,007,199,254,740,991)
/// may lose precision due to IEEE 754 double-precision limitations.
pub fn u64(endian: Endian) -> bitty.Parser(Int) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 8) {
      bitty.Continue(<<b0, b1, b2, b3, b4, b5, b6, b7>>, new_state, consumed) -> {
        let #(hi, lo) = case endian {
          BigEndian -> #(
            b0 * 16_777_216 + b1 * 65_536 + b2 * 256 + b3,
            b4 * 16_777_216 + b5 * 65_536 + b6 * 256 + b7,
          )
          LittleEndian -> #(
            b7 * 16_777_216 + b6 * 65_536 + b5 * 256 + b4,
            b3 * 16_777_216 + b2 * 65_536 + b1 * 256 + b0,
          )
        }
        bitty.Continue(hi * 4_294_967_296 + lo, new_state, consumed)
      }
      bitty.Continue(_, _, _) -> bitty.stop_expected(state, "8 bytes")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse a 64-bit signed integer (two's complement) with the given endianness.
/// On the JavaScript target, values outside the safe integer range
/// (-2^53 + 1 to 2^53 - 1) may lose precision.
pub fn i64(endian: Endian) -> bitty.Parser(Int) {
  u64(endian)
  |> bitty.map(fn(unsigned) {
    case unsigned >= 9_223_372_036_854_775_808 {
      True -> unsigned - 18_446_744_073_709_551_616
      False -> unsigned
    }
  })
}

/// Parse a 32-bit IEEE 754 float with the given endianness.
pub fn f32(endian: Endian) -> bitty.Parser(Float) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 4) {
      bitty.Continue(<<b0, b1, b2, b3>>, new_state, consumed) -> {
        let ordered = case endian {
          BigEndian -> <<b0, b1, b2, b3>>
          LittleEndian -> <<b3, b2, b1, b0>>
        }
        case ordered {
          <<value:float-size(32)>> -> bitty.Continue(value, new_state, consumed)
          _ -> bitty.stop_expected(state, "valid f32")
        }
      }
      bitty.Continue(_, _, _) -> bitty.stop_expected(state, "4 bytes")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse a 64-bit IEEE 754 double with the given endianness.
pub fn f64(endian: Endian) -> bitty.Parser(Float) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, 8) {
      bitty.Continue(<<b0, b1, b2, b3, b4, b5, b6, b7>>, new_state, consumed) -> {
        let ordered = case endian {
          BigEndian -> <<b0, b1, b2, b3, b4, b5, b6, b7>>
          LittleEndian -> <<b7, b6, b5, b4, b3, b2, b1, b0>>
        }
        case ordered {
          <<value:float>> -> bitty.Continue(value, new_state, consumed)
          _ -> bitty.stop_expected(state, "valid f64")
        }
      }
      bitty.Continue(_, _, _) -> bitty.stop_expected(state, "8 bytes")
      bitty.Stop(error, consumed, committed) ->
        bitty.Stop(error, consumed, committed)
    }
  })
}

/// Parse an LEB128-encoded unsigned 32-bit variable-length integer.
/// Consumes at most 5 bytes.
///
/// ```gleam
/// let assert Ok(val) = bitty.run(num.var_u32(), on: <<0xAC, 0x02>>)
/// assert val == 300
/// ```
pub fn var_u32() -> bitty.Parser(Int) {
  bitty.make_parser(varint_loop(_, 0, 0, 0, 5, 4, 0x0F, "u32"))
}

/// Parse an LEB128-encoded unsigned 64-bit variable-length integer.
/// Consumes at most 10 bytes.
/// On the JavaScript target, values above 2^53 - 1 (9,007,199,254,740,991)
/// may lose precision due to IEEE 754 double-precision limitations.
pub fn var_u64() -> bitty.Parser(Int) {
  bitty.make_parser(varint_loop(_, 0, 0, 0, 10, 9, 0x01, "u64"))
}

fn varint_loop(
  state: bitty.State,
  acc: Int,
  shift: Int,
  bytes_read: Int,
  max_bytes: Int,
  last_byte_index: Int,
  last_byte_max: Int,
  label: String,
) -> bitty.Step(Int) {
  use <- bool.guard(
    when: bytes_read >= max_bytes,
    return: varint_overflow(
      state,
      "varint within " <> int.to_string(max_bytes) <> " bytes",
    ),
  )
  case bitty.read_n_bytes(state, 1) {
    bitty.Continue(<<byte>>, byte_state, _) -> {
      let value = int.bitwise_and(byte, 0x7F)
      use <- bool.guard(
        when: bytes_read == last_byte_index && value > last_byte_max,
        return: varint_overflow(
          byte_state,
          "varint within " <> label <> " range",
        ),
      )
      let new_acc = int.bitwise_or(acc, int.bitwise_shift_left(value, shift))
      case int.bitwise_and(byte, 0x80) != 0 {
        True ->
          varint_loop(
            byte_state,
            new_acc,
            shift + 7,
            bytes_read + 1,
            max_bytes,
            last_byte_index,
            last_byte_max,
            label,
          )
        False -> bitty.Continue(new_acc, byte_state, True)
      }
    }
    bitty.Continue(_, _, _) -> bitty.stop_expected(state, "varint byte")
    bitty.Stop(error, consumed, committed) ->
      bitty.Stop(error, consumed, committed)
  }
}

fn varint_overflow(state: bitty.State, expected: String) -> bitty.Step(a) {
  bitty.Stop(
    bitty.BittyError(
      at: bitty.Location(byte: state.byte_offset, bit: state.bit_offset),
      expected: [expected],
      context: [],
      message: None,
    ),
    True,
    state.committed,
  )
}

/// Read `count` bytes as a raw `BitArray` representing an unsigned integer.
/// Unlike `bytes.take`, this is intended for numeric byte sequences
/// (e.g. ASN.1 DER integer encodings) where the bytes represent a single
/// big-endian value.
pub fn uint_bytes(bytes count: Int) -> bitty.Parser(BitArray) {
  bitty.make_parser(bitty.read_n_bytes(_, count))
}

/// Read `count` bytes as a two's complement signed integer `BitArray`,
/// stripping redundant leading zero bytes while preserving the sign bit.
/// Returns the normalized raw bytes, not a decoded `Int`.
/// Useful for ASN.1 DER integer encoding.
pub fn int_bytes_twos_complement(bytes count: Int) -> bitty.Parser(BitArray) {
  uint_bytes(bytes: count)
  |> bitty.map(strip_leading_zeros)
}

fn strip_leading_zeros(bytes: BitArray) -> BitArray {
  case bytes {
    <<0x00, next, _:bits>> if next >= 0x80 -> bytes
    <<0x00, _, _:bits>> ->
      case bit_array.slice(bytes, 1, bit_array.byte_size(bytes) - 1) {
        Ok(rest) -> strip_leading_zeros(rest)
        _ -> bytes
      }
    _ -> bytes
  }
}
