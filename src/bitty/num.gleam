//// Numeric parsers for fixed-width integers, floats, and variable-length
//// encodings. Includes single-byte (`u8`, `i8`) and multi-byte types with
//// endianness control.

import bitty
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/option

/// Byte order for multi-byte numeric parsers.
pub type Endian {
  BigEndian
  LittleEndian
}

/// Parse a single byte as an unsigned integer (0–255).
pub fn u8() -> bitty.Parser(Int) {
  fixed_int(1, BigEndian, Unsigned)
}

/// Parse a single byte as a signed integer (-128–127).
pub fn i8() -> bitty.Parser(Int) {
  fixed_int(1, BigEndian, Signed)
}

/// Parse a 16-bit unsigned integer with the given endianness.
///
/// ```gleam
/// let assert Ok(val) =
///   bitty.run(num.u16(num.BigEndian), on: <<0x01, 0x00>>)
/// assert val == 256
/// ```
pub fn u16(endian: Endian) -> bitty.Parser(Int) {
  fixed_int(2, endian, Unsigned)
}

/// Parse a 32-bit unsigned integer with the given endianness.
pub fn u32(endian: Endian) -> bitty.Parser(Int) {
  fixed_int(4, endian, Unsigned)
}

/// Parse a 16-bit signed integer (two's complement) with the given endianness.
pub fn i16(endian: Endian) -> bitty.Parser(Int) {
  fixed_int(2, endian, Signed)
}

/// Parse a 32-bit signed integer (two's complement) with the given endianness.
pub fn i32(endian: Endian) -> bitty.Parser(Int) {
  fixed_int(4, endian, Signed)
}

/// Parse a 64-bit unsigned integer with the given endianness.
/// On the JavaScript target, values above 2^53 - 1 (9,007,199,254,740,991)
/// may lose precision due to IEEE 754 double-precision limitations.
pub fn u64(endian: Endian) -> bitty.Parser(Int) {
  fixed_int(8, endian, Unsigned)
}

/// Parse a 64-bit signed integer (two's complement) with the given endianness.
/// On the JavaScript target, values outside the safe integer range
/// (-2^53 + 1 to 2^53 - 1) may lose precision.
pub fn i64(endian: Endian) -> bitty.Parser(Int) {
  fixed_int(8, endian, Signed)
}

/// Parse a 32-bit IEEE 754 float with the given endianness.
pub fn f32(endian: Endian) -> bitty.Parser(Float) {
  fixed_size(4, decode_f32(_, endian))
}

/// Parse a 64-bit IEEE 754 double with the given endianness.
pub fn f64(endian: Endian) -> bitty.Parser(Float) {
  fixed_size(8, decode_f64(_, endian))
}

type Signedness {
  Signed
  Unsigned
}

fn fixed_int(
  count: Int,
  endian: Endian,
  signedness: Signedness,
) -> bitty.Parser(Int) {
  let bits = count * 8
  fixed_size(count, decode_int(_, bits, endian, signedness))
}

fn decode_int(
  raw: BitArray,
  bits: Int,
  endian: Endian,
  signedness: Signedness,
) -> Result(Int, Nil) {
  case endian, signedness, raw {
    BigEndian, Unsigned, <<value:unsigned-big-size(bits)>> -> Ok(value)
    BigEndian, Signed, <<value:signed-big-size(bits)>> -> Ok(value)
    LittleEndian, Unsigned, <<value:unsigned-little-size(bits)>> -> Ok(value)
    LittleEndian, Signed, <<value:signed-little-size(bits)>> -> Ok(value)
    _, _, _ -> Error(Nil)
  }
}

fn decode_f32(raw: BitArray, endian: Endian) -> Result(Float, Nil) {
  case endian, raw {
    BigEndian, <<value:float-big-size(32)>> -> Ok(value)
    LittleEndian, <<value:float-little-size(32)>> -> Ok(value)
    _, _ -> Error(Nil)
  }
}

fn decode_f64(raw: BitArray, endian: Endian) -> Result(Float, Nil) {
  case endian, raw {
    BigEndian, <<value:float-big-size(64)>> -> Ok(value)
    LittleEndian, <<value:float-little-size(64)>> -> Ok(value)
    _, _ -> Error(Nil)
  }
}

fn fixed_size(
  count: Int,
  decode: fn(BitArray) -> Result(a, Nil),
) -> bitty.Parser(a) {
  bitty.make_parser(fn(state: bitty.State) {
    case bitty.read_n_bytes(state, count) {
      bitty.Continue(raw, new_state, consumed) ->
        case decode(raw) {
          Ok(value) -> bitty.Continue(value, new_state, consumed)
          Error(Nil) ->
            bitty.stop_expected(state, int.to_string(count) <> " bytes")
        }
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
      message: option.None,
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
