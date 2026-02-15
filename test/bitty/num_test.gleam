import bitty
import bitty/bits
import bitty/num
import qcheck

pub fn u16_endian_round_trip_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(0, 65_535), fn(val) {
    let b0 = val / 256
    let b1 = val % 256
    let be_result = bitty.run(num.u16(num.BigEndian), on: <<b0, b1>>)
    let le_result = bitty.run(num.u16(num.LittleEndian), on: <<b1, b0>>)
    assert be_result == Ok(val)
    assert le_result == Ok(val)
  })
}

pub fn u16_insufficient_input_fails_test() {
  let assert Error(_) = bitty.run(num.u16(num.BigEndian), on: <<0x01>>)
}

pub fn u32_endian_round_trip_test() {
  qcheck.run(
    qcheck.default_config(),
    qcheck.bounded_int(0, 4_294_967_295),
    fn(val) {
      let b0 = val / 16_777_216
      let b1 = val % 16_777_216 / 65_536
      let b2 = val % 65_536 / 256
      let b3 = val % 256
      let be_result = bitty.run(num.u32(num.BigEndian), on: <<b0, b1, b2, b3>>)
      let le_result =
        bitty.run(num.u32(num.LittleEndian), on: <<b3, b2, b1, b0>>)
      assert be_result == Ok(val)
      assert le_result == Ok(val)
    },
  )
}

pub fn u32_insufficient_input_fails_test() {
  let assert Error(_) =
    bitty.run(num.u32(num.BigEndian), on: <<0x01, 0x02, 0x03>>)
}

pub fn i8_roundtrip_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(-128, 127), fn(val) {
    let byte = case val < 0 {
      True -> val + 256
      False -> val
    }
    let result = bitty.run(num.i8(), on: <<byte>>)
    assert result == Ok(val)
  })
}

pub fn i16_endian_roundtrip_test() {
  qcheck.run(
    qcheck.default_config(),
    qcheck.bounded_int(-32_768, 32_767),
    fn(val) {
      let unsigned = case val < 0 {
        True -> val + 65_536
        False -> val
      }
      let b0 = unsigned / 256
      let b1 = unsigned % 256
      let be_result = bitty.run(num.i16(num.BigEndian), on: <<b0, b1>>)
      let le_result = bitty.run(num.i16(num.LittleEndian), on: <<b1, b0>>)
      assert be_result == Ok(val)
      assert le_result == Ok(val)
    },
  )
}

pub fn i32_endian_roundtrip_test() {
  qcheck.run(
    qcheck.default_config(),
    qcheck.bounded_int(-2_147_483_648, 2_147_483_647),
    fn(val) {
      let unsigned = case val < 0 {
        True -> val + 4_294_967_296
        False -> val
      }
      let b0 = unsigned / 16_777_216
      let b1 = unsigned % 16_777_216 / 65_536
      let b2 = unsigned % 65_536 / 256
      let b3 = unsigned % 256
      let be_result = bitty.run(num.i32(num.BigEndian), on: <<b0, b1, b2, b3>>)
      let le_result =
        bitty.run(num.i32(num.LittleEndian), on: <<b3, b2, b1, b0>>)
      assert be_result == Ok(val)
      assert le_result == Ok(val)
    },
  )
}

pub fn u64_be_test() {
  let result = bitty.run(num.u64(num.BigEndian), on: <<0, 0, 0, 1, 0, 0, 0, 0>>)
  assert result == Ok(4_294_967_296)
}

pub fn u64_le_test() {
  let result =
    bitty.run(num.u64(num.LittleEndian), on: <<1, 0, 0, 0, 0, 0, 0, 0>>)
  assert result == Ok(1)
}

pub fn u64_insufficient_input_fails_test() {
  let assert Error(_) =
    bitty.run(num.u64(num.BigEndian), on: <<0x01, 0x02, 0x03, 0x04>>)
}

pub fn i64_positive_test() {
  let result = bitty.run(num.i64(num.BigEndian), on: <<0, 0, 0, 0, 0, 0, 0, 1>>)
  assert result == Ok(1)
}

@target(erlang)
pub fn i64_negative_one_test() {
  let result =
    bitty.run(num.i64(num.BigEndian), on: <<
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
    >>)
  assert result == Ok(-1)
}

pub fn f32_be_one_test() {
  let result = bitty.run(num.f32(num.BigEndian), on: <<0x3F, 0x80, 0x00, 0x00>>)
  assert result == Ok(1.0)
}

pub fn f32_le_one_test() {
  let result =
    bitty.run(num.f32(num.LittleEndian), on: <<0x00, 0x00, 0x80, 0x3F>>)
  assert result == Ok(1.0)
}

pub fn f32_negative_and_zero_test() {
  let neg_result =
    bitty.run(num.f32(num.BigEndian), on: <<0xBF, 0x80, 0x00, 0x00>>)
  assert neg_result == Ok(-1.0)

  let zero_result =
    bitty.run(num.f32(num.BigEndian), on: <<0x00, 0x00, 0x00, 0x00>>)
  assert zero_result == Ok(0.0)
}

pub fn f32_insufficient_input_fails_test() {
  let assert Error(_) = bitty.run(num.f32(num.BigEndian), on: <<0x3F, 0x80>>)
}

pub fn f64_be_one_test() {
  let result =
    bitty.run(num.f64(num.BigEndian), on: <<
      0x3F,
      0xF0,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
    >>)
  assert result == Ok(1.0)
}

pub fn f64_le_one_test() {
  let result =
    bitty.run(num.f64(num.LittleEndian), on: <<
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0x00,
      0xF0,
      0x3F,
    >>)
  assert result == Ok(1.0)
}

pub fn f64_negative_and_zero_test() {
  let neg_result =
    bitty.run(num.f64(num.BigEndian), on: <<
      0xBF, 0xF0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    >>)
  assert neg_result == Ok(-1.0)

  let zero_result =
    bitty.run(num.f64(num.BigEndian), on: <<
      0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    >>)
  assert zero_result == Ok(0.0)
}

pub fn f64_insufficient_input_fails_test() {
  let assert Error(_) =
    bitty.run(num.f64(num.BigEndian), on: <<0x3F, 0xF0, 0x00, 0x00>>)
}

pub fn var_u32_single_byte_test() {
  let result = bitty.run(num.var_u32(), on: <<0x05>>)
  assert result == Ok(5)
}

pub fn var_u32_multi_byte_test() {
  let result = bitty.run(num.var_u32(), on: <<0xAC, 0x02>>)
  assert result == Ok(300)
}

pub fn var_u32_zero_test() {
  let result = bitty.run(num.var_u32(), on: <<0x00>>)
  assert result == Ok(0)
}

pub fn var_u32_max_valid_test() {
  let result = bitty.run(num.var_u32(), on: <<0xFF, 0xFF, 0xFF, 0xFF, 0x0F>>)
  assert result == Ok(4_294_967_295)
}

pub fn var_u32_overflow_test() {
  let result = bitty.run(num.var_u32(), on: <<0xFF, 0xFF, 0xFF, 0xFF, 0x1F>>)
  let assert Error(_) = result
}

pub fn var_u32_too_many_bytes_test() {
  let assert Error(_) =
    bitty.run(num.var_u32(), on: <<0x80, 0x80, 0x80, 0x80, 0x80, 0x01>>)
}

@target(erlang)
pub fn var_u64_max_valid_test() {
  let result =
    bitty.run(num.var_u64(), on: <<
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0x01,
    >>)
  assert result == Ok(18_446_744_073_709_551_615)
}

pub fn var_u64_overflow_test() {
  let result =
    bitty.run(num.var_u64(), on: <<
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0xFF,
      0x02,
    >>)
  let assert Error(_) = result
}

pub fn uint_bytes_test() {
  let result = bitty.run(num.uint_bytes(3), on: <<0x01, 0x02, 0x03>>)
  assert result == Ok(<<0x01, 0x02, 0x03>>)
}

pub fn int_bytes_twos_complement_positive_strips_leading_zeros_test() {
  let result =
    bitty.run(num.int_bytes_twos_complement(3), on: <<0x00, 0x01, 0x02>>)
  assert result == Ok(<<0x01, 0x02>>)
}

pub fn int_bytes_twos_complement_negative_kept_test() {
  let result = bitty.run(num.int_bytes_twos_complement(2), on: <<0xFF, 0xFE>>)
  assert result == Ok(<<0xFF, 0xFE>>)
}

pub fn int_bytes_twos_complement_preserves_sign_bit_test() {
  let result = bitty.run(num.int_bytes_twos_complement(2), on: <<0x00, 0x80>>)
  assert result == Ok(<<0x00, 0x80>>)
}

pub fn int_bytes_twos_complement_multiple_leading_zeros_test() {
  let result =
    bitty.run(num.int_bytes_twos_complement(4), on: <<0x00, 0x00, 0x01, 0x02>>)
  assert result == Ok(<<0x01, 0x02>>)
}

pub fn int_bytes_twos_complement_single_byte_unchanged_test() {
  let result = bitty.run(num.int_bytes_twos_complement(1), on: <<0x42>>)
  assert result == Ok(<<0x42>>)
}

pub fn int_bytes_twos_complement_all_zeros_test() {
  let result =
    bitty.run(num.int_bytes_twos_complement(3), on: <<0x00, 0x00, 0x00>>)
  assert result == Ok(<<0x00>>)
}

pub fn int_bytes_twos_complement_chained_sign_bit_preservation_test() {
  let result =
    bitty.run(num.int_bytes_twos_complement(3), on: <<0x00, 0x00, 0x80>>)
  assert result == Ok(<<0x00, 0x80>>)
}

pub fn u16_be_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use val <- bitty.then(num.u16(num.BigEndian))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(val)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == Ok(0xBCDE)
}

pub fn u16_le_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use val <- bitty.then(num.u16(num.LittleEndian))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(val)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == Ok(0xDEBC)
}

pub fn u32_be_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use val <- bitty.then(num.u32(num.BigEndian))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(val)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD, 0xEF, 0x12, 0x34>>)
  assert result == Ok(0xBCDEF123)
}

pub fn var_u32_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use val <- bitty.then(num.var_u32())
    use _ <- bitty.then(bits.uint(4))
    bitty.success(val)
  }
  let result = bitty.run(parser, on: <<0xA0, 0x50>>)
  assert result == Ok(5)
}
