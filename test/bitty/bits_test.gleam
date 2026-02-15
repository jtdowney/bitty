import bitty
import bitty/bits
import bitty/num
import gleam/int
import qcheck

pub fn bit_true_test() {
  let assert Ok(#(value, _rest)) = bitty.run_partial(bits.bit(), on: <<0x80>>)
  assert value == True
}

pub fn bit_false_test() {
  let assert Ok(#(value, _rest)) = bitty.run_partial(bits.bit(), on: <<0x00>>)
  assert value == False
}

pub fn bit_sequence_test() {
  let parser = {
    use b0 <- bitty.then(bits.bit())
    use b1 <- bitty.then(bits.bit())
    use b2 <- bitty.then(bits.bit())
    use b3 <- bitty.then(bits.bit())
    bitty.success(#(b0, b1, b2, b3))
  }
  let assert Ok(#(value, _rest)) = bitty.run_partial(parser, on: <<0xA5>>)
  assert value == #(True, False, True, False)
}

pub fn uint_4bits_test() {
  let assert Ok(#(value, _rest)) = bitty.run_partial(bits.uint(4), on: <<0xAB>>)
  assert value == 10
}

pub fn uint_12bits_test() {
  let assert Ok(#(value, _rest)) =
    bitty.run_partial(bits.uint(12), on: <<0xAB, 0xC0>>)
  assert value == 0xABC
}

pub fn uint_negative_bits_fails_test() {
  let assert Error(_) = bitty.run_partial(bits.uint(-1), on: <<0xFF>>)
}

pub fn uint_various_widths_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(1, 8), fn(width) {
    let max_val = int.bitwise_shift_left(1, width) - 1
    let byte = int.bitwise_shift_left(max_val, 8 - width)
    let assert Ok(#(value, _rest)) =
      bitty.run_partial(bits.uint(width), on: <<byte>>)
    assert value == max_val
  })
}

pub fn uint_36bits_test() {
  let assert Ok(#(value, _rest)) =
    bitty.run_partial(bits.uint(36), on: <<0xFF, 0xFF, 0xFF, 0xFF, 0xF0>>)
  assert value == 68_719_476_735
}

pub fn align_skips_remaining_bits_test() {
  let parser = {
    use val <- bitty.then(bits.uint(3))
    use _ <- bitty.then(bitty.align())
    bitty.success(val)
  }
  let result = bitty.run(parser, on: <<0xE0>>)
  assert result == Ok(7)
}

pub fn bit_then_byte_after_align_test() {
  let parser = {
    use flag <- bitty.then(bits.bit())
    use _ <- bitty.then(bitty.align())
    use byte <- bitty.then(num.u8())
    bitty.success(#(flag, byte))
  }
  let result = bitty.run(parser, on: <<0x80, 0xAB>>)
  assert result == Ok(#(True, 0xAB))
}

pub fn uint_partial_consumption_no_backtrack_test() {
  let parser = bitty.one_of([bits.uint(bits: 12), bitty.success(42)])
  let result = bitty.run(parser, on: <<0xAB>>)
  assert result != Ok(42)
}
