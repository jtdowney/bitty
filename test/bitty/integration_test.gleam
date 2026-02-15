import bitty
import bitty/bits
import bitty/bytes as b
import bitty/num
import gleam/int

pub type Tlv {
  Tlv(tag: Int, length: Int, value: BitArray)
}

fn der_tlv() -> bitty.Parser(Tlv) {
  use tag <- bitty.then(num.u8())
  use len_byte <- bitty.then(num.u8())
  case len_byte < 0x80 {
    True ->
      b.take(len_byte)
      |> bitty.map(fn(value) { Tlv(tag, len_byte, value) })
    False -> {
      let n = int.bitwise_and(len_byte, 0x7F)
      use len_bytes <- bitty.then(num.uint_bytes(n))
      let length = decode_be_int(len_bytes, 0)
      b.take(length)
      |> bitty.map(fn(value) { Tlv(tag, length, value) })
    }
  }
}

fn decode_be_int(bytes: BitArray, acc: Int) -> Int {
  case bytes {
    <<byte, rest:bytes>> -> decode_be_int(rest, acc * 256 + byte)
    _ -> acc
  }
}

pub fn der_tlv_short_form_test() {
  let input = <<0x02, 0x02, 0x01, 0x02>>
  let result = bitty.run(der_tlv(), on: input)
  assert result == Ok(Tlv(tag: 0x02, length: 2, value: <<0x01, 0x02>>))
}

pub fn der_tlv_long_form_test() {
  let value_bytes = <<0:size(1024)>>
  let input = <<0x30, 0x81, 0x80, value_bytes:bits>>
  let assert Ok(tlv) = bitty.run(der_tlv(), on: input)
  assert tlv.tag == 0x30
  assert tlv.length == 128
}

pub fn within_bytes_tlv_test() {
  let inner_data = <<0x01, 0x02>>
  let input = <<0x02, 0x02, inner_data:bits, 0xFF>>
  let parser =
    bitty.within_bytes(4, der_tlv())
    |> bitty.then(fn(tlv) {
      num.u8() |> bitty.map(fn(trailer) { #(tlv, trailer) })
    })
  let result = bitty.run(parser, on: input)
  assert result == Ok(#(Tlv(tag: 0x02, length: 2, value: <<0x01, 0x02>>), 0xFF))
}

pub fn use_syntax_test() {
  let parser = {
    use first <- bitty.then(num.u8())
    use second <- bitty.then(num.u8())
    bitty.success(#(first, second))
  }
  let result = bitty.run(parser, on: <<0xAA, 0xBB>>)
  assert result == Ok(#(0xAA, 0xBB))
}

pub fn bit_field_parsing_test() {
  let parser = {
    use type_val <- bitty.then(bits.uint(3))
    use payload <- bitty.then(bits.uint(5))
    bitty.success(#(type_val, payload))
  }
  let result = bitty.run(parser, on: <<0xAB>>)
  assert result == Ok(#(5, 11))
}
