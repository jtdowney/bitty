import bitty
import bitty/bits
import bitty/bytes as b
import bitty/num
import bitty/string as s
import gleam/bit_array
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

pub fn mixed_bit_and_byte_parsing_test() {
  let parser = {
    use flag <- bitty.then(bits.bit())
    use _ <- bitty.then(bits.align())
    use value <- bitty.then(num.u8())
    bitty.success(#(flag, value))
  }
  let result = bitty.run(parser, on: <<0x80, 0x42>>)
  assert result == Ok(#(True, 0x42))
}

pub fn many_u8_test() {
  let parser = bitty.many(num.u8())
  let result = bitty.run(parser, on: <<1, 2, 3, 4, 5>>)
  assert result == Ok([1, 2, 3, 4, 5])
}

pub type HttpRequest {
  HttpRequest(method: String, path: String, version: String)
}

fn http_request_line() -> bitty.Parser(HttpRequest) {
  use method <- bitty.then(s.alpha1())
  use _ <- bitty.then(s.space1())
  use path <- bitty.then(s.take_while1(fn(c) { c != " " }))
  use _ <- bitty.then(s.space1())
  use version <- bitty.then(s.not_line_ending())
  use _ <- bitty.then(s.crlf())
  bitty.success(HttpRequest(method, path, version))
}

pub fn http_request_line_test() {
  let input = bit_array.from_string("GET /index.html HTTP/1.1\r\n")
  let result = bitty.run(http_request_line(), on: input)
  assert result
    == Ok(HttpRequest(method: "GET", path: "/index.html", version: "HTTP/1.1"))
}

pub fn length_prefixed_string_test() {
  let parser = {
    use len <- bitty.then(num.u8())
    use value <- bitty.then(s.utf8(len))
    bitty.success(value)
  }
  let input = <<5, 0x68, 0x65, 0x6C, 0x6C, 0x6F>>
  let result = bitty.run(parser, on: input)
  assert result == Ok("hello")
}

pub fn binary_header_string_payload_test() {
  let parser = {
    use tag <- bitty.then(num.u8())
    use len <- bitty.then(num.u16(num.BigEndian))
    use payload <- bitty.then(s.utf8(len))
    bitty.success(#(tag, payload))
  }
  let input = <<0x01, 0x00, 0x05, 0x68, 0x65, 0x6C, 0x6C, 0x6F>>
  let result = bitty.run(parser, on: input)
  assert result == Ok(#(1, "hello"))
}
