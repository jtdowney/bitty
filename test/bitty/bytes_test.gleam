import bitty
import bitty/bits
import bitty/bytes as b
import bitty/num
import gleam/bit_array
import gleam/list
import qcheck

pub fn u8_reads_any_byte_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(0, 255), fn(byte) {
    let result = bitty.run(num.u8(), on: <<byte>>)
    assert result == Ok(byte)
  })
}

pub fn u8_empty_input_fails_test() {
  let result = bitty.run(num.u8(), on: <<>>)
  let assert Error(_) = result
}

pub fn byte_returns_single_byte_test() {
  let result = bitty.run(b.byte(), on: <<0xAB>>)
  assert result == Ok(<<0xAB>>)
}

pub fn byte_fails_on_empty_input_test() {
  let assert Error(_) = bitty.run(b.byte(), on: <<>>)
}

pub fn take_reads_exact_bytes_test() {
  let result = bitty.run(b.take(3), on: <<1, 2, 3>>)
  assert result == Ok(<<1, 2, 3>>)
}

pub fn take_zero_on_empty_test() {
  let result = bitty.run(b.take(0), on: <<>>)
  assert result == Ok(<<>>)
}

pub fn take_insufficient_input_fails_test() {
  let result = bitty.run(b.take(5), on: <<1, 2, 3>>)
  let assert Error(_) = result
}

pub fn take_roundtrip_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(0, 64), fn(n) {
    let bytes = build_bytes(n, <<>>)
    let result = bitty.run(b.take(n), on: bytes)
    assert result == Ok(bytes)
  })
}

pub fn skip_advances_past_bytes_test() {
  let parser = b.skip(2) |> bitty.then(fn(_) { num.u8() })
  let result = bitty.run(parser, on: <<0xAA, 0xBB, 0xCC>>)
  assert result == Ok(0xCC)
}

pub fn skip_negative_fails_test() {
  let assert Error(_) = bitty.run_partial(b.skip(-1), on: <<1, 2, 3>>)
}

pub fn skip_zero_is_noop_test() {
  let parser = b.skip(0) |> bitty.then(fn(_) { num.u8() })
  let result = bitty.run(parser, on: <<0xAB>>)
  assert result == Ok(0xAB)
}

pub fn skip_exact_input_size_test() {
  let result = bitty.run(b.skip(3), on: <<1, 2, 3>>)
  assert result == Ok(Nil)
}

pub fn skip_exceeds_input_fails_test() {
  let assert Error(_) = bitty.run(b.skip(4), on: <<1, 2, 3>>)
}

pub fn skip_then_rest_partition_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(0, 32), fn(k) {
    let total = k + 4
    let input = build_bytes(total, <<>>)
    let parser = b.skip(k) |> bitty.then(fn(_) { b.rest() })
    let assert Ok(rest) = bitty.run(parser, on: input)
    assert bit_array.byte_size(rest) == total - k
  })
}

pub fn rest_returns_remaining_bytes_test() {
  let parser = num.u8() |> bitty.then(fn(_) { b.rest() })
  let result = bitty.run(parser, on: <<0x01, 0x02, 0x03>>)
  assert result == Ok(<<0x02, 0x03>>)
}

pub fn rest_empty_returns_empty_test() {
  let result = bitty.run(b.rest(), on: <<>>)
  assert result == Ok(<<>>)
}

pub fn rest_all_bytes_test() {
  let result = bitty.run(b.rest(), on: <<1, 2, 3>>)
  assert result == Ok(<<1, 2, 3>>)
}

pub fn peek_does_not_advance_test() {
  let parser =
    b.peek(2)
    |> bitty.then(fn(peeked) {
      b.take(3) |> bitty.map(fn(taken) { #(peeked, taken) })
    })
  let result = bitty.run(parser, on: <<1, 2, 3>>)
  assert result == Ok(#(<<1, 2>>, <<1, 2, 3>>))
}

pub fn tag_matches_prefix_test() {
  let parser =
    b.tag(<<0x89, 0x50, 0x4E, 0x47>>) |> bitty.then(fn(_) { b.rest() })
  let result = bitty.run(parser, on: <<0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A>>)
  assert result == Ok(<<0x0D, 0x0A>>)
}

pub fn tag_mismatch_fails_test() {
  let result = bitty.run(b.tag(<<0x89, 0x50>>), on: <<0x89, 0x51>>)
  let assert Error(_) = result
}

pub fn tag_empty_pattern_test() {
  let result = bitty.run(b.tag(<<>>), on: <<>>)
  assert result == Ok(Nil)
}

pub fn peek_insufficient_input_fails_test() {
  let assert Error(_) = bitty.run_partial(b.peek(3), on: <<0x01, 0x02>>)
}

pub fn u8_after_bits_test() {
  let parser = {
    use _flags <- bitty.then(bits.uint(4))
    use byte <- bitty.then(num.u8())
    use _trail <- bitty.then(bits.uint(4))
    bitty.success(byte)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD>>)
  assert result == Ok(0xBC)
}

pub fn take_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use bytes <- bitty.then(b.take(2))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(bytes)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == Ok(<<0xBC, 0xDE>>)
}

pub fn skip_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use _ <- bitty.then(b.skip(1))
    use byte <- bitty.then(num.u8())
    use _ <- bitty.then(bits.uint(4))
    bitty.success(byte)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == Ok(0xDE)
}

pub fn peek_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use peeked <- bitty.then(b.peek(1))
    use taken <- bitty.then(b.take(1))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(#(peeked, taken))
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD>>)
  assert result == Ok(#(<<0xBC>>, <<0xBC>>))
}

pub fn tag_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use _ <- bitty.then(b.tag(<<0xBC, 0xDE>>))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(Nil)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == Ok(Nil)
}

pub fn rest_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use remaining <- bitty.then(b.rest())
    bitty.success(remaining)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD>>)
  assert result == Ok(<<0xBC, 0xD:size(4)>>)
}

pub fn rest_after_bits_trailing_only_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use remaining <- bitty.then(b.rest())
    bitty.success(remaining)
  }
  let result = bitty.run(parser, on: <<0xAB>>)
  assert result == Ok(<<0xB:size(4)>>)
}

pub fn byte_if_match_test() {
  let result = bitty.run(b.byte_if(fn(byte) { byte == <<0xFF>> }), on: <<0xFF>>)
  assert result == Ok(<<0xFF>>)
}

pub fn byte_if_no_match_test() {
  let result = bitty.run(b.byte_if(fn(byte) { byte == <<0xFF>> }), on: <<0x00>>)
  let assert Error(_) = result
}

pub fn byte_if_empty_fails_test() {
  let result = bitty.run(b.byte_if(fn(_) { True }), on: <<>>)
  let assert Error(_) = result
}

pub fn take_while_some_match_test() {
  let assert Ok(#(result, _)) =
    bitty.run_partial(b.take_while(fn(byte) { byte != <<0x00>> }), on: <<
      1,
      2,
      0,
      3,
    >>)
  assert result == <<1, 2>>
}

pub fn take_while_all_match_test() {
  let result = bitty.run(b.take_while(fn(_) { True }), on: <<1, 2, 3>>)
  assert result == Ok(<<1, 2, 3>>)
}

pub fn take_while_none_match_test() {
  let assert Ok(#(result, _)) =
    bitty.run_partial(b.take_while(fn(_) { False }), on: <<1, 2, 3>>)
  assert result == <<>>
}

pub fn take_while_all_bytes_satisfy_predicate_pbt_test() {
  let gen =
    qcheck.generic_list(
      qcheck.bounded_int(0, 255),
      qcheck.bounded_int(0, 32),
    )
  qcheck.run(qcheck.default_config(), gen, fn(bytes) {
    let threshold = 128
    let predicate = fn(byte: BitArray) {
      let assert <<b>> = byte
      b < threshold
    }
    let input =
      list.fold(bytes, <<>>, fn(acc, b) { <<acc:bits, b:size(8)>> })
    let assert Ok(#(result, _)) =
      bitty.run_partial(b.take_while(predicate), on: input)
    assert all_bytes_below(result, threshold) == True
  })
}

pub fn take_while_empty_test() {
  let result = bitty.run(b.take_while(fn(_) { True }), on: <<>>)
  assert result == Ok(<<>>)
}

pub fn take_while1_some_match_test() {
  let assert Ok(#(result, _)) =
    bitty.run_partial(b.take_while1(fn(byte) { byte != <<0x00>> }), on: <<
      1,
      2,
      0,
      3,
    >>)
  assert result == <<1, 2>>
}

pub fn take_while1_no_match_fails_test() {
  let result =
    bitty.run(b.take_while1(fn(byte) { byte == <<0xFF>> }), on: <<0x00, 0x01>>)
  let assert Error(_) = result
}

pub fn take_while1_empty_fails_test() {
  let result = bitty.run(b.take_while1(fn(_) { True }), on: <<>>)
  let assert Error(_) = result
}

pub fn take_until_sentinel_test() {
  let assert Ok(#(result, _)) =
    bitty.run_partial(b.take_until(fn(byte) { byte == <<0x00>> }), on: <<
      1,
      2,
      0,
      3,
    >>)
  assert result == <<1, 2>>
}

pub fn take_until_empty_input_test() {
  let result = bitty.run(b.take_until(fn(_) { True }), on: <<>>)
  assert result == Ok(<<>>)
}

pub fn take_until_immediate_match_test() {
  let assert Ok(#(result, _)) =
    bitty.run_partial(b.take_until(fn(_) { True }), on: <<1, 2, 3>>)
  assert result == <<>>
}

pub fn take_until_no_sentinel_test() {
  let result =
    bitty.run(b.take_until(fn(byte) { byte == <<0x00>> }), on: <<1, 2, 3>>)
  assert result == Ok(<<1, 2, 3>>)
}

pub fn byte_if_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.byte_if(fn(byte) { byte == <<0xBC>> }))
    use _ <- bitty.then(bits.uint(4))
    bitty.success(matched)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD>>)
  assert result == Ok(<<0xBC>>)
}

pub fn byte_if_after_bits_no_match_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.byte_if(fn(byte) { byte == <<0xFF>> }))
    bitty.success(matched)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD>>)
  let assert Error(_) = result
}

pub fn take_while_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.take_while(fn(byte) { byte != <<0xDE>> }))
    bitty.success(matched)
  }
  // After 4 bits of 0xAB, remaining bits produce bytes 0xBC, 0xDE...
  let assert Ok(#(result, _)) =
    bitty.run_partial(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == <<0xBC>>
}

pub fn take_while_after_bits_all_match_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.take_while(fn(_) { True }))
    bitty.success(matched)
  }
  // After 4 bits, only 1 full byte (0xBC) can be read; trailing 4 bits are
  // not a complete byte so take_while stops.
  let assert Ok(#(result, _)) = bitty.run_partial(parser, on: <<0xAB, 0xCD>>)
  assert result == <<0xBC>>
}

pub fn take_while1_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.take_while1(fn(byte) { byte != <<0xDE>> }))
    bitty.success(matched)
  }
  let assert Ok(#(result, _)) =
    bitty.run_partial(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == <<0xBC>>
}

pub fn take_while1_after_bits_no_match_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.take_while1(fn(_) { False }))
    bitty.success(matched)
  }
  let result = bitty.run(parser, on: <<0xAB, 0xCD>>)
  let assert Error(_) = result
}

pub fn take_until_after_bits_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(4))
    use matched <- bitty.then(b.take_until(fn(byte) { byte == <<0xDE>> }))
    bitty.success(matched)
  }
  let assert Ok(#(result, _)) =
    bitty.run_partial(parser, on: <<0xAB, 0xCD, 0xEF>>)
  assert result == <<0xBC>>
}

fn build_bytes(n: Int, acc: BitArray) -> BitArray {
  case n <= 0 {
    True -> acc
    False -> build_bytes(n - 1, <<acc:bits, 0xAA>>)
  }
}

fn all_bytes_below(data: BitArray, threshold: Int) -> Bool {
  case data {
    <<b, rest:bits>> ->
      case b < threshold {
        True -> all_bytes_below(rest, threshold)
        False -> False
      }
    _ -> True
  }
}
