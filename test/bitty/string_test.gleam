import bitty
import bitty/bits
import bitty/num
import bitty/string as s
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/string
import qcheck

pub fn utf8_round_trip_test() {
  qcheck.run(qcheck.default_config(), qcheck.non_empty_string(), fn(original) {
    let bytes = bit_array.from_string(original)
    let byte_len = bit_array.byte_size(bytes)
    let result = bitty.run(s.utf8(byte_len), on: bytes)
    assert result == Ok(original)
  })
}

pub fn utf8_invalid_bytes_test() {
  let assert Error(_) = bitty.run(s.utf8(2), on: <<0xFF, 0xFE>>)
}

pub fn literal_match_then_continue_test() {
  let input = bit_array.from_string("GET /index")
  let parser =
    s.literal("GET")
    |> bitty.then(fn(_) { s.utf8(7) })
  let result = bitty.run(parser, on: input)
  assert result == Ok(" /index")
}

pub fn literal_mismatch_test() {
  let input = bit_array.from_string("POST")
  let assert Error(_) = bitty.run_partial(s.literal("GET"), on: input)
}

pub fn null_terminated_round_trip_test() {
  qcheck.run(
    qcheck.default_config(),
    qcheck.non_empty_string_from(qcheck.alphabetic_ascii_codepoint()),
    fn(original) {
      let bytes = bit_array.from_string(original)
      let input = <<bytes:bits, 0x00>>
      let result = bitty.run(s.null_terminated(), on: input)
      assert result == Ok(original)
    },
  )
}

pub fn null_terminated_then_more_data_test() {
  let parser =
    s.null_terminated()
    |> bitty.then(fn(str) { num.u8() |> bitty.map(fn(byte) { #(str, byte) }) })
  let result = bitty.run(parser, on: <<0x68, 0x69, 0x00, 0xFF>>)
  assert result == Ok(#("hi", 0xFF))
}

pub fn null_terminated_no_null_fails_test() {
  let assert Error(_) = bitty.run(s.null_terminated(), on: <<0x68, 0x69>>)
}

pub fn fixed_round_trip_test() {
  let gen = qcheck.non_empty_string_from(qcheck.alphabetic_ascii_codepoint())
  qcheck.run(qcheck.default_config(), gen, fn(original) {
    let bytes = bit_array.from_string(original)
    let byte_len = bit_array.byte_size(bytes)
    let padded = <<bytes:bits, 0x00, 0x00, 0x00>>
    let result = bitty.run(s.fixed(byte_len + 3), on: padded)
    assert result == Ok(original)
  })
}

pub fn fixed_space_padded_test() {
  let result = bitty.run(s.fixed(5), on: <<0x68, 0x69, 0x20, 0x20, 0x20>>)
  assert result == Ok("hi")
}

pub fn grapheme_round_trip_test() {
  qcheck.run(qcheck.default_config(), qcheck.non_empty_string(), fn(original) {
    let bytes = bit_array.from_string(original)
    let parser = bitty.many(s.grapheme())
    let assert Ok(graphemes) = bitty.run(parser, on: bytes)
    let reassembled = string.join(graphemes, "")
    assert reassembled == original
  })
}

pub fn grapheme_fails_on_empty_input_test() {
  let assert Error(_) = bitty.run(s.grapheme(), on: <<>>)
}

pub fn grapheme_if_backtrack_test() {
  let input = bit_array.from_string("B")
  let parser =
    bitty.one_of([
      s.grapheme_if(fn(c) { c == "A" }),
      s.grapheme_if(fn(c) { c == "B" }),
    ])
  let result = bitty.run(parser, on: input)
  assert result == Ok("B")
}

pub fn take_graphemes_round_trip_test() {
  qcheck.run(qcheck.default_config(), qcheck.non_empty_string(), fn(original) {
    let bytes = bit_array.from_string(original)
    let count =
      string.to_graphemes(original)
      |> list.length
    let result = bitty.run(s.take_graphemes(count), on: bytes)
    assert result == Ok(original)
  })
}

pub fn take_graphemes_then_bytes_test() {
  let parser =
    s.take_graphemes(3)
    |> bitty.then(fn(str) { num.u8() |> bitty.map(fn(byte) { #(str, byte) }) })
  let result = bitty.run(parser, on: <<0x68, 0x69, 0x21, 0xFF>>)
  assert result == Ok(#("hi!", 0xFF))
}

pub fn take_graphemes_zero_returns_empty_test() {
  let input = bit_array.from_string("hello")
  let result = bitty.run_partial(s.take_graphemes(0), on: input)
  assert result == Ok(#("", <<"hello">>))
}

pub fn take_while_partition_test() {
  qcheck.run(qcheck.default_config(), qcheck.non_empty_string(), fn(original) {
    let bytes = bit_array.from_string(original)
    let is_a = fn(c) { c == "a" }
    let parser =
      s.take_while(is_a)
      |> bitty.then(fn(prefix) {
        s.take_while(fn(_) { True })
        |> bitty.map(fn(suffix) { prefix <> suffix })
      })
    let result = bitty.run(parser, on: bytes)
    assert result == Ok(original)
  })
}

pub fn take_while1_no_match_fails_test() {
  let input = bit_array.from_string("bbb")
  let assert Error(_) =
    bitty.run_partial(s.take_while1(fn(c) { c == "a" }), on: input)
}

pub fn take_until_preserves_separator_test() {
  let input = bit_array.from_string("hello world")
  let parser =
    s.take_until(fn(c) { c == " " })
    |> bitty.then(fn(before) {
      s.grapheme() |> bitty.map(fn(sep) { #(before, sep) })
    })
  let result = bitty.run_partial(parser, on: input)
  assert result == Ok(#(#("hello", " "), bit_array.from_string("world")))
}

pub fn take_until_no_match_returns_all_test() {
  let input = bit_array.from_string("hello")
  let result = bitty.run(s.take_until(fn(c) { c == "!" }), on: input)
  assert result == Ok("hello")
}

pub fn alpha_empty_on_no_match_test() {
  let input = bit_array.from_string("123")
  let result = bitty.run_partial(s.alpha(), on: input)
  assert result == Ok(#("", <<"123">>))
}

pub fn alpha1_fails_on_no_match_test() {
  let input = bit_array.from_string("123")
  let assert Error(_) = bitty.run_partial(s.alpha1(), on: input)
}

pub fn alphanumeric_empty_on_no_match_test() {
  let input = bit_array.from_string("!!!")
  let result = bitty.run_partial(s.alphanumeric(), on: input)
  assert result == Ok(#("", <<"!!!">>))
}

pub fn alphanumeric1_fails_on_no_match_test() {
  let input = bit_array.from_string("!!!")
  let assert Error(_) = bitty.run_partial(s.alphanumeric1(), on: input)
}

pub fn digit_empty_on_no_match_test() {
  let input = bit_array.from_string("abc")
  let result = bitty.run_partial(s.digit(), on: input)
  assert result == Ok(#("", <<"abc">>))
}

pub fn digit1_fails_on_no_match_test() {
  let input = bit_array.from_string("abc")
  let assert Error(_) = bitty.run_partial(s.digit1(), on: input)
}

pub fn hex_digit_empty_on_no_match_test() {
  let input = bit_array.from_string("xyz")
  let result = bitty.run_partial(s.hex_digit(), on: input)
  assert result == Ok(#("", <<"xyz">>))
}

pub fn hex_digit1_fails_on_no_match_test() {
  let input = bit_array.from_string("xyz")
  let assert Error(_) = bitty.run_partial(s.hex_digit1(), on: input)
}

pub fn space_empty_on_no_match_test() {
  let input = bit_array.from_string("hello")
  let result = bitty.run_partial(s.space(), on: input)
  assert result == Ok(#("", <<"hello">>))
}

pub fn space1_fails_on_no_match_test() {
  let input = bit_array.from_string("hello")
  let assert Error(_) = bitty.run_partial(s.space1(), on: input)
}

pub fn multispace_empty_on_no_match_test() {
  let input = bit_array.from_string("hello")
  let result = bitty.run_partial(s.multispace(), on: input)
  assert result == Ok(#("", <<"hello">>))
}

pub fn multispace1_fails_on_no_match_test() {
  let input = bit_array.from_string("abc")
  let assert Error(_) = bitty.run_partial(s.multispace1(), on: input)
}

pub fn newline_matches_test() {
  let input = bit_array.from_string("\nhello")
  let result = bitty.run_partial(s.newline(), on: input)
  assert result == Ok(#(Nil, <<"hello">>))
}

pub fn newline_fails_on_crlf_test() {
  let input = bit_array.from_string("\r\n")
  let assert Error(_) = bitty.run_partial(s.newline(), on: input)
}

pub fn tab_matches_test() {
  let input = bit_array.from_string("\thello")
  let result = bitty.run_partial(s.tab(), on: input)
  assert result == Ok(#(Nil, <<"hello">>))
}

pub fn tab_fails_on_space_test() {
  let input = bit_array.from_string(" hello")
  let assert Error(_) = bitty.run_partial(s.tab(), on: input)
}

pub fn crlf_matches_test() {
  let input = bit_array.from_string("\r\nhello")
  let result = bitty.run_partial(s.crlf(), on: input)
  assert result == Ok(#(Nil, <<"hello">>))
}

pub fn crlf_fails_on_bare_lf_test() {
  let input = bit_array.from_string("\nhello")
  let assert Error(_) = bitty.run_partial(s.crlf(), on: input)
}

pub fn line_ending_matches_lf_test() {
  let input = bit_array.from_string("\nhello")
  let result = bitty.run_partial(s.line_ending(), on: input)
  assert result == Ok(#(Nil, <<"hello">>))
}

pub fn line_ending_matches_crlf_test() {
  let input = bit_array.from_string("\r\nhello")
  let result = bitty.run_partial(s.line_ending(), on: input)
  assert result == Ok(#(Nil, <<"hello">>))
}

pub fn line_ending_fails_on_bare_cr_test() {
  let input = bit_array.from_string("\rhello")
  let assert Error(_) = bitty.run_partial(s.line_ending(), on: input)
}

pub fn not_line_ending_stops_at_lf_test() {
  let input = bit_array.from_string("hello\nworld")
  let result = bitty.run_partial(s.not_line_ending(), on: input)
  assert result == Ok(#("hello", <<"\nworld">>))
}

pub fn not_line_ending_stops_at_cr_test() {
  let input = bit_array.from_string("hello\r\nworld")
  let result = bitty.run_partial(s.not_line_ending(), on: input)
  assert result == Ok(#("hello", <<"\r\nworld">>))
}

pub fn not_line_ending_consumes_all_without_newline_test() {
  let input = bit_array.from_string("hello")
  let result = bitty.run(s.not_line_ending(), on: input)
  assert result == Ok("hello")
}

pub fn parse_line_with_line_ending_test() {
  let input = bit_array.from_string("hello\r\nworld\n")
  let parser =
    s.not_line_ending()
    |> bitty.then(fn(line) { s.line_ending() |> bitty.map(fn(_) { line }) })
  let result = bitty.run_partial(bitty.many(parser), on: input)
  assert result == Ok(#(["hello", "world"], <<>>))
}

pub fn digit1_then_alpha1_test() {
  let input = bit_array.from_string("123abc!")
  let parser =
    s.digit1()
    |> bitty.then(fn(digits) {
      s.alpha1() |> bitty.map(fn(letters) { #(digits, letters) })
    })
  let result = bitty.run_partial(parser, on: input)
  assert result == Ok(#(#("123", "abc"), <<"!">>))
}

pub fn grapheme_fails_when_unaligned_test() {
  let parser =
    bits.uint(bits: 4)
    |> bitty.then(fn(_) { s.grapheme() })
  let assert Error(_) = bitty.run(parser, on: <<0xAB>>)
}

pub fn grapheme_if_fails_when_unaligned_test() {
  let parser =
    bits.uint(bits: 4)
    |> bitty.then(fn(_) { s.grapheme_if(fn(_) { True }) })
  let assert Error(_) = bitty.run(parser, on: <<0xAB>>)
}

pub fn take_while_fails_when_unaligned_test() {
  let parser =
    bits.uint(bits: 4)
    |> bitty.then(fn(_) { s.take_while(fn(_) { True }) })
  let assert Error(_) = bitty.run(parser, on: <<0xAB>>)
}

pub fn take_graphemes_fails_when_unaligned_test() {
  let parser =
    bits.uint(bits: 4)
    |> bitty.then(fn(_) { s.take_graphemes(1) })
  let assert Error(_) = bitty.run(parser, on: <<0xAB>>)
}

pub fn null_terminated_rejects_non_nul_test() {
  let assert Error(_) = bitty.run(s.null_terminated(), on: <<"hi", 0x01>>)
}

pub fn take_graphemes_negative_count_fails_test() {
  let assert Error(_) = bitty.run(s.take_graphemes(-1), on: <<"hello">>)
}

pub fn grapheme_if_emoji_test() {
  let parser = s.grapheme_if(fn(c) { c == "🎉" })
  let result = bitty.run(parser, on: <<"🎉">>)
  assert result == Ok("🎉")
}

pub fn utf8_zero_length_test() {
  let result = bitty.run_partial(s.utf8(0), on: <<"hello">>)
  assert result == Ok(#("", <<"hello">>))
}

pub fn fixed_all_null_test() {
  let result = bitty.run(s.fixed(3), on: <<0, 0, 0>>)
  assert result == Ok("")
}

pub fn fixed_all_space_test() {
  let result = bitty.run(s.fixed(3), on: <<" ", " ", " ">>)
  assert result == Ok("")
}

pub fn take_graphemes_beyond_input_test() {
  let assert Error(_) = bitty.run(s.take_graphemes(10), on: <<"hi">>)
}

pub fn integer_parses_digits_test() {
  let result = bitty.run_partial(s.integer(), on: <<"42abc">>)
  assert result == Ok(#(42, <<"abc">>))
}

pub fn integer_single_digit_test() {
  let result = bitty.run(s.integer(), on: <<"7">>)
  assert result == Ok(7)
}

pub fn integer_fails_on_no_digits_test() {
  let assert Error(_) = bitty.run(s.integer(), on: <<"abc">>)
}

pub fn integer_consumes_all_leading_digits_test() {
  let result = bitty.run_partial(s.integer(), on: <<"123456rest">>)
  assert result == Ok(#(123_456, <<"rest">>))
}

pub fn take_graphemes_partial_failure_blocks_backtrack_test() {
  let parser =
    bitty.one_of([
      s.take_graphemes(5),
      bitty.success("fallback"),
    ])
  let assert Error(_) = bitty.run(parser, on: <<"abc">>)
}

pub fn integer_round_trip_pbt_test() {
  qcheck.run(
    qcheck.default_config(),
    qcheck.bounded_int(0, 999_999),
    fn(original) {
      let input = bit_array.from_string(int.to_string(original))
      let result = bitty.run(s.integer(), on: input)
      assert result == Ok(original)
    },
  )
}

pub fn alpha_classification_pbt_test() {
  let gen =
    qcheck.non_empty_string_from(qcheck.bounded_codepoint(from: 0x20, to: 0x7E))
  qcheck.run(qcheck.default_config(), gen, fn(input) {
    let bytes = bit_array.from_string(input)
    let assert Ok(#(matched, _)) = bitty.run_partial(s.alpha(), on: bytes)
    let valid =
      string.to_graphemes(matched)
      |> list.all(fn(c) {
        case <<c:utf8>> {
          <<b>> -> { b >= 0x41 && b <= 0x5A } || { b >= 0x61 && b <= 0x7A }
          _ -> False
        }
      })
    assert valid
  })
}

pub fn digit_classification_pbt_test() {
  let gen =
    qcheck.non_empty_string_from(qcheck.bounded_codepoint(from: 0x20, to: 0x7E))
  qcheck.run(qcheck.default_config(), gen, fn(input) {
    let bytes = bit_array.from_string(input)
    let assert Ok(#(matched, _)) = bitty.run_partial(s.digit(), on: bytes)
    let valid =
      string.to_graphemes(matched)
      |> list.all(fn(c) {
        case <<c:utf8>> {
          <<b>> -> b >= 0x30 && b <= 0x39
          _ -> False
        }
      })
    assert valid
  })
}

pub fn hex_digit_classification_pbt_test() {
  let gen =
    qcheck.non_empty_string_from(qcheck.bounded_codepoint(from: 0x20, to: 0x7E))
  qcheck.run(qcheck.default_config(), gen, fn(input) {
    let bytes = bit_array.from_string(input)
    let assert Ok(#(matched, _)) = bitty.run_partial(s.hex_digit(), on: bytes)
    let valid =
      string.to_graphemes(matched)
      |> list.all(fn(c) {
        case bit_array.from_string(c) {
          <<b>> ->
            { b >= 0x30 && b <= 0x39 }
            || { b >= 0x41 && b <= 0x46 }
            || { b >= 0x61 && b <= 0x66 }
          _ -> False
        }
      })
    assert valid
  })
}
