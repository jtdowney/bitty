import bitty
import bitty/bits
import bitty/bytes as b
import bitty/num
import gleam/bit_array
import gleam/option.{None, Some}
import qcheck
import unitest

pub fn main() -> Nil {
  unitest.main()
}

pub fn success_run_returns_value_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(-1000, 1000), fn(x) {
    let result = bitty.success(x) |> bitty.run(<<>>)
    assert result == Ok(x)
  })
}

pub fn success_run_partial_returns_value_and_remainder_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(-1000, 1000), fn(x) {
    let with_trailing = bitty.success(x) |> bitty.run_partial(<<1, 2, 3>>)
    assert with_trailing == Ok(#(x, <<1, 2, 3>>))

    let empty = bitty.success(x) |> bitty.run_partial(<<>>)
    assert empty == Ok(#(x, <<>>))
  })
}

pub fn run_partial_after_bits_excludes_partial_byte_test() {
  let parser = bits.uint(3)
  let result = bitty.run_partial(parser, on: <<0xE0, 0xAB>>)
  assert result == Ok(#(7, <<0xAB>>))
}

pub fn run_partial_byte_aligned_unchanged_test() {
  let parser = num.u8()
  let result = bitty.run_partial(parser, on: <<0x01, 0x02>>)
  assert result == Ok(#(1, <<0x02>>))
}

pub fn run_with_location_returns_location_and_remainder_test() {
  let result = bitty.success(99) |> bitty.run_with_location(<<5, 6>>)
  assert result == Ok(#(99, bitty.Location(byte: 0, bit: 0), <<5, 6>>))
}

pub fn run_with_location_empty_input_test() {
  let result = bitty.success(42) |> bitty.run_with_location(<<>>)
  assert result == Ok(#(42, bitty.Location(byte: 0, bit: 0), <<>>))
}

pub fn fail_run_returns_error_test() {
  qcheck.run(qcheck.default_config(), qcheck.string(), fn(msg) {
    let result = bitty.fail(msg) |> bitty.run(<<>>)
    assert result
      == Error(bitty.BittyError(
        at: bitty.Location(byte: 0, bit: 0),
        expected: [],
        context: [],
        message: Some(msg),
      ))
  })
}

pub fn run_unconsumed_input_error_test() {
  let result = bitty.success(1) |> bitty.run(<<0xFF>>)
  assert result
    == Error(bitty.BittyError(
      at: bitty.Location(byte: 0, bit: 0),
      expected: ["end of input"],
      context: [],
      message: None,
    ))
}

pub fn map_transforms_value_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(-1000, 1000), fn(x) {
    let result =
      bitty.success(x) |> bitty.map(fn(v) { v * 2 }) |> bitty.run(<<>>)
    assert result == Ok(x * 2)
  })
}

pub fn then_chains_parsers_test() {
  qcheck.run(qcheck.default_config(), qcheck.bounded_int(-1000, 1000), fn(x) {
    let result =
      bitty.success(x)
      |> bitty.then(fn(v) { bitty.success(v + 5) })
      |> bitty.run(<<>>)
    assert result == Ok(x + 5)
  })
}

pub fn then_propagates_failure_test() {
  let result =
    bitty.success(10)
    |> bitty.then(fn(_) { bitty.fail("nope") })
    |> bitty.run(<<>>)
  let assert Error(_) = result
}

pub fn one_of_first_matches_test() {
  let parser = bitty.one_of([bitty.success(1), bitty.success(2)])
  let result = bitty.run(parser, on: <<>>)
  assert result == Ok(1)
}

pub fn one_of_falls_through_to_matching_branch_test() {
  let parser =
    bitty.one_of([
      b.tag(<<0x01>>) |> bitty.map(fn(_) { 1 }),
      b.tag(<<0x02>>) |> bitty.map(fn(_) { 2 }),
    ])
  let result = bitty.run(parser, on: <<0x02>>)
  assert result == Ok(2)
}

pub fn one_of_none_match_fails_test() {
  let parser =
    bitty.one_of([
      b.tag(<<0x01>>) |> bitty.map(fn(_) { 1 }),
      b.tag(<<0x02>>) |> bitty.map(fn(_) { 2 }),
    ])
  let result = bitty.run(parser, on: <<0x03>>)
  let assert Error(_) = result
}

pub fn one_of_empty_list_fails_test() {
  let parser: bitty.Parser(Int) = bitty.one_of([])
  let result = bitty.run(parser, on: <<>>)
  let assert Error(_) = result
}

pub fn attempt_allows_backtracking_after_consumption_test() {
  let parser =
    bitty.one_of([
      bitty.attempt(b.tag(<<0x01>>) |> bitty.then(fn(_) { b.tag(<<0xFF>>) }))
        |> bitty.map(fn(_) { 1 }),
      b.tag(<<0x01>>)
        |> bitty.then(fn(_) { b.tag(<<0x02>>) })
        |> bitty.map(fn(_) { 2 }),
    ])
  let result = bitty.run(parser, on: <<0x01, 0x02>>)
  assert result == Ok(2)
}

pub fn cut_prevents_backtracking_test() {
  let parser =
    bitty.one_of([
      bitty.attempt(
        bitty.cut(b.tag(<<0x01>>))
        |> bitty.then(fn(_) { bitty.fail("committed failure") }),
      )
        |> bitty.map(fn(_) { 1 }),
      b.tag(<<0x01>>) |> bitty.map(fn(_) { 2 }),
    ])
  let result = bitty.run(parser, on: <<0x01, 0x02>>)
  let assert Error(_) = result
}

pub fn many_cut_parser_terminates_test() {
  let parser = bitty.many(bitty.cut(num.u8()))
  let result = bitty.run(parser, on: <<1, 2, 3>>)
  assert result == Ok([1, 2, 3])
}

pub fn cut_preserves_consumed_true_test() {
  let parser = bitty.cut(num.u8())
  let result = bitty.run(parser, on: <<0xAB>>)
  assert result == Ok(0xAB)
}

pub fn cut_only_commits_on_success_test() {
  let parser =
    bitty.one_of([
      bitty.cut(b.tag(<<0x01>>)) |> bitty.map(fn(_) { 1 }),
      b.tag(<<0x02>>) |> bitty.map(fn(_) { 2 }),
    ])
  let result = bitty.run(parser, on: <<0x02>>)
  assert result == Ok(2)
}

pub fn label_replaces_expected_on_error_test() {
  let parser = num.u8() |> bitty.label(named: "version byte")
  let assert Error(error) = bitty.run(parser, on: <<>>)
  assert error.expected == ["version byte"]
}

pub fn context_adds_to_error_context_test() {
  let parser = num.u8() |> bitty.context(in: "header")
  let assert Error(error) = bitty.run(parser, on: <<>>)
  assert error.context == ["header"]
}

pub fn nested_context_accumulates_test() {
  let parser =
    num.u8()
    |> bitty.context(in: "field")
    |> bitty.context(in: "record")
    |> bitty.context(in: "message")
  let assert Error(error) = bitty.run(parser, on: <<>>)
  assert error.context == ["message", "record", "field"]
}

pub fn many_collects_all_values_test() {
  let parser = bitty.many(num.u8())
  let result = bitty.run(parser, on: <<1, 2, 3>>)
  assert result == Ok([1, 2, 3])
}

pub fn many_empty_input_returns_empty_list_test() {
  let parser = bitty.many(num.u8())
  let result = bitty.run(parser, on: <<>>)
  assert result == Ok([])
}

pub fn many1_requires_at_least_one_test() {
  let parser = bitty.many1(num.u8())
  let result = bitty.run(parser, on: <<1, 2>>)
  assert result == Ok([1, 2])
}

pub fn many1_empty_fails_test() {
  let parser = bitty.many1(num.u8())
  let result = bitty.run(parser, on: <<>>)
  let assert Error(_) = result
}

pub fn repeat_exact_count_test() {
  let parser = bitty.repeat(num.u8(), times: 3)
  let result = bitty.run(parser, on: <<10, 20, 30>>)
  assert result == Ok([10, 20, 30])
}

pub fn repeat_insufficient_input_fails_test() {
  let parser = bitty.repeat(num.u8(), times: 3)
  let result = bitty.run(parser, on: <<10, 20>>)
  let assert Error(_) = result
}

pub fn repeat_non_positive_returns_empty_list_test() {
  let result0 = bitty.repeat(num.u8(), times: 0) |> bitty.run(on: <<>>)
  assert result0 == Ok([])

  let result_neg = bitty.repeat(num.u8(), times: -1) |> bitty.run(on: <<>>)
  assert result_neg == Ok([])

  let result_with_input =
    bitty.repeat(num.u8(), times: 0) |> bitty.run_partial(on: <<1, 2, 3>>)
  assert result_with_input == Ok(#([], <<1, 2, 3>>))
}

pub fn optional_present_test() {
  let parser = bitty.optional(num.u8())
  let result = bitty.run_partial(parser, on: <<0xAB>>)
  assert result == Ok(#(option.Some(0xAB), <<>>))
}

pub fn optional_absent_test() {
  let parser = bitty.optional(num.u8())
  let result = bitty.run_partial(parser, on: <<>>)
  assert result == Ok(#(option.None, <<>>))
}

pub fn optional_consuming_failure_propagates_test() {
  let inner =
    num.u8()
    |> bitty.then(fn(_) { bitty.fail("after consuming") })
  let parser = bitty.optional(inner)
  let result = bitty.run_partial(parser, on: <<0xAB>>)
  let assert Error(_) = result
}

pub fn location_reports_current_offset_test() {
  let parser = b.skip(3) |> bitty.then(fn(_) { bitty.location() })
  let result = bitty.run(parser, on: <<1, 2, 3>>)
  assert result == Ok(bitty.Location(byte: 3, bit: 0))
}

pub fn location_after_bits_reports_bit_offset_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(3))
    bitty.location()
  }
  let result = bitty.run_partial(parser, on: <<0xFF>>)
  assert result == Ok(#(bitty.Location(byte: 0, bit: 3), <<>>))
}

pub fn end_succeeds_at_end_of_input_test() {
  let parser =
    num.u8()
    |> bitty.then(fn(byte) { bitty.end() |> bitty.map(fn(_) { byte }) })
  let result = bitty.run(parser, on: <<0xAB>>)
  assert result == Ok(0xAB)
}

pub fn end_fails_with_remaining_input_test() {
  let result = bitty.run_partial(bitty.end(), on: <<0x01>>)
  let assert Error(_) = result
}

pub fn end_fails_at_non_aligned_bit_position_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(3))
    bitty.end()
  }
  let assert Error(_) = bitty.run_partial(parser, on: <<0xFF>>)
}

pub fn within_bytes_full_consumption_test() {
  let inner =
    num.u8()
    |> bitty.then(fn(a) { num.u8() |> bitty.map(fn(b_val) { #(a, b_val) }) })
  let parser = bitty.within_bytes(2, inner)
  let result = bitty.run(parser, on: <<0x01, 0x02>>)
  assert result == Ok(#(1, 2))
}

pub fn within_bytes_advances_outer_test() {
  let inner = {
    use val <- bitty.then(num.u8())
    use _ <- bitty.then(num.u8())
    bitty.success(val)
  }
  let parser =
    bitty.within_bytes(2, inner)
    |> bitty.then(fn(val) { num.u8() |> bitty.map(fn(after) { #(val, after) }) })
  let result = bitty.run(parser, on: <<0x01, 0x02, 0x03>>)
  assert result == Ok(#(1, 3))
}

pub fn within_bytes_incomplete_consumption_fails_test() {
  let parser = bitty.within_bytes(3, num.u8())
  let result = bitty.run(parser, on: <<0x01, 0x02, 0x03>>)
  let assert Error(_) = result
}

pub fn within_bytes_insufficient_input_fails_test() {
  let parser = bitty.within_bytes(5, num.u8())
  let result = bitty.run(parser, on: <<0x01, 0x02>>)
  let assert Error(_) = result
}

pub fn within_bytes_non_consuming_fail_allows_backtrack_test() {
  let parser =
    bitty.one_of([
      bitty.within_bytes(2, bitty.fail("nope")),
      bitty.success(99),
    ])
  let result = bitty.run_partial(parser, on: <<1, 2>>)
  assert result == Ok(#(99, <<1, 2>>))
}

pub fn within_bytes_consuming_fail_blocks_backtrack_test() {
  let inner =
    num.u8()
    |> bitty.then(fn(_) { bitty.fail("after consuming") })
  let parser =
    bitty.one_of([
      bitty.within_bytes(2, inner),
      bitty.success(99),
    ])
  let result = bitty.run_partial(parser, on: <<1, 2>>)
  let assert Error(_) = result
}

pub fn within_bytes_cut_inside_preserves_commit_state_test() {
  let inner =
    bitty.cut(num.u8())
    |> bitty.then(fn(_) { bitty.fail("committed failure inside window") })
  let parser =
    bitty.one_of([
      bitty.attempt(bitty.within_bytes(2, inner)),
      bitty.success(99),
    ])
  let assert Error(_) = bitty.run_partial(parser, on: <<1, 2>>)
}

pub fn within_bytes_zero_length_does_not_infinite_loop_test() {
  let parser = bitty.many(bitty.within_bytes(0, bitty.success(Nil)))
  let assert Error(_) = bitty.run(parser, on: <<>>)
}

pub fn within_bytes_partial_returns_unconsumed_test() {
  let parser = bitty.within_bytes_partial(4, num.u8())
  let result = bitty.run(parser, on: <<0x01, 0x02, 0x03, 0x04>>)
  assert result == Ok(#(1, <<0x02, 0x03, 0x04>>))
}

pub fn within_bytes_partial_full_consumption_test() {
  let parser =
    bitty.within_bytes_partial(2, {
      use a <- bitty.then(num.u8())
      use b_val <- bitty.then(num.u8())
      bitty.success(#(a, b_val))
    })
  let result = bitty.run(parser, on: <<0x01, 0x02>>)
  assert result == Ok(#(#(1, 2), <<>>))
}

pub fn preceded_discards_prefix_test() {
  let parser = bitty.preceded(b.tag(<<0x00>>), num.u8())
  let result = bitty.run(parser, on: <<0x00, 0x42>>)
  assert result == Ok(0x42)
}

pub fn preceded_prefix_failure_propagates_test() {
  let parser = bitty.preceded(b.tag(<<0xFF>>), num.u8())
  let assert Error(_) = bitty.run(parser, on: <<0x00, 0x42>>)
}

pub fn terminated_discards_suffix_test() {
  let parser = bitty.terminated(num.u8(), b.tag(<<0x00>>))
  let result = bitty.run(parser, on: <<0x42, 0x00>>)
  assert result == Ok(0x42)
}

pub fn terminated_suffix_failure_propagates_test() {
  let parser = bitty.terminated(num.u8(), b.tag(<<0xFF>>))
  let assert Error(_) = bitty.run(parser, on: <<0x42, 0x00>>)
}

pub fn delimited_returns_inner_value_test() {
  let parser = bitty.delimited(b.tag(<<0x28>>), num.u8(), b.tag(<<0x29>>))
  let result = bitty.run(parser, on: <<0x28, 0x42, 0x29>>)
  assert result == Ok(0x42)
}

pub fn delimited_open_failure_propagates_test() {
  let parser = bitty.delimited(b.tag(<<0x28>>), num.u8(), b.tag(<<0x29>>))
  let assert Error(_) = bitty.run(parser, on: <<0xFF, 0x42, 0x29>>)
}

pub fn delimited_close_failure_propagates_test() {
  let parser = bitty.delimited(b.tag(<<0x28>>), num.u8(), b.tag(<<0x29>>))
  let assert Error(_) = bitty.run(parser, on: <<0x28, 0x42, 0xFF>>)
}

pub fn pair_returns_tuple_test() {
  let parser = bitty.pair(num.u8(), num.u8())
  let result = bitty.run(parser, on: <<1, 2>>)
  assert result == Ok(#(1, 2))
}

pub fn pair_first_failure_propagates_test() {
  let parser = bitty.pair(num.u8(), num.u8())
  let assert Error(_) = bitty.run(parser, on: <<>>)
}

pub fn pair_second_failure_propagates_test() {
  let parser = bitty.pair(num.u8(), num.u8())
  let assert Error(_) = bitty.run(parser, on: <<1>>)
}

pub fn separated_pair_returns_tuple_test() {
  let parser =
    bitty.separated_pair(num.u8(), by: b.tag(<<0x2C>>), then: num.u8())
  let result = bitty.run(parser, on: <<1, 0x2C, 2>>)
  assert result == Ok(#(1, 2))
}

pub fn separated_pair_separator_failure_propagates_test() {
  let parser =
    bitty.separated_pair(num.u8(), by: b.tag(<<0x2C>>), then: num.u8())
  let assert Error(_) = bitty.run(parser, on: <<1, 0xFF, 2>>)
}

pub fn separated_pair_second_failure_propagates_test() {
  let parser =
    bitty.separated_pair(num.u8(), by: b.tag(<<0x2C>>), then: num.u8())
  let assert Error(_) = bitty.run(parser, on: <<1, 0x2C>>)
}

pub fn separated_multiple_items_test() {
  let parser = bitty.separated(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run(parser, on: <<1, 0x2C, 2, 0x2C, 3>>)
  assert result == Ok([1, 2, 3])
}

pub fn separated_single_item_test() {
  let parser = bitty.separated(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run(parser, on: <<0x42>>)
  assert result == Ok([0x42])
}

pub fn separated_empty_input_returns_empty_list_test() {
  let parser = bitty.separated(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run(parser, on: <<>>)
  assert result == Ok([])
}

pub fn separated1_requires_at_least_one_test() {
  let parser = bitty.separated1(num.u8(), by: b.tag(<<0x2C>>))
  let assert Error(_) = bitty.run(parser, on: <<>>)
}

pub fn separated1_multiple_items_test() {
  let parser = bitty.separated1(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run(parser, on: <<1, 0x2C, 2>>)
  assert result == Ok([1, 2])
}

pub fn separated1_single_item_test() {
  let parser = bitty.separated1(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run(parser, on: <<0x42>>)
  assert result == Ok([0x42])
}

pub fn separated1_trailing_separator_test() {
  let parser = bitty.separated1(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run_partial(parser, on: <<1, 0x2C, 2, 0x2C>>)
  assert result == Ok(#([1, 2], <<0x2C>>))
}

pub fn separated_trailing_separator_test() {
  let parser = bitty.separated(num.u8(), by: b.tag(<<0x2C>>))
  let result = bitty.run_partial(parser, on: <<1, 0x2C, 2, 0x2C>>)
  assert result == Ok(#([1, 2], <<0x2C>>))
}

pub fn from_result_with_use_syntax_test() {
  let parser = {
    use raw <- bitty.then(b.take(5))
    use text <- bitty.then(bitty.from_result(bit_array.to_string(raw)))
    bitty.success(text)
  }
  let result = bitty.run(parser, on: <<"hello":utf8>>)
  assert result == Ok("hello")
}

pub fn from_result_error_fails_test() {
  let result = bitty.from_result(Error(Nil)) |> bitty.run(<<>>)
  assert result
    == Error(bitty.BittyError(
      at: bitty.Location(byte: 0, bit: 0),
      expected: [],
      context: [],
      message: Some("Nil"),
    ))
}

pub fn from_result_invalid_utf8_fails_test() {
  let parser = {
    use raw <- bitty.then(b.take(2))
    use text <- bitty.then(bitty.from_result(bit_array.to_string(raw)))
    bitty.success(text)
  }
  let assert Error(_) = bitty.run(parser, on: <<0xFF, 0xFE>>)
}

pub fn align_noop_when_already_aligned_test() {
  let parser = bits.align()
  let result = bitty.run(parser, on: <<>>)
  assert result == Ok(Nil)
}

pub fn many_rejects_non_consuming_parser_test() {
  let parser = bitty.many(bitty.success(1))
  let assert Error(error) = bitty.run(parser, on: <<>>)
  assert error.expected == ["consuming parser in many"]
}

pub fn one_of_merges_expected_from_all_branches_test() {
  let parser =
    bitty.one_of([
      b.tag(<<1>>) |> bitty.map(fn(_) { 1 }),
      b.tag(<<2>>) |> bitty.map(fn(_) { 2 }),
    ])
  let assert Error(error) = bitty.run(parser, on: <<3>>)
  assert error.expected == ["tag", "tag"]
}

pub fn error_location_after_partial_consumption_test() {
  let parser = {
    use _ <- bitty.then(b.take(3))
    num.u8()
  }
  let assert Error(error) = bitty.run(parser, on: <<1, 2, 3>>)
  assert error.at == bitty.Location(byte: 3, bit: 0)
}

pub fn within_bytes_negative_length_fails_test() {
  let assert Error(_) =
    bitty.run_partial(bitty.within_bytes(-1, num.u8()), on: <<0x01>>)
}

pub fn within_bytes_partial_unaligned_fails_test() {
  let parser = {
    use _ <- bitty.then(bits.uint(3))
    bitty.within_bytes_partial(1, num.u8())
  }
  let assert Error(_) = bitty.run_partial(parser, on: <<0xFF, 0xFF>>)
}
