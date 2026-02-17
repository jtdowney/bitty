//// Zero-copy binary parser combinators for Gleam, targeting both BEAM and
//// JavaScript. Build parsers by composing primitives from `bitty/bytes`,
//// `bitty/bits`, and `bitty/num` with the combinators in this module.
//// Parsers are designed for Gleam's `use` syntax.

import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/order
import gleam/result
import gleam/string

/// A position in the input, expressed as a byte offset and a bit offset
/// within that byte (0–7).
pub type Location {
  Location(byte: Int, bit: Int)
}

/// A parse error with the position it occurred at, what was expected,
/// context labels from enclosing `context` calls, and an optional message
/// from `fail`.
pub type BittyError {
  BittyError(
    at: Location,
    expected: List(String),
    context: List(String),
    message: Option(String),
  )
}

/// An opaque binary parser that, when run, consumes input and produces a
/// value of type `a` or fails with a `BittyError`.
pub opaque type Parser(a) {
  Parser(run: fn(State) -> Step(a))
}

@internal
pub type State {
  State(input: BitArray, byte_offset: Int, bit_offset: Int, committed: Bool)
}

@internal
pub type Step(a) {
  Continue(value: a, state: State, consumed: Bool)
  Stop(error: BittyError, consumed: Bool, committed: Bool)
}

@internal
pub fn make_parser(f: fn(State) -> Step(a)) -> Parser(a) {
  Parser(f)
}

@internal
pub fn run_parser(parser: Parser(a), state: State) -> Step(a) {
  parser.run(state)
}

/// Create a parser that always succeeds with the given value without
/// consuming any input.
pub fn success(value: a) -> Parser(a) {
  Parser(fn(state) { Continue(value, state, False) })
}

/// Create a parser that always fails with the given message without
/// consuming any input.
pub fn fail(message: String) -> Parser(a) {
  Parser(fn(state) {
    Stop(
      BittyError(
        at: Location(byte: state.byte_offset, bit: state.bit_offset),
        expected: [],
        context: [],
        message: Some(message),
      ),
      False,
      state.committed,
    )
  })
}

/// Convert a `Result(a, e)` into a parser: `Ok(value)` succeeds with the
/// value, `Error(e)` fails with the inspected error as the message.
/// Useful for lifting fallible conversions (e.g. `bit_array.to_string`)
/// into a parser pipeline with `use` syntax.
pub fn from_result(result: Result(a, e)) -> Parser(a) {
  case result {
    Ok(value) -> success(value)
    Error(e) -> fail(string.inspect(e))
  }
}

/// Run a parser on the given input, requiring it to consume all bytes.
/// Returns `Error` if parsing fails or if unconsumed input remains.
///
/// ```gleam
/// let assert Ok(byte) = bitty.run(num.u8(), on: <<0xFF>>)
/// assert byte == 255
/// ```
pub fn run(parser: Parser(a), on input: BitArray) -> Result(a, BittyError) {
  use #(value, final_state) <- result.try(do_run(parser, input))
  let remaining =
    bit_array.byte_size(final_state.input) - final_state.byte_offset
  case remaining == 0 && final_state.bit_offset == 0 {
    True -> Ok(value)
    False ->
      Error(BittyError(
        at: Location(byte: final_state.byte_offset, bit: final_state.bit_offset),
        expected: ["end of input"],
        context: [],
        message: None,
      ))
  }
}

/// Run a parser on the given input, returning the parsed value and any
/// unconsumed bytes. Does not require all input to be consumed.
///
/// ```gleam
/// let assert Ok(#(byte, rest)) =
///   bitty.run_partial(num.u8(), on: <<0xAA, 0xBB>>)
/// assert byte == 0xAA
/// assert rest == <<0xBB>>
/// ```
pub fn run_partial(
  parser: Parser(a),
  on input: BitArray,
) -> Result(#(a, BitArray), BittyError) {
  use #(value, final_state) <- result.try(do_run(parser, input))
  Ok(#(value, remaining_slice(final_state)))
}

/// Like `run_partial`, but also returns the `Location` where the parser
/// stopped. Useful for incremental or streaming parsing.
pub fn run_with_location(
  parser: Parser(a),
  on input: BitArray,
) -> Result(#(a, Location, BitArray), BittyError) {
  use #(value, final_state) <- result.try(do_run(parser, input))
  let loc = Location(byte: final_state.byte_offset, bit: final_state.bit_offset)
  Ok(#(value, loc, remaining_slice(final_state)))
}

fn do_run(parser: Parser(a), input: BitArray) -> Result(#(a, State), BittyError) {
  let state = new_state(input)
  case parser.run(state) {
    Continue(value, final_state, _consumed) -> Ok(#(value, final_state))
    Stop(error, _consumed, _committed) -> Error(error)
  }
}

/// Transform the result of a parser by applying `f` to the parsed value.
pub fn map(parser: Parser(a), with f: fn(a) -> b) -> Parser(b) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(value, state1, consumed) -> Continue(f(value), state1, consumed)
      Stop(error, consumed, committed) -> Stop(error, consumed, committed)
    }
  })
}

/// Sequence two parsers: run `parser`, then pass its result to `next` to
/// get the second parser. Designed for Gleam's `use` syntax:
///
/// ```gleam
/// use length <- bitty.then(num.u8())
/// use data <- bitty.then(bytes.take(length))
/// bitty.success(data)
/// ```
pub fn then(parser: Parser(a), next: fn(a) -> Parser(b)) -> Parser(b) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(value, state1, consumed1) ->
        case { next(value) }.run(state1) {
          Continue(value2, state2, consumed2) ->
            Continue(value2, state2, consumed1 || consumed2)
          Stop(error, consumed2, committed2) ->
            Stop(error, consumed1 || consumed2, committed2 || state1.committed)
        }
      Stop(error, consumed, committed) -> Stop(error, consumed, committed)
    }
  })
}

/// Try each parser in order, returning the first success. A parser that
/// consumes input before failing will **not** backtrack — wrap alternatives
/// in `attempt` if backtracking is needed. Errors from non-consuming
/// failures at the same position are merged.
///
/// ```gleam
/// let parser = bitty.one_of([
///   bitty.attempt(bytes.tag(<<0x01>>) |> bitty.map(fn(_) { "one" })),
///   bytes.tag(<<0x02>>) |> bitty.map(fn(_) { "two" }),
/// ])
/// let assert Ok("two") = bitty.run(parser, on: <<0x02>>)
/// ```
pub fn one_of(parsers: List(Parser(a))) -> Parser(a) {
  Parser(fn(state) {
    try_parsers(
      parsers,
      state,
      BittyError(
        at: Location(byte: state.byte_offset, bit: state.bit_offset),
        expected: [],
        context: [],
        message: None,
      ),
    )
  })
}

fn try_parsers(
  parsers: List(Parser(a)),
  state: State,
  last_error: BittyError,
) -> Step(a) {
  case parsers {
    [] -> Stop(last_error, False, False)
    [parser, ..rest] ->
      case parser.run(state) {
        Continue(_, _, _) as success -> success
        Stop(error, consumed, committed) ->
          case consumed || committed {
            True -> Stop(error, consumed, committed)
            False -> try_parsers(rest, state, merge_errors(last_error, error))
          }
      }
  }
}

fn merge_errors(e1: BittyError, e2: BittyError) -> BittyError {
  let loc1 = e1.at.byte * 8 + e1.at.bit
  let loc2 = e2.at.byte * 8 + e2.at.bit
  case int.compare(loc1, loc2) {
    order.Gt -> e1
    order.Lt -> e2
    order.Eq ->
      BittyError(..e1, expected: list.append(e1.expected, e2.expected))
  }
}

/// Wrap a parser to allow backtracking on failure. If the inner parser
/// fails after consuming input, `attempt` resets the consumed flag so
/// that `one_of` can try the next alternative.
///
/// ```gleam
/// let parser = bitty.one_of([
///   bitty.attempt(bytes.tag(<<0xCA, 0xFE>>)),
///   bytes.tag(<<0xCA, 0x11>>),
/// ])
/// let assert Ok(Nil) = bitty.run(parser, on: <<0xCA, 0x11>>)
/// ```
pub fn attempt(parser: Parser(a)) -> Parser(a) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(_, _, _) as success -> success
      Stop(error, _consumed, committed) -> Stop(error, False, committed)
    }
  })
}

/// Commit to the current parse path on success. After `cut`, a later
/// failure will not backtrack past this point, producing better error
/// messages. Typically used after matching a tag or discriminator.
///
/// ```gleam
/// use _ <- bitty.then(bitty.cut(bytes.tag(<<0x01>>)))
/// use value <- bitty.then(num.u8())
/// bitty.success(value)
/// ```
pub fn cut(parser: Parser(a)) -> Parser(a) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(value, state1, consumed) ->
        Continue(value, State(..state1, committed: True), consumed)
      Stop(error, consumed, committed) -> Stop(error, consumed, committed)
    }
  })
}

/// Replace the `expected` list in a parse error with `name`.
/// Useful for giving user-friendly names to complex parsers.
///
/// ```gleam
/// let parser = num.u8() |> bitty.label(named: "message type")
/// let assert Error(e) = bitty.run(parser, on: <<>>)
/// assert e.expected == ["message type"]
/// ```
pub fn label(parser: Parser(a), named name: String) -> Parser(a) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(_, _, _) as success -> success
      Stop(error, consumed, committed) ->
        Stop(BittyError(..error, expected: [name]), consumed, committed)
    }
  })
}

/// Push `name` onto the error context stack on failure, creating a
/// breadcrumb trail (e.g. `["TLS record", "handshake", "certificate"]`).
///
/// ```gleam
/// let parser = num.u8() |> bitty.context(in: "header")
/// let assert Error(e) = bitty.run(parser, on: <<>>)
/// assert e.context == ["header"]
/// ```
pub fn context(parser: Parser(a), in name: String) -> Parser(a) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(_, _, _) as success -> success
      Stop(error, consumed, committed) ->
        Stop(
          BittyError(..error, context: [name, ..error.context]),
          consumed,
          committed,
        )
    }
  })
}

/// Repeat a parser zero or more times, collecting results into a list.
/// The inner parser **must** consume input on each iteration; a
/// non-consuming parser causes an immediate error to prevent infinite loops.
/// Stops when the parser fails without consuming input.
///
/// ```gleam
/// let assert Ok(values) =
///   bitty.run(bitty.many(num.u8()), on: <<1, 2, 3>>)
/// assert values == [1, 2, 3]
/// ```
pub fn many(parser: Parser(a)) -> Parser(List(a)) {
  Parser(fn(state) { many_loop(parser, state, [], False) })
}

fn many_loop(
  parser: Parser(a),
  state: State,
  acc: List(a),
  any_consumed: Bool,
) -> Step(List(a)) {
  case parser.run(State(..state, committed: False)) {
    Continue(value, state1, True) ->
      many_loop(
        parser,
        State(..state1, committed: state1.committed || state.committed),
        [value, ..acc],
        True,
      )
    Continue(_value, _state1, False) ->
      Stop(
        BittyError(
          at: Location(byte: state.byte_offset, bit: state.bit_offset),
          expected: ["consuming parser in many"],
          context: [],
          message: None,
        ),
        any_consumed,
        state.committed,
      )
    Stop(_error, False, False) ->
      Continue(list.reverse(acc), state, any_consumed)
    Stop(error, consumed, committed) ->
      Stop(error, any_consumed || consumed, committed || state.committed)
  }
}

/// Like `many`, but requires at least one successful match.
pub fn many1(parser: Parser(a)) -> Parser(List(a)) {
  parser
  |> then(fn(first) { many(parser) |> map(fn(rest) { [first, ..rest] }) })
}

/// Run a parser exactly `count` times, collecting results into a list.
pub fn repeat(parser: Parser(a), times count: Int) -> Parser(List(a)) {
  Parser(fn(state) { repeat_loop(parser, state, [], count, False) })
}

fn repeat_loop(
  parser: Parser(a),
  state: State,
  acc: List(a),
  remaining: Int,
  any_consumed: Bool,
) -> Step(List(a)) {
  case remaining <= 0 {
    True -> Continue(list.reverse(acc), state, any_consumed)
    False ->
      case parser.run(state) {
        Continue(value, state1, consumed) ->
          repeat_loop(
            parser,
            state1,
            [value, ..acc],
            remaining - 1,
            any_consumed || consumed,
          )
        Stop(error, consumed, committed) ->
          Stop(error, any_consumed || consumed, committed)
      }
  }
}

/// Run `prefix` then `parser`, discarding the prefix result and returning
/// only the parser's value.
///
/// ```gleam
/// use value <- bitty.then(bitty.preceded(bytes.tag(<<0x00>>), num.u8()))
/// bitty.success(value)
/// ```
pub fn preceded(prefix: Parser(a), parser: Parser(b)) -> Parser(b) {
  prefix |> then(fn(_) { parser })
}

/// Run `parser` then `suffix`, discarding the suffix result and returning
/// only the parser's value.
///
/// ```gleam
/// use value <- bitty.then(bitty.terminated(num.u8(), bytes.tag(<<0x00>>)))
/// bitty.success(value)
/// ```
pub fn terminated(parser: Parser(a), suffix: Parser(b)) -> Parser(a) {
  parser |> then(fn(value) { suffix |> map(fn(_) { value }) })
}

/// Run `open`, `parser`, then `close`, returning only the parser's value.
///
/// ```gleam
/// let parser = bitty.delimited(
///   bytes.tag(<<0x28>>),
///   num.u8(),
///   bytes.tag(<<0x29>>),
/// )
/// let assert Ok(value) = bitty.run(parser, on: <<0x28, 42, 0x29>>)
/// assert value == 42
/// ```
pub fn delimited(
  open: Parser(a),
  parser: Parser(b),
  close: Parser(c),
) -> Parser(b) {
  preceded(open, terminated(parser, close))
}

/// Run two parsers in sequence and return their results as a tuple.
///
/// ```gleam
/// let parser = bitty.pair(num.u8(), num.u8())
/// let assert Ok(value) = bitty.run(parser, on: <<1, 2>>)
/// assert value == #(1, 2)
/// ```
pub fn pair(first: Parser(a), second: Parser(b)) -> Parser(#(a, b)) {
  first |> then(fn(a) { second |> map(fn(b) { #(a, b) }) })
}

/// Run two parsers separated by a third, discarding the separator's result.
///
/// ```gleam
/// let parser = bitty.separated_pair(
///   num.u8(),
///   by: bytes.tag(<<0x2C>>),
///   then: num.u8(),
/// )
/// let assert Ok(value) = bitty.run(parser, on: <<1, 0x2C, 2>>)
/// assert value == #(1, 2)
/// ```
pub fn separated_pair(
  first: Parser(a),
  by separator: Parser(b),
  then second: Parser(c),
) -> Parser(#(a, c)) {
  first |> then(fn(a) { preceded(separator, second) |> map(fn(c) { #(a, c) }) })
}

/// Parse zero or more occurrences of `parser` separated by `separator`.
/// The separator parser's result is discarded. Returns a list of the
/// parsed values. Succeeds with an empty list if the first item fails
/// without consuming input.
///
/// A trailing separator (one not followed by a valid item) is left
/// unconsumed. Compose with `end()` if you need to ensure all input
/// is consumed.
///
/// ```gleam
/// let parser = bitty.separated(num.u8(), by: bytes.tag(<<0x2C>>))
/// let assert Ok(values) = bitty.run(parser, on: <<1, 0x2C, 2, 0x2C, 3>>)
/// assert values == [1, 2, 3]
/// ```
pub fn separated(parser: Parser(a), by separator: Parser(b)) -> Parser(List(a)) {
  one_of([
    separated1(parser, by: separator),
    success([]),
  ])
}

/// Like `separated`, but requires at least one item.
///
/// A trailing separator (one not followed by a valid item) is left
/// unconsumed. Compose with `end()` if you need to ensure all input
/// is consumed.
///
/// ```gleam
/// let parser = bitty.separated1(num.u8(), by: bytes.tag(<<0x2C>>))
/// let assert Ok(values) = bitty.run(parser, on: <<1, 0x2C, 2, 0x2C, 3>>)
/// assert values == [1, 2, 3]
/// ```
pub fn separated1(parser: Parser(a), by separator: Parser(b)) -> Parser(List(a)) {
  parser
  |> then(fn(first) {
    many(attempt(preceded(separator, parser)))
    |> map(fn(rest) { [first, ..rest] })
  })
}

/// Try a parser, returning `Some(value)` on success or `None` if it fails
/// without consuming input. A consuming failure still propagates.
pub fn optional(parser: Parser(a)) -> Parser(Option(a)) {
  Parser(fn(state) {
    case parser.run(state) {
      Continue(value, state1, consumed) ->
        Continue(Some(value), state1, consumed)
      Stop(_error, False, False) -> Continue(None, state, False)
      Stop(error, consumed, committed) -> Stop(error, consumed, committed)
    }
  })
}

/// Return the current `Location` in the input without consuming anything.
pub fn location() -> Parser(Location) {
  Parser(fn(state) {
    Continue(
      Location(byte: state.byte_offset, bit: state.bit_offset),
      state,
      False,
    )
  })
}

/// Skip remaining bits in the current byte to reach the next byte boundary.
/// If already aligned, this is a no-op. Use after bit-level parsing to
/// resume byte-aligned operations.
@deprecated("Use bitty/bits.align instead")
pub fn align() -> Parser(Nil) {
  Parser(fn(state) {
    case state.bit_offset == 0 {
      True -> Continue(Nil, state, False)
      False ->
        Continue(
          Nil,
          State(..state, byte_offset: state.byte_offset + 1, bit_offset: 0),
          True,
        )
    }
  })
}

/// Succeed only if all input has been consumed. Fails with
/// `"end of input"` expected if bytes remain.
pub fn end() -> Parser(Nil) {
  Parser(fn(state) {
    let remaining = bit_array.byte_size(state.input) - state.byte_offset
    case remaining == 0 && state.bit_offset == 0 {
      True -> Continue(Nil, state, False)
      False ->
        Stop(
          BittyError(
            at: Location(byte: state.byte_offset, bit: state.bit_offset),
            expected: ["end of input"],
            context: [],
            message: None,
          ),
          False,
          state.committed,
        )
    }
  })
}

/// Run `inner` on a zero-copy window of exactly `byte_len` bytes.
/// The inner parser must consume the entire window or the parse fails.
/// Requires byte alignment.
///
/// ```gleam
/// use length <- bitty.then(num.u8())
/// use value <- bitty.then(bitty.within_bytes(length, run: bytes.rest()))
/// bitty.success(value)
/// ```
pub fn within_bytes(byte_len: Int, run inner: Parser(a)) -> Parser(a) {
  Parser(fn(state) {
    use <- bool.guard(
      when: byte_len < 0,
      return: stop_expected(state, "non-negative byte length"),
    )
    use <- require_aligned(state)
    within_bytes_run(state, byte_len, inner)
  })
}

fn within_bytes_setup(
  state: State,
  byte_len: Int,
  inner: Parser(a),
  on_window: fn(State, BitArray, Step(a)) -> Step(b),
) -> Step(b) {
  let remaining = bit_array.byte_size(state.input) - state.byte_offset
  case remaining < byte_len {
    True ->
      stop_expected(state, "at least " <> int.to_string(byte_len) <> " bytes")
    False ->
      case bit_array.slice(state.input, state.byte_offset, byte_len) {
        Ok(window) -> {
          let inner_state =
            State(
              input: window,
              byte_offset: 0,
              bit_offset: 0,
              committed: False,
            )
          on_window(state, window, inner.run(inner_state))
        }
        _ -> stop_expected(state, "valid slice")
      }
  }
}

fn within_bytes_run(state: State, byte_len: Int, inner: Parser(a)) -> Step(a) {
  within_bytes_setup(state, byte_len, inner, fn(st, _window, result) {
    case result {
      Continue(value, inner_end, _consumed) ->
        within_bytes_check(st, byte_len, value, inner_end)
      Stop(error, consumed, committed) ->
        within_bytes_stop(st, error, consumed, committed)
    }
  })
}

fn within_bytes_check(
  state: State,
  byte_len: Int,
  value: a,
  inner_end: State,
) -> Step(a) {
  case inner_end.byte_offset == byte_len && inner_end.bit_offset == 0 {
    True ->
      Continue(
        value,
        State(..state, byte_offset: state.byte_offset + byte_len),
        byte_len > 0,
      )
    False ->
      Stop(
        BittyError(
          at: Location(
            byte: state.byte_offset + inner_end.byte_offset,
            bit: inner_end.bit_offset,
          ),
          expected: [
            "complete consumption of "
            <> int.to_string(byte_len)
            <> "-byte window",
          ],
          context: [],
          message: None,
        ),
        True,
        state.committed,
      )
  }
}

fn within_bytes_partial_check(
  state: State,
  byte_len: Int,
  value: a,
  inner_end: State,
  window: BitArray,
) -> Step(#(a, BitArray)) {
  let effective_inner_offset = effective_byte_offset(inner_end)
  let unconsumed = byte_len - effective_inner_offset
  let leftover = case
    bit_array.slice(window, effective_inner_offset, unconsumed)
  {
    Ok(rest) -> rest
    _ -> <<>>
  }
  Continue(
    #(value, leftover),
    State(..state, byte_offset: state.byte_offset + byte_len),
    byte_len > 0,
  )
}

fn within_bytes_stop(
  state: State,
  error: BittyError,
  consumed: Bool,
  committed: Bool,
) -> Step(a) {
  Stop(
    BittyError(
      ..error,
      at: Location(byte: state.byte_offset + error.at.byte, bit: error.at.bit),
    ),
    consumed,
    committed || state.committed,
  )
}

/// Like `within_bytes`, but the inner parser may stop early. Returns the
/// parsed value and any unconsumed bytes from the window as a tuple.
/// Requires byte alignment.
pub fn within_bytes_partial(
  byte_len: Int,
  run inner: Parser(a),
) -> Parser(#(a, BitArray)) {
  Parser(fn(state) {
    use <- bool.guard(
      when: byte_len < 0,
      return: stop_expected(state, "non-negative byte length"),
    )
    use <- require_aligned(state)
    within_bytes_partial_run(state, byte_len, inner)
  })
}

fn within_bytes_partial_run(
  state: State,
  byte_len: Int,
  inner: Parser(a),
) -> Step(#(a, BitArray)) {
  within_bytes_setup(state, byte_len, inner, fn(st, window, result) {
    case result {
      Continue(value, inner_end, _consumed) ->
        within_bytes_partial_check(st, byte_len, value, inner_end, window)
      Stop(error, consumed, committed) ->
        within_bytes_stop(st, error, consumed, committed)
    }
  })
}

@internal
pub fn read_uint(
  state: State,
  remaining: Int,
  acc: Int,
  consumed: Bool,
) -> Step(Int) {
  case remaining <= 0 {
    True -> Continue(acc, state, consumed)
    False ->
      case bit_array.slice(state.input, state.byte_offset, 1) {
        Ok(<<byte>>) -> {
          let available = 8 - state.bit_offset
          let to_read = int.min(available, remaining)
          let shift = available - to_read
          let mask = int.bitwise_shift_left(1, to_read) - 1
          let bits_val =
            int.bitwise_and(int.bitwise_shift_right(byte, shift), mask)
          let new_acc =
            int.bitwise_or(int.bitwise_shift_left(acc, to_read), bits_val)
          let #(new_byte, new_bit) =
            advance_bits(state.byte_offset, state.bit_offset, to_read)
          let new_state =
            State(..state, byte_offset: new_byte, bit_offset: new_bit)
          read_uint(new_state, remaining - to_read, new_acc, consumed)
        }
        _ ->
          Stop(
            BittyError(
              at: Location(byte: state.byte_offset, bit: state.bit_offset),
              expected: [int.to_string(remaining) <> " more bits"],
              context: [],
              message: None,
            ),
            consumed,
            state.committed,
          )
      }
  }
}

@internal
pub fn advance_bits(
  byte_offset: Int,
  bit_offset: Int,
  count: Int,
) -> #(Int, Int) {
  let total_bits = bit_offset + count
  #(byte_offset + total_bits / 8, total_bits % 8)
}

@internal
pub fn read_n_bytes(state: State, count: Int) -> Step(BitArray) {
  use <- bool.guard(
    when: count < 0,
    return: stop_expected(state, "non-negative byte count"),
  )

  case state.bit_offset == 0 {
    True -> read_n_bytes_aligned(state, count)
    False -> read_n_bytes_unaligned(state, count, <<>>, count > 0)
  }
}

fn read_n_bytes_aligned(state: State, count: Int) -> Step(BitArray) {
  case bit_array.slice(state.input, state.byte_offset, count) {
    Ok(bytes) ->
      Continue(
        bytes,
        State(..state, byte_offset: state.byte_offset + count),
        count > 0,
      )
    _ -> stop_expected(state, int.to_string(count) <> " bytes")
  }
}

fn read_n_bytes_unaligned(
  state: State,
  remaining: Int,
  acc: BitArray,
  consumed: Bool,
) -> Step(BitArray) {
  case remaining <= 0 {
    True -> Continue(acc, state, consumed)
    False ->
      case read_uint(state, 8, 0, consumed) {
        Continue(byte_val, new_state, _) ->
          read_n_bytes_unaligned(
            new_state,
            remaining - 1,
            <<acc:bits, byte_val>>,
            consumed,
          )
        Stop(error, consumed2, committed) -> Stop(error, consumed2, committed)
      }
  }
}

@internal
pub fn stop_expected(state: State, expected: String) -> Step(a) {
  Stop(
    BittyError(
      at: Location(byte: state.byte_offset, bit: state.bit_offset),
      expected: [expected],
      context: [],
      message: None,
    ),
    False,
    state.committed,
  )
}

@internal
pub fn require_aligned(state: State, then continue: fn() -> Step(a)) -> Step(a) {
  case state.bit_offset != 0 {
    True -> stop_expected(state, "byte alignment")
    False -> continue()
  }
}

@internal
pub fn effective_byte_offset(state: State) -> Int {
  case state.bit_offset != 0 {
    True -> state.byte_offset + 1
    False -> state.byte_offset
  }
}

fn remaining_slice(state: State) -> BitArray {
  let offset = effective_byte_offset(state)
  let remaining = bit_array.byte_size(state.input) - offset
  case bit_array.slice(state.input, offset, remaining) {
    Ok(rest) -> rest
    _ -> <<>>
  }
}

fn new_state(input: BitArray) -> State {
  State(input: input, byte_offset: 0, bit_offset: 0, committed: False)
}
