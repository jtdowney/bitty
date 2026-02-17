import bitty
import bitty/bytes
import bitty/string as s
import gleam/bit_array
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type HttpHeader {
  HttpHeader(name: String, value: String)
}

pub type HttpRequest {
  HttpRequest(
    method: String,
    uri: String,
    version: String,
    headers: List(HttpHeader),
  )
}

pub type HttpResponse {
  HttpResponse(
    version: String,
    status_code: Int,
    reason_phrase: String,
    headers: List(HttpHeader),
    body: String,
  )
}

pub fn request() -> bitty.Parser(HttpRequest) {
  use method <- bitty.then(s.alpha1())
  use _ <- bitty.then(s.literal(" "))
  use uri <- bitty.then(s.take_while1(fn(c) { c != " " }))
  use _ <- bitty.then(s.literal(" "))
  use _ <- bitty.then(s.literal("HTTP/"))
  use version <- bitty.then(s.not_line_ending())
  use _ <- bitty.then(s.crlf())
  use headers <- bitty.then(headers())
  use _ <- bitty.then(s.crlf())
  bitty.success(HttpRequest(method:, uri:, version:, headers:))
}

pub fn response() -> bitty.Parser(HttpResponse) {
  use _ <- bitty.then(s.literal("HTTP/"))
  use version <- bitty.then(s.take_while1(fn(c) { c != " " }))
  use _ <- bitty.then(s.literal(" "))
  use status_code <- bitty.then(s.integer())
  use _ <- bitty.then(s.literal(" "))
  use reason_phrase <- bitty.then(s.not_line_ending())
  use _ <- bitty.then(s.crlf())
  use headers <- bitty.then(headers())
  use _ <- bitty.then(s.crlf())
  use body_bytes <- bitty.then(bytes.rest())
  let body =
    bit_array.to_string(body_bytes)
    |> result.unwrap("<binary>")
  bitty.success(HttpResponse(version:, status_code:, reason_phrase:, headers:, body:))
}

fn headers() -> bitty.Parser(List(HttpHeader)) {
  bitty.many(bitty.attempt(header_line()))
}

fn header_line() -> bitty.Parser(HttpHeader) {
  use name <- bitty.then(s.take_while1(fn(c) { c != ":" }))
  use _ <- bitty.then(s.literal(": "))
  use value <- bitty.then(s.not_line_ending())
  use _ <- bitty.then(s.crlf())
  bitty.success(HttpHeader(name:, value:))
}

pub fn request_to_string(r: HttpRequest) -> String {
  string.join(
    [
      "HTTP Request:",
      "  Method:  " <> r.method,
      "  URI:     " <> r.uri,
      "  Version: HTTP/" <> r.version,
      headers_to_string(r.headers),
    ],
    "\n",
  )
}

pub fn response_to_string(r: HttpResponse) -> String {
  let body_preview = case string.length(r.body) > 80 {
    True -> string.slice(r.body, 0, 80) <> "..."
    False -> r.body
  }
  string.join(
    [
      "HTTP Response:",
      "  Version: HTTP/" <> r.version,
      "  Status:  " <> int.to_string(r.status_code) <> " " <> r.reason_phrase,
      headers_to_string(r.headers),
      "  Body:    " <> body_preview,
    ],
    "\n",
  )
}

fn headers_to_string(headers: List(HttpHeader)) -> String {
  case headers {
    [] -> "  Headers: (none)"
    _ ->
      "  Headers:\n"
      <> headers
      |> list.map(fn(h) { "    " <> h.name <> ": " <> h.value })
      |> string.join("\n")
  }
}
