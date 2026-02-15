import bitty
import bitty/bits
import bitty/bytes as b
import bitty/num
import gleam/bit_array
import gleam/bool
import gleam/int
import gleam/list
import gleam/result
import gleam/string

pub type DnsHeader {
  DnsHeader(
    id: Int,
    qr: Bool,
    opcode: Int,
    aa: Bool,
    tc: Bool,
    rd: Bool,
    ra: Bool,
    rcode: Int,
    qd_count: Int,
    an_count: Int,
    ns_count: Int,
    ar_count: Int,
  )
}

pub type DnsQuestion {
  DnsQuestion(name: List(String), qtype: Int, qclass: Int)
}

pub type DnsRecord {
  DnsRecord(
    name: List(String),
    rtype: Int,
    rclass: Int,
    ttl: Int,
    rdata: BitArray,
  )
}

pub type DnsMessage {
  DnsMessage(
    header: DnsHeader,
    questions: List(DnsQuestion),
    answers: List(DnsRecord),
  )
}

pub fn message() -> bitty.Parser(DnsMessage) {
  use hdr <- bitty.then(header())
  use questions <- bitty.then(bitty.repeat(question(), times: hdr.qd_count))
  use answers <- bitty.then(bitty.repeat(record(), times: hdr.an_count))
  bitty.success(DnsMessage(header: hdr, questions:, answers:))
}

fn header() -> bitty.Parser(DnsHeader) {
  use id <- bitty.then(num.u16(num.BigEndian))
  use qr <- bitty.then(bits.bit())
  use opcode <- bitty.then(bits.uint(4))
  use aa <- bitty.then(bits.bit())
  use tc <- bitty.then(bits.bit())
  use rd <- bitty.then(bits.bit())
  use ra <- bitty.then(bits.bit())
  use _z <- bitty.then(bits.uint(3))
  use rcode <- bitty.then(bits.uint(4))
  use qd_count <- bitty.then(num.u16(num.BigEndian))
  use an_count <- bitty.then(num.u16(num.BigEndian))
  use ns_count <- bitty.then(num.u16(num.BigEndian))
  use ar_count <- bitty.then(num.u16(num.BigEndian))
  bitty.success(DnsHeader(
    id:,
    qr:,
    opcode:,
    aa:,
    tc:,
    rd:,
    ra:,
    rcode:,
    qd_count:,
    an_count:,
    ns_count:,
    ar_count:,
  ))
}

fn question() -> bitty.Parser(DnsQuestion) {
  use labels <- bitty.then(name())
  use qtype <- bitty.then(num.u16(num.BigEndian))
  use qclass <- bitty.then(num.u16(num.BigEndian))
  bitty.success(DnsQuestion(name: labels, qtype:, qclass:))
}

fn name() -> bitty.Parser(List(String)) {
  use len <- bitty.then(num.u8())
  case int.bitwise_and(len, 0xC0) == 0xC0 {
    True -> {
      use _offset_low <- bitty.then(num.u8())
      bitty.success(["<compressed>"])
    }
    False -> name_label(len)
  }
}

fn name_label(len: Int) -> bitty.Parser(List(String)) {
  use <- bool.guard(when: len == 0, return: bitty.success([]))
  use label_bytes <- bitty.then(b.take(len))
  let label = bit_array.to_string(label_bytes) |> result.unwrap("<invalid>")
  use rest <- bitty.then(name())
  bitty.success([label, ..rest])
}

fn record() -> bitty.Parser(DnsRecord) {
  use name <- bitty.then(name())
  use rtype <- bitty.then(num.u16(num.BigEndian))
  use rclass <- bitty.then(num.u16(num.BigEndian))
  use ttl <- bitty.then(num.u32(num.BigEndian))
  use rdlength <- bitty.then(num.u16(num.BigEndian))
  use rdata <- bitty.then(b.take(rdlength))
  bitty.success(DnsRecord(name:, rtype:, rclass:, ttl:, rdata:))
}

pub fn message_to_string(m: DnsMessage) -> String {
  let parts = [header_to_string(m.header), questions_to_string(m.questions)]
  let parts = case m.answers {
    [] -> parts
    _ -> list.append(parts, [answers_to_string(m.answers)])
  }
  string.join(parts, "\n")
}

fn header_to_string(h: DnsHeader) -> String {
  string.join(
    [
      "DNS Header:",
      "  ID:       0x" <> string.pad_start(int.to_base16(h.id), 4, "0"),
      "  QR:       " <> bool_to_string(h.qr) <> qr_label(h.qr),
      "  Opcode:   " <> int.to_string(h.opcode),
      "  AA:       " <> bool_to_string(h.aa),
      "  TC:       " <> bool_to_string(h.tc),
      "  RD:       " <> bool_to_string(h.rd),
      "  RA:       " <> bool_to_string(h.ra),
      "  RCODE:    " <> int.to_string(h.rcode),
      "  Questions:     " <> int.to_string(h.qd_count),
      "  Answers:       " <> int.to_string(h.an_count),
      "  Authority:     " <> int.to_string(h.ns_count),
      "  Additional:    " <> int.to_string(h.ar_count),
    ],
    "\n",
  )
}

fn questions_to_string(questions: List(DnsQuestion)) -> String {
  questions
  |> list.index_map(fn(q, i) { question_to_string(q, i) })
  |> string.join("\n")
}

fn question_to_string(q: DnsQuestion, index: Int) -> String {
  string.join(
    [
      "Question " <> int.to_string(index + 1) <> ":",
      "  Name:   " <> string.join(q.name, "."),
      "  Type:   " <> qtype_to_string(q.qtype),
      "  Class:  " <> qclass_to_string(q.qclass),
    ],
    "\n",
  )
}

fn answers_to_string(answers: List(DnsRecord)) -> String {
  answers
  |> list.index_map(fn(r, i) { answer_to_string(r, i) })
  |> string.join("\n")
}

fn answer_to_string(r: DnsRecord, index: Int) -> String {
  string.join(
    [
      "Answer " <> int.to_string(index + 1) <> ":",
      "  Name:   " <> string.join(r.name, "."),
      "  Type:   " <> qtype_to_string(r.rtype),
      "  Class:  " <> qclass_to_string(r.rclass),
      "  TTL:    " <> int.to_string(r.ttl),
      "  Data:   " <> rdata_to_string(r.rtype, r.rdata),
    ],
    "\n",
  )
}

fn rdata_to_string(rtype: Int, rdata: BitArray) -> String {
  case rtype {
    1 ->
      case rdata {
        <<a, b_, c, d>> ->
          int.to_string(a)
          <> "."
          <> int.to_string(b_)
          <> "."
          <> int.to_string(c)
          <> "."
          <> int.to_string(d)
        _ -> bit_array.inspect(rdata)
      }
    _ -> bit_array.inspect(rdata)
  }
}

fn bool_to_string(b: Bool) -> String {
  case b {
    True -> "1"
    False -> "0"
  }
}

fn qr_label(qr: Bool) -> String {
  case qr {
    False -> " (Query)"
    True -> " (Response)"
  }
}

fn qtype_to_string(qtype: Int) -> String {
  case qtype {
    1 -> "A (1)"
    2 -> "NS (2)"
    5 -> "CNAME (5)"
    15 -> "MX (15)"
    28 -> "AAAA (28)"
    _ -> int.to_string(qtype)
  }
}

fn qclass_to_string(qclass: Int) -> String {
  case qclass {
    1 -> "IN (1)"
    _ -> int.to_string(qclass)
  }
}
