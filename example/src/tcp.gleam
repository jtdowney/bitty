import bitty
import bitty/bits
import bitty/bytes
import bitty/num
import gleam/int
import gleam/list
import gleam/string

pub type TcpFlags {
  TcpFlags(
    cwr: Bool,
    ece: Bool,
    urg: Bool,
    ack: Bool,
    psh: Bool,
    rst: Bool,
    syn: Bool,
    fin: Bool,
  )
}

pub type TcpHeader {
  TcpHeader(
    source_port: Int,
    destination_port: Int,
    sequence_number: Int,
    acknowledgment_number: Int,
    data_offset: Int,
    flags: TcpFlags,
    window_size: Int,
    checksum: Int,
    urgent_pointer: Int,
    options: BitArray,
  )
}

pub fn header() -> bitty.Parser(TcpHeader) {
  use source_port <- bitty.then(num.u16(num.BigEndian))
  use destination_port <- bitty.then(num.u16(num.BigEndian))
  use sequence_number <- bitty.then(num.u32(num.BigEndian))
  use acknowledgment_number <- bitty.then(num.u32(num.BigEndian))
  use data_offset <- bitty.then(bits.uint(4))
  use _reserved <- bitty.then(bits.uint(4))
  use cwr <- bitty.then(bits.bit())
  use ece <- bitty.then(bits.bit())
  use urg <- bitty.then(bits.bit())
  use ack <- bitty.then(bits.bit())
  use psh <- bitty.then(bits.bit())
  use rst <- bitty.then(bits.bit())
  use syn <- bitty.then(bits.bit())
  use fin <- bitty.then(bits.bit())
  use window_size <- bitty.then(num.u16(num.BigEndian))
  use checksum <- bitty.then(num.u16(num.BigEndian))
  use urgent_pointer <- bitty.then(num.u16(num.BigEndian))
  let options_len = { data_offset * 4 } - 20
  use options <- bitty.then(bytes.take(options_len))
  bitty.success(TcpHeader(
    source_port:,
    destination_port:,
    sequence_number:,
    acknowledgment_number:,
    data_offset:,
    flags: TcpFlags(cwr:, ece:, urg:, ack:, psh:, rst:, syn:, fin:),
    window_size:,
    checksum:,
    urgent_pointer:,
    options:,
  ))
}

pub fn header_to_string(h: TcpHeader) -> String {
  string.join(
    [
      "TCP Header:",
      "  Source Port:      " <> int.to_string(h.source_port),
      "  Destination Port: " <> int.to_string(h.destination_port),
      "  Sequence Number:  " <> int.to_string(h.sequence_number),
      "  Ack Number:       " <> int.to_string(h.acknowledgment_number),
      "  Data Offset:      " <> int.to_string(h.data_offset),
      "  Flags:            " <> flags_to_string(h.flags),
      "  Window Size:      " <> int.to_string(h.window_size),
      "  Checksum:         0x"
        <> string.pad_start(int.to_base16(h.checksum), 4, "0"),
      "  Urgent Pointer:   " <> int.to_string(h.urgent_pointer),
    ],
    "\n",
  )
}

pub fn flags_to_string(flags: TcpFlags) -> String {
  let parts =
    [
      #(flags.cwr, "CWR"),
      #(flags.ece, "ECE"),
      #(flags.urg, "URG"),
      #(flags.ack, "ACK"),
      #(flags.psh, "PSH"),
      #(flags.rst, "RST"),
      #(flags.syn, "SYN"),
      #(flags.fin, "FIN"),
    ]
    |> list.filter(fn(pair) { pair.0 })
    |> list.map(fn(pair) { pair.1 })

  case parts {
    [] -> "(none)"
    _ -> string.join(parts, " ")
  }
}
