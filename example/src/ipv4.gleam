import bitty
import bitty/bits
import bitty/num
import gleam/int
import gleam/string

pub type Ipv4Address {
  Ipv4Address(a: Int, b: Int, c: Int, d: Int)
}

pub type Ipv4Flags {
  Ipv4Flags(dont_fragment: Bool, more_fragments: Bool)
}

pub type Ipv4Header {
  Ipv4Header(
    version: Int,
    ihl: Int,
    dscp: Int,
    ecn: Int,
    total_length: Int,
    identification: Int,
    flags: Ipv4Flags,
    fragment_offset: Int,
    ttl: Int,
    protocol: Int,
    checksum: Int,
    source: Ipv4Address,
    destination: Ipv4Address,
  )
}

pub fn header() -> bitty.Parser(Ipv4Header) {
  use version <- bitty.then(bits.uint(4))
  use ihl <- bitty.then(bits.uint(4))
  use dscp <- bitty.then(bits.uint(6))
  use ecn <- bitty.then(bits.uint(2))
  use total_length <- bitty.then(num.u16(num.BigEndian))
  use identification <- bitty.then(num.u16(num.BigEndian))
  use _reserved <- bitty.then(bits.bit())
  use dont_fragment <- bitty.then(bits.bit())
  use more_fragments <- bitty.then(bits.bit())
  use fragment_offset <- bitty.then(bits.uint(13))
  use ttl <- bitty.then(num.u8())
  use protocol <- bitty.then(num.u8())
  use checksum <- bitty.then(num.u16(num.BigEndian))
  use source <- bitty.then(address())
  use destination <- bitty.then(address())
  bitty.success(Ipv4Header(
    version:,
    ihl:,
    dscp:,
    ecn:,
    total_length:,
    identification:,
    flags: Ipv4Flags(dont_fragment:, more_fragments:),
    fragment_offset:,
    ttl:,
    protocol:,
    checksum:,
    source:,
    destination:,
  ))
}

fn address() -> bitty.Parser(Ipv4Address) {
  use a <- bitty.then(num.u8())
  use b_ <- bitty.then(num.u8())
  use c <- bitty.then(num.u8())
  use d <- bitty.then(num.u8())
  bitty.success(Ipv4Address(a:, b: b_, c:, d:))
}

pub fn header_to_string(h: Ipv4Header) -> String {
  string.join(
    [
      "IPv4 Header:",
      "  Version:         " <> int.to_string(h.version),
      "  IHL:             " <> int.to_string(h.ihl),
      "  DSCP:            " <> int.to_string(h.dscp),
      "  ECN:             " <> int.to_string(h.ecn),
      "  Total Length:    " <> int.to_string(h.total_length),
      "  Identification:  0x"
        <> string.pad_start(int.to_base16(h.identification), 4, "0"),
      "  Flags:           " <> flags_to_string(h.flags),
      "  Fragment Offset: " <> int.to_string(h.fragment_offset),
      "  TTL:             " <> int.to_string(h.ttl),
      "  Protocol:        " <> protocol_to_string(h.protocol),
      "  Checksum:        0x"
        <> string.pad_start(int.to_base16(h.checksum), 4, "0"),
      "  Source:          " <> address_to_string(h.source),
      "  Destination:     " <> address_to_string(h.destination),
    ],
    "\n",
  )
}

fn address_to_string(addr: Ipv4Address) -> String {
  int.to_string(addr.a)
  <> "."
  <> int.to_string(addr.b)
  <> "."
  <> int.to_string(addr.c)
  <> "."
  <> int.to_string(addr.d)
}

fn flags_to_string(flags: Ipv4Flags) -> String {
  case flags.dont_fragment, flags.more_fragments {
    True, True -> "DF MF"
    True, False -> "DF"
    False, True -> "MF"
    False, False -> "(none)"
  }
}

fn protocol_to_string(proto: Int) -> String {
  case proto {
    1 -> "ICMP (1)"
    6 -> "TCP (6)"
    17 -> "UDP (17)"
    _ -> int.to_string(proto)
  }
}
