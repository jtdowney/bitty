import bitty
import bitty/bytes as b
import bitty/num
import gleam/int
import gleam/list
import gleam/string

pub type MacAddress {
  MacAddress(bytes: BitArray)
}

pub type EthernetFrame {
  EthernetFrame(
    destination: MacAddress,
    source: MacAddress,
    ethertype: Int,
    payload: BitArray,
  )
}

pub fn frame() -> bitty.Parser(EthernetFrame) {
  use destination <- bitty.then(mac_address())
  use source <- bitty.then(mac_address())
  use ethertype <- bitty.then(num.u16(num.BigEndian))
  use payload <- bitty.then(b.rest())
  bitty.success(EthernetFrame(destination:, source:, ethertype:, payload:))
}

fn mac_address() -> bitty.Parser(MacAddress) {
  use bytes <- bitty.then(b.take(6))
  bitty.success(MacAddress(bytes:))
}

pub fn mac_to_string(mac: MacAddress) -> String {
  case mac.bytes {
    <<b0, b1, b2, b3, b4, b5>> ->
      [b0, b1, b2, b3, b4, b5]
      |> list.map(fn(byte) { string.pad_start(int.to_base16(byte), 2, "0") })
      |> string.join(":")
    _ -> "<invalid>"
  }
}

pub fn ethertype_to_string(et: Int) -> String {
  case et {
    0x0800 -> "IPv4 (0x0800)"
    0x0806 -> "ARP (0x0806)"
    0x86DD -> "IPv6 (0x86DD)"
    _ -> "0x" <> string.pad_start(int.to_base16(et), 4, "0")
  }
}

pub fn frame_to_string(f: EthernetFrame) -> String {
  string.join(
    [
      "Ethernet Frame:",
      "  Destination: " <> mac_to_string(f.destination),
      "  Source:      " <> mac_to_string(f.source),
      "  EtherType:   " <> ethertype_to_string(f.ethertype),
    ],
    "\n",
  )
}
