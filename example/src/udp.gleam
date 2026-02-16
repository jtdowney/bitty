import bitty
import bitty/num
import gleam/int
import gleam/string

pub type UdpHeader {
  UdpHeader(source_port: Int, destination_port: Int, length: Int, checksum: Int)
}

pub fn header() -> bitty.Parser(UdpHeader) {
  use source_port <- bitty.then(num.u16(num.BigEndian))
  use destination_port <- bitty.then(num.u16(num.BigEndian))
  use length <- bitty.then(num.u16(num.BigEndian))
  use checksum <- bitty.then(num.u16(num.BigEndian))
  bitty.success(UdpHeader(source_port:, destination_port:, length:, checksum:))
}

pub fn header_to_string(h: UdpHeader) -> String {
  string.join(
    [
      "UDP Header:",
      "  Source Port:      " <> int.to_string(h.source_port),
      "  Destination Port: " <> int.to_string(h.destination_port),
      "  Length:           " <> int.to_string(h.length),
      "  Checksum:         0x"
        <> string.pad_start(int.to_base16(h.checksum), 4, "0"),
    ],
    "\n",
  )
}
