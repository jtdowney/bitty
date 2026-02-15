import bitty
import bitty/bytes as b
import bitty/num

pub type PcapHeader {
  PcapHeader(
    version_major: Int,
    version_minor: Int,
    snaplen: Int,
    link_type: Int,
  )
}

pub type PcapPacket {
  PcapPacket(timestamp_secs: Int, timestamp_usecs: Int, data: BitArray)
}

pub type PcapFile {
  PcapFile(header: PcapHeader, packets: List(PcapPacket))
}

pub fn file() -> bitty.Parser(PcapFile) {
  use header <- bitty.then(pcap_header())
  use packets <- bitty.then(bitty.many(packet()))
  bitty.success(PcapFile(header:, packets:))
}

fn pcap_header() -> bitty.Parser(PcapHeader) {
  use _magic <- bitty.then(b.tag(<<0xD4, 0xC3, 0xB2, 0xA1>>))
  use version_major <- bitty.then(num.u16(num.LittleEndian))
  use version_minor <- bitty.then(num.u16(num.LittleEndian))
  use _tz_offset <- bitty.then(b.skip(4))
  use _ts_accuracy <- bitty.then(b.skip(4))
  use snaplen <- bitty.then(num.u32(num.LittleEndian))
  use link_type <- bitty.then(num.u32(num.LittleEndian))
  bitty.success(PcapHeader(version_major:, version_minor:, snaplen:, link_type:))
}

fn packet() -> bitty.Parser(PcapPacket) {
  use timestamp_secs <- bitty.then(num.u32(num.LittleEndian))
  use timestamp_usecs <- bitty.then(num.u32(num.LittleEndian))
  use captured_length <- bitty.then(num.u32(num.LittleEndian))
  use _original_length <- bitty.then(num.u32(num.LittleEndian))
  use data <- bitty.then(b.take(captured_length))
  bitty.success(PcapPacket(timestamp_secs:, timestamp_usecs:, data:))
}
