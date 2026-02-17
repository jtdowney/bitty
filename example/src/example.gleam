import bitty
import dns
import ethernet
import gleam/bit_array
import gleam/int
import gleam/io
import gleam/list
import http
import ipv4
import pcap
import simplifile
import tcp
import udp

pub fn main() {
  let assert Ok(data) = simplifile.read_bits("capture.pcap")
  let assert Ok(pcap_file) = bitty.run(pcap.file(), on: data)

  io.println(
    "PCAP: version "
    <> int.to_string(pcap_file.header.version_major)
    <> "."
    <> int.to_string(pcap_file.header.version_minor)
    <> ", "
    <> int.to_string(list.length(pcap_file.packets))
    <> " packets",
  )

  list.index_map(pcap_file.packets, fn(pkt, i) {
    io.println("\n=== Packet " <> int.to_string(i + 1) <> " ===")
    parse_packet(pkt)
  })
}

fn parse_packet(pkt: pcap.PcapPacket) {
  case bitty.run(ethernet.frame(), on: pkt.data) {
    Ok(frame) -> {
      io.println(ethernet.frame_to_string(frame))
      case frame.ethertype {
        0x0800 -> parse_ipv4(frame.payload)
        _ -> io.println("  (skipping non-IPv4 frame)")
      }
    }
    Error(_) -> io.println("  Failed to parse Ethernet frame")
  }
}

fn parse_ipv4(data: BitArray) {
  case bitty.run_partial(ipv4.header(), on: data) {
    Ok(#(header, rest)) -> {
      io.println(ipv4.header_to_string(header))
      case header.protocol {
        6 -> parse_tcp(rest)
        17 -> parse_udp_dns(rest)
        _ ->
          io.println(
            "  (skipping protocol " <> int.to_string(header.protocol) <> ")",
          )
      }
    }
    Error(_) -> io.println("  Failed to parse IPv4 header")
  }
}

fn parse_tcp(data: BitArray) {
  case bitty.run_partial(tcp.header(), on: data) {
    Ok(#(header, rest)) -> {
      io.println(tcp.header_to_string(header))
      let has_payload = bit_array.byte_size(rest) > 0
      let has_psh = header.flags.psh
      case has_psh && has_payload {
        True -> parse_http(header, rest)
        False -> Nil
      }
    }
    Error(_) -> io.println("  Failed to parse TCP header")
  }
}

fn parse_http(tcp_header: tcp.TcpHeader, data: BitArray) {
  case tcp_header.destination_port {
    80 ->
      case bitty.run(http.request(), on: data) {
        Ok(req) -> io.println(http.request_to_string(req))
        Error(_) -> io.println("  Failed to parse HTTP request")
      }
    _ ->
      case tcp_header.source_port {
        80 ->
          case bitty.run(http.response(), on: data) {
            Ok(resp) -> io.println(http.response_to_string(resp))
            Error(_) -> io.println("  Failed to parse HTTP response")
          }
        _ ->
          io.println(
            "  TCP Payload: "
            <> int.to_string(bit_array.byte_size(data))
            <> " bytes",
          )
      }
  }
}

fn parse_udp_dns(data: BitArray) {
  let parser = {
    use header <- bitty.then(udp.header())
    use message <- bitty.then(dns.message())
    bitty.success(#(header, message))
  }
  case bitty.run_partial(parser, on: data) {
    Ok(#(#(header, message), _)) -> {
      io.println(udp.header_to_string(header))
      io.println(dns.message_to_string(message))
    }
    Error(_) -> io.println("  Failed to parse UDP/DNS")
  }
}
