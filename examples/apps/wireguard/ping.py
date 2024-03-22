from __future__ import annotations

import ipaddress

from rflx.model import NeverVerify
from rflx.pyrflx import MessageValue, PyRFLX, utils
from rflx.pyrflx.error import PyRFLXError


def icmp_checksum(message: bytes, **kwargs: object) -> int:
    first_arg = kwargs.get("Tag'First .. Checksum'First - 1")
    assert isinstance(first_arg, tuple)
    tag_first, checksum_first_minus_one = first_arg
    assert tag_first == 0
    assert checksum_first_minus_one == 15
    second_arg = kwargs.get("Checksum'Last + 1 .. Message'Last")
    assert isinstance(second_arg, tuple)
    checksum_last_plus_one, data_last = second_arg
    assert checksum_last_plus_one == 32
    assert data_last == 511
    checksum_size = kwargs.get("Checksum'Size")
    assert isinstance(checksum_size, int)
    assert checksum_size == 16

    checksum_bytes = message[tag_first : (checksum_first_minus_one + 1) // 8]
    checksum_bytes += b"\x00" * (checksum_size // 8)
    checksum_bytes += message[(checksum_last_plus_one // 8) : (data_last + 1) // 8]
    return utils.internet_checksum(checksum_bytes)


PYRFLX = PyRFLX.from_specs(["specs/ipv4.rflx"], NeverVerify())
ICMP = PYRFLX.package("ICMP")
ICMP.set_checksum_functions({"Message": {"Checksum": icmp_checksum}})
IP = PYRFLX.package("IPv4")

ICMP_DATA = bytes(list(range(56)))


def handle_ping(data: bytes) -> bytes | None:
    try:
        ip_packet = parse_ip_layer(data)
        icmp_packet = ip_packet.get("Payload")
        assert isinstance(icmp_packet, MessageValue)
    except PyRFLXError as e:
        print("invalid ping data response:", e)  # noqa: T201
        return None

    packet_src = str(ipaddress.IPv4Address(ip_packet.get("Source")))
    packet_dest = str(ipaddress.IPv4Address(ip_packet.get("Destination")))

    if icmp_packet.get("Tag") == "Echo_Request":
        handle_ping_response(icmp_packet, "10.1.0.1", packet_src)
        return None

    return handle_ping_request(icmp_packet, packet_src, packet_dest)


def handle_ping_response(packet: MessageValue, target_ip: str, packet_src: str) -> None:
    packet_src = str(ipaddress.IPv4Address(packet.get("Source")))
    if packet_src != target_ip:
        print(  # noqa: T201
            "Received ICMP_ECHO source IP is different than the ICMP_REQUEST's destination IP",
        )
        return

    reply = packet.get("Payload")
    assert isinstance(reply, MessageValue)

    if reply.get("Tag") != "ICMP::Echo_Reply":
        print("Expected ICMP::Echo_Reply but got", reply.get("Tag"))  # noqa: T201
        return

    reply_seq = str(reply.get("Sequence_Number"))
    print(  # noqa: T201
        f"{int(reply.size) // 8} bytes from {packet_src}: icmp_seq={reply_seq}",
        flush=True,
    )

    if reply.get("Data") != ICMP_DATA:
        print("mismatch between sent and received data")  # noqa: T201


def handle_ping_request(packet: MessageValue, src_ip: str, dest_ip: str) -> bytes:
    src = int(ipaddress.IPv4Address(src_ip))
    dest = int(ipaddress.IPv4Address(dest_ip))
    identifier = packet.get("Identifier")
    sequence_number = packet.get("Sequence_Number")
    data = packet.get("Data")

    assert isinstance(identifier, int)
    assert isinstance(sequence_number, int)
    assert isinstance(data, bytes)

    return create_request(dest, src, identifier, sequence_number, data, "Echo_Reply")


def create_request(
    src: int,
    dst: int,
    identifier: int,
    seq: int,
    data: bytes | None = None,
    icmp_tag: str = "Echo_Request",
) -> bytes:
    msg = ICMP.new_message("Message")
    msg.set("Tag", icmp_tag)
    msg.set("Code_Zero", 0)
    msg.set("Checksum", 0)
    msg.set("Identifier", identifier)
    msg.set("Sequence_Number", seq)
    msg.set("Data", data or ICMP_DATA)
    msg.update_checksums()

    pkt = IP.new_message("Packet")
    pkt.set("Version", 4)
    pkt.set("IHL", 5)
    pkt.set("DSCP", 0)
    pkt.set("ECN", 0)
    pkt.set("Total_Length", 20 + len(msg.bytestring))
    pkt.set("Identification", 1)
    pkt.set("Flag_R", "False")
    pkt.set("Flag_DF", "False")
    pkt.set("Flag_MF", "False")
    pkt.set("Fragment_Offset", 0)
    pkt.set("TTL", 64)
    pkt.set("Protocol", "P_ICMP")
    pkt.set("Header_Checksum", 0)
    pkt.set("Source", src)
    pkt.set("Destination", dst)
    pkt.set("Options", [])
    pkt.set("Payload", msg.bytestring)

    # Compute checksum
    checksum = utils.internet_checksum(pkt.bytestring[:20])

    pkt.set("Header_Checksum", checksum)
    pkt.set("Source", src)
    pkt.set("Destination", dst)
    pkt.set("Options", [])
    pkt.set("Payload", msg.bytestring)

    return pkt.bytestring


def parse_ip_layer(message: bytes) -> MessageValue:
    pkt = IP.new_message("Packet")
    pkt.parse(message)
    return pkt


def parse_icmp_layer(message: bytes) -> MessageValue:
    pkt = ICMP.new_message("Message")
    pkt.parse(message)
    return pkt
