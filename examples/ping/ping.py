#!/usr/bin/env -S python3 -O

import ipaddress
import socket
import sys
import time

from rflx.pyrflx import MessageValue, PyRFLX

PYRFLX = PyRFLX(["specs/ipv4.rflx"])
ICMP = PYRFLX["ICMP"]
IP = PYRFLX["IPv4"]

ICMP_DATA = bytes(list(range(0, 56)))


def ping(target: str) -> None:
    local = get_local_ip_address()
    target_ip = socket.gethostbyname(target)

    print(f"PING {target} ({target_ip})")

    sock_out = socket.socket(socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_RAW)
    sock_in = socket.socket(socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_ICMP)

    seq = 0
    while True:
        sock_out.sendto(
            create_request(
                int(ipaddress.IPv4Address(local)), int(ipaddress.IPv4Address(target_ip)), seq
            ),
            (target, 0),
        )

        packet = parse_reply(sock_in.recv(4096))
        packet_src = str(ipaddress.IPv4Address(packet.get("Source")))
        if packet_src != target_ip:
            continue

        reply = packet.get("Payload")
        assert isinstance(reply, MessageValue)

        reply_seq = str(reply.get("Sequence_Number"))
        print(f"{int(reply.size) // 8} bytes from {packet_src}: icmp_seq={reply_seq}")

        if reply.get("Tag") != "Echo_Reply":
            print("unexpected type")
        if reply.get("Data") != ICMP_DATA:
            print("mismatch between sent and received data")

        time.sleep(1)
        seq = (seq + 1) % 2 ** 16


def create_request(src: int, dst: int, seq: int) -> bytes:
    msg = ICMP["Message"]
    msg.set("Tag", "Echo_Request")
    msg.set("Code_Zero", 0)
    msg.set("Checksum", 0)
    msg.set("Identifier", 0)
    msg.set("Sequence_Number", seq)
    msg.set("Data", ICMP_DATA)
    msg.set("Checksum", icmp_checksum(msg.bytestring))

    pkt = IP["Packet"]
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
    pkt.set("Protocol", "PROTOCOL_ICMP")
    pkt.set("Header_Checksum", 0)
    pkt.set("Source", src)
    pkt.set("Destination", dst)
    pkt.set("Options", [])
    pkt.set("Payload", msg.bytestring)

    return pkt.bytestring


def parse_reply(message: bytes) -> MessageValue:
    pkt = IP["Packet"]
    pkt.parse(message)
    return pkt


def icmp_checksum(message: bytes) -> int:
    def add_ones_complement(num1: int, num2: int) -> int:
        mod = 1 << 16
        result = num1 + num2
        return result if result < mod else (result + 1) % mod

    chunks = [int.from_bytes(message[i : i + 2], "big") for i in range(0, len(message), 2)]
    intermediary_result = chunks[0]
    for i in range(1, len(chunks)):
        intermediary_result = add_ones_complement(intermediary_result, chunks[i])

    return intermediary_result ^ 0xFFFF


def get_local_ip_address() -> str:
    s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
    s.connect(("8.8.8.8", 80))
    address = s.getsockname()[0]
    s.close()
    return address


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("ping: usage error: Destination address required")
    ping(sys.argv[1])
