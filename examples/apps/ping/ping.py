#!/usr/bin/env -S ./python -O

import errno
import fcntl
import ipaddress
import os
import select
import socket
import sys
import time

from rflx.pyrflx import MessageValue, PyRFLX, utils


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


PYRFLX = PyRFLX.from_specs(["specs/ipv4.rflx"], skip_model_verification=True)
ICMP = PYRFLX.package("ICMP")
ICMP.set_checksum_functions({"Message": {"Checksum": icmp_checksum}})
IP = PYRFLX.package("IPv4")

ICMP_DATA = bytes(list(range(0, 56)))


def ping(target: str) -> None:
    target_ip = socket.gethostbyname(target)

    print(f"PING {target} ({target_ip})")  # noqa: T201

    sock_out = socket.socket(socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_RAW)
    sock_in = socket.socket(socket.AF_INET, socket.SOCK_RAW, socket.IPPROTO_ICMP)
    fcntl.fcntl(sock_in, fcntl.F_SETFL, os.O_NONBLOCK)

    seq = 0
    while True:
        sock_out.sendto(
            create_request(0, int(ipaddress.IPv4Address(target_ip)), seq),
            (target, 0),
        )
        seq = (seq + 1) % 2**16

        receiving = True
        while receiving:
            try:
                select.select([sock_in], [], [], 1)
                data = sock_in.recv(4096)
                try:
                    packet = parse_reply(data)
                except AssertionError:
                    # Invalid data has been parsed
                    continue
                packet_src = str(ipaddress.IPv4Address(packet.get("Source")))
                if packet_src != target_ip:
                    continue

                reply = packet.get("Payload")
                assert isinstance(reply, MessageValue)

                if reply.get("Tag") != "ICMP::Echo_Reply":
                    continue

                reply_seq = str(reply.get("Sequence_Number"))
                print(  # noqa: T201
                    f"{int(reply.size) // 8} bytes from {packet_src}: icmp_seq={reply_seq}",
                    flush=True,
                )

                if reply.get("Data") != ICMP_DATA:
                    print("mismatch between sent and received data")  # noqa: T201

                time.sleep(1)
                receiving = False
            except OSError as e:  # noqa: PERF203
                if e.args[0] in [errno.EAGAIN, errno.EWOULDBLOCK]:
                    time.sleep(1)
                    receiving = False
                else:
                    print(e)  # noqa: T201
                    sys.exit(1)


def create_request(src: int, dst: int, seq: int) -> bytes:
    msg = ICMP.new_message("Message")
    msg.set("Tag", "Echo_Request")
    msg.set("Code_Zero", 0)
    msg.set("Checksum", 0)
    msg.set("Identifier", 0)
    msg.set("Sequence_Number", seq)
    msg.set("Data", ICMP_DATA)
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

    return pkt.bytestring


def parse_reply(message: bytes) -> MessageValue:
    pkt = IP.new_message("Packet")
    pkt.parse(message)
    return pkt


if __name__ == "__main__":
    if len(sys.argv) < 2:
        sys.exit("ping: usage error: Destination address required")
    ping(sys.argv[1])
