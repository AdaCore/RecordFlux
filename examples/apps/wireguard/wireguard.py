#!/usr/bin/env python3
from __future__ import annotations

import argparse
import base64
import dataclasses
import os
import signal
import sys
from pathlib import Path
from typing import TYPE_CHECKING

import tai64_bindings

from rflx.model import NeverVerify
from rflx.pyrflx import MessageValue, PyRFLX

if TYPE_CHECKING:
    from .const_values import CONSTRUCTION, IDENTIFIER, LABEL_MAC1, WIREGUARD_PID_FILE
    from .crypto import blk2s_hash, chacha20poly1305_encrypt, dh, dh_generate, hmac, mac
    from .handlers import PACKET_HANDLERS, Connection
    from .wg_socket import WireguardSocket
else:
    from const_values import CONSTRUCTION, IDENTIFIER, LABEL_MAC1, WIREGUARD_PID_FILE
    from crypto import blk2s_hash, chacha20poly1305_encrypt, dh, dh_generate, hmac, mac
    from handlers import PACKET_HANDLERS, Connection
    from wg_socket import WireguardSocket

PYRFLX = PyRFLX.from_specs(["specs/wireguard.rflx"], NeverVerify())
WIREGUARD = PYRFLX.package("Wireguard")


@dataclasses.dataclass()
class StaticKeys:
    static_private_key: bytes
    static_public_key: bytes
    preshared_public_key: bytes


def timestamp() -> bytes:
    t = tai64_bindings.Tai64n.now()
    return t.to_bytes()


def load_keys(args: argparse.Namespace) -> StaticKeys:
    private_key_bytes = Path(args.static_private_key).read_bytes()
    public_key_bytes = Path(args.static_public_key).read_bytes()
    public_key_receiver_bytes = Path(args.peer_public_key).read_bytes()

    private_key_bytes = base64.standard_b64decode(private_key_bytes)
    public_key_bytes = base64.standard_b64decode(public_key_bytes)
    public_key_receiver_bytes = base64.standard_b64decode(public_key_receiver_bytes)

    assert len(private_key_bytes) == 32
    assert len(public_key_bytes) == 32
    assert len(public_key_receiver_bytes) == 32

    return StaticKeys(
        static_private_key=private_key_bytes,
        static_public_key=public_key_bytes,
        preshared_public_key=public_key_receiver_bytes,
    )


def parse_message(message: bytes) -> MessageValue:
    wg_message = WIREGUARD.new_message("Handshake")
    wg_message.parse(message)
    return wg_message


def create_handshake(static_keys: StaticKeys) -> tuple[bytes, Connection]:
    message = WIREGUARD.new_message("Handshake")
    initiator_chaining_key = blk2s_hash(CONSTRUCTION)
    initiator_hash = blk2s_hash(
        blk2s_hash(initiator_chaining_key, IDENTIFIER),
        static_keys.preshared_public_key,
    )
    initiator_ephemeral_private, unencrypted_ephemeral = dh_generate()
    message.set("Message_Type", "Handshake_Init")
    message.set("Reserved", 0)
    session_id = os.urandom(4)
    message.set("Sender", session_id)
    message.set("Ephemeral", unencrypted_ephemeral)
    initiator_hash = blk2s_hash(initiator_hash, unencrypted_ephemeral)

    temp = hmac(initiator_chaining_key, unencrypted_ephemeral)
    initiator_chaining_key = hmac(temp, b"\x01")

    temp = hmac(
        initiator_chaining_key,
        dh(initiator_ephemeral_private, static_keys.preshared_public_key),
    )
    initiator_chaining_key = hmac(temp, b"\x01")
    key = hmac(temp, initiator_chaining_key + b"\x02")

    encrypted_static = chacha20poly1305_encrypt(
        key,
        0,
        static_keys.static_public_key,
        initiator_hash,
    )
    initiator_hash = blk2s_hash(initiator_hash, encrypted_static)
    message.set("Static", encrypted_static)

    temp = hmac(
        initiator_chaining_key,
        dh(static_keys.static_private_key, static_keys.preshared_public_key),
    )
    initiator_chaining_key = hmac(temp, b"\x01")
    key = hmac(temp, initiator_chaining_key + b"\x02")

    encrypted_timestamp = chacha20poly1305_encrypt(key, 0, timestamp(), initiator_hash)
    initiator_hash = blk2s_hash(initiator_hash, encrypted_timestamp)
    message.set("Timestamp", encrypted_timestamp)

    # Set macs
    message.set("Mac_First", b"\x00" * 16)
    message.set("Mac_Second", b"\x00" * 16)
    messages_bytes = message.bytestring
    msg_alpha = messages_bytes[: len(messages_bytes) - 32]
    mac1 = mac(blk2s_hash(LABEL_MAC1, static_keys.preshared_public_key), msg_alpha)
    message.set("Mac_First", mac1)
    message.set("Mac_Second", b"\x00" * 16)  # We don't handle cookies so Mac_Second stays at 0

    return message.bytestring, Connection(
        static_public_key=static_keys.static_public_key,
        static_private_key=static_keys.static_private_key,
        initiator_chain=initiator_chaining_key,
        initiator_hash=initiator_hash,
        ephemeral_public=unencrypted_ephemeral,
        ephemeral_private=initiator_ephemeral_private,
        receiver_public_key=static_keys.preshared_public_key,
        session_id=session_id,
    )


def create_cli() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="RecordFlux Wireguard",
        description="Simplified implementation of the Wireguard protocol using RecordFlux",
        epilog="Keys can be generated using wg(8)",
    )

    parser.add_argument(
        "-p",
        "--port",
        help="Port used by wireguard",
        required=True,
        nargs="?",
        type=int,
    )
    parser.add_argument(
        "--static-private-key",
        help="Curve25519 private key",
        default="python_wg_privatekey",
    )
    parser.add_argument(
        "--static-public-key",
        help="Public key derived from the static-private-key",
        default="python_wg_publickey",
    )
    parser.add_argument(
        "--peer-public-key",
        help="Peer public Curve25519 key",
        default="linux_wg_publickey",
    )

    return parser


def wireguard() -> None:
    running = True
    args = create_cli().parse_args(sys.argv[1:])
    send_socket = WireguardSocket("127.0.0.1", args.port)
    static_keys = load_keys(args)

    def _gracefull_stop(*_: object) -> None:
        Path(WIREGUARD_PID_FILE).unlink(missing_ok=True)
        nonlocal running
        running = False

    signal.signal(signal.SIGTERM, _gracefull_stop)

    handshake_init, conn = create_handshake(static_keys)
    send_socket.send_all(handshake_init)

    while running:
        received_bytes = send_socket.read()
        message = parse_message(received_bytes)
        message_type = message.get("Message_Type")
        assert isinstance(message_type, str)
        rp = PACKET_HANDLERS[message_type](conn, message)

        if rp is not None:
            send_socket.send_all(rp)


if __name__ == "__main__":
    wireguard()
