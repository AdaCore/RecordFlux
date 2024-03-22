from __future__ import annotations

import dataclasses
import math
import os
from pathlib import Path
from typing import TYPE_CHECKING, Callable

from rflx.model import NeverVerify
from rflx.pyrflx import MessageValue, PyRFLX

if TYPE_CHECKING:
    from .const_values import WIREGUARD_PID_FILE
    from .crypto import blk2s_hash, chacha20poly1305_decrypt, chacha20poly1305_encrypt, dh, hmac
    from .ping import handle_ping
else:
    from const_values import WIREGUARD_PID_FILE
    from crypto import blk2s_hash, chacha20poly1305_decrypt, chacha20poly1305_encrypt, dh, hmac
    from ping import handle_ping

PYRFLX = PyRFLX.from_specs(["specs/wireguard.rflx"], NeverVerify())
WIREGUARD = PYRFLX.package("Wireguard")


@dataclasses.dataclass()
class Connection:
    static_public_key: bytes
    static_private_key: bytes
    session_id: bytes | None = None
    receiver_id: bytes | None = None
    initiator_chain: bytes | None = None
    initiator_hash: bytes | None = None
    ephemeral_public: bytes | None = None
    ephemeral_private: bytes | None = None
    receiver_public_key: bytes | None = None
    receving_key: bytes | None = None
    sending_key: bytes | None = None
    sending_index: int = 0
    packet_counter: int = 0


HandlerType = Callable[[Connection, MessageValue], bytes | None]


def _create_transport_data_message(conn: Connection, clear_data: bytes) -> bytes:
    packet = WIREGUARD.new_message("Handshake")
    packet.set("Message_Type", "Transport_Data_Message")
    packet.set("Reserved", 0)
    assert conn.receiver_id is not None
    packet.set("Receiver", conn.receiver_id)
    conn.packet_counter += 1
    packet.set("Counter", conn.packet_counter.to_bytes(8, "little"))
    zero_padding = b"\x00" * (16 * math.ceil(len(clear_data) / 16) - len(clear_data))
    raw_data = clear_data + zero_padding
    assert conn.sending_key is not None
    packet.set(
        "Packet",
        chacha20poly1305_encrypt(conn.sending_key, conn.packet_counter, raw_data, b""),
    )
    return packet.bytestring


def _announce_readiness() -> None:
    """
    Announce readiness.

    Write the process pid to `WIREGUARD_PID_FILE` to indicate to the outside world that
    the handshake is complete and we're ready to handle data packet.
    """
    Path(WIREGUARD_PID_FILE).write_text(str(os.getpid()))


def handle_handshake_init(_conn: Connection, _packet: MessageValue) -> bytes | None:
    raise NotImplementedError


def handle_handshake_response(conn: Connection, packet: MessageValue) -> bytes | None:
    # We don't support optional preshared key.
    preshared_key = b"\x00" * 32

    uncrypted_ephemeral = packet.get("Ephemeral")
    assert conn.initiator_hash is not None
    assert conn.initiator_chain is not None
    assert conn.ephemeral_private is not None
    assert isinstance(uncrypted_ephemeral, bytes)

    conn.initiator_hash = blk2s_hash(conn.initiator_hash, uncrypted_ephemeral)
    temp = hmac(conn.initiator_chain, uncrypted_ephemeral)
    chaining_key = hmac(temp, b"\x01")
    ephemeral_shared = dh(conn.ephemeral_private, uncrypted_ephemeral)
    temp = hmac(chaining_key, ephemeral_shared)
    chaining_key = hmac(temp, b"\x01")
    temp = hmac(chaining_key, dh(conn.static_private_key, uncrypted_ephemeral))
    chaining_key = hmac(temp, b"\x01")
    temp = hmac(chaining_key, preshared_key)
    chaining_key = hmac(temp, b"\x01")
    temp2 = hmac(temp, chaining_key, b"\x02")
    key = hmac(temp, temp2, b"\x03")
    conn.initiator_hash = blk2s_hash(conn.initiator_hash, temp2)
    empty_field_bytes = packet.get("Empty")
    assert isinstance(empty_field_bytes, bytes)
    chacha20poly1305_decrypt(key, 0, empty_field_bytes, conn.initiator_hash)

    ck_hash = hmac(chaining_key, b"")
    sending_key = hmac(ck_hash, b"\x01")
    receiving_key = hmac(ck_hash, sending_key, b"\x02")

    conn.receving_key = receiving_key
    conn.sending_key = sending_key
    conn.initiator_chain = chaining_key
    sender = packet.get("Sender")
    assert isinstance(sender, bytes)
    conn.receiver_id = sender

    # Tell the world that we're ready
    _announce_readiness()

    # Send a keep-alive packet
    return _create_transport_data_message(conn, b"")


def handle_transport_data_message(conn: Connection, packet: MessageValue) -> bytes | None:
    # We don't check receiver index here. We assume only a single peer.
    counter_bytes = packet.get("Counter")
    assert isinstance(counter_bytes, bytes)
    counter = int.from_bytes(counter_bytes, "little")

    encrypted_data = packet.get("Packet")
    assert isinstance(encrypted_data, bytes)
    assert conn.receving_key is not None
    decrypted_data = chacha20poly1305_decrypt(conn.receving_key, counter, encrypted_data, b"")

    # Keep-alive packet, do nothing
    if decrypted_data == b"":
        return None

    ping_reply = handle_ping(decrypted_data)

    return _create_transport_data_message(conn, ping_reply) if ping_reply is not None else None


def handle_cookie(*_: object) -> bytes | None:
    raise NotImplementedError


PACKET_HANDLERS: dict[str, HandlerType] = {
    "Wireguard::Handshake_Init": handle_handshake_init,
    "Wireguard::Handshake_Response": handle_handshake_response,
    "Wireguard::Cookie": handle_cookie,
    "Wireguard::Transport_Data_Message": handle_transport_data_message,
}
