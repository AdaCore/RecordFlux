"""
Wireguard crypto functions.

Cryptography functions as described in https://www.wireguard.com/papers/wireguard.pdf
in section 5.4.
"""

from __future__ import annotations

import hashlib
from hmac import HMAC

from cryptography.hazmat.primitives.asymmetric.x25519 import X25519PrivateKey, X25519PublicKey
from cryptography.hazmat.primitives.ciphers.aead import ChaCha20Poly1305
from nacl.public import PrivateKey


def blk2s_hash(*args: bytes) -> bytes:
    h = hashlib.blake2s(digest_size=32)
    for a in args:
        h.update(a)
    return h.digest()


def dh(private_key: bytes, public_key: bytes) -> bytes:
    x25519_private_key = X25519PrivateKey.from_private_bytes(private_key)
    peer_public_key = X25519PublicKey.from_public_bytes(public_key)
    return x25519_private_key.exchange(peer_public_key)


def dh_generate() -> tuple[bytes, bytes]:
    random_key = PrivateKey.generate()
    return bytes(random_key), bytes(random_key.public_key)


def _create_chacha20poly1305_nonce(counter: int) -> bytes:
    # From the wireguard spec: nonce are composed of 32 bits of zeros followed by
    # the 64-bits little-endian value of counter
    return (b"\x00" * 4) + counter.to_bytes(length=8, byteorder="little")


def chacha20poly1305_encrypt(
    key: bytes,
    counter: int,
    plain_text: bytes,
    auth_text: bytes,
) -> bytes:
    chacha = ChaCha20Poly1305(key)
    nonce = _create_chacha20poly1305_nonce(counter)
    return chacha.encrypt(nonce, plain_text, auth_text)


def chacha20poly1305_decrypt(
    key: bytes,
    counter: int,
    plain_text: bytes,
    auth_text: bytes,
) -> bytes:
    chacha = ChaCha20Poly1305(key)
    nonce = _create_chacha20poly1305_nonce(counter)
    return chacha.decrypt(nonce, plain_text, auth_text)


def mac(key: bytes, value: bytes) -> bytes:
    h = hashlib.blake2s(key=key, digest_size=16)
    h.update(value)
    return h.digest()


def hmac(key: bytes, *values: bytes) -> bytes:
    h = HMAC(key, digestmod=hashlib.blake2s)
    h.digest_size = 32

    for value in values:
        h.update(value)

    return h.digest()
