from __future__ import annotations

import hashlib
import json
from pathlib import Path

from rflx import __version__
from rflx.const import CACHE_PATH
from rflx.error import Subsystem, warn
from rflx.model.message import AbstractMessage

DEFAULT_FILE = CACHE_PATH / "verification.json"


class Cache:
    """
    Cache the successful verification of messages.

    The cache holds a list of hashes of all message variants that were successfully verified. The
    state of the cache is persisted on disk and restored on initialization.
    """

    def __init__(self, file: Path = DEFAULT_FILE, enabled: bool = True) -> None:
        self._file = file
        self._enabled = enabled

        if not enabled:
            return

        self._verified: dict[str, list[str]] = {}

        self._load_cache()

    def is_verified(self, message: AbstractMessage) -> bool:
        if not self._enabled:
            return False

        return (
            message.full_name in self._verified
            and self._message_hash(message) in self._verified[message.full_name]
        )

    def add_verified(self, message: AbstractMessage) -> None:
        if not self._enabled:
            return

        message_hash = self._message_hash(message)
        if message.full_name not in self._verified:
            self._verified[message.full_name] = []
        if message_hash not in self._verified[message.full_name]:
            self._verified[message.full_name].append(message_hash)
            self._write_cache()

    def _load_cache(self) -> None:
        try:
            with self._file.open(encoding="utf-8") as f:
                cache = json.load(f)
                if isinstance(cache, dict) and all(
                    isinstance(i, str)
                    and isinstance(l, list)
                    and all(isinstance(h, str) for h in l)
                    for i, l in cache.items()
                ):
                    self._verified = cache
                else:
                    raise TypeError  # noqa: TRY301
        except (json.JSONDecodeError, TypeError):
            warn("verification cache will be ignored due to invalid format", Subsystem.PARSER)
        except FileNotFoundError:
            pass

    def _write_cache(self) -> None:
        self._file.parent.mkdir(parents=True, exist_ok=True)
        with self._file.open("w", encoding="utf-8") as f:
            json.dump(self._verified, f)

    @staticmethod
    def _message_hash(message: AbstractMessage) -> str:
        types = "|".join(str(t) for t in message.types.values())
        return hashlib.md5(f"{__version__}|{message}|{types}".encode()).hexdigest()  # noqa: S324
