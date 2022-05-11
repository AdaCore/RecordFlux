from __future__ import annotations

import hashlib
import json
import pathlib

from rflx import __version__
from rflx.error import Subsystem, warn
from rflx.model.message import AbstractMessage

CACHE_DIR = pathlib.Path.home() / ".cache" / "RecordFlux"
VERIFICATION_FILE = "verification.json"


class Cache:
    def __init__(self, enabled: bool = True) -> None:
        self._enabled = enabled

        if not enabled:
            return

        self._verification: dict[str, list[str]] = {}

        self._load_cache()

    def is_verified(self, message: AbstractMessage) -> bool:
        if not self._enabled:
            return False

        return (
            message.full_name in self._verification
            and self._message_hash(message) in self._verification[message.full_name]
        )

    def add_verified(self, message: AbstractMessage) -> None:
        if not self._enabled:
            return

        message_hash = self._message_hash(message)
        if message.full_name not in self._verification:
            self._verification[message.full_name] = []
        if message_hash not in self._verification[message.full_name]:
            self._verification[message.full_name].append(message_hash)
            self._write_cache()

    def _load_cache(self) -> None:
        try:
            with open(CACHE_DIR / VERIFICATION_FILE, encoding="utf-8") as f:
                cache = json.load(f)
                if isinstance(cache, dict) and all(
                    isinstance(i, str)
                    and isinstance(l, list)
                    and all(isinstance(h, str) for h in l)
                    for i, l in cache.items()
                ):
                    self._verification = cache
                else:
                    raise TypeError
        except (json.JSONDecodeError, TypeError):
            warn("verification cache will be ignored due to invalid format", Subsystem.PARSER)
        except FileNotFoundError:
            pass

    def _write_cache(self) -> None:
        CACHE_DIR.mkdir(parents=True, exist_ok=True)
        with open(CACHE_DIR / VERIFICATION_FILE, "w", encoding="utf-8") as f:
            json.dump(self._verification, f)

    @staticmethod
    def _message_hash(message: AbstractMessage) -> str:
        types = "|".join(str(t) for t in message.types.values())
        return hashlib.md5(f"{__version__}|{message}|{types}".encode("utf-8")).hexdigest()
