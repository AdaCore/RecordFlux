import hashlib
import json
import pathlib
from typing import Dict

from rflx import __version__
from rflx.model.message import AbstractMessage

CACHE_DIR = pathlib.Path.home() / ".cache" / "RecordFlux"
VERIFICATION_FILE = "verification.json"


class Cache:
    def __init__(self, enabled: bool = True) -> None:
        self._enabled = enabled

        if not enabled:
            return

        self._verification: Dict[str, str] = {}

        self._load_cache()

    def is_verified(self, message: AbstractMessage) -> bool:
        if not self._enabled:
            return False

        return message.full_name in self._verification and self._verification[
            message.full_name
        ] == self._message_hash(message)

    def add_verified(self, message: AbstractMessage) -> None:
        if not self._enabled:
            return

        message_hash = self._message_hash(message)
        if (
            message.full_name not in self._verification
            or message_hash != self._verification[message.full_name]
        ):
            self._verification[message.full_name] = message_hash
            self._write_cache()

    def _load_cache(self) -> None:
        try:
            with open(CACHE_DIR / VERIFICATION_FILE) as f:
                self._verification = json.load(f)
        except (FileNotFoundError, json.JSONDecodeError):
            pass

    def _write_cache(self) -> None:
        CACHE_DIR.mkdir(parents=True, exist_ok=True)
        with open(CACHE_DIR / VERIFICATION_FILE, "w") as f:
            json.dump(self._verification, f)

    @staticmethod
    def _message_hash(message: AbstractMessage) -> str:
        types = "|".join(str(t) for t in message.types.values())
        return hashlib.md5(f"{__version__}|{message}|{types}".encode("utf-8")).hexdigest()
