from __future__ import annotations

import hashlib
import importlib.resources
import json
import os
import time
import typing as ty
from functools import lru_cache, singledispatch
from pathlib import Path
from typing import Literal, TextIO

from rflx.const import CACHE_PATH
from rflx.error import warn
from rflx.model.message import Message, Refinement
from rflx.model.top_level_declaration import TopLevelDeclaration
from rflx.rapidflux import NO_LOCATION, ErrorEntry, RecordFluxError, Severity
from rflx.version import dependencies

DEFAULT_FILE = CACHE_PATH / "verification.json"


class FileLock:
    """ContextManager that handles cache file lock."""

    LOCK_TIMEOUT = 5

    def __init__(
        self,
        cache_file: Path,
        mode: Literal["r", "w"],
        encoding: str = "utf-8",
    ) -> None:
        self._cache_file = cache_file
        self._lock_file = cache_file.with_suffix(".lock")
        self._mode = mode
        self._encoding = encoding

    def __enter__(self) -> TextIO:
        begin_time = time.time()
        while True:
            try:
                self._lock_file.touch(exist_ok=False)
            except FileExistsError:  # noqa: PERF203
                if (time.time() - begin_time) < self.LOCK_TIMEOUT:
                    time.sleep(0.1)
                else:
                    cache_locked_pid = Path.read_text(self._lock_file)
                    raise RecordFluxError(
                        [
                            ErrorEntry(
                                f"failed to acquire cache lock"
                                f" after {FileLock.LOCK_TIMEOUT} seconds",
                                Severity.ERROR,
                                NO_LOCATION,
                            ),
                            ErrorEntry(
                                f"the cache is locked by a process"
                                f' with a PID of "{cache_locked_pid}"',
                                Severity.NOTE,
                                NO_LOCATION,
                            ),
                            ErrorEntry(
                                f"if the process that owns the lock isn't active anymore, deleting "
                                f'"{self._lock_file}" will solve this issue',
                                Severity.HELP,
                                NO_LOCATION,
                            ),
                        ],
                    ) from None
            else:
                # TODO(eng/recordflux/RecordFlux#1424): Remove no cover pragma
                break  # pragma: no cover

        self._lock_file.write_text(str(os.getpid()), encoding="utf-8")

        try:
            return self._cache_file.open(self._mode, encoding=self._encoding)
        except Exception as e:
            self._lock_file.unlink()
            raise e from e

    def __exit__(self, *_: object) -> None:
        self._lock_file.unlink()


class Cache:
    """
    Cache the successful verification of top level declarations.

    The cache holds a list of hashes of all top level declaration variants that were successfully
    verified. The state of the cache is persisted on disk and restored on initialization.
    """

    def __init__(self, file: Path = DEFAULT_FILE) -> None:
        self._file = file

        self._verified: dict[str, list[str]] = {}

        self._load_cache()

    @property
    def is_verified_reason(self) -> str:
        return "cached"

    def is_verified(self, digest: Digest) -> bool:
        return digest.key in self._verified and digest.value in self._verified[digest.key]

    def add_verified(self, digest: Digest) -> None:
        if not digest.value:
            return

        if digest.key not in self._verified:
            self._verified[digest.key] = []
        if digest.value not in self._verified[digest.key]:
            self._verified[digest.key].append(digest.value)
            self._write_cache()

    def _load_cache(self) -> None:
        try:
            with FileLock(self._file, "r") as f:
                cache_content = f.read().strip()
                cache = json.loads(cache_content)
            if isinstance(cache, dict) and all(
                isinstance(i, str) and isinstance(l, list) and all(isinstance(h, str) for h in l)
                for i, l in cache.items()
            ):
                self._verified = cache
            else:
                raise TypeError  # noqa: TRY301
        except (json.JSONDecodeError, TypeError):
            warn(
                f"verification cache will be ignored due to invalid format:\n{cache_content}",
            )
        except FileNotFoundError:
            pass

    def _write_cache(self) -> None:
        self._file.parent.mkdir(parents=True, exist_ok=True)
        (self._file.parent / "CACHEDIR.TAG").write_text(
            "Signature: 8a477f597d28d172789f06886806bc55\n"
            "# This file is a cache directory tag created by rflx.\n"
            "# For information about cache directory tags see https://bford.info/cachedir/",
        )
        with FileLock(self._file, "w") as f:
            json.dump(self._verified, f)


class AlwaysVerify(Cache):
    def __init__(self) -> None:
        pass

    def is_verified(self, digest: Digest) -> bool:  # noqa: ARG002
        return False

    def add_verified(self, digest: Digest) -> None:
        pass


class NeverVerify(Cache):
    def __init__(self) -> None:
        warn("model verification skipped")

    def is_verified(self, digest: Digest) -> bool:  # noqa: ARG002
        return True

    def add_verified(self, digest: Digest) -> None:
        pass

    @property
    def is_verified_reason(self) -> str:
        return "verification disabled"


class Digest:
    def __init__(self, declaration: TopLevelDeclaration) -> None:
        components = _digest_components(declaration)
        self._full_name = declaration.full_name
        self._value = (
            hashlib.blake2b(
                "|".join([fingerprint(), *components]).encode(),
            ).hexdigest()
            if components
            else None
        )

    @property
    def key(self) -> str:
        return self._full_name

    @property
    def value(self) -> str | None:
        return self._value


@lru_cache
def fingerprint() -> str:
    """Return a fingerprint for any code or dependency that could affect the verification result."""
    m = hashlib.blake2b()

    for d in dependencies():
        m.update(d.encode("utf-8"))

    for f in Path(str(importlib.resources.files("rflx"))).rglob("*"):
        if not f.is_file() or any(
            p in str(f)
            for p in [
                "__pycache__",
                "rflx/converter",
                "rflx/generator",
                "rflx/ide",
                "rflx/ls",
                "rflx/pyrflx",
                "rflx/templates",
            ]
        ):
            continue
        m.update(f.read_bytes())

    return m.hexdigest()


@singledispatch
def _digest_components(_: TopLevelDeclaration) -> list[str]:
    return []


@_digest_components.register
def _(message: Message) -> ty.List[str]:  # noqa: UP006
    return [
        str(message),
        *[str(t) for t in message.types.values()],
    ]


@_digest_components.register
def _(refinement: Refinement) -> ty.List[str]:  # noqa: UP006
    pdu_digest = Digest(refinement.pdu)
    sdu_digest = Digest(refinement.sdu)
    assert pdu_digest.value
    assert sdu_digest.value
    return [
        str(refinement.package.name),
        pdu_digest.value,
        refinement.field.name,
        sdu_digest.value,
        str(refinement.condition),
    ]
