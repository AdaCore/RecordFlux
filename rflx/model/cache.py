from __future__ import annotations

import hashlib
import json
import typing as ty
from functools import singledispatch
from pathlib import Path
from typing import Optional

from rflx import __version__
from rflx.const import CACHE_PATH
from rflx.error import Subsystem, warn
from rflx.model.message import Message, Refinement
from rflx.model.top_level_declaration import TopLevelDeclaration

DEFAULT_FILE = CACHE_PATH / "verification.json"


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


class AlwaysVerify(Cache):
    def __init__(self) -> None:
        pass

    def is_verified(self, digest: Digest) -> bool:  # noqa: ARG002
        return False

    def add_verified(self, digest: Digest) -> None:
        pass


class NeverVerify(Cache):
    def __init__(self) -> None:
        warn("model verification skipped", Subsystem.MODEL)

    def is_verified(self, digest: Digest) -> bool:  # noqa: ARG002
        return True

    def add_verified(self, digest: Digest) -> None:
        pass


class Digest:
    def __init__(self, declaration: TopLevelDeclaration) -> None:
        components = _digest_components(declaration)
        self._full_name = declaration.full_name
        self._value = (
            hashlib.blake2b(
                "|".join([__version__, *components]).encode(),
            ).hexdigest()
            if components
            else None
        )

    @property
    def key(self) -> str:
        return self._full_name

    @property
    def value(self) -> Optional[str]:
        return self._value


@singledispatch
def _digest_components(_: TopLevelDeclaration) -> ty.List[str]:  # noqa: UP006
    # TODO(eng/recordflux/RecordFlux#1359): Replace typing.List by list
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
