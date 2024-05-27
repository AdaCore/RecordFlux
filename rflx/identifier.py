from __future__ import annotations

from collections.abc import Generator
from typing import Union

from rflx.rapidflux import ID as ID

StrID = Union[str, ID]


def id_generator() -> Generator[ID, None, None]:
    i = 0
    while True:
        yield ID(f"T_{i}")
        i += 1
