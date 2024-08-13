from __future__ import annotations

from collections.abc import Generator
from typing import Final, Union

from rflx.rapidflux import ID as ID

# TODO(eng/recordflux/RecordFlux#1424): Replace with PEP604 union
StrID = Union[str, ID]

ID_PREFIX: Final = "T_"


def id_generator() -> Generator[ID, None, None]:
    i = 0
    while True:
        yield ID(f"{ID_PREFIX}{i}")
        i += 1
