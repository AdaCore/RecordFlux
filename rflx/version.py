from __future__ import annotations

import re
from importlib import metadata
from typing import Optional

from rflx import __version__
from rflx.error import fatal_fail


def version() -> str:
    dependencies = [
        f"{r.name} {metadata.version(r.name)}"
        for r in (Requirement(r) for r in metadata.requires("RecordFlux") or [])
        if r.extra != "devel"
    ]
    return "\n".join(
        [
            f"RecordFlux {__version__}",
            *dependencies,
        ],
    )


class Requirement:
    def __init__(self, string: str) -> None:
        self.name: str
        self.extra: Optional[str]

        match = re.match(r'([^<=> (]{1,})[^;]*(?: *; extra == [\'"](.*)[\'"])?', string)

        if match:
            groups = match.groups()
            assert len(groups) == 2
            assert isinstance(groups[0], str)
            self.name = groups[0]
            self.extra = groups[1]
        else:
            fatal_fail(f'failed parsing requirement "{string}"')
