import textwrap
from dataclasses import dataclass, field
from typing import List

from rflx.identifier import ID

from . import session, type_


@dataclass
class Package:
    name: ID
    imports: List[ID] = field(default_factory=list)
    types: List[type_.Type] = field(default_factory=list)
    sessions: List[session.Session] = field(default_factory=list)

    @property
    def imports_str(self) -> str:
        return "\n".join(f"with {i};" for i in self.imports)

    @property
    def begin_str(self) -> str:
        return f"package {self.name} is"

    @property
    def end_str(self) -> str:
        return f"end {self.name};"

    @property
    def types_str(self) -> str:
        raw = "\n\n".join(f"{t};" for t in self.types)
        return textwrap.indent(raw, " " * 3)

    @property
    def sessions_str(self) -> str:
        raw = "\n\n".join(f"{s};" for s in self.sessions)
        return textwrap.indent(raw, " " * 3)

    def __str__(self) -> str:
        return "\n\n".join(
            filter(
                None,
                [self.imports_str, self.begin_str, self.types_str, self.sessions_str, self.end_str],
            )
        )
