import textwrap
from dataclasses import dataclass, field
from typing import List, Set

from rflx import const
from rflx.identifier import ID

from . import session, type_


@dataclass
class Package:
    name: ID
    imports: Set[ID] = field(default_factory=set)
    types: List[type_.Type] = field(default_factory=list)
    sessions: List[session.Session] = field(default_factory=list)

    @property
    def imports_str(self) -> str:
        explicit_imports = self.imports - {
            self.name,
            const.BUILTINS_PACKAGE,
            const.INTERNAL_PACKAGE,
        }
        return "\n".join(f"with {i};" for i in sorted(explicit_imports))

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
        pieces = [self.imports_str, self.begin_str, self.types_str, self.sessions_str, self.end_str]
        return "\n\n".join(filter(None, pieces))
