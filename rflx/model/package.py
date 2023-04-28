from __future__ import annotations

import textwrap
from dataclasses import dataclass, field

from rflx import const
from rflx.identifier import ID

from . import top_level_declaration


@dataclass
class Package:
    name: ID
    imports: frozenset[ID] = field(default_factory=frozenset)
    declarations: list[top_level_declaration.TopLevelDeclaration] = field(default_factory=list)

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
    def declarations_str(self) -> str:
        raw = "\n\n".join(f"{t};" for t in self.declarations)
        return textwrap.indent(raw, " " * 3)

    def __str__(self) -> str:
        pieces = [self.imports_str, self.begin_str, self.declarations_str, self.end_str]
        return "\n\n".join(filter(None, pieces))
