from __future__ import annotations

import itertools
from collections.abc import Sequence
from pathlib import Path
from typing import Optional

from rflx import const
from rflx.common import Base, unique, verbose_repr
from rflx.error import RecordFluxError, Severity, Subsystem
from rflx.identifier import ID

from . import message, session, top_level_declaration, type_
from .package import Package


class Model(Base):
    def __init__(
        self, declarations: Optional[Sequence[top_level_declaration.TopLevelDeclaration]] = None
    ) -> None:
        self._declarations = declarations or []

        self._add_missing_types_and_validate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["types", "sessions"])

    def __str__(self) -> str:
        return "\n\n".join(self.create_specifications().values())

    @property
    def declarations(self) -> Sequence[top_level_declaration.TopLevelDeclaration]:
        return self._declarations

    @property
    def types(self) -> list[type_.Type]:
        return [d for d in self._declarations if isinstance(d, type_.Type)]

    @property
    def messages(self) -> list[message.Message]:
        return [d for d in self._declarations if isinstance(d, message.Message)]

    @property
    def refinements(self) -> list[message.Refinement]:
        return [d for d in self._declarations if isinstance(d, message.Refinement)]

    @property
    def sessions(self) -> list[session.Session]:
        return [d for d in self._declarations if isinstance(d, session.Session)]

    @property
    def packages(self) -> dict[ID, list[top_level_declaration.TopLevelDeclaration]]:
        return {p: list(d) for p, d in itertools.groupby(self._declarations, lambda x: x.package)}

    def create_specifications(self) -> dict[ID, str]:
        pkgs: dict[ID, Package] = {}
        for d in self.declarations:
            if isinstance(d, type_.Type):
                if not type_.is_builtin_type(d.name) and not type_.is_internal_type(d.name):
                    pkg_name: ID = d.package
                    pkg = pkgs.setdefault(pkg_name, Package(pkg_name))
                    pkg.imports |= {dep.package for dep in d.direct_dependencies}
                    pkg.declarations.append(d)
            else:
                pkg_name = d.package
                pkgs.setdefault(pkg_name, Package(pkg_name)).declarations.append(d)
        return {id: str(pkg) for id, pkg in pkgs.items()}

    def write_specification_files(self, output_dir: Path) -> None:
        """Write corresponding specification files (one per package) into given directory."""
        for package, specification in self.create_specifications().items():
            header = (
                "-- style: disable = line-length\n\n"
                if any(len(l) > 120 for l in specification.split("\n"))
                else ""
            )
            (output_dir / f"{package.flat.lower()}.rflx").write_text(f"{header}{specification}")

    def _add_missing_types_and_validate(self) -> None:
        error = self._check_duplicates()

        declarations: list[top_level_declaration.TopLevelDeclaration] = []

        for d in self._declarations:
            if isinstance(d, type_.Type):
                for t in d.dependencies:
                    declarations.append(t)
                declarations.append(d)

            if isinstance(d, session.Session):
                for t in d.direct_dependencies.values():
                    declarations.append(t)
                declarations.append(d)

        self._declarations = list(unique(declarations))

        error += self._check_conflicts()
        error.propagate()

    def _check_duplicates(self) -> RecordFluxError:
        error = RecordFluxError()
        types: dict[ID, type_.Type] = {}
        sessions: dict[ID, session.Session] = {}

        for t in self.types:
            if t.identifier in types:
                error.extend(
                    [
                        (
                            f'conflicting refinement of "{t.pdu.identifier}" with'
                            f' "{t.sdu.identifier}"'
                            if isinstance(t, message.Refinement)
                            else f'name conflict for type "{t.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            t.location,
                        ),
                        (
                            "previous occurrence of refinement"
                            if isinstance(t, message.Refinement)
                            else f'previous occurrence of "{t.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            types[t.identifier].location,
                        ),
                    ],
                )
            types[t.identifier] = t

        for s in self.sessions:
            if s.identifier in types or s.identifier in sessions:
                error.extend(
                    [
                        (
                            f'name conflict for session "{s.identifier}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            s.location,
                        ),
                        (
                            f'previous occurrence of "{s.identifier}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            types[s.identifier].location
                            if s.identifier in types
                            else sessions[s.identifier].location,
                        ),
                    ],
                )
            sessions[s.identifier] = s

        return error

    def _check_conflicts(self) -> RecordFluxError:
        error = RecordFluxError()

        for e1, e2 in [
            (e1, e2)
            for i1, e1 in enumerate(self.types)
            for i2, e2 in enumerate(self.types)
            if (
                isinstance(e1, type_.Enumeration)
                and isinstance(e2, type_.Enumeration)
                and i1 < i2
                and (
                    e1.package == e2.package
                    or e1.package == const.BUILTINS_PACKAGE
                    or e2.package == const.BUILTINS_PACKAGE
                )
            )
        ]:
            identical_literals = set(e2.literals) & set(e1.literals)

            if identical_literals:
                literals_message = ", ".join([f"{l}" for l in sorted(identical_literals)])
                error.extend(
                    [
                        (
                            f"conflicting literals: {literals_message}",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            e2.location,
                        ),
                        *[
                            (
                                f'previous occurrence of "{l}"',
                                Subsystem.MODEL,
                                Severity.INFO,
                                l.location,
                            )
                            for l in sorted(identical_literals)
                        ],
                    ],
                )

        literals = [
            ID(t.package * l, location=l.location)
            for t in self.types
            if isinstance(t, type_.Enumeration)
            for l in t.literals
        ]
        name_conflicts = [
            (l, t)
            for l in literals
            for t in self.types
            if (l.parent == t.package or type_.is_builtin_type(t.identifier))
            and l.name == t.identifier.name
        ]
        for literal, conflicting_type in name_conflicts:
            error.extend(
                [
                    (
                        f'literal "{literal.name}" conflicts with type declaration',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        literal.location,
                    ),
                    (
                        f'conflicting type "{conflicting_type.identifier}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        conflicting_type.location,
                    ),
                ],
            )

        return error
