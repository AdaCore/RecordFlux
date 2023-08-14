from __future__ import annotations

import itertools
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

from rflx import const
from rflx.common import Base, unique, verbose_repr
from rflx.error import RecordFluxError, Severity, Subsystem
from rflx.identifier import ID

from . import message, session, top_level_declaration, type_
from .cache import Cache
from .package import Package


@dataclass
class UncheckedModel(Base):
    declarations: Sequence[top_level_declaration.UncheckedTopLevelDeclaration]
    error: RecordFluxError

    def checked(
        self,
        cache: Cache,
        skip_verification: bool = False,
        workers: int = 1,
    ) -> Model:
        error = RecordFluxError(self.error)
        declarations: list[top_level_declaration.TopLevelDeclaration] = []

        for d in self.declarations:
            try:
                checked_declaration = d.checked(declarations)
                if isinstance(checked_declaration, message.UnprovenMessage):
                    try:
                        proven_message = checked_declaration.proven(
                            skip_verification or cache.is_verified(checked_declaration),
                            workers,
                        )
                        declarations.append(proven_message)
                        cache.add_verified(proven_message)
                    except RecordFluxError as e:
                        error.extend(e)
                elif isinstance(checked_declaration, session.UnprovenSession):
                    try:
                        proven_session = checked_declaration.proven(workers)
                        declarations.append(proven_session)
                    except RecordFluxError as e:
                        error.extend(e)
                else:
                    declarations.append(checked_declaration)
            except RecordFluxError as e:  # noqa: PERF203
                error.extend(e)

        error += _check_duplicates(declarations)
        error += _check_conflicts(declarations)
        error.propagate()

        return Model(declarations)


class Model(Base):
    def __init__(
        self,
        declarations: Optional[Sequence[top_level_declaration.TopLevelDeclaration]] = None,
    ) -> None:
        self._declarations = declarations or []

        error = _check_duplicates(self._declarations)
        self._declarations = self._add_type_dependencies(self._declarations)
        error += _check_conflicts(self._declarations)
        error.propagate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["declarations"])

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
        return {identifier: str(pkg) for identifier, pkg in pkgs.items()}

    def write_specification_files(self, output_dir: Path) -> None:
        """Write corresponding specification files (one per package) into given directory."""
        for package, specification in self.create_specifications().items():
            header = (
                "-- style: disable = line-length\n\n"
                if any(len(l) > 120 for l in specification.split("\n"))
                else ""
            )
            (output_dir / f"{package.flat.lower()}.rflx").write_text(f"{header}{specification}")

    @staticmethod
    def _add_type_dependencies(
        declarations: Sequence[top_level_declaration.TopLevelDeclaration],
    ) -> list[top_level_declaration.TopLevelDeclaration]:
        """Add missing type dependencies to list of declarations."""
        result: list[top_level_declaration.TopLevelDeclaration] = []

        for d in declarations:
            if isinstance(d, type_.Type):
                result.extend(d.dependencies)
                result.append(d)

            if isinstance(d, session.Session):
                result.extend(d.direct_dependencies.values())
                result.append(d)

        return list(unique(result))


def _check_duplicates(
    declarations: Sequence[top_level_declaration.TopLevelDeclaration],
) -> RecordFluxError:
    error = RecordFluxError()
    seen: dict[ID, top_level_declaration.TopLevelDeclaration] = {}

    for d in declarations:
        if d.identifier in seen:
            error.extend(
                [
                    (
                        f'conflicting refinement of "{d.pdu.identifier}" with'
                        f' "{d.sdu.identifier}"'
                        if isinstance(d, message.Refinement)
                        else f'name conflict for type "{d.identifier}"'
                        if isinstance(d, type_.Type)
                        else f'name conflict for session "{d.identifier}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        d.location,
                    ),
                    (
                        "previous occurrence of refinement"
                        if isinstance(d, message.Refinement)
                        else f'previous occurrence of "{d.identifier}"',
                        Subsystem.MODEL,
                        Severity.INFO,
                        seen[d.identifier].location,
                    ),
                ],
            )
        seen[d.identifier] = d

    return error


def _check_conflicts(
    declarations: Sequence[top_level_declaration.TopLevelDeclaration],
) -> RecordFluxError:
    error = RecordFluxError()

    for e1, e2 in [
        (e1, e2)
        for i1, e1 in enumerate(declarations)
        for i2, e2 in enumerate(declarations)
        if (
            isinstance(e1, type_.Enumeration)
            and isinstance(e2, type_.Enumeration)
            and i1 < i2
            and (
                e1.package in (e2.package, const.BUILTINS_PACKAGE)
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
        ID(d.package * l, location=l.location)
        for d in declarations
        if isinstance(d, type_.Enumeration)
        for l in d.literals
    ]
    name_conflicts = [
        (l, d)
        for l in literals
        for d in declarations
        if (l.parent == d.package or type_.is_builtin_type(d.identifier))
        and l.name == d.identifier.name
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
