from __future__ import annotations

import itertools
from collections.abc import Sequence
from dataclasses import dataclass
from pathlib import Path

from rflx import const
from rflx.common import Base, unique, verbose_repr
from rflx.identifier import ID
from rflx.rapidflux import Annotation, ErrorEntry, RecordFluxError, Severity, logging

from . import message, state_machine, top_level_declaration, type_decl
from .cache import Cache, Digest
from .package import Package
from .type_decl import BUILTIN_TYPES


@dataclass
class UncheckedModel(Base):
    declarations: Sequence[top_level_declaration.UncheckedTopLevelDeclaration]
    style_checks: dict[Path, frozenset[const.StyleCheck]]
    error: RecordFluxError

    def checked(
        self,
        cache: Cache,
        workers: int = 1,
    ) -> Model:
        error = RecordFluxError(self.error.entries)
        declarations: list[top_level_declaration.TopLevelDeclaration] = []

        for d in self.declarations:
            try:
                unverified = d.checked(declarations, skip_verification=True)
                digest = Digest(unverified)
                if cache.is_verified(digest):
                    logging.info(
                        "Skipping verification of {identifier} ({reason})",
                        identifier=d.identifier,
                        reason=cache.is_verified_reason,
                    )
                    checked = unverified
                else:
                    logging.info("Verifying {identifier}", identifier=d.identifier)
                    checked = d.checked(declarations, workers=workers)
                    checked.check_style(error, self.style_checks)
                declarations.append(checked)
                cache.add_verified(digest)
            except RecordFluxError as e:  # noqa: PERF203
                error.extend(e.entries)

        return Model(declarations, error)


class Model(Base):
    def __init__(
        self,
        declarations: Sequence[top_level_declaration.TopLevelDeclaration] | None = None,
        error: RecordFluxError | None = None,
    ) -> None:
        self._declarations = declarations or []

        error = error or RecordFluxError()
        error.extend(_check_duplicates(self._declarations).entries)
        self._declarations = self._add_type_dependencies(self._declarations)
        error.extend(_check_conflicts(self._declarations).entries)
        error.propagate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["declarations"])

    def __str__(self) -> str:
        return "\n\n".join(self.create_specifications().values())

    @property
    def declarations(self) -> Sequence[top_level_declaration.TopLevelDeclaration]:
        return self._declarations

    @property
    def types(self) -> list[type_decl.TypeDecl]:
        return [d for d in self._declarations if isinstance(d, type_decl.TypeDecl)]

    @property
    def messages(self) -> list[message.Message]:
        return [d for d in self._declarations if isinstance(d, message.Message)]

    @property
    def refinements(self) -> list[message.Refinement]:
        return [d for d in self._declarations if isinstance(d, message.Refinement)]

    @property
    def state_machines(self) -> list[state_machine.StateMachine]:
        return [d for d in self._declarations if isinstance(d, state_machine.StateMachine)]

    @property
    def packages(self) -> dict[ID, list[top_level_declaration.TopLevelDeclaration]]:
        return {p: list(d) for p, d in itertools.groupby(self._declarations, lambda x: x.package)}

    def create_specifications(self) -> dict[ID, str]:
        pkgs: dict[ID, Package] = {}
        for d in self.declarations:
            if isinstance(d, type_decl.TypeDecl):
                if not type_decl.is_builtin_type(d.name) and not type_decl.is_internal_type(d.name):
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
            if isinstance(d, type_decl.TypeDecl):
                result.extend(d.dependencies)
                result.append(d)

            if isinstance(d, state_machine.StateMachine):
                result.extend(d.direct_dependencies.values())
                result.append(d)

        return list(unique(result))


def _check_duplicates(
    declarations: Sequence[top_level_declaration.TopLevelDeclaration],
) -> RecordFluxError:
    error = RecordFluxError()
    seen: dict[ID, top_level_declaration.TopLevelDeclaration] = {}

    for d in declarations:
        if d.package in (const.BUILTINS_PACKAGE, const.INTERNAL_PACKAGE):
            continue

        if type_decl.is_builtin_type(d.identifier.name) or type_decl.is_internal_type(
            d.identifier.name,
        ):
            error.push(
                ErrorEntry(
                    f'illegal redefinition of built-in type "{d.identifier.name}"',
                    Severity.ERROR,
                    d.location,
                ),
            )

        elif d.identifier in seen:
            error.push(
                ErrorEntry(
                    (
                        f'conflicting refinement of "{d.pdu.identifier}" with'
                        f' "{d.sdu.identifier}"'
                        if isinstance(d, message.Refinement)
                        else (
                            f'name conflict for type "{d.identifier}"'
                            if isinstance(d, type_decl.TypeDecl)
                            else f'name conflict for state machine "{d.identifier}"'
                        )
                    ),
                    Severity.ERROR,
                    d.location,
                    annotations=(
                        [
                            Annotation(
                                (
                                    "previous occurrence of refinement"
                                    if isinstance(d, message.Refinement)
                                    else f'previous occurrence of "{d.identifier}"'
                                ),
                                Severity.NOTE,
                                seen[d.identifier].location,
                            ),
                        ]
                    ),
                ),
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
            isinstance(e1, type_decl.Enumeration)
            and isinstance(e2, type_decl.Enumeration)
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
            error.push(
                ErrorEntry(
                    f"conflicting literals: {literals_message}",
                    Severity.ERROR,
                    e2.location,
                    annotations=[
                        Annotation(
                            f'previous occurrence of "{link}"',
                            Severity.NOTE,
                            link.location,
                        )
                        for link in sorted(identical_literals)
                    ],
                ),
            )

    literals = [
        ID(d.package * l, location=l.location)
        for d in declarations
        if isinstance(d, type_decl.Enumeration)
        for l in d.literals
    ]
    name_conflicts = {
        *(set(literals) & {d.identifier for d in declarations}),
        *({l.name for l in literals} & {t.name for t in BUILTIN_TYPES}),
    }
    if name_conflicts:
        for literal, conflicting_type in [
            (l, d)
            for l in literals
            for d in declarations
            if {l, l.name} & name_conflicts and l.name == d.identifier.name
        ]:
            error.push(
                ErrorEntry(
                    f'literal "{literal.name}" conflicts with type declaration',
                    Severity.ERROR,
                    literal.location,
                    annotations=(
                        [
                            Annotation(
                                f'conflicting type "{conflicting_type.identifier}"',
                                Severity.NOTE,
                                conflicting_type.location,
                            ),
                        ]
                    ),
                ),
            )

    return error
