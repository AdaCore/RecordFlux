from pathlib import Path
from typing import Dict, Sequence

from rflx import const
from rflx.common import Base, unique, verbose_repr
from rflx.error import RecordFluxError, Severity, Subsystem
from rflx.identifier import ID

from . import message, session, type_
from .package import Package


class Model(Base):
    def __init__(
        self, types: Sequence[type_.Type] = None, sessions: Sequence[session.Session] = None
    ) -> None:
        self._types = types or []
        self._sessions = sessions or []

        self._add_missing_types_and_validate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["types", "sessions"])

    def __str__(self) -> str:
        return "\n\n".join(self.create_specifications().values())

    @property
    def types(self) -> Sequence[type_.Type]:
        return self._types

    @property
    def messages(self) -> Sequence[message.Message]:
        return [m for m in self._types if isinstance(m, message.Message)]

    @property
    def refinements(self) -> Sequence[message.Refinement]:
        return [m for m in self._types if isinstance(m, message.Refinement)]

    @property
    def sessions(self) -> Sequence[session.Session]:
        return self._sessions

    def create_specifications(self) -> Dict[ID, str]:
        pkgs: Dict[ID, Package] = {}
        for ty in self._types:
            if not type_.is_builtin_type(ty.name) and not type_.is_internal_type(ty.name):
                pkg_name: ID = ty.package
                pkg = pkgs.setdefault(pkg_name, Package(pkg_name))
                pkg.imports |= {dep.package for dep in ty.direct_dependencies}
                pkg.types.append(ty)
        for sess in self._sessions:
            pkg_name = sess.package
            pkgs.setdefault(pkg_name, Package(pkg_name)).sessions.append(sess)
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

        types = [t for s in self._sessions for t in s.direct_dependencies.values()] + [*self._types]
        types = [d for t in types for d in t.dependencies]
        self._types = list(unique(types))

        error += self._check_conflicts()
        error.propagate()

    def _check_duplicates(self) -> RecordFluxError:
        error = RecordFluxError()
        types: Dict[ID, type_.Type] = {}
        sessions: Dict[ID, session.Session] = {}

        for t in self._types:
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

        for s in self._sessions:
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
            for i1, e1 in enumerate(self._types)
            for i2, e2 in enumerate(self._types)
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
            for t in self._types
            if isinstance(t, type_.Enumeration)
            for l in t.literals
        ]
        name_conflicts = [
            (l, t)
            for l in literals
            for t in self._types
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
