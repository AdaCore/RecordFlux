from typing import Sequence

from rflx.common import Base, verbose_repr
from rflx.error import RecordFluxError, Severity, Subsystem

from . import const, message, session, type_


class Model(Base):
    def __init__(
        self, types: Sequence[type_.Type], sessions: Sequence[session.Session] = None
    ) -> None:
        self.types = types
        self.sessions = sessions or []
        self.__check_types()

    def __repr__(self) -> str:
        return verbose_repr(self, ["types"])

    def __str__(self) -> str:
        return "\n\n".join(
            f"{t};"
            for t in self.types
            if not type_.is_builtin_type(t.name) and not type_.is_internal_type(t.name)
        )

    @property
    def messages(self) -> Sequence[message.Message]:
        return [m for m in self.types if isinstance(m, message.Message)]

    @property
    def refinements(self) -> Sequence[message.Refinement]:
        return [m for m in self.types if isinstance(m, message.Refinement)]

    def __check_types(self) -> None:
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
                error.append(
                    f"conflicting literals: {literals_message}",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    e2.location,
                )
                error.extend(
                    [
                        (
                            f'previous occurrence of "{l}"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            l.location,
                        )
                        for l in sorted(identical_literals)
                    ]
                )

        literals = {
            l: t for t in self.types if isinstance(t, type_.Enumeration) for l in t.literals
        }
        type_set = {t.identifier.name for t in self.types}
        name_conflicts = [n for n in literals.keys() if n in type_set]
        for name in sorted(name_conflicts):
            error.append(
                f'literal conflicts with type "{name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                name.location,
            )
            type_location = [t.location for t in self.types if t.identifier.name == name][0]
            error.append(
                "conflicting type declaration", Subsystem.MODEL, Severity.INFO, type_location,
            )

        error.propagate()
