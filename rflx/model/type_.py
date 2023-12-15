from __future__ import annotations

import typing as ty
from abc import abstractmethod
from collections import abc
from dataclasses import dataclass
from pathlib import Path
from typing import Optional

import rflx.typing_ as rty
from rflx import const, expression as expr
from rflx.common import indent_next, verbose_repr
from rflx.error import Location, Severity, Subsystem, fail
from rflx.identifier import ID, StrID

from . import message
from .top_level_declaration import TopLevelDeclaration, UncheckedTopLevelDeclaration


class Type(TopLevelDeclaration):
    @property
    def type_(self) -> rty.Type:
        return rty.Undefined()

    @property
    def direct_dependencies(self) -> list[Type]:
        """
        Return a list consisting of the type and all the types on which the type directly depends.

        The dependencies are not determined recursively, i.e. the dependencies
        of dependencies are not considered.
        """
        return [self]

    @property
    def dependencies(self) -> list[Type]:
        """
        Return a list consisting of the type and all types on which the type depends.

        The dependencies are determined recursively in *postorder*.
        """
        return [self]

    @property
    def qualified_identifier(self) -> ID:
        """
        Return the qualified identifier of this type.

        The qualified identifier of a type is defined as its complete package
        path followed by the type name, or just the type name if the type is
        builtin or internal.
        """
        identifier = self.identifier
        return (
            ID(self.name, location=identifier.location)
            if is_builtin_type(identifier) or is_internal_type(identifier)
            else identifier
        )


class Scalar(Type):
    def __init__(
        self,
        identifier: StrID,
        size: expr.Expr,
        location: Optional[Location] = None,
    ) -> None:
        super().__init__(identifier, location)

        self.error.propagate()

        self._size = size

    @property
    def size(self) -> expr.Number:
        size_num = self._size.simplified()
        assert isinstance(size_num, expr.Number)
        return size_num

    @property
    def size_expr(self) -> expr.Expr:
        return self._size

    @property
    @abstractmethod
    def value_count(self) -> expr.Number:
        raise NotImplementedError

    @abstractmethod
    def constraints(
        self,
        name: str,
        proof: bool = False,
        same_package: bool = True,
    ) -> abc.Sequence[expr.Expr]:
        raise NotImplementedError


class Integer(Scalar):
    def __init__(
        self,
        identifier: StrID,
        first: expr.Expr,
        last: expr.Expr,
        size: expr.Expr,
        location: Optional[Location] = None,
    ) -> None:
        super().__init__(identifier, size, location)

        first_num = first.simplified()
        last_num = last.simplified()
        size_num = size.simplified()

        if not isinstance(first_num, expr.Number):
            self.error.extend(
                [
                    (
                        f'first of "{self.name}" contains variable',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        if not isinstance(last_num, expr.Number):
            self.error.fail(
                f'last of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if int(last_num) >= 2**const.MAX_SCALAR_SIZE:
            self.error.extend(
                [
                    (
                        f'last of "{self.name}" exceeds limit (2**{const.MAX_SCALAR_SIZE} - 1)',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        if not isinstance(size_num, expr.Number):
            self.error.extend(
                [
                    (
                        f'size of "{self.name}" contains variable',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        self.error.propagate()

        assert isinstance(first_num, expr.Number)
        assert isinstance(last_num, expr.Number)
        assert isinstance(size_num, expr.Number)

        if first_num < expr.Number(0):
            self.error.extend(
                [
                    (
                        f'first of "{self.name}" negative',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )
        if first_num > last_num:
            self.error.extend(
                [
                    (
                        f'range of "{self.name}" negative',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        if int(last_num).bit_length() > int(size_num):
            self.error.extend(
                [
                    (
                        f'size of "{self.name}" too small',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        # Eng/RecordFlux/RecordFlux#1077
        # size of integers is limited to 63bits

        if int(size_num) > const.MAX_SCALAR_SIZE:
            self.error.extend(
                [
                    (
                        f'size of "{self.name}" exceeds limit (2**{const.MAX_SCALAR_SIZE})',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        self.error.propagate()

        self._first_expr = first
        self._first = first_num
        self._last_expr = last
        self._last = last_num

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "first_expr", "last_expr", "size_expr"])

    def __str__(self) -> str:
        return (
            f"type {self.name} is range {self.first_expr} .. {self.last_expr}"
            f" with Size => {self.size_expr}"
        )

    @property
    def type_(self) -> rty.Type:
        return rty.Integer(
            self.full_name,
            rty.Bounds(self.first.value, self.last.value),
            location=self.location,
        )

    @property
    def value_count(self) -> expr.Number:
        return self.last - self.first + expr.Number(1)

    @property
    def first(self) -> expr.Number:
        return self._first

    @property
    def first_expr(self) -> expr.Expr:
        return self._first_expr

    @property
    def last(self) -> expr.Number:
        return self._last

    @property
    def last_expr(self) -> expr.Expr:
        return self._last_expr

    def constraints(
        self,
        name: str,
        proof: bool = False,  # noqa: ARG002
        same_package: bool = True,  # noqa: ARG002
    ) -> abc.Sequence[expr.Expr]:
        return [
            expr.GreaterEqual(
                expr.Variable(name, type_=self.type_),
                self.first,
                location=self.location,
            ),
            expr.LessEqual(
                expr.Variable(name, type_=self.type_),
                self.last,
                location=self.location,
            ),
            expr.Equal(expr.Size(name), self.size, location=self.location),
        ]


class Enumeration(Scalar):
    def __init__(  # noqa: PLR0912
        self,
        identifier: StrID,
        literals: abc.Sequence[tuple[StrID, expr.Number]],
        size: expr.Expr,
        always_valid: bool,
        location: Optional[Location] = None,
    ) -> None:
        super().__init__(identifier, size, location)

        for i1, e1 in enumerate(literals):
            for i2, e2 in enumerate(literals):
                if i2 < i1 and e1[0] == e2[0]:
                    self.error.extend(
                        [
                            (
                                f'duplicate literal "{e1[0]}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                e1[0].location if isinstance(e1[0], ID) else self.location,
                            ),
                            (
                                "previous occurrence",
                                Subsystem.MODEL,
                                Severity.INFO,
                                e2[0].location if isinstance(e2[0], ID) else self.location,
                            ),
                        ],
                    )

        self.literals = {}
        for k, v in literals:
            if " " in str(k) or "." in str(k):
                self.error.extend(
                    [
                        (
                            f'invalid literal name "{k}" in "{self.name}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        ),
                    ],
                )
                continue
            self.literals[ID(k)] = v

        size_num = size.simplified()

        if not isinstance(size_num, expr.Number):
            self.error.fail(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if self.literals.values():
            min_literal_value = min(map(int, self.literals.values()))
            max_literal_value = max(map(int, self.literals.values()))
            if min_literal_value < 0 or max_literal_value > 2**const.MAX_SCALAR_SIZE - 1:
                self.error.extend(
                    [
                        (
                            f'enumeration value of "{self.name}"'
                            f" outside of permitted range (0 .. 2**{const.MAX_SCALAR_SIZE} - 1)",
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        ),
                    ],
                )
            if max_literal_value.bit_length() > int(size_num):
                self.error.extend(
                    [
                        (
                            f'size of "{self.name}" too small',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            self.location,
                        ),
                    ],
                )

        # Eng/RecordFlux/RecordFlux#1077
        # size of integers is limited to 63bits

        if int(size_num) > const.MAX_SCALAR_SIZE:
            self.error.extend(
                [
                    (
                        f'size of "{self.name}" exceeds limit (2**{const.MAX_SCALAR_SIZE})',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )
        for i1, v1 in enumerate(self.literals.values()):
            for i2, v2 in enumerate(self.literals.values()):
                if i1 < i2 and v1 == v2:
                    self.error.extend(
                        [
                            (
                                f'duplicate enumeration value "{v1}" in "{self.name}"',
                                Subsystem.MODEL,
                                Severity.ERROR,
                                v2.location,
                            ),
                            ("previous occurrence", Subsystem.MODEL, Severity.INFO, v1.location),
                        ],
                    )

        if always_valid and len(self.literals) == 2 ** int(size_num):
            self.error.extend(
                [
                    (
                        f'unnecessary always-valid aspect on "{self.name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        self.location,
                    ),
                ],
            )

        self.error.propagate()

        self.always_valid = always_valid

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "literals", "size_expr", "always_valid"])

    def __str__(self) -> str:
        literals = ", ".join(f"{l} => {v}" for l, v in self.literals.items())
        always_valid = f", Always_Valid => {self.always_valid}" if self.always_valid else ""
        aspects = f"with Size => {self.size_expr}{always_valid}"
        result = f"type {self.name} is ({literals}) {aspects}"

        if len(result) > 100:
            literals = ",\n".join(f"{l} => {v}" for l, v in self.literals.items())
            result = f"type {self.name} is\n   ({indent_next(literals, 4)})\n{aspects}"

        return result

    @property
    def type_(self) -> rty.Type:
        return rty.Enumeration(
            self.full_name,
            list(map(ID, self.literals.keys())),
            self.always_valid,
            location=self.location,
        )

    @property
    def value_count(self) -> expr.Number:
        if self.always_valid:
            size_num = self.size.simplified()
            assert isinstance(size_num, expr.Number)
            return expr.Number(2 ** int(size_num))
        return expr.Number(len(self.literals))

    def constraints(
        self,
        name: str,
        proof: bool = False,
        same_package: bool = True,
    ) -> abc.Sequence[expr.Expr]:
        literals = dict(self.literals)

        if proof and self.always_valid:
            # Add a dummy literal to indicate that there are other valid values,
            # thus preventing a condition from being falsely detected as always true.
            for v in range(0, 2**self.size.value):  # pragma: no branch
                if expr.Number(v) not in literals.values():
                    literals["L_" + self.identifier.name + f"_{v}"] = expr.Number(v)
                    break

        prefixed_literals = {self.package * l: v for l, v in literals.items()}
        if self.package == const.BUILTINS_PACKAGE:
            pass
        elif same_package:
            literals = {**literals, **prefixed_literals}
        else:
            literals = prefixed_literals

        result: list[expr.Expr] = [
            expr.Or(
                *[
                    expr.Equal(
                        expr.Variable(name, type_=self.type_),
                        expr.Literal(l),
                        self.location,
                    )
                    for l in literals
                ],
                location=self.location,
            ),
        ]
        result.extend(
            [
                expr.Equal(expr.Literal(l, type_=self.type_), v, self.location)
                for l, v in literals.items()
            ],
        )
        result.append(expr.Equal(expr.Size(name), self.size, self.location))
        return result


class Composite(Type):
    @property
    @abstractmethod
    def element_size(self) -> expr.Expr:
        raise NotImplementedError


class Sequence(Composite):
    def __init__(
        self,
        identifier: StrID,
        element_type: Type,
        location: Optional[Location] = None,
    ) -> None:
        super().__init__(identifier, location)
        self.element_type = element_type

        if not isinstance(element_type, Scalar) and not isinstance(element_type, message.Message):
            self.error.extend(
                [
                    (
                        f'invalid element type of sequence "{self.name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        location,
                    ),
                    (
                        f'type "{element_type.name}" must be scalar or message',
                        Subsystem.MODEL,
                        Severity.INFO,
                        element_type.location,
                    ),
                ],
            )

        if isinstance(element_type, Scalar):
            element_type_size = element_type.size.simplified()
            if not isinstance(element_type_size, expr.Number) or int(element_type_size) % 8 != 0:
                self.error.extend(
                    [
                        (
                            f'unsupported element type size of sequence "{self.name}"',
                            Subsystem.MODEL,
                            Severity.ERROR,
                            location,
                        ),
                        (
                            f'type "{element_type.name}" has size {element_type_size},'
                            r" must be multiple of 8",
                            Subsystem.MODEL,
                            Severity.INFO,
                            element_type.location,
                        ),
                    ],
                )

        if isinstance(element_type, message.Message):
            error_entry = (
                f'invalid element type of sequence "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

            if element_type.is_null:
                self.error.extend(
                    [
                        error_entry,
                        (
                            "null messages must not be used as sequence element",
                            Subsystem.MODEL,
                            Severity.INFO,
                            element_type.location,
                        ),
                    ],
                )

            if element_type.parameters:
                self.error.extend(
                    [
                        error_entry,
                        (
                            "parameterized messages must not be used as sequence element",
                            Subsystem.MODEL,
                            Severity.INFO,
                            element_type.location,
                        ),
                    ],
                )

            if any(
                bool(e.findall(lambda x: x in [expr.Size("Message"), expr.Last("Message")]))
                for l in element_type.structure
                for e in [l.condition, l.size]
            ):
                self.error.extend(
                    [
                        error_entry,
                        (
                            "messages used as sequence element must not depend"
                            ' on "Message\'Size" or "Message\'Last"',
                            Subsystem.MODEL,
                            Severity.INFO,
                            element_type.location,
                        ),
                    ],
                )

        self.error.propagate()

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "element_type"])

    def __str__(self) -> str:
        return f"type {self.name} is sequence of {self.element_type.qualified_identifier}"

    @property
    def type_(self) -> rty.Type:
        return rty.Sequence(self.full_name, self.element_type.type_)

    @property
    def element_size(self) -> expr.Expr:
        return expr.Size(self.element_type.name)

    @property
    def direct_dependencies(self) -> list[Type]:
        return [self.element_type, self]

    @property
    def dependencies(self) -> list[Type]:
        return [*self.element_type.dependencies, self]


class Opaque(Composite):
    def __init__(self, location: Optional[Location] = None) -> None:
        super().__init__(const.INTERNAL_PACKAGE * "Opaque", location)

    def __repr__(self) -> str:
        return verbose_repr(self, [])

    def __str__(self) -> str:
        return ""

    @property
    def type_(self) -> rty.Type:
        return rty.OPAQUE

    @property
    def element_size(self) -> expr.Expr:
        return expr.Number(8)


@dataclass
class UncheckedType(UncheckedTopLevelDeclaration):
    identifier: ID

    @abstractmethod
    def checked(
        self,
        declarations: ty.Sequence[TopLevelDeclaration],
        skip_verification: bool = False,
        workers: int = 1,
    ) -> Type:
        raise NotImplementedError


@dataclass
class UncheckedInteger(UncheckedType):
    identifier: ID
    first: expr.Expr
    last: expr.Expr
    size: expr.Expr
    location: Optional[Location]

    def checked(
        self,
        _declarations: ty.Sequence[TopLevelDeclaration],
        skip_verification: bool = False,  # noqa: ARG002
        workers: int = 1,  # noqa: ARG002
    ) -> Integer:
        return Integer(self.identifier, self.first, self.last, self.size, self.location)


@dataclass
class UncheckedEnumeration(UncheckedType):
    identifier: ID
    literals: abc.Sequence[tuple[ID, expr.Number]]
    size: expr.Expr
    always_valid: bool
    location: Optional[Location]

    def checked(
        self,
        _declarations: ty.Sequence[TopLevelDeclaration],
        skip_verification: bool = False,  # noqa: ARG002
        workers: int = 1,  # noqa: ARG002
    ) -> Enumeration:
        return Enumeration(
            self.identifier,
            self.literals,
            self.size,
            self.always_valid,
            self.location,
        )


@dataclass
class UncheckedSequence(UncheckedType):
    identifier: ID
    element_identifier: ID
    location: Optional[Location]

    def checked(
        self,
        declarations: ty.Sequence[TopLevelDeclaration],
        skip_verification: bool = False,  # noqa: ARG002
        workers: int = 1,  # noqa: ARG002
    ) -> Sequence:
        element_type = next(
            (
                t
                for t in declarations
                if isinstance(t, Type) and t.identifier == self.element_identifier
            ),
            None,
        )
        if not element_type:
            fail(
                f'undefined element type "{self.element_identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.element_identifier.location,
            )
        return Sequence(self.identifier, element_type, self.location)


@dataclass
class UncheckedOpaque(UncheckedType):
    identifier: ID
    location: Optional[Location]

    def checked(
        self,
        _declarations: ty.Sequence[TopLevelDeclaration],
        skip_verification: bool = False,  # noqa: ARG002
        workers: int = 1,  # noqa: ARG002
    ) -> Opaque:
        return Opaque(location=Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0)))


UNCHECKED_OPAQUE = UncheckedOpaque(
    const.INTERNAL_PACKAGE * "Opaque",
    Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0)),
)
OPAQUE = UNCHECKED_OPAQUE.checked([])

INTERNAL_TYPES = {
    OPAQUE.identifier: OPAQUE,
}

UNCHECKED_BOOLEAN = UncheckedEnumeration(
    const.BUILTINS_PACKAGE * "Boolean",
    [
        (ID("False", Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0))), expr.Number(0)),
        (ID("True", Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0))), expr.Number(1)),
    ],
    expr.Number(1),
    always_valid=False,
    location=rty.BOOLEAN.location,
)
BOOLEAN = UNCHECKED_BOOLEAN.checked([])

BUILTIN_TYPES = {
    BOOLEAN.identifier: BOOLEAN,
}

BUILTIN_LITERALS = {l for t in BUILTIN_TYPES.values() for l in t.literals}


def is_internal_type(identifier: StrID) -> bool:
    return ID(identifier) in INTERNAL_TYPES or any(
        ID(identifier) == ID(t.name) for t in INTERNAL_TYPES.values()
    )


def is_builtin_type(identifier: StrID) -> bool:
    return ID(identifier) in BUILTIN_TYPES or any(
        ID(identifier) == ID(t.name) for t in BUILTIN_TYPES.values()
    )


def internal_type_identifier(identifier: ID, package: Optional[ID] = None) -> ID:
    """
    Return the internal identifier of a type.

    The internal identifier is defined as its complete package path
    (`__BUILTINS__` for builtin types, and `__INTERNAL__` for internal types)
    followed by the type name.
    """
    if is_builtin_type(identifier):
        return ID(const.BUILTINS_PACKAGE * identifier.name, location=identifier.location)

    if is_internal_type(identifier):
        return ID(const.INTERNAL_PACKAGE * identifier.name, location=identifier.location)

    if len(identifier.parts) == 1 and package:
        return ID(package * identifier, location=identifier.location)

    return identifier


def enum_literals(types: abc.Iterable[Type], package: ID) -> dict[ID, Enumeration]:
    literals = {}

    for t in types:
        if isinstance(t, Enumeration):
            for l in t.literals:
                if t.package in [const.BUILTINS_PACKAGE, package]:
                    literals[l] = t
                if t.package != const.BUILTINS_PACKAGE:
                    literals[t.package * l] = t

    return literals


def unqualified_enum_literals(types: abc.Iterable[Type], package: ID) -> dict[ID, Enumeration]:
    return {
        l: t
        for t in types
        if isinstance(t, Enumeration) and t.package == package
        for l in t.literals
    }


def qualified_enum_literals(types: abc.Iterable[Type]) -> dict[ID, Enumeration]:
    return {
        l if t.package == const.BUILTINS_PACKAGE else t.package * l: t
        for t in types
        if isinstance(t, Enumeration)
        for l in t.literals
    }


def qualified_type_names(types: abc.Iterable[Type]) -> dict[ID, Type]:
    return {
        t.identifier.name if t.package == const.BUILTINS_PACKAGE else t.identifier: t for t in types
    }
