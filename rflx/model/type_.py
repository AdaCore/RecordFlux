import typing as ty
from abc import abstractmethod
from pathlib import Path

import rflx.typing_ as rty
from rflx import const, expression as expr
from rflx.common import Base, verbose_repr
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID

from . import message


class Type(Base):
    def __init__(
        self, identifier: StrID, location: Location = None, error: RecordFluxError = None
    ) -> None:
        identifier = ID(identifier)
        self.error = error or RecordFluxError()

        if len(identifier.parts) != 2:
            self.error.append(
                f'invalid format of type identifier "{identifier}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )

        self.identifier = identifier
        self.location = location

    def __hash__(self) -> int:
        return hash(self.identifier)

    @property
    def full_name(self) -> str:
        return str(self.identifier)

    @property
    def name(self) -> str:
        return str(self.identifier.name)

    @property
    def package(self) -> ID:
        return self.identifier.parent

    @property
    def type_(self) -> rty.Type:
        return rty.Undefined()

    @property
    def dependencies(self) -> ty.List["Type"]:
        """
        Return a list consisting of the type and all types on which the type depends. The
        dependencies are determined recursively.
        """
        return [self]


class Scalar(Type):
    def __init__(self, identifier: StrID, size: expr.Expr, location: Location = None) -> None:
        super().__init__(identifier, location)
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
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> ty.Sequence[expr.Expr]:
        raise NotImplementedError


class Integer(Scalar):
    @property
    def type_(self) -> rty.Type:
        return rty.Integer(self.full_name, rty.Bounds(self.first.value, self.last.value))

    @property
    def value_count(self) -> expr.Number:
        return self.last - self.first + expr.Number(1)

    @property
    @abstractmethod
    def first(self) -> expr.Number:
        raise NotImplementedError

    @property
    @abstractmethod
    def last(self) -> expr.Number:
        raise NotImplementedError


class ModularInteger(Integer):
    def __init__(self, identifier: StrID, modulus: expr.Expr, location: Location = None) -> None:
        super().__init__(identifier, expr.UNDEFINED, location)

        modulus_num = modulus.simplified()

        if not isinstance(modulus_num, expr.Number):
            self.error.append(
                f'modulus of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return

        modulus_int = int(modulus_num)

        if modulus_int > 2 ** 64:
            self.error.append(
                f'modulus of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                modulus.location,
            )
        if modulus_int == 0 or (modulus_int & (modulus_int - 1)) != 0:
            self.error.append(
                f'modulus of "{self.name}" not power of two',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.__modulus = modulus
        self._size = expr.Number((modulus_int - 1).bit_length())

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "modulus"])

    def __str__(self) -> str:
        return f"type {self.name} is mod {self.modulus}"

    @property
    def modulus(self) -> expr.Expr:
        return self.__modulus

    @property
    def first(self) -> expr.Number:
        return expr.Number(0)

    @property
    def last(self) -> expr.Number:
        modulus = self.modulus.simplified()
        assert isinstance(modulus, expr.Number)
        return modulus - expr.Number(1)

    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> ty.Sequence[expr.Expr]:
        if proof:
            return [
                expr.Less(expr.Variable(name), self.__modulus, location=self.location),
                expr.GreaterEqual(expr.Variable(name), expr.Number(0), location=self.location),
                expr.Equal(expr.Size(name), self.size, location=self.location),
            ]
        return [expr.TRUE]


class RangeInteger(Integer):
    def __init__(
        self,
        identifier: StrID,
        first: expr.Expr,
        last: expr.Expr,
        size: expr.Expr,
        location: Location = None,
    ) -> None:
        super().__init__(identifier, size, location)

        first_num = first.simplified()
        last_num = last.simplified()
        size_num = size.simplified()

        if not isinstance(first_num, expr.Number):
            self.error.append(
                f'first of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if not isinstance(last_num, expr.Number):
            self.error.append(
                f'last of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return
        if int(last_num) >= 2 ** 63:
            self.error.append(
                f'last of "{self.name}" exceeds limit (2**63 - 1)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if not isinstance(size_num, expr.Number):
            self.error.append(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if self.error.check():
            return

        assert isinstance(first_num, expr.Number)
        assert isinstance(last_num, expr.Number)
        assert isinstance(size_num, expr.Number)

        if first_num < expr.Number(0):
            self.error.append(
                f'first of "{self.name}" negative',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        if first_num > last_num:
            self.error.append(
                f'range of "{self.name}" negative',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        if int(last_num).bit_length() > int(size_num):
            self.error.append(
                f'size of "{self.name}" too small',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        if int(size_num) > 64:
            self.error.append(
                f'size of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.__first_expr = first
        self.__first = first_num
        self.__last_expr = last
        self.__last = last_num

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "first_expr", "last_expr", "size_expr"])

    def __str__(self) -> str:
        return (
            f"type {self.name} is range {self.first_expr} .. {self.last_expr}"
            f" with Size => {self.size_expr}"
        )

    @property
    def first(self) -> expr.Number:
        return self.__first

    @property
    def first_expr(self) -> expr.Expr:
        return self.__first_expr

    @property
    def last(self) -> expr.Number:
        return self.__last

    @property
    def last_expr(self) -> expr.Expr:
        return self.__last_expr

    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> ty.Sequence[expr.Expr]:
        if proof:
            return [
                expr.GreaterEqual(expr.Variable(name), self.first, location=self.location),
                expr.LessEqual(expr.Variable(name), self.last, location=self.location),
                expr.Equal(expr.Size(name), self.size, location=self.location),
            ]

        if self.first.simplified() == self.last.simplified():
            return [expr.Equal(expr.Variable(name), self.first)]

        c: expr.Expr = expr.TRUE
        if self.first.simplified() != self.base_first.simplified():
            c = expr.GreaterEqual(expr.Variable(name), self.first)
        if self.last.simplified() != self.base_last.simplified():
            c = expr.And(c, expr.LessEqual(expr.Variable(name), self.last))
        return [c.simplified()]

    @property
    def base_first(self) -> expr.Expr:
        return expr.Number(0)

    @property
    def base_last(self) -> expr.Expr:
        return expr.Sub(expr.Pow(expr.Number(2), self.size), expr.Number(1))


class Enumeration(Scalar):
    def __init__(
        self,
        identifier: StrID,
        literals: ty.Sequence[ty.Tuple[StrID, expr.Number]],
        size: expr.Expr,
        always_valid: bool,
        location: Location = None,
    ) -> None:
        # pylint: disable=too-many-branches, too-many-locals
        super().__init__(identifier, size, location)

        for i1, e1 in enumerate(literals):
            for i2, e2 in enumerate(literals):
                if i2 < i1 and e1[0] == e2[0]:
                    self.error.append(
                        f'duplicate literal "{e1[0]}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        e1[0].location if isinstance(e1[0], ID) else self.location,
                    )
                    self.error.append(
                        "previous occurrence",
                        Subsystem.MODEL,
                        Severity.INFO,
                        e2[0].location if isinstance(e2[0], ID) else self.location,
                    )

        self.literals = {}
        for k, v in literals:
            if " " in str(k) or "." in str(k):
                self.error.append(
                    f'invalid literal name "{k}" in "{self.name}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
                continue
            self.literals[ID(k)] = v

        size_num = size.simplified()

        if not isinstance(size_num, expr.Number):
            self.error.append(
                f'size of "{self.name}" contains variable',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
            return

        if self.literals.values():
            min_literal_value = min(map(int, self.literals.values()))
            max_literal_value = max(map(int, self.literals.values()))
            if min_literal_value < 0 or max_literal_value > 2 ** 63 - 1:
                self.error.append(
                    f'enumeration value of "{self.name}"'
                    " outside of permitted range (0 .. 2**63 - 1)",
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
            if max_literal_value.bit_length() > int(size_num):
                self.error.append(
                    f'size of "{self.name}" too small',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    self.location,
                )
        if int(size_num) > 64:
            self.error.append(
                f'size of "{self.name}" exceeds limit (2**64)',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )
        for i1, v1 in enumerate(self.literals.values()):
            for i2, v2 in enumerate(self.literals.values()):
                if i1 < i2 and v1 == v2:
                    self.error.append(
                        f'duplicate enumeration value "{v1}" in "{self.name}"',
                        Subsystem.MODEL,
                        Severity.ERROR,
                        v2.location,
                    )
                    self.error.append(
                        "previous occurrence", Subsystem.MODEL, Severity.INFO, v1.location
                    )

        if always_valid and len(self.literals) == 2 ** int(size_num):
            self.error.append(
                f'unnecessary always-valid aspect on "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                self.location,
            )

        self.always_valid = always_valid

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "literals", "size_expr", "always_valid"])

    def __str__(self) -> str:
        literals = ", ".join(f"{l} => {v}" for l, v in self.literals.items())
        always_valid = f", Always_Valid => {self.always_valid}" if self.always_valid else ""
        return f"type {self.name} is ({literals}) with Size => {self.size_expr}{always_valid}"

    @property
    def type_(self) -> rty.Type:
        return rty.Enumeration(self.full_name, self.always_valid)

    @property
    def value_count(self) -> expr.Number:
        if self.always_valid:
            size_num = self.size.simplified()
            assert isinstance(size_num, expr.Number)
            return expr.Number(2 ** int(size_num))
        return expr.Number(len(self.literals))

    def constraints(
        self, name: str, proof: bool = False, same_package: bool = True
    ) -> ty.Sequence[expr.Expr]:
        if proof:
            prefixed_literals = {self.package * l: v for l, v in self.literals.items()}
            if self.package == const.BUILTINS_PACKAGE:
                literals = self.literals
            elif same_package:
                literals = {**self.literals, **prefixed_literals}
            else:
                literals = prefixed_literals
            result: ty.List[expr.Expr] = [
                expr.Or(
                    *[
                        expr.Equal(expr.Variable(name), expr.Variable(l), self.location)
                        for l in literals
                    ],
                    location=self.location,
                )
            ]
            result.extend(
                [expr.Equal(expr.Variable(l), v, self.location) for l, v in literals.items()]
            )
            result.append(expr.Equal(expr.Size(name), self.size, self.location))
            return result
        return [expr.TRUE]


class Composite(Type):
    @property
    @abstractmethod
    def element_size(self) -> expr.Expr:
        raise NotImplementedError


class Sequence(Composite):
    def __init__(self, identifier: StrID, element_type: Type, location: Location = None) -> None:
        super().__init__(identifier, location)
        self.element_type = element_type

        if not isinstance(element_type, Scalar) and not (
            isinstance(element_type, message.Message) and element_type.structure
        ):
            self.error.append(
                f'invalid element type of sequence "{self.name}"',
                Subsystem.MODEL,
                Severity.ERROR,
                location,
            )
            self.error.append(
                f'type "{element_type.name}" must be scalar or non-null message',
                Subsystem.MODEL,
                Severity.INFO,
                element_type.location,
            )

        if isinstance(element_type, Scalar):
            element_type_size = element_type.size.simplified()
            if not isinstance(element_type_size, expr.Number) or int(element_type_size) % 8 != 0:
                self.error.append(
                    f'unsupported element type size of sequence "{self.name}"',
                    Subsystem.MODEL,
                    Severity.ERROR,
                    location,
                )
                self.error.append(
                    f'type "{element_type.name}" has size {element_type_size},'
                    r" must be multiple of 8",
                    Subsystem.MODEL,
                    Severity.INFO,
                    element_type.location,
                )

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier", "element_type"])

    def __str__(self) -> str:
        return f"type {self.name} is sequence of {self.element_type.name}"

    @property
    def type_(self) -> rty.Type:
        return rty.Sequence(self.full_name, self.element_type.type_)

    @property
    def element_size(self) -> expr.Expr:
        return expr.Size(self.element_type.name)

    @property
    def dependencies(self) -> ty.List["Type"]:
        return [self, *self.element_type.dependencies]


class Opaque(Composite):
    def __init__(self, location: Location = None) -> None:
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


class Private(Type):
    def __init__(self, identifier: StrID, location: Location = None) -> None:
        super().__init__(identifier, location)

    def __repr__(self) -> str:
        return verbose_repr(self, ["identifier"])

    def __str__(self) -> str:
        return f"type {self.name} is private"

    @property
    def type_(self) -> rty.Type:
        return rty.Private(str(self.identifier))


OPAQUE = Opaque(location=Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0)))

INTERNAL_TYPES = {
    OPAQUE.identifier: OPAQUE,
}

BOOLEAN = Enumeration(
    const.BUILTINS_PACKAGE * "Boolean",
    [
        (ID("False", Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0))), expr.Number(0)),
        (ID("True", Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0))), expr.Number(1)),
    ],
    expr.Number(1),
    always_valid=False,
    location=Location((0, 0), Path(str(const.BUILTINS_PACKAGE)), (0, 0)),
)

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


def qualified_type_identifier(identifier: ID, package: ID = None) -> ID:
    if is_builtin_type(identifier):
        return ID(const.BUILTINS_PACKAGE * identifier.name, location=identifier.location)

    if is_internal_type(identifier):
        return ID(const.INTERNAL_PACKAGE * identifier.name, location=identifier.location)

    if len(identifier.parts) == 1 and package:
        return ID(package * identifier, location=identifier.location)

    return identifier


def enum_literals(types: ty.Iterable[Type], package: ID) -> ty.Dict[ID, Enumeration]:
    literals = {}

    for t in types:
        if isinstance(t, Enumeration):
            for l in t.literals:
                if t.package == const.BUILTINS_PACKAGE or t.package == package:
                    literals[l] = t
                if t.package != const.BUILTINS_PACKAGE:
                    literals[t.package * l] = t

    return literals


def qualified_enum_literals(types: ty.Iterable[Type]) -> ty.Dict[ID, Enumeration]:
    return {
        l if t.package == const.BUILTINS_PACKAGE else t.package * l: t
        for t in types
        if isinstance(t, Enumeration)
        for l in t.literals
    }


def qualified_type_literals(types: ty.Iterable[Type]) -> ty.Dict[ID, Type]:
    return {
        t.identifier.name if t.package == const.BUILTINS_PACKAGE else t.identifier: t for t in types
    }
