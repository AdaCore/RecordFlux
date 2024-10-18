"""Intermediate representation."""

from __future__ import annotations

import re
from abc import abstractmethod
from collections.abc import Generator, Mapping, Sequence
from concurrent.futures import ProcessPoolExecutor
from enum import Enum
from functools import singledispatch
from sys import intern
from typing import TYPE_CHECKING, Protocol, TypeVar

import z3
from attr import define, field, frozen

from rflx import ty
from rflx.common import Base
from rflx.const import MAX_SCALAR_SIZE, MP_CONTEXT
from rflx.error import info
from rflx.identifier import ID, StrID
from rflx.rapidflux import NO_LOCATION, Location

if TYPE_CHECKING:
    from rflx.model import type_decl

INT_MIN: int = 0
INT_MAX: int = 2**MAX_SCALAR_SIZE - 1


class Origin(Protocol):
    def __str__(self) -> str: ...  # pragma: no cover

    @property
    def location(self) -> Location: ...  # pragma: no cover


class ConstructedOrigin(Origin):
    def __init__(self, string_representation: str, location: Location) -> None:
        self._string_representation = string_representation
        self._location = location

    def __str__(self) -> str:
        return self._string_representation

    @property
    def location(self) -> Location:
        return self._location


class ProofResult(Enum):
    SAT = z3.sat
    UNSAT = z3.unsat
    UNKNOWN = z3.unknown


class ProofJob(Base):
    def __init__(
        self,
        facts: Sequence[Stmt],
        results: Mapping[ProofResult, Sequence[int]],
        logic: str = "QF_NIA",
    ):
        self._facts = facts
        self._logic = logic
        self._results = results
        self._result: Sequence[int]

    @property
    def result(self) -> Sequence[int]:
        return self._result

    def check(self) -> ProofJob:
        """Check the specified facts and return the corresponding object depending on the result."""

        solver = z3.SolverFor(self._logic)

        for f in self._facts:
            solver.add(f.to_z3_expr())

        proof_result = ProofResult(solver.check())
        self._result = self._results[proof_result]

        return self


class ProofManager(Base):
    def __init__(self, workers: int) -> None:
        self._jobs: list[ProofJob] = []
        self._workers = workers

    def add(self, jobs: Sequence[ProofJob]) -> None:
        self._jobs.extend(jobs)

    def check(self) -> list[ProofJob]:
        with ProcessPoolExecutor(max_workers=self._workers, mp_context=MP_CONTEXT) as executor:
            result = list(executor.map(ProofManager._check, self._jobs))

        self._jobs.clear()

        return result

    @staticmethod
    def _check(job: ProofJob) -> ProofJob:
        return job.check()


class Cond(Base):
    def __init__(self, goal: BoolExpr, facts: Sequence[Stmt] | None = None) -> None:
        self._goal = goal
        self._facts = facts or []

    @property
    def goal(self) -> BoolExpr:
        return self._goal

    @property
    def facts(self) -> Sequence[Stmt]:
        return self._facts


@define(init=False, eq=False)
class Stmt(Base):
    """Statement in three-address code (TAC) format."""

    origin: Origin | None
    _str: str | None = field(init=False, default=None)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) == str(other)
        return NotImplemented

    def __str__(self) -> str:
        if self._str is not None:
            return self._str

        self._update_str()
        assert self._str is not None
        return self._str  # type: ignore[unreachable]

    @property
    def location(self) -> Location:
        return self.origin.location if self.origin else NO_LOCATION

    @property
    @abstractmethod
    def accessed_vars(self) -> list[ID]:
        raise NotImplementedError

    def substituted(self, mapping: Mapping[ID, ID]) -> Stmt:
        raise NotImplementedError

    @abstractmethod
    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        raise NotImplementedError

    @abstractmethod
    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def precondition_proofs(self, facts: Sequence[Stmt]) -> list[ProofJob]:
        raise NotImplementedError

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError


@define(eq=False)
class VarDecl(Stmt):
    identifier: ID = field(converter=ID)
    type_: ty.NamedType
    expression: ComplexExpr | None = None
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return []

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.BoolRef:
        if isinstance(self.type_, ty.Integer):
            first = z3.Int(str(First(self.type_.identifier, self.type_)))
            last = z3.Int(str(Last(self.type_.identifier, self.type_)))
            return z3.And(
                first == z3.IntVal(self.type_.bounds.lower),
                last == z3.IntVal(self.type_.bounds.upper),
                z3.Int(str(self.identifier)) >= first,
                z3.Int(str(self.identifier)) <= last,
            )
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        initialization = f" = {self.expression}" if self.expression else ""
        self._str = intern(f"Var {self.identifier} : {self.type_.identifier}{initialization}")


@define(eq=False)
class Assign(Stmt):
    target: ID = field(converter=ID)
    expression: Expr
    type_: ty.NamedType
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return self.expression.accessed_vars

    def substituted(self, mapping: Mapping[ID, ID]) -> Assign:
        return Assign(
            mapping.get(self.target, self.target),
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return (
            self.expression.preconditions(variable_id, self.type_)
            if isinstance(self.expression, BinaryIntExpr)
            else self.expression.preconditions(variable_id)
        )

    def to_z3_expr(self) -> z3.BoolRef:
        target: Var
        if isinstance(self.expression, IntExpr):
            target = IntVar(self.target, self.expression.type_)
        elif isinstance(self.expression, BoolExpr):
            target = BoolVar(self.target)
        else:
            return z3.BoolVal(val=True)
        return target.to_z3_expr() == self.expression.to_z3_expr()

    @property
    def target_var(self) -> Var:
        if isinstance(self.expression, IntExpr):
            return IntVar(self.target, self.expression.type_)
        if isinstance(self.expression, BoolExpr):
            return BoolVar(self.target)
        assert isinstance(self.expression, ObjVar)
        return ObjVar(self.target, self.expression.type_)

    def _update_str(self) -> None:
        self._str = intern(f"{self.target} := {self.expression}")


@define(eq=False)
class FieldAssign(Stmt):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    expression: Expr
    type_: ty.Message
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.message, *self.expression.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAssign:
        return FieldAssign(
            mapping.get(self.message, self.message),
            self.field,
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            Cond(FieldValidNext(self.message, self.field, self.type_)),
            Cond(SufficientSpace(self.message, self.field, self.type_)),
            *self.expression.preconditions(variable_id),
        ]

    def to_z3_expr(self) -> z3.BoolRef:
        target: z3.ExprRef
        if isinstance(self.expression, IntExpr):
            target = z3.Int(f"{self.message}.{self.field}")
        elif isinstance(self.expression, BoolExpr):
            target = z3.Bool(f"{self.message}.{self.field}")
        else:
            return z3.BoolVal(val=True)
        return target == self.expression.to_z3_expr()

    def _update_str(self) -> None:
        self._str = intern(f"{self.message}.{self.field} := {self.expression}")


@define(eq=False)
class Append(Stmt):
    sequence: ID = field(converter=ID)
    expression: Expr
    type_: ty.Sequence
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.sequence, *self.expression.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> Append:
        return Append(
            mapping.get(self.sequence, self.sequence),
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            Cond(Valid(self.sequence, self.type_)),
            Cond(HasElement(self.sequence, self.type_)),
            *self.expression.preconditions(variable_id),
        ]

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.sequence}'Append ({self.expression})")


@define(eq=False)
class Extend(Stmt):
    sequence: ID = field(converter=ID)
    expression: Expr
    type_: ty.Sequence
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.sequence, *self.expression.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> Extend:
        return Extend(
            mapping.get(self.sequence, self.sequence),
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [Cond(Valid(self.sequence, self.type_)), *self.expression.preconditions(variable_id)]

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.sequence}'Extend ({self.expression})")


@define(eq=False)
class Reset(Stmt):
    identifier: ID = field(converter=ID)
    parameter_values: Mapping[ID, Expr]
    type_: ty.Any
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [
            self.identifier,
            *[i for p in self.parameter_values.values() for i in p.accessed_vars],
        ]

    def substituted(self, mapping: Mapping[ID, ID]) -> Reset:
        return Reset(
            mapping.get(self.identifier, self.identifier),
            {p: v.substituted(mapping) for p, v in self.parameter_values.items()},
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [c for v in self.parameter_values.values() for c in v.preconditions(variable_id)]

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        parameter_values = " => ".join(f"{i} => {e}" for i, e in self.parameter_values.items())
        if parameter_values:
            parameter_values = f" ({parameter_values})"
        self._str = intern(f"{self.identifier}'Reset{parameter_values}")


@define(eq=False)
class ChannelStmt(Stmt):
    channel: ID = field(converter=ID)
    expression: BasicExpr
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [*self.expression.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> ChannelStmt:
        return self.__class__(
            self.channel,
            self.expression.substituted(mapping),
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.expression.preconditions(variable_id)

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.channel}'{self.__class__.__name__} ({self.expression})")


@define(eq=False)
class Read(ChannelStmt):
    pass


@define(eq=False)
class Write(ChannelStmt):
    pass


@define(eq=False)
class Check(Stmt):
    expression: BoolExpr
    origin: Origin | None = None

    @property
    def location(self) -> Location:
        return self.expression.location

    @property
    def accessed_vars(self) -> list[ID]:
        return self.expression.accessed_vars

    def substituted(self, mapping: Mapping[ID, ID]) -> Check:
        return Check(
            self.expression.substituted(mapping),
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.expression.preconditions(variable_id)

    def to_z3_expr(self) -> z3.BoolRef:
        return self.expression.to_z3_expr()

    def _update_str(self) -> None:
        self._str = intern(f"Check {self.expression}")


Self = TypeVar("Self", bound="Expr")


@define(init=False)
class Expr(Base):
    """Expression in three-address code (TAC) format."""

    origin: Origin | None
    _str: str | None = field(init=False, default=None)

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) == str(other)
        return NotImplemented

    def __str__(self) -> str:
        if self._str is not None:
            return self._str

        self._update_str()
        assert self._str is not None
        return self._str  # type: ignore[unreachable]

    @property
    @abstractmethod
    def type_(self) -> ty.Any:
        raise NotImplementedError

    @property
    def origin_str(self) -> str:
        if self.origin:
            return str(self.origin)
        return str(self)

    @property
    def location(self) -> Location:
        return self.origin.location if self.origin else NO_LOCATION

    @property
    @abstractmethod
    def accessed_vars(self) -> list[ID]:
        raise NotImplementedError

    def substituted(self: Self, mapping: Mapping[ID, ID]) -> Self:
        raise NotImplementedError

    @abstractmethod
    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError


class BasicExpr(Expr):
    pass


class IntExpr(Expr):
    @property
    @abstractmethod
    def type_(self) -> ty.AnyInteger:
        raise NotImplementedError

    @abstractmethod
    def to_z3_expr(self) -> z3.ArithRef:
        raise NotImplementedError


class BoolExpr(Expr):
    @property
    def type_(self) -> ty.Enumeration:
        return ty.BOOLEAN

    @abstractmethod
    def to_z3_expr(self) -> z3.BoolRef:
        raise NotImplementedError


class BasicIntExpr(BasicExpr, IntExpr):
    @property
    @abstractmethod
    def type_(self) -> ty.AnyInteger:
        raise NotImplementedError


class BasicBoolExpr(BasicExpr, BoolExpr):
    pass


@define(eq=False)
class Var(BasicExpr):
    identifier: ID = field(converter=ID)
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.identifier]

    def _update_str(self) -> None:
        self._str = intern(str(self.identifier))


@define(eq=False)
class IntVar(Var, BasicIntExpr):
    identifier: ID = field(converter=ID)
    var_type: ty.AnyInteger
    origin: Origin | None = None

    @property
    def type_(self) -> ty.AnyInteger:
        return self.var_type

    def substituted(self, mapping: Mapping[ID, ID]) -> IntVar:
        if self.identifier in mapping:
            return IntVar(mapping[self.identifier], self.var_type, self.origin)
        return self

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self.identifier))


@define(eq=False)
class BoolVar(Var, BasicBoolExpr):
    identifier: ID = field(converter=ID)
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Enumeration:
        return ty.BOOLEAN

    def substituted(self, mapping: Mapping[ID, ID]) -> BoolVar:
        if self.identifier in mapping:
            return BoolVar(mapping[self.identifier], self.origin)
        return self

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self.identifier))


@define(eq=False)
class ObjVar(Var):
    identifier: ID = field(converter=ID)
    var_type: ty.Any
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Any:
        return self.var_type

    def substituted(self, mapping: Mapping[ID, ID]) -> ObjVar:
        if self.identifier in mapping:
            return ObjVar(mapping[self.identifier], self.var_type, self.origin)
        return self

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.BoolVal(val=True)


@define(eq=False)
class EnumLit(BasicExpr):
    identifier: ID = field(converter=ID)
    enum_type: ty.Enumeration
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Enumeration:
        return self.enum_type

    @property
    def accessed_vars(self) -> list[ID]:
        return []

    def substituted(self, _mapping: Mapping[ID, ID]) -> EnumLit:
        return self

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self.identifier))

    def _update_str(self) -> None:
        self._str = intern(str(self.identifier))


@define(eq=False)
class IntVal(BasicIntExpr):
    value: int
    origin: Origin | None = None

    @property
    def type_(self) -> ty.UniversalInteger:
        return ty.UniversalInteger(ty.Bounds(self.value, self.value))

    @property
    def accessed_vars(self) -> list[ID]:
        return []

    def substituted(self, _mapping: Mapping[ID, ID]) -> IntVal:
        return self

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.IntVal(self.value)

    def _update_str(self) -> None:
        self._str = intern(str(self.value))


@define(eq=False)
class BoolVal(BasicBoolExpr):
    value: bool
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return []

    def substituted(self, _mapping: Mapping[ID, ID]) -> BoolVal:
        return self

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=self.value)

    def _update_str(self) -> None:
        self._str = intern(str(self.value))


@define(eq=False)
class Attr(Expr):
    prefix: ID = field(converter=ID)
    prefix_type: ty.Any
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.prefix]

    def substituted(self, mapping: Mapping[ID, ID]) -> Attr:
        return self.__class__(
            mapping.get(self.prefix, self.prefix),
            self.prefix_type,
            self.origin,
        )

    def _update_str(self) -> None:
        symbol = re.sub(r"([a-z])([A-Z])", r"\1_\2", self.__class__.__name__)
        self._str = intern(f"{self.prefix}'{symbol}")


@define(eq=False)
class IntAttr(Attr, IntExpr):
    prefix: ID = field(converter=ID)
    prefix_type: ty.Any
    origin: Origin | None = None

    def substituted(self, mapping: Mapping[ID, ID]) -> IntAttr:
        return self.__class__(
            mapping.get(self.prefix, self.prefix),
            self.prefix_type,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self))


@define(eq=False)
class Size(IntAttr):
    @property
    def type_(self) -> ty.AnyInteger:
        return (
            ty.BIT_LENGTH
            if isinstance(self.prefix_type, (ty.Composite, ty.Compound))
            else ty.UNIVERSAL_INTEGER
        )


@define(eq=False)
class Length(IntAttr):
    @property
    def type_(self) -> ty.UniversalInteger:
        return ty.UNIVERSAL_INTEGER


@define(eq=False)
class First(IntAttr):
    @property
    def type_(self) -> ty.UniversalInteger:
        return ty.UNIVERSAL_INTEGER


@define(eq=False)
class Last(IntAttr):
    @property
    def type_(self) -> ty.UniversalInteger:
        return ty.UNIVERSAL_INTEGER


@define(eq=False)
class ValidChecksum(Attr, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class Valid(Attr, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class Present(Attr):
    @property
    def type_(self) -> ty.Enumeration:
        return ty.BOOLEAN

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


@define(eq=False)
class HasData(Attr, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class Head(Attr):
    prefix: ID = field(converter=ID)
    prefix_type: ty.Composite
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Any:
        assert isinstance(self.prefix_type.element, ty.Any)
        return self.prefix_type.element

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class Opaque(Attr):
    prefix: ID = field(converter=ID)
    prefix_type: ty.Message | ty.Sequence
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Sequence:
        return ty.OPAQUE

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            Cond(Valid(self.prefix, self.prefix_type)),
        ]

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class FieldAccessAttr(Expr):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: ty.Compound
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.message]

    @property
    def field_type(self) -> ty.Any:
        type_ = self.message_type.field_types[self.field]
        assert isinstance(type_, ty.Any)
        return type_

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAccessAttr:
        return self.__class__(
            mapping.get(self.message, self.message),
            self.field,
            self.message_type,
            self.origin,
        )

    @property
    def _symbol(self) -> str:
        return re.sub(r"([a-z])([A-Z])", r"\1_\2", self.__class__.__name__)

    def _update_str(self) -> None:
        self._str = intern(f"{self.message}.{self.field}'{self._symbol}")


@define(eq=False)
class FieldValidNext(FieldAccessAttr, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))

    @property
    def _symbol(self) -> str:
        return "Valid_Next"


@define(eq=False)
class FieldValid(FieldAccessAttr, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))

    @property
    def _symbol(self) -> str:
        return "Valid"


@define(eq=False)
class FieldPresent(FieldAccessAttr, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))

    @property
    def _symbol(self) -> str:
        return "Present"


@define(eq=False)
class FieldSize(FieldAccessAttr, IntExpr):
    @property
    def type_(self) -> ty.Integer:
        return ty.BIT_LENGTH

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return (
            [
                Cond(FieldValidNext(self.message, self.field, self.message_type)),
            ]
            if isinstance(self.message_type, ty.Message)
            else []
        )

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self))

    @property
    def _symbol(self) -> str:
        return "Size"


@define(eq=False)
class UnaryExpr(Expr):
    expression: BasicExpr
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return self.expression.accessed_vars

    def substituted(self, mapping: Mapping[ID, ID]) -> UnaryExpr:
        return self.__class__(self.expression.substituted(mapping), self.origin)


@define(eq=False)
class UnaryIntExpr(UnaryExpr, IntExpr):
    expression: BasicIntExpr
    origin: Origin | None = None

    @property
    def type_(self) -> ty.AnyInteger:
        return self.expression.type_


@define(eq=False)
class UnaryBoolExpr(UnaryExpr, BoolExpr):
    expression: BasicBoolExpr
    origin: Origin | None = None


@define(eq=False)
class BinaryExpr(Expr):
    left: BasicExpr
    right: BasicExpr
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [*self.left.accessed_vars, *self.right.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> BinaryExpr:
        return self.__class__(
            self.left.substituted(mapping),
            self.right.substituted(mapping),
            self.origin,
        )

    @property
    def origin_str(self) -> str:
        if self.origin is not None:
            return str(self.origin)
        return f"{self.left.origin_str}{self._symbol}{self.right.origin_str}"

    @property
    def location(self) -> Location:
        if self.origin is not None:
            return self.origin.location
        if self.left.origin is not None:
            return self.left.origin.location
        return NO_LOCATION

    def _update_str(self) -> None:
        self._str = intern(f"{self.left}{self._symbol}{self.right}")

    @property
    def _symbol(self) -> str:
        raise NotImplementedError


@define(eq=False)
class BinaryIntExpr(BinaryExpr, IntExpr):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Origin | None = None

    @abstractmethod
    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        target_type: ty.Type | None = None,
    ) -> list[Cond]:
        raise NotImplementedError

    @property
    def type_(self) -> ty.AnyInteger:
        type_ = self.left.type_.common_type(self.right.type_)
        assert isinstance(type_, ty.AnyInteger)
        return type_


@define(eq=False)
class BinaryBoolExpr(BinaryExpr, BoolExpr):
    left: BasicBoolExpr
    right: BasicBoolExpr
    origin: Origin | None = None


@define(eq=False)
class Neg(UnaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return -self.expression.to_z3_expr()

    def _update_str(self) -> None:
        self._str = intern(f"-{self.expression}")


@define(eq=False)
class Add(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() + self.right.to_z3_expr()

    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        target_type: ty.Type | None = None,
    ) -> list[Cond]:
        target_type = target_type or self.type_
        v_id = next(variable_id)
        v_type = ty.BASE_INTEGER
        upper_bound = (
            target_type.bounds.upper
            if isinstance(target_type, ty.AnyInteger) and target_type.bounds is not None
            else INT_MAX
        )
        return [
            *self.left.preconditions(variable_id),
            *self.right.preconditions(variable_id),
            # Left + Right <= Upper_Bound
            Cond(
                LessEqual(
                    self.left,
                    IntVar(
                        v_id,
                        v_type,
                        origin=(
                            ConstructedOrigin(
                                f"{upper_bound} - {self.right.origin}",
                                self.origin.location,
                            )
                            if self.origin and self.right.origin
                            else None
                        ),
                    ),
                ),
                [
                    VarDecl(v_id, v_type, None, origin=self.origin),
                    Assign(
                        v_id,
                        Sub(
                            IntVal(upper_bound),
                            (
                                IntConversion(
                                    ty.BASE_INTEGER,
                                    self.right,
                                )
                                if self.right.type_ != ty.BASE_INTEGER
                                and not isinstance(self.right.type_, ty.UniversalInteger)
                                else self.right
                            ),
                        ),
                        v_type,
                        origin=self.origin,
                    ),
                ],
            ),
        ]

    @property
    def _symbol(self) -> str:
        return " + "


@define(eq=False)
class Sub(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() - self.right.to_z3_expr()

    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        _target_type: ty.Type | None = None,
    ) -> list[Cond]:
        return [
            *self.left.preconditions(variable_id),
            *self.right.preconditions(variable_id),
            # Left >= Right
            Cond(GreaterEqual(self.left, self.right)),
        ]

    @property
    def _symbol(self) -> str:
        return " - "


@define(eq=False)
class Mul(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() * self.right.to_z3_expr()

    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        target_type: ty.Type | None = None,
    ) -> list[Cond]:
        target_type = target_type or self.type_
        v_id = next(variable_id)
        v_type = ty.BASE_INTEGER
        upper_bound = (
            target_type.bounds.upper
            if isinstance(target_type, ty.AnyInteger) and target_type.bounds is not None
            else INT_MAX
        )
        return [
            *self.left.preconditions(variable_id),
            *self.right.preconditions(variable_id),
            # Left * Right <= Upper_Bound
            Cond(
                LessEqual(
                    self.left,
                    IntVar(
                        v_id,
                        v_type,
                        origin=(
                            ConstructedOrigin(
                                f"{upper_bound} / {self.right.origin}",
                                self.origin.location,
                            )
                            if self.origin and self.right.origin
                            else None
                        ),
                    ),
                ),
                [
                    VarDecl(v_id, v_type, None, origin=self.origin),
                    Assign(v_id, Div(IntVal(upper_bound), self.right), v_type, origin=self.origin),
                ],
            ),
        ]

    @property
    def _symbol(self) -> str:
        return " * "


@define(eq=False)
class Div(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() / self.right.to_z3_expr()

    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        _target_type: ty.Type | None = None,
    ) -> list[Cond]:
        return [
            *self.left.preconditions(variable_id),
            *self.right.preconditions(variable_id),
            # Right /= 0
            Cond(NotEqual(self.right, IntVal(0))),
        ]

    @property
    def _symbol(self) -> str:
        return " / "


@define(eq=False)
class Pow(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() ** self.right.to_z3_expr()

    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        target_type: ty.Type | None = None,
    ) -> list[Cond]:
        target_type = target_type or self.type_
        v_id = next(variable_id)
        v_type = ty.BASE_INTEGER
        upper_bound = (
            target_type.bounds.upper
            if isinstance(target_type, ty.AnyInteger) and target_type.bounds is not None
            else INT_MAX
        )
        return [
            *self.left.preconditions(variable_id),
            *self.right.preconditions(variable_id),
            # Left ** Right <= Upper_Bound
            Cond(
                LessEqual(
                    IntVar(v_id, self.type_, origin=self.origin),
                    IntVal(upper_bound),
                ),
                [
                    VarDecl(v_id, v_type, None, origin=self.origin),
                    Assign(v_id, self, v_type, origin=self.origin),
                ],
            ),
        ]

    @property
    def _symbol(self) -> str:
        return " ** "


@define(eq=False)
class Mod(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() % self.right.to_z3_expr()

    def preconditions(
        self,
        variable_id: Generator[ID, None, None],
        _target_type: ty.Type | None = None,
    ) -> list[Cond]:
        return [
            *self.left.preconditions(variable_id),
            *self.right.preconditions(variable_id),
            # Right /= 0
            Cond(NotEqual(self.right, IntVal(0))),
        ]

    @property
    def _symbol(self) -> str:
        return " mod "


@define(eq=False)
class Not(UnaryBoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Not(self.expression.to_z3_expr())

    def _update_str(self) -> None:
        self._str = intern(f"not {self.expression}")


@define(eq=False)
class And(BinaryBoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.And(self.left.to_z3_expr(), self.right.to_z3_expr())

    @property
    def _symbol(self) -> str:
        return " and "


@define(eq=False)
class Or(BinaryBoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Or(self.left.to_z3_expr(), self.right.to_z3_expr())

    @property
    def _symbol(self) -> str:
        return " or "


@define(eq=False)
class Relation(BoolExpr, BinaryExpr):
    left: BasicExpr
    right: BasicExpr
    origin: Origin | None = None


@define(eq=False)
class Less(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Origin | None = None

    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() < self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " < "


@define(eq=False)
class LessEqual(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Origin | None = None

    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() <= self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " <= "


@define(eq=False)
class Equal(Relation):
    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() == self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " = "


@define(eq=False)
class GreaterEqual(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Origin | None = None

    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() >= self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " >= "


@define(eq=False)
class Greater(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Origin | None = None

    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() > self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " > "


@define(eq=False)
class NotEqual(Relation):
    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() != self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " /= "


@define(eq=False)
class Call(Expr):
    identifier: ID = field(converter=ID)
    arguments: Sequence[Expr]
    argument_types: Sequence[ty.Any]
    origin: Origin | None = None
    _preconditions: list[Cond] = field(init=False, factory=list)

    @property
    def accessed_vars(self) -> list[ID]:
        return [i for a in self.arguments for i in a.accessed_vars]

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            *[c for a in self.arguments for c in a.preconditions(variable_id)],
            *self._preconditions,
        ]

    def set_preconditions(self, preconditions: list[Cond]) -> None:
        self._preconditions = preconditions

    def _update_str(self) -> None:
        self._str = intern(
            str(self.identifier)
            + (f" ({', '.join(str(a) for a in self.arguments)})" if self.arguments else ""),
        )


@define(eq=False)
class IntCall(Call, IntExpr):
    identifier: ID = field(converter=ID)
    arguments: Sequence[Expr]
    argument_types: Sequence[ty.Any]
    type_: ty.AnyInteger
    origin: Origin | None = None

    def substituted(self, mapping: Mapping[ID, ID]) -> IntCall:
        return self.__class__(
            self.identifier,
            [a.substituted(mapping) for a in self.arguments],
            self.argument_types,
            self.type_,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ArithRef:
        # TODO(eng/recordflux/RecordFlux#1338): Return value need not to be identical
        # The return value may be different even if the arguments are the same.
        return z3.Int(str(self.identifier))

    def _update_str(self) -> None:
        self._str = intern(
            str(self.identifier)
            + (f" ({', '.join(str(a) for a in self.arguments)})" if self.arguments else ""),
        )


@define(eq=False)
class BoolCall(Call, BoolExpr):
    def substituted(self, mapping: Mapping[ID, ID]) -> BoolCall:
        return self.__class__(
            self.identifier,
            [a.substituted(mapping) for a in self.arguments],
            self.argument_types,
            self.origin,
        )

    def to_z3_expr(self) -> z3.BoolRef:
        # TODO(eng/recordflux/RecordFlux#1338): Return value need not to be identical
        # The return value may be different even if the arguments are the same.
        return z3.Bool(str(self.identifier))


@define(eq=False)
class ObjCall(Call):
    identifier: ID = field(converter=ID)
    arguments: Sequence[Expr]
    argument_types: Sequence[ty.Any]
    type_: ty.Any
    origin: Origin | None = None

    def substituted(self, mapping: Mapping[ID, ID]) -> ObjCall:
        return self.__class__(
            self.identifier,
            [a.substituted(mapping) for a in self.arguments],
            self.argument_types,
            self.type_,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class FieldAccess(Expr):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: ty.Compound
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [self.message]

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAccess:
        return self.__class__(
            mapping.get(self.message, self.message),
            self.field,
            self.message_type,
            self.origin,
        )

    def preconditions(self, _: Generator[ID, None, None]) -> list[Cond]:
        return (
            [Cond(FieldValid(self.message, self.field, self.message_type, self.origin))]
            if isinstance(self.message_type, ty.Message)
            and self.field in self.message_type.field_types
            else []
        )

    def _update_str(self) -> None:
        self._str = intern(f"{self.message}.{self.field}")


@define(eq=False)
class IntFieldAccess(FieldAccess, IntExpr):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: ty.Compound
    origin: Origin | None = None

    @property
    def type_(self) -> ty.AnyInteger:
        type_ = self.message_type.types[self.field]
        assert isinstance(type_, ty.AnyInteger)
        return type_

    def substituted(self, mapping: Mapping[ID, ID]) -> IntFieldAccess:
        return self.__class__(
            mapping.get(self.message, self.message),
            self.field,
            self.message_type,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self))

    def _update_str(self) -> None:
        self._str = intern(f"{self.message}.{self.field}")


@define(eq=False)
class BoolFieldAccess(FieldAccess, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class ObjFieldAccess(FieldAccess):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: ty.Compound
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Any:
        type_ = self.message_type.field_types[self.field]
        assert isinstance(type_, ty.Any)
        return type_

    def substituted(self, mapping: Mapping[ID, ID]) -> ObjFieldAccess:
        return self.__class__(
            mapping.get(self.message, self.message),
            self.field,
            self.message_type,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class IfExpr(Expr):
    condition: BasicBoolExpr
    then_expr: ComplexExpr
    else_expr: ComplexExpr
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [
            *self.condition.accessed_vars,
            *self.then_expr.accessed_vars,
            *self.else_expr.accessed_vars,
        ]

    def substituted(self, mapping: Mapping[ID, ID]) -> IfExpr:
        return self.__class__(
            self.condition.substituted(mapping),
            self.then_expr.substituted(mapping),
            self.else_expr.substituted(mapping),
            self.origin,
        )

    def to_z3_expr(self) -> z3.ExprRef:
        # TODO(eng/recordflux/RecordFlux#1339): Fix handling of complex expressions
        return z3.If(
            self.condition.to_z3_expr(),
            self.then_expr.expr.to_z3_expr(),
            self.else_expr.expr.to_z3_expr(),
        )

    def _update_str(self) -> None:
        self._str = intern(f"(if {self.condition} then {self.then_expr} else {self.else_expr})")


@define(eq=False)
class IntIfExpr(IfExpr, IntExpr):
    condition: BasicBoolExpr
    then_expr: ComplexIntExpr
    else_expr: ComplexIntExpr
    return_type: ty.AnyInteger
    origin: Origin | None = None

    @property
    def type_(self) -> ty.AnyInteger:
        return self.return_type

    def to_z3_expr(self) -> z3.ArithRef:
        result = super().to_z3_expr()
        assert isinstance(result, z3.ArithRef)
        return result


@define(eq=False)
class BoolIfExpr(IfExpr, BoolExpr):
    condition: BasicBoolExpr
    then_expr: ComplexBoolExpr
    else_expr: ComplexBoolExpr
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Enumeration:
        return ty.BOOLEAN

    def to_z3_expr(self) -> z3.BoolRef:
        result = super().to_z3_expr()
        assert isinstance(result, z3.BoolRef)
        return result


@define(eq=False)
class Conversion(Expr):
    target_type: ty.NamedType
    argument: Expr
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Any:
        return self.target_type

    @property
    def accessed_vars(self) -> list[ID]:
        return self.argument.accessed_vars

    def substituted(self, mapping: Mapping[ID, ID]) -> Conversion:
        return self.__class__(
            self.target_type,
            self.argument.substituted(mapping),
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.argument.preconditions(variable_id)

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.target_type.identifier} ({self.argument})")


@define(eq=False)
class IntConversion(Conversion, BasicIntExpr):
    target_type: ty.Integer
    argument: IntExpr
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Integer:
        return self.target_type

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            *self.argument.preconditions(variable_id),
            # TARGET_TYPE'First <= ARGUMENT
            Cond(
                LessEqual(
                    IntConversion(
                        ty.BASE_INTEGER,
                        First(self.target_type.identifier, self.argument.type_),
                    ),
                    IntConversion(
                        ty.BASE_INTEGER,
                        self.argument,
                    ),
                ),
            ),
            # ARGUMENT <= TARGET_TYPE'Last
            Cond(
                LessEqual(
                    IntConversion(
                        ty.BASE_INTEGER,
                        self.argument,
                    ),
                    IntConversion(
                        ty.BASE_INTEGER,
                        Last(self.target_type.identifier, self.argument.type_),
                    ),
                ),
            ),
        ]

    def to_z3_expr(self) -> z3.ArithRef:
        return self.argument.to_z3_expr()


@define(eq=False)
class Comprehension(Expr):
    iterator: ID = field(converter=ID)
    sequence: Var | FieldAccess
    selector: ComplexExpr
    condition: ComplexBoolExpr
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Aggregate:
        return ty.Aggregate(self.selector.expr.type_)

    @property
    def accessed_vars(self) -> list[ID]:
        return [
            *self.sequence.accessed_vars,
            *self.selector.accessed_vars,
            *self.condition.accessed_vars,
        ]

    def substituted(self, mapping: Mapping[ID, ID]) -> Comprehension:
        return self.__class__(
            mapping.get(self.iterator, self.iterator),
            self.sequence.substituted(mapping),
            self.selector.__class__(
                [s.substituted(mapping) for s in self.selector.stmts],
                self.selector.expr.substituted(mapping),
            ),
            self.condition.__class__(
                [s.substituted(mapping) for s in self.condition.stmts],
                self.condition.expr.substituted(mapping),
            ),
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.sequence.preconditions(variable_id)

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern(
            f"[for {self.iterator} in {self.sequence} if {self.condition} => {self.selector}]",
        )


@define(eq=False)
class Find(Expr):
    iterator: ID = field(converter=ID)
    sequence: Var | FieldAccess
    selector: ComplexExpr
    condition: ComplexBoolExpr
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Any:
        return self.selector.expr.type_

    @property
    def accessed_vars(self) -> list[ID]:
        return [
            *self.sequence.accessed_vars,
            *self.selector.accessed_vars,
            *self.condition.accessed_vars,
        ]

    def substituted(self, mapping: Mapping[ID, ID]) -> Find:
        return self.__class__(
            mapping.get(self.iterator, self.iterator),
            self.sequence.substituted(mapping),
            self.selector.__class__(
                [s.substituted(mapping) for s in self.selector.stmts],
                self.selector.expr.substituted(mapping),
            ),
            self.condition.__class__(
                [s.substituted(mapping) for s in self.condition.stmts],
                self.condition.expr.substituted(mapping),
            ),
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.sequence.preconditions(variable_id)

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern(
            f"Find (for {self.iterator} in {self.sequence} if {self.condition} => {self.selector})",
        )


@define(eq=False)
class Agg(Expr):
    elements: Sequence[BasicExpr]
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Aggregate:
        return ty.Aggregate(ty.common_type([e.type_ for e in self.elements]))

    @property
    def accessed_vars(self) -> list[ID]:
        return [i for e in self.elements for i in e.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> Agg:
        return self.__class__(
            [e.substituted(mapping) for e in self.elements],
            origin=self.origin,
        )

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern("[" + ", ".join(map(str, self.elements)) + "]")


def _named_agg_elements_converter(
    elements: Sequence[tuple[StrID | BasicExpr, BasicExpr]],
) -> Sequence[tuple[ID | BasicExpr, BasicExpr]]:
    return [(ID(n) if isinstance(n, str) else n, e) for n, e in elements]


@define(eq=False)
class NamedAgg(Expr):
    """Only used by code generator and therefore provides minimum functionality."""

    elements: Sequence[tuple[ID | BasicExpr, BasicExpr]] = field(
        converter=_named_agg_elements_converter,
    )
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Any:
        raise NotImplementedError

    @property
    def accessed_vars(self) -> list[ID]:
        return [i for (_, e) in self.elements for i in e.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> NamedAgg:
        raise NotImplementedError

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        raise NotImplementedError

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern("[" + ", ".join([f"{k} => {v}" for k, v in self.elements]) + "]")


@define(eq=False)
class Str(Expr):
    string: str
    origin: Origin | None = None

    @property
    def type_(self) -> ty.Sequence:
        return ty.OPAQUE

    @property
    def accessed_vars(self) -> list[ID]:
        return []

    def substituted(self, _mapping: Mapping[ID, ID]) -> Str:
        return self

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern(f'"{self.string}"')


@define(eq=False)
class MsgAgg(Expr):
    identifier: ID = field(converter=ID)
    field_values: Mapping[ID, Expr]
    type_: ty.Message
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [i for v in self.field_values.values() for i in v.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> MsgAgg:
        return self.__class__(
            self.identifier,
            {f: v.substituted(mapping) for f, v in self.field_values.items()},
            self.type_,
            origin=self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [c for v in self.field_values.values() for c in v.preconditions(variable_id)]

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        field_values = (
            " => ".join(f"{i} => {e}" for i, e in self.field_values.items())
            if self.field_values
            else "null message"
        )
        self._str = intern(f"{self.identifier}'({field_values})")


@define(eq=False)
class DeltaMsgAgg(Expr):
    identifier: ID = field(converter=ID)
    field_values: Mapping[ID, Expr]
    type_: ty.Message
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [i for v in self.field_values.values() for i in v.accessed_vars]

    def substituted(self, mapping: Mapping[ID, ID]) -> DeltaMsgAgg:
        return self.__class__(
            self.identifier,
            {f: v.substituted(mapping) for f, v in self.field_values.items()},
            self.type_,
            origin=self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [c for v in self.field_values.values() for c in v.preconditions(variable_id)]

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        field_values = (
            " => ".join(f"{i} => {e}" for i, e in self.field_values.items())
            if self.field_values
            else "null message"
        )
        self._str = intern(f"{self.identifier} with delta {field_values}")


@define(eq=False)
class CaseExpr(Expr):
    expression: BasicExpr
    choices: Sequence[tuple[Sequence[BasicExpr], BasicExpr]]
    type_: ty.Any
    origin: Origin | None = None

    @property
    def accessed_vars(self) -> list[ID]:
        return [
            *self.expression.accessed_vars,
            *[i for (_, e) in self.choices for i in e.accessed_vars],
        ]

    def substituted(self, mapping: Mapping[ID, ID]) -> CaseExpr:
        return self.__class__(
            self.expression.substituted(mapping),
            [
                (
                    [v.substituted(mapping) for v in vs],
                    e.substituted(mapping),
                )
                for vs, e in self.choices
            ],
            self.type_,
            self.origin,
        )

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        data = ",\n".join(f"      when {' | '.join(map(str, c))} => {e}" for c, e in self.choices)
        self._str = intern(f"(case {self.expression} is\n{data})")


@define(eq=False)
class SufficientSpace(FieldAccessAttr, BoolExpr):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: ty.Message
    origin: Origin | None = None

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class HasElement(Attr, BoolExpr):
    prefix: ID = field(converter=ID)
    prefix_type: ty.Sequence
    origin: Origin | None = None

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@frozen
class Decl:
    identifier: ID = field(converter=ID)
    location: Location


@frozen
class FormalDecl(Decl):
    pass


@frozen
class Argument:
    identifier: ID = field(converter=ID)
    type_identifier: ID = field(converter=ID)
    type_: ty.Type


@frozen
class FuncDecl(FormalDecl):
    identifier: ID = field(converter=ID)
    arguments: Sequence[Argument]
    return_type: ID = field(converter=ID)
    type_: ty.Type
    location: Location


@frozen
class ChannelDecl(FormalDecl):
    identifier: ID = field(converter=ID)
    readable: bool
    writable: bool
    location: Location


@frozen
class ComplexExpr(Base):
    stmts: list[Stmt]
    expr: Expr

    def __str__(self) -> str:
        statements = "".join([f"{s}; " for s in self.stmts])
        return f"{{{statements}{self.expr}}}"

    def is_expr(self) -> bool:
        return len(self.stmts) == 0

    def is_basic_expr(self) -> bool:
        return self.is_expr() and isinstance(self.expr, BasicExpr)

    @property
    def accessed_vars(self) -> list[ID]:
        return [
            *[i for s in self.stmts for i in s.accessed_vars],
            *self.expr.accessed_vars,
        ]

    def substituted(self, mapping: Mapping[ID, ID]) -> ComplexExpr:
        return self.__class__(
            [s.substituted(mapping) for s in self.stmts],
            self.expr.substituted(mapping),
        )


@frozen
class ComplexIntExpr(ComplexExpr):
    stmts: list[Stmt]
    expr: IntExpr


@frozen
class ComplexBoolExpr(ComplexExpr):
    stmts: list[Stmt]
    expr: BoolExpr


@frozen
class Transition:
    target: ID = field(converter=ID)
    condition: ComplexExpr
    description: str | None
    location: Location


@frozen
class State:
    identifier: ID = field(converter=ID)
    transitions: Sequence[Transition]
    exception_transition: Transition | None
    actions: Sequence[Stmt]
    description: str | None
    location: Location

    @property
    def declarations(self) -> list[VarDecl]:
        return [a for a in self.actions if isinstance(a, VarDecl)]


FINAL_STATE = State("Final", [], None, [], None, NO_LOCATION)


@frozen(init=False)
class StateMachine:
    identifier: ID = field(converter=ID)
    states: Sequence[State]
    declarations: Sequence[VarDecl]
    parameters: Sequence[FormalDecl]
    types: Mapping[ID, type_decl.TypeDecl]
    location: Location

    def __init__(  # noqa: PLR0913
        self,
        identifier: ID,
        states: Sequence[State],
        declarations: Sequence[VarDecl],
        parameters: Sequence[FormalDecl],
        types: Mapping[ID, type_decl.TypeDecl],
        location: Location,
        variable_id: Generator[ID, None, None],
    ) -> None:
        states = [
            State(
                s.identifier,
                [
                    Transition(
                        t.target,
                        ComplexExpr(
                            add_required_checks(
                                [
                                    *t.condition.stmts,
                                    # Add a dummy assignment to ensure that the
                                    # transition condition is considered when
                                    # adding required checks.
                                    Assign(
                                        "RFLX_Transition_Condition",
                                        t.condition.expr,
                                        ty.BOOLEAN,
                                    ),
                                ],
                                variable_id,
                            )[:-1],
                            t.condition.expr,
                        ),
                        t.description,
                        t.location,
                    )
                    for t in s.transitions
                ],
                s.exception_transition,
                add_required_checks(s.actions, variable_id),
                s.description,
                s.location,
            )
            for s in states
        ]
        self.__attrs_init__(identifier, states, declarations, parameters, types, location)

    @property
    def package(self) -> ID:
        return self.identifier.parent

    @property
    def initial_state(self) -> State:
        return self.states[0]

    @identifier.validator
    def _check_identifier(self, attribute: str, value: ID) -> None:
        assert len(value.parts) == 2, attribute


def add_conversions(statements: Sequence[Stmt]) -> list[Stmt]:
    expression: Expr
    result: list[Stmt] = []

    for statement in statements:
        if isinstance(statement, Assign):
            expression = _convert_expression(statement.expression, statement.type_)
            result.append(
                Assign(
                    statement.target,
                    expression,
                    statement.type_,
                    statement.origin,
                ),
            )
            continue

        if isinstance(statement, FieldAssign):
            field_type = statement.type_.field_types[statement.field]
            expression = _convert_expression(statement.expression, field_type)
            result.append(
                FieldAssign(
                    statement.message,
                    statement.field,
                    expression,
                    statement.type_,
                    statement.origin,
                ),
            )
            continue

        if isinstance(statement, Append):
            expression = _convert_expression(statement.expression, statement.type_)
            result.append(
                Append(
                    statement.sequence,
                    expression,
                    statement.type_,
                    statement.origin,
                ),
            )
            continue

        result.append(statement)

    return result


@singledispatch
def _convert_expression(expression: Expr, target_type: ty.Type) -> Expr:
    if target_type.is_compatible_strong(expression.type_) or not isinstance(
        target_type,
        (ty.Integer, ty.Enumeration),
    ):
        return expression

    assert False


@_convert_expression.register(BinaryIntExpr)
def _(expression: BinaryIntExpr, target_type: ty.Type) -> Expr:
    assert isinstance(target_type, ty.Integer)

    return expression.__class__(
        (
            expression.left
            if target_type.is_compatible_strong(expression.left.type_)
            else IntConversion(
                target_type,
                expression.left,
                expression.left.origin,
            )
        ),
        (
            expression.right
            if target_type.is_compatible_strong(expression.right.type_)
            else IntConversion(
                target_type,
                expression.right,
                expression.right.origin,
            )
        ),
        origin=expression.origin,
    )


@_convert_expression.register(IntExpr)
def _(expression: IntExpr, target_type: ty.Type) -> Expr:
    if target_type.is_compatible_strong(expression.type_) or not isinstance(
        target_type,
        ty.Integer,
    ):
        return expression

    return IntConversion(
        target_type,
        expression,
        expression.origin,
    )


@_convert_expression.register(CaseExpr)
def _(expression: CaseExpr, target_type: ty.Type) -> Expr:
    if target_type.is_compatible_strong(expression.type_) or not isinstance(
        target_type,
        ty.Integer,
    ):
        return expression

    choices = []

    for k, v in expression.choices:
        assert isinstance(v, IntExpr)
        choices.append(
            (
                k,
                (
                    v
                    if target_type.is_compatible_strong(v.type_)
                    else IntConversion(target_type, v, v.origin)
                ),
            ),
        )

    return expression.__class__(
        expression.expression,
        choices,
        target_type,
        origin=expression.origin,
    )


@_convert_expression.register(MsgAgg)
@_convert_expression.register(DeltaMsgAgg)
def _(
    expression: MsgAgg | DeltaMsgAgg,
    target_type: ty.Type,  # noqa: ARG001
) -> Expr:
    field_values: dict[ID, Expr] = {
        f: _convert_expression(v, expression.type_.types[f])
        for f, v in expression.field_values.items()
    }

    return expression.__class__(
        expression.identifier,
        field_values,
        expression.type_,
        expression.origin,
    )


def add_checks(statements: Sequence[Stmt], variable_id: Generator[ID, None, None]) -> list[Stmt]:
    """Add check statements in places where preconditions must be ensured."""

    result = []

    for statement in statements:
        preconditions = statement.preconditions(variable_id)

        for precondition in preconditions:
            result.extend([*precondition.facts, Check(precondition.goal)])

        if isinstance(statement, Assign) and isinstance(
            statement.expression,
            (Comprehension, Find),
        ):
            statement.expression = statement.expression.__class__(
                statement.expression.iterator,
                statement.expression.sequence,
                statement.expression.selector.__class__(
                    [
                        *[
                            x
                            for s in statement.expression.selector.stmts
                            for p in s.preconditions(variable_id)
                            for x in [*p.facts, Check(p.goal)]
                        ],
                        *statement.expression.selector.stmts,
                        *[
                            x
                            for p in statement.expression.selector.expr.preconditions(variable_id)
                            for x in [*p.facts, Check(p.goal)]
                        ],
                    ],
                    statement.expression.selector.expr,
                ),
                statement.expression.condition.__class__(
                    [
                        *[
                            x
                            for s in statement.expression.condition.stmts
                            for p in s.preconditions(variable_id)
                            for x in [*p.facts, Check(p.goal)]
                        ],
                        *statement.expression.condition.stmts,
                        *[
                            x
                            for p in statement.expression.condition.expr.preconditions(variable_id)
                            for x in [*p.facts, Check(p.goal)]
                        ],
                    ],
                    statement.expression.condition.expr,
                ),
                statement.expression.origin,
            )

        result.append(statement)

    return result


def add_required_checks(
    statements: Sequence[Stmt],
    variable_id: Generator[ID, None, None],
) -> list[Stmt]:
    """
    Add check statements in places where preconditions are not always true.

    For each statement it is checked, if its preconditions are always true. If this is not the
    case, a check statement is added in front of the respective statement. The check statements
    in the resulting list mark the places where the code generator must insert explicit checks.
    """

    # TODO(eng/recordflux/RecordFlux#1764): Fix removal of unnecessary checks in IR
    result = add_checks(add_conversions(statements), variable_id)

    for s in result:
        if isinstance(s, Check):
            info(
                f'precondition "{s.expression}" must be checked at runtime',
                s.expression.location,
            )

    return result


def to_integer(type_: ty.AnyInteger) -> ty.Integer:
    return type_ if isinstance(type_, ty.Integer) else ty.BASE_INTEGER
