"""Intermediate representation."""

from __future__ import annotations

import re
from abc import abstractmethod
from collections.abc import Generator, Mapping, Sequence
from concurrent.futures import ProcessPoolExecutor
from enum import Enum
from sys import intern
from typing import TYPE_CHECKING, Optional, Protocol, TypeVar, Union

import z3
from attr import define, field, frozen

from rflx import typing_ as rty
from rflx.common import Base
from rflx.const import MAX_SCALAR_SIZE, MP_CONTEXT
from rflx.error import Location, Subsystem, info
from rflx.identifier import ID, StrID

if TYPE_CHECKING:
    from rflx.model import type_ as mty

INT_MIN: int = 0
INT_MAX: int = 2**MAX_SCALAR_SIZE - 1


class Origin(Protocol):
    def __str__(self) -> str:
        ...  # pragma: no cover

    @property
    def location(self) -> Optional[Location]:
        ...  # pragma: no cover


class ConstructedOrigin(Origin):
    def __init__(self, string_representation: str, location: Optional[Location]) -> None:
        self._string_representation = string_representation
        self._location = location

    def __str__(self) -> str:
        return self._string_representation

    @property
    def location(self) -> Optional[Location]:
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
            result = list(executor.map(ProofManager._check, self._jobs))  # noqa: SLF001

        self._jobs.clear()

        return result

    @staticmethod
    def _check(job: ProofJob) -> ProofJob:
        return job.check()


class Cond(Base):
    def __init__(self, goal: BoolExpr, facts: Optional[Sequence[Stmt]] = None) -> None:
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

    origin: Optional[Origin]
    _str: Optional[str] = field(init=False, default=None)

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
    def location(self) -> Optional[Location]:
        return self.origin.location if self.origin else None

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
    type_: rty.NamedType
    expression: Optional[ComplexExpr] = None
    origin: Optional[Origin] = None

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.BoolRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        initialization = f" = {self.expression}" if self.expression else ""
        self._str = intern(f"Var {self.identifier} : {self.type_.identifier}{initialization}")


@define(eq=False)
class Assign(Stmt):
    target: ID = field(converter=ID)
    expression: Expr
    type_: rty.NamedType
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> Assign:
        return Assign(
            mapping[self.target] if self.target in mapping else self.target,
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.expression.preconditions(variable_id)

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
    type_: rty.Message
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAssign:
        return FieldAssign(
            mapping[self.message] if self.message in mapping else self.message,
            self.field,
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.expression.preconditions(variable_id)

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
    type_: rty.Sequence
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> Append:
        return Append(
            mapping[self.sequence] if self.sequence in mapping else self.sequence,
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.expression.preconditions(variable_id)

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.sequence}'Append ({self.expression})")


@define(eq=False)
class Extend(Stmt):
    sequence: ID = field(converter=ID)
    expression: Expr
    type_: rty.Sequence
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> Extend:
        return Extend(
            mapping[self.sequence] if self.sequence in mapping else self.sequence,
            self.expression.substituted(mapping),
            self.type_,
            self.origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self.expression.preconditions(variable_id)

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.sequence}'Extend ({self.expression})")


@define(eq=False)
class Reset(Stmt):
    identifier: ID = field(converter=ID)
    parameter_values: Mapping[ID, Expr]
    type_: rty.Any
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> Reset:
        return Reset(
            mapping[self.identifier] if self.identifier in mapping else self.identifier,
            {p: v.substituted(mapping) for p, v in self.parameter_values.items()},
            self.type_,
            self.origin,
        )

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

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
    origin: Optional[Origin] = None

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
    origin: Optional[Origin] = None

    @property
    def location(self) -> Optional[Location]:
        return self.expression.location

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

    origin: Optional[Origin]
    _str: Optional[str] = field(init=False, default=None)

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
    def type_(self) -> rty.Any:
        raise NotImplementedError

    @property
    def origin_str(self) -> str:
        if self.origin:
            return str(self.origin)
        return str(self)

    @property
    def location(self) -> Optional[Location]:
        return self.origin.location if self.origin else None

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
    def type_(self) -> rty.AnyInteger:
        raise NotImplementedError

    @abstractmethod
    def to_z3_expr(self) -> z3.ArithRef:
        raise NotImplementedError


class BoolExpr(Expr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    @abstractmethod
    def to_z3_expr(self) -> z3.BoolRef:
        raise NotImplementedError


class BasicIntExpr(BasicExpr, IntExpr):
    @property
    @abstractmethod
    def type_(self) -> rty.AnyInteger:
        raise NotImplementedError


class BasicBoolExpr(BasicExpr, BoolExpr):
    pass


@define(eq=False)
class Var(BasicExpr):
    identifier: ID = field(converter=ID)
    origin: Optional[Origin] = None

    def _update_str(self) -> None:
        self._str = intern(str(self.identifier))


@define(eq=False)
class IntVar(Var, BasicIntExpr):
    identifier: ID = field(converter=ID)
    var_type: rty.AnyInteger
    negative: bool = False
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.AnyInteger:
        return self.var_type

    def substituted(self, mapping: Mapping[ID, ID]) -> IntVar:
        if self.identifier in mapping:
            return IntVar(mapping[self.identifier], self.var_type, self.negative, self.origin)
        return self

    def to_z3_expr(self) -> z3.ArithRef:
        expression = z3.Int(str(self.identifier))
        return -expression if self.negative else expression


@define(eq=False)
class BoolVar(Var, BasicBoolExpr):
    identifier: ID = field(converter=ID)
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def substituted(self, mapping: Mapping[ID, ID]) -> BoolVar:
        if self.identifier in mapping:
            return BoolVar(mapping[self.identifier], self.origin)
        return self

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self.identifier))


@define(eq=False)
class ObjVar(Var):
    identifier: ID = field(converter=ID)
    var_type: rty.Any
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Any:
        return self.var_type

    def substituted(self, mapping: Mapping[ID, ID]) -> ObjVar:
        if self.identifier in mapping:
            return ObjVar(mapping[self.identifier], self.var_type, self.origin)
        return self

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class EnumLit(BasicExpr):
    identifier: ID = field(converter=ID)
    enum_type: rty.Enumeration
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Enumeration:
        return self.enum_type

    def substituted(self, _mapping: Mapping[ID, ID]) -> EnumLit:
        return self

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self.identifier))

    def _update_str(self) -> None:
        self._str = intern(str(self.identifier))


@define(eq=False)
class IntVal(BasicIntExpr):
    value: int
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.UniversalInteger:
        return rty.UniversalInteger(rty.Bounds(self.value, self.value))

    def substituted(self, _mapping: Mapping[ID, ID]) -> IntVal:
        return self

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.IntVal(self.value)

    def _update_str(self) -> None:
        self._str = intern(str(self.value))


@define(eq=False)
class BoolVal(BasicBoolExpr):
    value: bool
    origin: Optional[Origin] = None

    def substituted(self, _mapping: Mapping[ID, ID]) -> BoolVal:
        return self

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.BoolVal(val=self.value)

    def _update_str(self) -> None:
        self._str = intern(str(self.value))


@define(eq=False)
class Attr(Expr):
    prefix: ID = field(converter=ID)
    prefix_type: rty.Any
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> Attr:
        return self.__class__(
            mapping[self.prefix] if self.prefix in mapping else self.prefix,
            self.prefix_type,
            self.origin,
        )

    def _update_str(self) -> None:
        symbol = re.sub(r"([a-z])([A-Z])", r"\1_\2", self.__class__.__name__)
        self._str = intern(f"{self.prefix}'{symbol}")


@define(eq=False)
class IntAttr(Attr, IntExpr):
    prefix: ID = field(converter=ID)
    prefix_type: rty.Any
    negative: bool = False
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> IntAttr:
        return self.__class__(
            mapping[self.prefix] if self.prefix in mapping else self.prefix,
            self.prefix_type,
            self.negative,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ArithRef:
        return -z3.Int(str(self)) if self.negative else z3.Int(str(self))

    def _update_str(self) -> None:
        super(IntAttr, self)._update_str()  # noqa: UP008
        if self.negative:
            self._str = f"-{self._str}"


@define(eq=False)
class Size(IntAttr):
    @property
    def type_(self) -> rty.AnyInteger:
        return (
            rty.BIT_LENGTH
            if isinstance(self.prefix_type, (rty.Composite, rty.Compound))
            else rty.UniversalInteger()
        )


@define(eq=False)
class Length(IntAttr):
    @property
    def type_(self) -> rty.UniversalInteger:
        return rty.UniversalInteger()


@define(eq=False)
class First(IntAttr):
    @property
    def type_(self) -> rty.UniversalInteger:
        return rty.UniversalInteger()


@define(eq=False)
class Last(IntAttr):
    @property
    def type_(self) -> rty.UniversalInteger:
        return rty.UniversalInteger()


@define(eq=False)
class ValidChecksum(Attr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


@define(eq=False)
class Valid(Attr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


@define(eq=False)
class Present(Attr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


@define(eq=False)
class HasData(Attr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


@define(eq=False)
class Head(Attr):
    prefix: ID = field(converter=ID)
    prefix_type: rty.Composite
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Any:
        assert isinstance(self.prefix_type.element, rty.Any)
        return self.prefix_type.element

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class Opaque(Attr):
    prefix: ID = field(converter=ID)
    prefix_type: Union[rty.Message, rty.Sequence]
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Sequence:
        return rty.OPAQUE

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError


@define(eq=False)
class FieldAccessAttr(Expr):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: rty.Compound
    origin: Optional[Origin] = None

    @property
    def field_type(self) -> rty.Any:
        type_ = self.message_type.field_types[self.field]
        assert isinstance(type_, rty.Any)
        return type_

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAccessAttr:
        return self.__class__(
            mapping[self.message] if self.message in mapping else self.message,
            self.field,
            self.message_type,
            self.origin,
        )

    def _update_str(self) -> None:
        symbol = re.sub(r"([a-z])([A-Z])", r"\1_\2", self.__class__.__name__)
        self._str = intern(f"{self.message}.{self.field}'{symbol}")


@define(eq=False)
class FieldValid(FieldAccessAttr, BoolExpr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class FieldPresent(FieldAccessAttr, BoolExpr):
    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class FieldSize(FieldAccessAttr, IntExpr):
    @property
    def type_(self) -> rty.Integer:
        return rty.BIT_LENGTH

    def to_z3_expr(self) -> z3.ArithRef:
        return z3.Int(str(self))


@define(eq=False)
class UnaryExpr(Expr):
    expression: BasicExpr
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> UnaryExpr:
        return self.__class__(self.expression.substituted(mapping), self.origin)


@define(eq=False)
class UnaryBoolExpr(UnaryExpr, BoolExpr):
    expression: BasicBoolExpr
    origin: Optional[Origin] = None


@define(eq=False)
class BinaryExpr(Expr):
    left: BasicExpr
    right: BasicExpr
    origin: Optional[Origin] = None

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
    def location(self) -> Optional[Location]:
        if self.origin is not None:
            return self.origin.location
        if self.left.origin is not None:
            return self.left.origin.location
        return None

    def _update_str(self) -> None:
        self._str = intern(f"{self.left}{self._symbol}{self.right}")

    @property
    def _symbol(self) -> str:
        raise NotImplementedError


@define(eq=False)
class BinaryIntExpr(BinaryExpr, IntExpr):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.AnyInteger:
        type_ = self.left.type_.common_type(self.right.type_)
        assert isinstance(type_, rty.AnyInteger)
        return type_


@define(eq=False)
class BinaryBoolExpr(BinaryExpr, BoolExpr):
    left: BasicBoolExpr
    right: BasicBoolExpr
    origin: Optional[Origin] = None


@define(eq=False)
class Add(BinaryIntExpr):
    def to_z3_expr(self) -> z3.ArithRef:
        return self.left.to_z3_expr() + self.right.to_z3_expr()

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        v_id = next(variable_id)
        v_type = to_integer(self.type_)
        upper_bound = (
            self.type_.bounds.upper
            if isinstance(self.type_, rty.BoundedInteger) and self.type_.bounds.upper is not None
            else INT_MAX
        )
        return [
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
                            self.right,
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

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
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

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        v_id = next(variable_id)
        v_type = to_integer(self.type_)
        upper_bound = (
            self.type_.bounds.upper
            if isinstance(self.type_, rty.BoundedInteger) and self.type_.bounds.upper is not None
            else INT_MAX
        )
        return [
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

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
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

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        v_id = next(variable_id)
        v_type = to_integer(self.type_)
        upper_bound = (
            self.type_.bounds.upper
            if isinstance(self.type_, rty.BoundedInteger) and self.type_.bounds.upper is not None
            else INT_MAX
        )
        return [
            # Left ** Right <= Upper_Bound
            Cond(
                LessEqual(
                    IntVar(v_id, self.type_, origin=self.origin),
                    IntVal(upper_bound),
                ),
                [
                    VarDecl(v_id, v_type, None, origin=self.origin),
                    Assign(v_id, self, to_integer(self.type_), origin=self.origin),
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

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
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
    origin: Optional[Origin] = None


@define(eq=False)
class Less(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Optional[Origin] = None

    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() < self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " < "


@define(eq=False)
class LessEqual(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Optional[Origin] = None

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
    origin: Optional[Origin] = None

    def to_z3_expr(self) -> z3.BoolRef:
        return self.left.to_z3_expr() >= self.right.to_z3_expr()

    @property
    def _symbol(self) -> str:
        return " >= "


@define(eq=False)
class Greater(Relation):
    left: BasicIntExpr
    right: BasicIntExpr
    origin: Optional[Origin] = None

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
    argument_types: Sequence[rty.Any]
    origin: Optional[Origin] = None
    _preconditions: list[Cond] = field(init=False, factory=list)

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._preconditions

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
    argument_types: Sequence[rty.Any]
    type_: rty.AnyInteger
    negative: bool = False
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> IntCall:
        return self.__class__(
            self.identifier,
            [a.substituted(mapping) for a in self.arguments],
            self.argument_types,
            self.type_,
            self.negative,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ArithRef:
        # TODO(eng/recordflux/RecordFlux#1338): Return value need not to be identical
        # The return value may be different even if the arguments are the same.
        expression = z3.Int(str(self.identifier))
        return -expression if self.negative else expression

    def _update_str(self) -> None:
        self._str = intern(
            ("-" if self.negative else "")
            + str(self.identifier)
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
    argument_types: Sequence[rty.Any]
    type_: rty.Any
    origin: Optional[Origin] = None

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
    message_type: rty.Compound
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAccess:
        return self.__class__(
            mapping[self.message] if self.message in mapping else self.message,
            self.field,
            self.message_type,
            self.origin,
        )

    def _update_str(self) -> None:
        self._str = intern(f"{self.message}.{self.field}")


@define(eq=False)
class IntFieldAccess(FieldAccess, IntExpr):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: rty.Compound
    negative: bool = False
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.AnyInteger:
        type_ = self.message_type.types[self.field]
        assert isinstance(type_, rty.AnyInteger)
        return type_

    def substituted(self, mapping: Mapping[ID, ID]) -> IntFieldAccess:
        return self.__class__(
            mapping[self.message] if self.message in mapping else self.message,
            self.field,
            self.message_type,
            self.negative,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ArithRef:
        expression = z3.Int(str(self))
        return -expression if self.negative else expression

    def _update_str(self) -> None:
        sign = "-" if self.negative else ""
        self._str = intern(f"{sign}{self.message}.{self.field}")


@define(eq=False)
class BoolFieldAccess(FieldAccess, BoolExpr):
    def to_z3_expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


@define(eq=False)
class ObjFieldAccess(FieldAccess):
    message: ID = field(converter=ID)
    field: ID = field(converter=ID)
    message_type: rty.Compound
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Any:
        type_ = self.message_type.field_types[self.field]
        assert isinstance(type_, rty.Any)
        return type_

    def substituted(self, mapping: Mapping[ID, ID]) -> ObjFieldAccess:
        return self.__class__(
            mapping[self.message] if self.message in mapping else self.message,
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
    origin: Optional[Origin] = None

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
    return_type: rty.AnyInteger
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.AnyInteger:
        return self.return_type

    def to_z3_expr(self) -> z3.ArithRef:
        result = super(IntIfExpr, self).to_z3_expr()  # noqa: UP008
        assert isinstance(result, z3.ArithRef)
        return result


@define(eq=False)
class BoolIfExpr(IfExpr, BoolExpr):
    condition: BasicBoolExpr
    then_expr: ComplexBoolExpr
    else_expr: ComplexBoolExpr
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Enumeration:
        return rty.BOOLEAN

    def to_z3_expr(self) -> z3.BoolRef:
        result = super(BoolIfExpr, self).to_z3_expr()  # noqa: UP008
        assert isinstance(result, z3.BoolRef)
        return result


@define(eq=False)
class Conversion(Expr):
    identifier: ID = field(converter=ID)
    argument: Expr
    target_type: rty.Any
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Any:
        return self.target_type

    def substituted(self, mapping: Mapping[ID, ID]) -> Conversion:
        return self.__class__(
            self.identifier,
            self.argument.substituted(mapping),
            self.target_type,
            self.origin,
        )

    def to_z3_expr(self) -> z3.ExprRef:
        return z3.BoolVal(val=True)

    def _update_str(self) -> None:
        self._str = intern(f"{self.identifier} ({self.argument})")


@define(eq=False)
class IntConversion(Conversion, BasicIntExpr):
    identifier: ID = field(converter=ID)
    argument: IntExpr
    target_type: rty.Integer
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Integer:
        return self.target_type

    def to_z3_expr(self) -> z3.ArithRef:
        return self.argument.to_z3_expr()


@define(eq=False)
class Comprehension(Expr):
    iterator: ID = field(converter=ID)
    sequence: Union[Var, FieldAccess]
    selector: ComplexExpr
    condition: ComplexBoolExpr
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Aggregate:
        return rty.Aggregate(self.selector.expr.type_)

    def substituted(self, mapping: Mapping[ID, ID]) -> Comprehension:
        return self.__class__(
            mapping[self.iterator] if self.iterator in mapping else self.iterator,
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

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern(
            f"[for {self.iterator} in {self.sequence} if {self.condition} => {self.selector}]",
        )


@define(eq=False)
class Find(Expr):
    iterator: ID = field(converter=ID)
    sequence: Union[Var, FieldAccess]
    selector: ComplexExpr
    condition: ComplexBoolExpr
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Any:
        return self.selector.expr.type_

    def substituted(self, mapping: Mapping[ID, ID]) -> Find:
        return self.__class__(
            mapping[self.iterator] if self.iterator in mapping else self.iterator,
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

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    def to_z3_expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern(
            f"Find (for {self.iterator} in {self.sequence} if {self.condition} => {self.selector})",
        )


@define(eq=False)
class Agg(Expr):
    elements: Sequence[BasicExpr]
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Aggregate:
        return rty.Aggregate(rty.common_type([e.type_ for e in self.elements]))

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
    elements: Sequence[tuple[Union[StrID, BasicExpr], BasicExpr]],
) -> Sequence[tuple[Union[ID, BasicExpr], BasicExpr]]:
    return [(ID(n) if isinstance(n, str) else n, e) for n, e in elements]


@define(eq=False)
class NamedAgg(Expr):
    """Only used by code generator and therefore provides minimum functionality."""

    elements: Sequence[tuple[Union[ID, BasicExpr], BasicExpr]] = field(
        converter=_named_agg_elements_converter,
    )
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Any:
        raise NotImplementedError

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
    origin: Optional[Origin] = None

    @property
    def type_(self) -> rty.Sequence:
        return rty.OPAQUE

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
    type_: rty.Message
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> MsgAgg:
        return self.__class__(
            self.identifier,
            {f: v.substituted(mapping) for f, v in self.field_values.items()},
            self.type_,
            origin=self.origin,
        )

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

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
    type_: rty.Message
    origin: Optional[Origin] = None

    def substituted(self, mapping: Mapping[ID, ID]) -> DeltaMsgAgg:
        return self.__class__(
            self.identifier,
            {f: v.substituted(mapping) for f, v in self.field_values.items()},
            self.type_,
            origin=self.origin,
        )

    def preconditions(self, _variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

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
    type_: rty.Any
    origin: Optional[Origin] = None

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


@frozen
class Decl:
    identifier: ID = field(converter=ID)
    location: Optional[Location]


@frozen
class FormalDecl(Decl):
    pass


@frozen
class Argument:
    identifier: ID = field(converter=ID)
    type_identifier: ID = field(converter=ID)
    type_: rty.Type


@frozen
class FuncDecl(FormalDecl):
    identifier: ID = field(converter=ID)
    arguments: Sequence[Argument]
    return_type: ID = field(converter=ID)
    type_: rty.Type
    location: Optional[Location]


@frozen
class ChannelDecl(FormalDecl):
    identifier: ID = field(converter=ID)
    readable: bool
    writable: bool
    location: Optional[Location]


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
    description: Optional[str]
    location: Optional[Location]


@frozen
class State:
    identifier: ID = field(converter=ID)
    transitions: Sequence[Transition]
    exception_transition: Optional[Transition]
    actions: Sequence[Stmt]
    description: Optional[str]
    location: Optional[Location]

    @property
    def declarations(self) -> list[VarDecl]:
        return [a for a in self.actions if isinstance(a, VarDecl)]


FINAL_STATE = State("Final", [], None, [], None, None)


@frozen(init=False)
class Session:
    identifier: ID = field(converter=ID)
    states: Sequence[State]
    declarations: Sequence[VarDecl]
    parameters: Sequence[FormalDecl]
    types: Mapping[ID, mty.Type]
    location: Optional[Location]

    def __init__(  # noqa: PLR0913
        self,
        identifier: ID,
        states: Sequence[State],
        declarations: Sequence[VarDecl],
        parameters: Sequence[FormalDecl],
        types: Mapping[ID, mty.Type],
        location: Optional[Location],
        variable_id: Generator[ID, None, None],
        workers: int = 1,
    ) -> None:
        manager = ProofManager(workers)
        states = [
            State(
                s.identifier,
                [
                    Transition(
                        t.target,
                        ComplexExpr(
                            add_required_checks(t.condition.stmts, manager, variable_id),
                            t.condition.expr,
                        ),
                        t.description,
                        t.location,
                    )
                    for t in s.transitions
                ],
                s.exception_transition,
                add_required_checks(s.actions, manager, variable_id),
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


def add_checks(statements: Sequence[Stmt], variable_id: Generator[ID, None, None]) -> list[Stmt]:
    """Add check statements in places where preconditions must be ensured."""

    result = []

    for statement in statements:
        preconditions = statement.preconditions(variable_id)

        for precondition in preconditions:
            result.extend([*precondition.facts, Check(precondition.goal)])

        result.append(statement)

    return result


def remove_unnecessary_checks(statements: Sequence[Stmt], manager: ProofManager) -> list[Stmt]:
    """Remove all checks that are always true."""

    always_true: list[int] = []

    facts: list[Stmt] = []

    for i, s in enumerate(statements):
        if isinstance(s, Check):
            manager.add(
                [
                    ProofJob(
                        [
                            Check(Not(BoolVar("__GOAL__"))),
                            Assign("__GOAL__", s.expression, s.expression.type_),
                            *facts,
                        ],
                        {
                            ProofResult.UNSAT: [i],
                            ProofResult.SAT: [],
                            ProofResult.UNKNOWN: [],
                        },
                    ),
                ],
            )
            facts.append(s)

    results = manager.check()

    for r in results:
        always_true.extend(r.result)

    result = list(statements)
    for i in reversed(always_true):
        result = [*result[:i], *result[i + 1 :]]

    return remove_unused_assignments(result)


def remove_unused_assignments(statements: Sequence[Stmt]) -> list[Stmt]:
    # TODO(eng/recordflux/RecordFlux#1339): Add removal of unused assignments
    return list(statements)


def add_required_checks(
    statements: Sequence[Stmt],
    manager: ProofManager,
    variable_id: Generator[ID, None, None],
) -> list[Stmt]:
    """
    Add check statements in places where preconditions are not always true.

    For each statement it is checked, if its preconditions are always true. If this is not the
    case, a check statement is added in front of the respective statement. The check statements
    in the resulting list mark the places where the code generator must insert explicit checks.
    """
    result = remove_unnecessary_checks(add_checks(statements, variable_id), manager)

    for s in result:
        if isinstance(s, Check):
            info(
                f'precondition "{s.expression.origin_str}" must be checked at runtime',
                Subsystem.MODEL,
                s.expression.location,
            )

    return result


def to_integer(type_: rty.AnyInteger) -> rty.Integer:
    return type_ if isinstance(type_, rty.Integer) else rty.BASE_INTEGER
