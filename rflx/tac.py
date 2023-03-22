# pylint: disable = too-many-lines, fixme

"""
Intermediate representation in three-address code (TAC) format.

This module is still under development (cf. Eng/RecordFlux/RecordFlux#1204).
"""

from __future__ import annotations

import re
from abc import abstractmethod
from collections import Counter
from collections.abc import Generator, Mapping, Sequence
from concurrent.futures import ProcessPoolExecutor
from enum import Enum
from sys import intern
from typing import Optional, Protocol, TypeVar

import z3

from rflx.common import Base
from rflx.const import MAX_SCALAR_SIZE
from rflx.error import Location, RecordFluxError, Severity, Subsystem
from rflx.identifier import ID, StrID

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
        logic: str = "QF_NIA",
    ):
        self._facts = facts
        self._logic = logic

        self._results: Mapping[ProofResult, object]
        self._result: object

    @property
    @abstractmethod
    def result(self) -> object:
        raise NotImplementedError

    def check(self) -> ProofJob:
        """Check the specified facts and return the corresponding object depending on the result."""

        solver = z3.SolverFor(self._logic)

        for f in self._facts:
            solver.add(f.z3expr)

        proof_result = ProofResult(solver.check())
        self._result = self._results[proof_result]

        return self


class StmtListProofJob(ProofJob):
    def __init__(
        self,
        facts: Sequence[Stmt],
        results: Mapping[ProofResult, list[Stmt]],
        logic: str = "QF_NIA",
    ):
        super().__init__(facts, logic)
        self._results: Mapping[ProofResult, list[Stmt]] = results

        self._result: list[Stmt]

    @property
    def result(self) -> list[Stmt]:
        return self._result


class ErrorProofJob(ProofJob):
    def __init__(
        self,
        facts: Sequence[Stmt],
        results: Mapping[ProofResult, RecordFluxError],
        logic: str = "QF_NIA",
    ):
        super().__init__(facts, logic)
        self._results: Mapping[ProofResult, RecordFluxError] = results

        self._result: RecordFluxError

    @property
    def result(self) -> RecordFluxError:
        return self._result


class ProofManager(Base):
    def __init__(self, workers: int) -> None:
        self._jobs: list[ProofJob] = []
        self._workers = workers

    def add(self, jobs: Sequence[ProofJob]) -> None:
        self._jobs.extend(jobs)

    def check(self) -> list[ProofJob]:
        with ProcessPoolExecutor(max_workers=self._workers) as executor:
            result = list(executor.map(ProofManager._check, self._jobs))

        self._jobs.clear()

        return result

    @staticmethod
    def _check(job: ProofJob) -> ProofJob:
        return job.check()


class Cond(Base):
    def __init__(self, goal: BoolExpr, facts: Optional[list[Stmt]] = None) -> None:
        self._goal = goal
        self._facts = facts or []

    @property
    def goal(self) -> BoolExpr:
        return self._goal

    @property
    def facts(self) -> list[Stmt]:
        return self._facts


class Stmt(Base):
    _str: str

    def __init__(self, origin: Optional[Origin] = None) -> None:
        self._origin = origin

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) == str(other)
        return NotImplemented

    def __str__(self) -> str:
        try:
            return self._str
        except AttributeError:
            self._update_str()
            return self._str

    @property
    def origin(self) -> Optional[Origin]:
        return self._origin

    @property
    def location(self) -> Optional[Location]:
        return self._origin.location if self._origin else None

    def substituted(self, mapping: Mapping[ID, ID]) -> Stmt:
        raise NotImplementedError

    @abstractmethod
    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        raise NotImplementedError

    @property
    @abstractmethod
    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError

    def precondition_proofs(self, facts: Sequence[Stmt]) -> list[ProofJob]:
        raise NotImplementedError

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError


class Assign(Stmt):
    def __init__(
        self,
        target: StrID,
        expression: Expr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._target = ID(target)
        self._expression = expression

    @property
    def target(self) -> ID:
        return self._target

    @property
    def expression(self) -> Expr:
        return self._expression

    def substituted(self, mapping: Mapping[ID, ID]) -> Assign:
        return Assign(
            mapping[self._target] if self._target in mapping else self._target,
            self._expression.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._expression.preconditions(variable_id)

    @property
    def z3expr(self) -> z3.BoolRef:
        target: Var
        if isinstance(self._expression, IntExpr):
            target = IntVar(self._target)
        elif isinstance(self._expression, BoolExpr):
            target = BoolVar(self._target)
        else:
            return z3.BoolVal(True)
        return target.z3expr == self._expression.z3expr

    @property
    def target_var(self) -> Var:
        if isinstance(self._expression, IntExpr):
            return IntVar(self._target)
        if isinstance(self._expression, BoolExpr):
            return BoolVar(self._target)
        assert isinstance(self._expression, ObjVar)
        return ObjVar(self._target)

    def _update_str(self) -> None:
        self._str = intern(f"{self._target} := {self._expression}")


class FieldAssign(Stmt):
    def __init__(
        self,
        message: StrID,
        field: StrID,
        expression: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._message = ID(message)
        self._field = ID(field)
        self._expression = expression

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAssign:
        return FieldAssign(
            mapping[self._message] if self._message in mapping else self._message,
            self._field,
            self._expression.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._expression.preconditions(variable_id)

    @property
    def z3expr(self) -> z3.BoolRef:
        target: z3.ExprRef
        if isinstance(self._expression, IntExpr):
            target = z3.Int(f"{self._message}.{self._field}")
        elif isinstance(self._expression, BoolExpr):
            target = z3.Bool(f"{self._message}.{self._field}")
        else:
            return z3.BoolVal(True)
        return target == self._expression.z3expr

    def _update_str(self) -> None:
        self._str = intern(f"{self._message}.{self._field} := {self._expression}")


class Append(Stmt):
    def __init__(
        self,
        sequence: StrID,
        expression: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._sequence = ID(sequence)
        self._expression = expression

    def substituted(self, mapping: Mapping[ID, ID]) -> Append:
        return Append(
            mapping[self._sequence] if self._sequence in mapping else self._sequence,
            self._expression.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._expression.preconditions(variable_id)

    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)

    def _update_str(self) -> None:
        self._str = intern(f"{self._sequence}'Append ({self._expression})")


class Extend(Stmt):
    def __init__(
        self,
        sequence: StrID,
        expression: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._sequence = ID(sequence)
        self._expression = expression

    def substituted(self, mapping: Mapping[ID, ID]) -> Extend:
        return Extend(
            mapping[self._sequence] if self._sequence in mapping else self._sequence,
            self._expression.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._expression.preconditions(variable_id)

    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)

    def _update_str(self) -> None:
        self._str = intern(f"{self._sequence}'Extend ({self._expression})")


class Reset(Stmt):
    def __init__(
        self,
        identifier: StrID,
        parameter_values: Optional[Mapping[ID, BasicExpr]] = None,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._identifier = ID(identifier)
        self._parameter_values = parameter_values or {}

    def substituted(self, mapping: Mapping[ID, ID]) -> Reset:
        return Reset(
            mapping[self._identifier] if self._identifier in mapping else self._identifier,
            {p: v.substituted(mapping) for p, v in self._parameter_values.items()},
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)

    def _update_str(self) -> None:
        parameter_values = " => ".join(f"{i} => {e}" for i, e in self._parameter_values.items())
        if parameter_values:
            parameter_values = f" ({parameter_values})"
        self._str = intern(f"{self._identifier}'Reset{parameter_values}")


class ChannelStmt(Stmt):
    def __init__(
        self,
        channel: StrID,
        expression: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._channel = ID(channel)
        self._expression = expression

    def substituted(self, mapping: Mapping[ID, ID]) -> ChannelStmt:
        return self.__class__(
            self._channel,
            self._expression.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._expression.preconditions(variable_id)

    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(True)

    def _update_str(self) -> None:
        self._str = intern(f"{self._channel}'{self.__class__.__name__} ({self._expression})")


class Read(ChannelStmt):
    pass


class Write(ChannelStmt):
    pass


class Assert(Stmt):
    def __init__(
        self,
        expression: BoolExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(origin)
        self._expression = expression

    def substituted(self, mapping: Mapping[ID, ID]) -> Assert:
        return Assert(
            self._expression.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._expression.preconditions(variable_id)

    @property
    def z3expr(self) -> z3.BoolRef:
        return self._expression.z3expr

    def _update_str(self) -> None:
        self._str = intern(f"Assert {self._expression}")


Self = TypeVar("Self", bound="Expr")


class Expr(Base):
    _str: str
    _origin: Optional[Origin]

    def __eq__(self, other: object) -> bool:
        if isinstance(other, self.__class__):
            return str(self) == str(other)
        return NotImplemented

    def __str__(self) -> str:
        try:
            return self._str
        except AttributeError:
            self._update_str()
            return self._str

    @property
    def origin(self) -> Optional[Origin]:
        return self._origin

    @property
    def origin_str(self) -> str:
        return str(self._origin)

    @property
    def location(self) -> Optional[Location]:
        return self._origin.location if self._origin else None

    def substituted(self: Self, mapping: Mapping[ID, ID]) -> Self:
        raise NotImplementedError

    @property
    @abstractmethod
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        # pylint: disable = unused-argument
        return []

    @abstractmethod
    def _update_str(self) -> None:
        raise NotImplementedError


class BasicExpr(Expr):
    pass


class IntExpr(Expr):
    @property
    @abstractmethod
    def z3expr(self) -> z3.ArithRef:
        raise NotImplementedError


class BoolExpr(Expr):
    @property
    @abstractmethod
    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class BasicIntExpr(BasicExpr, IntExpr):
    pass


class BasicBoolExpr(BasicExpr, BoolExpr):
    pass


class Var(BasicExpr):
    def __init__(
        self,
        identifier: StrID,
        origin: Optional[Origin] = None,
    ) -> None:
        self._identifier = ID(identifier)
        self._origin = origin

    @property
    def identifier(self) -> ID:
        return self._identifier

    def _update_str(self) -> None:
        self._str = intern(str(self._identifier))


class IntVar(Var, BasicIntExpr):
    def __init__(
        self,
        identifier: StrID,
        negative: bool = False,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(identifier)
        self._negative = negative
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> IntVar:
        if self._identifier in mapping:
            return IntVar(mapping[self._identifier], self._negative, self._origin)
        return self

    @property
    def z3expr(self) -> z3.ArithRef:
        expression = z3.Int(str(self._identifier))
        return -expression if self._negative else expression


class BoolVar(Var, BasicBoolExpr):
    def substituted(self, mapping: Mapping[ID, ID]) -> BoolVar:
        if self._identifier in mapping:
            return BoolVar(mapping[self._identifier], self._origin)
        return self

    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.Bool(str(self._identifier))


class ObjVar(Var):
    def substituted(self, mapping: Mapping[ID, ID]) -> ObjVar:
        if self._identifier in mapping:
            return ObjVar(mapping[self._identifier], self._origin)
        return self

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class EnumLit(BasicIntExpr):
    def __init__(
        self,
        identifier: StrID,
        origin: Optional[Origin] = None,
    ) -> None:
        assert str(identifier) not in ("True", "False")
        self._identifier = ID(identifier)
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> EnumLit:
        return self

    @property
    def z3expr(self) -> z3.ArithRef:
        return z3.Int(str(self._identifier))

    def _update_str(self) -> None:
        self._str = intern(str(self._identifier))


class IntVal(BasicIntExpr):
    def __init__(self, value: int, origin: Optional[Origin] = None) -> None:
        self._value = value
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> IntVal:
        return self

    @property
    def z3expr(self) -> z3.ArithRef:
        return z3.IntVal(self._value)

    def _update_str(self) -> None:
        self._str = intern(str(self._value))


class BoolVal(BasicBoolExpr):
    def __init__(self, value: bool, origin: Optional[Origin] = None) -> None:
        self._value = value
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> BoolVal:
        return self

    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(self._value)

    def _update_str(self) -> None:
        self._str = intern(str(self._value))


class Attr(Expr):
    def __init__(self, prefix: StrID, origin: Optional[Origin] = None) -> None:
        self._prefix = ID(prefix)
        self._origin = origin

    @property
    def prefix(self) -> ID:
        return self._prefix

    def substituted(self, mapping: Mapping[ID, ID]) -> Attr:
        return self.__class__(
            mapping[self._prefix] if self._prefix in mapping else self._prefix, self._origin
        )

    def _update_str(self) -> None:
        symbol = re.sub(r"([a-z])([A-Z])", r"\1_\2", self.__class__.__name__)
        self._str = intern(f"{self._prefix}'{symbol}")


class Size(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Int(str(self))


class Length(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Int(str(self))


class First(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Int(str(self))


class Last(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Int(str(self))


class ValidChecksum(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


class Valid(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


class Present(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


class HasData(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.Bool(str(self))


class Head(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class Opaque(Attr):
    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class UnaryExpr(Expr):
    def __init__(
        self,
        expression: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._expression = expression
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> UnaryExpr:
        return self.__class__(self._expression.substituted(mapping), self._origin)


class UnaryBoolExpr(UnaryExpr, BoolExpr):
    def __init__(
        self,
        expression: BasicBoolExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(expression)
        self._expression: BasicBoolExpr
        self._origin = origin


class BinaryExpr(Expr):
    def __init__(
        self,
        left: BasicExpr,
        right: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._left = left
        self._right = right
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> BinaryExpr:
        return self.__class__(
            self._left.substituted(mapping), self._right.substituted(mapping), self._origin
        )

    @property
    def origin_str(self) -> str:
        if self._origin is not None:
            return str(self._origin)
        return f"{self._left.origin_str}{self._symbol}{self._right.origin_str}"

    @property
    def location(self) -> Optional[Location]:
        if self._origin is not None:
            return self._origin.location
        if self._left.origin is not None:
            return self._left.origin.location
        return None

    def _update_str(self) -> None:
        self._str = intern(f"{self._left}{self._symbol}{self._right}")

    @property
    def _symbol(self) -> str:
        raise NotImplementedError


class BinaryIntExpr(BinaryExpr, IntExpr):
    def __init__(
        self,
        left: BasicIntExpr,
        right: BasicIntExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(left, right)
        self._left: BasicIntExpr
        self._right: BasicIntExpr
        self._origin = origin


class BinaryBoolExpr(BinaryExpr, BoolExpr):
    def __init__(
        self,
        left: BasicBoolExpr,
        right: BasicBoolExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(left, right)
        self._left: BasicBoolExpr
        self._right: BasicBoolExpr
        self._origin = origin


class Add(BinaryIntExpr):
    @property
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr + self._right.z3expr

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        v_id = next(variable_id)
        return [
            # Left + Right <= INT_MAX
            Cond(
                LessEqual(
                    self._left,
                    IntVar(
                        v_id,
                        origin=(
                            ConstructedOrigin(
                                f"{INT_MAX} - {self._right.origin}",
                                self._origin.location,
                            )
                            if self._origin and self._right.origin
                            else None
                        ),
                    ),
                ),
                [Assign(v_id, Sub(IntVal(INT_MAX), self._right))],
            )
        ]

    @property
    def _symbol(self) -> str:
        return " + "


class Sub(BinaryIntExpr):
    @property
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr - self._right.z3expr

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            # Left >= Right
            Cond(
                GreaterEqual(
                    self._left,
                    self._right,
                    origin=(
                        ConstructedOrigin(
                            f"{self._left.origin} >= {self._right.origin}",
                            self._origin.location,
                        )
                        if self._origin and self._left.origin and self._right.origin
                        else None
                    ),
                )
            ),
        ]

    @property
    def _symbol(self) -> str:
        return " - "


class Mul(BinaryIntExpr):
    @property
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr * self._right.z3expr

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        v_id = next(variable_id)
        return [
            # Left * Right <= INT_MAX
            Cond(
                LessEqual(
                    self._left,
                    IntVar(v_id),
                    origin=(
                        ConstructedOrigin(
                            f"{self._left.origin} <= {INT_MAX} / {self._right.origin}",
                            self._origin.location,
                        )
                        if self._origin and self._left.origin and self._right.origin
                        else None
                    ),
                ),
                [Assign(v_id, Div(IntVal(INT_MAX), self._right))],
            ),
        ]

    @property
    def _symbol(self) -> str:
        return " * "


class Div(BinaryIntExpr):
    @property
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr / self._right.z3expr

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            # Right /= 0
            Cond(
                NotEqual(
                    self._right,
                    IntVal(0),
                    origin=(
                        ConstructedOrigin(
                            f"{self._right.origin} /= 0",
                            self._right.origin.location,
                        )
                        if self._right.origin
                        else None
                    ),
                )
            )
        ]

    @property
    def _symbol(self) -> str:
        return " / "


class Pow(BinaryIntExpr):
    @property
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr**self._right.z3expr

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        v_id = next(variable_id)
        return [
            # Left ** Right <= INT_MAX
            Cond(
                LessEqual(
                    IntVar(v_id),
                    IntVal(INT_MAX),
                    origin=(
                        ConstructedOrigin(
                            f"{self._origin} <= {INT_MAX}",
                            self._origin.location,
                        )
                        if self._origin and self._left.origin and self._right.origin
                        else None
                    ),
                ),
                [Assign(v_id, self)],
            )
        ]

    @property
    def _symbol(self) -> str:
        return " ** "


class Mod(BinaryIntExpr):
    @property
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr % self._right.z3expr

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return [
            # Right /= 0
            Cond(
                NotEqual(
                    self._right,
                    IntVal(0),
                    origin=(
                        ConstructedOrigin(
                            f"{self._right.origin} /= 0",
                            self._right.origin.location,
                        )
                        if self._right.origin
                        else None
                    ),
                )
            )
        ]

    @property
    def _symbol(self) -> str:
        return " mod "


class Not(UnaryBoolExpr):
    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.Not(self._expression.z3expr)

    def _update_str(self) -> None:
        self._str = intern(f"not {self._expression}")


class And(BinaryBoolExpr):
    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.And(self._left.z3expr, self._right.z3expr)

    @property
    def _symbol(self) -> str:
        return " and "


class Or(BinaryBoolExpr):
    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.Or(self._left.z3expr, self._right.z3expr)

    @property
    def _symbol(self) -> str:
        return " or "


class Relation(BoolExpr, BinaryExpr):
    def __init__(
        self,
        left: BasicIntExpr,
        right: BasicIntExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(left, right, origin)
        self._left: BasicIntExpr = left
        self._right: BasicIntExpr = right
        self._origin = origin


class Less(Relation):
    @property
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr < self._right.z3expr

    @property
    def _symbol(self) -> str:
        return " < "


class LessEqual(Relation):
    @property
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr <= self._right.z3expr

    @property
    def _symbol(self) -> str:
        return " <= "


class Equal(Relation):
    @property
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr == self._right.z3expr

    @property
    def _symbol(self) -> str:
        return " = "


class GreaterEqual(Relation):
    @property
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr >= self._right.z3expr

    @property
    def _symbol(self) -> str:
        return " >= "


class Greater(Relation):
    @property
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr > self._right.z3expr

    @property
    def _symbol(self) -> str:
        return " > "


class NotEqual(Relation):
    @property
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr != self._right.z3expr

    @property
    def _symbol(self) -> str:
        return " /= "


class Call(Expr):
    def __init__(
        self,
        identifier: StrID,
        *arguments: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._identifier = ID(identifier)
        self._arguments = list(arguments)
        self._preconditions: list[Cond] = []
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> Call:
        return self.__class__(
            self._identifier,
            *[a.substituted(mapping) for a in self._arguments],
            origin=self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return self._preconditions

    def set_preconditions(self, preconditions: list[Cond]) -> None:
        self._preconditions = preconditions

    def _update_str(self) -> None:
        self._str = intern(
            str(self._identifier)
            + (f" ({', '.join(str(a) for a in self._arguments)})" if self._arguments else "")
        )


class IntCall(Call, IntExpr):
    def __init__(
        self,
        identifier: StrID,
        *arguments: BasicExpr,
        negative: bool = False,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(identifier, *arguments)
        self._negative = negative
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> IntCall:
        return self.__class__(
            self._identifier,
            *[a.substituted(mapping) for a in self._arguments],
            negative=self._negative,
            origin=self._origin,
        )

    @property
    def z3expr(self) -> z3.ArithRef:
        # TODO: return value need not to be identical for identical arguments
        expression = z3.Int(str(self._identifier))
        return -expression if self._negative else expression

    def _update_str(self) -> None:
        self._str = intern(
            ("-" if self._negative else "")
            + str(self._identifier)
            + (f" ({', '.join(str(a) for a in self._arguments)})" if self._arguments else "")
        )


class BoolCall(Call, BoolExpr):
    @property
    def z3expr(self) -> z3.BoolRef:
        # TODO: return value need not to be identical for identical arguments
        return z3.Bool(str(self._identifier))


class ObjCall(Call):
    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class FieldAccess(Expr):
    def __init__(
        self,
        message: StrID,
        field: StrID,
        origin: Optional[Origin] = None,
    ) -> None:
        self._message = ID(message)
        self._field = ID(field)
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> FieldAccess:
        return self.__class__(
            mapping[self._message] if self._message in mapping else self._message,
            self._field,
            self._origin,
        )

    def _update_str(self) -> None:
        self._str = intern(f"{self._message}.{self._field}")


class IntFieldAccess(FieldAccess, IntExpr):
    def __init__(
        self,
        message: StrID,
        field: StrID,
        negative: bool = False,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(message, field, origin)
        self._negative = negative

    def substituted(self, mapping: Mapping[ID, ID]) -> IntFieldAccess:
        return self.__class__(
            mapping[self._message] if self._message in mapping else self._message,
            self._field,
            self._negative,
            self._origin,
        )

    @property
    def z3expr(self) -> z3.ArithRef:
        expression = z3.Int(str(self))
        return -expression if self._negative else expression

    def _update_str(self) -> None:
        sign = "-" if self._negative else ""
        self._str = intern(f"{sign}{self._message}.{self._field}")


class BoolFieldAccess(FieldAccess, BoolExpr):
    @property
    def z3expr(self) -> z3.BoolRef:
        return z3.Bool(str(self))


class ObjFieldAccess(FieldAccess):
    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError


class IfExpr(Expr):
    _condition: BasicBoolExpr
    _then_expr: BasicExpr
    _else_expr: BasicExpr

    def __init__(
        self,
        condition: BasicBoolExpr,
        then_expr: BasicExpr,
        else_expr: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._condition = condition
        self._then_expr = then_expr
        self._else_expr = else_expr
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> IfExpr:
        return self.__class__(
            self._condition.substituted(mapping),
            self._then_expr.substituted(mapping),
            self._else_expr.substituted(mapping),
            self._origin,
        )

    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.If(self._condition.z3expr, self._then_expr.z3expr, self._else_expr.z3expr)

    def _update_str(self) -> None:
        self._str = intern(f"(if {self._condition} then {self._then_expr} else {self._else_expr})")


class IntIfExpr(IfExpr):
    _condition: BasicBoolExpr
    _then_expr: BasicIntExpr
    _else_expr: BasicIntExpr

    def __init__(
        self,
        condition: BasicBoolExpr,
        then_expr: BasicIntExpr,
        else_expr: BasicIntExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(condition, then_expr, else_expr, origin)


class BoolIfExpr(IfExpr):
    _condition: BasicBoolExpr
    _then_expr: BasicBoolExpr
    _else_expr: BasicBoolExpr

    def __init__(
        self,
        condition: BasicBoolExpr,
        then_expr: BasicBoolExpr,
        else_expr: BasicBoolExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        super().__init__(condition, then_expr, else_expr, origin)


class Conversion(Expr):
    def __init__(
        self,
        identifier: StrID,
        argument: Expr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._identifier = ID(identifier)
        self._argument = argument
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> Conversion:
        return self.__class__(
            self._identifier,
            self._argument.substituted(mapping),
            self._origin,
        )

    @property
    def z3expr(self) -> z3.ExprRef:
        return z3.BoolVal(True)

    def _update_str(self) -> None:
        self._str = intern(f"{self._identifier} ({self._argument})")


class Comprehension(Expr):  # pylint: disable = too-many-instance-attributes
    def __init__(  # pylint: disable = too-many-arguments  # noqa: PLR0913
        self,
        iterator: StrID,
        sequence: BasicExpr,
        selector_stmts: list[Stmt],
        selector: BasicExpr,
        condition_stmts: list[Stmt],
        condition: BoolExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._iterator = ID(iterator)
        self._sequence = sequence
        self._selector_stmts = selector_stmts
        self._selector = selector
        self._condition_stmts = condition_stmts
        self._condition = condition
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> Comprehension:
        return self.__class__(
            self._iterator,
            self._sequence.substituted(mapping),
            [s.substituted(mapping) for s in self._selector_stmts],
            self._selector.substituted(mapping),
            [s.substituted(mapping) for s in self._condition_stmts],
            self._condition.substituted(mapping),
            self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        selector = str(self._selector)
        if self._selector_stmts:
            selector = f"{{{'; '.join(str(s) for s in self._selector_stmts)}; {selector}}}"
        condition = str(self._condition)
        if self._condition_stmts:
            condition = f"{{{'; '.join(str(s) for s in self._condition_stmts)}; {condition}}}"
        self._str = intern(
            f"[for {self._iterator} in {self._sequence} if {condition} => {selector}]"
        )


class Agg(Expr):
    def __init__(
        self,
        *elements: BasicExpr,
        origin: Optional[Origin] = None,
    ) -> None:
        self._elements = list(elements)
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> Agg:
        return self.__class__(
            *[e.substituted(mapping) for e in self._elements],
            origin=self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern("[" + ", ".join(map(str, self._elements)) + "]")


class Str(Expr):
    def __init__(self, string: str, origin: Optional[Origin] = None) -> None:
        self._string = string
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> Str:
        return self

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        self._str = intern(f'"{self._string}"')


class MsgAgg(Expr):
    def __init__(
        self,
        identifier: StrID,
        field_values: Mapping[ID, BasicExpr],
        origin: Optional[Origin] = None,
    ) -> None:
        self._identifier = ID(identifier)
        self._field_values = field_values or {}
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> MsgAgg:
        return self.__class__(
            self._identifier,
            {f: v.substituted(mapping) for f, v in self._field_values.items()},
            origin=self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        field_values = (
            " => ".join(f"{i} => {e}" for i, e in self._field_values.items())
            if self._field_values
            else "null message"
        )
        self._str = intern(f"{self._identifier}'({field_values})")


class DeltaMsgAgg(Expr):
    def __init__(
        self,
        identifier: StrID,
        field_values: Mapping[ID, BasicExpr],
        origin: Optional[Origin] = None,
    ) -> None:
        self._identifier = ID(identifier)
        self._field_values = field_values or {}
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> DeltaMsgAgg:
        return self.__class__(
            self._identifier,
            {f: v.substituted(mapping) for f, v in self._field_values.items()},
            origin=self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        field_values = (
            " => ".join(f"{i} => {e}" for i, e in self._field_values.items())
            if self._field_values
            else "null message"
        )
        self._str = intern(f"{self._identifier} with delta {field_values}")


class CaseExpr(Expr):
    def __init__(
        self,
        expression: BasicExpr,
        choices: Sequence[tuple[Sequence[BasicIntExpr], list[Stmt], BasicExpr]],
        origin: Optional[Origin] = None,
    ) -> None:
        self._expression = expression
        self._choices = choices
        self._origin = origin

    def substituted(self, mapping: Mapping[ID, ID]) -> CaseExpr:
        return self.__class__(
            self._expression.substituted(mapping),
            [
                (
                    [v.substituted(mapping) for v in vs],
                    [s.substituted(mapping) for s in s],
                    e.substituted(mapping),
                )
                for vs, s, e in self._choices
            ],
            origin=self._origin,
        )

    def preconditions(self, variable_id: Generator[ID, None, None]) -> list[Cond]:
        return []

    @property
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    def _update_str(self) -> None:
        data = ",\n".join(
            f"      when {' | '.join(map(str, c))} => "
            + (f"{{{'; '.join(str(s) for s in s)};}}; {e}" if s else str(e))
            for c, s, e in self._choices
        )
        self._str = intern(f"(case {self._expression} is\n{data})")


def add_checks(
    statements: list[Stmt], manager: ProofManager, variable_id: Generator[ID, None, None]
) -> list[Stmt]:
    """
    Add assert statements in places where preconditions are not statically true.

    For each statement it is checked, if its preconditions are statically true. If this is not the
    case, an assert statement is added in front of the respective statement. The assert statements
    in the resulting list mark the places where the code generator must insert explicit checks.
    """
    assert len({s.target for s in statements if isinstance(s, Assign)}) == len(
        [s.target for s in statements if isinstance(s, Assign)]
    ), "statements must be in SSA form"

    facts: list[Stmt] = []
    statement_precondition_count: list[tuple[Stmt, int]] = []

    for statement in statements:
        preconditions = statement.preconditions(variable_id)
        manager.add(
            [
                StmtListProofJob(
                    [
                        Assert(Not(BoolVar("__GOAL__"))),
                        Assign("__GOAL__", precondition.goal),
                        *precondition.facts,
                        *facts,
                    ],
                    {
                        ProofResult.UNSAT: [],
                        ProofResult.SAT: [*precondition.facts, Assert(precondition.goal)],
                        ProofResult.UNKNOWN: [*precondition.facts, Assert(precondition.goal)],
                    },
                )
                for precondition in preconditions
            ]
        )
        facts.append(statement)
        statement_precondition_count.append((statement, len(preconditions)))

    proof_results = manager.check()
    result = []

    for statement, precondition_count in statement_precondition_count:
        for _ in range(precondition_count):
            r = proof_results.pop(0)
            assert isinstance(r, StmtListProofJob)
            result.extend(r.result)
        result.append(statement)

    return result


def check_preconditions(
    statements: list[Stmt], manager: ProofManager, variable_id: Generator[ID, None, None]
) -> RecordFluxError:
    assert len({s.target for s in statements if isinstance(s, Assign)}) == len(
        [s.target for s in statements if isinstance(s, Assign)]
    ), "statements must be in SSA form"

    facts: list[Stmt] = []

    for s in statements:
        manager.add(
            [
                ErrorProofJob(
                    [
                        Assert(Not(BoolVar("__GOAL__"))),
                        Assign("__GOAL__", precondition.goal),
                        *precondition.facts,
                        *facts,
                    ],
                    {
                        ProofResult.UNSAT: RecordFluxError(),
                        ProofResult.SAT: RecordFluxError(
                            [
                                (
                                    "precondition might fail,"
                                    f" cannot prove {precondition.goal.origin_str}",
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    precondition.goal.location,
                                )
                            ]
                        ),
                        ProofResult.UNKNOWN: RecordFluxError(
                            [
                                (
                                    "precondition might fail,"
                                    f" cannot prove {precondition.goal.origin_str}"
                                    " (timeout)",
                                    Subsystem.MODEL,
                                    Severity.ERROR,
                                    precondition.goal.location,
                                )
                            ]
                        ),
                    },
                )
                for precondition in s.preconditions(variable_id)
            ]
        )
        facts.append(s)

    error = RecordFluxError()
    results = manager.check()

    for r in results:
        assert isinstance(r, ErrorProofJob), r
        error.extend(r.result)

    return error


def to_ssa(statements: list[Stmt], assigned: Optional[list[ID]] = None) -> list[Stmt]:
    """Transform the statements into Static Single-Assignment form."""
    occurrences = dict(Counter([s.target for s in statements if isinstance(s, Assign)]))
    assigned = assigned or []
    subs: dict[ID, ID] = {}
    result: list[Stmt] = []

    for s in statements:
        if isinstance(s, Assign):
            if occurrences[s.target] > 1:
                newtarget = ID(f"S_{s.target}_{assigned.count(s.target)}")
                assert newtarget not in assigned
                assigned.append(s.target)
                assigned.append(newtarget)
                result.append(Assign(newtarget, s.expression.substituted(subs), s.origin))
                subs[s.target] = newtarget
            else:
                assigned.append(s.target)
                result.append(s)
        else:
            result.append(s)

    return result
