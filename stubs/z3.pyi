#  pylint: disable=pointless-statement,unused-argument,invalid-name,no-self-use
from typing import Iterable, Optional

class Context:  # noqa: E302, conflicts with isort
    ...


class ExprRef:
    def __eq__(self, other: 'ExprRef') -> 'BoolRef':  # type: ignore
        ...

    def __ne__(self, other: 'ExprRef') -> 'BoolRef':  # type: ignore
        ...


class BoolRef(ExprRef):
    ...


class ArithRef(ExprRef):
    def __sub__(self, other: 'ArithRef') -> 'ArithRef':
        ...

    def __add__(self, other: 'ArithRef') -> 'ArithRef':
        ...

    def __mul__(self, other: 'ArithRef') -> 'ArithRef':
        ...

    def __pow__(self, other: 'ArithRef') -> 'ArithRef':
        ...

    def __mod__(self, other: 'ArithRef') -> 'ArithRef':
        ...

    def __truediv__(self, other: 'ArithRef') -> 'ArithRef':
        ...

    def __gt__(self, other: 'ArithRef') -> BoolRef:
        ...

    def __ge__(self, other: 'ArithRef') -> BoolRef:
        ...

    def __lt__(self, other: 'ArithRef') -> BoolRef:
        ...

    def __le__(self, other: 'ArithRef') -> BoolRef:
        ...

    def __neg__(self) -> 'ArithRef':
        ...


def Int(name: str, ctx: Optional[Context] = None) -> ArithRef:
    ...


def IntVal(val: int, ctx: Optional[Context] = None) -> ArithRef:
    ...


def Bool(name: str, ctx: Optional[Context] = None) -> BoolRef:
    ...


def BoolVal(val: bool, ctx: Optional[Context] = None) -> BoolRef:
    ...


def Not(val: BoolRef, ctx: Optional[Context] = None) -> BoolRef:
    ...


def And(*args: BoolRef) -> BoolRef:
    ...


def Or(*args: BoolRef) -> BoolRef:
    ...


def If(c: BoolRef, t: ExprRef, e: ExprRef, ctx: Optional[Context] = None) -> ExprRef:
    ...


def ForAll(v: Iterable[ExprRef], cond: ExprRef) -> ExprRef:
    ...


def simplify(e: ExprRef) -> ExprRef:
    ...


class CheckSatResult:
    ...


sat = CheckSatResult()
unsat = CheckSatResult()
unknown = CheckSatResult()


class Solver:
    def add(self, *expr: ExprRef) -> None:
        ...

    def check(self, *asns: ExprRef) -> CheckSatResult:
        ...
