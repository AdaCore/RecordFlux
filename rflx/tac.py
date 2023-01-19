# pylint: disable = fixme

"""
Intermediate representation in three-address code (TAC) format.

This module is still under development (cf. Eng/RecordFlux/RecordFlux#1204).
"""

from __future__ import annotations

from abc import abstractmethod

import z3

from rflx.common import Base
from rflx.identifier import ID, StrID


class Stmt(Base):
    @abstractmethod
    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class Assign(Stmt):
    def __init__(self, target: StrID, expression: Expr) -> None:
        self._target = ID(target)
        self._expression = expression

    @property
    def target(self) -> ID:
        return self._target

    @property
    def expression(self) -> Expr:
        return self._expression

    def z3expr(self) -> z3.BoolRef:
        target = (
            IntVar(self._target) if isinstance(self._expression, IntExpr) else BoolVar(self._target)
        )
        return target.z3expr() == self._expression.z3expr()


class FieldAssign(Stmt):
    pass  # TODO


class Append(Stmt):
    pass  # TODO


class Extend(Stmt):
    pass  # TODO


class Reset(Stmt):
    pass  # TODO


class Read(Stmt):
    pass  # TODO


class Write(Stmt):
    pass  # TODO


class Assert(Stmt):
    def __init__(self, expression: BoolExpr) -> None:
        self._expression = expression

    def z3expr(self) -> z3.BoolRef:
        return self._expression.z3expr()


class Expr(Base):
    @abstractmethod
    def z3expr(self) -> z3.ExprRef:
        raise NotImplementedError

    @property
    def preconditions(self) -> list[Stmt]:
        return []


class BasicExpr(Expr):
    pass


class IntExpr(Expr):
    @abstractmethod
    def z3expr(self) -> z3.ArithRef:
        raise NotImplementedError


class BoolExpr(Expr):
    @abstractmethod
    def z3expr(self) -> z3.BoolRef:
        raise NotImplementedError


class BasicIntExpr(BasicExpr, IntExpr):
    pass


class BasicBoolExpr(BasicExpr, BoolExpr):
    pass


class Var(BasicExpr):
    def __init__(self, identifier: StrID) -> None:
        self._identifier = ID(identifier)

    @property
    def identifier(self) -> ID:
        return self._identifier


class IntVar(Var, BasicIntExpr):
    def __init__(self, identifier: StrID, negative: bool = False) -> None:
        super().__init__(identifier)
        self._negative = negative

    def z3expr(self) -> z3.ArithRef:
        expr = z3.Int(str(self._identifier))
        return -expr if self._negative else expr


class BoolVar(Var, BasicBoolExpr):
    def z3expr(self) -> z3.BoolRef:
        return z3.Bool(str(self._identifier))


class MsgVar(Var):
    def z3expr(self) -> z3.ExprRef:
        return z3.Const(str(self._identifier), z3.DeclareSort("Msg"))


class SeqVar(Var):
    def z3expr(self) -> z3.ExprRef:
        return z3.Const(str(self._identifier), z3.DeclareSort("Seq"))


class EnumLit(BasicIntExpr):
    def __init__(self, identifier: StrID) -> None:
        assert str(identifier) not in ("True", "False")
        self._identifier = ID(identifier)

    def z3expr(self) -> z3.ArithRef:
        return z3.Int(str(self._identifier))

    # TODO: return value of literal
    # def z3facts(self) -> list[z3.ExprRef]:
    #     pass


class IntVal(BasicIntExpr):
    def __init__(self, value: int) -> None:
        self._value = value

    def z3expr(self) -> z3.ArithRef:
        return z3.IntVal(self._value)


class BoolVal(BasicBoolExpr):
    def __init__(self, value: bool) -> None:
        self._value = value

    def z3expr(self) -> z3.BoolRef:
        return z3.BoolVal(self._value)


class UnaryExpr(Expr):
    def __init__(self, expression: BasicExpr) -> None:
        self._expression = expression


class UnaryBoolExpr(UnaryExpr, BoolExpr):
    def __init__(self, expression: BasicBoolExpr) -> None:
        super().__init__(expression)
        self._expression: BasicBoolExpr


class BinaryExpr(Expr):
    def __init__(self, left: BasicExpr, right: BasicExpr) -> None:
        self._left = left
        self._right = right


class BinaryIntExpr(BinaryExpr, IntExpr):
    def __init__(self, left: BasicIntExpr, right: BasicIntExpr) -> None:
        super().__init__(left, right)
        self._left: BasicIntExpr
        self._right: BasicIntExpr


class BinaryBoolExpr(BinaryExpr, BoolExpr):
    def __init__(self, left: BasicBoolExpr, right: BasicBoolExpr) -> None:
        super().__init__(left, right)
        self._left: BasicBoolExpr
        self._right: BasicBoolExpr


class Add(BinaryIntExpr):
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr() + self._right.z3expr()

    @property
    def preconditions(self) -> list[Stmt]:
        type_last = IntVar("Type'Last")  # TODO
        return [
            # Left + Right <= Type'Last
            Assign("D", Sub(type_last, self._right)),
            Assert(LessEqual(self._left, IntVar("D"))),
        ]


class Sub(BinaryIntExpr):
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr() - self._right.z3expr()

    @property
    def preconditions(self) -> list[Stmt]:
        type_first = IntVar("Type'First")  # TODO
        return [
            # Left - Right >= Type'First
            Assign("S", Add(type_first, self._right)),
            Assert(GreaterEqual(self._left, IntVar("S"))),
        ]


class Mul(BinaryIntExpr):
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr() * self._right.z3expr()

    @property
    def preconditions(self) -> list[Stmt]:
        type_last = IntVar("Type'Last")  # TODO
        return [
            # Left * Right <= Type'Last
            Assign("D", Div(type_last, self._right)),
            Assert(LessEqual(self._left, IntVar("D"))),
        ]


class Div(BinaryIntExpr):
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr() / self._right.z3expr()

    @property
    def preconditions(self) -> list[Stmt]:
        return [
            # Right /= 0
            Assert(NotEqual(self._right, IntVal(0))),
        ]


class Pow(BinaryIntExpr):
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr() ** self._right.z3expr()

    @property
    def preconditions(self) -> list[Stmt]:
        type_last = IntVar("Type'Last")  # TODO
        return [
            # Left ** Right <= Type'Last
            Assign("P", Pow(self._left, self._right)),
            Assert(LessEqual(IntVar("P"), type_last)),
        ]


class Mod(BinaryIntExpr):
    def z3expr(self) -> z3.ArithRef:
        return self._left.z3expr() % self._right.z3expr()

    @property
    def preconditions(self) -> list[Stmt]:
        return [
            # Right /= 0
            Assert(NotEqual(self._right, IntVal(0))),
        ]


class Not(UnaryBoolExpr):
    def z3expr(self) -> z3.BoolRef:
        return z3.Not(self._expression.z3expr())


class And(BinaryBoolExpr):
    def z3expr(self) -> z3.BoolRef:
        return z3.And(self._left.z3expr(), self._right.z3expr())


class Or(BinaryBoolExpr):
    def z3expr(self) -> z3.BoolRef:
        return z3.Or(self._left.z3expr(), self._right.z3expr())


class Relation(BoolExpr):
    def __init__(self, left: BasicIntExpr, right: BasicIntExpr) -> None:
        self._left = left
        self._right = right


class Less(Relation):
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr() < self._right.z3expr()


class LessEqual(Relation):
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr() <= self._right.z3expr()


class Equal(Relation):
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr() == self._right.z3expr()


class GreaterEqual(Relation):
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr() >= self._right.z3expr()


class Greater(Relation):
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr() > self._right.z3expr()


class NotEqual(Relation):
    def z3expr(self) -> z3.BoolRef:
        return self._left.z3expr() != self._right.z3expr()


# class QuantifiedExpr(Expr):
#     def __init__(self, parameter: StrID, iterable: BasicExpr, predicate: Expr) -> None:
#         self._parameter = ID(parameter)
#         self._iterable = iterable
#         self._predicate = predicate


# class ForAll(QuantifiedExpr):
#     pass  # TODO


# class ForSome(QuantifiedExpr):
#     pass  # TODO


class Call(Expr):
    def __init__(self, identifier: StrID, *arguments: BasicExpr) -> None:
        self._identifier = ID(identifier)
        self._arguments = list(arguments)
        self._preconditions: list[Stmt] = []

    @property
    def preconditions(self) -> list[Stmt]:
        return self._preconditions

    @preconditions.setter
    def preconditions(self, preconditions: list[Stmt]) -> None:
        self._preconditions = preconditions


class IntCall(Call, IntExpr):
    def __init__(self, identifier: StrID, *arguments: BasicExpr, negative: bool = False) -> None:
        super().__init__(identifier, *arguments)
        self._negative = negative

    def z3expr(self) -> z3.ArithRef:
        # TODO: consider non-idempotent calls
        expr = z3.Int(str(self._identifier))
        return -expr if self._negative else expr


class BoolCall(Call, BoolExpr):
    def z3expr(self) -> z3.BoolRef:
        # TODO: consider non-idempotent calls
        return z3.Bool(str(self._identifier))


class IntFieldAccess(IntExpr):
    def __init__(self, message: StrID, field: StrID, negative: bool = False) -> None:
        self._message = ID(message)
        self._field = ID(field)
        self._negative = negative

    def z3expr(self) -> z3.ArithRef:
        expr = z3.Int(f"{self._message}.{self._field}")
        return -expr if self._negative else expr


class BoolFieldAccess(BoolExpr):
    def __init__(self, message: StrID, field: StrID) -> None:
        self._message = ID(message)
        self._field = ID(field)

    def z3expr(self) -> z3.BoolRef:
        return z3.Bool(f"{self._message}.{self._field}")


class IntIfExpr(Expr):
    def __init__(
        self, condition: BasicBoolExpr, then_expr: BasicIntExpr, else_expr: BasicIntExpr
    ) -> None:
        self._condition = condition
        self._then_expr = then_expr
        self._else_expr = else_expr

    def z3expr(self) -> z3.ExprRef:
        return z3.If(self._condition.z3expr(), self._then_expr.z3expr(), self._else_expr.z3expr())


class BoolIfExpr(Expr):
    def __init__(
        self, condition: BasicBoolExpr, then_expr: BasicBoolExpr, else_expr: BasicBoolExpr
    ) -> None:
        self._condition = condition
        self._then_expr = then_expr
        self._else_expr = else_expr

    def z3expr(self) -> z3.ExprRef:
        return z3.If(self._condition.z3expr(), self._then_expr.z3expr(), self._else_expr.z3expr())
