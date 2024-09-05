from typing import Callable

import pytest
import z3

from rflx import typing_ as rty
from rflx.expr import (
    FALSE,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
    Call,
    Div,
    Equal,
    Expr,
    First,
    Greater,
    GreaterEqual,
    IfExpr,
    Last,
    Length,
    Less,
    LessEqual,
    Mod,
    Mul,
    Neg,
    Not,
    NotEqual,
    Number,
    Or,
    OrElse,
    Pow,
    Selected,
    Size,
    String,
    Sub,
    Variable,
)
from rflx.expr_proof import Proof, ProofResult, Z3TypeError, _to_z3
from rflx.rapidflux import Location
from tests.utils import assert_equal


def test_proof_invalid_logic() -> None:
    location = Location((1, 2))
    expr = Less(Mod(Variable("X"), Variable("Y")), Number(100), location=location)
    p = Proof(expr, logic="QF_IDL")
    assert p.result == ProofResult.UNKNOWN
    assert p.error == [
        (
            "Benchmark is not in QF_IDL (integer difference logic).",
            location,
        ),
    ]


def test_to_z3_true() -> None:
    assert _to_z3(TRUE) == z3.BoolVal(val=True)


def test_to_z3_false() -> None:
    assert _to_z3(FALSE) == z3.BoolVal(val=False)


def test_to_z3_not() -> None:
    assert _to_z3(Not(TRUE)) == z3.Not(z3.BoolVal(val=True))
    assert _to_z3(Not(FALSE)) == z3.Not(z3.BoolVal(val=False))
    with pytest.raises(Z3TypeError):
        _to_z3(Not(Variable("X")))


@pytest.mark.parametrize("expression", [And, AndThen, Or, OrElse])
def test_to_z3_bool_expr_error(expression: Callable[[Expr, Expr], Expr]) -> None:
    with pytest.raises(Z3TypeError):
        _to_z3(expression(Number(1), Number(2)))


def test_to_z3_and() -> None:
    assert_equal(
        _to_z3(And(TRUE, FALSE, TRUE)),
        z3.And(z3.BoolVal(val=True), z3.BoolVal(val=False), z3.BoolVal(val=True)),
    )
    assert_equal(
        _to_z3(And(TRUE, TRUE, TRUE)),
        z3.And(z3.BoolVal(val=True), z3.BoolVal(val=True), z3.BoolVal(val=True)),
    )
    assert_equal(
        _to_z3(And(TRUE, TRUE)),
        z3.And(z3.BoolVal(val=True), z3.BoolVal(val=True)),
    )


def test_to_z3_or() -> None:
    assert_equal(
        _to_z3(Or(TRUE, FALSE, TRUE)),
        z3.Or(z3.BoolVal(val=True), z3.BoolVal(val=False), z3.BoolVal(val=True)),
    )
    assert_equal(
        _to_z3(Or(TRUE, TRUE, TRUE)),
        z3.Or(z3.BoolVal(val=True), z3.BoolVal(val=True), z3.BoolVal(val=True)),
    )
    assert_equal(
        _to_z3(Or(TRUE, TRUE)),
        z3.Or(z3.BoolVal(val=True), z3.BoolVal(val=True)),
    )


@pytest.mark.parametrize("expression", [Add, Mul, Sub, Div, Pow, Mod])
def test_to_z3_math_expr_error(expression: Callable[[Expr, Expr], Expr]) -> None:
    with pytest.raises(Z3TypeError):
        _to_z3(expression(String("X"), TRUE))


def test_to_z3_neg() -> None:
    assert _to_z3(Neg(Variable("X"))) == -z3.Int("X")
    assert _to_z3(Neg(Number(42))) == -z3.IntVal(42)
    assert _to_z3(Neg(Number(-42))) == -z3.IntVal(-42)
    with pytest.raises(Z3TypeError):
        _to_z3(Neg(TRUE))


def test_to_z3_add() -> None:
    assert _to_z3(Add(Number(42), Number(1))) == z3.IntVal(42) + z3.IntVal(1)
    assert_equal(
        _to_z3(Add(Number(42), Number(1), Number(10))),
        z3.Sum(z3.IntVal(42), z3.IntVal(1), z3.IntVal(10)),
    )


def test_to_z3_mul() -> None:
    assert _to_z3(Mul(Number(6), Number(4))) == z3.IntVal(6) * z3.IntVal(4)
    assert_equal(
        _to_z3(Mul(Number(2), Number(4), Number(8))),
        z3.Product(z3.IntVal(2), z3.IntVal(4), z3.IntVal(8)),
    )


def test_to_z3_sub() -> None:
    assert _to_z3(Sub(Number(6), Number(4))) == z3.IntVal(6) - z3.IntVal(4)
    assert _to_z3(Sub(Number(12), Number(20))) == z3.IntVal(12) - z3.IntVal(20)


def test_to_z3_div() -> None:
    assert _to_z3(Div(Number(6), Number(3))) == z3.IntVal(6) / z3.IntVal(3)


def test_to_z3_pow() -> None:
    assert _to_z3(Pow(Number(6), Number(2))) == z3.IntVal(6) ** z3.IntVal(2)


def test_to_z3_mod() -> None:
    assert _to_z3(Mod(Number(1000), Number(5))) == z3.IntVal(1000) % z3.IntVal(5)
    assert _to_z3(Mod(Pow(Number(6), Number(2)), Number(5))) == z3.IntVal(36) % z3.IntVal(5)


def test_to_z3_mod_error() -> None:
    with pytest.raises(Z3TypeError):
        _to_z3(Mod(Pow(Variable("X"), Number(2)), Number(5)))


def test_to_z3_variable() -> None:
    assert _to_z3(Variable("X")) == z3.Int("X")
    assert _to_z3(Neg(Variable("X"))) == -z3.Int("X")
    assert z3.simplify(_to_z3(Sub(Variable("X"), Variable("X")))) == z3.IntVal(0)


@pytest.mark.parametrize(
    ("attribute", "z3name"),
    [
        (Size("X"), "X'Size"),
        (Length("X"), "X'Length"),
        (First("X"), "X'First"),
        (Last("X"), "X'Last"),
    ],
)
def test_to_z3_attribute(attribute: Expr, z3name: str) -> None:
    assert _to_z3(attribute) == z3.Int(z3name)
    assert _to_z3(-attribute) == -z3.Int(z3name)


def test_to_z3_attribute_error() -> None:
    with pytest.raises(Z3TypeError):
        _to_z3(First(Call("X", rty.BASE_INTEGER)))


def test_to_z3_aggregate() -> None:
    assert _to_z3(Aggregate(Number(1), Number(2))) == z3.Int("[1, 2]")


@pytest.mark.parametrize("relation", [Less, LessEqual, GreaterEqual, Greater])
def test_to_z3_relation_error(relation: Callable[[Expr, Expr], Expr]) -> None:
    with pytest.raises(Z3TypeError):
        _to_z3(relation(Variable("X", type_=rty.BOOLEAN), Number(1)))


def test_to_z3_less() -> None:
    assert _to_z3(Less(Number(1), Number(100))) == (z3.IntVal(1) < z3.IntVal(100))


def test_to_z3_less_equal() -> None:
    assert _to_z3(LessEqual(Number(1), Number(100))) == (z3.IntVal(1) <= z3.IntVal(100))


def test_to_z3_equal() -> None:
    assert _to_z3(Equal(Number(100), Number(100))) == (z3.IntVal(100) == z3.IntVal(100))


def test_to_z3_greater_equal() -> None:
    assert _to_z3(GreaterEqual(Number(100), Number(1))) == (z3.IntVal(100) >= z3.IntVal(1))


def test_to_z3_greater() -> None:
    assert _to_z3(Greater(Number(100), Number(1))) == (z3.IntVal(100) > z3.IntVal(1))


def test_to_z3_not_equal() -> None:
    assert _to_z3(NotEqual(Number(100), Number(1))) == (z3.IntVal(100) != z3.IntVal(1))


def test_to_z3_if_expr() -> None:
    assert _to_z3(IfExpr([(TRUE, Variable("Y"))], Variable("Z"))) == z3.If(
        z3.BoolVal(val=True),
        z3.Int("Y"),
        z3.Int("Z"),
    )
    with pytest.raises(Z3TypeError, match=r"^more than one condition$"):
        _to_z3(
            IfExpr(
                [(Variable("X"), Variable("Y")), (Variable("Y"), Variable("X"))],
                Variable("Z"),
            ),
        )
    with pytest.raises(Z3TypeError, match=r"^missing else expression$"):
        _to_z3(IfExpr([(Variable("X"), Variable("Y"))]))
    with pytest.raises(Z3TypeError, match=r"^non-boolean condition$"):
        _to_z3(IfExpr([(Variable("X"), Variable("Y"))], Variable("Z")))


def test_to_z3_number() -> None:
    assert _to_z3(Number(42)) == z3.IntVal(42)


def test_to_z3_selected() -> None:
    assert _to_z3(Selected(Variable("X"), "Y")) == z3.Int("X.Y")
    assert _to_z3(Neg(Selected(Variable("X"), "Y"))) == -z3.Int("X.Y")
