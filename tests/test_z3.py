import unittest

import z3

from rflx.expression import (
    FALSE,
    TRUE,
    Add,
    And,
    Case,
    Div,
    Equal,
    Greater,
    GreaterEqual,
    If,
    Less,
    LessEqual,
    Mod,
    Mul,
    Not,
    NotEqual,
    Number,
    Or,
    Pow,
    Sub,
    Variable,
)


class TestZ3(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_true(self) -> None:
        self.assertEqual(TRUE.z3expr(), z3.BoolVal(True))

    def test_false(self) -> None:
        self.assertEqual(FALSE.z3expr(), z3.BoolVal(False))

    def test_not(self) -> None:
        self.assertEqual(Not(TRUE).z3expr(), z3.Not(z3.BoolVal(True)))
        self.assertEqual(Not(FALSE).z3expr(), z3.Not(z3.BoolVal(False)))

    def test_and(self) -> None:
        self.assertEqual(
            And(TRUE, FALSE, TRUE).z3expr(),
            z3.And(z3.BoolVal(True), z3.BoolVal(False), z3.BoolVal(True)),
        )
        self.assertEqual(
            And(TRUE, TRUE, TRUE).z3expr(),
            z3.And(z3.BoolVal(True), z3.BoolVal(True), z3.BoolVal(True)),
        )
        self.assertEqual(And(TRUE, TRUE).z3expr(), z3.And(z3.BoolVal(True), z3.BoolVal(True)))

    def test_or(self) -> None:
        self.assertEqual(
            Or(TRUE, FALSE, TRUE).z3expr(),
            z3.Or(z3.BoolVal(True), z3.BoolVal(False), z3.BoolVal(True)),
        )
        self.assertEqual(
            Or(TRUE, TRUE, TRUE).z3expr(),
            z3.Or(z3.BoolVal(True), z3.BoolVal(True), z3.BoolVal(True)),
        )
        self.assertEqual(Or(TRUE, TRUE).z3expr(), z3.Or(z3.BoolVal(True), z3.BoolVal(True)))

    def test_int(self) -> None:
        self.assertEqual(Number(42).z3expr(), z3.IntVal(42))

    def test_add(self) -> None:
        self.assertEqual(
            Add(Number(42), Number(1)).z3expr(), z3.IntVal(0) + z3.IntVal(42) + z3.IntVal(1)
        )
        self.assertEqual(
            Add(Number(42), Number(1), Number(10)).z3expr(),
            z3.IntVal(0) + z3.IntVal(42) + z3.IntVal(1) + z3.IntVal(10),
        )

    def test_mul(self) -> None:
        self.assertEqual(Mul(Number(6), Number(4)).z3expr(), z3.IntVal(6) * z3.IntVal(4))
        self.assertEqual(
            Mul(Number(2), Number(4), Number(8)).z3expr(),
            z3.IntVal(2) * z3.IntVal(4) * z3.IntVal(8),
        )

    def test_substituted(self) -> None:
        self.assertEqual(Sub(Number(6), Number(4)).z3expr(), z3.IntVal(6) - z3.IntVal(4))
        self.assertEqual(Sub(Number(12), Number(20)).z3expr(), z3.IntVal(12) - z3.IntVal(20))

    def test_div(self) -> None:
        self.assertEqual(Div(Number(6), Number(3)).z3expr(), z3.IntVal(6) / z3.IntVal(3))

    def test_pow(self) -> None:
        self.assertEqual(Pow(Number(6), Number(2)).z3expr(), z3.IntVal(6) ** z3.IntVal(2))

    def test_mod(self) -> None:
        self.assertEqual(Mod(Number(1000), Number(5)).z3expr(), z3.IntVal(1000) % z3.IntVal(5))

    def test_variable(self) -> None:
        self.assertEqual(Variable("RecordFlux").z3expr(), z3.Int("RecordFlux"))
        self.assertEqual(z3.simplify(Sub(Variable("foo"), Variable("foo")).z3expr()), z3.IntVal(0))

    def test_less_than(self) -> None:
        self.assertEqual(Less(Number(1), Number(100)).z3expr(), z3.IntVal(1) < z3.IntVal(100))

    def test_less_equal(self) -> None:
        self.assertEqual(LessEqual(Number(1), Number(100)).z3expr(), z3.IntVal(1) <= z3.IntVal(100))

    def test_equal(self) -> None:
        self.assertEqual(Equal(Number(100), Number(100)).z3expr(), z3.IntVal(100) == z3.IntVal(100))

    def test_greater_equal(self) -> None:
        self.assertEqual(
            GreaterEqual(Number(100), Number(1)).z3expr(), z3.IntVal(100) >= z3.IntVal(1)
        )

    def test_greater(self) -> None:
        self.assertEqual(Greater(Number(100), Number(1)).z3expr(), z3.IntVal(100) > z3.IntVal(1))

    def test_not_equal(self) -> None:
        self.assertEqual(NotEqual(Number(100), Number(1)).z3expr(), z3.IntVal(100) != z3.IntVal(1))

    def test_if(self) -> None:
        self.assertEqual(
            If(
                [
                    (Greater(Variable("a"), Number(5)), Number(1)),
                    (Greater(Variable("b"), Number(100)), Number(10)),
                ],
                Number(100),
            ).z3expr(),
            z3.If(
                z3.Int("a") > z3.IntVal(5),
                z3.IntVal(1),
                z3.If(z3.Int("b") > z3.IntVal(100), z3.IntVal(10), z3.IntVal(100)),
            ),
        )

    def test_case(self) -> None:
        self.assertEqual(
            Case(Variable("x"), [(Number(5), Number(1)), (Number(10), Number(2))]).z3expr(),
            z3.If(
                z3.Int("x") == z3.IntVal(5),
                z3.IntVal(1),
                z3.If(z3.Int("x") == z3.IntVal(10), z3.IntVal(2), z3.BoolVal(False)),
            ),
        )
