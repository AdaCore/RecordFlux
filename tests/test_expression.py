# pylint: disable=too-many-lines

import unittest

import z3

from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AndThen,
    Attribute,
    Call,
    Case,
    Constrained,
    Div,
    Equal,
    First,
    ForAllIn,
    ForAllOf,
    Greater,
    GreaterEqual,
    If,
    In,
    Last,
    Length,
    Less,
    LessEqual,
    Mod,
    Mul,
    NamedAggregate,
    Not,
    NotEqual,
    NotIn,
    Number,
    Old,
    Or,
    OrElse,
    Pos,
    Pow,
    Range,
    Result,
    Size,
    Slice,
    Sub,
    Val,
    ValueRange,
    Variable,
)
from rflx.identifier import ID

EXPR = Equal(Variable("UNDEFINED_1"), Variable("UNDEFINED_2"))


def multilinestr(string: str) -> str:
    assert all(
        l.startswith(19 * " ") for l in string.split("\n")[1:]
    ), "invalid format of multi-line string"
    return string.replace(19 * " ", "")


class TestExpression(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_true_neg(self) -> None:
        self.assertEqual(-TRUE, FALSE)

    def test_true_simplified(self) -> None:
        self.assertEqual(TRUE.simplified(), TRUE)

    def test_true_variables(self) -> None:
        self.assertEqual(TRUE.variables(), [])

    def test_true_z3expr(self) -> None:
        self.assertEqual(TRUE.z3expr(), z3.BoolVal(True))

    def test_false_neg(self) -> None:
        self.assertEqual(-FALSE, TRUE)

    def test_false_simplified(self) -> None:
        self.assertEqual(FALSE.simplified(), FALSE)

    def test_false_variables(self) -> None:
        self.assertEqual(FALSE.variables(), [])

    def test_false_z3expr(self) -> None:
        self.assertEqual(FALSE.z3expr(), z3.BoolVal(False))

    def test_not_neg(self) -> None:
        self.assertEqual(-Not(Variable("X")), Variable("X"))

    def test_not_z3expr(self) -> None:
        self.assertEqual(Not(TRUE).z3expr(), z3.Not(z3.BoolVal(True)))
        with self.assertRaises(TypeError):
            Not(Variable("X")).z3expr()

    def test_bin_expr_findall(self) -> None:
        self.assertEqual(
            Less(Variable("X"), Number(1)).findall(lambda x: isinstance(x, Number)), [Number(1)],
        )

    def test_bin_expr_substituted(self) -> None:
        self.assertEqual(
            Less(Variable("X"), Number(1)).substituted(
                lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
            ),
            Less(Variable("P_X"), Number(1)),
        )
        self.assertEqual(
            Sub(Variable("X"), Number(1)).substituted(
                lambda x: Variable("Y") if x == Sub(Variable("X"), Number(1)) else x
            ),
            Variable("Y"),
        )
        self.assertEqual(
            NotEqual(Variable("X"), Number(1)).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Equal(x.left, x.right) if isinstance(x, NotEqual) else x)
            ),
            Equal(Variable("P_X"), Number(1)),
        )

    def test_ass_expr_findall(self) -> None:
        self.assertEqual(
            And(Equal(Variable("X"), Number(1)), Variable("Y"), Number(2)).findall(
                lambda x: isinstance(x, Number)
            ),
            [Number(1), Number(2)],
        )

    def test_ass_expr_substituted(self) -> None:
        self.assertEqual(
            And(Equal(Variable("X"), Number(1)), Variable("Y")).substituted(
                lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
            ),
            And(Equal(Variable("P_X"), Number(1)), Variable("P_Y")),
        )
        self.assertEqual(
            Mul(Variable("X"), Number(1)).substituted(
                lambda x: Variable("Y") if x == Mul(Variable("X"), Number(1)) else x
            ),
            Variable("Y"),
        )
        self.assertEqual(
            And(Equal(Variable("X"), Number(1)), Variable("Y")).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Or(*x.terms) if isinstance(x, And) else x)
            ),
            Or(Equal(Variable("P_X"), Number(1)), Variable("P_Y")),
        )

    def test_log_expr_str(self) -> None:
        self.assertEqual(
            str(And(Variable("A"), Or(Variable("B"), Variable("C")), Variable("D"))),
            multilinestr(
                """A
                   and (B
                        or C)
                   and D"""
            ),
        )
        self.assertEqual(
            str(AndThen(Variable("A"), OrElse(Variable("B"), Variable("C")), Variable("D"))),
            multilinestr(
                """A
                   and then (B
                             or else C)
                   and then D"""
            ),
        )

    def test_and_neg(self) -> None:
        self.assertEqual(-And(Variable("X"), Number(1)), And(-Variable("X"), Number(-1)))

    def test_and_variables(self) -> None:
        self.assertEqual(
            And(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_and_contains(self) -> None:
        self.assertTrue(Variable("X") in And(TRUE, Less(Variable("X"), Number(42))))
        self.assertFalse(Variable("Y") in And(TRUE, Less(Variable("X"), Number(42))))

    def test_and_simplified(self) -> None:
        self.assertEqual(And(TRUE, TRUE).simplified(), TRUE)
        self.assertEqual(And(TRUE, FALSE, TRUE).simplified(), FALSE)
        self.assertEqual(And(TRUE, EXPR).simplified(), EXPR)
        self.assertEqual(And(EXPR, TRUE).simplified(), EXPR)
        self.assertEqual(And(EXPR, FALSE).simplified(), FALSE)
        self.assertEqual(And(FALSE, EXPR).simplified(), FALSE)

    def test_and_str(self) -> None:
        self.assertEqual(str(And(Variable("X"), Variable("Y"))), "X\nand Y")
        self.assertEqual(str(And()), "True")

    def test_or_neg(self) -> None:
        self.assertEqual(-Or(Variable("X"), Number(1)), Or(-Variable("X"), Number(-1)))

    def test_or_variables(self) -> None:
        self.assertEqual(
            Or(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_or_contains(self) -> None:
        self.assertTrue(Variable("X") in Or(Less(Variable("X"), Number(42)), TRUE))
        self.assertFalse(Variable("Y") in Or(Less(Variable("X"), Number(42)), TRUE))

    def test_or_simplified(self) -> None:
        self.assertEqual(Or(TRUE, TRUE).simplified(), TRUE)
        self.assertEqual(Or(TRUE, EXPR).simplified(), TRUE)
        self.assertEqual(Or(EXPR, TRUE).simplified(), TRUE)

    def test_or_str(self) -> None:
        self.assertEqual(str(Or(Variable("X"), Variable("Y"))), "X\nor Y")
        self.assertEqual(str(Or()), "True")

    def test_undefined_neg(self) -> None:
        self.assertEqual(-UNDEFINED, -UNDEFINED)

    def test_undefined_simplified(self) -> None:
        self.assertEqual(UNDEFINED.simplified(), UNDEFINED)

    def test_undefined_str(self) -> None:
        self.assertEqual(str(UNDEFINED), "__UNDEFINED__")

    def test_number_neg(self) -> None:
        self.assertEqual(-Number(42), Number(-42))

    def test_number_simplified(self) -> None:
        self.assertEqual(Number(42).simplified(), Number(42))

    def test_number_add(self) -> None:
        self.assertEqual(Number(5) + Number(3), Number(8))

    def test_number_substituted(self) -> None:
        self.assertEqual(Number(5) - Number(3), Number(2))

    def test_number_mul(self) -> None:
        self.assertEqual(Number(4) * Number(2), Number(8))

    def test_number_div(self) -> None:
        self.assertEqual(Number(4) // Number(2), Number(2))

    def test_number_pow(self) -> None:
        self.assertEqual(Number(2) ** Number(4), Number(16))

    def test_number_eq(self) -> None:
        self.assertTrue(Number(1) == Number(1))
        self.assertTrue(Number(1, 10) == Number(1, 16))
        self.assertTrue(Number(42, 16) == Number(42, 10))
        self.assertFalse(Number(1) == Number(2))
        self.assertFalse(Number(1, 16) == Number(2, 16))

    def test_number_ne(self) -> None:
        self.assertFalse(Number(1) != Number(1))
        self.assertFalse(Number(1, 10) != Number(1, 16))
        self.assertFalse(Number(42, 16) != Number(42, 10))
        self.assertTrue(Number(1) != Number(2))
        self.assertTrue(Number(1, 16) != Number(2, 16))

    def test_number_lt(self) -> None:
        self.assertEqual(Number(1) < Number(2), True)
        self.assertEqual(Number(2) < Number(2), False)
        self.assertEqual(Number(3) < Number(2), False)
        self.assertEqual(Variable("X") < Number(2), False)
        self.assertEqual(Number(2) < Variable("X"), False)

    def test_number_le(self) -> None:
        self.assertEqual(Number(1) <= Number(2), True)
        self.assertEqual(Number(2) <= Number(2), True)
        self.assertEqual(Number(3) <= Number(2), False)
        self.assertEqual(Variable("X") <= Number(2), False)
        self.assertEqual(Number(2) <= Variable("X"), False)

    def test_number_gt(self) -> None:
        self.assertEqual(Number(1) > Number(2), False)
        self.assertEqual(Number(2) > Number(2), False)
        self.assertEqual(Number(3) > Number(2), True)
        self.assertEqual(Variable("X") > Number(2), False)
        self.assertEqual(Number(2) > Variable("X"), False)

    def test_number_ge(self) -> None:
        self.assertEqual(Number(1) >= Number(2), False)
        self.assertEqual(Number(2) >= Number(2), True)
        self.assertEqual(Number(3) >= Number(2), True)
        self.assertEqual(Variable("X") >= Number(2), False)
        self.assertEqual(Number(2) >= Variable("X"), False)

    def test_add_neg(self) -> None:
        self.assertEqual(-Add(Variable("X"), Number(1)), Add(Variable("X", True), Number(-1)))

    def test_add_variables(self) -> None:
        self.assertEqual(
            Add(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_add_simplified(self) -> None:
        self.assertEqual(Add(Variable("X"), Number(1)).simplified(), Add(Variable("X"), Number(1)))
        self.assertEqual(Add(Variable("X"), Number(0)).simplified(), Variable("X"))
        self.assertEqual(Add(Number(2), Number(3), Number(5)).simplified(), Number(10))
        self.assertEqual(
            Add(Variable("X"), Variable("Y"), Variable("X", True)).simplified(), Variable("Y")
        )
        self.assertEqual(
            Add(Variable("X"), Variable("Y"), Variable("X"), -Variable("X")).simplified(),
            Add(Variable("X"), Variable("Y")),
        )

    def test_add_lt(self) -> None:
        self.assertEqual(Add(Variable("X"), Number(1)) < Add(Variable("X"), Number(2)), True)
        self.assertEqual(Add(Variable("X"), Number(2)) < Add(Variable("X"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(3)) < Add(Variable("X"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(1)) < Add(Variable("Y"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(2)) < Add(Variable("Y"), Number(1)), False)
        self.assertEqual(
            Add(Variable("X"), Number(2)) < Add(Variable("Y"), Variable("Z"), Number(1)), False
        )

    def test_add_le(self) -> None:
        self.assertEqual(Add(Variable("X"), Number(1)) <= Add(Variable("X"), Number(2)), True)
        self.assertEqual(Add(Variable("X"), Number(2)) <= Add(Variable("X"), Number(2)), True)
        self.assertEqual(Add(Variable("X"), Number(3)) <= Add(Variable("X"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(1)) <= Add(Variable("Y"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(2)) <= Add(Variable("Y"), Number(1)), False)
        self.assertEqual(
            Add(Variable("X"), Number(2)) <= Add(Variable("Y"), Variable("Z"), Number(1)), False
        )

    def test_add_gt(self) -> None:
        self.assertEqual(Add(Variable("X"), Number(1)) > Add(Variable("X"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(2)) > Add(Variable("X"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(3)) > Add(Variable("X"), Number(2)), True)
        self.assertEqual(Add(Variable("X"), Number(1)) > Add(Variable("Y"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(2)) > Add(Variable("Y"), Number(1)), False)
        self.assertEqual(
            Add(Variable("X"), Number(2)) > Add(Variable("Y"), Variable("Z"), Number(1)), False
        )

    def test_add_ge(self) -> None:
        self.assertEqual(Add(Variable("X"), Number(1)) >= Add(Variable("X"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(2)) >= Add(Variable("X"), Number(2)), True)
        self.assertEqual(Add(Variable("X"), Number(3)) >= Add(Variable("X"), Number(2)), True)
        self.assertEqual(Add(Variable("X"), Number(1)) >= Add(Variable("Y"), Number(2)), False)
        self.assertEqual(Add(Variable("X"), Number(2)) >= Add(Variable("Y"), Number(1)), False)
        self.assertEqual(
            Add(Variable("X"), Number(2)) >= Add(Variable("Y"), Variable("Z"), Number(1)), False
        )

    def test_mul_neg(self) -> None:
        self.assertEqual(-Mul(Variable("X"), Number(2)), Mul(Variable("X"), Number(-2)))

    def test_mul_variables(self) -> None:
        self.assertEqual(
            Mul(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_mul_simplified(self) -> None:
        self.assertEqual(Mul(Variable("X"), Number(2)).simplified(), Mul(Variable("X"), Number(2)))
        self.assertEqual(Mul(Variable("X"), Number(1)).simplified(), Variable("X"))
        self.assertEqual(Mul(Number(2), Number(3), Number(5)).simplified(), Number(30))

    def test_sub_neg(self) -> None:
        self.assertEqual(-Sub(Number(1), Variable("X")), Sub(Number(-1), Variable("X")))

    def test_sub_variables(self) -> None:
        self.assertEqual(Sub(Variable("X"), Call("Y")).variables(), [Variable("X")])

    def test_sub_simplified(self) -> None:
        self.assertEqual(Sub(Number(1), Variable("X")).simplified(), Add(Number(1), -Variable("X")))
        self.assertEqual(Sub(Variable("X"), Number(1)).simplified(), Add(Variable("X"), Number(-1)))
        self.assertEqual(Sub(Number(6), Number(2)).simplified(), Number(4))
        self.assertEqual(
            Sub(Variable("X"), Variable("Y")).simplified(), Add(Variable("X"), -Variable("Y"))
        )
        self.assertTrue(
            Equal(Variable("Q"), Sub(Add(Variable("Y"), Variable("Q")), Variable("Y"))).simplified()
        )

    def test_div_neg(self) -> None:
        self.assertEqual(-Div(Variable("X"), Number(1)), Div(Variable("X", True), Number(1)))

    def test_div_variables(self) -> None:
        self.assertEqual(Div(Variable("X"), Call("Y")).variables(), [Variable("X")])
        self.assertEqual(
            Div(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_div_simplified(self) -> None:
        self.assertEqual(Div(Variable("X"), Number(1)).simplified(), Div(Variable("X"), Number(1)))
        self.assertEqual(Div(Number(6), Number(2)).simplified(), Number(3))
        self.assertEqual(Div(Number(9), Number(2)).simplified(), Div(Number(9), Number(2)))

    def test_pow_simplified(self) -> None:
        self.assertEqual(Pow(Variable("X"), Number(1)).simplified(), Pow(Variable("X"), Number(1)))
        self.assertEqual(
            Pow(Variable("X"), Add(Number(1), Number(1))).simplified(),
            Pow(Variable("X"), Number(2)),
        )
        self.assertEqual(Pow(Number(6), Number(2)).simplified(), Number(36))

    def test_pow_variables(self) -> None:
        self.assertEqual(
            Pow(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_mod_simplified(self) -> None:
        self.assertEqual(Mod(Variable("X"), Number(1)).simplified(), Mod(Variable("X"), Number(1)))
        self.assertEqual(
            Mod(Variable("X"), Add(Number(1), Number(1))).simplified(),
            Mod(Variable("X"), Number(2)),
        )
        self.assertEqual(Mod(Number(6), Number(2)).simplified(), Number(0))

    def test_mod_variables(self) -> None:
        self.assertEqual(
            Mod(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_term_simplified(self) -> None:
        self.assertEqual(
            Add(
                Mul(Number(1), Number(6)), Sub(Variable("X"), Number(10)), Add(Number(1), Number(3))
            ).simplified(),
            Variable("X"),
        )

    def test_variable_invalid_name(self) -> None:
        with self.assertRaises(AssertionError):
            Variable("Foo (Bar)")

    def test_variable_neg(self) -> None:
        self.assertEqual(-Variable("X"), Variable("X", True))

    def test_variable_neg_variables(self) -> None:
        self.assertEqual((-Variable("X")).variables(), [Variable("X", True)])

    def test_variable_substituted(self) -> None:
        self.assertEqual(
            Variable("X").substituted(lambda x: Number(42) if x == Variable("X") else x), Number(42)
        )

    def test_variable_simplified(self) -> None:
        self.assertEqual(Variable("X").simplified(), Variable("X"))

    def test_variable_z3expr(self) -> None:
        self.assertEqual(Variable("X").z3expr(), z3.Int("X"))
        self.assertEqual(Variable("X", True).z3expr(), -z3.Int("X"))

    def test_attribute(self) -> None:
        self.assertTrue(isinstance(Size("X"), Attribute))
        self.assertTrue(isinstance(Length("X"), Attribute))
        self.assertTrue(isinstance(First("X"), Attribute))
        self.assertTrue(isinstance(Last("X"), Attribute))
        self.assertTrue(isinstance(Range("X"), Attribute))
        self.assertTrue(isinstance(Old("X"), Attribute))
        self.assertTrue(isinstance(Result("X"), Attribute))
        self.assertTrue(isinstance(Constrained("X"), Attribute))
        self.assertEqual(First("X"), First(Variable("X")))
        self.assertEqual(First("X"), First(ID("X")))
        self.assertEqual(First("X"), First(Variable(ID("X"))))

    def test_attribute_neg(self) -> None:
        self.assertEqual(-First("X"), First("X", True))

    def test_attribute_substituted(self) -> None:
        self.assertEqual(
            First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(42)
        )
        self.assertEqual(
            -First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(-42)
        )
        self.assertEqual(
            First("X").substituted(lambda x: Call("Y") if x == Variable("X") else x),
            First(Call("Y")),
        )
        self.assertEqual(
            -First("X").substituted(lambda x: Call("Y") if x == Variable("X") else x),
            -First(Call("Y")),
        )
        self.assertEqual(
            -First("X").substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Last(x.prefix) if isinstance(x, First) else x)
            ),
            -Last(Variable("P_X")),
        )

    def test_attribute_simplified(self) -> None:
        self.assertEqual(First("X").simplified(), First("X"))

    def test_attribute_str(self) -> None:
        self.assertEqual(str(First("X")), "X'First")

    def test_attribute_variables(self) -> None:
        self.assertEqual(First("X").variables(), [Variable("X")])
        self.assertEqual(First("X").variables(proof=True), [Variable("X'First")])
        self.assertEqual(First(Call("X")).variables(), [])
        with self.assertRaises(TypeError):
            First(Call("X")).variables(proof=True)

    def test_attribute_z3expr(self) -> None:
        self.assertEqual(First("X").z3expr(), z3.Int("X'First"))
        with self.assertRaises(TypeError):
            First(Call("X")).z3expr()

    def test_attribute_expression_substituted(self) -> None:
        self.assertEqual(
            Val("X", Variable("Y")).substituted(
                lambda x: Number(42) if x == Val("X", Variable("Y")) else x
            ),
            Number(42),
        )
        self.assertEqual(
            -Val("X", Variable("Y")).substituted(
                lambda x: Number(42) if x == Val("X", Variable("Y")) else x
            ),
            Number(-42),
        )
        self.assertEqual(
            Val("X", Variable("Y")).substituted(lambda x: Call("Y") if x == Variable("Y") else x),
            Val("X", Call("Y")),
        )
        self.assertEqual(
            -Val("X", Variable("Y")).substituted(lambda x: Call("Y") if x == Variable("Y") else x),
            -Val("X", Call("Y")),
        )
        self.assertEqual(
            -Val("X", Variable("Y")).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Pos(x.prefix, x.expression) if isinstance(x, Val) else x)
            ),
            -Pos("P_X", Variable("P_Y")),
        )

    def test_attribute_expression_simplified(self) -> None:
        self.assertEqual(Val("X", Add(Number(1), Number(1))).simplified(), Val("X", Number(2)))

    def test_attribute_expression_str(self) -> None:
        self.assertEqual(str(Val("X", Number(1))), "X'Val (1)")

    def test_aggregate_substituted(self) -> None:
        self.assertEqual(
            Aggregate(First("X")).substituted(
                lambda x: Number(42) if x == Aggregate(First("X")) else x
            ),
            Number(42),
        )
        self.assertEqual(
            Aggregate(First("X")).substituted(lambda x: Number(42) if x == First("X") else x),
            Aggregate(Number(42)),
        )
        self.assertEqual(
            Aggregate(Variable("X")).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Aggregate(*(x.elements + [Variable("Y")])) if isinstance(x, Aggregate) else x)
            ),
            Aggregate(Variable("P_X"), Variable("P_Y")),
        )

    def test_aggregate_simplified(self) -> None:
        self.assertEqual(
            Aggregate(Add(Number(1), Number(1))).simplified(), Aggregate(Number(2)),
        )

    def test_named_aggregate_substituted(self) -> None:
        self.assertEqual(
            NamedAggregate(("First", First("X"))).substituted(
                lambda x: Number(42) if x == NamedAggregate(("First", First("X"))) else x
            ),
            Number(42),
        )
        self.assertEqual(
            NamedAggregate(("First", First("X"))).substituted(
                lambda x: Number(42) if x == First("X") else x
            ),
            NamedAggregate(("First", Number(42))),
        )
        self.assertEqual(
            NamedAggregate(("First", First("X"))).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (
                    NamedAggregate(*[*x.elements, (ID("Last"), Last("Y"))])
                    if isinstance(x, NamedAggregate)
                    else x
                )
            ),
            NamedAggregate(("First", First("P_X")), ("Last", Last("P_Y"))),
        )

    def test_named_aggregate_simplified(self) -> None:
        self.assertEqual(
            NamedAggregate(("First", Add(Number(1), Number(1)))).simplified(),
            NamedAggregate(("First", Number(2))),
        )

    def test_relation_substituted(self) -> None:
        self.assertEqual(
            Equal(Variable("X"), Variable("Y")).substituted(
                lambda x: Number(1) if x == Variable("X") else x
            ),
            Equal(Number(1), Variable("Y")),
        )
        self.assertEqual(
            Equal(Variable("X"), Variable("Y")).substituted(
                lambda x: Number(1) if x == Variable("Y") else x
            ),
            Equal(Variable("X"), Number(1)),
        )
        self.assertEqual(
            Equal(Variable("X"), Variable("Y")).substituted(
                lambda x: Number(1) if x == Equal(Variable("X"), Variable("Y")) else x
            ),
            Number(1),
        )

    def test_relation_simplified(self) -> None:
        self.assertEqual(
            Equal(Variable("X"), Add(Number(1), Number(1))).simplified(),
            Equal(Variable("X"), Number(2)),
        )
        self.assertEqual(
            Equal(Add(Number(1), Number(1)), Variable("X")).simplified(),
            Equal(Number(2), Variable("X")),
        )
        self.assertEqual(
            Equal(Add(Number(1), Number(1)), Add(Number(1), Number(1))).simplified(), TRUE,
        )

    def test_relation_contains(self) -> None:
        self.assertTrue(Variable("X") in Less(Variable("X"), Number(42)))

    def test_relation_variables(self) -> None:
        self.assertEqual(Less(Variable("X"), Call("Y")).variables(), [Variable("X")])
        self.assertEqual(
            Less(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")]
        )

    def test_less_neg(self) -> None:
        self.assertEqual(-Less(Variable("X"), Number(1)), GreaterEqual(Variable("X"), Number(1)))

    def test_less_simplified(self) -> None:
        self.assertEqual(
            Less(Number(0), Number(1)).simplified(), TRUE,
        )
        self.assertEqual(
            Less(Number(1), Number(1)).simplified(), FALSE,
        )
        self.assertEqual(
            Less(Number(2), Number(1)).simplified(), FALSE,
        )

    def test_less_equal_neg(self) -> None:
        self.assertEqual(-LessEqual(Variable("X"), Number(1)), Greater(Variable("X"), Number(1)))

    def test_less_equal_simplified(self) -> None:
        self.assertEqual(
            LessEqual(Number(0), Number(1)).simplified(), TRUE,
        )
        self.assertEqual(
            LessEqual(Number(1), Number(1)).simplified(), TRUE,
        )
        self.assertEqual(
            LessEqual(Number(2), Number(1)).simplified(), FALSE,
        )

    def test_equal_neg(self) -> None:
        self.assertEqual(-Equal(Variable("X"), Number(1)), NotEqual(Variable("X"), Number(1)))

    def test_equal_simplified(self) -> None:
        self.assertEqual(
            Equal(Number(0), Number(1)).simplified(), FALSE,
        )
        self.assertEqual(
            Equal(Number(1), Number(1)).simplified(), TRUE,
        )
        self.assertEqual(
            Equal(Number(2), Number(1)).simplified(), FALSE,
        )

    def test_greater_neg(self) -> None:
        self.assertEqual(-Greater(Variable("X"), Number(1)), LessEqual(Variable("X"), Number(1)))

    def test_greater_simplified(self) -> None:
        self.assertEqual(
            Greater(Number(0), Number(1)).simplified(), FALSE,
        )
        self.assertEqual(
            Greater(Number(1), Number(1)).simplified(), FALSE,
        )
        self.assertEqual(
            Greater(Number(2), Number(1)).simplified(), TRUE,
        )

    def test_greater_equal_neg(self) -> None:
        self.assertEqual(-GreaterEqual(Variable("X"), Number(1)), Less(Variable("X"), Number(1)))

    def test_greater_equal_simplified(self) -> None:
        self.assertEqual(
            GreaterEqual(Number(0), Number(1)).simplified(), FALSE,
        )
        self.assertEqual(
            GreaterEqual(Number(1), Number(1)).simplified(), TRUE,
        )
        self.assertEqual(
            GreaterEqual(Number(2), Number(1)).simplified(), TRUE,
        )

    def test_not_equal_neg(self) -> None:
        self.assertEqual(-NotEqual(Variable("X"), Number(1)), Equal(Variable("X"), Number(1)))

    def test_not_equal_simplified(self) -> None:
        self.assertEqual(
            NotEqual(Number(0), Number(1)).simplified(), TRUE,
        )
        self.assertEqual(
            NotEqual(Number(1), Number(1)).simplified(), FALSE,
        )
        self.assertEqual(
            NotEqual(Number(2), Number(1)).simplified(), TRUE,
        )

    def test_in_neg(self) -> None:
        self.assertEqual(-In(Variable("X"), Number(1)), NotIn(Variable("X"), Number(1)))

    def test_in_simplified(self) -> None:
        self.assertEqual(
            In(Variable("X"), Add(Number(21), Number(21))).simplified(),
            In(Variable("X"), Number(42)),
        )

    def test_in_str(self) -> None:
        self.assertEqual(str(In(Variable("X"), Variable("Y"))), "X in Y")

    def test_not_in_neg(self) -> None:
        self.assertEqual(-NotIn(Variable("X"), Number(1)), In(Variable("X"), Number(1)))

    def test_not_in_simplified(self) -> None:
        self.assertEqual(
            NotIn(Variable("X"), Add(Number(21), Number(21))).simplified(),
            NotIn(Variable("X"), Number(42)),
        )

    def test_not_in_str(self) -> None:
        self.assertEqual(str(NotIn(Variable("X"), Variable("Y"))), "X not in Y")

    def test_slice_substituted(self) -> None:
        self.assertEqual(
            Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
                lambda x: Variable("Z") if x == Variable("X") else x
            ),
            Slice(Variable("Z"), Variable("Y"), Variable("Y")),
        )
        self.assertEqual(
            Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
                lambda x: Variable("Z") if x == Variable("Y") else x
            ),
            Slice(Variable("X"), Variable("Z"), Variable("Z")),
        )
        self.assertEqual(
            Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (
                    Slice(Variable("X"), Variable("Y"), Variable("Z"))
                    if isinstance(x, Slice)
                    else x
                )
            ),
            Slice(Variable("P_X"), Variable("P_Y"), Variable("P_Z")),
        )

    def test_slice_simplified(self) -> None:
        self.assertEqual(
            Slice(
                Variable("Buffer"),
                First("Buffer"),
                Add(Last("Buffer"), Add(Number(21), Number(21))),
            ).simplified(),
            Slice(Variable("Buffer"), First("Buffer"), Add(Last("Buffer"), Number(42))),
        )

    def test_if_expr_findall(self) -> None:
        self.assertEqual(
            If(
                [
                    (Equal(Variable("X"), Number(42)), Number(21)),
                    (Variable("Y"), Number(42)),
                    (Number(42), Variable("Z")),
                ]
            ).findall(lambda x: isinstance(x, Number)),
            [Number(42), Number(21), Number(42), Number(42)],
        )

    def test_if_expr_substituted(self) -> None:
        self.assertEqual(
            If(
                [
                    (Equal(Variable("X"), Number(42)), Number(21)),
                    (Variable("Y"), Number(42)),
                    (Number(42), Variable("Z")),
                ]
            ).substituted(lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x),
            If(
                [
                    (Equal(Variable("P_X"), Number(42)), Number(21)),
                    (Variable("P_Y"), Number(42)),
                    (Number(42), Variable("P_Z")),
                ]
            ),
        )
        self.assertEqual(
            If(
                [
                    (Equal(Variable("X"), Number(42)), Number(21)),
                    (Variable("Y"), Number(42)),
                    (Number(42), Variable("Z")),
                ]
            ).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (
                    If([*x.condition_expressions, (Variable("Z"), Number(1))], x.else_expression)
                    if isinstance(x, If)
                    else x
                )
            ),
            If(
                [
                    (Equal(Variable("P_X"), Number(42)), Number(21)),
                    (Variable("P_Y"), Number(42)),
                    (Number(42), Variable("P_Z")),
                    (Variable("P_Z"), Number(1)),
                ]
            ),
        )

    def test_if_simplified(self) -> None:
        self.assertEqual(
            If(
                [
                    (Variable("X"), Number(21)),
                    (Variable("Y"), Add(Number(21), Number(21))),
                    (Add(Number(21), Number(21)), Variable("Z")),
                ]
            ).simplified(),
            If(
                [
                    (Variable("X"), Number(21)),
                    (Variable("Y"), Number(42)),
                    (Number(42), Variable("Z")),
                ]
            ),
        )
        self.assertEqual(If([(TRUE, Variable("X"))]).simplified(), Variable("X"))

    def test_if_variables(self) -> None:
        self.assertEqual(
            If(
                [
                    (Variable("X"), Number(21)),
                    (Variable("Y"), Add(Number(21), Number(21))),
                    (Add(Number(21), Number(21)), Variable("Z")),
                ]
            ).variables(),
            [Variable("X"), Variable("Y"), Variable("Z")],
        )

    def test_if_str(self) -> None:
        self.assertEqual(
            str(If([(Variable("X"), Number(1)), (Variable("Y"), Number(2))], Number(3))),
            multilinestr(
                """(if
                       X
                    then
                       1
                    elsif
                       Y
                    then
                       2
                    else
                       3)"""
            ),
        )

    def test_case_substituted(self) -> None:
        self.assertEqual(
            Case(Variable("X"), [(Variable("Y"), Variable("Z"))]).substituted(
                lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
            ),
            Case(Variable("P_X"), [(Variable("P_Y"), Variable("P_Z"))]),
        )
        self.assertEqual(
            Case(Variable("X"), [(Variable("Y"), Number(0))]).substituted(
                lambda x: Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (
                    Case(x.control_expression, [*x.case_statements, (Variable("Z"), Number(1))])
                    if isinstance(x, Case)
                    else x
                )
            ),
            Case(Variable("P_X"), [(Variable("P_Y"), Number(0)), (Variable("P_Z"), Number(1))]),
        )

    def test_case_simplified(self) -> None:
        self.assertEqual(
            Case(
                Add(Number(21), Number(21)),
                [
                    (Variable("X"), Number(21)),
                    (Variable("Y"), Add(Number(21), Number(21))),
                    (Add(Number(21), Number(21)), Variable("Z")),
                ],
            ).simplified(),
            Case(
                Number(42),
                [
                    (Variable("X"), Number(21)),
                    (Variable("Y"), Number(42)),
                    (Number(42), Variable("Z")),
                ],
            ),
        )

    def test_case_variables(self) -> None:
        self.assertEqual(
            Case(
                Add(Number(21), Number(21)),
                [
                    (Variable("X"), Number(21)),
                    (Variable("Y"), Add(Number(21), Number(21))),
                    (Add(Number(21), Number(21)), Variable("Z")),
                ],
            ).variables(),
            [Variable("X"), Variable("Y"), Variable("Z")],
        )

    def test_case_str(self) -> None:
        self.assertEqual(
            str(
                Case(
                    Variable("X"),
                    [
                        (Variable("Y"), Number(1)),
                        (Variable("Z"), Number(1)),
                        (Variable("others"), Number(2)),
                    ],
                )
            ),
            multilinestr(
                """(case X is
                       when Y | Z =>
                          1,
                       when others =>
                          2)"""
            ),
        )

    def test_value_range_simplified(self) -> None:
        self.assertEqual(
            ValueRange(Number(1), Add(Number(21), Number(21))).simplified(),
            ValueRange(Number(1), Number(42)),
        )

    def test_quantified_expression_simplified(self) -> None:
        self.assertEqual(
            ForAllOf(
                "X", Variable("List"), Add(Last("Y"), Add(Number(21), Number(21)))
            ).simplified(),
            ForAllOf("X", Variable("List"), Add(Last("Y"), Number(42))),
        )

    def test_quantified_expression_variables(self) -> None:
        self.assertEqual(
            ForAllOf(
                "A", Variable("List"), Add(Variable("X"), Add(Variable("Y"), Variable("Z")))
            ).variables(),
            [Variable("List"), Variable("X"), Variable("Y"), Variable("Z")],
        )

    def test_quantified_expression_str(self) -> None:
        self.assertEqual(
            str(ForAllOf("X", Variable("Y"), Variable("X"))), "(for all X of Y =>\n    X)"
        )
        self.assertEqual(
            str(ForAllIn("X", Variable("Y"), Variable("X"))), "(for all X in Y =>\n    X)"
        )

    def test_expr_contains(self) -> None:
        self.assertTrue(
            Variable("X")
            in Or(Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42))))
        )
        self.assertFalse(
            Variable("Z")
            in Or(Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42))))
        )
        self.assertTrue(
            Less(Variable("X"), Number(42))
            in Or(Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42))))
        )
        self.assertFalse(
            Less(Variable("Z"), Number(42))
            in Or(Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(1))))
        )

    def test_expr_variables(self) -> None:
        self.assertEqual(
            Or(
                Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
            ).variables(),
            [Variable("Y"), Variable("X")],
        )
        self.assertEqual(
            Or(
                Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
            ).variables(),
            [Variable("Y"), Variable("X")],
        )
        self.assertEqual(
            Or(
                Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
            ).variables(),
            [Variable("Y"), Variable("X")],
        )
        self.assertEqual(
            Or(
                Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
            ).variables(),
            [Variable("Y"), Variable("X")],
        )

    def test_expr_variables_duplicates(self) -> None:
        self.assertEqual(
            And(Variable("X"), Variable("Y"), Variable("X")).variables(),
            [Variable("X"), Variable("Y")],
        )
        self.assertEqual(
            Or(Variable("X"), Variable("Y"), Variable("X")).variables(),
            [Variable("X"), Variable("Y")],
        )
        self.assertEqual(
            Add(Variable("X"), Variable("Y"), Variable("X")).variables(),
            [Variable("X"), Variable("Y")],
        )
        self.assertEqual(
            Mul(Variable("X"), Variable("Y"), Variable("X")).variables(),
            [Variable("X"), Variable("Y")],
        )
        self.assertEqual(Sub(Variable("X"), Variable("X")).variables(), [Variable("X")])
        self.assertEqual(Div(Variable("X"), Variable("X")).variables(), [Variable("X")])
        self.assertEqual(
            Or(
                Greater(Variable("X"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
            ).variables(),
            [Variable("X")],
        )

    def test_expr_substituted_pre(self) -> None:
        with self.assertRaises(AssertionError):
            Number(1).substituted()
        with self.assertRaises(AssertionError):
            Add(Number(1), Number(1)).substituted()
        with self.assertRaises(AssertionError):
            Number(1).substituted(lambda x: x, {})
        with self.assertRaises(AssertionError):
            Add(Number(1), Number(1)).substituted(lambda x: x, {})

    def test_length_z3variables(self) -> None:
        self.assertEqual(Length("Z").variables(True), [Variable("Z'Length")])

    def test_last_z3variables(self) -> None:
        self.assertEqual(Last("Z").variables(True), [Variable("Z'Last")])

    def test_first_z3variables(self) -> None:
        self.assertEqual(First("Z").variables(True), [Variable("Z'First")])

    def test_size_z3variables(self) -> None:
        self.assertEqual(Size("Z").variables(True), [Variable("Z'Size")])

    def test_not_variables(self) -> None:
        self.assertEqual(Not(Variable("X")).variables(), [Variable("X")])

    def test_number_str(self) -> None:
        self.assertEqual(str(Number(15)), "15")

    def test_number_str_long(self) -> None:
        self.assertEqual(str(Number(539535)), "539535")

    def test_number_str_neg_long(self) -> None:
        self.assertEqual(str(Number(-539535)), "(-539535)")

    def test_number_str_hex(self) -> None:
        self.assertEqual(str(Number(4096, 16)), "16#1000#")

    def test_number_str_neg_hex(self) -> None:
        self.assertEqual(str(Number(-4096, 16)), "(-16#1000#)")

    def test_number_str_dec(self) -> None:
        self.assertEqual(str(Number(4096, 10)), "10#4096#")

    def test_number_str_oct(self) -> None:
        self.assertEqual(str(Number(45432, 8)), "8#130570#")

    def test_number_str_neg_oct(self) -> None:
        self.assertEqual(str(Number(-45432, 8)), "(-8#130570#)")

    def test_number_str_bin(self) -> None:
        self.assertEqual(str(Number(454, 2)), "2#111000110#")

    def test_expr_str(self) -> None:
        self.assertEqual(
            str(
                And(
                    If([(Variable("X"), Number(1)), (Variable("Y"), Number(2))], Number(3)),
                    Variable("A"),
                    Or(Variable("B"), Variable("C")),
                    Variable("D"),
                )
            ),
            multilinestr(
                """(if
                       X
                    then
                       1
                    elsif
                       Y
                    then
                       2
                    else
                       3)
                   and A
                   and (B
                        or C)
                   and D"""
            ),
        )
        self.assertEqual(
            str(
                ForAllOf(
                    "X",
                    Variable("Z"),
                    If([(Variable("X"), Number(1)), (Variable("Y"), Number(2))], Number(3)),
                )
            ),
            multilinestr(
                """(for all X of Z =>
                       (if
                           X
                        then
                           1
                        elsif
                           Y
                        then
                           2
                        else
                           3))"""
            ),
        )
