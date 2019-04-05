import unittest

from rflx.expression import (FALSE, TRUE, UNDEFINED, Add, And, Case, Div, Equal, ExpressionError,
                             First, Greater, GreaterEqual, If, Last, Length, LengthValue, Less,
                             LessEqual, Mul, NotEqual, Number, Or, Slice, Sub, Value)

EXPR = Equal(UNDEFINED, UNDEFINED)


class TestExpression(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_true_neg(self) -> None:
        self.assertEqual(-TRUE,
                         FALSE)

    def test_true_simplified(self) -> None:
        self.assertEqual(TRUE.simplified(),
                         TRUE)

    def test_false_neg(self) -> None:
        self.assertEqual(-FALSE,
                         TRUE)

    def test_false_simplified(self) -> None:
        self.assertEqual(FALSE.simplified(),
                         FALSE)

    def test_and_contains(self) -> None:
        self.assertTrue(Value('X') in And(TRUE, Less(Value('X'), Number(42))))
        self.assertFalse(Value('Y') in And(TRUE, Less(Value('X'), Number(42))))

    def test_and_simplified(self) -> None:
        self.assertEqual(And(TRUE, TRUE).simplified(),
                         TRUE)
        self.assertEqual(And(TRUE, EXPR).simplified(),
                         EXPR)
        self.assertEqual(And(EXPR, TRUE).simplified(),
                         EXPR)

    def test_or_contains(self) -> None:
        self.assertTrue(Value('X') in Or(Less(Value('X'), Number(42)), TRUE))
        self.assertFalse(Value('Y') in Or(Less(Value('X'), Number(42)), TRUE))

    def test_or_simplified(self) -> None:
        self.assertEqual(Or(TRUE, TRUE).simplified(),
                         TRUE)
        self.assertEqual(Or(TRUE, EXPR).simplified(),
                         TRUE)
        self.assertEqual(Or(EXPR, TRUE).simplified(),
                         TRUE)

    def test_undefined_neg(self) -> None:
        self.assertEqual(-UNDEFINED,
                         UNDEFINED)

    def test_undefined_simplified(self) -> None:
        self.assertEqual(UNDEFINED.simplified(),
                         UNDEFINED)

    def test_undefined_to_bytes(self) -> None:
        self.assertEqual(UNDEFINED.to_bytes(),
                         UNDEFINED)

    def test_number_neg(self) -> None:
        self.assertEqual(-Number(42),
                         Number(-42))

    def test_number_simplified(self) -> None:
        self.assertEqual(Number(42).simplified(),
                         Number(42))

    def test_number_to_bytes(self) -> None:
        self.assertEqual(Number(48).to_bytes(),
                         Number(6))
        self.assertEqual(Number(47).to_bytes(),
                         Number(5))

    def test_number_add(self) -> None:
        self.assertEqual(Number(5) + Number(3), Number(8))

    def test_number_sub(self) -> None:
        self.assertEqual(Number(5) - Number(3), Number(2))

    def test_number_mul(self) -> None:
        self.assertEqual(Number(4) * Number(2), Number(8))

    def test_number_div(self) -> None:
        self.assertEqual(Number(4) // Number(2), Number(2))

    def test_number_pow(self) -> None:
        self.assertEqual(Number(2)**Number(4), Number(16))

    def test_number_lt(self) -> None:
        self.assertEqual(Number(1) < Number(2),
                         True)
        self.assertEqual(Number(2) < Number(2),
                         False)
        self.assertEqual(Number(3) < Number(2),
                         False)
        self.assertEqual(Value('X') < Number(2),
                         False)
        self.assertEqual(Number(2) < Value('X'),
                         False)

    def test_number_le(self) -> None:
        self.assertEqual(Number(1) <= Number(2),
                         True)
        self.assertEqual(Number(2) <= Number(2),
                         True)
        self.assertEqual(Number(3) <= Number(2),
                         False)
        self.assertEqual(Value('X') <= Number(2),
                         False)
        self.assertEqual(Number(2) <= Value('X'),
                         False)

    def test_number_gt(self) -> None:
        self.assertEqual(Number(1) > Number(2),
                         False)
        self.assertEqual(Number(2) > Number(2),
                         False)
        self.assertEqual(Number(3) > Number(2),
                         True)
        self.assertEqual(Value('X') > Number(2),
                         False)
        self.assertEqual(Number(2) > Value('X'),
                         False)

    def test_number_ge(self) -> None:
        self.assertEqual(Number(1) >= Number(2),
                         False)
        self.assertEqual(Number(2) >= Number(2),
                         True)
        self.assertEqual(Number(3) >= Number(2),
                         True)
        self.assertEqual(Value('X') >= Number(2),
                         False)
        self.assertEqual(Number(2) >= Value('X'),
                         False)

    def test_add_neg(self) -> None:
        self.assertEqual(-Add(Value('X'), Number(1)),
                         Add(Value('X', True), Number(-1)))

    def test_add_simplified(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)).simplified(),
                         Add(Value('X'), Number(1)))
        self.assertEqual(Add(Value('X'), Number(0)).simplified(),
                         Value('X'))
        self.assertEqual(Add(Number(2), Number(3), Number(5)).simplified(),
                         Number(10))
        self.assertEqual(Add(Value('X'), Value('Y'), Value('X', True)).simplified(),
                         Value('Y'))
        self.assertEqual(Add(Value('X'), Value('Y'), Value('X'), -Value('X')).simplified(),
                         Add(Value('X'), Value('Y')))

    def test_add_to_bytes(self) -> None:
        self.assertEqual(Add(Value('X'), Number(8)).to_bytes(),
                         Add(Value('X'), Number(1)))

    def test_add_lt(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) < Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(2)) < Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(3)) < Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(1)) < Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) < Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) < Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_add_le(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) <= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(2)) <= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(3)) <= Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(1)) <= Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) <= Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) <= Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_add_gt(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) > Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) > Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(3)) > Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(1)) > Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) > Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) > Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_add_ge(self) -> None:
        self.assertEqual(Add(Value('X'), Number(1)) >= Add(Value('X'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) >= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(3)) >= Add(Value('X'), Number(2)),
                         True)
        self.assertEqual(Add(Value('X'), Number(1)) >= Add(Value('Y'), Number(2)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) >= Add(Value('Y'), Number(1)),
                         False)
        self.assertEqual(Add(Value('X'), Number(2)) >= Add(Value('Y'), Value('Z'), Number(1)),
                         False)

    def test_mul_neg(self) -> None:
        self.assertEqual(-Mul(Value('X'), Number(2)),
                         Mul(Value('X'), Number(-2)))

    def test_mul_simplified(self) -> None:
        self.assertEqual(Mul(Value('X'), Number(2)).simplified(),
                         Mul(Value('X'), Number(2)))
        self.assertEqual(Mul(Value('X'), Number(1)).simplified(),
                         Value('X'))
        self.assertEqual(Mul(Number(2), Number(3), Number(5)).simplified(),
                         Number(30))

    def test_mul_to_bytes(self) -> None:
        self.assertEqual(Mul(Value('X'), Number(8)).to_bytes(),
                         Mul(Value('X'), Number(1)))

    def test_sub_neg(self) -> None:
        self.assertEqual(-Sub(Number(1), Value('X')),
                         Sub(Number(-1), Value('X')))

    def test_sub_simplified(self) -> None:
        self.assertEqual(Sub(Number(1), Value('X')).simplified(),
                         Add(Value('X'), Number(-1)))
        self.assertEqual(Sub(Value('X'), Number(1)).simplified(),
                         Add(Value('X'), Number(-1)))
        self.assertEqual(Sub(Number(6), Number(2)).simplified(),
                         Number(4))
        self.assertEqual(Sub(Value('X'), Value('Y')).simplified(),
                         Add(Value('X'), Value('Y', True)))

    def test_sub_to_bytes(self) -> None:
        self.assertEqual(Sub(Value('X'), Number(8)).to_bytes(),
                         Sub(Value('X'), Number(1)))

    def test_div_neg(self) -> None:
        self.assertEqual(-Div(Value('X'), Number(1)),
                         Div(Value('X', True), Number(1)))

    def test_div_simplified(self) -> None:
        self.assertEqual(Div(Value('X'), Number(1)).simplified(),
                         Div(Value('X'), Number(1)))
        self.assertEqual(Div(Number(6), Number(2)).simplified(),
                         Number(3))
        self.assertEqual(Div(Number(9), Number(2)).simplified(),
                         Div(Number(9), Number(2)))

    def test_div_to_bytes(self) -> None:
        self.assertEqual(Div(Value('X'), Number(8)).to_bytes(),
                         Div(Value('X'), Number(1)))

    def test_term_simplified(self) -> None:
        self.assertEqual(Add(Mul(Number(1), Number(6)),
                             Sub(Value('X'), Number(10)),
                             Add(Number(1), Number(3))).simplified(),
                         Value('X'))

    def test_term_to_bytes(self) -> None:
        self.assertEqual(Add(Mul(Number(8), Number(48)),
                             Sub(Value('X'), Number(80)),
                             Div(Number(8), Number(24))).to_bytes(),
                         Add(Mul(Number(1), Number(6)),
                             Sub(Value('X'), Number(10)),
                             Div(Number(1), Number(3))))

    def test_distributivity_simplified(self) -> None:
        self.assertEqual(Add(Sub(Value('X'), Add(Value('X'), Number(1))),
                             Add(Value('X'), Number(1))).simplified(),
                         Value('X'))
        self.assertEqual(Div(Add(Mul(Value('X'), Number(8)), Number(144)), Number(8)).simplified(),
                         Add(Value('X'), Number(18)))
        self.assertEqual(Div(Sub(Mul(Value('X'), Number(8)), Number(148)), Number(8)).simplified(),
                         Add(Value('X'), Div(Number(-148), Number(8))))

    def test_value_name(self) -> None:
        try:
            Value('Foo')
        except ExpressionError:
            self.fail(f'unexpected ExpressionError')

    def test_value_invalid_name(self) -> None:
        with self.assertRaises(ExpressionError):
            Value('Foo (Bar)')

    def test_value_neg(self) -> None:
        self.assertEqual(-Value('X'),
                         Value('X', True))

    def test_value_simplified(self) -> None:
        self.assertEqual(Value('X').simplified(),
                         Value('X'))
        self.assertEqual(Value('X').simplified({Value('X'): Number(42)}),
                         Number(42))

    def test_length_simplified(self) -> None:
        self.assertEqual(Length('X').simplified(),
                         Length('X'))
        self.assertEqual(Length('X').simplified({Length('X'): Number(42)}),
                         Number(42))
        self.assertEqual(-Length('X').simplified({Length('X'): Number(42)}),
                         Number(-42))

    def test_first_neg(self) -> None:
        self.assertEqual(-First('X'),
                         First('X', True))

    def test_first_simplified(self) -> None:
        self.assertEqual(First('X').simplified(),
                         First('X'))
        self.assertEqual(First('X').simplified({First('X'): Number(42)}),
                         Number(42))
        self.assertEqual(-First('X').simplified({First('X'): Number(42)}),
                         Number(-42))

    def test_last_neg(self) -> None:
        self.assertEqual(-Last('X'),
                         Last('X', True))

    def test_last_simplified(self) -> None:
        self.assertEqual(Last('X').simplified(),
                         Last('X'))
        self.assertEqual(Last('X').simplified({Last('X'): Number(42)}),
                         Number(42))
        self.assertEqual(-Last('X').simplified({Last('X'): Number(42)}),
                         Number(-42))

    def test_relation_contains(self) -> None:
        self.assertTrue(Value('X') in Less(Value('X'), Number(42)))

    def test_relation_converted(self) -> None:
        self.assertEqual(
            Less(Value('X'), Number(1)).converted(
                lambda x: LengthValue(x) if isinstance(x, Value) else x),
            Less(LengthValue('X'), Number(1)))

    def test_less_neg(self) -> None:
        self.assertEqual(-Less(Value('X'), Number(1)),
                         GreaterEqual(Value('X'), Number(1)))

    def test_less_simplified(self) -> None:
        self.assertEqual(Less(Value('X'), Add(Number(21), Number(21))).simplified(),
                         Less(Value('X'), Number(42)))

    def test_less_equal_neg(self) -> None:
        self.assertEqual(-LessEqual(Value('X'), Number(1)),
                         Greater(Value('X'), Number(1)))

    def test_less_equal_simplified(self) -> None:
        self.assertEqual(LessEqual(Value('X'), Add(Number(21), Number(21))).simplified(),
                         LessEqual(Value('X'), Number(42)))

    def test_equal_neg(self) -> None:
        self.assertEqual(-Equal(Value('X'), Number(1)),
                         NotEqual(Value('X'), Number(1)))

    def test_equal_simplified(self) -> None:
        self.assertEqual(Equal(Value('X'), Add(Number(21), Number(21))).simplified(),
                         Equal(Value('X'), Number(42)))

    def test_greater_neg(self) -> None:
        self.assertEqual(-Greater(Value('X'), Number(1)),
                         LessEqual(Value('X'), Number(1)))

    def test_greater_simplified(self) -> None:
        self.assertEqual(Greater(Value('X'), Add(Number(21), Number(21))).simplified(),
                         Greater(Value('X'), Number(42)))

    def test_greater_equal_neg(self) -> None:
        self.assertEqual(-GreaterEqual(Value('X'), Number(1)),
                         Less(Value('X'), Number(1)))

    def test_greater_equal_simplified(self) -> None:
        self.assertEqual(GreaterEqual(Value('X'), Add(Number(21), Number(21))).simplified(),
                         GreaterEqual(Value('X'), Number(42)))

    def test_not_equal_neg(self) -> None:
        self.assertEqual(-NotEqual(Value('X'), Number(1)),
                         Equal(Value('X'), Number(1)))

    def test_not_equal_simplified(self) -> None:
        self.assertEqual(NotEqual(Value('X'), Add(Number(21), Number(21))).simplified(),
                         NotEqual(Value('X'), Number(42)))

    def test_slice_simplified(self) -> None:
        self.assertEqual(Slice('Buffer',
                               First('Buffer'),
                               Add(Last('Buffer'), Add(Number(21), Number(21)))).simplified(),
                         Slice('Buffer',
                               First('Buffer'),
                               Add(Last('Buffer'), Number(42))))

    def test_if_simplified(self) -> None:
        self.assertEqual(If([(Value('X'), Number(21)),
                             (Value('Y'), Add(Number(21), Number(21))),
                             (Add(Number(21), Number(21)), Value('Z'))]).simplified(),
                         If([(Value('X'), Number(21)),
                             (Value('Y'), Number(42)),
                             (Number(42), Value('Z'))]))

    def test_case_simplified(self) -> None:
        self.assertEqual(Case(Add(Number(21), Number(21)),
                              [(Value('X'), Number(21)),
                               (Value('Y'), Add(Number(21), Number(21))),
                               (Add(Number(21), Number(21)), Value('Z'))]).simplified(),
                         Case(Number(42),
                              [(Value('X'), Number(21)),
                               (Value('Y'), Number(42)),
                               (Number(42), Value('Z'))]))

    def test_expr_contains(self) -> None:
        self.assertTrue(Value('X') in Or(Greater(Value('Y'), Number(42)),
                                         And(TRUE, Less(Value('X'), Number(42)))))
        self.assertFalse(Value('Z') in Or(Greater(Value('Y'), Number(42)),
                                          And(TRUE, Less(Value('X'), Number(42)))))
        self.assertTrue(Less(Value('X'), Number(42)) in Or(Greater(Value('Y'), Number(42)),
                                                           And(TRUE, Less(Value('X'), Number(42)))))
        self.assertFalse(Less(Value('Z'), Number(42)) in Or(Greater(Value('Y'), Number(42)),
                                                            And(TRUE, Less(Value('X'), Number(1)))))
