import unittest

from rflx.expression import (FALSE, TRUE, UNDEFINED, Add, Aggregate, And, Case, Div, Equal, First,
                             ForAllOf, Greater, GreaterEqual, If, Last, Length, Less, LessEqual,
                             Mod, Mul, Name, NamedAggregate, Not, NotEqual, Number, Or, Pow, Range,
                             Size, Slice, Sub, Variable)

EXPR = Equal(UNDEFINED, UNDEFINED)


class TestExpression(unittest.TestCase):  # pylint: disable=too-many-public-methods
    def setUp(self) -> None:
        self.maxDiff = None  # pylint: disable=invalid-name

    def test_true_neg(self) -> None:
        self.assertEqual(
            -TRUE,
            FALSE)

    def test_true_simplified(self) -> None:
        self.assertEqual(
            TRUE.simplified(),
            TRUE)

    def test_true_variables(self) -> None:
        self.assertEqual(
            TRUE.variables(),
            [])

    def test_false_neg(self) -> None:
        self.assertEqual(
            -FALSE,
            TRUE)

    def test_false_simplified(self) -> None:
        self.assertEqual(
            FALSE.simplified(),
            FALSE)

    def test_false_variables(self) -> None:
        self.assertEqual(
            FALSE.variables(),
            [])

    def test_not_neg(self) -> None:
        self.assertEqual(
            -Not(Variable('X')),
            Variable('X'))

    def test_bin_expr_converted(self) -> None:
        self.assertEqual(
            Less(Variable('X'), Number(1)).converted(
                lambda x: Name(x.name) if isinstance(x, Variable) else x),
            Less(Name('X'), Number(1)))
        self.assertEqual(
            Sub(Variable('X'), Number(1)).converted(
                lambda x: Name('Y') if x == Sub(Variable('X'), Number(1)) else x),
            Name('Y'))

    def test_ass_expr_converted(self) -> None:
        self.assertEqual(
            And(Variable('X'), Number(1)).converted(
                lambda x: Name(x.name) if isinstance(x, Variable) else x),
            And(Name('X'), Number(1)))
        self.assertEqual(
            Mul(Variable('X'), Number(1)).converted(
                lambda x: Name('Y') if x == Mul(Variable('X'), Number(1)) else x),
            Name('Y'))

    def test_and_neg(self) -> None:
        self.assertEqual(
            -And(Variable('X'), Number(1)),
            And(-Variable('X'), Number(-1)))

    def test_and_variables(self) -> None:
        self.assertEqual(
            And(Variable('X'), Variable('Y'), Name('Z')).variables(),
            [Variable('X'), Variable('Y')])

    def test_and_contains(self) -> None:
        self.assertTrue(
            Variable('X') in And(TRUE, Less(Variable('X'), Number(42))))
        self.assertFalse(
            Variable('Y') in And(TRUE, Less(Variable('X'), Number(42))))

    def test_and_simplified(self) -> None:
        self.assertEqual(
            And(TRUE, TRUE).simplified(),
            TRUE)
        self.assertEqual(
            And(TRUE, FALSE, TRUE).simplified(),
            FALSE)
        self.assertEqual(
            And(TRUE, EXPR).simplified(),
            EXPR)
        self.assertEqual(
            And(EXPR, TRUE).simplified(),
            EXPR)

    def test_or_neg(self) -> None:
        self.assertEqual(
            -Or(Variable('X'), Number(1)),
            Or(-Variable('X'), Number(-1)))

    def test_or_variables(self) -> None:
        self.assertEqual(
            Or(Variable('X'), Variable('Y'), Name('Z')).variables(),
            [Variable('X'), Variable('Y')])

    def test_or_contains(self) -> None:
        self.assertTrue(
            Variable('X') in Or(Less(Variable('X'), Number(42)), TRUE))
        self.assertFalse(
            Variable('Y') in Or(Less(Variable('X'), Number(42)), TRUE))

    def test_or_simplified(self) -> None:
        self.assertEqual(
            Or(TRUE, TRUE).simplified(),
            TRUE)
        self.assertEqual(
            Or(TRUE, EXPR).simplified(),
            TRUE)
        self.assertEqual(
            Or(EXPR, TRUE).simplified(),
            TRUE)

    def test_undefined_neg(self) -> None:
        self.assertEqual(
            -UNDEFINED,
            -UNDEFINED)

    def test_undefined_simplified(self) -> None:
        self.assertEqual(
            UNDEFINED.simplified(),
            UNDEFINED)

    def test_number_neg(self) -> None:
        self.assertEqual(
            -Number(42),
            Number(-42))

    def test_number_simplified(self) -> None:
        self.assertEqual(
            Number(42).simplified(),
            Number(42))

    def test_number_add(self) -> None:
        self.assertEqual(
            Number(5) + Number(3), Number(8))

    def test_number_sub(self) -> None:
        self.assertEqual(
            Number(5) - Number(3), Number(2))

    def test_number_mul(self) -> None:
        self.assertEqual(
            Number(4) * Number(2), Number(8))

    def test_number_div(self) -> None:
        self.assertEqual(
            Number(4) // Number(2), Number(2))

    def test_number_pow(self) -> None:
        self.assertEqual(
            Number(2)**Number(4), Number(16))

    def test_number_lt(self) -> None:
        self.assertEqual(
            Number(1) < Number(2),
            True)
        self.assertEqual(
            Number(2) < Number(2),
            False)
        self.assertEqual(
            Number(3) < Number(2),
            False)
        self.assertEqual(
            Variable('X') < Number(2),
            False)
        self.assertEqual(
            Number(2) < Variable('X'),
            False)

    def test_number_le(self) -> None:
        self.assertEqual(
            Number(1) <= Number(2),
            True)
        self.assertEqual(
            Number(2) <= Number(2),
            True)
        self.assertEqual(
            Number(3) <= Number(2),
            False)
        self.assertEqual(
            Variable('X') <= Number(2),
            False)
        self.assertEqual(
            Number(2) <= Variable('X'),
            False)

    def test_number_gt(self) -> None:
        self.assertEqual(
            Number(1) > Number(2),
            False)
        self.assertEqual(
            Number(2) > Number(2),
            False)
        self.assertEqual(
            Number(3) > Number(2),
            True)
        self.assertEqual(
            Variable('X') > Number(2),
            False)
        self.assertEqual(
            Number(2) > Variable('X'),
            False)

    def test_number_ge(self) -> None:
        self.assertEqual(
            Number(1) >= Number(2),
            False)
        self.assertEqual(
            Number(2) >= Number(2),
            True)
        self.assertEqual(
            Number(3) >= Number(2),
            True)
        self.assertEqual(
            Variable('X') >= Number(2),
            False)
        self.assertEqual(
            Number(2) >= Variable('X'),
            False)

    def test_add_neg(self) -> None:
        self.assertEqual(
            -Add(Variable('X'), Number(1)),
            Add(Variable('X', True), Number(-1)))

    def test_add_variables(self) -> None:
        self.assertEqual(
            Add(Variable('X'), Variable('Y'), Name('Z')).variables(),
            [Variable('X'), Variable('Y')])

    def test_add_simplified(self) -> None:
        self.assertEqual(
            Add(Variable('X'), Number(1)).simplified(),
            Add(Variable('X'), Number(1)))
        self.assertEqual(
            Add(Variable('X'), Number(0)).simplified(),
            Variable('X'))
        self.assertEqual(
            Add(Number(2), Number(3), Number(5)).simplified(),
            Number(10))
        self.assertEqual(
            Add(Variable('X'), Variable('Y'), Variable('X', True)).simplified(),
            Variable('Y'))
        self.assertEqual(
            Add(Variable('X'), Variable('Y'), Variable('X'), -Variable('X')).simplified(),
            Add(Variable('X'), Variable('Y')))

    def test_add_lt(self) -> None:
        self.assertEqual(
            Add(Variable('X'), Number(1)) < Add(Variable('X'), Number(2)),
            True)
        self.assertEqual(
            Add(Variable('X'), Number(2)) < Add(Variable('X'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(3)) < Add(Variable('X'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(1)) < Add(Variable('Y'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) < Add(Variable('Y'), Number(1)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) < Add(Variable('Y'), Variable('Z'), Number(1)),
            False)

    def test_add_le(self) -> None:
        self.assertEqual(
            Add(Variable('X'), Number(1)) <= Add(Variable('X'), Number(2)),
            True)
        self.assertEqual(
            Add(Variable('X'), Number(2)) <= Add(Variable('X'), Number(2)),
            True)
        self.assertEqual(
            Add(Variable('X'), Number(3)) <= Add(Variable('X'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(1)) <= Add(Variable('Y'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) <= Add(Variable('Y'), Number(1)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) <= Add(Variable('Y'), Variable('Z'), Number(1)),
            False)

    def test_add_gt(self) -> None:
        self.assertEqual(
            Add(Variable('X'), Number(1)) > Add(Variable('X'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) > Add(Variable('X'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(3)) > Add(Variable('X'), Number(2)),
            True)
        self.assertEqual(
            Add(Variable('X'), Number(1)) > Add(Variable('Y'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) > Add(Variable('Y'), Number(1)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) > Add(Variable('Y'), Variable('Z'), Number(1)),
            False)

    def test_add_ge(self) -> None:
        self.assertEqual(
            Add(Variable('X'), Number(1)) >= Add(Variable('X'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) >= Add(Variable('X'), Number(2)),
            True)
        self.assertEqual(
            Add(Variable('X'), Number(3)) >= Add(Variable('X'), Number(2)),
            True)
        self.assertEqual(
            Add(Variable('X'), Number(1)) >= Add(Variable('Y'), Number(2)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) >= Add(Variable('Y'), Number(1)),
            False)
        self.assertEqual(
            Add(Variable('X'), Number(2)) >= Add(Variable('Y'), Variable('Z'), Number(1)),
            False)

    def test_mul_neg(self) -> None:
        self.assertEqual(
            -Mul(Variable('X'), Number(2)),
            Mul(Variable('X'), Number(-2)))

    def test_mul_variables(self) -> None:
        self.assertEqual(
            Mul(Variable('X'), Variable('Y'), Name('Z')).variables(),
            [Variable('X'), Variable('Y')])

    def test_mul_simplified(self) -> None:
        self.assertEqual(
            Mul(Variable('X'), Number(2)).simplified(),
            Mul(Variable('X'), Number(2)))
        self.assertEqual(
            Mul(Variable('X'), Number(1)).simplified(),
            Variable('X'))
        self.assertEqual(
            Mul(Number(2), Number(3), Number(5)).simplified(),
            Number(30))

    def test_sub_neg(self) -> None:
        self.assertEqual(
            -Sub(Number(1), Variable('X')),
            Sub(Number(-1), Variable('X')))

    def test_sub_variables(self) -> None:
        self.assertEqual(
            Sub(Variable('X'), Name('Y')).variables(),
            [Variable('X')])

    def test_sub_simplified(self) -> None:
        self.assertEqual(
            Sub(Number(1), Variable('X')).simplified(),
            Add(Variable('X'), Number(-1)))
        self.assertEqual(
            Sub(Variable('X'), Number(1)).simplified(),
            Add(Variable('X'), Number(-1)))
        self.assertEqual(
            Sub(Number(6), Number(2)).simplified(),
            Number(4))
        self.assertEqual(
            Sub(Variable('X'), Variable('Y')).simplified(),
            Add(Variable('X'), Variable('Y', True)))

    def test_div_neg(self) -> None:
        self.assertEqual(
            -Div(Variable('X'), Number(1)),
            Div(Variable('X', True), Number(1)))

    def test_div_variables(self) -> None:
        self.assertEqual(
            Div(Variable('X'), Name('Y')).variables(),
            [Variable('X')])
        self.assertEqual(
            Div(Variable('X'), Variable('Y')).variables(),
            [Variable('X'), Variable('Y')])

    def test_div_simplified(self) -> None:
        self.assertEqual(
            Div(Variable('X'), Number(1)).simplified(),
            Div(Variable('X'), Number(1)))
        self.assertEqual(
            Div(Number(6), Number(2)).simplified(),
            Number(3))
        self.assertEqual(
            Div(Number(9), Number(2)).simplified(),
            Div(Number(9), Number(2)))

    def test_pow_simplified(self) -> None:
        self.assertEqual(
            Pow(Variable('X'), Number(1)).simplified(),
            Pow(Variable('X'), Number(1)))
        self.assertEqual(
            Pow(Variable('X'), Add(Number(1), Number(1))).simplified(),
            Pow(Variable('X'), Number(2)))
        self.assertEqual(
            Pow(Number(6), Number(2)).simplified(),
            Number(36))

    def test_pow_variables(self) -> None:
        self.assertEqual(
            Pow(Variable('X'), Variable('Y')).variables(),
            [Variable('X'), Variable('Y')])

    def test_mod_simplified(self) -> None:
        self.assertEqual(
            Mod(Variable('X'), Number(1)).simplified(),
            Mod(Variable('X'), Number(1)))
        self.assertEqual(
            Mod(Variable('X'), Add(Number(1), Number(1))).simplified(),
            Mod(Variable('X'), Number(2)))
        self.assertEqual(
            Mod(Number(6), Number(2)).simplified(),
            Number(0))

    def test_mod_variables(self) -> None:
        self.assertEqual(
            Mod(Variable('X'), Variable('Y')).variables(),
            [Variable('X'), Variable('Y')])

    def test_term_simplified(self) -> None:
        self.assertEqual(
            Add(Mul(Number(1), Number(6)),
                Sub(Variable('X'), Number(10)),
                Add(Number(1), Number(3))).simplified(),
            Variable('X'))

    def test_variable_invalid_name(self) -> None:
        with self.assertRaises(AssertionError):
            Variable('Foo (Bar)')

    def test_variable_neg(self) -> None:
        self.assertEqual(
            -Variable('X'),
            Variable('X', True))

    def test_variable_neg_variables(self) -> None:
        self.assertEqual(
            (-Variable('X')).variables(),
            [Variable('X', True)])

    def test_variable_simplified(self) -> None:
        self.assertEqual(
            Variable('X').simplified(),
            Variable('X'))
        self.assertEqual(
            Variable('X').simplified({Variable('X'): Number(42)}),
            Number(42))

    def test_length_simplified(self) -> None:
        self.assertEqual(
            Length('X').simplified(),
            Length('X'))
        self.assertEqual(
            Length('X').simplified({Length('X'): Number(42)}),
            Number(42))
        self.assertEqual(
            -Length('X').simplified({Length('X'): Number(42)}),
            Number(-42))

    def test_first_neg(self) -> None:
        self.assertEqual(
            -First('X'),
            First('X', True))

    def test_first_simplified(self) -> None:
        self.assertEqual(
            First('X').simplified(),
            First('X'))
        self.assertEqual(
            First('X').simplified({First('X'): Number(42)}),
            Number(42))
        self.assertEqual(
            -First('X').simplified({First('X'): Number(42)}),
            Number(-42))

    def test_last_neg(self) -> None:
        self.assertEqual(
            -Last('X'),
            Last('X', True))

    def test_last_simplified(self) -> None:
        self.assertEqual(
            Last('X').simplified(),
            Last('X'))
        self.assertEqual(
            Last('X').simplified({Last('X'): Number(42)}),
            Number(42))
        self.assertEqual(
            -Last('X').simplified({Last('X'): Number(42)}),
            Number(-42))

    def test_aggregate_simplified(self) -> None:
        self.assertEqual(
            Aggregate(Last('X')).simplified({Last('X'): Number(42)}),
            Aggregate(Number(42)))

    def test_named_aggregate_simplified(self) -> None:
        self.assertEqual(
            NamedAggregate(('Last', Last('X'))).simplified({Last('X'): Number(42)}),
            NamedAggregate(('Last', Number(42))))

    def test_relation_contains(self) -> None:
        self.assertTrue(
            Variable('X') in Less(Variable('X'), Number(42)))

    def test_relation_variables(self) -> None:
        self.assertEqual(
            Less(Variable('X'), Name('Y')).variables(),
            [Variable('X')])
        self.assertEqual(
            Less(Variable('X'), Variable('Y')).variables(),
            [Variable('X'), Variable('Y')])

    def test_less_neg(self) -> None:
        self.assertEqual(
            -Less(Variable('X'), Number(1)),
            GreaterEqual(Variable('X'), Number(1)))

    def test_less_simplified(self) -> None:
        self.assertEqual(
            Less(Variable('X'), Add(Number(21), Number(21))).simplified(),
            Less(Variable('X'), Number(42)))

    def test_less_equal_neg(self) -> None:
        self.assertEqual(
            -LessEqual(Variable('X'), Number(1)),
            Greater(Variable('X'), Number(1)))

    def test_less_equal_simplified(self) -> None:
        self.assertEqual(
            LessEqual(Variable('X'), Add(Number(21), Number(21))).simplified(),
            LessEqual(Variable('X'), Number(42)))

    def test_equal_neg(self) -> None:
        self.assertEqual(
            -Equal(Variable('X'), Number(1)),
            NotEqual(Variable('X'), Number(1)))

    def test_equal_simplified(self) -> None:
        self.assertEqual(
            Equal(Variable('X'), Add(Number(21), Number(21))).simplified(),
            Equal(Variable('X'), Number(42)))

    def test_greater_neg(self) -> None:
        self.assertEqual(
            -Greater(Variable('X'), Number(1)),
            LessEqual(Variable('X'), Number(1)))

    def test_greater_simplified(self) -> None:
        self.assertEqual(
            Greater(Variable('X'), Add(Number(21), Number(21))).simplified(),
            Greater(Variable('X'), Number(42)))

    def test_greater_equal_neg(self) -> None:
        self.assertEqual(
            -GreaterEqual(Variable('X'), Number(1)),
            Less(Variable('X'), Number(1)))

    def test_greater_equal_simplified(self) -> None:
        self.assertEqual(
            GreaterEqual(Variable('X'), Add(Number(21), Number(21))).simplified(),
            GreaterEqual(Variable('X'), Number(42)))

    def test_not_equal_neg(self) -> None:
        self.assertEqual(
            -NotEqual(Variable('X'), Number(1)),
            Equal(Variable('X'), Number(1)))

    def test_not_equal_simplified(self) -> None:
        self.assertEqual(
            NotEqual(Variable('X'), Add(Number(21), Number(21))).simplified(),
            NotEqual(Variable('X'), Number(42)))

    def test_slice_simplified(self) -> None:
        self.assertEqual(
            Slice('Buffer',
                  First('Buffer'),
                  Add(Last('Buffer'), Add(Number(21), Number(21)))).simplified(),
            Slice('Buffer',
                  First('Buffer'),
                  Add(Last('Buffer'), Number(42))))

    def test_if_simplified(self) -> None:
        self.assertEqual(
            If([(Variable('X'), Number(21)),
                (Variable('Y'), Add(Number(21), Number(21))),
                (Add(Number(21), Number(21)), Variable('Z'))]).simplified(),
            If([(Variable('X'), Number(21)),
                (Variable('Y'), Number(42)),
                (Number(42), Variable('Z'))]))

    def test_if_variables(self) -> None:
        self.assertEqual(
            If([(Variable('X'), Number(21)),
                (Variable('Y'), Add(Number(21), Number(21))),
                (Add(Number(21), Number(21)), Variable('Z'))]).variables(),
            [Variable('X'), Variable('Y'), Variable('Z')])

    def test_case_simplified(self) -> None:
        self.assertEqual(
            Case(Add(Number(21), Number(21)),
                 [(Variable('X'), Number(21)),
                  (Variable('Y'), Add(Number(21), Number(21))),
                  (Add(Number(21), Number(21)), Variable('Z'))]).simplified(),
            Case(Number(42),
                 [(Variable('X'), Number(21)),
                  (Variable('Y'), Number(42)),
                  (Number(42), Variable('Z'))]))

    def test_case_variables(self) -> None:
        self.assertEqual(
            Case(Add(Number(21), Number(21)),
                 [(Variable('X'), Number(21)),
                  (Variable('Y'), Add(Number(21), Number(21))),
                  (Add(Number(21), Number(21)), Variable('Z'))]).variables(),
            [Variable('X'), Variable('Y'), Variable('Z')])

    def test_range_simplified(self) -> None:
        self.assertEqual(
            Range(Number(1), Add(Number(21), Number(21))).simplified(),
            Range(Number(1), Number(42)))

    def test_quantified_expression_simplified(self) -> None:
        self.assertEqual(
            ForAllOf('X', Name('List'), Add(Last('Y'), Add(Number(21), Number(21)))).simplified(),
            ForAllOf('X', Name('List'), Add(Last('Y'), Number(42))))

    def test_quantified_expression_variables(self) -> None:
        self.assertEqual(
            ForAllOf('A', Name('List'), Add(Variable('X'),
                                            Add(Variable('Y'), Variable('Z')))).variables(),
            [Variable('X'), Variable('Y'), Variable('Z')])

    def test_expr_contains(self) -> None:
        self.assertTrue(
            Variable('X') in Or(Greater(Variable('Y'), Number(42)),
                                And(TRUE, Less(Variable('X'), Number(42)))))
        self.assertFalse(
            Variable('Z') in Or(Greater(Variable('Y'), Number(42)),
                                And(TRUE, Less(Variable('X'), Number(42)))))
        self.assertTrue(
            Less(Variable('X'), Number(42)) in Or(Greater(Variable('Y'), Number(42)),
                                                  And(TRUE, Less(Variable('X'), Number(42)))))
        self.assertFalse(
            Less(Variable('Z'), Number(42)) in Or(Greater(Variable('Y'), Number(42)),
                                                  And(TRUE, Less(Variable('X'), Number(1)))))

    def test_expr_variables(self) -> None:
        self.assertEqual(
            Or(Greater(Variable('Y'), Number(42)),
               And(TRUE, Less(Variable('X'), Number(42)))).variables(),
            [Variable('Y'), Variable('X')])
        self.assertEqual(
            Or(Greater(Variable('Y'), Number(42)),
               And(TRUE, Less(Variable('X'), Number(42)))).variables(),
            [Variable('Y'), Variable('X')])
        self.assertEqual(
            Or(Greater(Variable('Y'), Number(42)),
               And(TRUE, Less(Variable('X'), Number(42)))).variables(),
            [Variable('Y'), Variable('X')])
        self.assertEqual(
            Or(Greater(Variable('Y'), Number(42)),
               And(TRUE, Less(Variable('X'), Number(1)))).variables(),
            [Variable('Y'), Variable('X')])

    def test_expr_variables_duplicates(self) -> None:
        self.assertEqual(
            And(Variable('X'), Variable('Y'), Variable('X')).variables(),
            [Variable('X'), Variable('Y')])
        self.assertEqual(
            Or(Variable('X'), Variable('Y'), Variable('X')).variables(),
            [Variable('X'), Variable('Y')])
        self.assertEqual(
            Add(Variable('X'), Variable('Y'), Variable('X')).variables(),
            [Variable('X'), Variable('Y')])
        self.assertEqual(
            Mul(Variable('X'), Variable('Y'), Variable('X')).variables(),
            [Variable('X'), Variable('Y')])
        self.assertEqual(
            Sub(Variable('X'), Variable('X')).variables(),
            [Variable('X')])
        self.assertEqual(
            Div(Variable('X'), Variable('X')).variables(),
            [Variable('X')])
        self.assertEqual(
            Or(Greater(Variable('X'), Number(42)),
               And(TRUE, Less(Variable('X'), Number(1)))).variables(),
            [Variable('X')])

    def test_name_variables(self) -> None:
        self.assertEqual(
            Name('Z').variables(),
            [])

    def test_length_z3variables(self) -> None:
        self.assertEqual(
            Length('Z').variables(True),
            [Variable('Z__Length')])

    def test_last_z3variables(self) -> None:
        self.assertEqual(
            Last('Z').variables(True),
            [Variable('Z__Last')])

    def test_first_z3variables(self) -> None:
        self.assertEqual(
            First('Z').variables(True),
            [Variable('Z__First')])

    def test_size_z3variables(self) -> None:
        self.assertEqual(
            Size('Z').variables(True),
            [Variable('Z__Size')])

    def test_not_variables(self) -> None:
        self.assertEqual(
            Not(Variable('X')).variables(),
            [Variable('X')])
