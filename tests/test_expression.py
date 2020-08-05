# pylint: disable=too-many-lines

import pytest
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
    Expr,
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
from tests.utils import assert_equal

EXPR = Equal(Variable("UNDEFINED_1"), Variable("UNDEFINED_2"))


def multilinestr(string: str) -> str:
    assert all(
        l.startswith(15 * " ") for l in string.split("\n")[1:]
    ), "invalid format of multi-line string"
    return string.replace(15 * " ", "")


def test_true_neg() -> None:
    assert -TRUE == FALSE


def test_true_simplified() -> None:
    assert TRUE.simplified() == TRUE


def test_true_variables() -> None:
    assert TRUE.variables() == []


def test_true_z3expr() -> None:
    assert TRUE.z3expr() == z3.BoolVal(True)


def test_false_neg() -> None:
    assert -FALSE == TRUE


def test_false_simplified() -> None:
    assert FALSE.simplified() == FALSE


def test_false_variables() -> None:
    assert FALSE.variables() == []


def test_false_z3expr() -> None:
    assert FALSE.z3expr() == z3.BoolVal(False)


def test_not_neg() -> None:
    # pylint: disable=comparison-with-itself
    assert -Not(Variable("X")) == Variable("X")
    assert -Variable("X") != Variable("X")
    y = Variable("Y")
    assert y == y
    assert y != -y


def test_not_simplified() -> None:
    assert_equal(
        Not(Less(Variable("X"), Variable("Y"))).simplified(),
        GreaterEqual(Variable("X"), Variable("Y")),
    )
    assert_equal(
        Not(LessEqual(Variable("X"), Variable("Y"))).simplified(),
        Greater(Variable("X"), Variable("Y")),
    )
    assert_equal(
        Not(Equal(Variable("X"), Variable("Y"))).simplified(),
        NotEqual(Variable("X"), Variable("Y")),
    )
    assert_equal(
        Not(GreaterEqual(Variable("X"), Variable("Y"))).simplified(),
        Less(Variable("X"), Variable("Y")),
    )
    assert_equal(
        Not(Greater(Variable("X"), Variable("Y"))).simplified(),
        LessEqual(Variable("X"), Variable("Y")),
    )
    assert_equal(
        Not(NotEqual(Variable("X"), Variable("Y"))).simplified(),
        Equal(Variable("X"), Variable("Y")),
    )


def test_not_z3expr() -> None:
    assert Not(TRUE).z3expr() == z3.Not(z3.BoolVal(True))
    with pytest.raises(TypeError):
        Not(Variable("X")).z3expr()


def test_bin_expr_findall() -> None:
    assert Less(Variable("X"), Number(1)).findall(lambda x: isinstance(x, Number)) == [Number(1)]


def test_bin_expr_substituted() -> None:
    assert_equal(
        Less(Variable("X"), Number(1)).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        Less(Variable("P_X"), Number(1)),
    )
    assert_equal(
        Sub(Variable("X"), Number(1)).substituted(
            lambda x: Variable("Y") if x == Sub(Variable("X"), Number(1)) else x
        ),
        Variable("Y"),
    )
    assert_equal(
        NotEqual(Variable("X"), Number(1)).substituted(
            lambda x: Variable(f"P_{x}")
            if isinstance(x, Variable)
            else (Equal(x.left, x.right) if isinstance(x, NotEqual) else x)
        ),
        Equal(Variable("P_X"), Number(1)),
    )


def test_ass_expr_findall() -> None:
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Variable("Y"), Number(2)).findall(
            lambda x: isinstance(x, Number)
        ),
        [Number(1), Number(2)],
    )


def test_ass_expr_substituted() -> None:
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        And(Equal(Variable("P_X"), Number(1)), Variable("P_Y")),
    )
    assert_equal(
        Mul(Variable("X"), Number(1)).substituted(
            lambda x: Variable("Y") if x == Mul(Variable("X"), Number(1)) else x
        ),
        Variable("Y"),
    )
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}")
            if isinstance(x, Variable)
            else (Or(*x.terms) if isinstance(x, And) else x)
        ),
        Or(Equal(Variable("P_X"), Number(1)), Variable("P_Y")),
    )


def test_log_expr_str() -> None:
    assert_equal(
        str(And(Variable("A"), Or(Variable("B"), Variable("C")), Variable("D"))),
        multilinestr(
            """A
               and (B
                    or C)
               and D"""
        ),
    )
    assert_equal(
        str(AndThen(Variable("A"), OrElse(Variable("B"), Variable("C")), Variable("D"))),
        multilinestr(
            """A
               and then (B
                         or else C)
               and then D"""
        ),
    )


def test_and_neg() -> None:
    assert -And(Variable("X"), Number(1)) == And(-Variable("X"), Number(-1))


def test_and_variables() -> None:
    assert_equal(
        And(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
    )


def test_and_contains() -> None:
    assert Variable("X") in And(TRUE, Less(Variable("X"), Number(42)))
    assert Variable("Y") not in And(TRUE, Less(Variable("X"), Number(42)))


def test_and_simplified() -> None:
    assert And(TRUE, TRUE).simplified() == TRUE
    assert And(TRUE, FALSE, TRUE).simplified() == FALSE
    assert And(TRUE, EXPR).simplified() == EXPR
    assert And(EXPR, TRUE).simplified() == EXPR
    assert And(EXPR, FALSE).simplified() == FALSE
    assert And(FALSE, EXPR).simplified() == FALSE


def test_and_str() -> None:
    assert str(And(Variable("X"), Variable("Y"))) == "X\nand Y"
    assert str(And()) == "True"


def test_or_neg() -> None:
    assert -Or(Variable("X"), Number(1)) == Or(-Variable("X"), Number(-1))


def test_or_variables() -> None:
    assert Or(Variable("X"), Variable("Y"), Call("Z")).variables() == [Variable("X"), Variable("Y")]


def test_or_contains() -> None:
    assert Variable("X") in Or(Less(Variable("X"), Number(42)), TRUE)
    assert Variable("Y") not in Or(Less(Variable("X"), Number(42)), TRUE)


def test_or_simplified() -> None:
    assert Or(TRUE, TRUE).simplified() == TRUE
    assert Or(TRUE, EXPR).simplified() == TRUE
    assert Or(EXPR, TRUE).simplified() == TRUE


def test_or_str() -> None:
    assert str(Or(Variable("X"), Variable("Y"))) == "X\nor Y"
    assert str(Or()) == "True"


def test_undefined_neg() -> None:
    assert -UNDEFINED == -UNDEFINED


def test_undefined_simplified() -> None:
    assert UNDEFINED.simplified() == UNDEFINED


def test_undefined_str() -> None:
    assert str(UNDEFINED) == "__UNDEFINED__"


def test_number_neg() -> None:
    assert -Number(42) == Number(-42)


def test_number_simplified() -> None:
    assert Number(42).simplified() == Number(42)


def test_number_add() -> None:
    assert Number(5) + Number(3) == Number(8)


def test_number_substituted() -> None:
    assert Number(5) - Number(3) == Number(2)


def test_number_mul() -> None:
    assert Number(4) * Number(2) == Number(8)


def test_number_div() -> None:
    assert Number(4) // Number(2) == Number(2)


def test_number_pow() -> None:
    assert Number(2) ** Number(4) == Number(16)


def test_number_eq() -> None:
    # pylint: disable=unneeded-not
    assert Number(1) == Number(1)
    assert Number(1, 10) == Number(1, 16)
    assert Number(42, 16) == Number(42, 10)
    assert not Number(1) == Number(2)
    assert not Number(1, 16) == Number(2, 16)


def test_number_ne() -> None:
    # pylint: disable=unneeded-not
    assert not Number(1) != Number(1)
    assert not Number(1, 10) != Number(1, 16)
    assert not Number(42, 16) != Number(42, 10)
    assert Number(1) != Number(2)
    assert Number(1, 16) != Number(2, 16)


def test_number_lt() -> None:
    # pylint: disable=unneeded-not
    assert Number(1) < Number(2)
    assert not Number(2) < Number(2)
    assert not Number(3) < Number(2)
    assert not Variable("X") < Number(2)
    assert not Number(2) < Variable("X")


def test_number_le() -> None:
    # pylint: disable=unneeded-not
    assert Number(1) <= Number(2)
    assert Number(2) <= Number(2)
    assert not Number(3) <= Number(2)
    assert not Variable("X") <= Number(2)
    assert not Number(2) <= Variable("X")


def test_number_gt() -> None:
    # pylint: disable=unneeded-not
    assert not Number(1) > Number(2)
    assert not Number(2) > Number(2)
    assert Number(3) > Number(2)
    assert not Variable("X") > Number(2)
    assert not Number(2) > Variable("X")


def test_number_ge() -> None:
    # pylint: disable=unneeded-not
    assert not Number(1) >= Number(2)
    assert Number(2) >= Number(2)
    assert Number(3) >= Number(2)
    assert not Variable("X") >= Number(2)
    assert not Number(2) >= Variable("X")


def test_number_hashable() -> None:
    assert {Number(1), Number(2)}


def test_add_neg() -> None:
    assert -Add(Variable("X"), Number(1)) == Add(Variable("X", True), Number(-1))


def test_add_variables() -> None:
    assert_equal(
        Add(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
    )


def test_add_simplified() -> None:
    assert Add(Variable("X"), Number(1)).simplified() == Add(Variable("X"), Number(1))
    assert Add(Variable("X"), Number(0)).simplified() == Variable("X")
    assert Add(Number(2), Number(3), Number(5)).simplified() == Number(10)
    assert Add(Variable("X"), Variable("Y"), Variable("X", True)).simplified() == Variable("Y")
    assert Add(Variable("X"), Variable("Y"), Variable("X"), -Variable("X")).simplified() == Add(
        Variable("X"), Variable("Y")
    )


def test_add_lt() -> None:
    # pylint: disable=unneeded-not
    assert Add(Variable("X"), Number(1)) < Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(2)) < Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(3)) < Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(1)) < Add(Variable("Y"), Number(2))
    assert not Add(Variable("X"), Number(2)) < Add(Variable("Y"), Number(1))
    assert not Add(Variable("X"), Number(2)) < Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_le() -> None:
    # pylint: disable=unneeded-not
    assert Add(Variable("X"), Number(1)) <= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(2)) <= Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(3)) <= Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(1)) <= Add(Variable("Y"), Number(2))
    assert not Add(Variable("X"), Number(2)) <= Add(Variable("Y"), Number(1))
    assert not Add(Variable("X"), Number(2)) <= Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_gt() -> None:
    # pylint: disable=unneeded-not
    assert not Add(Variable("X"), Number(1)) > Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(2)) > Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(3)) > Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(1)) > Add(Variable("Y"), Number(2))
    assert not Add(Variable("X"), Number(2)) > Add(Variable("Y"), Number(1))
    assert not Add(Variable("X"), Number(2)) > Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_ge() -> None:
    # pylint: disable=unneeded-not
    assert not Add(Variable("X"), Number(1)) >= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(2)) >= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(3)) >= Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(1)) >= Add(Variable("Y"), Number(2))
    assert not Add(Variable("X"), Number(2)) >= Add(Variable("Y"), Number(1))
    assert not Add(Variable("X"), Number(2)) >= Add(Variable("Y"), Variable("Z"), Number(1))


def test_mul_neg() -> None:
    assert -Mul(Variable("X"), Number(2)) == Mul(Variable("X"), Number(-2))


def test_mul_variables() -> None:
    assert_equal(
        Mul(Variable("X"), Variable("Y"), Call("Z")).variables(), [Variable("X"), Variable("Y")]
    )


def test_mul_simplified() -> None:
    assert Mul(Variable("X"), Number(2)).simplified() == Mul(Variable("X"), Number(2))
    assert Mul(Variable("X"), Number(1)).simplified() == Variable("X")
    assert Mul(Number(2), Number(3), Number(5)).simplified() == Number(30)


def test_sub_neg() -> None:
    assert -Sub(Number(1), Variable("X")) == Sub(Number(-1), Variable("X"))


def test_sub_variables() -> None:
    assert Sub(Variable("X"), Call("Y")).variables() == [Variable("X")]


def test_sub_simplified() -> None:
    assert Sub(Number(1), Variable("X")).simplified() == Add(Number(1), -Variable("X"))
    assert Sub(Variable("X"), Number(1)).simplified() == Add(Variable("X"), Number(-1))
    assert Sub(Number(6), Number(2)).simplified() == Number(4)
    assert Sub(Variable("X"), Variable("Y")).simplified() == Add(Variable("X"), -Variable("Y"))
    assert (
        Equal(Variable("Q"), Sub(Add(Variable("Y"), Variable("Q")), Variable("Y")))
        .simplified()
        .simplified()
        == TRUE
    )


def test_div_neg() -> None:
    assert -Div(Variable("X"), Number(1)) == Div(Variable("X", True), Number(1))


def test_div_variables() -> None:
    assert Div(Variable("X"), Call("Y")).variables() == [Variable("X")]
    assert Div(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_div_simplified() -> None:
    assert Div(Variable("X"), Number(1)).simplified() == Div(Variable("X"), Number(1))
    assert Div(Number(6), Number(2)).simplified() == Number(3)
    assert Div(Number(9), Number(2)).simplified() == Div(Number(9), Number(2))


def test_pow_simplified() -> None:
    assert Pow(Variable("X"), Number(1)).simplified() == Pow(Variable("X"), Number(1))
    assert Pow(Variable("X"), Add(Number(1), Number(1))).simplified() == Pow(
        Variable("X"), Number(2)
    )
    assert Pow(Number(6), Number(2)).simplified() == Number(36)


def test_pow_variables() -> None:
    assert Pow(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_mod_simplified() -> None:
    assert Mod(Variable("X"), Number(1)).simplified() == Mod(Variable("X"), Number(1))
    assert Mod(Variable("X"), Add(Number(1), Number(1))).simplified() == Mod(
        Variable("X"), Number(2)
    )
    assert Mod(Number(6), Number(2)).simplified() == Number(0)


def test_mod_variables() -> None:
    assert Mod(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_term_simplified() -> None:
    assert_equal(
        Add(
            Mul(Number(1), Number(6)), Sub(Variable("X"), Number(10)), Add(Number(1), Number(3))
        ).simplified(),
        Variable("X"),
    )


@pytest.mark.skipif(not __debug__, reason="depends on contract")
def test_variable_invalid_name() -> None:
    with pytest.raises(AssertionError):
        Variable("Foo (Bar)")


def test_variable_neg() -> None:
    assert -Variable("X") == Variable("X", True)


def test_variable_neg_variables() -> None:
    assert (-Variable("X")).variables() == [Variable("X", True)]


def test_variable_substituted() -> None:
    assert_equal(
        Variable("X").substituted(lambda x: Number(42) if x == Variable("X") else x), Number(42)
    )


def test_variable_simplified() -> None:
    assert Variable("X").simplified() == Variable("X")


def test_variable_z3expr() -> None:
    assert Variable("X").z3expr() == z3.Int("X")
    assert Variable("X", True).z3expr() == -z3.Int("X")


def test_attribute() -> None:
    assert isinstance(Size("X"), Attribute)
    assert isinstance(Length("X"), Attribute)
    assert isinstance(First("X"), Attribute)
    assert isinstance(Last("X"), Attribute)
    assert isinstance(Range("X"), Attribute)
    assert isinstance(Old("X"), Attribute)
    assert isinstance(Result("X"), Attribute)
    assert isinstance(Constrained("X"), Attribute)
    assert First("X") == First(Variable("X"))
    assert First("X") == First(ID("X"))
    assert First("X") == First(Variable(ID("X")))


def test_attribute_neg() -> None:
    assert -First("X") == First("X", True)


def test_attributes_findall() -> None:
    assert First("X").findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_attribute_substituted() -> None:
    assert_equal(First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(42))
    assert_equal(
        -First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(-42)
    )
    assert_equal(
        First("X").substituted(lambda x: Call("Y") if x == Variable("X") else x), First(Call("Y")),
    )
    assert_equal(
        -First("X").substituted(lambda x: Call("Y") if x == Variable("X") else x),
        -First(Call("Y")),
    )
    assert_equal(
        -First("X").substituted(
            lambda x: Variable(f"P_{x}")
            if isinstance(x, Variable)
            else (Last(x.prefix) if isinstance(x, First) else x)
        ),
        -Last(Variable("P_X")),
    )


def test_attribute_simplified() -> None:
    assert First("X").simplified() == First("X")


def test_attribute_str() -> None:
    assert str(First("X")) == "X'First"


def test_attribute_variables() -> None:
    assert First("X").variables() == [Variable("X")]
    assert First("X").variables() == [Variable("X")]
    with pytest.raises(TypeError):
        First(Call("X")).variables()


def test_attribute_z3expr() -> None:
    assert First("X").z3expr() == z3.Int("X'First")
    with pytest.raises(TypeError):
        First(Call("X")).z3expr()


def test_attribute_expression_substituted() -> None:
    assert_equal(
        Val("X", Variable("Y")).substituted(
            lambda x: Number(42) if x == Val("X", Variable("Y")) else x
        ),
        Number(42),
    )
    assert_equal(
        -Val("X", Variable("Y")).substituted(
            lambda x: Number(42) if x == Val("X", Variable("Y")) else x
        ),
        Number(-42),
    )
    assert_equal(
        Val("X", Variable("Y")).substituted(lambda x: Call("Y") if x == Variable("Y") else x),
        Val("X", Call("Y")),
    )
    assert_equal(
        -Val("X", Variable("Y")).substituted(lambda x: Call("Y") if x == Variable("Y") else x),
        -Val("X", Call("Y")),
    )
    assert_equal(
        -Val("X", Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}")
            if isinstance(x, Variable)
            else (Pos(x.prefix, x.expression) if isinstance(x, Val) else x)
        ),
        -Pos("P_X", Variable("P_Y")),
    )


def test_attribute_expression_simplified() -> None:
    assert Val("X", Add(Number(1), Number(1))).simplified() == Val("X", Number(2))


def test_attribute_expression_str() -> None:
    assert str(Val("X", Number(1))) == "X'Val (1)"


def test_aggregate_substituted() -> None:
    assert_equal(
        Aggregate(First("X")).substituted(
            lambda x: Number(42) if x == Aggregate(First("X")) else x
        ),
        Number(42),
    )
    assert_equal(
        Aggregate(First("X")).substituted(lambda x: Number(42) if x == First("X") else x),
        Aggregate(Number(42)),
    )
    assert_equal(
        Aggregate(Variable("X")).substituted(
            lambda x: Variable(f"P_{x}")
            if isinstance(x, Variable)
            else (Aggregate(*(x.elements + [Variable("Y")])) if isinstance(x, Aggregate) else x)
        ),
        Aggregate(Variable("P_X"), Variable("P_Y")),
    )


def test_aggregate_simplified() -> None:
    assert Aggregate(Add(Number(1), Number(1))).simplified() == Aggregate(Number(2))


def test_named_aggregate_substituted() -> None:
    assert_equal(
        NamedAggregate(("First", First("X"))).substituted(
            lambda x: Number(42) if x == NamedAggregate(("First", First("X"))) else x
        ),
        Number(42),
    )
    assert_equal(
        NamedAggregate(("First", First("X"))).substituted(
            lambda x: Number(42) if x == First("X") else x
        ),
        NamedAggregate(("First", Number(42))),
    )
    assert_equal(
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


def test_named_aggregate_simplified() -> None:
    assert_equal(
        NamedAggregate(("First", Add(Number(1), Number(1)))).simplified(),
        NamedAggregate(("First", Number(2))),
    )


def test_relation_substituted() -> None:
    assert_equal(
        Equal(Variable("X"), Variable("Y")).substituted(
            lambda x: Number(1) if x == Variable("X") else x
        ),
        Equal(Number(1), Variable("Y")),
    )
    assert_equal(
        Equal(Variable("X"), Variable("Y")).substituted(
            lambda x: Number(1) if x == Variable("Y") else x
        ),
        Equal(Variable("X"), Number(1)),
    )
    assert_equal(
        Equal(Variable("X"), Variable("Y")).substituted(
            lambda x: Number(1) if x == Equal(Variable("X"), Variable("Y")) else x
        ),
        Number(1),
    )


def test_relation_simplified() -> None:
    assert_equal(
        Equal(Variable("X"), Add(Number(1), Number(1))).simplified(),
        Equal(Variable("X"), Number(2)),
    )
    assert_equal(
        Equal(Add(Number(1), Number(1)), Variable("X")).simplified(),
        Equal(Number(2), Variable("X")),
    )
    assert_equal(
        Equal(Add(Number(1), Number(1)), Add(Number(1), Number(1))).simplified(), TRUE,
    )


def test_relation_contains() -> None:
    assert Variable("X") in Less(Variable("X"), Number(42))


def test_relation_variables() -> None:
    assert Less(Variable("X"), Call("Y")).variables() == [Variable("X")]
    assert Less(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_less_neg() -> None:
    assert -Less(Variable("X"), Number(1)) == GreaterEqual(Variable("X"), Number(1))


def test_less_simplified() -> None:
    assert Less(Number(0), Number(1)).simplified() == TRUE
    assert Less(Number(1), Number(1)).simplified() == FALSE
    assert Less(Number(2), Number(1)).simplified() == FALSE


def test_less_equal_neg() -> None:
    assert -LessEqual(Variable("X"), Number(1)) == Greater(Variable("X"), Number(1))


def test_less_equal_simplified() -> None:
    assert LessEqual(Number(0), Number(1)).simplified() == TRUE
    assert LessEqual(Number(1), Number(1)).simplified() == TRUE
    assert LessEqual(Number(2), Number(1)).simplified() == FALSE


def test_equal_neg() -> None:
    assert -Equal(Variable("X"), Number(1)) == NotEqual(Variable("X"), Number(1))


def test_equal_simplified() -> None:
    assert Equal(Number(0), Number(1)).simplified() == FALSE
    assert Equal(Number(1), Number(1)).simplified() == TRUE
    assert Equal(Number(2), Number(1)).simplified() == FALSE


def test_greater_neg() -> None:
    assert -Greater(Variable("X"), Number(1)) == LessEqual(Variable("X"), Number(1))


def test_greater_simplified() -> None:
    assert Greater(Number(0), Number(1)).simplified() == FALSE
    assert Greater(Number(1), Number(1)).simplified() == FALSE
    assert Greater(Number(2), Number(1)).simplified() == TRUE


def test_greater_equal_neg() -> None:
    assert -GreaterEqual(Variable("X"), Number(1)) == Less(Variable("X"), Number(1))


def test_greater_equal_simplified() -> None:
    assert GreaterEqual(Number(0), Number(1)).simplified() == FALSE
    assert GreaterEqual(Number(1), Number(1)).simplified() == TRUE
    assert GreaterEqual(Number(2), Number(1)).simplified() == TRUE


def test_not_equal_neg() -> None:
    assert -NotEqual(Variable("X"), Number(1)) == Equal(Variable("X"), Number(1))


def test_not_equal_simplified() -> None:
    assert NotEqual(Number(0), Number(1)).simplified() == TRUE
    assert NotEqual(Number(1), Number(1)).simplified() == FALSE
    assert NotEqual(Number(2), Number(1)).simplified() == TRUE


def test_in_neg() -> None:
    assert -In(Variable("X"), Number(1)) == NotIn(Variable("X"), Number(1))


def test_in_simplified() -> None:
    assert_equal(
        In(Variable("X"), Add(Number(21), Number(21))).simplified(), In(Variable("X"), Number(42)),
    )


def test_in_str() -> None:
    assert str(In(Variable("X"), Variable("Y"))) == "X in Y"


def test_not_in_neg() -> None:
    assert -NotIn(Variable("X"), Number(1)) == In(Variable("X"), Number(1))


def test_not_in_simplified() -> None:
    assert_equal(
        NotIn(Variable("X"), Add(Number(21), Number(21))).simplified(),
        NotIn(Variable("X"), Number(42)),
    )


def test_not_in_str() -> None:
    assert str(NotIn(Variable("X"), Variable("Y"))) == "X not in Y"


def test_slice_neg() -> None:
    assert Slice(Variable("X"), Variable("Y"), Variable("Z")) == -Slice(
        Variable("X"), Variable("Y"), Variable("Z")
    )


def test_slice_substituted() -> None:
    assert_equal(
        Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
            lambda x: Variable("Z") if x == Variable("X") else x
        ),
        Slice(Variable("Z"), Variable("Y"), Variable("Y")),
    )
    assert_equal(
        Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
            lambda x: Variable("Z") if x == Variable("Y") else x
        ),
        Slice(Variable("X"), Variable("Z"), Variable("Z")),
    )
    assert_equal(
        Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}")
            if isinstance(x, Variable)
            else (Slice(Variable("X"), Variable("Y"), Variable("Z")) if isinstance(x, Slice) else x)
        ),
        Slice(Variable("P_X"), Variable("P_Y"), Variable("P_Z")),
    )
    assert_equal(
        Slice(Variable("X"), Variable("Y"), Variable("Y")).substituted(
            lambda x: Variable("Z") if isinstance(x, Slice) else x
        ),
        Variable("Z"),
    )


def test_slice_simplified() -> None:
    assert_equal(
        Slice(
            Variable("Buffer"), First("Buffer"), Add(Last("Buffer"), Add(Number(21), Number(21))),
        ).simplified(),
        Slice(Variable("Buffer"), First("Buffer"), Add(Last("Buffer"), Number(42))),
    )


def test_if_findall() -> None:
    assert_equal(
        If(
            [
                (Equal(Variable("X"), Number(42)), Number(21)),
                (Variable("Y"), Number(42)),
                (Number(42), Variable("Z")),
            ]
        ).findall(lambda x: isinstance(x, Number)),
        [Number(42), Number(21), Number(42), Number(42)],
    )


def test_if_substituted() -> None:
    if_expr = If(
        [
            (Equal(Variable("X"), Number(42)), Number(21)),
            (Variable("Y"), Number(42)),
            (Number(42), Variable("Z")),
        ]
    )

    assert_equal(
        if_expr.substituted(lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x),
        If(
            [
                (Equal(Variable("P_X"), Number(42)), Number(21)),
                (Variable("P_Y"), Number(42)),
                (Number(42), Variable("P_Z")),
            ]
        ),
    )
    assert_equal(
        if_expr.substituted(
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
    assert_equal(
        if_expr.substituted(lambda x: Variable("Z") if isinstance(x, If) else x), Variable("Z"),
    )


def test_if_simplified() -> None:
    assert_equal(
        If(
            [
                (Variable("X"), Number(21)),
                (Variable("Y"), Add(Number(21), Number(21))),
                (Add(Number(21), Number(21)), Variable("Z")),
            ]
        ).simplified(),
        If([(Variable("X"), Number(21)), (Variable("Y"), Number(42)), (Number(42), Variable("Z"))]),
    )
    assert If([(TRUE, Variable("X"))]).simplified() == Variable("X")


def test_if_variables() -> None:
    assert_equal(
        If(
            [
                (Variable("X"), Number(21)),
                (Variable("Y"), Add(Number(21), Number(21))),
                (Add(Number(21), Number(21)), Variable("Z")),
            ],
        ).variables(),
        [Variable("X"), Variable("Y"), Variable("Z")],
    )
    assert_equal(
        If([(Variable("X"), Number(21))], Variable("Z")).variables(),
        [Variable("X"), Variable("Z")],
    )


def test_if_z3expr() -> None:
    assert If([]).z3expr() == z3.BoolVal(False)


def test_if_str() -> None:
    assert_equal(
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


def test_case_substituted() -> None:
    assert_equal(
        Case(Variable("X"), [(Variable("Y"), Variable("Z"))]).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        Case(Variable("P_X"), [(Variable("P_Y"), Variable("P_Z"))]),
    )
    assert_equal(
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
    assert_equal(
        Case(Variable("X"), [(Variable("Y"), Variable("Z"))]).substituted(
            lambda x: Variable("Z") if isinstance(x, Case) else x
        ),
        Variable("Z"),
    )


def test_case_simplified() -> None:
    assert_equal(
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


def test_case_variables() -> None:
    assert_equal(
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


def test_case_str() -> None:
    assert_equal(
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


def test_value_range_simplified() -> None:
    assert_equal(
        ValueRange(Number(1), Add(Number(21), Number(21))).simplified(),
        ValueRange(Number(1), Number(42)),
    )


def test_value_range_substituted() -> None:
    expr = ValueRange(lower=First("Test"), upper=Sub(Last("Test"), Number(1)))

    def func(expr: Expr) -> Expr:
        if expr == First("Test"):
            return Number(1)
        if expr == Last("Test"):
            return Number(11)
        return expr

    assert expr.substituted(func) == ValueRange(lower=Number(1), upper=Sub(Number(11), Number(1)))

    assert_equal(
        ValueRange(lower=Variable("X"), upper=Variable("Y")).substituted(
            lambda x: Variable("Z") if isinstance(x, ValueRange) else x
        ),
        Variable("Z"),
    )


def test_quantified_expression_simplified() -> None:
    assert_equal(
        ForAllOf("X", Variable("List"), Add(Last("Y"), Add(Number(21), Number(21)))).simplified(),
        ForAllOf("X", Variable("List"), Add(Last("Y"), Number(42))),
    )


def test_quantified_expression_variables() -> None:
    assert_equal(
        ForAllOf(
            "A", Variable("List"), Add(Variable("X"), Add(Variable("Y"), Variable("Z")))
        ).variables(),
        [Variable("List"), Variable("X"), Variable("Y"), Variable("Z")],
    )


def test_quantified_expression_str() -> None:
    assert str(ForAllOf("X", Variable("Y"), Variable("X"))) == "(for all X of Y =>\n    X)"
    assert str(ForAllIn("X", Variable("Y"), Variable("X"))) == "(for all X in Y =>\n    X)"


def test_expr_contains() -> None:
    assert Variable("X") in Or(
        Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
    )
    assert Variable("Z") not in Or(
        Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
    )
    assert Less(Variable("X"), Number(42)) in Or(
        Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
    )
    assert Less(Variable("Z"), Number(42)) not in Or(
        Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
    )


def test_expr_variables() -> None:
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
        ).variables(),
        [Variable("Y"), Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
        ).variables(),
        [Variable("Y"), Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
        ).variables(),
        [Variable("Y"), Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
        ).variables(),
        [Variable("Y"), Variable("X")],
    )


def test_expr_variables_duplicates() -> None:
    assert_equal(
        And(Variable("X"), Variable("Y"), Variable("X")).variables(),
        [Variable("X"), Variable("Y")],
    )
    assert_equal(
        Or(Variable("X"), Variable("Y"), Variable("X")).variables(), [Variable("X"), Variable("Y")],
    )
    assert_equal(
        Add(Variable("X"), Variable("Y"), Variable("X")).variables(),
        [Variable("X"), Variable("Y")],
    )
    assert_equal(
        Mul(Variable("X"), Variable("Y"), Variable("X")).variables(),
        [Variable("X"), Variable("Y")],
    )
    assert_equal(Sub(Variable("X"), Variable("X")).variables(), [Variable("X")])
    assert_equal(Div(Variable("X"), Variable("X")).variables(), [Variable("X")])
    assert_equal(
        Or(
            Greater(Variable("X"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
        ).variables(),
        [Variable("X")],
    )


@pytest.mark.skipif(not __debug__, reason="depends on contract")
def test_expr_substituted_pre() -> None:
    with pytest.raises(AssertionError):
        Number(1).substituted()
    with pytest.raises(AssertionError):
        Add(Number(1), Number(1)).substituted()
    with pytest.raises(AssertionError):
        Number(1).substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        Add(Number(1), Number(1)).substituted(lambda x: x, {})


def test_length_z3variables() -> None:
    assert Length("Z").variables() == [Variable("Z")]


def test_last_z3variables() -> None:
    assert Last("Z").variables() == [Variable("Z")]


def test_first_z3variables() -> None:
    assert First("Z").variables() == [Variable("Z")]


def test_size_z3variables() -> None:
    assert Size("Z").variables() == [Variable("Z")]


def test_not_variables() -> None:
    assert Not(Variable("X")).variables() == [Variable("X")]


def test_number_str() -> None:
    assert str(Number(15)) == "15"


def test_number_str_long() -> None:
    assert str(Number(539535)) == "539535"


def test_number_str_neg_long() -> None:
    assert str(Number(-539535)) == "(-539535)"


def test_number_str_hex() -> None:
    assert str(Number(4096, 16)) == "16#1000#"


def test_number_str_neg_hex() -> None:
    assert str(Number(-4096, 16)) == "(-16#1000#)"


def test_number_str_dec() -> None:
    assert str(Number(4096, 10)) == "10#4096#"


def test_number_str_oct() -> None:
    assert str(Number(45432, 8)) == "8#130570#"


def test_number_str_neg_oct() -> None:
    assert str(Number(-45432, 8)) == "(-8#130570#)"


def test_number_str_bin() -> None:
    assert str(Number(454, 2)) == "2#111000110#"


def test_expr_str() -> None:
    assert_equal(
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
    assert_equal(
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


def test_call_str() -> None:
    assert str(Add(Number(1), Call("Test", []))) == "1 + Test"
    assert str(Add(Number(1), -Call("Test", []))) == "1 - Test"
    assert str(Call("Test", [])) == "Test"
    assert str(-Call("Test", [])) == "(-Test)"
