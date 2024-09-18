from __future__ import annotations

import math
import textwrap
from collections.abc import Callable, Mapping

import pytest

from rflx import ty
from rflx.expr import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AndThen,
    Attribute,
    Call,
    CaseExpr,
    Comprehension,
    Conversion,
    DeltaMessageAggregate,
    Div,
    Equal,
    Expr,
    First,
    ForAllIn,
    ForAllOf,
    ForSomeIn,
    Greater,
    GreaterEqual,
    HasData,
    Head,
    IfExpr,
    In,
    Indexed,
    Last,
    Length,
    Less,
    LessEqual,
    Literal,
    MessageAggregate,
    Mod,
    Mul,
    Name,
    Neg,
    Not,
    NotEqual,
    NotIn,
    Number,
    Opaque,
    Or,
    OrElse,
    Pow,
    Precedence,
    Present,
    QualifiedExpr,
    Rem,
    Selected,
    Size,
    String,
    Sub,
    Val,
    Valid,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID, StrID
from rflx.model import Integer
from rflx.rapidflux import Location, RecordFluxError
from tests.data import models
from tests.utils import assert_equal, check_regex

EXPR = Equal(Variable("UNDEFINED_1"), Variable("UNDEFINED_2"))
TINY_INT = Integer("P::Tiny", Number(1), Number(3), Number(8), location=Location((1, 2)))
INT = Integer("P::Int", Number(1), Number(100), Number(8), location=Location((3, 2)))

INT_TY = ty.Integer("I", ty.Bounds(10, 100))
ENUM_TY = ty.Enumeration("E", [ID("E1"), ID("E2")])
MSG_TY = ty.Message("M")
SEQ_TY = ty.Sequence("S", ty.Message("M"))


def assert_type(expr: Expr, type_: ty.Type) -> None:
    expr.check_type(type_).propagate()
    assert expr.type_ == type_


def assert_type_error(expr: Expr, regex: str) -> None:
    check_regex(regex)
    with pytest.raises(RecordFluxError, match=regex):
        expr.check_type(ty.Any()).propagate()


def test_true_type() -> None:
    assert_type(
        TRUE,
        ty.BOOLEAN,
    )


def test_true_simplified() -> None:
    assert TRUE.simplified() == TRUE


def test_true_variables() -> None:
    assert not TRUE.variables()


def test_false_type() -> None:
    assert_type(
        FALSE,
        ty.BOOLEAN,
    )


def test_false_simplified() -> None:
    assert FALSE.simplified() == FALSE


def test_false_variables() -> None:
    assert not FALSE.variables()


def test_not_type() -> None:
    assert_type(
        Not(Variable("X", type_=ty.BOOLEAN)),
        ty.BOOLEAN,
    )


def test_not_type_error() -> None:
    assert_type_error(
        Not(Variable("X", type_=INT_TY, location=Location((10, 20)))),
        r'^<stdin>:10:20: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r'<stdin>:10:20: error: found integer type "I" \(10 \.\. 100\)$',
    )


def test_not_neg() -> None:
    assert -Not(Variable("X")) == Variable("X")
    assert -Variable("X") != Variable("X")
    y = Variable("Y")
    assert y == y  # noqa: PLR0124
    assert y != -y


def test_not_findall() -> None:
    assert Not(Variable("X")).findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_not_substituted() -> None:
    assert_equal(
        Not(Variable("X")).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        Not(Variable("P_X")),
    )
    assert_equal(
        Not(Variable("X")).substituted(lambda x: Variable("Y") if x == Not(Variable("X")) else x),
        Variable("Y"),
    )


def test_not_simplified() -> None:
    assert_equal(Not(TRUE).simplified(), FALSE)
    assert_equal(Not(FALSE).simplified(), TRUE)
    assert_equal(Or(FALSE, Not(And(TRUE, FALSE)), EXPR).simplified(), TRUE)
    assert_equal(And(TRUE, Not(Or(FALSE, TRUE)), EXPR).simplified(), FALSE)
    assert_equal(
        Not(
            And(FALSE, Not(And(TRUE, FALSE))),
        ).simplified(),
        TRUE,
    )
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


def test_bin_expr_findall() -> None:
    assert Less(Variable("X"), Number(1)).findall(lambda x: isinstance(x, Number)) == [Number(1)]


def test_bin_expr_substituted() -> None:
    assert_equal(
        Less(Variable("X"), Number(1)).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        Less(Variable("P_X"), Number(1)),
    )
    assert_equal(
        Sub(Variable("X"), Number(1)).substituted(
            lambda x: Variable("Y") if x == Sub(Variable("X"), Number(1)) else x,
        ),
        Variable("Y"),
    )
    assert_equal(
        NotEqual(Variable("X"), Number(1)).substituted(
            lambda x: (
                Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Equal(x.left, x.right) if isinstance(x, NotEqual) else x)
            ),
        ),
        Equal(Variable("P_X"), Number(1)),
    )


def test_bin_expr_substituted_location() -> None:
    expr = Less(Variable("X"), Number(1), location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


def test_ass_expr_str() -> None:
    assert (
        str(
            Add(
                Number(1),
                IfExpr([(Variable("A"), Variable("B"))], Variable("C")),
                IfExpr([(Variable("X"), Variable("Y"))], Variable("Z")),
            ),
        )
        == "1 + (if A then B else C) + (if X then Y else Z)"
    )


def test_ass_expr_findall() -> None:
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Less(Variable("Y"), Number(2))).findall(
            lambda x: isinstance(x, Number),
        ),
        [Number(1), Number(2)],
    )


def test_ass_expr_simplified() -> None:
    assert_equal(
        Add(
            Number(8),
            IfExpr(
                [
                    (
                        And(
                            Variable("A"),
                            Or(Variable("B"), Variable("C")),
                            Equal(Variable("D"), TRUE),
                        ),
                        Variable("X"),
                    ),
                ],
                Variable("Y"),
            ),
            Number(16),
            IfExpr(
                [
                    (
                        And(
                            Variable("A"),
                            Or(Variable("B"), Variable("C")),
                            Equal(Variable("D"), FALSE),
                        ),
                        Variable("X"),
                    ),
                ],
                Variable("Y"),
            ),
            Number(24),
        ).simplified(),
        Add(
            IfExpr(
                [
                    (
                        Or(
                            And(
                                Variable("A"),
                                Or(Variable("B"), Variable("C")),
                                Variable("D"),
                            ),
                            And(
                                Variable("A"),
                                Or(Variable("B"), Variable("C")),
                                Not(Variable("D")),
                            ),
                        ),
                        Variable("X"),
                    ),
                ],
                Variable("Y"),
            ),
            Number(48),
        ),
    )


def test_ass_expr_substituted() -> None:
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        And(Equal(Variable("P_X"), Number(1)), Variable("P_Y")),
    )
    assert_equal(
        Mul(Variable("X"), Number(1)).substituted(
            lambda x: Variable("Y") if x == Mul(Variable("X"), Number(1)) else x,
        ),
        Variable("Y"),
    )
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Variable("Y")).substituted(
            lambda x: (
                Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Or(*x.terms) if isinstance(x, And) else x)
            ),
        ),
        Or(Equal(Variable("P_X"), Number(1)), Variable("P_Y")),
    )


def test_ass_expr_substituted_location() -> None:
    expr = And(
        Equal(Variable("X"), Number(1)),
        Variable("Y"),
        location=Location((1, 2)),
    ).substituted(lambda x: x)
    assert expr.location


def test_ass_expr_if_expr_simplified_location() -> None:
    simplified_if = Add(
        IfExpr(
            [(Or(Variable("X"), Variable("Y"), location=Location((1, 1))), Variable("A"))],
            Number(42),
        ),
        IfExpr(
            [(Or(Variable("X"), Variable("A"), location=Location((1, 2))), Variable("A"))],
            Number(42),
        ),
    ).simplified()
    assert isinstance(simplified_if, IfExpr)
    assert simplified_if.condition_expressions[0][0].location == Location((1, 1), end=(1, 2))


def test_bool_expr_str() -> None:
    assert_equal(
        str(And(Variable("A"), Or(Variable("B"), Variable("C")), Variable("D"))),
        textwrap.dedent(
            """\
            A
            and (B
                 or C)
            and D""",
        ),
    )
    assert_equal(
        str(AndThen(Variable("A"), OrElse(Variable("B"), Variable("C")), Variable("D"))),
        textwrap.dedent(
            """\
            A
            and then (B
                      or else C)
            and then D""",
        ),
    )


@pytest.mark.parametrize("operation", [And, Or])
def test_bool_expr_type(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        operation(Variable("X", type_=ty.BOOLEAN), Variable("Y", type_=ty.BOOLEAN)),
        ty.BOOLEAN,
    )


@pytest.mark.parametrize("operation", [And, Or])
def test_bool_expr_type_error(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        operation(
            Variable("X", type_=ty.Integer("A", ty.Bounds(0, 100)), location=Location((10, 20))),
            Number(1, location=Location((10, 30))),
        ),
        r'^<stdin>:10:20: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r'<stdin>:10:20: error: found integer type "A" \(0 .. 100\)\n'
        r'<stdin>:10:30: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:30: error: found type universal integer \(1 .. 1\)$",
    )


def test_and_neg() -> None:
    with pytest.raises(NotImplementedError):
        -And(Variable("X"), TRUE)


def test_and_variables() -> None:
    assert_equal(And(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")])


def test_and_contains() -> None:
    assert Variable("X") in And(TRUE, Less(Variable("X"), Number(42)))
    assert Variable("Y") not in And(TRUE, Less(Variable("X"), Number(42)))


def test_and_simplified() -> None:
    assert And().simplified() == TRUE
    assert And(TRUE, TRUE).simplified() == TRUE
    assert And(TRUE, FALSE, TRUE).simplified() == FALSE
    assert And(TRUE, EXPR).simplified() == EXPR
    assert And(EXPR, TRUE).simplified() == EXPR
    assert And(EXPR, FALSE).simplified() == FALSE
    assert And(FALSE, EXPR).simplified() == FALSE
    assert (
        And(
            Equal(Variable("X"), Number(0)),
            NotEqual(Variable("X"), Number(0)),
            Variable("Y"),
        ).simplified()
        == FALSE
    )
    assert And(
        Equal(Variable("X"), Number(0)),
        Equal(Variable("X"), Number(0)),
    ).simplified() == Equal(Variable("X"), Number(0))


def test_and_str() -> None:
    assert str(And(Variable("X"), Variable("Y"))) == "X\nand Y"
    assert str(And()) == "True"


def test_or_neg() -> None:
    with pytest.raises(NotImplementedError):
        -Or(Variable("X"), TRUE)


def test_or_variables() -> None:
    assert Or(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_or_contains() -> None:
    assert Variable("X") in Or(Less(Variable("X"), Number(42)), TRUE)
    assert Variable("Y") not in Or(Less(Variable("X"), Number(42)), TRUE)


def test_or_simplified() -> None:
    assert Or().simplified() == FALSE
    assert Or(TRUE, TRUE).simplified() == TRUE
    assert Or(TRUE, EXPR).simplified() == TRUE
    assert Or(EXPR, TRUE).simplified() == TRUE
    assert (
        Or(
            Equal(Variable("X"), Number(0)),
            NotEqual(Variable("X"), Number(0)),
            Variable("Y"),
        ).simplified()
        == TRUE
    )
    assert Or(
        Equal(Variable("X"), Number(0)),
        Equal(Variable("X"), Number(0)),
    ).simplified() == Equal(Variable("X"), Number(0))


def test_or_str() -> None:
    assert str(Or(Variable("X"), Variable("Y"))) == "X\nor Y"
    assert str(Or()) == "False"


def test_undefined_simplified() -> None:
    assert UNDEFINED.simplified() == UNDEFINED


def test_undefined_str() -> None:
    assert str(UNDEFINED) == "__UNDEFINED__"


def test_number_type() -> None:
    assert_type(
        Number(1),
        ty.UniversalInteger(ty.Bounds(1, 1)),
    )


def test_number_neg() -> None:
    assert -Number(42) == Number(-42)


def test_number_simplified() -> None:
    assert Number(42).simplified() == Number(42)


def test_number_add() -> None:
    assert Number(5) + Number(3) == Number(8)


def test_number_add_location() -> None:
    result = Number(5, location=Location((1, 1))) + Number(3, location=Location((1, 2)))
    assert isinstance(result, Number)
    assert result.location == Location((1, 1), end=(1, 2))


def test_number_sub() -> None:
    assert Number(5) - Number(3) == Number(2)


def test_number_sub_location() -> None:
    result = Number(5, location=Location((1, 1))) - Number(3, location=Location((1, 2)))
    assert isinstance(result, Number)
    assert result.location == Location((1, 1), end=(1, 2))


def test_number_mul() -> None:
    assert Number(4) * Number(2) == Number(8)


def test_number_mul_location() -> None:
    result = Number(5, location=Location((1, 1))) * Number(3, location=Location((1, 2)))
    assert isinstance(result, Number)
    assert result.location == Location((1, 1), end=(1, 2))


def test_number_div() -> None:
    assert Number(4) // Number(2) == Number(2)


def test_number_div_location() -> None:
    result = Number(5, location=Location((1, 1))) // Number(3, location=Location((1, 2)))
    assert isinstance(result, Div)
    assert result.location == Location((1, 1), end=(1, 2))


def test_number_pow() -> None:
    assert Number(2) ** Number(4) == Number(16)


def test_number_pow_location() -> None:
    result = Number(5, location=Location((1, 1))) ** Number(3, location=Location((1, 2)))
    assert isinstance(result, Number)
    assert result.location == Location((1, 1), end=(1, 2))


def test_number_mod_location() -> None:
    result = Number(3, location=Location((1, 1))) % Number(2, location=Location((1, 2)))
    assert isinstance(result, Number)
    assert result.location == Location((1, 1), end=(1, 2))


def test_number_eq() -> None:
    assert Number(1) == Number(1)
    assert Number(1, 10) == Number(1, 16)
    assert Number(42, 16) == Number(42, 10)


def test_number_ne() -> None:
    assert Number(1) != Number(2)
    assert Number(1, 16) != Number(2, 16)


def test_number_lt() -> None:
    assert Number(1) < Number(2)
    assert not Number(2) < Number(2)
    assert not Number(3) < Number(2)
    assert not Variable("X") < Number(2)
    assert not Number(2) < Variable("X")


def test_number_le() -> None:
    assert Number(1) <= Number(2)
    assert Number(2) <= Number(2)
    assert not Number(3) <= Number(2)
    assert not Variable("X") <= Number(2)
    assert not Number(2) <= Variable("X")


def test_number_gt() -> None:
    assert not Number(1) > Number(2)
    assert not Number(2) > Number(2)
    assert Number(3) > Number(2)
    assert Variable("X") > Number(2)
    assert not Number(2) > Variable("X")


def test_number_ge() -> None:
    assert not Number(1) >= Number(2)
    assert Number(2) >= Number(2)
    assert Number(3) >= Number(2)
    assert Variable("X") >= Number(2)
    assert not Number(2) >= Variable("X")


def test_number_hashable() -> None:
    assert {Number(1), Number(2)}


@pytest.mark.parametrize("operation", [Add, Mul, Sub, Div, Pow])
def test_math_expr_type(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        operation(Variable("X", type_=INT_TY), Variable("Y", type_=INT_TY)),
        ty.BASE_INTEGER,
    )


@pytest.mark.parametrize("operation", [Add, Mul, Sub, Div, Pow])
def test_math_expr_type_error(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        operation(
            Variable("X", type_=ty.BOOLEAN, location=Location((10, 20))),
            Variable("True", type_=ty.BOOLEAN, location=Location((10, 30))),
        ),
        r"^<stdin>:10:20: error: expected integer type\n"
        r'<stdin>:10:20: error: found enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:30: error: expected integer type\n"
        r'<stdin>:10:30: error: found enumeration type "__BUILTINS__::Boolean"$',
    )


def test_neg_str() -> None:
    assert str(Neg(Variable("X"))) == "-X"
    assert str(Neg(Neg(Variable("X")))) == "-(-X)"


def test_neg_type() -> None:
    assert_type(
        Neg(Variable("X", type_=INT_TY)),
        INT_TY,
    )


def test_neg_type_error() -> None:
    assert_type_error(
        Neg(Variable("X", type_=ty.BOOLEAN, location=Location((10, 20)))),
        r"^<stdin>:10:20: error: expected integer type\n"
        r'<stdin>:10:20: error: found enumeration type "__BUILTINS__::Boolean"$',
    )


def test_neg_neg() -> None:
    assert -Neg(Variable("X")) == Variable("X")
    assert -Variable("X") != Variable("X")
    y = Variable("Y")
    assert y == y  # noqa: PLR0124
    assert y != -y


def test_neg_findall() -> None:
    assert Neg(Variable("X")).findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_neg_substituted() -> None:
    assert_equal(
        Neg(Variable("X")).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        Neg(Variable("P_X")),
    )
    assert_equal(
        Neg(Variable("X")).substituted(lambda x: Variable("Y") if x == Neg(Variable("X")) else x),
        Variable("Y"),
    )


@pytest.mark.parametrize(
    ("expr", "expected"),
    [
        # Argument is Neg
        (Neg(Neg(Variable("X"))), Variable("X")),
        # Argument simplifies to Neg
        (Neg(Neg(Neg(Variable("X")))), Neg(Variable("X"))),
        (Neg(Neg(Neg(Neg(Variable("X"))))), Variable("X")),
        # Argument is Number
        (Neg(Number(42)), Number(-42)),
        (Neg(Number(-42)), Number(42)),
        # Argument simplifies to Number
        (Neg(Neg(Number(42))), Number(42)),
        (Neg(Neg(Neg(Number(42)))), Number(-42)),
        (Neg(Mul(Number(3), Neg(Number(2)))), Number(6)),
        (Neg(Mul(Number(3), Neg(Number(-2)))), Number(-6)),
        (Sub(Number(0), Neg(Add(Number(8), Number(7)))), Number(15)),
        # Argument simplifies to some other expression
        (
            Neg(Mul(Variable("X"), Add(Number(2), Number(3)))),
            Mul(Variable("X"), Number(-5)),
        ),
        # Argument cannot be simplified
        (Neg(Variable("X")), Neg(Variable("X"))),
    ],
)
def test_neg_simplified(expr: Expr, expected: Expr) -> None:
    assert expr.simplified() == expected


def test_add_str() -> None:
    assert str(Add(Variable("X"), Number(1))) == "X + 1"
    assert str(-Add(Variable("X"), Number(1))) == "-X - 1"
    assert str(Add(Number(1), Call("Test", ty.BASE_INTEGER, []))) == "1 + Test"
    assert str(Add(Number(1), -Call("Test", ty.BASE_INTEGER, []))) == "1 - Test"
    assert str(Add()) == "0"


def test_add_neg() -> None:
    assert -Add(Variable("X"), Number(1)) == Add(Neg(Variable("X")), Number(-1))


@pytest.mark.parametrize(
    ("args"),
    [
        [],
        [0],
        [42],
        # 0 in argument
        [0, 0],
        [0, 5],
        [1, 0],
        [1, 0, 0],
        # Argument sign variations
        [1, 5],
        [-1, 5],
        [1, -5],
        [-1, -5],
        [1, 5, 42],
        [-1, 5, -42],
        [1, -5, 42],
        [-1, -5, -42],
    ],
)
def test_add_neg_eval(args: list[int]) -> None:
    assert (-Add(*[Number(a) for a in args])).simplified() == Number(-sum(args))


def test_add_variables() -> None:
    assert_equal(Add(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")])


def test_add_simplified() -> None:
    assert Add(Variable("X"), Number(1)).simplified() == Add(Variable("X"), Number(1))
    assert Add(Variable("X"), Number(0)).simplified() == Variable("X")
    assert Add(Number(2), Number(3), Number(5)).simplified() == Number(10)
    assert Add(Variable("X"), Variable("Y"), Neg(Variable("X"))).simplified() == Variable(
        "Y",
    )
    assert Add(Variable("X"), Variable("Y"), Variable("X"), -Variable("X")).simplified() == Add(
        Variable("X"),
        Variable("Y"),
    )


def test_add_lt() -> None:
    assert Add(Variable("X"), Number(1)) < Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(2)) < Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(3)) < Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(1)) < Add(Variable("Y"), Number(2))
    assert Add(Variable("X"), Number(2)) < Add(Variable("Y"), Number(1))
    assert Add(Variable("X"), Number(2)) < Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_le() -> None:
    assert Add(Variable("X"), Number(1)) <= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(2)) <= Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(3)) <= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(1)) <= Add(Variable("Y"), Number(2))
    assert Add(Variable("X"), Number(2)) <= Add(Variable("Y"), Number(1))
    assert Add(Variable("X"), Number(2)) <= Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_gt() -> None:
    assert not Add(Variable("X"), Number(1)) > Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(2)) > Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(3)) > Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(1)) > Add(Variable("Y"), Number(2))
    assert not Add(Variable("X"), Number(2)) > Add(Variable("Y"), Number(1))
    assert not Add(Variable("X"), Number(2)) > Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_ge() -> None:
    assert not Add(Variable("X"), Number(1)) >= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(2)) >= Add(Variable("X"), Number(2))
    assert Add(Variable("X"), Number(3)) >= Add(Variable("X"), Number(2))
    assert not Add(Variable("X"), Number(1)) >= Add(Variable("Y"), Number(2))
    assert not Add(Variable("X"), Number(2)) >= Add(Variable("Y"), Number(1))
    assert not Add(Variable("X"), Number(2)) >= Add(Variable("Y"), Variable("Z"), Number(1))


def test_add_simplified_location() -> None:
    simplified = Add(
        Number(1),
        Number(1),
        Number(1),
        Number(1),
        location=Location((1, 1)),
    ).simplified()
    assert simplified == Number(4)
    assert simplified.location == Location((1, 1))


def test_mul_neg() -> None:
    assert -Mul(Variable("X"), Number(2)) == Mul(Variable("X"), Number(-2))


@pytest.mark.parametrize(
    ("args"),
    [
        [],
        [0],
        [42],
        # 0 in argument
        [0, 0],
        [0, 5],
        [1, 0],
        [1, 0, 0],
        # Argument sign variations
        [1, 5],
        [-1, 5],
        [1, -5],
        [-1, -5],
        [1, 5, 42],
        [-1, 5, -42],
        [1, -5, 42],
        [-1, -5, -42],
    ],
)
def test_mul_neg_eval(args: list[int]) -> None:
    assert (-Mul(*[Number(a) for a in args])).simplified() == Number(-math.prod(args))


def test_mul_variables() -> None:
    assert_equal(Mul(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")])


def test_mul_simplified() -> None:
    assert Mul(Variable("X"), Number(2)).simplified() == Mul(Variable("X"), Number(2))
    assert Mul(Variable("X"), Number(1)).simplified() == Variable("X")
    assert Mul(Number(2), Number(3), Number(5)).simplified() == Number(30)


def test_sub_neg() -> None:
    assert -Sub(Number(1), Variable("X")) == Sub(Variable("X"), Number(1))


@pytest.mark.parametrize(
    ("left", "right"),
    [
        # 0 in argument
        (0, 0),
        (0, 5),
        (1, 0),
        # Argument sign variations
        (1, 5),
        (-1, 5),
        (1, -5),
        (-1, -5),
    ],
)
def test_sub_neg_eval(left: int, right: int) -> None:
    assert (-Sub(Number(left), Number(right))).simplified() == Number(-(left - right))


def test_sub_variables() -> None:
    assert Sub(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


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


def test_sub_simplified_location() -> None:
    simplified = Sub(
        Number(5, location=Location((1, 1))),
        Number(2, location=Location((1, 2))),
    ).simplified()
    assert isinstance(simplified, Number)
    assert simplified.location == Location((1, 1), end=(1, 2))


def test_sub_simplified_to_add() -> None:
    simplified = Sub(
        Variable(ID("X", location=Location((1, 1)))),
        Number(2, location=Location((1, 2))),
    ).simplified()
    assert isinstance(simplified, Add)
    assert simplified.location == Location((1, 1), end=(1, 2))


def test_div_neg() -> None:
    assert -Div(Variable("X"), Number(5)) == Div(-(Variable("X")), Number(5))


@pytest.mark.parametrize(
    ("left", "right"),
    [
        # 0 in argument
        (0, 5),
        # Argument sign variations
        (10, 5),
        (-10, 5),
        (10, -5),
        (-10, -5),
    ],
)
def test_div_neg_eval(left: int, right: int) -> None:
    assert (-Div(Number(left), Number(right))).simplified() == Number(-(left // right))


def test_div_variables() -> None:
    assert Div(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_div_simplified() -> None:
    assert Div(Variable("X"), Number(1)).simplified() == Div(Variable("X"), Number(1))
    assert Div(Number(6), Number(2)).simplified() == Number(3)
    assert Div(Number(9), Number(2)).simplified() == Div(Number(9), Number(2))


def test_pow_neg() -> None:
    assert -Pow(Variable("X"), Number(5)) == -Pow(Variable("X"), Number(5))


@pytest.mark.parametrize(
    ("left", "right"),
    [
        # 0 in argument
        (0, 0),
        (-10, 0),
        (0, 4),
        # Argument sign variations
        # Constraints:
        # * The second argument cannot be negative.
        # * The second argument must be tested with an even and odd value.
        (10, 4),
        (-10, 4),
        (10, 5),
        (-10, 5),
    ],
)
def test_pow_neg_eval(left: int, right: int) -> None:
    assert (-Pow(Number(left), Number(right))).simplified() == Number(-(left**right))


def test_pow_simplified() -> None:
    assert Pow(Variable("X"), Number(1)).simplified() == Pow(Variable("X"), Number(1))
    assert Pow(Variable("X"), Add(Number(1), Number(1))).simplified() == Pow(
        Variable("X"),
        Number(2),
    )
    assert Pow(Number(6), Number(2)).simplified() == Number(36)


def test_pow_variables() -> None:
    assert Pow(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_rem_neg() -> None:
    assert -Rem(Variable("X"), Number(5)) == -Rem(Variable("X"), Number(5))


@pytest.mark.parametrize(
    ("left", "right", "expected"),
    [
        # 0 in argument
        (0, 3, -Rem(Number(0), Number(3))),
        # Argument sign variations
        (7, 3, -Rem(Number(7), Number(3))),
        (-7, 3, -Rem(Number(-7), Number(3))),
        (7, -3, -Rem(Number(7), Number(-3))),
        (-7, -3, -Rem(Number(-7), Number(-3))),
    ],
)
def test_rem_neg_eval(left: int, right: int, expected: Expr) -> None:
    assert (-Rem(Number(left), Number(right))).simplified() == expected


def test_mod_neg() -> None:
    assert -Mod(Variable("X"), Number(5)) == -Mod(Variable("X"), Number(5))


@pytest.mark.parametrize(
    ("left", "right"),
    [
        # 0 in argument
        (0, 3),
        # Argument sign variations
        (7, 3),
        (-7, 3),
        (7, -3),
        (-7, -3),
    ],
)
def test_mod_neg_eval(left: int, right: int) -> None:
    assert (-Mod(Number(left), Number(right))).simplified() == Number(-(left % right))


def test_mod_simplified() -> None:
    assert Mod(Variable("X"), Number(1)).simplified() == Mod(Variable("X"), Number(1))
    assert Mod(Variable("X"), Add(Number(1), Number(1))).simplified() == Mod(
        Variable("X"),
        Number(2),
    )
    assert Mod(Number(6), Number(2)).simplified() == Number(0)


def test_mod_variables() -> None:
    assert Mod(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_term_simplified() -> None:
    assert_equal(
        Add(
            Mul(Number(1), Number(6)),
            Sub(Variable("X"), Number(10)),
            Add(Number(1), Number(3)),
        ).simplified(),
        Variable("X"),
    )


def test_variable_invalid_name() -> None:
    with pytest.raises(BaseException, match=r"^invalid identifier$"):
        Variable("Foo (Bar)")


def test_variable_type() -> None:
    assert_type(
        Variable("X", type_=ty.BOOLEAN),
        ty.BOOLEAN,
    )
    assert_type(
        Variable("X", type_=INT_TY),
        INT_TY,
    )


def test_variable_type_error() -> None:
    assert_type_error(
        Variable("X", location=Location((10, 20))),
        r'^<stdin>:10:20: error: undefined variable "X"$',
    )


def test_variable_neg() -> None:
    assert -Variable("X") == Neg(Variable("X"))


def test_variable_variables() -> None:
    assert Variable("X").variables() == [Variable("X")]
    assert (-Variable("X")).variables() == [Variable("X")]


def test_variable_substituted() -> None:
    assert_equal(
        Variable("X").substituted(lambda x: Number(42) if x == Variable("X") else x),
        Number(42),
    )


def test_mutable_variable_substituted() -> None:
    x = Variable("X")
    assert_equal(x.substituted(mapping={Variable("X"): Number(42)}), Number(42))


def test_immutable_variable_substituted() -> None:
    x = Variable("X", immutable=True)
    assert_equal(x.substituted(mapping={Variable("X"): Number(42)}), Variable("X"))


def test_variable_simplified() -> None:
    assert Variable("X").simplified() == Variable("X")


def test_attribute() -> None:
    assert isinstance(Size("X"), Attribute)
    assert isinstance(Length("X"), Attribute)
    assert isinstance(First("X"), Attribute)
    assert isinstance(Last("X"), Attribute)
    assert First("X") == First(Variable("X"))
    assert First("X") == First(ID("X"))
    assert First("X") == First(Variable(ID("X")))


@pytest.mark.parametrize(
    ("attribute", "expr", "expected"),
    [
        (Size, Variable("X", type_=INT_TY), ty.UNIVERSAL_INTEGER),
        (Length, Variable("X", type_=INT_TY), ty.UNIVERSAL_INTEGER),
        (First, Variable("X", type_=INT_TY), ty.UNIVERSAL_INTEGER),
        (Last, Variable("X", type_=INT_TY), ty.UNIVERSAL_INTEGER),
        (ValidChecksum, Variable("X", type_=INT_TY), ty.BOOLEAN),
        (Valid, Variable("X", type_=ty.Message("A")), ty.BOOLEAN),
        (
            Present,
            Selected(
                Variable("X", type_=ty.Message("M", {("F",)}, {ID("F"): INT_TY})),
                "F",
            ),
            ty.BOOLEAN,
        ),
        (Head, Variable("X", type_=ty.Sequence("A", INT_TY)), INT_TY),
        (Opaque, Variable("X", type_=ty.Message("A")), ty.OPAQUE),
        (
            Head,
            Comprehension(
                "X",
                Variable("Y", type_=ty.Sequence("A", INT_TY)),
                Variable("X", type_=INT_TY),
                TRUE,
                location=Location((10, 30)),
            ),
            INT_TY,
        ),
        (
            Head,
            Variable(
                "Z",
                type_=ty.Sequence("Universal::Options", ty.Message("Universal::Option")),
                location=Location((10, 20)),
            ),
            ty.Message("Universal::Option"),
        ),
    ],
)
def test_attribute_type(attribute: Callable[[Expr], Expr], expr: Expr, expected: ty.Type) -> None:
    assert_type(
        attribute(expr),
        expected,
    )


@pytest.mark.parametrize(
    ("expr", "match"),
    [
        (
            Present(Variable("X", location=Location((10, 30)))),
            r"^<stdin>:10:30: error: invalid prefix for attribute Present$",
        ),
        (
            Head(
                Opaque(
                    Variable(
                        "X",
                        type_=ty.Sequence("A", INT_TY),
                        location=Location((10, 30)),
                    ),
                ),
            ),
            r"^<stdin>:10:30: error: prefix of attribute Head must be a name or comprehension$",
        ),
        (
            Opaque(
                Call(
                    "X",
                    ty.UNDEFINED,
                    [Variable("Y", location=Location((10, 30)))],
                    location=Location((10, 20)),
                ),
            ),
            r'^<stdin>:10:30: error: undefined variable "Y"\n'
            r'<stdin>:10:20: error: undefined function "X"$',
        ),
    ],
)
def test_attribute_type_error(expr: Expr, match: str) -> None:
    assert_type_error(
        expr,
        match,
    )


def test_attribute_neg() -> None:
    assert -First("X") == Neg(First("X"))


def test_attributes_findall() -> None:
    assert First("X").findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_attribute_substituted() -> None:
    assert_equal(First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(42))
    assert_equal(
        -First("X").substituted(lambda x: Number(42) if x == First("X") else x),
        Number(-42),
    )
    assert_equal(
        First("X").substituted(lambda x: Call("Y", ty.BASE_INTEGER) if x == Variable("X") else x),
        First(Call("Y", ty.BASE_INTEGER)),
    )
    assert_equal(
        -First("X").substituted(lambda x: Call("Y", ty.BASE_INTEGER) if x == Variable("X") else x),
        -First(Call("Y", ty.BASE_INTEGER)),
    )
    assert_equal(
        -First("X").substituted(
            lambda x: (
                Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Last(x.prefix) if isinstance(x, First) else x)
            ),
        ),
        -Last(Variable("P_X")),
    )
    assert_equal(
        First("X").substituted(lambda x: First("Y") if x == Variable("X") else x),
        First("X"),
    )


def test_attribute_substituted_location() -> None:
    expr = First(Variable("X", location=Location((1, 2)))).substituted(lambda x: x)
    assert expr.location


def test_attribute_simplified() -> None:
    assert First("X").simplified() == First("X")


def test_attribute_str() -> None:
    assert str(First("X")) == "X'First"
    assert str(HasData("X")) == "X'Has_Data"


def test_attribute_variables() -> None:
    assert First("X").variables() == [Variable("X")]
    assert First(Call("X", ty.BASE_INTEGER, [Variable("Y")])).variables() == [
        Variable("X"),
        Variable("Y"),
    ]


def test_val_substituted() -> None:
    assert_equal(  # pragma: no branch
        Val("X", Variable("Y")).substituted(
            lambda x: Number(42) if x == Val("X", Variable("Y")) else x,
        ),
        Val("X", Variable("Y")),
    )
    assert_equal(  # pragma: no branch
        -Val("X", Variable("Y")).substituted(
            lambda x: Number(42) if x == Val("X", Variable("Y")) else x,
        ),
        -Val("X", Variable("Y")),
    )


def test_val_simplified() -> None:
    assert Val("X", Add(Number(1), Number(1))).simplified() == Val("X", Add(Number(1), Number(1)))


def test_val_str() -> None:
    assert str(Val("X", Number(1))) == "X'Val (1)"


def test_aggregate_type() -> None:
    assert_type(
        Aggregate(Number(0), Number(1)),
        ty.Aggregate(ty.UniversalInteger(ty.Bounds(0, 1))),
    )


def test_aggregate_substituted() -> None:
    assert_equal(
        Aggregate(First("X")).substituted(
            lambda x: Number(42) if x == Aggregate(First("X")) else x,
        ),
        Number(42),
    )
    assert_equal(
        Aggregate(First("X")).substituted(lambda x: Number(42) if x == First("X") else x),
        Aggregate(Number(42)),
    )
    assert_equal(
        Aggregate(Variable("X")).substituted(
            lambda x: (
                Variable(f"P_{x}")
                if isinstance(x, Variable)
                else (Aggregate(*([*x.elements, Variable("Y")])) if isinstance(x, Aggregate) else x)
            ),
        ),
        Aggregate(Variable("P_X"), Variable("P_Y")),
    )


def test_aggregate_substituted_location() -> None:
    expr = Aggregate(Number(0), location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


def test_aggregate_simplified() -> None:
    assert Aggregate(Add(Number(1), Number(1))).simplified() == Aggregate(Number(2))


def test_aggregate_str() -> None:
    assert str(Aggregate(Number(1))) == "[1]"
    assert str(Aggregate(Number(1), Number(2))) == "[1, 2]"


def test_aggregate_precedence() -> None:
    assert Aggregate(Number(1), Number(2)).precedence == Precedence.LITERAL


@pytest.mark.parametrize("relation", [Less, LessEqual, Equal, GreaterEqual, Greater, NotEqual])
def test_relation_integer_type(relation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        relation(Variable("X", type_=INT_TY), Variable("Y", type_=INT_TY)),
        ty.BOOLEAN,
    )


@pytest.mark.parametrize("relation", [Less, LessEqual, Equal, GreaterEqual, Greater, NotEqual])
def test_relation_integer_type_error(relation: Callable[[Expr, Expr], Expr]) -> None:
    integer_type = (
        r'integer type "I" \(10 \.\. 100\)' if relation in [Equal, NotEqual] else r"integer type"
    )
    assert_type_error(
        relation(
            Variable("X", type_=INT_TY),
            Variable("True", type_=ty.BOOLEAN, location=Location((10, 30))),
        ),
        rf"^<stdin>:10:30: error: expected {integer_type}\n"
        r'<stdin>:10:30: error: found enumeration type "__BUILTINS__::Boolean"$',
    )


@pytest.mark.parametrize("relation", [In, NotIn])
def test_relation_composite_type(relation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        relation(
            Variable("X", type_=INT_TY),
            Variable("Y", type_=ty.Sequence("A", INT_TY)),
        ),
        ty.BOOLEAN,
    )


@pytest.mark.parametrize("relation", [In, NotIn])
def test_relation_composite_type_error(relation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        relation(
            Variable("X", type_=INT_TY, location=Location((10, 20))),
            Variable("True", type_=ty.BOOLEAN, location=Location((10, 30))),
        ),
        r"^<stdin>:10:30: error: expected aggregate"
        r' with element integer type "I" \(10 \.\. 100\)\n'
        r'<stdin>:10:30: error: found enumeration type "__BUILTINS__::Boolean"$',
    )
    assert_type_error(
        relation(
            Variable("X", type_=INT_TY, location=Location((10, 20))),
            Variable("Y", type_=ty.Sequence("A", ty.BOOLEAN), location=Location((10, 30))),
        ),
        r"^<stdin>:10:30: error: expected aggregate"
        r' with element integer type "I" \(10 \.\. 100\)\n'
        r'<stdin>:10:30: error: found sequence type "A"'
        r' with element enumeration type "__BUILTINS__::Boolean"$',
    )


def test_relation_substituted() -> None:
    assert_equal(
        Equal(Variable("X"), Variable("Y")).substituted(
            lambda x: Number(1) if x == Variable("X") else x,
        ),
        Equal(Number(1), Variable("Y")),
    )
    assert_equal(
        Equal(Variable("X"), Variable("Y")).substituted(
            lambda x: Number(1) if x == Variable("Y") else x,
        ),
        Equal(Variable("X"), Number(1)),
    )
    assert_equal(
        Equal(Variable("X"), Variable("Y")).substituted(
            lambda x: Number(1) if x == Equal(Variable("X"), Variable("Y")) else x,
        ),
        Number(1),
    )


def test_relation_substituted_location() -> None:
    expr = Equal(Variable("X"), Variable("Y"), location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


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
        Equal(Add(Number(1), Number(1)), Add(Number(1), Number(1))).simplified(),
        TRUE,
    )
    assert_equal(Equal(String("Foo Bar"), String("Foo Bar")).simplified(), TRUE)
    assert_equal(
        Equal(String("Foo"), Aggregate(Number(70), Number(111), Number(111))).simplified(),
        TRUE,
    )
    assert_equal(
        Equal(
            Aggregate(Number(0), Number(1), Number(2)),
            Aggregate(Number(0), Number(1), Number(2)),
        ).simplified(),
        TRUE,
    )
    assert_equal(
        Equal(
            Aggregate(Number(1), Number(2), Number(3)),
            Aggregate(Number(4), Number(5), Number(6)),
        ).simplified(),
        FALSE,
    )

    l = Literal("L")
    k = Literal("K")
    assert Equal(l, l).simplified() == TRUE
    assert Equal(l, k).simplified() == FALSE
    assert NotEqual(l, l).simplified() == FALSE
    assert NotEqual(l, k).simplified() == TRUE

    assert Equal(TRUE, TRUE).simplified() == TRUE
    assert Equal(TRUE, FALSE).simplified() == FALSE
    assert NotEqual(TRUE, TRUE).simplified() == FALSE
    assert NotEqual(TRUE, FALSE).simplified() == TRUE

    assert Equal(Variable("X"), Variable("X")).simplified() == TRUE
    assert LessEqual(Variable("X"), Variable("X")).simplified() == TRUE
    assert GreaterEqual(Variable("X"), Variable("X")).simplified() == TRUE
    assert NotEqual(Variable("X"), Variable("X")).simplified() == FALSE
    assert Equal(Variable("X"), Variable("Y")).simplified() == Equal(Variable("X"), Variable("Y"))
    assert NotEqual(Variable("X"), Variable("Y")).simplified() == NotEqual(
        Variable("X"),
        Variable("Y"),
    )


def test_relation_contains() -> None:
    assert Variable("X") in Less(Variable("X"), Number(42))


def test_relation_variables() -> None:
    assert Less(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_less_neg() -> None:
    assert -Less(Variable("X"), Number(1)) == GreaterEqual(Variable("X"), Number(1))


def test_less_simplified() -> None:
    assert Less(Number(0), Number(1)).simplified() == TRUE
    assert Less(Number(1), Number(1)).simplified() == FALSE
    assert Less(Number(2), Number(1)).simplified() == FALSE
    assert Less(Div(Number(1), Number(8)), Div(Number(1), Number(8))).simplified() == FALSE
    assert Less(Div(Number(1), Number(8)), Div(Number(2), Number(8))).simplified() == TRUE
    assert Less(Div(Number(1), Number(6)), Div(Number(1), Number(8))).simplified() == FALSE


def test_less_equal_neg() -> None:
    assert -LessEqual(Variable("X"), Number(1)) == Greater(Variable("X"), Number(1))


def test_less_equal_simplified() -> None:
    assert LessEqual(Number(0), Number(1)).simplified() == TRUE
    assert LessEqual(Number(1), Number(1)).simplified() == TRUE
    assert LessEqual(Number(2), Number(1)).simplified() == FALSE
    assert LessEqual(Div(Number(1), Number(8)), Div(Number(1), Number(8))).simplified() == TRUE
    assert LessEqual(Div(Number(1), Number(8)), Div(Number(2), Number(8))).simplified() == TRUE
    assert LessEqual(Div(Number(1), Number(6)), Div(Number(1), Number(8))).simplified() == FALSE


def test_equal_neg() -> None:
    assert -Equal(Variable("X"), Number(1)) == NotEqual(Variable("X"), Number(1))


def test_equal_simplified() -> None:
    assert Equal(Number(0), Number(1)).simplified() == FALSE
    assert Equal(Number(1), Number(1)).simplified() == TRUE
    assert Equal(Number(2), Number(1)).simplified() == FALSE
    assert Equal(Div(Number(1), Number(8)), Div(Number(1), Number(8))).simplified() == TRUE
    assert Equal(Div(Number(1), Number(8)), Div(Number(2), Number(8))).simplified() == FALSE
    assert Equal(Div(Number(1), Number(6)), Div(Number(1), Number(8))).simplified() == FALSE
    assert Equal(Variable("X"), TRUE).simplified() == Variable("X")
    assert Equal(Variable("X"), FALSE).simplified() == Not(Variable("X"))
    assert NotEqual(Variable("X"), FALSE).simplified() == Variable("X")
    assert NotEqual(Variable("X"), TRUE).simplified() == Not(Variable("X"))
    assert NotEqual(TRUE, Variable("X")).simplified() == Not(Variable("X"))


def test_greater_equal_neg() -> None:
    assert -GreaterEqual(Variable("X"), Number(1)) == Less(Variable("X"), Number(1))


def test_greater_equal_simplified() -> None:
    assert GreaterEqual(Number(0), Number(1)).simplified() == FALSE
    assert GreaterEqual(Number(1), Number(1)).simplified() == TRUE
    assert GreaterEqual(Number(2), Number(1)).simplified() == TRUE
    assert GreaterEqual(Div(Number(1), Number(8)), Div(Number(1), Number(8))).simplified() == TRUE
    assert GreaterEqual(Div(Number(1), Number(8)), Div(Number(2), Number(8))).simplified() == FALSE
    assert GreaterEqual(Div(Number(1), Number(6)), Div(Number(1), Number(8))).simplified() == TRUE


def test_greater_neg() -> None:
    assert -Greater(Variable("X"), Number(1)) == LessEqual(Variable("X"), Number(1))


def test_greater_simplified() -> None:
    assert Greater(Number(0), Number(1)).simplified() == FALSE
    assert Greater(Number(1), Number(1)).simplified() == FALSE
    assert Greater(Number(2), Number(1)).simplified() == TRUE
    assert Greater(Div(Number(1), Number(8)), Div(Number(1), Number(8))).simplified() == FALSE
    assert Greater(Div(Number(1), Number(8)), Div(Number(2), Number(8))).simplified() == FALSE
    assert Greater(Div(Number(1), Number(6)), Div(Number(1), Number(8))).simplified() == TRUE


def test_not_equal_neg() -> None:
    assert -NotEqual(Variable("X"), Number(1)) == Equal(Variable("X"), Number(1))


def test_not_equal_simplified() -> None:
    assert NotEqual(Number(0), Number(1)).simplified() == TRUE
    assert NotEqual(Number(1), Number(1)).simplified() == FALSE
    assert NotEqual(Number(2), Number(1)).simplified() == TRUE
    assert NotEqual(Div(Number(1), Number(8)), Div(Number(1), Number(8))).simplified() == FALSE
    assert NotEqual(Div(Number(1), Number(8)), Div(Number(2), Number(8))).simplified() == TRUE
    assert NotEqual(Div(Number(1), Number(6)), Div(Number(1), Number(8))).simplified() == TRUE


def test_in_neg() -> None:
    assert -In(Number(1), Variable("X")) == NotIn(Number(1), Variable("X"))


def test_in_simplified() -> None:
    assert_equal(
        In(Add(Number(21), Number(21)), Variable("X")).simplified(),
        In(Number(42), Variable("X")),
    )


def test_in_str() -> None:
    assert str(In(Variable("X"), Variable("Y"))) == "X in Y"


def test_not_in_neg() -> None:
    assert -NotIn(Number(1), Variable("X")) == In(Number(1), Variable("X"))


def test_not_in_simplified() -> None:
    assert_equal(
        NotIn(Add(Number(21), Number(21)), Variable("X")).simplified(),
        NotIn(Number(42), Variable("X")),
    )


def test_not_in_str() -> None:
    assert str(NotIn(Variable("X"), Variable("Y"))) == "X not in Y"


def test_if_expr_str() -> None:
    assert str(IfExpr([(Variable("X"), Variable("Y"))], Variable("Z"))) == "(if X then Y else Z)"
    assert str(IfExpr([(Variable("X"), Variable("Y"))])) == "(if X then Y)"
    assert str(
        IfExpr([(Variable("X" * 30), Variable("Y" * 30))], Variable("Z" * 30)),
    ) == textwrap.dedent(
        """\
        (if
            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         then
            YYYYYYYYYYYYYYYYYYYYYYYYYYYYYY
         else
            ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ)""",
    )
    assert str(IfExpr([(Variable("X" * 50), Variable("Y" * 50))])) == textwrap.dedent(
        """\
        (if
            XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
         then
            YYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYYY)""",
    )


def test_if_expr_substituted() -> None:
    assert IfExpr([(Variable("X"), Variable("Y"))], Variable("Z")).substituted(
        lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
    ) == IfExpr([(Variable("P_X"), Variable("P_Y"))], Variable("P_Z"))
    assert IfExpr([(Variable("X"), Variable("Y"))], Variable("Z")).substituted(
        lambda x: Variable("X") if isinstance(x, IfExpr) else x,
    ) == Variable("X")


def test_if_expr_simplified() -> None:
    assert IfExpr([(TRUE, Variable("X"))], Variable("Y")).simplified() == Variable("X")
    assert IfExpr([(FALSE, Variable("X"))], Variable("Y")).simplified() == Variable("Y")
    assert IfExpr([(Variable("X"), Variable("Y"))], Variable("Y")).simplified() == Variable("Y")
    assert IfExpr(
        [(Variable("X"), Variable("Y")), (Variable("Y"), Variable("X"))],
        Variable("Z"),
    ).simplified() == IfExpr(
        [(Variable("X"), Variable("Y")), (Variable("Y"), Variable("X"))],
        Variable("Z"),
    )


def test_value_range_type() -> None:
    assert_type(
        ValueRange(Number(1), Number(42)),
        ty.Any(),
    )


def test_value_range_type_error() -> None:
    assert_type_error(
        ValueRange(
            Variable("X", type_=ty.BOOLEAN, location=Location((10, 30))),
            Variable("Y", type_=ty.Sequence("A", INT_TY), location=Location((10, 40))),
            location=Location((10, 20)),
        ),
        r"^"
        r"<stdin>:10:30: error: expected integer type\n"
        r'<stdin>:10:30: error: found enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:40: error: expected integer type\n"
        r'<stdin>:10:40: error: found sequence type "A"'
        r' with element integer type "I" \(10 \.\. 100\)'
        r"$",
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
            lambda x: Variable("Z") if isinstance(x, ValueRange) else x,
        ),
        Variable("Z"),
    )


@pytest.mark.parametrize("expr", [ForAllIn, ForSomeIn])
def test_quantified_expression_type(expr: Callable[[str, Expr, Expr], Expr]) -> None:
    assert_type(
        expr(
            "X",
            Variable("Y", type_=ty.Sequence("A", INT_TY)),
            Variable("Z", type_=ty.BOOLEAN),
        ),
        ty.BOOLEAN,
    )


@pytest.mark.parametrize("expr", [ForAllIn, ForSomeIn])
@pytest.mark.parametrize(
    ("iterable", "predicate", "match"),
    [
        (
            Variable("Y", type_=ty.BOOLEAN, location=Location((10, 30))),
            Variable("Z", type_=ty.Sequence("A", INT_TY), location=Location((10, 40))),
            r"^<stdin>:10:30: error: expected composite type\n"
            r'<stdin>:10:30: error: found enumeration type "__BUILTINS__::Boolean"\n'
            r'<stdin>:10:40: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r'<stdin>:10:40: error: found sequence type "A"'
            r' with element integer type "I" \(10 \.\. 100\)$',
        ),
        (
            Variable("Y", type_=ty.BOOLEAN, location=Location((10, 30))),
            Equal(Variable("X"), Number(1)),
            r"^<stdin>:10:30: error: expected composite type\n"
            r'<stdin>:10:30: error: found enumeration type "__BUILTINS__::Boolean"$',
        ),
        (
            Variable("Y", type_=ty.Sequence("A", ty.BOOLEAN)),
            Equal(Variable("X"), Number(1, location=Location((10, 30)))),
            r'^<stdin>:10:30: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:10:30: error: found type universal integer \(1 .. 1\)$",
        ),
    ],
)
def test_quantified_expression_type_error(
    expr: Callable[[str, Expr, Expr, Location], Expr],
    iterable: Expr,
    predicate: Expr,
    match: str,
) -> None:
    assert_type_error(
        expr(
            "X",
            iterable,
            predicate,
            Location((10, 20)),
        ),
        match,
    )


def test_quantified_expression_substituted() -> None:
    assert_equal(
        ForAllOf("X", Variable("Y"), Add(Last("Z"), Add(Number(21), Number(21)))).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        ForAllOf("X", Variable("P_Y"), Add(Last("P_Z"), Add(Number(21), Number(21)))),
    )


def test_quantified_expression_substituted_location() -> None:
    expr = ForAllOf("X", Variable("Y"), TRUE, location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


def test_quantified_expression_simplified() -> None:
    assert_equal(
        ForAllOf("X", Variable("List"), Add(Last("Y"), Add(Number(21), Number(21)))).simplified(),
        ForAllOf("X", Variable("List"), Add(Last("Y"), Number(42))),
    )


def test_quantified_expression_variables() -> None:
    assert_equal(
        ForAllOf(
            "A",
            Variable("List"),
            Add(Variable("X"), Add(Variable("Y"), Variable("Z"))),
        ).variables(),
        [Variable("List"), Variable("X"), Variable("Y"), Variable("Z")],
    )


def test_quantified_expression_str() -> None:
    assert str(ForAllOf("X", Variable("Y"), Variable("Z"))) == "(for all X of Y =>\n    Z)"
    assert str(ForAllIn("X", Variable("Y"), Variable("Z"))) == "(for all X in Y =>\n    Z)"
    assert str(ForSomeIn("X", Variable("Y"), Variable("Z"))) == "(for some X in Y =>\n    Z)"


def test_for_all_in_variables() -> None:
    result = ForAllIn(
        "Q",
        Variable("List"),
        Equal(Selected(Variable("Q"), "Fld"), Variable("X")),
    ).variables()
    expected = [Variable("List"), Variable("X")]
    assert result == expected


def test_for_some_in_variables() -> None:
    result = ForSomeIn(
        "Q",
        Variable("List"),
        Equal(Selected(Variable("Q"), "Fld"), Variable("X")),
    ).variables()
    expected = [Variable("List"), Variable("X")]
    assert result == expected


def test_expr_contains() -> None:
    assert Variable("X") in Or(
        Greater(Variable("Y"), Number(42)),
        And(TRUE, Less(Variable("X"), Number(42))),
    )
    assert Variable("Z") not in Or(
        Greater(Variable("Y"), Number(42)),
        And(TRUE, Less(Variable("X"), Number(42))),
    )
    assert Less(Variable("X"), Number(42)) in Or(
        Greater(Variable("Y"), Number(42)),
        And(TRUE, Less(Variable("X"), Number(42))),
    )
    assert Less(Variable("Z"), Number(42)) not in Or(
        Greater(Variable("Y"), Number(42)),
        And(TRUE, Less(Variable("X"), Number(1))),
    )


def test_expr_variables() -> None:
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)),
            And(TRUE, Less(Variable("X"), Number(42))),
        ).variables(),
        [Variable("Y"), Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)),
            And(TRUE, Less(Variable("X"), Number(42))),
        ).variables(),
        [Variable("Y"), Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)),
            And(TRUE, Less(Variable("X"), Number(42))),
        ).variables(),
        [Variable("Y"), Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)),
            And(TRUE, Less(Variable("X"), Number(1))),
        ).variables(),
        [Variable("Y"), Variable("X")],
    )


def test_expr_variables_duplicates() -> None:
    assert_equal(
        And(Variable("X"), Variable("Y"), Variable("X")).variables(),
        [Variable("X"), Variable("Y")],
    )
    assert_equal(
        Or(Variable("X"), Variable("Y"), Variable("X")).variables(),
        [Variable("X"), Variable("Y")],
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
            Greater(Variable("X"), Number(42)),
            And(TRUE, Less(Variable("X"), Number(1))),
        ).variables(),
        [Variable("X")],
    )


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_expr_substituted_pre() -> None:
    mapping: Mapping[Name, Expr] = {Variable("X"): Number(1)}
    with pytest.raises(AssertionError):
        Number(1).substituted(lambda x: x, mapping)  # pragma: no branch
    with pytest.raises(AssertionError):
        Add(Number(1), Number(1)).substituted(lambda x: x, mapping)  # pragma: no branch
    with pytest.raises(AssertionError):
        Selected(Variable("X"), "F").substituted(lambda x: x, mapping)  # pragma: no branch
    with pytest.raises(AssertionError):
        Call("Sub", ty.BASE_INTEGER).substituted(lambda x: x, mapping)  # pragma: no branch
    with pytest.raises(AssertionError):
        ForAllOf("X", Variable("Y"), Variable("Z")).substituted(  # pragma: no branch
            lambda x: x,
            mapping,
        )
    with pytest.raises(AssertionError):
        Conversion("X", Variable("Y")).substituted(lambda x: x, mapping)  # pragma: no branch
    with pytest.raises(AssertionError):
        Comprehension(  # pragma: no branch
            "X",
            Variable("Y"),
            Variable("Z"),
            Variable("A"),
        ).substituted(lambda x: x, mapping)
    with pytest.raises(AssertionError):
        MessageAggregate("X", {"A": Number(5)}).substituted(
            lambda x: x,
            mapping,
        )  # pragma: no branch


def test_length_variables() -> None:
    assert Length("Z").variables() == [Variable("Z")]


def test_last_variables() -> None:
    assert Last("Z").variables() == [Variable("Z")]


def test_first_variables() -> None:
    assert First("Z").variables() == [Variable("Z")]


def test_size_variables() -> None:
    assert Size("Z").variables() == [Variable("Z")]


def test_valid_variables() -> None:
    assert Valid(Variable("X")).variables() == [Variable("X")]


def test_present_variables() -> None:
    assert Present(Variable("X")).variables() == [Variable("X")]


def test_head_variables() -> None:
    assert Head(Variable("X")).variables() == [Variable("X")]


def test_opaque_variables() -> None:
    assert Opaque(Variable("X")).variables() == [Variable("X")]


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


def test_number_simplified_location() -> None:
    assert Number(42, location=Location((1, 1))).simplified().location == Location((1, 1))


def test_string_variables() -> None:
    assert not String("X").variables()


def test_string_simplified() -> None:
    assert String("Test").simplified() == String("Test")


def test_string_substituted() -> None:
    assert String("Test").substituted(
        lambda x: String("TestSub") if x == String("Test") else x,
    ) == String("TestSub")


def test_string_elements() -> None:
    assert String("Test").elements == [Number(84), Number(101), Number(115), Number(116)]


def test_string_str() -> None:
    assert str(String("X Y")) == '"X Y"'
    assert str(Equal(String("S"), Variable("X"))) == '"S" = X'


def test_selected_type() -> None:
    assert_type(
        Selected(Variable("X", type_=ty.Message("M", {("F",)}, {ID("F"): INT_TY})), "F"),
        INT_TY,
    )


@pytest.mark.parametrize(
    ("expr", "match"),
    [
        (
            Selected(Variable("X", type_=ty.BOOLEAN, location=Location((10, 20))), "Y"),
            r"^<stdin>:10:20: error: expected message type\n"
            r'<stdin>:10:20: error: found enumeration type "__BUILTINS__::Boolean"$',
        ),
        (
            Selected(
                Variable(
                    "X",
                    type_=ty.Message("M", {("F",)}, {ID("F"): INT_TY}),
                ),
                "Y",
                location=Location((10, 20)),
            ),
            r'^<stdin>:10:20: error: invalid field "Y" for message type "M"$',
        ),
        (
            Selected(
                Variable(
                    "X",
                    type_=ty.Message(
                        "M",
                        {("F1",), ("F2",)},
                        {ID("F1"): INT_TY, ID("F2"): INT_TY},
                    ),
                ),
                "F",
                location=Location((10, 20)),
            ),
            r'^<stdin>:10:20: error: invalid field "F" for message type "M"\n'
            r"<stdin>:10:20: help: similar field names: F1, F2$",
        ),
    ],
)
def test_selected_type_error(expr: Expr, match: str) -> None:
    assert_type_error(
        expr,
        match,
    )


def test_selected_substituted() -> None:
    assert_equal(
        Selected(Variable("X"), "Y").substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        Selected(Variable("P_X"), "Y"),
    )
    assert_equal(
        Selected(Variable("X"), "Y").substituted(
            lambda x: Variable("Z") if isinstance(x, Selected) else x,
        ),
        Variable("Z"),
    )


def test_selected_substituted_location() -> None:
    expr = Selected(Variable("X"), "Y", location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


def test_selected_variables() -> None:
    result = Selected(Variable("X"), "Y").variables()
    expected = [Variable("X")]
    assert result == expected


def test_in_variables() -> None:
    result = In(Variable("A"), Variable("B")).variables()
    expected = [Variable("A"), Variable("B")]
    assert result == expected


def test_call_type() -> None:
    assert_type(
        Call(
            "X",
            ty.BOOLEAN,
            [Variable("Y", type_=INT_TY)],
            argument_types=[INT_TY],
        ),
        ty.BOOLEAN,
    )


def test_call_type_error() -> None:
    assert_type_error(
        Call(
            "X",
            ty.UNDEFINED,
            [Variable("Y", location=Location((10, 30)))],
            location=Location((10, 20)),
        ),
        r'^<stdin>:10:30: error: undefined variable "Y"\n'
        r'<stdin>:10:20: error: undefined function "X"$',
    )
    assert_type_error(
        Call(
            "X",
            ty.BOOLEAN,
            [
                Variable("Y", type_=INT_TY, location=Location((10, 30))),
                Variable("Z", type_=ty.BOOLEAN, location=Location((10, 40))),
            ],
            argument_types=[
                ty.BOOLEAN,
                INT_TY,
            ],
        ),
        r'^<stdin>:10:30: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r'<stdin>:10:30: error: found integer type "I" \(10 \.\. 100\)\n'
        r'<stdin>:10:40: error: expected integer type "I" \(10 \.\. 100\)\n'
        r'<stdin>:10:40: error: found enumeration type "__BUILTINS__::Boolean"$',
    )


def test_call_variables() -> None:
    result = Call("Sub", ty.BASE_INTEGER, [Variable("A"), Variable("B")]).variables()
    expected = [Variable("Sub"), Variable("A"), Variable("B")]
    assert result == expected


def test_call_findall() -> None:
    assert Call("X", ty.BASE_INTEGER, [Variable("Y"), Variable("Z")]).findall(
        lambda x: isinstance(x, Variable),
    ) == [
        Variable("Y"),
        Variable("Z"),
    ]


def test_call_str() -> None:
    assert str(Call("Test", ty.BASE_INTEGER, [])) == "Test"


def test_call_neg() -> None:
    assert -Call("Test", ty.BASE_INTEGER, []) == Neg(Call("Test", ty.BASE_INTEGER, []))


def test_conversion_type() -> None:
    assert_type(
        Conversion(
            "X",
            Selected(Variable("Y", type_=ty.Message("Y", {("Z",)}, {ID("Z"): ty.OPAQUE})), "Z"),
            type_=ty.Message("X"),
            argument_types=[ty.Message("Y")],
        ),
        ty.Message("X"),
    )


def test_conversion_type_error() -> None:
    assert_type_error(
        Conversion(
            "X",
            Selected(Variable("Y", location=Location((10, 30))), "Z"),
            location=Location((10, 20)),
        ),
        r'^<stdin>:10:30: error: undefined variable "Y"\n'
        r'<stdin>:10:20: error: invalid conversion to "X"\n'
        r'<stdin>:10:20: error: undefined type "X"$',
    )


def test_conversion_simplified() -> None:
    assert Conversion("X", Add(Number(1), Number(2))).simplified() == Conversion("X", Number(3))


def test_conversion_substituted() -> None:
    assert_equal(
        Conversion("X", Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        Conversion("X", Variable("P_Y")),
    )
    assert_equal(
        Conversion("X", Variable("Y")).substituted(
            lambda x: Variable("Z") if isinstance(x, Conversion) else x,
        ),
        Variable("Z"),
    )


def test_conversion_substituted_location() -> None:
    expr = Conversion("X", Variable("Y"), location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


def test_conversion_variables() -> None:
    result = Conversion("Sub", Variable("X")).variables()
    expected = [Variable("X")]
    assert result == expected


def test_qualified_expr_simplified() -> None:
    assert QualifiedExpr("X", Add(Number(21), Number(21))).simplified() == QualifiedExpr(
        "X",
        Number(42),
    )


def test_comprehension_type() -> None:
    assert_type(
        Comprehension(
            "X",
            Variable("Y", type_=ty.Sequence("A", INT_TY)),
            Add(Variable("X"), Variable("Z", type_=INT_TY)),
            TRUE,
        ),
        ty.Aggregate(ty.BASE_INTEGER),
    )
    assert_type(
        Comprehension(
            "X",
            Selected(
                Variable(
                    "Y",
                    type_=ty.Message(
                        "M",
                        {("F",)},
                        {ID("F"): ty.Sequence("A", INT_TY)},
                    ),
                ),
                "F",
            ),
            Variable("X"),
            Equal(Variable("X"), Number(1)),
        ),
        ty.Aggregate(INT_TY),
    )


def test_comprehension_type_error() -> None:
    assert_type_error(
        Comprehension(
            "X",
            Variable("Y", location=Location((10, 20))),
            Variable("X", location=Location((10, 30))),
            TRUE,
        ),
        r'^<stdin>:10:20: error: undefined variable "Y"$',
    )


def test_comprehension_simplified() -> None:
    assert Comprehension(
        "X",
        Variable("Y"),
        Add(Number(1), Number(2)),
        TRUE,
    ).simplified() == Comprehension("X", Variable("Y"), Number(3), TRUE)


def test_comprehension_substituted() -> None:
    assert_equal(
        Comprehension("X", Variable("Y"), Variable("Z"), TRUE).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        Comprehension("X", Variable("P_Y"), Variable("P_Z"), TRUE),
    )
    assert_equal(
        Comprehension("X", Variable("Y"), Variable("Z"), TRUE).substituted(
            lambda x: Variable("Z") if isinstance(x, Comprehension) else x,
        ),
        Variable("Z"),
    )


def test_comprehension_substituted_location() -> None:
    expr = Comprehension(
        "X",
        Variable("Y"),
        Variable("Z"),
        TRUE,
        location=Location((1, 2)),
    ).substituted(lambda x: x)
    assert expr.location


def test_comprehension_variables() -> None:
    result = Comprehension(
        "I",
        Variable("List"),
        Selected(Variable("I"), "Data"),
        Less(Selected(Variable("I"), "X"), Variable("Z")),
    ).variables()
    expected = [Variable("List"), Variable("Z")]
    assert result == expected


def test_comprehension_str() -> None:
    assert (
        str(Comprehension("X", Variable("Y"), Variable("Z"), TRUE)) == "[for X in Y if True => Z]"
    )
    assert (
        str(In(Variable("A"), Comprehension("X", Variable("Y"), Variable("Z"), TRUE)))
        == "A in [for X in Y if True => Z]"
    )


@pytest.mark.parametrize(
    ("field_values", "type_"),
    [
        (
            {"X": Variable("A", type_=INT_TY), "Y": Variable("B", type_=ty.BOOLEAN)},
            ty.Message(
                "M",
                {
                    ("X",),
                    ("X", "Y"),
                    ("X", "Y", "Z"),
                },
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
        ),
        (
            {"X": Variable("A", type_=ty.Message("I"))},
            ty.Message(
                "M",
                {
                    ("X",),
                },
                {},
                {
                    ID("X"): ty.OPAQUE,
                },
                [
                    ty.Refinement(ID("X"), ty.Message("I"), "P"),
                    ty.Refinement(ID("X"), ty.Message("J"), "P"),
                ],
            ),
        ),
    ],
)
def test_message_aggregate_type(field_values: Mapping[StrID, Expr], type_: ty.Type) -> None:
    assert_type(
        MessageAggregate(
            "M",
            field_values,
            type_=type_,
        ),
        type_,
    )


@pytest.mark.parametrize(
    ("field_values", "type_", "match"),
    [
        (
            {
                "X": Variable("A", location=Location((10, 30))),
                "Y": Variable("B", location=Location((10, 40))),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:30: error: undefined variable "A"\n'
            r'<stdin>:10:40: error: undefined variable "B"$',
        ),
        (
            {
                "X": Variable("A", type_=INT_TY),
                "Y": Variable("B", type_=ty.BOOLEAN),
                ID("Z", location=Location((10, 50))): Variable("Z", type_=INT_TY),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:50: error: invalid field "Z" for message type "M"$',
        ),
        (
            {
                ID("Y", location=Location((10, 30))): Variable("B", type_=ty.BOOLEAN),
                "X": Variable("A", type_=INT_TY),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:30: error: invalid position for field "Y" of message type "M"$',
        ),
        (
            {
                "X": Variable("A", type_=INT_TY),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                    ("X", "Y", "Z"),
                },
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                    ID("Z"): INT_TY,
                },
            ),
            r'^<stdin>:10:20: error: missing fields for message type "M"\n'
            r"<stdin>:10:20: note: possible next fields: Y$",
        ),
        (
            {
                "X": Variable("A", location=Location((10, 40))),
                "Y": Variable("B", location=Location((10, 30))),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y", "Z"),
                },
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                    ID("Z"): INT_TY,
                },
            ),
            r'^<stdin>:10:40: error: undefined variable "A"\n'
            r'<stdin>:10:30: error: undefined variable "B"\n'
            r'<stdin>:10:20: error: missing fields for message type "M"\n'
            r"<stdin>:10:20: note: possible next fields: Z$",
        ),
        (
            {
                "X": Variable("A", location=Location((10, 40))),
                "Y": Literal("B", location=Location((10, 30))),
            },
            ty.Undefined(),
            r'^<stdin>:10:40: error: undefined variable "A"\n'
            r'<stdin>:10:30: error: undefined literal "B"\n'
            r'<stdin>:10:20: error: undefined message "X"$',
        ),
    ],
)
def test_message_aggregate_type_error(
    field_values: Mapping[StrID, Expr],
    type_: ty.Type,
    match: str,
) -> None:
    assert_type_error(
        MessageAggregate("X", field_values, type_=type_, location=Location((10, 20))),
        match,
    )


def test_message_aggregate_simplified() -> None:
    assert MessageAggregate(
        "X",
        {"Y": Add(Number(1), Number(2)), "Z": Variable("B")},
    ).simplified() == MessageAggregate("X", {"Y": Number(3), "Z": Variable("B")})


def test_message_aggregate_substituted() -> None:
    assert_equal(
        MessageAggregate("X", {"Y": Variable("A"), "Z": Variable("B")}).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        MessageAggregate("X", {"Y": Variable("P_A"), "Z": Variable("P_B")}),
    )
    assert_equal(
        MessageAggregate("X", {"Y": Variable("A"), "Z": Variable("B")}).substituted(
            lambda x: Variable("Z") if isinstance(x, MessageAggregate) else x,
        ),
        Variable("Z"),
    )


def test_message_aggregate_substituted_location() -> None:
    expr = MessageAggregate(
        "X",
        {"Y": Variable("A"), "Z": Variable("B")},
        location=Location((1, 2)),
    ).substituted(lambda x: x)
    assert expr.location


def test_message_aggregate_variables() -> None:
    result = MessageAggregate(
        "Aggr",
        {"X": Variable("A"), "Y": Variable("B"), "Baz": Variable("C")},
    ).variables()
    expected = [Variable("A"), Variable("B"), Variable("C")]
    assert result == expected


@pytest.mark.parametrize(
    ("field_values", "type_"),
    [
        (
            {"Y": Variable("A", type_=INT_TY), "Z": Variable("B", type_=ty.BOOLEAN)},
            ty.Message(
                "M",
                {
                    ("X",),
                    ("X", "Y"),
                    ("X", "Y", "Z"),
                },
                {},
                {
                    ID("Y"): INT_TY,
                    ID("Z"): ty.BOOLEAN,
                },
            ),
        ),
        (
            {"Y": Variable("A", type_=ty.Message("I"))},
            ty.Message(
                "M",
                {
                    ("X", "Y", "Z"),
                },
                {},
                {
                    ID("Y"): ty.OPAQUE,
                },
                [
                    ty.Refinement(ID("Y"), ty.Message("I"), "P"),
                    ty.Refinement(ID("Y"), ty.Message("J"), "P"),
                ],
            ),
        ),
    ],
)
def test_delta_message_aggregate_type(field_values: Mapping[StrID, Expr], type_: ty.Type) -> None:
    assert_type(
        DeltaMessageAggregate(
            "M",
            field_values,
            type_=type_,
        ),
        type_,
    )


@pytest.mark.parametrize(
    ("field_values", "type_", "match"),
    [
        (
            {
                "X": Variable("A", location=Location((10, 30))),
                "Y": Variable("B", location=Location((10, 40))),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {},
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:30: error: undefined variable "A"\n'
            r'<stdin>:10:40: error: undefined variable "B"$',
        ),
        (
            {
                "X": Variable("A", type_=INT_TY),
                "Y": Variable("B", type_=ty.BOOLEAN),
                ID("Z", location=Location((10, 50))): Variable("Z", type_=INT_TY),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {},
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:50: error: invalid field "Z" for message type "M"$',
        ),
        (
            {
                "Y": Variable("B", type_=ty.BOOLEAN),
                ID("X", location=Location((10, 30))): Variable("A", type_=INT_TY),
            },
            ty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {},
                {
                    ID("X"): INT_TY,
                    ID("Y"): ty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:30: error: invalid position for field "X" of message type "M"$',
        ),
        (
            {
                "X": Variable("A", location=Location((10, 40))),
                "Y": Variable("B", location=Location((10, 30))),
            },
            ty.Undefined(),
            r'^<stdin>:10:40: error: undefined variable "A"\n'
            r'<stdin>:10:30: error: undefined variable "B"\n'
            r'<stdin>:10:20: error: undefined message "T"$',
        ),
    ],
)
def test_delta_message_aggregate_type_error(
    field_values: Mapping[StrID, Expr],
    type_: ty.Type,
    match: str,
) -> None:
    assert_type_error(
        DeltaMessageAggregate("T", field_values, type_=type_, location=Location((10, 20))),
        match,
    )


def test_delta_message_aggregate_substituted() -> None:
    assert_equal(
        DeltaMessageAggregate("X", {"Y": Variable("A"), "Z": Variable("B")}).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
        ),
        DeltaMessageAggregate("X", {"Y": Variable("P_A"), "Z": Variable("P_B")}),
    )
    assert_equal(
        DeltaMessageAggregate("X", {"Y": Variable("A"), "Z": Variable("B")}).substituted(
            lambda x: Variable("Z") if isinstance(x, DeltaMessageAggregate) else x,
        ),
        Variable("Z"),
    )


def test_delta_message_aggregate_substituted_location() -> None:
    expr = DeltaMessageAggregate(
        "X",
        {"Y": Variable("A"), "Z": Variable("B")},
        location=Location((1, 2)),
    ).substituted(lambda x: x)
    assert expr.location


def test_delta_message_aggregate_simplified() -> None:
    assert_equal(
        DeltaMessageAggregate(
            "X",
            {"Y": Variable("A"), "Z": Add(Number(1), Number(2))},
        ).simplified(),
        DeltaMessageAggregate("X", {"Y": Variable("A"), "Z": Number(3)}),
    )


def test_delta_message_aggregate_variables() -> None:
    result = DeltaMessageAggregate(
        "Aggr",
        {"X": Variable("A"), "Y": Variable("B"), "Baz": Variable("C")},
    ).variables()
    expected = [Variable("A"), Variable("B"), Variable("C")]
    assert result == expected


def test_indexed_neg() -> None:
    assert -Indexed(Variable("X"), Variable("Y")) == Neg(
        Indexed(
            Variable("X"),
            Variable("Y"),
        ),
    )


def test_case_variables() -> None:
    assert_equal(
        CaseExpr(
            Variable("C"),
            [([ID("V1"), ID("V2")], Number(1)), ([ID("V3")], Variable("E"))],
        ).variables(),
        [Variable("C"), Variable("E")],
    )


def test_case_substituted() -> None:
    c = CaseExpr(
        Variable("C"),
        [([ID("V1"), ID("V2")], Variable("E1")), ([ID("V3")], Variable("E2"))],
    )
    assert_equal(
        c.substituted(lambda x: Number(42) if x == Variable("E1") else x),
        CaseExpr(Variable("C"), [([ID("V1"), ID("V2")], Number(42)), ([ID("V3")], Variable("E2"))]),
    )
    assert_equal(
        c.substituted(
            lambda x: Number(42) if isinstance(x, Variable) and x.name.startswith("E") else x,
        ),
        CaseExpr(Variable("C"), [([ID("V1"), ID("V2")], Number(42)), ([ID("V3")], Number(42))]),
    )
    assert_equal(
        c.substituted(lambda x: Variable("C_Subst") if x == Variable("C") else x),
        CaseExpr(
            Variable("C_Subst"),
            [([ID("V1"), ID("V2")], Variable("E1")), ([ID("V3")], Variable("E2"))],
        ),
    )
    assert_equal(
        c.substituted(lambda x: Variable("C_Subst") if isinstance(x, CaseExpr) else x),
        Variable("C_Subst"),
    )


def test_case_substituted_location() -> None:
    c = CaseExpr(
        Variable("C"),
        [([ID("V1"), ID("V2")], Variable("E1")), ([ID("V3")], Variable("E2"))],
        location=Location((1, 2)),
    ).substituted(lambda x: x)
    assert c.location


def test_case_findall() -> None:
    assert_equal(
        CaseExpr(
            Variable("C1"),
            [([ID("V1"), ID("V2")], Variable("E1")), ([ID("V3")], Variable("E2"))],
        ).findall(lambda x: isinstance(x, Variable) and x.name.endswith("1")),
        [Variable("C1"), Variable("E1")],
    )


def test_case_type() -> None:
    c1 = Variable("C", type_=models.enumeration().type_)
    assert_type(CaseExpr(c1, [([ID("Zero"), ID("One")], TRUE), ([ID("Two")], FALSE)]), ty.BOOLEAN)
    assert_type(
        CaseExpr(c1, [([ID("Zero"), ID("One")], Number(1)), ([ID("Two")], Number(2))]),
        ty.UniversalInteger(ty.Bounds(1, 2)),
    )

    c2 = Variable("C", type_=TINY_INT.type_)
    assert_type(CaseExpr(c2, [([Number(1), Number(2)], TRUE), ([Number(3)], FALSE)]), ty.BOOLEAN)

    assert_type_error(
        CaseExpr(
            c1,
            [
                ([ID("V1", location=Location((1, 1))), ID("V2", location=Location((1, 2)))], TRUE),
                ([ID("V3", location=Location((1, 3)))], Number(1, location=Location((1, 4)))),
            ],
        ),
        r'^__BUILTINS__:1:1: error: dependent expression "True" has incompatible enumeration type '
        r'"__BUILTINS__::Boolean"\n'
        r'<stdin>:1:4: note: conflicting with "1" which has type universal integer \(1 .. 1\)$',
    )
    assert_type_error(
        CaseExpr(
            Opaque(
                Variable(
                    ID("X", location=Location((1, 1))),
                    type_=ty.Message(ID("A", location=Location((1, 2)))),
                    location=Location((1, 3)),
                ),
            ),
            [([ID("V", location=Location((1, 4)))], Number(1, location=Location((1, 5))))],
        ),
        r'^<stdin>:1:3: error: invalid discrete choice with sequence type "__INTERNAL__::Opaque" '
        r'with element integer type "__INTERNAL__::Byte" \(0 .. 255\)\n'
        r"<stdin>:1:3: note: expected enumeration or integer type$",
    )


def test_case_simplified() -> None:
    assert_equal(
        CaseExpr(
            Variable("C"),
            [([ID("V1"), ID("V2")], And(TRUE, FALSE)), ([ID("V3")], FALSE)],
        ).simplified(),
        CaseExpr(Variable("C"), [([ID("V1"), ID("V2")], FALSE), ([ID("V3")], FALSE)]),
    )


def test_case_invalid() -> None:
    assert_type_error(
        CaseExpr(
            Variable("C", type_=models.enumeration().type_),
            [([ID("Zero")], TRUE), ([ID("One")], FALSE)],
            location=Location((1, 2)),
        ),
        "^<stdin>:1:2: error: not all enumeration literals covered by case expression\n"
        '<stdin>:10:2: note: missing literal "Two"$',
    )
    assert_type_error(
        CaseExpr(
            Variable("C", type_=models.enumeration().type_),
            [([ID("Zero"), ID("One")], TRUE), ([ID("Two"), ID("Invalid")], FALSE)],
            location=Location((1, 2)),
        ),
        "^<stdin>:1:2: error: invalid literals used in case expression\n"
        '<stdin>:10:2: note: literal "Invalid" not part of "P::Enumeration"$',
    )
    assert_type_error(
        CaseExpr(
            Variable("C", type_=models.enumeration().type_),
            [
                ([ID("Zero"), ID("One", location=Location((2, 2)))], TRUE),
                ([ID("Two"), ID("One", location=Location((3, 2)))], FALSE),
            ],
            location=Location((1, 2)),
        ),
        "^<stdin>:1:2: error: duplicate literals used in case expression\n"
        '<stdin>:3:2: note: duplicate literal "One"$',
    )

    assert_type_error(
        CaseExpr(
            Variable("C", type_=TINY_INT.type_),
            [([Number(1)], TRUE), ([Number(2)], FALSE)],
            location=Location((2, 2)),
        ),
        '^<stdin>:2:2: error: case expression does not cover full range of "P::Tiny"\n'
        "<stdin>:1:2: note: missing value 3$",
    )
    assert_type_error(
        CaseExpr(
            Variable("C", type_=TINY_INT.type_),
            [([Number(1)], TRUE)],
            location=Location((2, 2)),
        ),
        '^<stdin>:2:2: error: case expression does not cover full range of "P::Tiny"\n'
        "<stdin>:1:2: note: missing range 2 .. 3$",
    )
    assert_type_error(
        CaseExpr(
            Variable("C", type_=INT.type_),
            [([Number(1), Number(2)], TRUE), ([Number(51)], FALSE), ([Number(53)], TRUE)],
            location=Location((5, 2)),
        ),
        '^<stdin>:5:2: error: case expression does not cover full range of "P::Int"\n'
        "<stdin>:3:2: note: missing range 3 .. 50\n"
        "<stdin>:3:2: note: missing value 52\n"
        "<stdin>:3:2: note: missing range 54 .. 100$",
    )
    assert_type_error(
        CaseExpr(
            Variable("C", type_=TINY_INT.type_),
            [([Number(1), Number(2)], TRUE), ([Number(3), Number(4)], FALSE)],
            location=Location((2, 2)),
        ),
        "^<stdin>:2:2: error: invalid literals used in case expression\n"
        '<stdin>:1:2: note: value 4 not part of "P::Tiny"$',
    )
    assert_type_error(
        CaseExpr(
            Variable("C", type_=TINY_INT.type_),
            [
                ([Number(1), Number(2, location=Location((1, 8)))], TRUE),
                ([Number(3), Number(2, location=Location((1, 14)))], FALSE),
            ],
            location=Location((1, 2)),
        ),
        "^<stdin>:1:2: error: duplicate literals used in case expression\n"
        '<stdin>:1:14: note: duplicate literal "2"$',
    )


def test_invalid_division_by_zero() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(r"^error: division by zero$"),
    ):
        Div(Number(255), Number(0)).simplified()


def test_invalid_modulo_by_zero() -> None:
    with pytest.raises(
        RecordFluxError,
        match=(r"^error: modulo by zero$"),
    ):
        Mod(Number(255), Number(0)).simplified()
