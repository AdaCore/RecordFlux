# pylint: disable=too-many-lines

from typing import Callable, Mapping

import pytest
import z3

import rflx.ada as ada
import rflx.typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.expression import (
    FALSE,
    TRUE,
    UNDEFINED,
    Add,
    Aggregate,
    And,
    AndThen,
    Attribute,
    Binding,
    Call,
    Comprehension,
    Conversion,
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
    In,
    Indexed,
    Last,
    Length,
    Less,
    LessEqual,
    MessageAggregate,
    Mod,
    Mul,
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
    Proof,
    ProofResult,
    Selected,
    Size,
    String,
    Sub,
    Val,
    Valid,
    ValidChecksum,
    ValueRange,
    Variable,
    Z3TypeError,
)
from rflx.identifier import ID, StrID
from tests.utils import assert_equal, multilinestr

EXPR = Equal(Variable("UNDEFINED_1"), Variable("UNDEFINED_2"))


def assert_type(expr: Expr, type_: rty.Type) -> None:
    expr.check_type(type_).propagate()
    assert expr.type_ == type_


def assert_type_error(expr: Expr, regex: str) -> None:
    with pytest.raises(RecordFluxError, match=regex):
        expr.check_type(rty.Any()).propagate()


def test_true_type() -> None:
    assert_type(
        TRUE,
        rty.BOOLEAN,
    )


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_true_neg() -> None:
    with pytest.raises(AssertionError):
        -TRUE  # pylint: disable = pointless-statement


def test_true_simplified() -> None:
    assert TRUE.simplified() == TRUE


def test_true_variables() -> None:
    assert TRUE.variables() == [TRUE]


def test_true_z3expr() -> None:
    assert TRUE.z3expr() == z3.BoolVal(True)


def test_false_type() -> None:
    assert_type(
        FALSE,
        rty.BOOLEAN,
    )


@pytest.mark.skipif(not __debug__, reason="depends on assertion")
def test_false_neg() -> None:
    with pytest.raises(AssertionError):
        -FALSE  # pylint: disable = pointless-statement


def test_false_simplified() -> None:
    assert FALSE.simplified() == FALSE


def test_false_variables() -> None:
    assert FALSE.variables() == [FALSE]


def test_false_z3expr() -> None:
    assert FALSE.z3expr() == z3.BoolVal(False)


def test_not_type() -> None:
    assert_type(
        Not(Variable("X", type_=rty.BOOLEAN)),
        rty.BOOLEAN,
    )


def test_not_type_error() -> None:
    assert_type_error(
        Not(Variable("X", type_=rty.AnyInteger(), location=Location((10, 20)))),
        r'^<stdin>:10:20: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:20: model: info: found integer type$",
    )


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
    assert Not(FALSE).z3expr() == z3.Not(z3.BoolVal(False))
    with pytest.raises(Z3TypeError):
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


def test_bin_expr_substituted_location() -> None:
    expr = Less(Variable("X"), Number(1), location=Location((1, 2))).substituted(lambda x: x)
    assert expr.location


def test_ass_expr_findall() -> None:
    assert_equal(
        And(Equal(Variable("X"), Number(1)), Less(Variable("Y"), Number(2))).findall(
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


def test_ass_expr_substituted_location() -> None:
    expr = And(
        Equal(Variable("X"), Number(1)), Variable("Y"), location=Location((1, 2))
    ).substituted(lambda x: x)
    assert expr.location


def test_bool_expr_str() -> None:
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


@pytest.mark.parametrize("operation", [And, Or])
def test_bool_expr_type(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        operation(Variable("X", type_=rty.BOOLEAN), Variable("Y", type_=rty.BOOLEAN)),
        rty.BOOLEAN,
    )


@pytest.mark.parametrize("operation", [And, Or])
def test_bool_expr_type_error(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        operation(
            Variable("X", type_=rty.Integer("A", rty.Bounds(0, 100)), location=Location((10, 20))),
            Number(1, location=Location((10, 30))),
        ),
        r'^<stdin>:10:20: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r'<stdin>:10:20: model: info: found integer type "A" \(0 .. 100\)\n'
        r'<stdin>:10:30: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:30: model: info: found type universal integer \(1\)$",
    )


@pytest.mark.parametrize("expression", [And, AndThen, Or, OrElse])
def test_bool_expr_ada_expr(expression: Callable[[Expr, Expr], Expr]) -> None:
    result = expression(Variable("X"), Variable("Y")).ada_expr()
    expected = getattr(ada, expression.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("expression", [And, AndThen, Or, OrElse])
def test_bool_expr_z3expr_error(expression: Callable[[Expr, Expr], Expr]) -> None:
    with pytest.raises(Z3TypeError):
        expression(Number(1), Number(2)).z3expr()


def test_and_neg() -> None:
    with pytest.raises(NotImplementedError):
        -And(Variable("X"), TRUE)  # pylint: disable=expression-not-assigned


def test_and_variables() -> None:
    assert_equal(And(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")])


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


def test_and_z3expr() -> None:
    assert_equal(
        And(TRUE, FALSE, TRUE).z3expr(),
        z3.And(z3.BoolVal(True), z3.BoolVal(False), z3.BoolVal(True)),
    )
    assert_equal(
        And(TRUE, TRUE, TRUE).z3expr(),
        z3.And(z3.BoolVal(True), z3.BoolVal(True), z3.BoolVal(True)),
    )
    assert_equal(And(TRUE, TRUE).z3expr(), z3.And(z3.BoolVal(True), z3.BoolVal(True)))


def test_and_str() -> None:
    assert str(And(Variable("X"), Variable("Y"))) == "X\nand Y"
    assert str(And()) == "True"


def test_or_neg() -> None:
    with pytest.raises(NotImplementedError):
        -Or(Variable("X"), TRUE)  # pylint: disable=expression-not-assigned


def test_or_variables() -> None:
    assert Or(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_or_contains() -> None:
    assert Variable("X") in Or(Less(Variable("X"), Number(42)), TRUE)
    assert Variable("Y") not in Or(Less(Variable("X"), Number(42)), TRUE)


def test_or_simplified() -> None:
    assert Or(TRUE, TRUE).simplified() == TRUE
    assert Or(TRUE, EXPR).simplified() == TRUE
    assert Or(EXPR, TRUE).simplified() == TRUE


def test_or_z3expr() -> None:
    assert_equal(
        Or(TRUE, FALSE, TRUE).z3expr(),
        z3.Or(z3.BoolVal(True), z3.BoolVal(False), z3.BoolVal(True)),
    )
    assert_equal(
        Or(TRUE, TRUE, TRUE).z3expr(),
        z3.Or(z3.BoolVal(True), z3.BoolVal(True), z3.BoolVal(True)),
    )
    assert_equal(Or(TRUE, TRUE).z3expr(), z3.Or(z3.BoolVal(True), z3.BoolVal(True)))


def test_or_str() -> None:
    assert str(Or(Variable("X"), Variable("Y"))) == "X\nor Y"
    assert str(Or()) == "True"


def test_undefined_simplified() -> None:
    assert UNDEFINED.simplified() == UNDEFINED


def test_undefined_str() -> None:
    assert str(UNDEFINED) == "__UNDEFINED__"


def test_number_type() -> None:
    assert_type(
        Number(1),
        rty.UniversalInteger(rty.Bounds(1, 1)),
    )


def test_number_neg() -> None:
    assert -Number(42) == Number(-42)


def test_number_simplified() -> None:
    assert Number(42).simplified() == Number(42)


def test_number_add() -> None:
    assert Number(5) + Number(3) == Number(8)


def test_number_sub() -> None:
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


@pytest.mark.parametrize("operation", [Add, Mul, Sub, Div, Pow])
def test_math_expr_type(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        operation(Variable("X", type_=rty.AnyInteger()), Variable("Y", type_=rty.AnyInteger())),
        rty.AnyInteger(),
    )
    assert_type(
        operation(Variable("X", type_=rty.Integer("A")), Variable("Y", type_=rty.Integer("A"))),
        rty.Integer("A"),
    )


@pytest.mark.parametrize("operation", [Add, Mul, Sub, Div, Pow])
def test_math_expr_type_error(operation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        operation(
            Variable("X", type_=rty.BOOLEAN, location=Location((10, 20))),
            Variable("True", type_=rty.BOOLEAN, location=Location((10, 30))),
        ),
        r"^<stdin>:10:20: model: error: expected integer type\n"
        r'<stdin>:10:20: model: info: found enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:30: model: error: expected integer type\n"
        r'<stdin>:10:30: model: info: found enumeration type "__BUILTINS__::Boolean"$',
    )


@pytest.mark.parametrize("expression", [Add, Mul, Sub, Div, Pow, Mod])
def test_math_expr_ada_expr(expression: Callable[[Expr, Expr], Expr]) -> None:
    result = expression(Variable("X"), Variable("Y")).ada_expr()
    expected = getattr(ada, expression.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("expression", [Add, Mul, Sub, Div, Pow, Mod])
def test_math_expr_z3expr_error(expression: Callable[[Expr, Expr], Expr]) -> None:
    with pytest.raises(Z3TypeError):
        expression(String("X"), Number(1)).z3expr()


def test_add_neg() -> None:
    assert -Add(Variable("X"), Number(1)) == Add(Variable("X", negative=True), Number(-1))


def test_add_variables() -> None:
    assert_equal(Add(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")])


def test_add_simplified() -> None:
    assert Add(Variable("X"), Number(1)).simplified() == Add(Variable("X"), Number(1))
    assert Add(Variable("X"), Number(0)).simplified() == Variable("X")
    assert Add(Number(2), Number(3), Number(5)).simplified() == Number(10)
    assert Add(Variable("X"), Variable("Y"), Variable("X", negative=True)).simplified() == Variable(
        "Y"
    )
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


def test_add_z3expr() -> None:
    assert Add(Number(42), Number(1)).z3expr() == z3.IntVal(42) + z3.IntVal(1)
    assert_equal(
        Add(Number(42), Number(1), Number(10)).z3expr(),
        z3.Sum(z3.IntVal(42), z3.IntVal(1), z3.IntVal(10)),
    )


def test_add_str() -> None:
    assert str(Add(Number(1), Call("Test", []))) == "1 + Test"
    assert str(Add(Number(1), -Call("Test", []))) == "1 - Test"
    assert str(Add()) == "0"


def test_mul_neg() -> None:
    assert -Mul(Variable("X"), Number(2)) == Mul(Variable("X"), Number(-2))


def test_mul_variables() -> None:
    assert_equal(Mul(Variable("X"), Variable("Y")).variables(), [Variable("X"), Variable("Y")])


def test_mul_simplified() -> None:
    assert Mul(Variable("X"), Number(2)).simplified() == Mul(Variable("X"), Number(2))
    assert Mul(Variable("X"), Number(1)).simplified() == Variable("X")
    assert Mul(Number(2), Number(3), Number(5)).simplified() == Number(30)


def test_mul_z3expr() -> None:
    assert Mul(Number(6), Number(4)).z3expr() == z3.IntVal(6) * z3.IntVal(4)
    assert_equal(
        Mul(Number(2), Number(4), Number(8)).z3expr(),
        z3.Product(z3.IntVal(2), z3.IntVal(4), z3.IntVal(8)),
    )


def test_sub_neg() -> None:
    assert -Sub(Number(1), Variable("X")) == Sub(Number(-1), Variable("X"))


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


def test_sub_z3expr() -> None:
    assert Sub(Number(6), Number(4)).z3expr() == z3.IntVal(6) - z3.IntVal(4)
    assert Sub(Number(12), Number(20)).z3expr() == z3.IntVal(12) - z3.IntVal(20)


def test_div_neg() -> None:
    assert -Div(Variable("X"), Number(1)) == Div(Variable("X", negative=True), Number(1))


def test_div_variables() -> None:
    assert Div(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_div_simplified() -> None:
    assert Div(Variable("X"), Number(1)).simplified() == Div(Variable("X"), Number(1))
    assert Div(Number(6), Number(2)).simplified() == Number(3)
    assert Div(Number(9), Number(2)).simplified() == Div(Number(9), Number(2))


def test_div_z3expr() -> None:
    assert Div(Number(6), Number(3)).z3expr() == z3.IntVal(6) / z3.IntVal(3)


def test_pow_simplified() -> None:
    assert Pow(Variable("X"), Number(1)).simplified() == Pow(Variable("X"), Number(1))
    assert Pow(Variable("X"), Add(Number(1), Number(1))).simplified() == Pow(
        Variable("X"), Number(2)
    )
    assert Pow(Number(6), Number(2)).simplified() == Number(36)


def test_pow_variables() -> None:
    assert Pow(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_pow_z3expr() -> None:
    assert Pow(Number(6), Number(2)).z3expr() == z3.IntVal(6) ** z3.IntVal(2)


def test_mod_simplified() -> None:
    assert Mod(Variable("X"), Number(1)).simplified() == Mod(Variable("X"), Number(1))
    assert Mod(Variable("X"), Add(Number(1), Number(1))).simplified() == Mod(
        Variable("X"), Number(2)
    )
    assert Mod(Number(6), Number(2)).simplified() == Number(0)


def test_mod_variables() -> None:
    assert Mod(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


def test_mod_z3expr() -> None:
    assert Mod(Number(1000), Number(5)).z3expr() == z3.IntVal(1000) % z3.IntVal(5)
    assert Mod(Pow(Number(6), Number(2)), Number(5)).z3expr() == z3.IntVal(36) % z3.IntVal(5)


def test_mod_z3expr_error() -> None:
    with pytest.raises(Z3TypeError):
        Mod(Pow(Variable("X"), Number(2)), Number(5)).z3expr()


def test_term_simplified() -> None:
    assert_equal(
        Add(
            Mul(Number(1), Number(6)), Sub(Variable("X"), Number(10)), Add(Number(1), Number(3))
        ).simplified(),
        Variable("X"),
    )


def test_variable_invalid_name() -> None:
    with pytest.raises(RecordFluxError):
        Variable("Foo (Bar)")


def test_variable_type() -> None:
    assert_type(
        Variable("X", type_=rty.BOOLEAN),
        rty.BOOLEAN,
    )
    assert_type(
        Variable("X", type_=rty.Integer("A")),
        rty.Integer("A"),
    )


def test_variable_type_error() -> None:
    assert_type_error(
        Variable("X", location=Location((10, 20))),
        r'^<stdin>:10:20: model: error: undefined variable "X"$',
    )


def test_variable_neg() -> None:
    assert -Variable("X") == Variable("X", negative=True)


def test_variable_variables() -> None:
    assert Variable("X").variables() == [Variable("X")]
    assert (-Variable("X")).variables() == [Variable("X", negative=True)]


def test_variable_substituted() -> None:
    assert_equal(
        Variable("X").substituted(lambda x: Number(42) if x == Variable("X") else x), Number(42)
    )


def test_mutable_variable_substituted() -> None:
    x = Variable("X")
    assert_equal(x.substituted(mapping={Variable("X"): Number(42)}), Number(42))


def test_immutable_variable_substituted() -> None:
    x = Variable("X", immutable=True)
    assert_equal(x.substituted(mapping={Variable("X"): Number(42)}), Variable("X"))


def test_variable_simplified() -> None:
    assert Variable("X").simplified() == Variable("X")


def test_variable_z3expr() -> None:
    assert Variable("X").z3expr() == z3.Int("X")
    assert Variable("X", negative=True).z3expr() == -z3.Int("X")
    assert z3.simplify(Sub(Variable("X"), Variable("X")).z3expr()) == z3.IntVal(0)


def test_attribute() -> None:
    assert isinstance(Size("X"), Attribute)
    assert isinstance(Length("X"), Attribute)
    assert isinstance(First("X"), Attribute)
    assert isinstance(Last("X"), Attribute)
    assert First("X") == First(Variable("X"))
    assert First("X") == First(ID("X"))
    assert First("X") == First(Variable(ID("X")))


@pytest.mark.parametrize(
    "attribute,expr,expected",
    [
        (Size, Variable("X", type_=rty.AnyInteger()), rty.UniversalInteger()),
        (Length, Variable("X", type_=rty.AnyInteger()), rty.UniversalInteger()),
        (First, Variable("X", type_=rty.AnyInteger()), rty.UniversalInteger()),
        (Last, Variable("X", type_=rty.AnyInteger()), rty.UniversalInteger()),
        (ValidChecksum, Variable("X", type_=rty.AnyInteger()), rty.BOOLEAN),
        (Valid, Variable("X", type_=rty.Message("A")), rty.BOOLEAN),
        (
            Present,
            Selected(
                Variable("X", type_=rty.Message("M", {("F",)}, {ID("F"): rty.Integer("A")})), "F"
            ),
            rty.BOOLEAN,
        ),
        (Head, Variable("X", type_=rty.Sequence("A", rty.Integer("B"))), rty.Integer("B")),
        (Opaque, Variable("X", type_=rty.Message("A")), rty.OPAQUE),
    ],
)
def test_attribute_type(attribute: Callable[[Expr], Expr], expr: Expr, expected: rty.Type) -> None:
    assert_type(
        attribute(expr),
        expected,
    )


@pytest.mark.parametrize(
    "expr,match",
    [
        (
            Present(Variable("X", location=Location((10, 30)))),
            r"^<stdin>:10:30: model: error: invalid prefix for attribute Present$",
        ),
        (
            Head(
                Comprehension(
                    "X",
                    Variable("Y", type_=rty.Sequence("A", rty.Integer("B"))),
                    Variable("X", type_=rty.Integer("B")),
                    TRUE,
                    location=Location((10, 30)),
                )
            ),
            r"^<stdin>:10:30: model: error: prefix of attribute Head must be a name$",
        ),
        (
            Opaque(
                Call(
                    "X", [Variable("Y", location=Location((10, 30)))], location=Location((10, 20))
                ),
            ),
            r'^<stdin>:10:30: model: error: undefined variable "Y"\n'
            r'<stdin>:10:20: model: error: undefined function "X"$',
        ),
    ],
)
def test_attribute_type_error(expr: Expr, match: str) -> None:
    assert_type_error(
        expr,
        match,
    )


def test_attribute_neg() -> None:
    assert -First("X") == First("X", negative=True)


def test_attributes_findall() -> None:
    assert First("X").findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_attribute_substituted() -> None:
    assert_equal(First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(42))
    assert_equal(
        -First("X").substituted(lambda x: Number(42) if x == First("X") else x), Number(-42)
    )
    assert_equal(
        First("X").substituted(lambda x: Call("Y") if x == Variable("X") else x),
        First(Call("Y")),
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
    assert First(Call("X", [Variable("Y")])).variables() == [Variable("X"), Variable("Y")]


@pytest.mark.parametrize(
    "attribute,z3name",
    [
        (Size("X"), "X'Size"),
        (Length("X"), "X'Length"),
        (First("X"), "X'First"),
        (Last("X"), "X'Last"),
    ],
)
def test_attribute_z3expr(attribute: Expr, z3name: str) -> None:
    assert attribute.z3expr() == z3.Int(z3name)
    assert (-attribute).z3expr() == -z3.Int(z3name)


def test_attribute_z3expr_error() -> None:
    with pytest.raises(Z3TypeError):
        First(Call("X")).z3expr()


def test_val_substituted() -> None:
    assert_equal(
        Val("X", Variable("Y")).substituted(
            lambda x: Number(42) if x == Val("X", Variable("Y")) else x
        ),
        Val("X", Variable("Y")),
    )
    assert_equal(
        -Val("X", Variable("Y")).substituted(
            lambda x: Number(42) if x == Val("X", Variable("Y")) else x
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
        rty.Aggregate(rty.UniversalInteger(rty.Bounds(0, 1))),
    )


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
        relation(Variable("X", type_=rty.AnyInteger()), Variable("Y", type_=rty.AnyInteger())),
        rty.BOOLEAN,
    )


@pytest.mark.parametrize("relation", [Less, LessEqual, Equal, GreaterEqual, Greater, NotEqual])
def test_relation_integer_type_error(relation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        relation(
            Variable("X", type_=rty.AnyInteger()),
            Variable("True", type_=rty.BOOLEAN, location=Location((10, 30))),
        ),
        r"^<stdin>:10:30: model: error: expected integer type\n"
        r'<stdin>:10:30: model: info: found enumeration type "__BUILTINS__::Boolean"$',
    )


@pytest.mark.parametrize("relation", [In, NotIn])
def test_relation_composite_type(relation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type(
        relation(
            Variable("X", type_=rty.AnyInteger()),
            Variable("Y", type_=rty.Sequence("A", rty.AnyInteger())),
        ),
        rty.BOOLEAN,
    )


@pytest.mark.parametrize("relation", [In, NotIn])
def test_relation_composite_type_error(relation: Callable[[Expr, Expr], Expr]) -> None:
    assert_type_error(
        relation(
            Variable("X", type_=rty.AnyInteger(), location=Location((10, 20))),
            Variable("True", type_=rty.BOOLEAN, location=Location((10, 30))),
        ),
        r"^<stdin>:10:30: model: error: expected aggregate"
        r" with element integer type\n"
        r'<stdin>:10:30: model: info: found enumeration type "__BUILTINS__::Boolean"$',
    )
    assert_type_error(
        relation(
            Variable("X", type_=rty.AnyInteger(), location=Location((10, 20))),
            Variable("Y", type_=rty.Sequence("A", rty.BOOLEAN), location=Location((10, 30))),
        ),
        r"^<stdin>:10:30: model: error: expected aggregate"
        r" with element integer type\n"
        r'<stdin>:10:30: model: info: found sequence type "A"'
        r' with element enumeration type "__BUILTINS__::Boolean"$',
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


def test_relation_contains() -> None:
    assert Variable("X") in Less(Variable("X"), Number(42))


def test_relation_variables() -> None:
    assert Less(Variable("X"), Variable("Y")).variables() == [Variable("X"), Variable("Y")]


@pytest.mark.parametrize("relation", [Less, LessEqual, GreaterEqual, Greater])
def test_relation_z3expr_error(relation: Callable[[Expr, Expr], Expr]) -> None:
    with pytest.raises(Z3TypeError):
        relation(String("X"), Number(1)).z3expr()


@pytest.mark.parametrize("relation", [Less, LessEqual, Equal, GreaterEqual, Greater, NotEqual])
def test_math_relation_ada_expr(relation: Callable[[Expr, Expr], Expr]) -> None:
    result = relation(Variable("X"), Variable("Y")).ada_expr()
    expected = getattr(ada, relation.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("relation", [In, NotIn])
def test_composite_relation_ada_expr(relation: Callable[[Expr, Expr], Expr]) -> None:
    result = relation(Variable("X"), Variable("Y")).ada_expr()
    expected = getattr(ada, relation.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


def test_less_neg() -> None:
    assert -Less(Variable("X"), Number(1)) == GreaterEqual(Variable("X"), Number(1))


def test_less_simplified() -> None:
    assert Less(Number(0), Number(1)).simplified() == TRUE
    assert Less(Number(1), Number(1)).simplified() == FALSE
    assert Less(Number(2), Number(1)).simplified() == FALSE


def test_less_z3expr() -> None:
    assert Less(Number(1), Number(100)).z3expr() == (z3.IntVal(1) < z3.IntVal(100))


def test_less_equal_neg() -> None:
    assert -LessEqual(Variable("X"), Number(1)) == Greater(Variable("X"), Number(1))


def test_less_equal_simplified() -> None:
    assert LessEqual(Number(0), Number(1)).simplified() == TRUE
    assert LessEqual(Number(1), Number(1)).simplified() == TRUE
    assert LessEqual(Number(2), Number(1)).simplified() == FALSE


def test_less_equal_z3expr() -> None:
    assert LessEqual(Number(1), Number(100)).z3expr() == (z3.IntVal(1) <= z3.IntVal(100))


def test_equal_neg() -> None:
    assert -Equal(Variable("X"), Number(1)) == NotEqual(Variable("X"), Number(1))


def test_equal_simplified() -> None:
    assert Equal(Number(0), Number(1)).simplified() == FALSE
    assert Equal(Number(1), Number(1)).simplified() == TRUE
    assert Equal(Number(2), Number(1)).simplified() == FALSE


def test_equal_z3expr() -> None:
    assert Equal(Number(100), Number(100)).z3expr() == (z3.IntVal(100) == z3.IntVal(100))


def test_greater_equal_neg() -> None:
    assert -GreaterEqual(Variable("X"), Number(1)) == Less(Variable("X"), Number(1))


def test_greater_equal_simplified() -> None:
    assert GreaterEqual(Number(0), Number(1)).simplified() == FALSE
    assert GreaterEqual(Number(1), Number(1)).simplified() == TRUE
    assert GreaterEqual(Number(2), Number(1)).simplified() == TRUE


def test_greater_equal_z3expr() -> None:
    assert GreaterEqual(Number(100), Number(1)).z3expr() == (z3.IntVal(100) >= z3.IntVal(1))


def test_greater_neg() -> None:
    assert -Greater(Variable("X"), Number(1)) == LessEqual(Variable("X"), Number(1))


def test_greater_simplified() -> None:
    assert Greater(Number(0), Number(1)).simplified() == FALSE
    assert Greater(Number(1), Number(1)).simplified() == FALSE
    assert Greater(Number(2), Number(1)).simplified() == TRUE


def test_greater_z3expr() -> None:
    assert Greater(Number(100), Number(1)).z3expr() == (z3.IntVal(100) > z3.IntVal(1))


def test_not_equal_neg() -> None:
    assert -NotEqual(Variable("X"), Number(1)) == Equal(Variable("X"), Number(1))


def test_not_equal_simplified() -> None:
    assert NotEqual(Number(0), Number(1)).simplified() == TRUE
    assert NotEqual(Number(1), Number(1)).simplified() == FALSE
    assert NotEqual(Number(2), Number(1)).simplified() == TRUE


def test_not_equal_z3expr() -> None:
    assert NotEqual(Number(100), Number(1)).z3expr() == (z3.IntVal(100) != z3.IntVal(1))


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


def test_value_range_type() -> None:
    assert_type(
        ValueRange(Number(1), Number(42)),
        rty.Any(),
    )


def test_value_range_type_error() -> None:
    assert_type_error(
        ValueRange(
            Variable("X", type_=rty.BOOLEAN, location=Location((10, 30))),
            Variable("Y", type_=rty.Sequence("A", rty.AnyInteger()), location=Location((10, 40))),
            location=Location((10, 20)),
        ),
        r"^<stdin>:10:30: model: error: expected integer type\n"
        r'<stdin>:10:30: model: info: found enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:40: model: error: expected integer type\n"
        r'<stdin>:10:40: model: info: found sequence type "A" with element integer type$',
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


def test_value_range_ada_expr() -> None:
    assert ValueRange(Variable("X"), Variable("Y")).ada_expr() == ada.ValueRange(
        ada.Variable("X"), ada.Variable("Y")
    )


@pytest.mark.parametrize("expr", [ForAllIn, ForSomeIn])
def test_quantified_expression_type(expr: Callable[[str, Expr, Expr], Expr]) -> None:
    assert_type(
        expr(
            "X",
            Variable("Y", type_=rty.Sequence("A", rty.Integer("B"))),
            Variable("Z", type_=rty.BOOLEAN),
        ),
        rty.BOOLEAN,
    )


@pytest.mark.parametrize("expr", [ForAllIn, ForSomeIn])
@pytest.mark.parametrize(
    "iterable,predicate,match",
    [
        (
            Variable("Y", type_=rty.BOOLEAN, location=Location((10, 30))),
            Variable("Z", type_=rty.Sequence("A", rty.AnyInteger()), location=Location((10, 40))),
            r"^<stdin>:10:30: model: error: expected composite type\n"
            r'<stdin>:10:30: model: info: found enumeration type "__BUILTINS__::Boolean"\n'
            r'<stdin>:10:40: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r'<stdin>:10:40: model: info: found sequence type "A" with element integer type$',
        ),
        (
            Variable("Y", type_=rty.BOOLEAN, location=Location((10, 30))),
            Equal(Variable("X"), Number(1)),
            r"^<stdin>:10:30: model: error: expected composite type\n"
            r'<stdin>:10:30: model: info: found enumeration type "__BUILTINS__::Boolean"$',
        ),
        (
            Variable("Y", type_=rty.Sequence("A", rty.BOOLEAN)),
            Equal(Variable("X"), Number(1, location=Location((10, 30)))),
            r'^<stdin>:10:30: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
            r"<stdin>:10:30: model: info: found type universal integer \(1\)$",
        ),
    ],
)
def test_quantified_expression_type_error(
    expr: Callable[[str, Expr, Expr, Location], Expr], iterable: Expr, predicate: Expr, match: str
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
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
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
            "A", Variable("List"), Add(Variable("X"), Add(Variable("Y"), Variable("Z")))
        ).variables(),
        [Variable("List"), Variable("X"), Variable("Y"), Variable("Z")],
    )


def test_quantified_expression_str() -> None:
    assert str(ForAllOf("X", Variable("Y"), Variable("Z"))) == "(for all X of Y =>\n    Z)"
    assert str(ForAllIn("X", Variable("Y"), Variable("Z"))) == "(for all X in Y =>\n    Z)"
    assert str(ForSomeIn("X", Variable("Y"), Variable("Z"))) == "(for some X in Y =>\n    Z)"


@pytest.mark.parametrize("expression", [ForAllOf, ForAllIn, ForSomeIn])
def test_quantified_expression_ada_expr(expression: Callable[[str, Expr, Expr], Expr]) -> None:
    result = expression("X", Variable("Y"), Variable("Z")).ada_expr()
    result = expected = getattr(ada, expression.__name__)("X", ada.Variable("Y"), ada.Variable("Z"))
    assert result == expected


def test_for_all_in_variables() -> None:
    result = ForAllIn(
        "Q", Variable("List"), Equal(Selected(Variable("Q"), "Fld"), Variable("X"))
    ).variables()
    expected = [Variable("List"), Variable("X")]
    assert result == expected


def test_for_some_in_variables() -> None:
    result = ForSomeIn(
        "Q", Variable("List"), Equal(Selected(Variable("Q"), "Fld"), Variable("X"))
    ).variables()
    expected = [Variable("List"), Variable("X")]
    assert result == expected


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
        [Variable("Y"), TRUE, Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
        ).variables(),
        [Variable("Y"), TRUE, Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(42)))
        ).variables(),
        [Variable("Y"), TRUE, Variable("X")],
    )
    assert_equal(
        Or(
            Greater(Variable("Y"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
        ).variables(),
        [Variable("Y"), TRUE, Variable("X")],
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
            Greater(Variable("X"), Number(42)), And(TRUE, Less(Variable("X"), Number(1)))
        ).variables(),
        [Variable("X"), TRUE],
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
    with pytest.raises(AssertionError):
        Selected(Variable("X"), "F").substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        Call("Sub").substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        ForAllOf("X", Variable("Y"), Variable("Z")).substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        Conversion("X", Variable("Y")).substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        Comprehension("X", Variable("Y"), Variable("Z"), Variable("A")).substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        MessageAggregate("X", {"A": Number(5)}).substituted(lambda x: x, {})
    with pytest.raises(AssertionError):
        Binding(Variable("X"), {"X": Number(5)}).substituted(lambda x: x, {})


def test_length_z3variables() -> None:
    assert Length("Z").variables() == [Variable("Z")]


def test_last_z3variables() -> None:
    assert Last("Z").variables() == [Variable("Z")]


def test_first_z3variables() -> None:
    assert First("Z").variables() == [Variable("Z")]


def test_size_z3variables() -> None:
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


def test_number_z3expr() -> None:
    assert Number(42).z3expr() == z3.IntVal(42)


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


def test_string_variables() -> None:
    assert String("X").variables() == []


def test_string_simplified() -> None:
    assert String("Test").simplified() == String("Test")


def test_string_elements() -> None:
    assert String("Test").elements == [Number(84), Number(101), Number(115), Number(116)]


def test_string_str() -> None:
    assert str(String("X Y")) == '"X Y"'
    assert str(Equal(String("S"), Variable("X"))) == '"S" = X'


def test_string_ada_expr() -> None:
    assert String("X Y").ada_expr() == ada.String("X Y")


def test_selected_type() -> None:
    assert_type(
        Selected(Variable("X", type_=rty.Message("M", {("F",)}, {ID("F"): rty.Integer("A")})), "F"),
        rty.Integer("A"),
    )


@pytest.mark.parametrize(
    "expr,match",
    [
        (
            Selected(Variable("X", type_=rty.BOOLEAN, location=Location((10, 20))), "Y"),
            r"^<stdin>:10:20: model: error: expected message type\n"
            r'<stdin>:10:20: model: info: found enumeration type "__BUILTINS__::Boolean"$',
        ),
        (
            Selected(
                Variable(
                    "X",
                    type_=rty.Message("M", {("F",)}, {ID("F"): rty.Integer("A")}),
                ),
                "Y",
                location=Location((10, 20)),
            ),
            r'^<stdin>:10:20: model: error: invalid field "Y" for message type "M"$',
        ),
        (
            Selected(
                Variable(
                    "X",
                    type_=rty.Message(
                        "M",
                        {("F1",), ("F2",)},
                        {ID("F1"): rty.Integer("A"), ID("F2"): rty.Integer("A")},
                    ),
                ),
                "F",
                location=Location((10, 20)),
            ),
            r'^<stdin>:10:20: model: error: invalid field "F" for message type "M"\n'
            r"<stdin>:10:20: model: info: similar field names: F1, F2$",
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
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        Selected(Variable("P_X"), "Y"),
    )
    assert_equal(
        Selected(Variable("X"), "Y").substituted(
            lambda x: Variable("Z") if isinstance(x, Selected) else x
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


def test_selected_z3expr() -> None:
    assert Selected(Variable("X"), "Y").z3expr() == z3.Int("X.Y")
    assert Selected(Variable("X"), "Y", negative=True).z3expr() == -z3.Int("X.Y")


def test_in_variables() -> None:
    result = In(Variable("A"), Variable("B")).variables()
    expected = [Variable("A"), Variable("B")]
    assert result == expected


def test_call_type() -> None:
    assert_type(
        Call(
            "X",
            [Variable("Y", type_=rty.Integer("A"))],
            type_=rty.BOOLEAN,
            argument_types=[rty.Integer("A")],
        ),
        rty.BOOLEAN,
    )


def test_call_type_error() -> None:
    assert_type_error(
        Call("X", [Variable("Y", location=Location((10, 30)))], location=Location((10, 20))),
        r'^<stdin>:10:30: model: error: undefined variable "Y"\n'
        r'<stdin>:10:20: model: error: undefined function "X"$',
    )
    assert_type_error(
        Call(
            "X",
            [
                Variable("Y", type_=rty.AnyInteger(), location=Location((10, 30))),
                Variable("Z", type_=rty.BOOLEAN, location=Location((10, 40))),
            ],
            type_=rty.BOOLEAN,
            argument_types=[
                rty.BOOLEAN,
                rty.AnyInteger(),
            ],
        ),
        r'^<stdin>:10:30: model: error: expected enumeration type "__BUILTINS__::Boolean"\n'
        r"<stdin>:10:30: model: info: found integer type\n"
        r"<stdin>:10:40: model: error: expected integer type\n"
        r'<stdin>:10:40: model: info: found enumeration type "__BUILTINS__::Boolean"$',
    )


def test_call_variables() -> None:
    result = Call("Sub", [Variable("A"), Variable("B")]).variables()
    expected = [Variable("Sub"), Variable("A"), Variable("B")]
    assert result == expected


def test_call_findall() -> None:
    assert Call("X", [Variable("Y"), Variable("Z")]).findall(lambda x: isinstance(x, Variable)) == [
        Variable("Y"),
        Variable("Z"),
    ]


def test_call_str() -> None:
    assert str(Call("Test", [])) == "Test"
    assert str(-Call("Test", [])) == "(-Test)"


def test_conversion_type() -> None:
    assert_type(
        Conversion(
            "X",
            Selected(Variable("Y", type_=rty.Message("Y", {("Z",)}, {ID("Z"): rty.OPAQUE})), "Z"),
            type_=rty.Message("X"),
            argument_types=[rty.Message("Y")],
        ),
        rty.Message("X"),
    )


def test_conversion_type_error() -> None:
    assert_type_error(
        Conversion(
            "X",
            Selected(Variable("Y", location=Location((10, 30))), "Z"),
            location=Location((10, 20)),
        ),
        r'^<stdin>:10:30: model: error: undefined variable "Y"\n'
        r'<stdin>:10:20: model: error: invalid conversion to "X"\n'
        r'<stdin>:10:20: model: error: undefined type "X"$',
    )


def test_conversion_substituted() -> None:
    assert_equal(
        Conversion("X", Variable("Y")).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        Conversion("X", Variable("P_Y")),
    )
    assert_equal(
        Conversion("X", Variable("Y")).substituted(
            lambda x: Variable("Z") if isinstance(x, Conversion) else x
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


def test_conversion_ada_expr() -> None:
    assert Conversion("X", Variable("Y")).ada_expr() == ada.Conversion("X", ada.Variable("Y"))


def test_comprehension_type() -> None:
    assert_type(
        Comprehension(
            "X",
            Variable("Y", type_=rty.Sequence("A", rty.Integer("B"))),
            Add(Variable("X"), Variable("Z", type_=rty.Integer("B"))),
            TRUE,
        ),
        rty.Aggregate(rty.Integer("B")),
    )
    assert_type(
        Comprehension(
            "X",
            Selected(
                Variable(
                    "Y",
                    type_=rty.Message(
                        "M", {("F",)}, {ID("F"): rty.Sequence("A", rty.Integer("B"))}
                    ),
                ),
                "F",
            ),
            Variable("X"),
            Equal(Variable("X"), Number(1)),
        ),
        rty.Aggregate(rty.Integer("B")),
    )


def test_comprehension_type_error() -> None:
    assert_type_error(
        Comprehension(
            "X",
            Variable("Y", location=Location((10, 20))),
            Variable("X", location=Location((10, 30))),
            TRUE,
        ),
        r'^<stdin>:10:20: model: error: undefined variable "Y"$',
    )


def test_comprehension_substituted() -> None:
    assert_equal(
        Comprehension("X", Variable("Y"), Variable("Z"), TRUE).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        Comprehension("X", Variable("P_Y"), Variable("P_Z"), Variable("P_True")),
    )
    assert_equal(
        Comprehension("X", Variable("Y"), Variable("Z"), TRUE).substituted(
            lambda x: Variable("Z") if isinstance(x, Comprehension) else x
        ),
        Variable("Z"),
    )


def test_comprehension_substituted_location() -> None:
    expr = Comprehension(
        "X", Variable("Y"), Variable("Z"), TRUE, location=Location((1, 2))
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
    "field_values,type_",
    [
        (
            {"X": Variable("A", type_=rty.Integer("A")), "Y": Variable("B", type_=rty.BOOLEAN)},
            rty.Message(
                "M",
                {
                    ("X",),
                    ("X", "Y"),
                    ("X", "Y", "Z"),
                },
                {
                    ID("X"): rty.Integer("A"),
                    ID("Y"): rty.BOOLEAN,
                },
            ),
        ),
        (
            {"X": Variable("A", type_=rty.Message("I"))},
            rty.Message(
                "M",
                {
                    ("X",),
                },
                {},
                {
                    ID("X"): rty.OPAQUE,
                },
                [
                    rty.Refinement(ID("X"), rty.Message("I"), "P"),
                    rty.Refinement(ID("X"), rty.Message("J"), "P"),
                ],
            ),
        ),
    ],
)
def test_message_aggregate_type(field_values: Mapping[StrID, Expr], type_: rty.Type) -> None:
    assert_type(
        MessageAggregate(
            "M",
            field_values,
            type_=type_,
        ),
        type_,
    )


@pytest.mark.parametrize(
    "field_values,type_,match",
    [
        (
            {
                "X": Variable("A", location=Location((10, 30))),
                "Y": Variable("B", location=Location((10, 40))),
            },
            rty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {
                    ID("X"): rty.Integer("A"),
                    ID("Y"): rty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:30: model: error: undefined variable "A"\n'
            r'<stdin>:10:40: model: error: undefined variable "B"$',
        ),
        (
            {
                "X": Variable("A", type_=rty.Integer("A")),
                "Y": Variable("B", type_=rty.BOOLEAN),
                ID("Z", location=Location((10, 50))): Variable("Z", type_=rty.Integer("A")),
            },
            rty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {
                    ID("X"): rty.Integer("A"),
                    ID("Y"): rty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:50: model: error: invalid field "Z" for message type "M"$',
        ),
        (
            {
                ID("Y", location=Location((10, 30))): Variable("B", type_=rty.BOOLEAN),
                "X": Variable("A", type_=rty.Integer("A")),
            },
            rty.Message(
                "M",
                {
                    ("X", "Y"),
                },
                {
                    ID("X"): rty.Integer("A"),
                    ID("Y"): rty.BOOLEAN,
                },
            ),
            r'^<stdin>:10:30: model: error: invalid position for field "Y" of message type "M"$',
        ),
        (
            {
                "X": Variable("A", type_=rty.Integer("A")),
            },
            rty.Message(
                "M",
                {
                    ("X", "Y"),
                    ("X", "Y", "Z"),
                },
                {
                    ID("X"): rty.Integer("A"),
                    ID("Y"): rty.BOOLEAN,
                    ID("Z"): rty.Integer("A"),
                },
            ),
            r'^<stdin>:10:20: model: error: missing fields for message type "M"\n'
            r"<stdin>:10:20: model: info: possible next fields: Y$",
        ),
        (
            {
                "X": Variable("A", location=Location((10, 40))),
                "Y": Variable("B", location=Location((10, 30))),
            },
            rty.Message(
                "M",
                {
                    ("X", "Y", "Z"),
                },
                {
                    ID("X"): rty.Integer("A"),
                    ID("Y"): rty.BOOLEAN,
                    ID("Z"): rty.Integer("A"),
                },
            ),
            r'^<stdin>:10:40: model: error: undefined variable "A"\n'
            r'<stdin>:10:30: model: error: undefined variable "B"\n'
            r'<stdin>:10:20: model: error: missing fields for message type "M"\n'
            r"<stdin>:10:20: model: info: possible next fields: Z$",
        ),
        (
            {
                "X": Variable("A", location=Location((10, 40))),
                "Y": Variable("B", location=Location((10, 30))),
            },
            rty.Undefined(),
            r'^<stdin>:10:40: model: error: undefined variable "A"\n'
            r'<stdin>:10:30: model: error: undefined variable "B"\n'
            r'<stdin>:10:20: model: error: undefined message "X"$',
        ),
    ],
)
def test_message_aggregate_type_error(
    field_values: Mapping[StrID, Expr], type_: rty.Type, match: str
) -> None:
    assert_type_error(
        MessageAggregate("X", field_values, type_=type_, location=Location((10, 20))),
        match,
    )


def test_message_aggregate_substituted() -> None:
    assert_equal(
        MessageAggregate("X", {"Y": Variable("A"), "Z": Variable("B")}).substituted(
            lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x
        ),
        MessageAggregate("X", {"Y": Variable("P_A"), "Z": Variable("P_B")}),
    )
    assert_equal(
        MessageAggregate("X", {"Y": Variable("A"), "Z": Variable("B")}).substituted(
            lambda x: Variable("Z") if isinstance(x, MessageAggregate) else x
        ),
        Variable("Z"),
    )


def test_message_aggregate_substituted_location() -> None:
    expr = MessageAggregate(
        "X", {"Y": Variable("A"), "Z": Variable("B")}, location=Location((1, 2))
    ).substituted(lambda x: x)
    assert expr.location


def test_message_aggregate_variables() -> None:
    result = MessageAggregate(
        "Aggr", {"X": Variable("A"), "Y": Variable("B"), "Baz": Variable("C")}
    ).variables()
    expected = [Variable("A"), Variable("B"), Variable("C")]
    assert result == expected


def test_binding_type() -> None:
    assert_type(
        Binding(
            And(Variable("A", type_=rty.BOOLEAN), Variable("Bound")),
            {
                "Bound": Less(
                    Variable("B", type_=rty.Integer("A")), Variable("C", type_=rty.Integer("A"))
                )
            },
        ),
        rty.BOOLEAN,
    )


def test_binding_type_error() -> None:
    assert_type_error(
        Binding(
            And(
                Variable("A", location=Location((10, 20))),
                Variable("Bound", location=Location((10, 30))),
            ),
            {"Bound": Variable("B", location=Location((10, 40)))},
        ),
        r'^<stdin>:10:40: model: error: undefined variable "B"\n'
        r'<stdin>:10:20: model: error: undefined variable "A"$',
    )


def test_binding_findall() -> None:
    assert Binding(
        And(Variable("A"), Variable("Bound")), {"Bound": Less(Variable("B"), Variable("C"))}
    ).findall(lambda x: isinstance(x, Variable)) == [
        Variable("A"),
        Variable("Bound"),
        Variable("B"),
        Variable("C"),
    ]


def test_binding_substituted() -> None:
    assert_equal(
        Binding(
            And(Variable("A"), Variable("Bound")), {"Bound": Less(Variable("B"), Variable("C"))}
        ).substituted(lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x),
        Binding(
            And(Variable("P_A"), Variable("P_Bound")),
            {"Bound": Less(Variable("P_B"), Variable("P_C"))},
        ),
    )
    assert_equal(
        Binding(
            And(Variable("A"), Variable("Bound")), {"Bound": Less(Variable("B"), Variable("C"))}
        ).substituted(lambda x: Variable("Z") if isinstance(x, Binding) else x),
        Variable("Z"),
    )


def test_binding_substituted_location() -> None:
    expr = Binding(
        And(Variable("A"), Variable("Bound")),
        {"Bound": Less(Variable("B"), Variable("C"))},
        location=Location((1, 2)),
    ).substituted(lambda x: x)
    assert expr.location


def test_binding_variables() -> None:
    result = Binding(
        And(Variable("A"), Variable("Bound")), {"Bound": Less(Variable("B"), Variable("C"))}
    ).variables()
    expected = [Variable("A"), Variable("B"), Variable("C")]
    assert result == expected


def test_binding_simplified_aggregate() -> None:
    binding = Binding(
        MessageAggregate("M1", {"Data": Variable("B1")}),
        {"B1": MessageAggregate("M2", {"Data": Variable("B2")})},
    )
    expected = MessageAggregate("M1", {"Data": MessageAggregate("M2", {"Data": Variable("B2")})})
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_forall_predicate() -> None:
    binding = Binding(
        ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Bar": Variable("Baz")},
    )
    expected = ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Baz")))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_length() -> None:
    binding = Binding(Length(Variable("A")), {"A": Variable("Baz")})
    expected = Length(Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_forall_iterable() -> None:
    binding = Binding(
        ForAllIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Y": Variable("Baz")},
    )
    expected = ForAllIn("X", Variable("Baz"), Equal(Variable("X"), Variable("Bar")))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_forsome_predicate() -> None:
    binding = Binding(
        ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Bar": Variable("Baz")},
    )
    expected = ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Variable("Baz")))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_forsome_iterable() -> None:
    binding = Binding(
        ForSomeIn("X", Variable("Y"), Equal(Variable("X"), Variable("Bar"))),
        {"Y": Variable("Baz")},
    )
    expected = ForSomeIn("X", Variable("Baz"), Equal(Variable("X"), Variable("Bar")))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_contains_left() -> None:
    binding = Binding(
        In(Variable("X"), Variable("Y")),
        {"X": Variable("Baz")},
    )
    expected = In(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_contains_right() -> None:
    binding = Binding(
        In(Variable("X"), Variable("Y")),
        {"Y": Variable("Baz")},
    )
    expected = In(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_not_contains_left() -> None:
    binding = Binding(
        NotIn(Variable("X"), Variable("Y")),
        {"X": Variable("Baz")},
    )
    expected = NotIn(Variable("Baz"), Variable("Y"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_not_contains_right() -> None:
    binding = Binding(
        NotIn(Variable("X"), Variable("Y")),
        {"Y": Variable("Baz")},
    )
    expected = NotIn(Variable("X"), Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_subprogram() -> None:
    binding = Binding(
        Call("Sub", [Variable("A"), Variable("B"), Variable("C")]),
        {"B": Variable("Baz")},
    )
    expected = Call("Sub", [Variable("A"), Variable("Baz"), Variable("C")])
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_field() -> None:
    binding = Binding(Selected(Variable("A"), "fld"), {"A": Variable("Baz")})
    expected = Selected(Variable("Baz"), "fld")
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_list_comprehension() -> None:
    binding = Binding(
        Comprehension(
            "E",
            Variable("List"),
            Selected(Variable("E"), "Bar"),
            Equal(Selected(Variable("E"), "Tag"), Variable("Foo")),
        ),
        {"List": Variable("Foo")},
    )
    expected = Comprehension(
        "E",
        Variable("Foo"),
        Selected(Variable("E"), "Bar"),
        Equal(Selected(Variable("E"), "Tag"), Variable("Foo")),
    )
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_conversion() -> None:
    binding = Binding(Conversion("Type", Variable("A")), {"A": Variable("Baz")})
    expected = Conversion("Type", Variable("Baz"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_conversion_name_unchanged() -> None:
    binding = Binding(Conversion("Type", Variable("A")), {"Type": Variable("Baz")})
    expected = Conversion("Type", Variable("A"))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_opaque() -> None:
    binding = Binding(Opaque(Call("Sub", [Variable("Bound")])), {"Bound": Variable("Foo")})
    expected = Opaque(Call("Sub", [Variable("Foo")]))
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_multiple_bindings() -> None:
    binding = Binding(
        Selected(Variable("A"), "fld"), {"A": Binding(Variable("B"), {"B": Variable("Baz")})}
    )
    expected = Selected(Variable("Baz"), "fld")
    result = binding.simplified()
    assert result == expected


def test_binding_simplified_multiple_variables() -> None:
    binding = Binding(Call("Sub", [Variable("A"), Variable("A")]), {"A": Variable("Baz")})
    expected = Call("Sub", [Variable("Baz"), Variable("Baz")])
    result = binding.simplified()
    assert result == expected


def test_indexed_neg() -> None:
    assert Indexed(Variable("X"), Variable("Y")) == -Indexed(
        Variable("X"), Variable("Y"), negative=True
    )
    assert Indexed(Variable("X"), Variable("Y")) != Indexed(
        Variable("X"), Variable("Y"), negative=True
    )


def test_proof_invalid_logic() -> None:
    expr = Less(Mod(Variable("X"), Variable("Y")), Number(100))
    p = Proof(expr, logic="QF_IDL")
    assert p.result == ProofResult.UNKNOWN
    assert p.error == [
        (
            "Benchmark is not in QF_IDL (integer difference logic).",
            None,
        )
    ]
