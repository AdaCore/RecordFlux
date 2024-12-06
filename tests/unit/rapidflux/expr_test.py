from __future__ import annotations

import pickle
from collections.abc import Callable, Mapping
from pathlib import Path
from tempfile import NamedTemporaryFile

import pytest

from rflx import ty
from rflx.rapidflux import ID, Location, RecordFluxError
from rflx.rapidflux.expr import Expr, Literal, Neg, Number, Variable
from tests.utils import check_regex

INT_TY = ty.Integer("I", ty.Bounds(10, 100))


def substitution(mapping: Mapping[Expr, Expr]) -> Callable[[Expr], Expr]:
    return lambda expression: mapping.get(expression, expression)


def assert_type(expr: Expr, type_: ty.Type | tuple[ty.Type, ...]) -> None:
    expr.check_type(type_).propagate()
    assert expr.type_ == type_ if isinstance(type_, ty.Type) else expr.type_ in type_


def assert_type_error(expr: Expr, regex: str) -> None:
    check_regex(regex)
    with pytest.raises(RecordFluxError, match=regex):
        expr.check_type(ty.Any()).propagate()


def assert_type_instance(expr: Expr, type_: type[ty.Type] | tuple[type[ty.Type], ...]) -> None:
    expr.check_type_instance(type_).propagate()


def test_variable_str() -> None:
    assert str(Variable("X")) == "X"


def test_variable_repr() -> None:
    assert (
        repr(Variable("X"))
        == 'Variable(ID("X", Location((1, 1), "<unknown>", (1, 1))), Undefined())'
    )
    assert (
        repr(Variable(ID("X", location=Location((1, 2)))))
        == 'Variable(ID("X", Location((1, 2), None, (1, 2))), Undefined())'
    )


def test_variable_eq() -> None:
    assert Variable(ID("X", location=Location((1, 2)))) == Variable("X")


def test_variable_ne() -> None:
    assert Variable("X") != Variable("Y")


def test_variable_neg() -> None:
    assert -Variable("X") == Neg(Variable("X"))


def test_variable_hashable() -> None:
    assert {Variable("X"), Variable("Y")}


def test_variable_location() -> None:
    assert Variable(ID("X", location=Location((1, 2)))).location == Location((1, 2))


def test_variable_invalid_identifier() -> None:
    with pytest.raises(BaseException, match=r"^invalid identifier$"):
        Variable("Foo (Bar)")


def test_variable_type() -> None:
    v = Variable("X")
    v.type_ = ty.BOOLEAN
    assert_type(v, (ty.BOOLEAN,))
    assert_type_instance(v, (ty.Enumeration,))
    assert_type(
        Variable("X", type_=INT_TY),
        INT_TY,
    )
    assert_type_instance(
        Variable("X", type_=INT_TY),
        ty.Integer,
    )


def test_variable_type_error() -> None:
    assert_type_error(
        Variable(ID("X", location=Location((10, 20)))),
        r'^<stdin>:10:20: error: undefined variable "X"$',
    )


def test_variable_identifier() -> None:
    assert Variable("X").identifier == ID("X")


def test_variable_name() -> None:
    assert Variable("X").name == "X"


def test_variable_variables() -> None:
    assert Variable("X").variables() == [Variable("X")]


def test_variable_findall() -> None:
    assert Variable("X").findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_variable_substituted() -> None:
    assert Variable("X").substituted(
        lambda x: Number(42) if x == Variable("X") else x,
    ) == Number(42)
    assert Variable("X").substituted(substitution({Variable("X"): Number(42)})) == Number(42)


def test_variable_simplified() -> None:
    assert Variable("X").simplified() == Variable("X")


def test_literal_str() -> None:
    assert str(Literal("X")) == "X"


def test_literal_repr() -> None:
    assert (
        repr(Literal("X")) == 'Literal(ID("X", Location((1, 1), "<unknown>", (1, 1))), Undefined())'
    )
    assert (
        repr(Literal(ID("X", location=Location((1, 2)))))
        == 'Literal(ID("X", Location((1, 2), None, (1, 2))), Undefined())'
    )


def test_literal_eq() -> None:
    assert Literal(ID("X", location=Location((1, 2)))) == Literal("X")


def test_literal_ne() -> None:
    assert Literal("X") != Literal("Y")


def test_literal_neg() -> None:
    with pytest.raises(BaseException, match=r"^literal cannot be negated$"):
        -Literal("X")


def test_literal_hashable() -> None:
    assert {Literal("X"), Literal("Y")}


def test_literal_location() -> None:
    assert Literal(ID("X", location=Location((1, 2)))).location == Location((1, 2))


def test_literal_invalid_identifier() -> None:
    with pytest.raises(BaseException, match=r"^invalid identifier$"):
        Literal("Foo (Bar)")


def test_literal_type() -> None:
    assert_type(
        Literal("X", type_=ty.BOOLEAN),
        (ty.BOOLEAN,),
    )
    assert_type_instance(
        Literal("X", type_=ty.BOOLEAN),
        (ty.Enumeration,),
    )
    assert_type(
        Literal("X", type_=INT_TY),
        INT_TY,
    )
    assert_type_instance(
        Literal("X", type_=INT_TY),
        ty.Integer,
    )


def test_literal_type_error() -> None:
    assert_type_error(
        Literal(ID("X", location=Location((10, 20)))),
        r'^<stdin>:10:20: error: undefined literal "X"$',
    )


def test_literal_identifier() -> None:
    assert Literal("X").identifier == ID("X")


def test_literal_name() -> None:
    assert Literal("X").name == "X"


def test_literal_variables() -> None:
    assert Literal("X").variables() == []


def test_literal_findall() -> None:
    assert Literal("X").findall(lambda x: isinstance(x, Literal)) == [Literal("X")]


def test_literal_substituted() -> None:
    assert Literal("X").substituted(
        lambda x: Number(42) if x == Literal("X") else x,
    ) == Number(42)
    assert Literal("X").substituted(substitution({Literal("X"): Number(42)})) == Number(42)


def test_literal_simplified() -> None:
    assert Literal("X").simplified() == Literal("X")


def test_number_str() -> None:
    assert str(Number(42)) == "42"


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


def test_number_repr() -> None:
    assert repr(Number(42)) == 'Number(42, Location((1, 1), "<unknown>", (1, 1)))'
    assert (
        repr(Number(42, location=Location((1, 2)))) == "Number(42, Location((1, 2), None, (1, 2)))"
    )


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
    assert not Variable("X") > Number(2)
    assert not Number(2) > Variable("X")


def test_number_ge() -> None:
    assert not Number(1) >= Number(2)
    assert Number(2) >= Number(2)
    assert Number(3) >= Number(2)
    assert not Variable("X") >= Number(2)
    assert not Number(2) >= Variable("X")


def test_number_neg() -> None:
    assert -Number(42) == Number(-42)


def test_number_hashable() -> None:
    assert {Number(1), Number(2)}


def test_number_type() -> None:
    assert_type(
        Number(1),
        ty.UniversalInteger(ty.Bounds(1, 1)),
    )


def test_number_value() -> None:
    assert Number(42).value == 42


def test_number_base() -> None:
    assert Number(42).base == 0
    assert Number(42, 8).base == 8


def test_number_findall() -> None:
    assert Number(42).findall(lambda x: isinstance(x, Number)) == [Number(42)]


def test_number_simplified() -> None:
    assert Number(42).simplified() == Number(42)


def test_number_simplified_location() -> None:
    assert Number(42, location=Location((1, 1))).simplified().location == Location((1, 1))


def test_neg_str() -> None:
    assert str(Neg(Variable("X"))) == "-X"
    assert str(Neg(Neg(Variable("X")))) == "-(-X)"


def test_neg_repr() -> None:
    assert repr(Neg(Variable("X"))) == (
        'Neg(Variable(ID("X", Location((1, 1), "<unknown>", (1, 1))), Undefined()),'
        ' Location((1, 1), "<unknown>", (1, 1)))'
    )
    assert repr(Neg(Variable(ID("X", location=Location((3, 4)))), location=Location((1, 2)))) == (
        'Neg(Variable(ID("X", Location((3, 4), None, (3, 4))), Undefined()),'
        " Location((1, 2), None, (1, 2)))"
    )


def test_neg_eq() -> None:
    assert Neg(Variable(ID("X")), location=Location((1, 2))) == Neg(Variable("X"))


def test_neg_ne() -> None:
    assert Neg(Variable("X")) != Neg(Variable("Y"))


def test_neg_neg() -> None:
    assert -Neg(Variable("X")) == Variable("X")
    assert -Variable("X") != Variable("X")
    y = Variable("Y")
    assert y == y  # noqa: PLR0124
    assert y != -y


def test_neg_location() -> None:
    assert Neg(Variable("X"), location=Location((1, 2))).location == Location((1, 2))


def test_neg_type() -> None:
    assert_type(
        Neg(Variable("X", type_=INT_TY)),
        INT_TY,
    )


def test_neg_type_error() -> None:
    assert_type_error(
        Neg(Variable(ID("X", location=Location((10, 20))), type_=ty.BOOLEAN)),
        r"^<stdin>:10:20: error: expected integer type\n"
        r'<stdin>:10:20: error: found enumeration type "__BUILTINS__::Boolean"$',
    )


def test_neg_expr() -> None:
    assert Neg(Variable("X")).expr == Variable("X")


def test_neg_variables() -> None:
    assert Neg(Variable("X")).variables() == [Variable("X")]


def test_neg_findall() -> None:
    assert Neg(Variable("X")).findall(lambda x: isinstance(x, Variable)) == [Variable("X")]


def test_neg_substituted() -> None:
    assert Neg(Variable("X")).substituted(
        lambda x: Variable(f"P_{x}") if isinstance(x, Variable) else x,
    ) == Neg(Variable("P_X"))
    assert Neg(Variable("X")).substituted(
        lambda x: Variable("Y") if x == Neg(Variable("X")) else x,
    ) == Variable("Y")


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
        # Argument cannot be simplified
        (Neg(Variable("X")), Neg(Variable("X"))),
    ],
)
def test_neg_simplified(expr: Expr, expected: Expr) -> None:
    assert expr.simplified() == expected


@pytest.mark.parametrize(
    ("left", "right"),
    [
        (Variable("X"), -Variable("X")),
        (Variable("X"), Literal("X")),
        (Variable("X"), Number(42)),
    ],
)
def test_ne(left: Expr, right: Expr) -> None:
    assert left != right
    assert right != left


@pytest.mark.parametrize(
    "obj",
    [
        Variable("X"),
        Literal("Y"),
        Number(42),
        Neg(Number(42)),
    ],
)
def test_pickle(obj: object) -> None:
    with NamedTemporaryFile("w+b") as f:
        pickle.dump(obj, f)
        f.flush()

        with Path(f.name).open("rb") as read_file:
            loaded = pickle.load(read_file)  # noqa: S301
            assert loaded == obj
