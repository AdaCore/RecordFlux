import pytest

from rflx import expression as expr, typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID
from rflx.model import statement as stmt


def test_reset_check_type() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=rty.Message("M", {("F",)}, {ID("F"): rty.Integer("I")}))
        return expression

    t = rty.Message("T", parameter_types={ID("Y"): rty.Integer("I")})

    reset = stmt.Reset("X", {ID("Y"): expr.Selected(expr.Variable("M"), "F")})
    reset.check_type(t, typify_variable).propagate()

    assert reset.type_ == t
    assert reset.associations[ID("Y")].type_ == rty.Integer("I")


def test_reset_check_type_error_undefined_argument_type() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        return expression

    reset = stmt.Reset(
        "X", {ID("Y"): expr.Selected(expr.Variable("M", location=Location((1, 2))), "F")}
    )
    with pytest.raises(
        RecordFluxError, match=r'^<stdin>:1:2: model: error: undefined variable "M"$'
    ):
        reset.check_type(
            rty.Message("T", parameter_types={ID("Y"): rty.Integer("I")}),
            typify_variable,
        ).propagate()


def test_reset_check_type_error_invalid_arguments() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=rty.Message("M", {("F",)}, {ID("F"): rty.Integer("I")}))
        return expression

    reset = stmt.Reset(
        "X",
        {ID("Z"): expr.Selected(expr.Variable("M"), "F", location=Location((1, 2)))},
        location=Location((1, 1)),
    )
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r'<stdin>:1:2: model: error: unexpected argument "Z"\n'
            r'<stdin>:1:1: model: error: missing argument "Y"'
            r"$"
        ),
    ):
        reset.check_type(
            rty.Message("T", parameter_types={ID("Y"): rty.Integer("I")}),
            typify_variable,
        ).propagate()


def test_reset_check_type_error_unexpected_arguments() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=rty.Message("M", {("F",)}, {ID("F"): rty.Integer("I")}))
        return expression

    reset = stmt.Reset(
        "X", {ID("Z"): expr.Selected(expr.Variable("M"), "F", location=Location((1, 2)))}
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:1:2: model: error: unexpected argument "Z"$',
    ):
        reset.check_type(
            rty.Sequence("T", rty.Integer("I")),
            typify_variable,
        ).propagate()
