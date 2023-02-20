import pytest

from rflx import expression as expr, tac, typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID, id_generator
from rflx.model import statement as stmt


def test_variable_assignment_to_tac() -> None:
    assert stmt.VariableAssignment(
        "X", expr.Add(expr.Variable("Y", type_=rty.Integer("I")), expr.Number(1))
    ).to_tac(id_generator()) == [
        tac.Assign("X", tac.Add(tac.IntVar("Y"), tac.IntVal(1))),
    ]
    assert stmt.VariableAssignment(
        "X",
        expr.Add(
            expr.Variable("Y", type_=rty.Integer("I")),
            expr.Sub(expr.Variable("Z", type_=rty.Integer("I")), expr.Number(1)),
        ),
    ).to_tac(id_generator()) == [
        tac.Assign("T_1", tac.Sub(tac.IntVar("Z"), tac.IntVal(1))),
        tac.Assign("X", tac.Add(tac.IntVar("Y"), tac.IntVar("T_1"))),
    ]


def test_message_field_assignment_to_tac() -> None:
    assert stmt.MessageFieldAssignment("X", "Y", expr.Number(1)).to_tac(id_generator()) == [
        tac.FieldAssign("X", "Y", tac.IntVal(1)),
    ]
    assert stmt.MessageFieldAssignment(
        "X", "Y", expr.Add(expr.Variable("Z", type_=rty.Integer("I")), expr.Number(1))
    ).to_tac(id_generator()) == [
        tac.Assign("T_0", tac.Add(tac.IntVar("Z"), tac.IntVal(1))),
        tac.FieldAssign("X", "Y", tac.IntVar("T_0")),
    ]


def test_append_to_tac() -> None:
    assert stmt.Append("X", expr.Number(1)).to_tac(id_generator()) == [
        tac.Append("X", tac.IntVal(1)),
    ]
    assert stmt.Append(
        "X", expr.Add(expr.Variable("Z", type_=rty.Integer("I")), expr.Number(1))
    ).to_tac(id_generator()) == [
        tac.Assign("T_0", tac.Add(tac.IntVar("Z"), tac.IntVal(1))),
        tac.Append("X", tac.IntVar("T_0")),
    ]


def test_extend_to_tac() -> None:
    assert stmt.Extend("X", expr.Number(1)).to_tac(id_generator()) == [
        tac.Extend("X", tac.IntVal(1)),
    ]
    assert stmt.Extend(
        "X", expr.Add(expr.Variable("Z", type_=rty.Integer("I")), expr.Number(1))
    ).to_tac(id_generator()) == [
        tac.Assign("T_0", tac.Add(tac.IntVar("Z"), tac.IntVal(1))),
        tac.Extend("X", tac.IntVar("T_0")),
    ]


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


def test_reset_to_tac() -> None:
    assert stmt.Reset("X").to_tac(id_generator()) == [
        tac.Reset("X"),
    ]
    assert stmt.Reset("X", {ID("Y"): expr.Number(1)}).to_tac(id_generator()) == [
        tac.Reset("X", {ID("Y"): tac.IntVal(1)}),
    ]
    assert stmt.Reset(
        "X", {ID("Y"): expr.Add(expr.Variable("Z", type_=rty.Integer("I")), expr.Number(1))}
    ).to_tac(id_generator()) == [
        tac.Assign("T_0", tac.Add(tac.IntVar("Z"), tac.IntVal(1))),
        tac.Reset("X", {ID("Y"): tac.IntVar("T_0")}),
    ]


def test_read_to_tac() -> None:
    assert stmt.Read("X", expr.Variable("M")).to_tac(id_generator()) == [
        tac.Read("X", tac.ObjVar("M")),
    ]
    assert stmt.Read("X", expr.Call("Y", type_=rty.Message("M"))).to_tac(id_generator()) == [
        tac.Assign("T_0", tac.ObjCall("Y")),
        tac.Read("X", tac.ObjVar("T_0")),
    ]


def test_write_to_tac() -> None:
    assert stmt.Write("X", expr.Variable("M")).to_tac(id_generator()) == [
        tac.Write("X", tac.ObjVar("M")),
    ]
    assert stmt.Write("X", expr.Call("Y", type_=rty.Message("M"))).to_tac(id_generator()) == [
        tac.Assign("T_0", tac.ObjCall("Y")),
        tac.Write("X", tac.ObjVar("T_0")),
    ]
