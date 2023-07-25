import pytest

from rflx import expression as expr, tac, typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID, id_generator
from rflx.model import statement as stmt

INT_TY = rty.Integer("I", rty.Bounds(10, 100))
MSG_TY = rty.Message("M")
SEQ_TY = rty.Sequence("S", rty.Message("M"))


def test_variable_assignment_to_tac() -> None:
    assert stmt.VariableAssignment(
        "X", expr.Add(expr.Variable("Y", type_=INT_TY), expr.Number(1)), INT_TY
    ).to_tac(id_generator()) == [
        tac.Assign("X", tac.Add(tac.IntVar("Y", INT_TY), tac.IntVal(1)), INT_TY),
    ]
    assert stmt.VariableAssignment(
        "X",
        expr.Add(
            expr.Variable("Y", type_=INT_TY),
            expr.Sub(expr.Variable("Z", type_=INT_TY), expr.Number(1)),
        ),
        INT_TY,
    ).to_tac(id_generator()) == [
        tac.VarDecl("T_0", rty.BASE_INTEGER),
        tac.Assign("T_0", tac.Sub(tac.IntVar("Z", INT_TY), tac.IntVal(1)), rty.BASE_INTEGER),
        tac.Assign(
            "X", tac.Add(tac.IntVar("Y", INT_TY), tac.IntVar("T_0", rty.BASE_INTEGER)), INT_TY
        ),
    ]


def test_message_field_assignment_to_tac() -> None:
    assert stmt.MessageFieldAssignment("X", "Y", expr.Number(1), MSG_TY).to_tac(id_generator()) == [
        tac.FieldAssign("X", "Y", tac.IntVal(1), MSG_TY),
    ]
    assert stmt.MessageFieldAssignment(
        "X",
        "Y",
        expr.Add(
            expr.Variable("Y", type_=INT_TY), expr.Variable("Z", type_=INT_TY), expr.Number(1)
        ),
        MSG_TY,
    ).to_tac(id_generator()) == [
        tac.VarDecl("T_0", rty.BASE_INTEGER),
        tac.Assign("T_0", tac.Add(tac.IntVar("Z", INT_TY), tac.IntVal(1)), rty.BASE_INTEGER),
        tac.FieldAssign(
            "X", "Y", tac.Add(tac.IntVar("Y", INT_TY), tac.IntVar("T_0", rty.BASE_INTEGER)), MSG_TY
        ),
    ]


def test_append_to_tac() -> None:
    assert stmt.Append("X", expr.Number(1), SEQ_TY).to_tac(id_generator()) == [
        tac.Append("X", tac.IntVal(1), SEQ_TY),
    ]
    assert stmt.Append(
        "X",
        expr.Add(
            expr.Variable("Y", type_=INT_TY), expr.Variable("Z", type_=INT_TY), expr.Number(1)
        ),
        SEQ_TY,
    ).to_tac(id_generator()) == [
        tac.VarDecl("T_0", rty.BASE_INTEGER),
        tac.Assign("T_0", tac.Add(tac.IntVar("Z", INT_TY), tac.IntVal(1)), rty.BASE_INTEGER),
        tac.Append(
            "X", tac.Add(tac.IntVar("Y", INT_TY), tac.IntVar("T_0", rty.BASE_INTEGER)), SEQ_TY
        ),
    ]


def test_extend_to_tac() -> None:
    assert stmt.Extend("X", expr.Variable("Y", type_=SEQ_TY), SEQ_TY).to_tac(id_generator()) == [
        tac.Extend("X", tac.ObjVar("Y", SEQ_TY), SEQ_TY),
    ]


def test_reset_check_type() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=rty.Message("M", {("F",)}, {ID("F"): INT_TY}))
        return expression

    t = rty.Message("T", parameter_types={ID("Y"): INT_TY})

    reset = stmt.Reset("X", {ID("Y"): expr.Selected(expr.Variable("M"), "F")})
    reset.check_type(t, typify_variable).propagate()

    assert reset.type_ == t
    assert reset.associations[ID("Y")].type_ == INT_TY


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
            rty.Message("T", parameter_types={ID("Y"): INT_TY}),
            typify_variable,
        ).propagate()


def test_reset_check_type_error_invalid_arguments() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=rty.Message("M", {("F",)}, {ID("F"): INT_TY}))
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
            rty.Message("T", parameter_types={ID("Y"): INT_TY}),
            typify_variable,
        ).propagate()


def test_reset_check_type_error_unexpected_arguments() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=rty.Message("M", {("F",)}, {ID("F"): INT_TY}))
        return expression

    reset = stmt.Reset(
        "X", {ID("Z"): expr.Selected(expr.Variable("M"), "F", location=Location((1, 2)))}
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:1:2: model: error: unexpected argument "Z"$',
    ):
        reset.check_type(
            rty.Sequence("T", INT_TY),
            typify_variable,
        ).propagate()


def test_reset_to_tac() -> None:
    assert stmt.Reset("X", {}, MSG_TY).to_tac(id_generator()) == [
        tac.Reset("X", {}, MSG_TY),
    ]
    assert stmt.Reset("X", {ID("Y"): expr.Number(1)}, MSG_TY).to_tac(id_generator()) == [
        tac.Reset("X", {ID("Y"): tac.IntVal(1)}, MSG_TY),
    ]
    assert stmt.Reset(
        "X", {ID("Y"): expr.Add(expr.Variable("Z", type_=INT_TY), expr.Number(1))}, MSG_TY
    ).to_tac(id_generator()) == [
        tac.Reset("X", {ID("Y"): tac.Add(tac.IntVar("Z", INT_TY), tac.IntVal(1))}, MSG_TY),
    ]


def test_read_to_tac() -> None:
    assert stmt.Read("X", expr.Variable("M", type_=MSG_TY)).to_tac(id_generator()) == [
        tac.Read("X", tac.ObjVar("M", MSG_TY)),
    ]
    assert stmt.Read("X", expr.Call("Y", type_=rty.Message("M"))).to_tac(id_generator()) == [
        tac.VarDecl("T_0", MSG_TY),
        tac.Assign("T_0", tac.ObjCall("Y", [], [], MSG_TY), MSG_TY),
        tac.Read("X", tac.ObjVar("T_0", MSG_TY)),
    ]


def test_write_to_tac() -> None:
    assert stmt.Write("X", expr.Variable("M", type_=MSG_TY)).to_tac(id_generator()) == [
        tac.Write("X", tac.ObjVar("M", MSG_TY)),
    ]
    assert stmt.Write("X", expr.Call("Y", type_=rty.Message("M"))).to_tac(id_generator()) == [
        tac.VarDecl("T_0", MSG_TY),
        tac.Assign("T_0", tac.ObjCall("Y", [], [], MSG_TY), MSG_TY),
        tac.Write("X", tac.ObjVar("T_0", MSG_TY)),
    ]
