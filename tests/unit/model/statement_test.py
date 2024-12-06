import pytest

from rflx import expr, ir, ty
from rflx.identifier import ID, id_generator
from rflx.model import statement as stmt
from rflx.rapidflux import Location, RecordFluxError

INT_TY = ty.Integer("I", ty.Bounds(10, 100))
MSG_TY = ty.Message("M")
SEQ_TY = ty.Sequence("S", ty.Message("M"))


def test_variable_assignment_to_ir() -> None:
    assert stmt.VariableAssignment(
        "X",
        expr.Add(expr.Variable("Y", type_=INT_TY), expr.Number(1)),
        INT_TY,
    ).to_ir(id_generator()) == [
        ir.Assign("X", ir.Add(ir.IntVar("Y", INT_TY), ir.IntVal(1)), ty.BASE_INTEGER),
    ]
    assert stmt.VariableAssignment(
        "X",
        expr.Add(
            expr.Variable("Y", type_=INT_TY),
            expr.Sub(expr.Variable("Z", type_=INT_TY), expr.Number(1)),
        ),
        INT_TY,
    ).to_ir(id_generator()) == [
        ir.VarDecl("T_0", ty.BASE_INTEGER),
        ir.Assign("T_0", ir.Sub(ir.IntVar("Z", INT_TY), ir.IntVal(1)), ty.BASE_INTEGER),
        ir.Assign(
            "X",
            ir.Add(ir.IntVar("Y", INT_TY), ir.IntVar("T_0", ty.BASE_INTEGER)),
            ty.BASE_INTEGER,
        ),
    ]


def test_message_field_assignment_to_ir() -> None:
    assert stmt.MessageFieldAssignment("X", "Y", expr.Number(1), MSG_TY).to_ir(id_generator()) == [
        ir.FieldAssign("X", "Y", ir.IntVal(1), MSG_TY),
    ]
    assert stmt.MessageFieldAssignment(
        "X",
        "Y",
        expr.Add(
            expr.Variable("Y", type_=INT_TY),
            expr.Variable("Z", type_=INT_TY),
            expr.Number(1),
        ),
        MSG_TY,
    ).to_ir(id_generator()) == [
        ir.VarDecl("T_0", ty.BASE_INTEGER),
        ir.Assign("T_0", ir.Add(ir.IntVar("Z", INT_TY), ir.IntVal(1)), ty.BASE_INTEGER),
        ir.FieldAssign(
            "X",
            "Y",
            ir.Add(ir.IntVar("Y", INT_TY), ir.IntVar("T_0", ty.BASE_INTEGER)),
            MSG_TY,
        ),
    ]


def test_append_to_ir() -> None:
    assert stmt.Append("X", expr.Number(1), SEQ_TY).to_ir(id_generator()) == [
        ir.Append("X", ir.IntVal(1), SEQ_TY),
    ]
    assert stmt.Append(
        "X",
        expr.Add(
            expr.Variable("Y", type_=INT_TY),
            expr.Variable("Z", type_=INT_TY),
            expr.Number(1),
        ),
        SEQ_TY,
    ).to_ir(id_generator()) == [
        ir.VarDecl("T_0", ty.BASE_INTEGER),
        ir.Assign("T_0", ir.Add(ir.IntVar("Z", INT_TY), ir.IntVal(1)), ty.BASE_INTEGER),
        ir.Append("X", ir.Add(ir.IntVar("Y", INT_TY), ir.IntVar("T_0", ty.BASE_INTEGER)), SEQ_TY),
    ]


def test_extend_to_ir() -> None:
    assert stmt.Extend("X", expr.Variable("Y", type_=SEQ_TY), SEQ_TY).to_ir(id_generator()) == [
        ir.Extend("X", ir.ObjVar("Y", SEQ_TY), SEQ_TY),
    ]


def test_reset_check_type() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=ty.Message("M", {("F",)}, {ID("F"): INT_TY}))
        return expression

    t = ty.Message("T", parameter_types={ID("Y"): INT_TY})

    reset = stmt.Reset("X", {ID("Y"): expr.Selected(expr.Variable("M"), "F")})
    reset.check_type(t, typify_variable).propagate()

    assert reset.type_ == t
    assert reset.associations[ID("Y")].type_ == INT_TY


def test_reset_check_type_error_undefined_argument_type() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        return expression

    reset = stmt.Reset(
        "X",
        {ID("Y"): expr.Selected(expr.Variable(ID("M", location=Location((1, 2)))), "F")},
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:1:2: error: undefined variable "M"$',
    ):
        reset.check_type(
            ty.Message("T", parameter_types={ID("Y"): INT_TY}),
            typify_variable,
        ).propagate()


def test_reset_check_type_error_invalid_arguments() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=ty.Message("M", {("F",)}, {ID("F"): INT_TY}))
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
            r'<stdin>:1:2: error: unexpected argument "Z"\n'
            r'<stdin>:1:1: error: missing argument "Y"'
            r"$"
        ),
    ):
        reset.check_type(
            ty.Message("T", parameter_types={ID("Y"): INT_TY}),
            typify_variable,
        ).propagate()


def test_reset_check_type_error_unexpected_arguments() -> None:
    def typify_variable(expression: expr.Expr) -> expr.Expr:
        if isinstance(expression, expr.Variable) and expression.identifier == ID("M"):
            return expr.Variable("M", type_=ty.Message("M", {("F",)}, {ID("F"): INT_TY}))
        return expression

    reset = stmt.Reset(
        "X",
        {ID("Z"): expr.Selected(expr.Variable("M"), "F", location=Location((1, 2)))},
    )
    with pytest.raises(
        RecordFluxError,
        match=r'^<stdin>:1:2: error: unexpected argument "Z"$',
    ):
        reset.check_type(
            ty.Sequence("T", INT_TY),
            typify_variable,
        ).propagate()


def test_reset_to_ir() -> None:
    assert stmt.Reset("X", {}, MSG_TY).to_ir(id_generator()) == [
        ir.Reset("X", {}, MSG_TY),
    ]
    assert stmt.Reset("X", {ID("Y"): expr.Number(1)}, MSG_TY).to_ir(id_generator()) == [
        ir.Reset("X", {ID("Y"): ir.IntVal(1)}, MSG_TY),
    ]
    assert stmt.Reset(
        "X",
        {ID("Y"): expr.Add(expr.Variable("Z", type_=INT_TY), expr.Number(1))},
        MSG_TY,
    ).to_ir(id_generator()) == [
        ir.Reset("X", {ID("Y"): ir.Add(ir.IntVar("Z", INT_TY), ir.IntVal(1))}, MSG_TY),
    ]


def test_read_to_ir() -> None:
    assert stmt.Read("X", expr.Variable("M", type_=MSG_TY)).to_ir(id_generator()) == [
        ir.Read("X", ir.ObjVar("M", MSG_TY)),
    ]
    assert stmt.Read("X", expr.Call("Y", type_=ty.Message("M"))).to_ir(id_generator()) == [
        ir.VarDecl("T_0", MSG_TY),
        ir.Assign("T_0", ir.ObjCall("Y", [], [], MSG_TY), MSG_TY),
        ir.Read("X", ir.ObjVar("T_0", MSG_TY)),
    ]


def test_write_to_ir() -> None:
    assert stmt.Write("X", expr.Variable("M", type_=MSG_TY)).to_ir(id_generator()) == [
        ir.Write("X", ir.ObjVar("M", MSG_TY)),
    ]
    assert stmt.Write("X", expr.Call("Y", type_=ty.Message("M"))).to_ir(id_generator()) == [
        ir.VarDecl("T_0", MSG_TY),
        ir.Assign("T_0", ir.ObjCall("Y", [], [], MSG_TY), MSG_TY),
        ir.Write("X", ir.ObjVar("T_0", MSG_TY)),
    ]
