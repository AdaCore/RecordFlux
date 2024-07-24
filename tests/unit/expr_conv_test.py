from typing import Callable

import pytest

from rflx import ada, expr_conv, ir, typing_ as rty
from rflx.expr import (
    FALSE,
    TRUE,
    Add,
    Aggregate,
    And,
    AndThen,
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
    Last,
    Length,
    Less,
    LessEqual,
    Literal,
    MessageAggregate,
    Mod,
    Mul,
    Neg,
    Not,
    NotEqual,
    NotIn,
    Number,
    Opaque,
    Or,
    OrElse,
    Pow,
    Present,
    QualifiedExpr,
    Rem,
    Selected,
    Size,
    String,
    Sub,
    Valid,
    ValidChecksum,
    ValueRange,
    Variable,
)
from rflx.identifier import ID, id_generator
from rflx.rapidflux import ty

INT_TY = rty.Integer("I", ty.Bounds(10, 100))
ENUM_TY = rty.Enumeration("E", [ID("E1"), ID("E2")])
MSG_TY = rty.Message("M")
SEQ_TY = rty.Sequence("S", rty.Message("M"))


@pytest.mark.parametrize("expression", [And, AndThen, Or, OrElse])
def test_to_ada_bool_expr(expression: Callable[[Expr, Expr], Expr]) -> None:
    result = expr_conv.to_ada(expression(Variable("X"), Variable("Y")))
    expected = getattr(ada, expression.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("expression", [Add, Mul, Sub, Div, Pow, Mod, Rem])
def test_to_ada_math_expr(expression: Callable[[Expr, Expr], Expr]) -> None:
    result = expr_conv.to_ada(expression(Variable("X"), Variable("Y")))
    expected = getattr(ada, expression.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("relation", [Less, LessEqual, Equal, GreaterEqual, Greater, NotEqual])
def test_to_ada_math_relation(relation: Callable[[Expr, Expr], Expr]) -> None:
    result = expr_conv.to_ada(relation(Variable("X"), Variable("Y")))
    expected = getattr(ada, relation.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


@pytest.mark.parametrize("relation", [In, NotIn])
def test_to_ada_composite_relation(relation: Callable[[Expr, Expr], Expr]) -> None:
    result = expr_conv.to_ada(relation(Variable("X"), Variable("Y")))
    expected = getattr(ada, relation.__name__)(ada.Variable("X"), ada.Variable("Y"))
    assert result == expected


def test_to_ada_if_expr() -> None:
    assert expr_conv.to_ada(IfExpr([(Variable("X"), Variable("Y"))], Variable("Z"))) == ada.IfExpr(
        [(ada.Variable("X"), ada.Variable("Y"))],
        ada.Variable("Z"),
    )


def test_to_ada_value_range() -> None:
    assert expr_conv.to_ada(ValueRange(Variable("X"), Variable("Y"))) == ada.ValueRange(
        ada.Variable("X"),
        ada.Variable("Y"),
    )


@pytest.mark.parametrize("expression", [ForAllOf, ForAllIn, ForSomeIn])
def test_to_ada_quantified_expression(expression: Callable[[str, Expr, Expr], Expr]) -> None:
    result = expr_conv.to_ada(expression("X", Variable("Y"), Variable("Z")))
    expected = getattr(ada, expression.__name__)("X", ada.Variable("Y"), ada.Variable("Z"))
    assert result == expected


def test_to_ada_string() -> None:
    assert expr_conv.to_ada(String("X Y")) == ada.String("X Y")


def test_to_ada_conversion() -> None:
    assert expr_conv.to_ada(Conversion("X", Variable("Y"))) == ada.Conversion(
        "X",
        ada.Variable("Y"),
    )


def test_to_ada_qualified_expr() -> None:
    assert expr_conv.to_ada(QualifiedExpr("X", Variable("Y"))) == ada.QualifiedExpr(
        "X",
        ada.Variable("Y"),
    )


def test_to_ir_true() -> None:
    assert expr_conv.to_ir(TRUE, id_generator()) == ir.ComplexExpr([], ir.BoolVal(value=True))


def test_to_ir_false() -> None:
    assert expr_conv.to_ir(FALSE, id_generator()) == ir.ComplexExpr([], ir.BoolVal(value=False))


def test_to_ir_not() -> None:
    assert expr_conv.to_ir(Not(TRUE), id_generator()) == ir.ComplexBoolExpr(
        [],
        ir.Not(ir.BoolVal(value=True)),
    )
    assert expr_conv.to_ir(
        Not(Variable("X", type_=rty.BOOLEAN)),
        id_generator(),
    ) == ir.ComplexBoolExpr(
        [],
        ir.Not(ir.BoolVar("X")),
    )
    assert expr_conv.to_ir(
        Not(Less(Variable("X", type_=INT_TY), Variable("Y", type_=INT_TY))),
        id_generator(),
    ) == ir.ComplexBoolExpr(
        [
            ir.VarDecl("T_0", rty.BOOLEAN),
            ir.Assign("T_0", ir.Less(ir.IntVar("X", INT_TY), ir.IntVar("Y", INT_TY)), rty.BOOLEAN),
        ],
        ir.Not(ir.BoolVar("T_0")),
    )


@pytest.mark.parametrize(("op", "ir_op"), [(And, ir.And), (Or, ir.Or)])
def test_to_ir_and_or(  # type: ignore[misc]
    op: Callable[..., Expr],
    ir_op: Callable[[ir.BoolExpr, ir.BoolExpr], ir.BoolExpr],
) -> None:
    assert expr_conv.to_ir(op(), id_generator()) == ir.ComplexBoolExpr(
        [],
        ir.BoolVal(value=True),
    )
    assert expr_conv.to_ir(
        op(Variable("X", type_=rty.BOOLEAN)),
        id_generator(),
    ) == ir.ComplexBoolExpr(
        [],
        ir.BoolVar("X"),
    )
    assert expr_conv.to_ir(
        op(
            Variable("X", type_=rty.BOOLEAN),
            Variable("Y", type_=rty.BOOLEAN),
            Variable("Z", type_=rty.BOOLEAN),
        ),
        id_generator(),
    ) == ir.ComplexBoolExpr(
        [
            ir.VarDecl("T_0", rty.BOOLEAN),
            ir.Assign("T_0", ir_op(ir.BoolVar("Y"), ir.BoolVar("Z")), rty.BOOLEAN),
        ],
        ir_op(ir.BoolVar("X"), ir.BoolVar("T_0")),
    )
    assert expr_conv.to_ir(
        op(
            op(
                Variable("X", type_=rty.BOOLEAN),
                Variable("Y", type_=rty.BOOLEAN),
            ),
            Variable("Z", type_=rty.BOOLEAN),
        ),
        id_generator(),
    ) == ir.ComplexBoolExpr(
        [
            ir.VarDecl("T_0", rty.BOOLEAN),
            ir.Assign("T_0", ir_op(ir.BoolVar("X"), ir.BoolVar("Y")), rty.BOOLEAN),
        ],
        ir_op(ir.BoolVar("T_0"), ir.BoolVar("Z")),
    )


def test_to_ir_neg() -> None:
    assert expr_conv.to_ir(Neg(Number(42)), id_generator()) == ir.ComplexIntExpr(
        [],
        ir.Neg(ir.IntVal(42)),
    )
    assert expr_conv.to_ir(Neg(Variable("X", type_=INT_TY)), id_generator()) == ir.ComplexIntExpr(
        [],
        ir.Neg(ir.IntVar("X", INT_TY)),
    )
    assert expr_conv.to_ir(
        Neg(Add(Variable("X", type_=INT_TY), Variable("Y", type_=INT_TY))),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [
            ir.VarDecl("T_0", rty.BASE_INTEGER),
            ir.Assign(
                "T_0",
                ir.Add(ir.IntVar("X", INT_TY), ir.IntVar("Y", INT_TY)),
                rty.BASE_INTEGER,
            ),
        ],
        ir.Neg(ir.IntVar("T_0", rty.BASE_INTEGER)),
    )


@pytest.mark.parametrize(("op", "ir_op"), [(Add, ir.Add), (Mul, ir.Mul)])
def test_to_ir_add_mul(  # type: ignore[misc]
    op: Callable[..., Expr],
    ir_op: Callable[[ir.IntExpr, ir.IntExpr], ir.IntExpr],
) -> None:
    assert expr_conv.to_ir(op(), id_generator()) == ir.ComplexIntExpr(
        [],
        ir.IntVal(0),
    )
    assert expr_conv.to_ir(op(Variable("X", type_=INT_TY)), id_generator()) == ir.ComplexIntExpr(
        [],
        ir.IntVar("X", INT_TY),
    )
    assert expr_conv.to_ir(
        op(
            Variable("X", type_=INT_TY),
            Variable("Y", type_=INT_TY),
            Variable("Z", type_=INT_TY),
        ),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [
            ir.VarDecl("T_0", rty.BASE_INTEGER),
            ir.Assign(
                "T_0",
                ir_op(ir.IntVar("Y", INT_TY), ir.IntVar("Z", INT_TY)),
                rty.BASE_INTEGER,
            ),
        ],
        ir_op(ir.IntVar("X", INT_TY), ir.IntVar("T_0", rty.BASE_INTEGER)),
    )
    assert expr_conv.to_ir(
        op(
            op(
                Variable("X", type_=INT_TY),
                Variable("Y", type_=INT_TY),
            ),
            Variable("Z", type_=INT_TY),
        ),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [
            ir.VarDecl("T_0", rty.BASE_INTEGER),
            ir.Assign(
                "T_0",
                ir_op(ir.IntVar("X", INT_TY), ir.IntVar("Y", INT_TY)),
                rty.BASE_INTEGER,
            ),
        ],
        ir_op(ir.IntVar("T_0", rty.BASE_INTEGER), ir.IntVar("Z", INT_TY)),
    )


@pytest.mark.parametrize(
    ("op", "ir_op"),
    [(Sub, ir.Sub), (Div, ir.Div), (Pow, ir.Pow), (Mod, ir.Mod)],
)
def test_to_ir_sub_div_pow_mod(  # type: ignore[misc]
    op: Callable[..., Expr],
    ir_op: Callable[[ir.IntExpr, ir.IntExpr], ir.IntExpr],
) -> None:
    assert expr_conv.to_ir(
        op(
            Variable("X", type_=INT_TY),
            Variable("Y", type_=INT_TY),
        ),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [],
        ir_op(ir.IntVar("X", INT_TY), ir.IntVar("Y", INT_TY)),
    )
    assert expr_conv.to_ir(
        op(
            op(
                Variable("X", type_=INT_TY),
                Variable("Y", type_=INT_TY),
            ),
            Variable("Z", type_=INT_TY),
        ),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [
            ir.VarDecl("T_0", rty.BASE_INTEGER),
            ir.Assign(
                "T_0",
                ir_op(ir.IntVar("X", INT_TY), ir.IntVar("Y", INT_TY)),
                rty.BASE_INTEGER,
            ),
        ],
        ir_op(ir.IntVar("T_0", rty.BASE_INTEGER), ir.IntVar("Z", INT_TY)),
    )


def test_to_ir_literal() -> None:
    assert expr_conv.to_ir(Literal("X", type_=ENUM_TY), id_generator()) == ir.ComplexExpr(
        [],
        ir.EnumLit("X", ENUM_TY),
    )


def test_to_ir_variable() -> None:
    assert expr_conv.to_ir(Variable("X", type_=rty.BOOLEAN), id_generator()) == ir.ComplexBoolExpr(
        [],
        ir.BoolVar("X"),
    )
    assert expr_conv.to_ir(Variable("X", type_=INT_TY), id_generator()) == ir.ComplexIntExpr(
        [],
        ir.IntVar("X", INT_TY),
    )
    assert expr_conv.to_ir(Neg(Variable("X", type_=INT_TY)), id_generator()) == ir.ComplexIntExpr(
        [],
        ir.Neg(ir.IntVar("X", INT_TY)),
    )
    assert expr_conv.to_ir(Variable("X", type_=MSG_TY), id_generator()) == ir.ComplexExpr(
        [],
        ir.ObjVar("X", MSG_TY),
    )
    assert expr_conv.to_ir(Variable("X", type_=SEQ_TY), id_generator()) == ir.ComplexExpr(
        [],
        ir.ObjVar("X", SEQ_TY),
    )


@pytest.mark.parametrize(
    ("attribute", "ir_attribute"),
    [
        (Size(Variable("X", type_=MSG_TY)), ir.Size("X", MSG_TY)),
        (Length(Variable("X", type_=MSG_TY)), ir.Length("X", MSG_TY)),
        (First(Variable("X", type_=MSG_TY)), ir.First("X", MSG_TY)),
        (Last(Variable("X", type_=MSG_TY)), ir.Last("X", MSG_TY)),
        (Head(Variable("X", type_=SEQ_TY), type_=MSG_TY), ir.Head("X", SEQ_TY)),
        (Opaque(Variable("X", type_=MSG_TY)), ir.Opaque("X", MSG_TY)),
    ],
)
def test_to_ir_attribute_int(attribute: Expr, ir_attribute: ir.Expr) -> None:
    assert expr_conv.to_ir(attribute, id_generator()) == ir.ComplexExpr([], ir_attribute)


@pytest.mark.parametrize(
    ("attribute", "ir_attribute"),
    [
        (ValidChecksum(Variable("X", type_=MSG_TY)), ir.ValidChecksum("X", MSG_TY)),
        (Valid(Variable("X", type_=MSG_TY)), ir.Valid("X", MSG_TY)),
        (Present(Variable("X", type_=MSG_TY)), ir.Present("X", MSG_TY)),
        (
            HasData(Variable("X", type_=rty.Channel(readable=True, writable=False))),
            ir.HasData("X", MSG_TY),
        ),
    ],
)
def test_to_ir_attribute_bool(attribute: Expr, ir_attribute: ir.Expr) -> None:
    assert expr_conv.to_ir(attribute, id_generator()) == ir.ComplexExpr([], ir_attribute)


def test_to_ir_aggregate() -> None:
    assert expr_conv.to_ir(
        Aggregate(Add(First(Variable("X", type_=INT_TY)), Number(1)), Number(2)),
        id_generator(),
    ) == ir.ComplexExpr(
        [
            ir.VarDecl("T_0", rty.BASE_INTEGER),
            ir.Assign("T_0", ir.First("X", INT_TY), rty.BASE_INTEGER),
            ir.VarDecl("T_1", rty.BASE_INTEGER),
            ir.Assign(
                "T_1",
                ir.Add(ir.IntVar("T_0", rty.BASE_INTEGER), ir.IntVal(1)),
                rty.BASE_INTEGER,
            ),
        ],
        ir.Agg([ir.IntVar("T_1", rty.BASE_INTEGER), ir.IntVal(2)]),
    )


@pytest.mark.parametrize(
    ("relation", "ir_relation"),
    [
        (Less, ir.Less),
        (LessEqual, ir.LessEqual),
        (Equal, ir.Equal),
        (GreaterEqual, ir.GreaterEqual),
        (Greater, ir.Greater),
        (NotEqual, ir.NotEqual),
    ],
)
def test_to_ir_relation(  # type: ignore[misc]
    relation: Callable[..., Expr],
    ir_relation: Callable[[ir.IntExpr, ir.IntExpr], ir.BoolExpr],
) -> None:
    assert expr_conv.to_ir(
        relation(Variable("X", type_=INT_TY), Number(10)),
        id_generator(),
    ) == ir.ComplexBoolExpr([], ir_relation(ir.IntVar("X", INT_TY), ir.IntVal(10)))


def test_to_ir_if_expr() -> None:
    assert expr_conv.to_ir(
        IfExpr(
            [(Variable("X", type_=rty.BOOLEAN), Variable("Y", type_=rty.BOOLEAN))],
            Variable("Z", type_=rty.BOOLEAN),
        ),
        id_generator(),
    ) == ir.ComplexBoolExpr(
        [],
        ir.BoolIfExpr(
            ir.BoolVar("X"),
            ir.ComplexBoolExpr([], ir.BoolVar("Y")),
            ir.ComplexBoolExpr([], ir.BoolVar("Z")),
            rty.BOOLEAN,
        ),
    )
    assert expr_conv.to_ir(
        IfExpr(
            [(Variable("X", type_=rty.BOOLEAN), Variable("Y", type_=INT_TY))],
            Variable("Z", type_=INT_TY),
        ),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [],
        ir.IntIfExpr(
            ir.BoolVar("X"),
            ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
            ir.ComplexIntExpr([], ir.IntVar("Z", INT_TY)),
            INT_TY,
        ),
    )
    assert expr_conv.to_ir(
        IfExpr(
            [
                (
                    And(Variable("X", type_=rty.BOOLEAN), TRUE),
                    Add(Variable("Y", type_=INT_TY), Number(1)),
                ),
            ],
            Sub(Variable("Z", type_=INT_TY), Number(2)),
        ),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [
            ir.VarDecl("T_0", rty.BOOLEAN),
            ir.Assign("T_0", ir.And(ir.BoolVar("X"), ir.BoolVal(value=True)), rty.BOOLEAN),
        ],
        ir.IntIfExpr(
            ir.BoolVar("T_0"),
            ir.ComplexIntExpr([], ir.Add(ir.IntVar("Y", INT_TY), ir.IntVal(1))),
            ir.ComplexIntExpr([], ir.Sub(ir.IntVar("Z", INT_TY), ir.IntVal(2))),
            INT_TY,
        ),
    )


def test_to_ir_number() -> None:
    assert expr_conv.to_ir(Number(42), id_generator()) == ir.ComplexIntExpr([], ir.IntVal(42))


def test_to_ir_selected() -> None:
    assert expr_conv.to_ir(
        Selected(Variable("X", type_=rty.Message("M")), "Y", type_=rty.BOOLEAN),
        id_generator(),
    ) == ir.ComplexExpr([], ir.BoolFieldAccess("X", "Y", MSG_TY))
    assert expr_conv.to_ir(
        Selected(Variable("X", type_=rty.Message("M")), "Y", type_=INT_TY),
        id_generator(),
    ) == ir.ComplexExpr([], ir.IntFieldAccess("X", "Y", MSG_TY))
    assert expr_conv.to_ir(
        Neg(Selected(Variable("X", type_=rty.Message("M")), "Y", type_=INT_TY)),
        id_generator(),
    ) == ir.ComplexIntExpr(
        [
            ir.VarDecl("T_0", INT_TY),
            ir.Assign("T_0", ir.IntFieldAccess("X", "Y", MSG_TY), INT_TY),
        ],
        ir.Neg(ir.IntVar("T_0", INT_TY)),
    )
    assert expr_conv.to_ir(
        Selected(Variable("X", type_=rty.Message("M")), "Y", type_=SEQ_TY),
        id_generator(),
    ) == ir.ComplexExpr([], ir.ObjFieldAccess("X", "Y", MSG_TY))


def test_to_ir_call() -> None:
    assert expr_conv.to_ir(
        Call(
            "X",
            INT_TY,
            [Variable("Y", type_=rty.BOOLEAN), Variable("Z", type_=INT_TY)],
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [],
        ir.IntCall("X", [ir.BoolVar("Y"), ir.IntVar("Z", INT_TY)], [rty.BOOLEAN, INT_TY], INT_TY),
    )
    assert expr_conv.to_ir(
        Call(
            "X",
            rty.BOOLEAN,
            [Variable("Y", type_=rty.BOOLEAN), Variable("Z", type_=rty.BOOLEAN)],
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [],
        ir.BoolCall("X", [ir.BoolVar("Y"), ir.BoolVar("Z")], [rty.BOOLEAN, rty.BOOLEAN]),
    )
    assert expr_conv.to_ir(
        Call(
            "X",
            rty.BOOLEAN,
            [
                And(Variable("X", type_=rty.BOOLEAN), TRUE),
                Add(Variable("Y", type_=INT_TY), Number(1)),
            ],
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [],
        ir.BoolCall(
            "X",
            [
                ir.And(ir.BoolVar("X"), ir.BoolVal(value=True)),
                ir.Add(ir.IntVar("Y", INT_TY), ir.IntVal(1)),
            ],
            [rty.BOOLEAN, INT_TY],
        ),
    )
    assert expr_conv.to_ir(
        Call(
            "X",
            MSG_TY,
            [Variable("Y", type_=rty.BOOLEAN), Variable("Z", type_=INT_TY)],
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [],
        ir.ObjCall("X", [ir.BoolVar("Y"), ir.IntVar("Z", INT_TY)], [rty.BOOLEAN, INT_TY], MSG_TY),
    )


def test_to_ir_conversion() -> None:
    assert expr_conv.to_ir(
        Conversion("I", Variable("Y", type_=rty.BOOLEAN), type_=INT_TY),
        id_generator(),
    ) == ir.ComplexExpr([], ir.Conversion(INT_TY, ir.BoolVar("Y")))


def test_to_ir_comprehension() -> None:
    assert expr_conv.to_ir(
        Comprehension(
            "X",
            Selected(Variable("M", type_=rty.Message("M")), "Y", type_=rty.Sequence("S", INT_TY)),
            Add(Variable("X", type_=INT_TY), Variable("Y", type_=INT_TY), Number(1)),
            Less(Sub(Variable("X", type_=INT_TY), Number(1)), Number(ir.INT_MAX)),
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [],
        ir.Comprehension(
            "X",
            ir.ObjFieldAccess("M", ID("Y"), MSG_TY),
            ir.ComplexExpr(
                [
                    ir.VarDecl("T_0", rty.BASE_INTEGER),
                    ir.Assign(
                        "T_0",
                        ir.Add(ir.IntVar("Y", INT_TY), ir.IntVal(1)),
                        rty.BASE_INTEGER,
                    ),
                ],
                ir.Add(ir.IntVar("X", INT_TY), ir.IntVar("T_0", rty.BASE_INTEGER)),
            ),
            ir.ComplexBoolExpr(
                [
                    ir.VarDecl("T_1", rty.BASE_INTEGER),
                    ir.Assign("T_1", ir.Sub(ir.IntVar("X", INT_TY), ir.IntVal(1)), rty.BOOLEAN),
                ],
                ir.Less(ir.IntVar("T_1", rty.BASE_INTEGER), ir.IntVal(ir.INT_MAX)),
            ),
        ),
    )


@pytest.mark.parametrize(
    ("agg", "ir_agg"),
    [(MessageAggregate, ir.MsgAgg), (DeltaMessageAggregate, ir.DeltaMsgAgg)],
)
def test_to_ir_message_aggregate(  # type: ignore[misc]
    agg: Callable[..., Expr],
    ir_agg: Callable[..., ir.Expr],
) -> None:
    assert expr_conv.to_ir(
        agg(
            "X",
            {
                "Y": Selected(
                    Variable("M", type_=rty.Message("M")),
                    "Y",
                    type_=rty.Sequence("S", INT_TY),
                ),
                "Z": Add(Variable("X", type_=INT_TY), Variable("Y", type_=INT_TY), Number(1)),
            },
            MSG_TY,
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [
            ir.VarDecl("T_0", rty.BASE_INTEGER),
            ir.Assign("T_0", ir.Add(ir.IntVar("Y", INT_TY), ir.IntVal(1)), rty.BASE_INTEGER),
        ],
        ir_agg(
            "X",
            {
                ID("Y"): ir.ObjFieldAccess("M", ID("Y"), MSG_TY),
                ID("Z"): ir.Add(ir.IntVar("X", INT_TY), ir.IntVar("T_0", rty.BASE_INTEGER)),
            },
            MSG_TY,
        ),
    )


def test_to_ir_case() -> None:
    assert expr_conv.to_ir(
        CaseExpr(
            Variable("X", type_=INT_TY),
            [
                ([Number(1), Number(3)], Number(0)),
                (
                    [Number(2)],
                    Variable("X", type_=INT_TY),
                ),
            ],
        ),
        id_generator(),
    ) == ir.ComplexExpr(
        [],
        ir.CaseExpr(
            ir.IntVar("X", INT_TY),
            [
                (
                    [ir.IntVal(1), ir.IntVal(3)],
                    ir.IntVal(0),
                ),
                (
                    [ir.IntVal(2)],
                    ir.IntVar("X", INT_TY),
                ),
            ],
            INT_TY,
        ),
    )
