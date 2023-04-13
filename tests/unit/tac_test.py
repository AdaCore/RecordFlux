from __future__ import annotations

from collections.abc import Callable

import pytest
import z3

from rflx import expression as expr, tac, typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID, id_generator

PROOF_MANAGER = tac.ProofManager(2)
INT_TY = rty.Integer("I", rty.Bounds(10, 100))
ENUM_TY = rty.Enumeration("E", [ID("E1"), ID("E2")])
MSG_TY = rty.Message("M", {("E", "I")}, {}, {ID("E"): ENUM_TY, ID("I"): INT_TY})
SEQ_TY = rty.Sequence("S", rty.Message("M"))


def test_constructed_origin_location() -> None:
    assert tac.ConstructedOrigin("X", Location((1, 2))).location == Location((1, 2))


def test_stmt_location() -> None:
    assert tac.Assign(
        "X",
        tac.IntVar("Y", INT_TY),
        INT_TY,
        origin=expr.Variable("X", location=Location((1, 2))),
    ).location == Location((1, 2))


def test_assign() -> None:
    assign = tac.Assign("X", tac.IntVar("Y", INT_TY), INT_TY)
    assert assign.target == ID("X")
    assert assign.expression == tac.IntVar("Y", INT_TY)


def test_assign_str() -> None:
    assert str(tac.Assign("X", tac.IntVar("Y", INT_TY), INT_TY)) == "X := Y"


def test_assign_substituted() -> None:
    assert tac.Assign("X", tac.IntVar("Y", INT_TY), INT_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Assign("Y", tac.IntVar("Z", INT_TY), INT_TY)


def test_assign_to_z3_expr() -> None:
    assert tac.Assign("X", tac.IntVar("Y", INT_TY), INT_TY).to_z3_expr() == (
        z3.Int("X") == z3.Int("Y")
    )
    assert tac.Assign("X", tac.BoolVar("Y"), rty.BOOLEAN).to_z3_expr() == (
        z3.Bool("X") == z3.Bool("Y")
    )
    assert tac.Assign("X", tac.ObjVar("Y", MSG_TY), MSG_TY).to_z3_expr() == z3.BoolVal(True)


def test_assign_target_var() -> None:
    assert tac.Assign("X", tac.IntVar("Y", INT_TY), INT_TY).target_var == tac.IntVar("X", INT_TY)
    assert tac.Assign("X", tac.BoolVar("Y"), rty.BOOLEAN).target_var == tac.BoolVar("X")
    assert tac.Assign("X", tac.ObjVar("Y", MSG_TY), MSG_TY).target_var == tac.ObjVar("X", MSG_TY)


def test_field_assign_str() -> None:
    assert str(tac.FieldAssign("X", "Y", tac.IntVar("Z", INT_TY), MSG_TY)) == "X.Y := Z"


def test_field_assign_type() -> None:
    assert tac.FieldAssign("X", "Y", tac.IntVar("Z", INT_TY), MSG_TY).type_ == MSG_TY


def test_field_assign_substituted() -> None:
    assert tac.FieldAssign("X", "Y", tac.IntVar("Z", INT_TY), MSG_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
            ID("Z"): ID("A"),
        }
    ) == tac.FieldAssign("Y", "Y", tac.IntVar("A", INT_TY), MSG_TY)


def test_field_assign_preconditions() -> None:
    assert not tac.FieldAssign("X", "Y", tac.IntVar("Z", INT_TY), MSG_TY).preconditions(
        id_generator()
    )


def test_field_assign_to_z3_expr() -> None:
    assert tac.FieldAssign("X", "Y", tac.IntVar("Z", INT_TY), MSG_TY).to_z3_expr() == (
        z3.Int("X.Y") == z3.Int("Z")
    )
    assert tac.FieldAssign("X", "Y", tac.BoolVar("Z"), MSG_TY).to_z3_expr() == (
        z3.Bool("X.Y") == z3.Bool("Z")
    )
    assert tac.FieldAssign("X", "Y", tac.ObjVar("Z", MSG_TY), MSG_TY).to_z3_expr() == z3.BoolVal(
        True
    )


def test_append_str() -> None:
    assert str(tac.Append("X", tac.IntVar("Y", INT_TY), SEQ_TY)) == "X'Append (Y)"


def test_append_substituted() -> None:
    assert tac.Append("X", tac.IntVar("Y", INT_TY), SEQ_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Append("Y", tac.IntVar("Z", INT_TY), SEQ_TY)


def test_append_preconditions() -> None:
    assert not tac.Append("X", tac.IntVar("Y", INT_TY), SEQ_TY).preconditions(id_generator())


def test_append_to_z3_expr() -> None:
    assert tac.Append("X", tac.ObjVar("Y", MSG_TY), SEQ_TY).to_z3_expr() == z3.BoolVal(True)


def test_extend_str() -> None:
    assert str(tac.Extend("X", tac.IntVar("Y", INT_TY), SEQ_TY)) == "X'Extend (Y)"


def test_extend_substituted() -> None:
    assert tac.Extend("X", tac.IntVar("Y", INT_TY), SEQ_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Extend("Y", tac.IntVar("Z", INT_TY), SEQ_TY)


def test_extend_preconditions() -> None:
    assert not tac.Extend("X", tac.IntVar("Y", INT_TY), SEQ_TY).preconditions(id_generator())


def test_extend_to_z3_expr() -> None:
    assert tac.Extend("X", tac.ObjVar("Y", SEQ_TY), SEQ_TY).to_z3_expr() == z3.BoolVal(True)


def test_reset_str() -> None:
    assert str(tac.Reset("X", {}, MSG_TY)) == "X'Reset"
    assert str(tac.Reset("X", {ID("Y"): tac.IntVar("Z", INT_TY)}, MSG_TY)) == "X'Reset (Y => Z)"


def test_reset_substituted() -> None:
    assert tac.Reset("X", {ID("Y"): tac.IntVar("Z", INT_TY)}, MSG_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
            ID("Z"): ID("A"),
        }
    ) == tac.Reset("Y", {ID("Y"): tac.IntVar("A", INT_TY)}, MSG_TY)


def test_reset_preconditions() -> None:
    assert not tac.Reset("X", {}, MSG_TY).preconditions(id_generator())
    assert not tac.Reset("X", {ID("Y"): tac.IntVar("Z", INT_TY)}, MSG_TY).preconditions(
        id_generator()
    )


def test_reset_to_z3_expr() -> None:
    assert tac.Reset("X", {}, MSG_TY).to_z3_expr() == z3.BoolVal(True)
    assert tac.Reset("X", {ID("Y"): tac.IntVar("Z", INT_TY)}, MSG_TY).to_z3_expr() == z3.BoolVal(
        True
    )


def test_read_str() -> None:
    assert str(tac.Read("X", tac.IntVar("Y", INT_TY))) == "X'Read (Y)"


def test_read_substituted() -> None:
    assert tac.Read("X", tac.IntVar("Y", INT_TY)).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Read("X", tac.IntVar("Z", INT_TY))


def test_read_preconditions() -> None:
    assert not tac.Read("X", tac.IntVar("Y", INT_TY)).preconditions(id_generator())


def test_read_to_z3_expr() -> None:
    assert tac.Read("X", tac.ObjVar("Y", MSG_TY)).to_z3_expr() == z3.BoolVal(True)


def test_write_str() -> None:
    assert str(tac.Write("X", tac.IntVar("Y", INT_TY))) == "X'Write (Y)"


def test_write_substituted() -> None:
    assert tac.Write("X", tac.IntVar("Y", INT_TY)).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Write("X", tac.IntVar("Z", INT_TY))


def test_write_preconditions() -> None:
    assert not tac.Write("X", tac.IntVar("Y", INT_TY)).preconditions(id_generator())


def test_write_z3_expr() -> None:
    assert tac.Write("X", tac.ObjVar("Y", MSG_TY)).to_z3_expr() == z3.BoolVal(True)


def test_assert_substituted() -> None:
    assert tac.Assert(tac.BoolVar("X")).substituted(
        {
            ID("X"): ID("Y"),
        }
    ) == tac.Assert(tac.BoolVar("Y"))


def test_assert_to_z3_expr() -> None:
    assert tac.Assert(tac.BoolVar("X")).to_z3_expr() == z3.Bool("X")


def test_assert_preconditions() -> None:
    assert not tac.Assert(tac.BoolVar("X")).preconditions(id_generator())


def test_expr_location() -> None:
    assert tac.IntVar(
        "X",
        INT_TY,
        origin=expr.Variable("X", location=Location((1, 2))),
    ).location == Location((1, 2))


def test_int_var_identifier() -> None:
    assert tac.IntVar("X", INT_TY).identifier == ID("X")


def test_int_var_type() -> None:
    assert tac.IntVar("X", INT_TY).type_ == INT_TY


def test_int_var_to_z3_expr() -> None:
    assert tac.IntVar("X", INT_TY).to_z3_expr() == z3.Int("X")


def test_int_var_preconditions() -> None:
    assert not tac.IntVar("X", INT_TY).preconditions(id_generator())


def test_bool_var_identifier() -> None:
    assert tac.BoolVar("X").identifier == ID("X")


def test_bool_var_type() -> None:
    assert tac.BoolVar("X").type_ == rty.BOOLEAN


def test_bool_var_to_z3_expr() -> None:
    assert tac.BoolVar("X").to_z3_expr() == z3.Bool("X")


def test_bool_var_preconditions() -> None:
    assert not tac.BoolVar("X").preconditions(id_generator())


def test_obj_var_identifier() -> None:
    assert tac.ObjVar("X", ENUM_TY).identifier == ID("X")


def test_obj_var_type() -> None:
    assert tac.ObjVar("X", ENUM_TY).type_ == ENUM_TY


def test_obj_var_substituted() -> None:
    assert tac.ObjVar("X", ENUM_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.ObjVar("Y", ENUM_TY)


def test_obj_var_to_z3_expr() -> None:
    with pytest.raises(NotImplementedError):
        tac.ObjVar("X", MSG_TY).to_z3_expr()


def test_enum_lit_str() -> None:
    assert str(tac.EnumLit("Lit", ENUM_TY)) == "Lit"


def test_enum_lit_type() -> None:
    assert tac.EnumLit("Lit", ENUM_TY).type_ == ENUM_TY


def test_enum_lit_substituted() -> None:
    assert tac.EnumLit("X", ENUM_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.EnumLit("X", ENUM_TY)


def test_enum_lit_to_z3_expr() -> None:
    assert tac.EnumLit("Lit", ENUM_TY).to_z3_expr() == z3.Int("Lit")


def test_int_val_str() -> None:
    assert str(tac.IntVal(1)) == "1"


def test_int_val_to_z3_expr() -> None:
    assert tac.IntVal(1).to_z3_expr() == z3.IntVal(1)


def test_bool_val_str() -> None:
    assert str(tac.BoolVal(True)) == "True"
    assert str(tac.BoolVal(False)) == "False"


def test_bool_val_to_z3_expr() -> None:
    assert tac.BoolVal(True).to_z3_expr() == z3.BoolVal(True)
    assert tac.BoolVal(False).to_z3_expr() == z3.BoolVal(False)


@pytest.mark.parametrize(
    "attribute, expected",
    [
        (tac.Size("X"), "X'Size"),
        (tac.Length("X"), "X'Length"),
        (tac.First("X"), "X'First"),
        (tac.Last("X"), "X'Last"),
        (tac.ValidChecksum("X"), "X'Valid_Checksum"),
        (tac.Valid("X"), "X'Valid"),
        (tac.Present("X"), "X'Present"),
        (tac.HasData("X"), "X'Has_Data"),
        (tac.Head("X", ENUM_TY), "X'Head"),
        (tac.Opaque("X"), "X'Opaque"),
    ],
)
def test_attr_str(attribute: tac.Attr, expected: str) -> None:
    assert str(attribute) == expected


@pytest.mark.parametrize(
    "attribute, expected",
    [
        (tac.Size("X"), rty.UniversalInteger()),
        (tac.Length("X"), rty.UniversalInteger()),
        (tac.First("X"), rty.UniversalInteger()),
        (tac.Last("X"), rty.UniversalInteger()),
        (tac.ValidChecksum("X"), rty.BOOLEAN),
        (tac.Valid("X"), rty.BOOLEAN),
        (tac.Present("X"), rty.BOOLEAN),
        (tac.HasData("X"), rty.BOOLEAN),
        (tac.Head("X", ENUM_TY), ENUM_TY),
        (tac.Opaque("X"), rty.OPAQUE),
    ],
)
def test_attr_type(attribute: tac.Attr, expected: rty.Type) -> None:
    assert attribute.type_ == expected


@pytest.mark.parametrize(
    "attribute, expected",
    [
        (tac.Size("X"), tac.Size("Y")),
        (tac.Length("X"), tac.Length("Y")),
        (tac.First("X"), tac.First("Y")),
        (tac.Last("X"), tac.Last("Y")),
        (tac.ValidChecksum("X"), tac.ValidChecksum("Y")),
        (tac.Valid("X"), tac.Valid("Y")),
        (tac.Present("X"), tac.Present("Y")),
        (tac.HasData("X"), tac.HasData("Y")),
        (tac.Head("X", ENUM_TY), tac.Head("Y", ENUM_TY)),
        (tac.Opaque("X"), tac.Opaque("Y")),
    ],
)
def test_attr_substituted(attribute: tac.Attr, expected: tac.Attr) -> None:
    assert (
        attribute.substituted(
            {
                ID("X"): ID("Y"),
                ID("Y"): ID("Z"),
            }
        )
        == expected
    )


@pytest.mark.parametrize(
    "attribute, expected",
    [
        (tac.Size("X"), z3.Int("X'Size")),
        (tac.Length("X"), z3.Int("X'Length")),
        (tac.First("X"), z3.Int("X'First")),
        (tac.Last("X"), z3.Int("X'Last")),
        (tac.ValidChecksum("X"), z3.Bool("X'Valid_Checksum")),
        (tac.Valid("X"), z3.Bool("X'Valid")),
        (tac.Present("X"), z3.Bool("X'Present")),
        (tac.HasData("X"), z3.Bool("X'Has_Data")),
    ],
)
def test_attr_z3_expr(attribute: tac.Attr, expected: z3.ExprRef) -> None:
    assert attribute.prefix == ID("X")
    assert attribute.to_z3_expr() == expected


@pytest.mark.parametrize(
    "binary_expr",
    [
        tac.Add,
        tac.Sub,
        tac.Mul,
        tac.Div,
        tac.Pow,
        tac.Mod,
        tac.And,
        tac.Or,
        tac.Less,
        tac.LessEqual,
        tac.Equal,
        tac.GreaterEqual,
        tac.Greater,
        tac.NotEqual,
    ],
)
def test_binary_expr_location(binary_expr: type[tac.BinaryExpr]) -> None:
    assert binary_expr(tac.IntVar("X", INT_TY), tac.IntVal(1)).location is None
    assert binary_expr(
        tac.IntVar("X", INT_TY), tac.IntVal(1), origin=tac.ConstructedOrigin("Z", Location((1, 2)))
    ).location == Location((1, 2))
    assert binary_expr(
        tac.IntVar("X", INT_TY, origin=tac.ConstructedOrigin("X", Location((1, 2)))), tac.IntVal(1)
    ).location == Location((1, 2))


@pytest.mark.parametrize(
    "binary_expr",
    [
        tac.Add,
        tac.Sub,
        tac.Mul,
        tac.Div,
        tac.Pow,
        tac.Mod,
        tac.And,
        tac.Or,
        tac.Less,
        tac.LessEqual,
        tac.Equal,
        tac.GreaterEqual,
        tac.Greater,
        tac.NotEqual,
    ],
)
def test_binary_expr_origin_str(binary_expr: type[tac.BinaryExpr]) -> None:
    assert binary_expr(tac.IntVar("X", INT_TY), tac.IntVal(1)).origin_str == str(
        binary_expr(tac.IntVar("X", INT_TY), tac.IntVal(1))
    )
    assert (
        binary_expr(
            tac.IntVar("X", INT_TY), tac.IntVal(1), origin=tac.ConstructedOrigin("Z", None)
        ).origin_str
        == "Z"
    )


@pytest.mark.parametrize(
    "binary_expr",
    [
        tac.Add,
        tac.Sub,
        tac.Mul,
        tac.Div,
        tac.Pow,
        tac.Mod,
        tac.And,
        tac.Or,
        tac.Less,
        tac.LessEqual,
        tac.Equal,
        tac.GreaterEqual,
        tac.Greater,
        tac.NotEqual,
    ],
)
def test_binary_expr_substituted(
    binary_expr: Callable[[tac.BasicIntExpr, tac.BasicIntExpr], tac.Relation]
) -> None:
    assert binary_expr(tac.IntVar("X", INT_TY), tac.IntVal(1)).substituted(
        {
            ID("X"): ID("Y"),
        }
    ) == binary_expr(tac.IntVar("Y", INT_TY), tac.IntVal(1))


def test_add_str() -> None:
    assert str(tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X + 1"


def test_add_to_z3_expr() -> None:
    assert tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") + z3.IntVal(1)
    )


def test_add_preconditions() -> None:
    assert tac.Add(
        tac.IntVar("X", INT_TY), tac.IntVal(1), origin=expr.Add(expr.Variable("X"), expr.Number(1))
    ).preconditions(id_generator()) == [
        tac.Cond(
            tac.LessEqual(tac.IntVar("X", INT_TY), tac.IntVar("T_0", INT_TY)),
            [tac.Assign("T_0", tac.Sub(tac.IntVal(tac.INT_MAX), tac.IntVal(1)), INT_TY)],
        ),
    ]


def test_sub_str() -> None:
    assert str(tac.Sub(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X - 1"


def test_sub_to_z3_expr() -> None:
    assert tac.Sub(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") - z3.IntVal(1)
    )


def test_sub_preconditions() -> None:
    assert tac.Sub(tac.IntVar("X", INT_TY), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(
            tac.GreaterEqual(tac.IntVar("X", INT_TY), tac.IntVal(1)),
        ),
    ]


def test_mul_str() -> None:
    assert str(tac.Mul(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X * 1"


def test_mul_to_z3_expr() -> None:
    assert tac.Mul(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") * z3.IntVal(1)
    )


def test_mul_preconditions() -> None:
    assert tac.Mul(tac.IntVar("X", INT_TY), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(
            tac.LessEqual(tac.IntVar("X", INT_TY), tac.IntVar("T_0", INT_TY)),
            [tac.Assign("T_0", tac.Div(tac.IntVal(tac.INT_MAX), tac.IntVal(1)), INT_TY)],
        ),
    ]


def test_div_str() -> None:
    assert str(tac.Div(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X / 1"


def test_div_to_z3_expr() -> None:
    assert tac.Div(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") / z3.IntVal(1)
    )


def test_div_preconditions() -> None:
    assert tac.Div(tac.IntVar("X", INT_TY), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(tac.NotEqual(tac.IntVal(1), tac.IntVal(0))),
    ]


def test_pow_str() -> None:
    assert str(tac.Pow(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X ** 1"


def test_pow_to_z3_expr() -> None:
    assert tac.Pow(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") ** z3.IntVal(1)
    )


def test_pow_preconditions() -> None:
    assert tac.Pow(tac.IntVar("X", INT_TY), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(
            tac.LessEqual(tac.IntVar("T_0", INT_TY), tac.IntVal(tac.INT_MAX)),
            [tac.Assign("T_0", tac.Pow(tac.IntVar("X", INT_TY), tac.IntVal(1)), INT_TY)],
        ),
    ]


def test_mod_str() -> None:
    assert str(tac.Mod(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X mod 1"


def test_mod_to_z3_expr() -> None:
    assert tac.Mod(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") % z3.IntVal(1)
    )


def test_mod_preconditions() -> None:
    assert tac.Mod(tac.IntVar("X", INT_TY), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(tac.NotEqual(tac.IntVal(1), tac.IntVal(0))),
    ]


@pytest.mark.parametrize(
    "unary_expr",
    [
        tac.Not,
    ],
)
def test_unary_expr_substituted(
    unary_expr: Callable[[tac.BasicIntExpr, tac.BasicIntExpr], tac.Relation]
) -> None:
    assert unary_expr(tac.IntVar("X", INT_TY), tac.IntVal(1)).substituted(
        {
            ID("X"): ID("Y"),
        }
    ) == unary_expr(tac.IntVar("Y", INT_TY), tac.IntVal(1))


def test_not_str() -> None:
    assert str(tac.Not(tac.BoolVar("X"))) == "not X"


def test_not_to_z3_expr() -> None:
    assert tac.Not(tac.BoolVar("X")).to_z3_expr() == z3.Not(z3.Bool("X"))


def test_and_str() -> None:
    assert str(tac.And(tac.BoolVar("X"), tac.BoolVal(True))) == "X and True"


def test_and_to_z3_expr() -> None:
    assert tac.And(tac.BoolVar("X"), tac.BoolVal(True)).to_z3_expr() == z3.And(
        z3.Bool("X"), z3.BoolVal(True)
    )


def test_or_str() -> None:
    assert str(tac.Or(tac.BoolVar("X"), tac.BoolVal(True))) == "X or True"


def test_or_to_z3_expr() -> None:
    assert tac.Or(tac.BoolVar("X"), tac.BoolVal(True)).to_z3_expr() == z3.Or(
        z3.Bool("X"), z3.BoolVal(True)
    )


def test_less_str() -> None:
    assert str(tac.Less(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X < 1"


def test_less_to_z3_expr() -> None:
    assert tac.Less(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") < z3.IntVal(1)
    )


def test_less_equal_str() -> None:
    assert str(tac.LessEqual(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X <= 1"


def test_less_equal_to_z3_expr() -> None:
    assert tac.LessEqual(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") <= z3.IntVal(1)
    )


def test_equal_str() -> None:
    assert str(tac.Equal(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X = 1"


def test_equal_to_z3_expr() -> None:
    assert tac.Equal(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") == z3.IntVal(1)
    )


def test_greater_equal_str() -> None:
    assert str(tac.GreaterEqual(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X >= 1"


def test_greater_equal_to_z3_expr() -> None:
    assert tac.GreaterEqual(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") >= z3.IntVal(1)
    )


def test_greater_str() -> None:
    assert str(tac.Greater(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X > 1"


def test_greater_to_z3_expr() -> None:
    assert tac.Greater(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") > z3.IntVal(1)
    )


def test_not_equal_str() -> None:
    assert str(tac.NotEqual(tac.IntVar("X", INT_TY), tac.IntVal(1))) == "X /= 1"


def test_not_equal_to_z3_expr() -> None:
    assert tac.NotEqual(tac.IntVar("X", INT_TY), tac.IntVal(1)).to_z3_expr() == (
        z3.Int("X") != z3.IntVal(1)
    )


def test_int_call_str() -> None:
    assert (
        str(tac.IntCall("X", [tac.IntVar("Y", INT_TY), tac.BoolVal(True)], INT_TY)) == "X (Y, True)"
    )


def test_int_call_substituted() -> None:
    assert tac.IntCall("X", [tac.IntVar("Y", INT_TY)], INT_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.IntCall("X", [tac.IntVar("Z", INT_TY)], INT_TY)


def test_int_call_to_z3_expr() -> None:
    assert tac.IntCall(
        "X", [tac.IntVar("Y", INT_TY), tac.BoolVal(True)], INT_TY
    ).to_z3_expr() == z3.Int("X")


def test_bool_call_str() -> None:
    assert str(tac.BoolCall("X", [tac.BoolVar("Y"), tac.BoolVal(True)])) == "X (Y, True)"


def test_bool_call_type() -> None:
    assert tac.BoolCall("X", [tac.BoolVar("Y")], tac.BoolVal(True)).type_ == rty.BOOLEAN


def test_bool_call_substituted() -> None:
    assert tac.BoolCall("X", [tac.BoolVar("Y")]).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.BoolCall("X", [tac.BoolVar("Z")])


def test_bool_call_to_z3_expr() -> None:
    assert tac.BoolCall("X", [tac.BoolVar("Y")], tac.BoolVal(True)).to_z3_expr() == z3.Bool("X")


def test_obj_call_str() -> None:
    assert (
        str(tac.ObjCall("X", [tac.ObjVar("Y", MSG_TY), tac.BoolVal(True)], ENUM_TY))
        == "X (Y, True)"
    )


def test_obj_call_type() -> None:
    assert tac.ObjCall("X", [tac.ObjVar("Y", MSG_TY), tac.BoolVal(True)], ENUM_TY).type_ == ENUM_TY


def test_obj_call_substituted() -> None:
    assert tac.ObjCall("X", [tac.BoolVar("Y")], ENUM_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.ObjCall("X", [tac.BoolVar("Z")], ENUM_TY)


def test_call_preconditions() -> None:
    call = tac.IntCall("X", [tac.IntVar("Y", INT_TY), tac.BoolVal(True)], INT_TY)
    assert not call.preconditions(id_generator())
    call.set_preconditions([tac.Cond(tac.Greater(tac.IntVar("Y", INT_TY), tac.IntVal(0)))])
    assert call.preconditions(id_generator()) == [
        tac.Cond(tac.Greater(tac.IntVar("Y", INT_TY), tac.IntVal(0)))
    ]


def test_int_field_access_str() -> None:
    assert str(tac.IntFieldAccess("M", "F", MSG_TY)) == "M.F"


def test_int_field_access_type() -> None:
    assert tac.IntFieldAccess("M", "I", MSG_TY).type_ == INT_TY


def test_int_field_access_substituted() -> None:
    assert tac.IntFieldAccess("M", "F", MSG_TY).substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")}
    ) == tac.IntFieldAccess("X", "F", MSG_TY)


def test_int_field_access_to_z3_expr() -> None:
    assert tac.IntFieldAccess("M", "F", MSG_TY).to_z3_expr() == z3.Int("M.F")


def test_bool_field_access_str() -> None:
    assert str(tac.BoolFieldAccess("M", "F", MSG_TY)) == "M.F"


def test_bool_field_access_substituted() -> None:
    assert tac.BoolFieldAccess("M", "F", MSG_TY).substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")}
    ) == tac.BoolFieldAccess("X", "F", MSG_TY)


def test_bool_field_access_to_z3_expr() -> None:
    assert tac.BoolFieldAccess("M", "F", MSG_TY).to_z3_expr() == z3.Bool("M.F")


def test_obj_field_access_str() -> None:
    assert str(tac.ObjFieldAccess("M", "F", MSG_TY)) == "M.F"


def test_obj_field_access_type() -> None:
    assert tac.ObjFieldAccess("M", "E", MSG_TY).type_ == ENUM_TY


def test_obj_field_access_substituted() -> None:
    assert tac.ObjFieldAccess("M", "F", MSG_TY).substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")}
    ) == tac.ObjFieldAccess("X", "F", MSG_TY)


def test_obj_field_access_to_z3_expr() -> None:
    with pytest.raises(NotImplementedError):
        tac.ObjFieldAccess("M", "F", MSG_TY).to_z3_expr()


def test_int_if_expr_str() -> None:
    assert (
        str(tac.IntIfExpr(tac.BoolVar("X"), tac.IntVar("Y", INT_TY), tac.IntVal(1), INT_TY))
        == "(if X then Y else 1)"
    )


def test_int_if_expr_type() -> None:
    assert (
        tac.IntIfExpr(tac.BoolVar("X"), tac.IntVar("Y", INT_TY), tac.IntVal(1), INT_TY).type_
        == INT_TY
    )


def test_int_if_expr_substituted() -> None:
    assert tac.IntIfExpr(
        tac.BoolVar("X"), tac.IntVar("Y", INT_TY), tac.IntVal(False), INT_TY
    ).substituted({ID("X"): ID("Y"), ID("Y"): ID("Z")}) == tac.IntIfExpr(
        tac.BoolVar("Y"), tac.IntVar("Z", INT_TY), tac.IntVal(False), INT_TY
    )


def test_int_if_expr_to_z3_expr() -> None:
    assert tac.IntIfExpr(
        tac.BoolVar("X"), tac.IntVar("Y", INT_TY), tac.IntVal(1), INT_TY
    ).to_z3_expr() == z3.If(z3.Bool("X"), z3.Int("Y"), z3.IntVal(1))


def test_bool_if_expr_str() -> None:
    assert (
        str(tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)))
        == "(if X then Y else False)"
    )


def test_bool_if_expr_type() -> None:
    assert (
        tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)).type_ == rty.BOOLEAN
    )


def test_bool_if_expr_substituted() -> None:
    assert tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.BoolIfExpr(tac.BoolVar("Y"), tac.BoolVar("Z"), tac.BoolVal(False))


def test_bool_if_expr_to_z3_expr() -> None:
    assert tac.BoolIfExpr(
        tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)
    ).to_z3_expr() == z3.If(z3.Bool("X"), z3.Bool("Y"), z3.BoolVal(False))


def test_conversion_str() -> None:
    assert str(tac.Conversion("X", tac.IntVar("Y", INT_TY), INT_TY)) == "X (Y)"


def test_conversion_type() -> None:
    assert tac.Conversion("X", tac.IntVar("Y", INT_TY), INT_TY).type_ == INT_TY


def test_conversion_substituted() -> None:
    assert tac.Conversion("X", tac.IntVar("Y", INT_TY), INT_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.Conversion("X", tac.IntVar("Z", INT_TY), INT_TY)


def test_conversion_to_z3_expr() -> None:
    assert tac.Conversion("X", tac.IntVar("Y", INT_TY), INT_TY).to_z3_expr() == z3.BoolVal(True)


def test_comprehension_str() -> None:
    assert (
        str(
            tac.Comprehension(
                "X", tac.ObjVar("Y", SEQ_TY), [], tac.ObjVar("X", MSG_TY), [], tac.BoolVal(True)
            )
        )
        == "[for X in Y if True => X]"
    )
    assert (
        str(
            tac.Comprehension(
                "X",
                tac.ObjVar("Y", SEQ_TY),
                [tac.Assign("A", tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1)), INT_TY)],
                tac.IntVar("A", INT_TY),
                [
                    tac.Assign(
                        "B", tac.Greater(tac.IntVar("X", INT_TY), tac.IntVar("0", INT_TY)), INT_TY
                    )
                ],
                tac.BoolVar("B"),
            )
        )
        == "[for X in Y if {B := X > 0; B} => {A := X + 1; A}]"
    )


def test_comprehension_type() -> None:
    assert tac.Comprehension(
        "X", tac.ObjVar("Y", SEQ_TY), [], tac.ObjVar("X", MSG_TY), [], tac.BoolVal(True)
    ).type_ == rty.Aggregate(MSG_TY)


def test_comprehension_precondition() -> None:
    assert not tac.Comprehension(
        "X", tac.ObjVar("Y", SEQ_TY), [], tac.ObjVar("X", MSG_TY), [], tac.BoolVal(True)
    ).preconditions(id_generator())
    assert not tac.Comprehension(
        "X",
        tac.ObjVar("Y", SEQ_TY),
        [tac.Assign("A", tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1)), INT_TY)],
        tac.IntVar("A", INT_TY),
        [tac.Assign("B", tac.Greater(tac.IntVar("X", INT_TY), tac.IntVar("0", INT_TY)), INT_TY)],
        tac.BoolVar("B"),
    ).preconditions(id_generator())


def test_agg_str() -> None:
    assert str(tac.Agg([tac.IntVar("X", INT_TY), tac.IntVal(1)])) == "[X, 1]"


def test_agg_type() -> None:
    assert tac.Agg([tac.IntVar("X", INT_TY), tac.IntVal(10)]).type_ == rty.Aggregate(INT_TY)


def test_agg_substituted() -> None:
    assert tac.Agg([tac.IntVar("X", INT_TY), tac.IntVal(1)]).substituted(
        {ID("X"): ID("Y")}
    ) == tac.Agg([tac.IntVar("Y", INT_TY), tac.IntVal(1)])


def test_agg_preconditions() -> None:
    assert not tac.Agg([tac.IntVar("X", INT_TY), tac.IntVal(1)]).preconditions(id_generator())


def test_str_str() -> None:
    assert str(tac.Str("X")) == '"X"'


def test_str_type() -> None:
    assert tac.Str("X").type_ == rty.OPAQUE


def test_str_substituted() -> None:
    assert tac.Str("X").substituted({ID("X"): ID("Y")}) == tac.Str("X")


def test_str_preconditions() -> None:
    assert not tac.Str("X").preconditions(id_generator())


def test_msg_agg_str() -> None:
    assert str(tac.MsgAgg("X", {}, MSG_TY)) == "X'(null message)"
    assert str(tac.MsgAgg("X", {ID("Y"): tac.IntVal(1)}, MSG_TY)) == "X'(Y => 1)"


def test_msg_agg_type() -> None:
    assert tac.MsgAgg("X", {}, MSG_TY).type_ == MSG_TY


def test_msg_agg_substituted() -> None:
    assert tac.MsgAgg("X", {ID("Y"): tac.IntVar("Z", INT_TY)}, MSG_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z"), ID("Z"): ID("A")}
    ) == tac.MsgAgg("X", {ID("Y"): tac.IntVar("A", INT_TY)}, MSG_TY)


def test_msg_agg_preconditions() -> None:
    assert not tac.MsgAgg("X", {ID("Y"): tac.IntVal(1)}, MSG_TY).preconditions(id_generator())


def test_delta_msg_agg_str() -> None:
    assert str(tac.DeltaMsgAgg("X", {}, MSG_TY)) == "X with delta null message"
    assert str(tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVal(1)}, MSG_TY)) == "X with delta Y => 1"


def test_delta_msg_agg_type() -> None:
    assert tac.DeltaMsgAgg("X", {}, MSG_TY).type_ == MSG_TY


def test_delta_msg_agg_substituted() -> None:
    assert tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVar("Z", INT_TY)}, MSG_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z"), ID("Z"): ID("A")}
    ) == tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVar("A", INT_TY)}, MSG_TY)


def test_delta_msg_agg_preconditions() -> None:
    assert not tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVal(1)}, MSG_TY).preconditions(id_generator())


def test_case_expr_str() -> None:
    assert str(
        tac.CaseExpr(
            tac.IntVar("X", INT_TY),
            [
                (
                    [tac.IntVal(1), tac.IntVal(3)],
                    [tac.Assign("T_1", tac.IntFieldAccess("M", ID("Y"), MSG_TY), INT_TY)],
                    tac.IntVar("T_1", INT_TY),
                ),
                (
                    [tac.IntVal(2)],
                    [tac.Assign("T_3", tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1)), INT_TY)],
                    tac.IntVar("T_3", INT_TY),
                ),
            ],
            INT_TY,
        )
    ) == (
        "(case X is\n"
        "      when 1 | 3 => {T_1 := M.Y;}; T_1,\n"
        "      when 2 => {T_3 := X + 1;}; T_3)"
    )


def test_case_expr_type() -> None:
    assert (
        tac.CaseExpr(
            tac.IntVar("X", INT_TY),
            [
                (
                    [tac.IntVal(1), tac.IntVal(3)],
                    [tac.Assign("T_1", tac.IntFieldAccess("M", ID("Y"), MSG_TY), INT_TY)],
                    tac.IntVar("T_1", INT_TY),
                ),
                (
                    [tac.IntVal(2)],
                    [tac.Assign("T_3", tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1)), INT_TY)],
                    tac.IntVar("T_3", INT_TY),
                ),
            ],
            INT_TY,
        ).type_
        == INT_TY
    )


def test_case_expr_precondition() -> None:
    assert not tac.CaseExpr(
        tac.IntVar("X", INT_TY),
        [
            (
                [tac.IntVal(1), tac.IntVal(3)],
                [tac.Assign("T_1", tac.IntFieldAccess("M", ID("Y"), MSG_TY), INT_TY)],
                tac.IntVar("T_1", INT_TY),
            ),
            (
                [tac.IntVal(2)],
                [tac.Assign("T_3", tac.Add(tac.IntVar("X", INT_TY), tac.IntVal(1)), INT_TY)],
                tac.IntVar("T_3", INT_TY),
            ),
        ],
        INT_TY,
    ).preconditions(id_generator())


def test_add_checks() -> None:
    assert tac.add_checks(
        [
            tac.Assign(
                "X",
                tac.Div(
                    tac.IntVar("Y", INT_TY),
                    tac.IntVar("Z", INT_TY),
                    origin=expr.Variable("X", location=Location((1, 2))),
                ),
                INT_TY,
            )
        ],
        PROOF_MANAGER,
        id_generator(),
    ) == [
        tac.Assert(
            tac.NotEqual(
                tac.IntVar("Z", INT_TY),
                tac.IntVal(0),
                origin=expr.Variable("X", location=Location((1, 2))),
            )
        ),
        tac.Assign(
            "X",
            tac.Div(
                tac.IntVar("Y", INT_TY),
                tac.IntVar("Z", INT_TY),
                origin=expr.Variable("X", location=Location((1, 2))),
            ),
            INT_TY,
        ),
    ]
    assert tac.add_checks(
        [
            tac.Assign("A", tac.Add(tac.IntVar("Y", INT_TY), tac.IntVal(1)), INT_TY),
            tac.Assign("B", tac.Div(tac.IntVar("A", INT_TY), tac.IntVar("Z", INT_TY)), INT_TY),
            tac.Assign("X", tac.Sub(tac.IntVar("B", INT_TY), tac.IntVal(1)), INT_TY),
            tac.Assign("Z", tac.IntVal(0), INT_TY),
            tac.Assign("C", tac.Add(tac.IntVar("Z", INT_TY), tac.IntVal(1)), INT_TY),
        ],
        PROOF_MANAGER,
        id_generator(),
    ) == [
        tac.Assign("T_0", tac.Sub(tac.IntVal(tac.INT_MAX), tac.IntVal(1)), INT_TY),
        tac.Assert(tac.LessEqual(tac.IntVar("Y", INT_TY), tac.IntVar("T_0", INT_TY))),
        tac.Assign("A", tac.Add(tac.IntVar("Y", INT_TY), tac.IntVal(1)), INT_TY),
        tac.Assert(tac.NotEqual(tac.IntVar("Z", INT_TY), tac.IntVal(0))),
        tac.Assign("B", tac.Div(tac.IntVar("A", INT_TY), tac.IntVar("Z", INT_TY)), INT_TY),
        tac.Assert(tac.GreaterEqual(tac.IntVar("B", INT_TY), tac.IntVal(1))),
        tac.Assign("X", tac.Sub(tac.IntVar("B", INT_TY), tac.IntVal(1)), INT_TY),
        tac.Assign("Z", tac.IntVal(0), INT_TY),
        tac.Assign("C", tac.Add(tac.IntVar("Z", INT_TY), tac.IntVal(1)), INT_TY),
    ]


def test_check_preconditions() -> None:
    tac.check_preconditions(
        [
            tac.Assign("Z", tac.IntVal(1), INT_TY),
            tac.Assign(
                "X",
                tac.Div(
                    tac.IntVar("Y", INT_TY),
                    tac.IntVar("Z", INT_TY),
                    origin=expr.Variable("X", location=Location((1, 2))),
                ),
                INT_TY,
            ),
        ],
        PROOF_MANAGER,
        id_generator(),
    ).propagate()


@pytest.mark.parametrize(
    "expression, error",
    [
        (
            tac.Div(
                tac.IntVar("Y", INT_TY),
                tac.IntVar("Z", INT_TY, origin=expr.Variable("Z", location=Location((1, 2)))),
            ),
            "Z /= 0",
        )
    ],
)
def test_check_preconditions_error(expression: tac.Expr, error: str) -> None:
    with pytest.raises(
        RecordFluxError,
        match=rf"^<stdin>:1:2: model: error: precondition might fail, cannot prove {error}$",
    ):
        tac.check_preconditions(
            [
                tac.Assign(
                    "X",
                    expression,
                    INT_TY,
                )
            ],
            PROOF_MANAGER,
            id_generator(),
        ).propagate()


def test_to_ssa() -> None:
    assert tac.to_ssa(
        [
            tac.Assign("A", tac.IntVal(1), INT_TY),
            tac.Assign("A", tac.IntVal(0), INT_TY),
            tac.Assign("A", tac.Div(tac.IntVar("Y", INT_TY), tac.IntVar("A", INT_TY)), INT_TY),
            tac.Assign("B", tac.IntVar("A", INT_TY), INT_TY),
            tac.Append("C", tac.IntVar("B", INT_TY), SEQ_TY),
        ]
    ) == [
        tac.Assign("S_A_0", tac.IntVal(1), INT_TY),
        tac.Assign("S_A_1", tac.IntVal(0), INT_TY),
        tac.Assign("S_A_2", tac.Div(tac.IntVar("Y", INT_TY), tac.IntVar("S_A_1", INT_TY)), INT_TY),
        tac.Assign("B", tac.IntVar("A", INT_TY), INT_TY),
        tac.Append("C", tac.IntVar("B", INT_TY), SEQ_TY),
    ]
    assert tac.to_ssa(
        [
            tac.Assign("A", tac.IntVal(0), INT_TY),
            tac.Assign("B", tac.IntVal(1), INT_TY),
            tac.Assign(
                "A",
                tac.CaseExpr(
                    tac.IntVar("X", INT_TY),
                    [
                        (
                            [tac.IntVal(1), tac.IntVal(3)],
                            [],
                            tac.IntVar("A", INT_TY),
                        ),
                        (
                            [tac.IntVal(2)],
                            [
                                tac.Assign(
                                    "T_0", tac.Add(tac.IntVar("B", INT_TY), tac.IntVal(1)), INT_TY
                                )
                            ],
                            tac.IntVar("T_0", INT_TY),
                        ),
                    ],
                    INT_TY,
                ),
                INT_TY,
            ),
            tac.Assign("B", tac.Div(tac.IntVar("A", INT_TY), tac.IntVar("B", INT_TY)), INT_TY),
        ]
    ) == [
        tac.Assign("S_A_0", tac.IntVal(0), INT_TY),
        tac.Assign("S_B_0", tac.IntVal(1), INT_TY),
        tac.Assign(
            "S_A_1",
            tac.CaseExpr(
                tac.IntVar("X", INT_TY),
                [
                    (
                        [tac.IntVal(1), tac.IntVal(3)],
                        [],
                        tac.IntVar("S_A_0", INT_TY),
                    ),
                    (
                        [tac.IntVal(2)],
                        [
                            tac.Assign(
                                "T_0", tac.Add(tac.IntVar("S_B_0", INT_TY), tac.IntVal(1)), INT_TY
                            )
                        ],
                        tac.IntVar("T_0", INT_TY),
                    ),
                ],
                INT_TY,
            ),
            INT_TY,
        ),
        tac.Assign(
            "S_B_1", tac.Div(tac.IntVar("S_A_1", INT_TY), tac.IntVar("S_B_0", INT_TY)), INT_TY
        ),
    ]
    assert tac.to_ssa(
        [
            tac.Assign("A", tac.IntVal(0), INT_TY),
            tac.Assign("B", tac.IntVal(1), INT_TY),
            tac.Assign(
                "A",
                tac.Comprehension(
                    "X",
                    tac.ObjVar("Y", SEQ_TY),
                    [tac.Assign("T_0", tac.Add(tac.IntVar("A", INT_TY), tac.IntVal(1)), INT_TY)],
                    tac.IntVar("T_0", INT_TY),
                    [
                        tac.Assign(
                            "T_1",
                            tac.Greater(tac.IntVar("B", INT_TY), tac.IntVar("0", INT_TY)),
                            INT_TY,
                        )
                    ],
                    tac.BoolVar("T_1"),
                ),
                SEQ_TY,
            ),
            tac.Assign("B", tac.Div(tac.IntVar("A", INT_TY), tac.IntVar("B", INT_TY)), INT_TY),
        ]
    ) == [
        tac.Assign("S_A_0", tac.IntVal(0), INT_TY),
        tac.Assign("S_B_0", tac.IntVal(1), INT_TY),
        tac.Assign(
            "S_A_1",
            tac.Comprehension(
                "X",
                tac.ObjVar("Y", SEQ_TY),
                [tac.Assign("T_0", tac.Add(tac.IntVar("S_A_0", INT_TY), tac.IntVal(1)), INT_TY)],
                tac.IntVar("T_0", INT_TY),
                [
                    tac.Assign(
                        "T_1",
                        tac.Greater(tac.IntVar("S_B_0", INT_TY), tac.IntVar("0", INT_TY)),
                        INT_TY,
                    )
                ],
                tac.BoolVar("T_1"),
            ),
            SEQ_TY,
        ),
        tac.Assign(
            "S_B_1", tac.Div(tac.IntVar("S_A_1", INT_TY), tac.IntVar("S_B_0", INT_TY)), INT_TY
        ),
    ]
