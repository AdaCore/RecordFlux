# pylint: disable = too-many-lines

from __future__ import annotations

from collections.abc import Callable

import pytest
import z3

from rflx import expression as expr, tac
from rflx.error import Location, RecordFluxError
from rflx.identifier import ID, id_generator

PROOF_MANAGER = tac.ProofManager(2)


def test_stmt_location() -> None:
    assert tac.Assign(
        "X",
        tac.IntVar("Y"),
        origin=expr.Variable("X", location=Location((1, 2))),
    ).location == Location((1, 2))


def test_assign() -> None:
    assign = tac.Assign("X", tac.IntVar("Y"))
    assert assign.target == ID("X")
    assert assign.expression == tac.IntVar("Y")


def test_assign_str() -> None:
    assert str(tac.Assign("X", tac.IntVar("Y"))) == "X := Y"


def test_assign_substituted() -> None:
    assert tac.Assign("X", tac.IntVar("Y")).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Assign("Y", tac.IntVar("Z"))


def test_assign_z3expr() -> None:
    assert tac.Assign("X", tac.IntVar("Y")).z3expr == (z3.Int("X") == z3.Int("Y"))
    assert tac.Assign("X", tac.BoolVar("Y")).z3expr == (z3.Bool("X") == z3.Bool("Y"))
    assert tac.Assign("X", tac.ObjVar("Y")).z3expr == z3.BoolVal(True)


def test_assign_target_var() -> None:
    assert tac.Assign("X", tac.IntVar("Y")).target_var == tac.IntVar("X")
    assert tac.Assign("X", tac.BoolVar("Y")).target_var == tac.BoolVar("X")
    assert tac.Assign("X", tac.ObjVar("Y")).target_var == tac.ObjVar("X")


def test_field_assign_str() -> None:
    assert str(tac.FieldAssign("X", "Y", tac.IntVar("Z"))) == "X.Y := Z"


def test_field_assign_substituted() -> None:
    assert tac.FieldAssign("X", "Y", tac.IntVar("Z")).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
            ID("Z"): ID("A"),
        }
    ) == tac.FieldAssign("Y", "Y", tac.IntVar("A"))


def test_field_assign_preconditions() -> None:
    assert not tac.FieldAssign("X", "Y", tac.IntVar("Z")).preconditions(id_generator())


def test_field_assign_z3expr() -> None:
    assert tac.FieldAssign("X", "Y", tac.IntVar("Z")).z3expr == (z3.Int("X.Y") == z3.Int("Z"))
    assert tac.FieldAssign("X", "Y", tac.BoolVar("Z")).z3expr == (z3.Bool("X.Y") == z3.Bool("Z"))
    assert tac.FieldAssign("X", "Y", tac.ObjVar("Z")).z3expr == z3.BoolVal(True)


def test_append_str() -> None:
    assert str(tac.Append("X", tac.IntVar("Y"))) == "X'Append (Y)"


def test_append_substituted() -> None:
    assert tac.Append("X", tac.IntVar("Y")).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Append("Y", tac.IntVar("Z"))


def test_append_preconditions() -> None:
    assert not tac.Append("X", tac.IntVar("Y")).preconditions(id_generator())


def test_append_z3expr() -> None:
    assert tac.Append("X", tac.ObjVar("Y")).z3expr == z3.BoolVal(True)


def test_extend_str() -> None:
    assert str(tac.Extend("X", tac.IntVar("Y"))) == "X'Extend (Y)"


def test_extend_substituted() -> None:
    assert tac.Extend("X", tac.IntVar("Y")).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Extend("Y", tac.IntVar("Z"))


def test_extend_preconditions() -> None:
    assert not tac.Extend("X", tac.IntVar("Y")).preconditions(id_generator())


def test_extend_z3expr() -> None:
    assert tac.Extend("X", tac.ObjVar("Y")).z3expr == z3.BoolVal(True)


def test_reset_str() -> None:
    assert str(tac.Reset("X", {})) == "X'Reset"
    assert str(tac.Reset("X", {ID("Y"): tac.IntVar("Z")})) == "X'Reset (Y => Z)"


def test_reset_substituted() -> None:
    assert tac.Reset("X", {ID("Y"): tac.IntVar("Z")}).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
            ID("Z"): ID("A"),
        }
    ) == tac.Reset("Y", {ID("Y"): tac.IntVar("A")})


def test_reset_preconditions() -> None:
    assert not tac.Reset("X", {}).preconditions(id_generator())
    assert not tac.Reset("X", {ID("Y"): tac.IntVar("Z")}).preconditions(id_generator())


def test_reset_z3expr() -> None:
    assert tac.Reset("X", {}).z3expr == z3.BoolVal(True)
    assert tac.Reset("X", {ID("Y"): tac.IntVar("Z")}).z3expr == z3.BoolVal(True)


def test_read_str() -> None:
    assert str(tac.Read("X", tac.IntVar("Y"))) == "X'Read (Y)"


def test_read_substituted() -> None:
    assert tac.Read("X", tac.IntVar("Y")).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Read("X", tac.IntVar("Z"))


def test_read_preconditions() -> None:
    assert not tac.Read("X", tac.IntVar("Y")).preconditions(id_generator())


def test_read_z3expr() -> None:
    assert tac.Read("X", tac.ObjVar("Y")).z3expr == z3.BoolVal(True)


def test_write_str() -> None:
    assert str(tac.Write("X", tac.IntVar("Y"))) == "X'Write (Y)"


def test_write_substituted() -> None:
    assert tac.Write("X", tac.IntVar("Y")).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.Write("X", tac.IntVar("Z"))


def test_write_preconditions() -> None:
    assert not tac.Write("X", tac.IntVar("Y")).preconditions(id_generator())


def test_write_z3expr() -> None:
    assert tac.Write("X", tac.ObjVar("Y")).z3expr == z3.BoolVal(True)


def test_assert_substituted() -> None:
    assert tac.Assert(tac.BoolVar("X")).substituted(
        {
            ID("X"): ID("Y"),
        }
    ) == tac.Assert(tac.BoolVar("Y"))


def test_assert_z3expr() -> None:
    assert tac.Assert(tac.BoolVar("X")).z3expr == z3.Bool("X")


def test_assert_preconditions() -> None:
    assert not tac.Assert(tac.BoolVar("X")).preconditions(id_generator())


def test_expr_location() -> None:
    assert tac.IntVar(
        "X",
        origin=expr.Variable("X", location=Location((1, 2))),
    ).location == Location((1, 2))


def test_int_var() -> None:
    var = tac.IntVar("X")
    assert var.identifier == ID("X")


def test_int_var_z3expr() -> None:
    assert tac.IntVar("X").z3expr == z3.Int("X")


def test_int_var_preconditions() -> None:
    assert not tac.IntVar("X").preconditions(id_generator())


def test_bool_var() -> None:
    var = tac.BoolVar("X")
    assert var.identifier == ID("X")


def test_bool_var_z3expr() -> None:
    assert tac.BoolVar("X").z3expr == z3.Bool("X")


def test_bool_var_preconditions() -> None:
    assert not tac.BoolVar("X").preconditions(id_generator())


def test_obj_var_z3expr() -> None:
    with pytest.raises(NotImplementedError):
        tac.ObjVar("X").z3expr  # pylint: disable = expression-not-assigned


def test_enum_lit_str() -> None:
    assert str(tac.EnumLit("Lit")) == "Lit"


def test_enum_lit_substituted() -> None:
    assert tac.EnumLit("X").substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        }
    ) == tac.EnumLit("X")


def test_enum_lit_z3expr() -> None:
    assert tac.EnumLit("Lit").z3expr == z3.Int("Lit")


def test_int_val_str() -> None:
    assert str(tac.IntVal(1)) == "1"


def test_int_val_z3expr() -> None:
    assert tac.IntVal(1).z3expr == z3.IntVal(1)


def test_bool_val_str() -> None:
    assert str(tac.BoolVal(True)) == "True"
    assert str(tac.BoolVal(False)) == "False"


def test_bool_val_z3expr() -> None:
    assert tac.BoolVal(True).z3expr == z3.BoolVal(True)
    assert tac.BoolVal(False).z3expr == z3.BoolVal(False)


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
    ],
)
def test_attr_str(attribute: tac.Attr, expected: str) -> None:
    assert str(attribute) == expected


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
def test_attr_z3expr(attribute: tac.Attr, expected: z3.ExprRef) -> None:
    assert attribute.prefix == ID("X")
    assert attribute.z3expr == expected


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
    assert binary_expr(tac.IntVar("X"), tac.IntVal(1)).substituted(
        {
            ID("X"): ID("Y"),
        }
    ) == binary_expr(tac.IntVar("Y"), tac.IntVal(1))


def test_add_str() -> None:
    assert str(tac.Add(tac.IntVar("X"), tac.IntVal(1))) == "X + 1"


def test_add_z3expr() -> None:
    assert tac.Add(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") + z3.IntVal(1))


def test_add_preconditions() -> None:
    assert tac.Add(
        tac.IntVar("X"), tac.IntVal(1), origin=expr.Add(expr.Variable("X"), expr.Number(1))
    ).preconditions(id_generator()) == [
        tac.Cond(
            tac.LessEqual(tac.IntVar("X"), tac.IntVar("T_0")),
            [tac.Assign("T_0", tac.Sub(tac.IntVal(tac.INT_MAX), tac.IntVal(1)))],
        ),
    ]


def test_sub_str() -> None:
    assert str(tac.Sub(tac.IntVar("X"), tac.IntVal(1))) == "X - 1"


def test_sub_z3expr() -> None:
    assert tac.Sub(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") - z3.IntVal(1))


def test_sub_preconditions() -> None:
    assert tac.Sub(tac.IntVar("X"), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(
            tac.GreaterEqual(tac.IntVar("X"), tac.IntVal(1)),
        ),
    ]


def test_mul_str() -> None:
    assert str(tac.Mul(tac.IntVar("X"), tac.IntVal(1))) == "X * 1"


def test_mul_z3expr() -> None:
    assert tac.Mul(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") * z3.IntVal(1))


def test_mul_preconditions() -> None:
    assert tac.Mul(tac.IntVar("X"), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(
            tac.LessEqual(tac.IntVar("X"), tac.IntVar("T_0")),
            [tac.Assign("T_0", tac.Div(tac.IntVal(tac.INT_MAX), tac.IntVal(1)))],
        ),
    ]


def test_div_str() -> None:
    assert str(tac.Div(tac.IntVar("X"), tac.IntVal(1))) == "X / 1"


def test_div_z3expr() -> None:
    assert tac.Div(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") / z3.IntVal(1))


def test_div_preconditions() -> None:
    assert tac.Div(tac.IntVar("X"), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(tac.NotEqual(tac.IntVal(1), tac.IntVal(0))),
    ]


def test_pow_str() -> None:
    assert str(tac.Pow(tac.IntVar("X"), tac.IntVal(1))) == "X ** 1"


def test_pow_z3expr() -> None:
    assert tac.Pow(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") ** z3.IntVal(1))


def test_pow_preconditions() -> None:
    assert tac.Pow(tac.IntVar("X"), tac.IntVal(1)).preconditions(id_generator()) == [
        tac.Cond(
            tac.LessEqual(tac.IntVar("T_0"), tac.IntVal(tac.INT_MAX)),
            [tac.Assign("T_0", tac.Pow(tac.IntVar("X"), tac.IntVal(1)))],
        ),
    ]


def test_mod_str() -> None:
    assert str(tac.Mod(tac.IntVar("X"), tac.IntVal(1))) == "X mod 1"


def test_mod_z3expr() -> None:
    assert tac.Mod(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") % z3.IntVal(1))


def test_mod_preconditions() -> None:
    assert tac.Mod(tac.IntVar("X"), tac.IntVal(1)).preconditions(id_generator()) == [
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
    assert unary_expr(tac.IntVar("X"), tac.IntVal(1)).substituted(
        {
            ID("X"): ID("Y"),
        }
    ) == unary_expr(tac.IntVar("Y"), tac.IntVal(1))


def test_not_str() -> None:
    assert str(tac.Not(tac.BoolVar("X"))) == "not X"


def test_not_z3expr() -> None:
    assert tac.Not(tac.BoolVar("X")).z3expr == z3.Not(z3.Bool("X"))


def test_and_str() -> None:
    assert str(tac.And(tac.BoolVar("X"), tac.BoolVal(True))) == "X and True"


def test_and_z3expr() -> None:
    assert tac.And(tac.BoolVar("X"), tac.BoolVal(True)).z3expr == z3.And(
        z3.Bool("X"), z3.BoolVal(True)
    )


def test_or_str() -> None:
    assert str(tac.Or(tac.BoolVar("X"), tac.BoolVal(True))) == "X or True"


def test_or_z3expr() -> None:
    assert tac.Or(tac.BoolVar("X"), tac.BoolVal(True)).z3expr == z3.Or(
        z3.Bool("X"), z3.BoolVal(True)
    )


def test_less_str() -> None:
    assert str(tac.Less(tac.IntVar("X"), tac.IntVal(1))) == "X < 1"


def test_less_z3expr() -> None:
    assert tac.Less(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") < z3.IntVal(1))


def test_less_equal_str() -> None:
    assert str(tac.LessEqual(tac.IntVar("X"), tac.IntVal(1))) == "X <= 1"


def test_less_equal_z3expr() -> None:
    assert tac.LessEqual(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") <= z3.IntVal(1))


def test_equal_str() -> None:
    assert str(tac.Equal(tac.IntVar("X"), tac.IntVal(1))) == "X = 1"


def test_equal_z3expr() -> None:
    assert tac.Equal(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") == z3.IntVal(1))


def test_greater_equal_str() -> None:
    assert str(tac.GreaterEqual(tac.IntVar("X"), tac.IntVal(1))) == "X >= 1"


def test_greater_equal_z3expr() -> None:
    assert tac.GreaterEqual(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") >= z3.IntVal(1))


def test_greater_str() -> None:
    assert str(tac.Greater(tac.IntVar("X"), tac.IntVal(1))) == "X > 1"


def test_greater_z3expr() -> None:
    assert tac.Greater(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") > z3.IntVal(1))


def test_not_equal_str() -> None:
    assert str(tac.NotEqual(tac.IntVar("X"), tac.IntVal(1))) == "X /= 1"


def test_not_equal_z3expr() -> None:
    assert tac.NotEqual(tac.IntVar("X"), tac.IntVal(1)).z3expr == (z3.Int("X") != z3.IntVal(1))


def test_int_call_str() -> None:
    assert str(tac.IntCall("X", tac.IntVar("Y"), tac.BoolVal(True))) == "X (Y, True)"


def test_int_call_substituted() -> None:
    assert tac.IntCall("X", tac.IntVar("Y")).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.IntCall("X", tac.IntVar("Z"))


def test_int_call_z3expr() -> None:
    assert tac.IntCall("X", tac.IntVar("Y"), tac.BoolVal(True)).z3expr == z3.Int("X")


def test_bool_call_str() -> None:
    assert str(tac.BoolCall("X", tac.BoolVar("Y"), tac.BoolVal(True))) == "X (Y, True)"


def test_bool_call_substituted() -> None:
    assert tac.BoolCall("X", tac.BoolVar("Y")).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.BoolCall("X", tac.BoolVar("Z"))


def test_bool_call_z3expr() -> None:
    assert tac.BoolCall("X", tac.BoolVar("Y"), tac.BoolVal(True)).z3expr == z3.Bool("X")


def test_obj_call_str() -> None:
    assert str(tac.ObjCall("X", tac.ObjVar("Y"), tac.BoolVal(True))) == "X (Y, True)"


def test_obj_call_substituted() -> None:
    assert tac.ObjCall("X", tac.BoolVar("Y")).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.ObjCall("X", tac.BoolVar("Z"))


def test_call_preconditions() -> None:
    call = tac.IntCall("X", tac.IntVar("Y"), tac.BoolVal(True))
    assert not call.preconditions(id_generator())
    call.set_preconditions([tac.Cond(tac.Greater(tac.IntVar("Y"), tac.IntVal(0)))])
    assert call.preconditions(id_generator()) == [
        tac.Cond(tac.Greater(tac.IntVar("Y"), tac.IntVal(0)))
    ]


def test_int_field_access_str() -> None:
    assert str(tac.IntFieldAccess("M", "F")) == "M.F"


def test_int_field_access_substituted() -> None:
    assert tac.IntFieldAccess("M", "F").substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")}
    ) == tac.IntFieldAccess("X", "F")


def test_int_field_access_z3expr() -> None:
    assert tac.IntFieldAccess("M", "F").z3expr == z3.Int("M.F")


def test_bool_field_access_str() -> None:
    assert str(tac.BoolFieldAccess("M", "F")) == "M.F"


def test_bool_field_access_substituted() -> None:
    assert tac.BoolFieldAccess("M", "F").substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")}
    ) == tac.BoolFieldAccess("X", "F")


def test_bool_field_access_z3expr() -> None:
    assert tac.BoolFieldAccess("M", "F").z3expr == z3.Bool("M.F")


def test_obj_field_access_str() -> None:
    assert str(tac.ObjFieldAccess("M", "F")) == "M.F"


def test_obj_field_access_substituted() -> None:
    assert tac.ObjFieldAccess("M", "F").substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")}
    ) == tac.ObjFieldAccess("X", "F")


def test_obj_field_access_z3expr() -> None:
    with pytest.raises(NotImplementedError):
        tac.ObjFieldAccess("M", "F").z3expr  # pylint: disable = expression-not-assigned


def test_int_if_expr_str() -> None:
    assert (
        str(tac.IntIfExpr(tac.BoolVar("X"), tac.IntVar("Y"), tac.IntVal(1)))
        == "(if X then Y else 1)"
    )


def test_int_if_expr_substituted() -> None:
    assert tac.IntIfExpr(tac.BoolVar("X"), tac.IntVar("Y"), tac.IntVal(False)).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.IntIfExpr(tac.BoolVar("Y"), tac.IntVar("Z"), tac.IntVal(False))


def test_int_if_expr_z3expr() -> None:
    assert tac.IntIfExpr(tac.BoolVar("X"), tac.IntVar("Y"), tac.IntVal(1)).z3expr == z3.If(
        z3.Bool("X"), z3.Int("Y"), z3.IntVal(1)
    )


def test_bool_if_expr_str() -> None:
    assert (
        str(tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)))
        == "(if X then Y else False)"
    )


def test_bool_if_expr_substituted() -> None:
    assert tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.BoolIfExpr(tac.BoolVar("Y"), tac.BoolVar("Z"), tac.BoolVal(False))


def test_bool_if_expr_z3expr() -> None:
    assert tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)).z3expr == z3.If(
        z3.Bool("X"), z3.Bool("Y"), z3.BoolVal(False)
    )


def test_conversion_str() -> None:
    assert str(tac.Conversion("X", tac.ObjVar("Y"))) == "X (Y)"


def test_conversion_substituted() -> None:
    assert tac.Conversion("X", tac.ObjVar("Y")).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")}
    ) == tac.Conversion("X", tac.ObjVar("Z"))


def test_conversion_z3expr() -> None:
    assert tac.Conversion("X", tac.ObjVar("Y")).z3expr == z3.BoolVal(True)


def test_comprehension_str() -> None:
    assert (
        str(tac.Comprehension("X", tac.ObjVar("Y"), [], tac.ObjVar("X"), [], tac.BoolVal(True)))
        == "[for X in Y if True => X]"
    )
    assert (
        str(
            tac.Comprehension(
                "X",
                tac.ObjVar("Y"),
                [tac.Assign("A", tac.Add(tac.IntVar("X"), tac.IntVal(1)))],
                tac.IntVar("A"),
                [tac.Assign("B", tac.Greater(tac.IntVar("X"), tac.IntVar("0")))],
                tac.BoolVar("B"),
            )
        )
        == "[for X in Y if {B := X > 0; B} => {A := X + 1; A}]"
    )


def test_comprehension_precondition() -> None:
    assert not tac.Comprehension(
        "X", tac.ObjVar("Y"), [], tac.ObjVar("X"), [], tac.BoolVal(True)
    ).preconditions(id_generator())
    assert not tac.Comprehension(
        "X",
        tac.ObjVar("Y"),
        [tac.Assign("A", tac.Add(tac.IntVar("X"), tac.IntVal(1)))],
        tac.IntVar("A"),
        [tac.Assign("B", tac.Greater(tac.IntVar("X"), tac.IntVar("0")))],
        tac.BoolVar("B"),
    ).preconditions(id_generator())


def test_agg_str() -> None:
    assert str(tac.Agg(tac.IntVar("X"), tac.IntVal(1))) == "[X, 1]"


def test_agg_substituted() -> None:
    assert tac.Agg(tac.IntVar("X"), tac.IntVal(1)).substituted({ID("X"): ID("Y")}) == tac.Agg(
        tac.IntVar("Y"), tac.IntVal(1)
    )


def test_agg_preconditions() -> None:
    assert not tac.Agg(tac.IntVar("X"), tac.IntVal(1)).preconditions(id_generator())


def test_str_str() -> None:
    assert str(tac.Str("X")) == '"X"'


def test_str_substituted() -> None:
    assert tac.Str("X").substituted({ID("X"): ID("Y")}) == tac.Str("X")


def test_str_preconditions() -> None:
    assert not tac.Str("X").preconditions(id_generator())


def test_msg_agg_str() -> None:
    assert str(tac.MsgAgg("X", {})) == "X'(null message)"
    assert str(tac.MsgAgg("X", {ID("Y"): tac.IntVal(1)})) == "X'(Y => 1)"


def test_msg_agg_substituted() -> None:
    assert tac.MsgAgg("X", {ID("Y"): tac.IntVar("Z")}).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z"), ID("Z"): ID("A")}
    ) == tac.MsgAgg("X", {ID("Y"): tac.IntVar("A")})


def test_msg_agg_preconditions() -> None:
    assert not tac.MsgAgg("X", {ID("Y"): tac.IntVal(1)}).preconditions(id_generator())


def test_delta_msg_agg_str() -> None:
    assert str(tac.DeltaMsgAgg("X", {})) == "X with delta null message"
    assert str(tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVal(1)})) == "X with delta Y => 1"


def test_delta_msg_agg_substituted() -> None:
    assert tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVar("Z")}).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z"), ID("Z"): ID("A")}
    ) == tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVar("A")})


def test_delta_msg_agg_preconditions() -> None:
    assert not tac.DeltaMsgAgg("X", {ID("Y"): tac.IntVal(1)}).preconditions(id_generator())


def test_case_expr_str() -> None:
    assert str(
        tac.CaseExpr(
            tac.IntVar("X"),
            [
                (
                    [tac.IntVal(1), tac.IntVal(3)],
                    [tac.Assign("T_1", tac.IntFieldAccess("M", ID("Y")))],
                    tac.IntVar("T_1"),
                ),
                (
                    [tac.IntVal(2)],
                    [tac.Assign("T_3", tac.Add(tac.IntVar("X"), tac.IntVal(1)))],
                    tac.IntVar("T_3"),
                ),
            ],
        )
    ) == (
        "(case X is\n"
        "      when 1 | 3 => {T_1 := M.Y;}; T_1,\n"
        "      when 2 => {T_3 := X + 1;}; T_3)"
    )


def test_case_expr_precondition() -> None:
    assert not tac.CaseExpr(
        tac.IntVar("X"),
        [
            (
                [tac.IntVal(1), tac.IntVal(3)],
                [tac.Assign("T_1", tac.IntFieldAccess("M", ID("Y")))],
                tac.IntVar("T_1"),
            ),
            (
                [tac.IntVal(2)],
                [tac.Assign("T_3", tac.Add(tac.IntVar("X"), tac.IntVal(1)))],
                tac.IntVar("T_3"),
            ),
        ],
    ).preconditions(id_generator())


def test_add_checks() -> None:
    assert tac.add_checks(
        [
            tac.Assign(
                "X",
                tac.Div(
                    tac.IntVar("Y"),
                    tac.IntVar("Z"),
                    origin=expr.Variable("X", location=Location((1, 2))),
                ),
            )
        ],
        PROOF_MANAGER,
        id_generator(),
    ) == [
        tac.Assert(
            tac.NotEqual(
                tac.IntVar("Z"), tac.IntVal(0), origin=expr.Variable("X", location=Location((1, 2)))
            )
        ),
        tac.Assign(
            "X",
            tac.Div(
                tac.IntVar("Y"),
                tac.IntVar("Z"),
                origin=expr.Variable("X", location=Location((1, 2))),
            ),
        ),
    ]
    assert tac.add_checks(
        [
            tac.Assign("A", tac.Add(tac.IntVar("Y"), tac.IntVal(1))),
            tac.Assign("B", tac.Div(tac.IntVar("A"), tac.IntVar("Z"))),
            tac.Assign("X", tac.Sub(tac.IntVar("B"), tac.IntVal(1))),
            tac.Assign("Z", tac.IntVal(0)),
            tac.Assign("C", tac.Add(tac.IntVar("Z"), tac.IntVal(1))),
        ],
        PROOF_MANAGER,
        id_generator(),
    ) == [
        tac.Assign("T_0", tac.Sub(tac.IntVal(tac.INT_MAX), tac.IntVal(1))),
        tac.Assert(tac.LessEqual(tac.IntVar("Y"), tac.IntVar("T_0"))),
        tac.Assign("A", tac.Add(tac.IntVar("Y"), tac.IntVal(1))),
        tac.Assert(tac.NotEqual(tac.IntVar("Z"), tac.IntVal(0))),
        tac.Assign("B", tac.Div(tac.IntVar("A"), tac.IntVar("Z"))),
        tac.Assert(tac.GreaterEqual(tac.IntVar("B"), tac.IntVal(1))),
        tac.Assign("X", tac.Sub(tac.IntVar("B"), tac.IntVal(1))),
        tac.Assign("Z", tac.IntVal(0)),
        tac.Assign("C", tac.Add(tac.IntVar("Z"), tac.IntVal(1))),
    ]


def test_check_preconditions() -> None:
    tac.check_preconditions(
        [
            tac.Assign("Z", tac.IntVal(1)),
            tac.Assign(
                "X",
                tac.Div(
                    tac.IntVar("Y"),
                    tac.IntVar("Z"),
                    origin=expr.Variable("X", location=Location((1, 2))),
                ),
            ),
        ],
        PROOF_MANAGER,
        id_generator(),
    ).propagate()

    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:1:2: model: error: precondition might fail, cannot prove Z /= 0$",
    ):
        tac.check_preconditions(
            [
                tac.Assign(
                    "X",
                    tac.Div(
                        tac.IntVar("Y"),
                        tac.IntVar("Z", origin=expr.Variable("Z", location=Location((1, 2)))),
                    ),
                )
            ],
            PROOF_MANAGER,
            id_generator(),
        ).propagate()


def test_to_ssa() -> None:
    assert tac.to_ssa(
        [
            tac.Assign("A", tac.IntVal(1)),
            tac.Assign("A", tac.IntVal(0)),
            tac.Assign("A", tac.Div(tac.IntVar("Y"), tac.IntVar("A"))),
            tac.Assign("B", tac.IntVar("A")),
            tac.Append("C", tac.IntVar("B")),
        ]
    ) == [
        tac.Assign("S_A_0", tac.IntVal(1)),
        tac.Assign("S_A_1", tac.IntVal(0)),
        tac.Assign("S_A_2", tac.Div(tac.IntVar("Y"), tac.IntVar("S_A_1"))),
        tac.Assign("B", tac.IntVar("A")),
        tac.Append("C", tac.IntVar("B")),
    ]
    assert tac.to_ssa(
        [
            tac.Assign("A", tac.IntVal(0)),
            tac.Assign("B", tac.IntVal(1)),
            tac.Assign(
                "A",
                tac.CaseExpr(
                    tac.IntVar("X"),
                    [
                        (
                            [tac.IntVal(1), tac.IntVal(3)],
                            [],
                            tac.IntVar("A"),
                        ),
                        (
                            [tac.IntVal(2)],
                            [tac.Assign("T_0", tac.Add(tac.IntVar("B"), tac.IntVal(1)))],
                            tac.IntVar("T_0"),
                        ),
                    ],
                ),
            ),
            tac.Assign("B", tac.Div(tac.IntVar("A"), tac.IntVar("B"))),
        ]
    ) == [
        tac.Assign("S_A_0", tac.IntVal(0)),
        tac.Assign("S_B_0", tac.IntVal(1)),
        tac.Assign(
            "S_A_1",
            tac.CaseExpr(
                tac.IntVar("X"),
                [
                    (
                        [tac.IntVal(1), tac.IntVal(3)],
                        [],
                        tac.IntVar("S_A_0"),
                    ),
                    (
                        [tac.IntVal(2)],
                        [tac.Assign("T_0", tac.Add(tac.IntVar("S_B_0"), tac.IntVal(1)))],
                        tac.IntVar("T_0"),
                    ),
                ],
            ),
        ),
        tac.Assign("S_B_1", tac.Div(tac.IntVar("S_A_1"), tac.IntVar("S_B_0"))),
    ]
    assert tac.to_ssa(
        [
            tac.Assign("A", tac.IntVal(0)),
            tac.Assign("B", tac.IntVal(1)),
            tac.Assign(
                "A",
                tac.Comprehension(
                    "X",
                    tac.ObjVar("Y"),
                    [tac.Assign("T_0", tac.Add(tac.IntVar("A"), tac.IntVal(1)))],
                    tac.IntVar("T_0"),
                    [tac.Assign("T_1", tac.Greater(tac.IntVar("B"), tac.IntVar("0")))],
                    tac.BoolVar("T_1"),
                ),
            ),
            tac.Assign("B", tac.Div(tac.IntVar("A"), tac.IntVar("B"))),
        ]
    ) == [
        tac.Assign("S_A_0", tac.IntVal(0)),
        tac.Assign("S_B_0", tac.IntVal(1)),
        tac.Assign(
            "S_A_1",
            tac.Comprehension(
                "X",
                tac.ObjVar("Y"),
                [tac.Assign("T_0", tac.Add(tac.IntVar("S_A_0"), tac.IntVal(1)))],
                tac.IntVar("T_0"),
                [tac.Assign("T_1", tac.Greater(tac.IntVar("S_B_0"), tac.IntVar("0")))],
                tac.BoolVar("T_1"),
            ),
        ),
        tac.Assign("S_B_1", tac.Div(tac.IntVar("S_A_1"), tac.IntVar("S_B_0"))),
    ]
