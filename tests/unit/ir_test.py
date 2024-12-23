from __future__ import annotations

from collections.abc import Callable

import pytest
import z3

from rflx import expr, ir, ty
from rflx.error import Location
from rflx.identifier import ID, id_generator
from rflx.rapidflux import NO_LOCATION

PROOF_MANAGER = ir.ProofManager(2)
INT_TY = ty.Integer("I", ty.Bounds(10, 100))
ENUM_TY = ty.Enumeration("E", [ID("E1"), ID("E2")])
MSG_TY = ty.Message("M", {("E", "I")}, {}, {ID("E"): ENUM_TY, ID("I"): INT_TY})
SEQ_TY = ty.Sequence("S", MSG_TY)


def test_constructed_origin_location() -> None:
    assert ir.ConstructedOrigin("X", Location((1, 2))).location == Location((1, 2))


def test_proof_manager() -> None:
    PROOF_MANAGER.add(
        [ir.ProofJob([ir.Check(ir.BoolVal(value=True))], results={ir.ProofResult.SAT: [1]})],
    )
    assert PROOF_MANAGER.check()[0].result == [1]


def test_stmt_location() -> None:
    assert ir.Assign(
        "X",
        ir.IntVar("Y", INT_TY),
        INT_TY,
        origin=expr.Variable(ID("X", location=Location((1, 2)))),
    ).location == Location((1, 2))


def test_var_decl() -> None:
    var_decl = ir.VarDecl("X", INT_TY)
    assert var_decl.identifier == ID("X")
    assert var_decl.type_ == INT_TY
    assert var_decl.expression is None


def test_var_decl_accessed_vars() -> None:
    assert ir.VarDecl("X", INT_TY).accessed_vars == []


def test_var_decl_to_z3_expr() -> None:
    assert ir.VarDecl("X", MSG_TY).to_z3_expr() == z3.BoolVal(val=True)
    assert ir.VarDecl("X", INT_TY).to_z3_expr() == z3.And(
        z3.Int("I'First") == z3.IntVal(10),
        z3.Int("I'Last") == z3.IntVal(100),
        z3.Int("X") >= z3.Int("I'First"),
        z3.Int("X") <= z3.Int("I'Last"),
    )


def test_assign() -> None:
    assign = ir.Assign("X", ir.IntVar("Y", INT_TY), INT_TY)
    assert assign.target == ID("X")
    assert assign.expression == ir.IntVar("Y", INT_TY)


def test_assign_accessed_vars() -> None:
    assert ir.Assign("X", ir.IntVar("Y", INT_TY), INT_TY).accessed_vars == [ID("Y")]


def test_assign_str() -> None:
    assert str(ir.Assign("X", ir.IntVar("Y", INT_TY), INT_TY)) == "X := Y"


def test_assign_substituted() -> None:
    assert ir.Assign("X", ir.IntVar("Y", INT_TY), INT_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.Assign("Y", ir.IntVar("Z", INT_TY), INT_TY)


def test_assign_to_z3_expr() -> None:
    assert ir.Assign("X", ir.IntVar("Y", INT_TY), INT_TY).to_z3_expr() == (
        z3.Int("X") == z3.Int("Y")
    )
    assert ir.Assign("X", ir.BoolVar("Y"), ty.BOOLEAN).to_z3_expr() == (
        z3.Bool("X") == z3.Bool("Y")
    )
    assert ir.Assign("X", ir.ObjVar("Y", MSG_TY), MSG_TY).to_z3_expr() == z3.BoolVal(val=True)


def test_assign_target_var() -> None:
    assert ir.Assign("X", ir.IntVar("Y", INT_TY), INT_TY).target_var == ir.IntVar("X", INT_TY)
    assert ir.Assign("X", ir.BoolVar("Y"), ty.BOOLEAN).target_var == ir.BoolVar("X")
    assert ir.Assign("X", ir.ObjVar("Y", MSG_TY), MSG_TY).target_var == ir.ObjVar("X", MSG_TY)


def test_field_assign_str() -> None:
    assert str(ir.FieldAssign("X", "Y", ir.IntVar("Z", INT_TY), MSG_TY)) == "X.Y := Z"


def test_field_assign_type() -> None:
    assert ir.FieldAssign("X", "Y", ir.IntVar("Z", INT_TY), MSG_TY).type_ == MSG_TY


def test_field_assign_accessed_vars() -> None:
    assert ir.FieldAssign("X", "Y", ir.IntVar("Z", INT_TY), MSG_TY).accessed_vars == [
        ID("X"),
        ID("Z"),
    ]


def test_field_assign_substituted() -> None:
    assert ir.FieldAssign("X", "Y", ir.IntVar("Z", INT_TY), MSG_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
            ID("Z"): ID("A"),
        },
    ) == ir.FieldAssign("Y", "Y", ir.IntVar("A", INT_TY), MSG_TY)


def test_field_assign_preconditions() -> None:
    assert ir.FieldAssign("X", "Y", ir.IntVar("Z", INT_TY), MSG_TY).preconditions(
        id_generator(),
    ) == [
        ir.Cond(ir.FieldValidNext("X", "Y", MSG_TY)),
        ir.Cond(ir.SufficientSpace("X", "Y", MSG_TY)),
    ]


def test_field_assign_to_z3_expr() -> None:
    assert ir.FieldAssign("X", "Y", ir.IntVar("Z", INT_TY), MSG_TY).to_z3_expr() == (
        z3.Int("X.Y") == z3.Int("Z")
    )
    assert ir.FieldAssign("X", "Y", ir.BoolVar("Z"), MSG_TY).to_z3_expr() == (
        z3.Bool("X.Y") == z3.Bool("Z")
    )
    assert ir.FieldAssign("X", "Y", ir.ObjVar("Z", MSG_TY), MSG_TY).to_z3_expr() == z3.BoolVal(
        val=True,
    )


def test_append_str() -> None:
    assert str(ir.Append("X", ir.IntVar("Y", INT_TY), SEQ_TY)) == "X'Append (Y)"


def test_append_accessed_vars() -> None:
    assert ir.Append("X", ir.IntVar("Y", INT_TY), SEQ_TY).accessed_vars == [
        ID("X"),
        ID("Y"),
    ]


def test_append_substituted() -> None:
    assert ir.Append("X", ir.IntVar("Y", INT_TY), SEQ_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.Append("Y", ir.IntVar("Z", INT_TY), SEQ_TY)


def test_append_preconditions() -> None:
    assert ir.Append("X", ir.IntVar("Y", INT_TY), SEQ_TY).preconditions(id_generator()) == [
        ir.Cond(ir.Valid("X", SEQ_TY)),
        ir.Cond(ir.HasElement("X", SEQ_TY)),
    ]


def test_append_to_z3_expr() -> None:
    assert ir.Append("X", ir.ObjVar("Y", MSG_TY), SEQ_TY).to_z3_expr() == z3.BoolVal(val=True)


def test_extend_str() -> None:
    assert str(ir.Extend("X", ir.IntVar("Y", INT_TY), SEQ_TY)) == "X'Extend (Y)"


def test_extend_accessed_vars() -> None:
    assert ir.Extend("X", ir.IntVar("Y", INT_TY), SEQ_TY).accessed_vars == [
        ID("X"),
        ID("Y"),
    ]


def test_extend_substituted() -> None:
    assert ir.Extend("X", ir.IntVar("Y", INT_TY), SEQ_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.Extend("Y", ir.IntVar("Z", INT_TY), SEQ_TY)


def test_extend_preconditions() -> None:
    assert ir.Extend("X", ir.IntVar("Y", INT_TY), SEQ_TY).preconditions(id_generator()) == [
        ir.Cond(ir.Valid("X", SEQ_TY)),
    ]


def test_extend_to_z3_expr() -> None:
    assert ir.Extend("X", ir.ObjVar("Y", SEQ_TY), SEQ_TY).to_z3_expr() == z3.BoolVal(val=True)


def test_reset_str() -> None:
    assert str(ir.Reset("X", {}, MSG_TY)) == "X'Reset"
    assert str(ir.Reset("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY)) == "X'Reset (Y => Z)"


def test_reset_accessed_vars() -> None:
    assert ir.Reset("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).accessed_vars == [
        ID("X"),
        ID("Z"),
    ]


def test_reset_substituted() -> None:
    assert ir.Reset("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
            ID("Z"): ID("A"),
        },
    ) == ir.Reset("Y", {ID("Y"): ir.IntVar("A", INT_TY)}, MSG_TY)


def test_reset_preconditions() -> None:
    assert not ir.Reset("X", {}, MSG_TY).preconditions(id_generator())
    assert not ir.Reset("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).preconditions(
        id_generator(),
    )


def test_reset_to_z3_expr() -> None:
    assert ir.Reset("X", {}, MSG_TY).to_z3_expr() == z3.BoolVal(val=True)
    assert ir.Reset("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).to_z3_expr() == z3.BoolVal(
        val=True,
    )


def test_read_str() -> None:
    assert str(ir.Read("X", ir.IntVar("Y", INT_TY))) == "X'Read (Y)"


def test_read_accessed_vars() -> None:
    assert ir.Read("X", ir.IntVar("Y", INT_TY)).accessed_vars == [
        ID("Y"),
    ]


def test_read_substituted() -> None:
    assert ir.Read("X", ir.IntVar("Y", INT_TY)).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.Read("X", ir.IntVar("Z", INT_TY))


def test_read_preconditions() -> None:
    assert not ir.Read("X", ir.IntVar("Y", INT_TY)).preconditions(id_generator())


def test_read_to_z3_expr() -> None:
    assert ir.Read("X", ir.ObjVar("Y", MSG_TY)).to_z3_expr() == z3.BoolVal(val=True)


def test_write_str() -> None:
    assert str(ir.Write("X", ir.IntVar("Y", INT_TY))) == "X'Write (Y)"


def test_write_accessed_vars() -> None:
    assert ir.Read("X", ir.IntVar("Y", INT_TY)).accessed_vars == [
        ID("Y"),
    ]


def test_write_substituted() -> None:
    assert ir.Write("X", ir.IntVar("Y", INT_TY)).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.Write("X", ir.IntVar("Z", INT_TY))


def test_write_preconditions() -> None:
    assert not ir.Write("X", ir.IntVar("Y", INT_TY)).preconditions(id_generator())


def test_write_z3_expr() -> None:
    assert ir.Write("X", ir.ObjVar("Y", MSG_TY)).to_z3_expr() == z3.BoolVal(val=True)


def test_check_accessed_vars() -> None:
    assert ir.Check(ir.BoolVar("X")).accessed_vars == [
        ID("X"),
    ]


def test_check_substituted() -> None:
    assert ir.Check(ir.BoolVar("X")).substituted(
        {
            ID("X"): ID("Y"),
        },
    ) == ir.Check(ir.BoolVar("Y"))


def test_check_to_z3_expr() -> None:
    assert ir.Check(ir.BoolVar("X")).to_z3_expr() == z3.Bool("X")


def test_check_preconditions() -> None:
    assert not ir.Check(ir.BoolVar("X")).preconditions(id_generator())


def test_expr_location() -> None:
    assert ir.IntVar(
        "X",
        INT_TY,
        origin=expr.Variable(ID("X", location=Location((1, 2)))),
    ).location == Location((1, 2))


def test_expr_origin_str() -> None:
    assert (
        ir.IntVar(
            "X",
            INT_TY,
            origin=expr.Variable("Y"),
        ).origin_str
        == "Y"
    )


def test_int_var_identifier() -> None:
    assert ir.IntVar("X", INT_TY).identifier == ID("X")


def test_int_var_type() -> None:
    assert ir.IntVar("X", INT_TY).type_ == INT_TY


def test_int_var_accessed_vars() -> None:
    assert ir.IntVar("X", INT_TY).accessed_vars == [
        ID("X"),
    ]


def test_int_var_substituted() -> None:
    assert ir.IntVar("X", INT_TY).substituted({}) == ir.IntVar("X", INT_TY)
    assert ir.IntVar("X", INT_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.IntVar("Y", INT_TY)


def test_int_var_to_z3_expr() -> None:
    assert ir.IntVar("X", INT_TY).to_z3_expr() == z3.Int("X")


def test_int_var_preconditions() -> None:
    assert not ir.IntVar("X", INT_TY).preconditions(id_generator())


def test_bool_var_identifier() -> None:
    assert ir.BoolVar("X").identifier == ID("X")


def test_bool_var_type() -> None:
    assert ir.BoolVar("X").type_ == ty.BOOLEAN


def test_bool_var_accessed_vars() -> None:
    assert ir.BoolVar("X", INT_TY).accessed_vars == [
        ID("X"),
    ]


def test_bool_var_substituted() -> None:
    assert ir.BoolVar("X").substituted({}) == ir.BoolVar("X")
    assert ir.BoolVar("X").substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.BoolVar("Y")


def test_bool_var_to_z3_expr() -> None:
    assert ir.BoolVar("X").to_z3_expr() == z3.Bool("X")


def test_bool_var_preconditions() -> None:
    assert not ir.BoolVar("X").preconditions(id_generator())


def test_obj_var_identifier() -> None:
    assert ir.ObjVar("X", ENUM_TY).identifier == ID("X")


def test_obj_var_type() -> None:
    assert ir.ObjVar("X", ENUM_TY).type_ == ENUM_TY


def test_obj_var_accessed_vars() -> None:
    assert ir.ObjVar("X", INT_TY).accessed_vars == [
        ID("X"),
    ]


def test_obj_var_substituted() -> None:
    assert ir.ObjVar("X", ENUM_TY).substituted({}) == ir.ObjVar("X", ENUM_TY)
    assert ir.ObjVar("X", ENUM_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.ObjVar("Y", ENUM_TY)


def test_obj_var_to_z3_expr() -> None:
    assert ir.ObjVar("X", MSG_TY).to_z3_expr() == z3.BoolVal(val=True)


def test_enum_lit_str() -> None:
    assert str(ir.EnumLit("Lit", ENUM_TY)) == "Lit"


def test_enum_lit_type() -> None:
    assert ir.EnumLit("Lit", ENUM_TY).type_ == ENUM_TY


def test_enum_lit_accessed_vars() -> None:
    assert ir.EnumLit("Lit", ENUM_TY).accessed_vars == []


def test_enum_lit_substituted() -> None:
    assert ir.EnumLit("X", ENUM_TY).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.EnumLit("X", ENUM_TY)


def test_enum_lit_to_z3_expr() -> None:
    assert ir.EnumLit("Lit", ENUM_TY).to_z3_expr() == z3.Int("Lit")


def test_int_val_str() -> None:
    assert str(ir.IntVal(1)) == "1"


def test_int_val_accessed_vars() -> None:
    assert ir.IntVal(1).accessed_vars == []


def test_int_val_to_z3_expr() -> None:
    assert ir.IntVal(1).to_z3_expr() == z3.IntVal(1)


def test_bool_val_str() -> None:
    assert str(ir.BoolVal(value=True)) == "True"
    assert str(ir.BoolVal(value=False)) == "False"


def test_bool_val_accessed_vars() -> None:
    assert ir.BoolVal(value=True).accessed_vars == []


def test_bool_val_to_z3_expr() -> None:
    assert ir.BoolVal(value=True).to_z3_expr() == z3.BoolVal(val=True)
    assert ir.BoolVal(value=False).to_z3_expr() == z3.BoolVal(val=False)


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.Size("X", MSG_TY), "X'Size"),
        (ir.Length("X", MSG_TY), "X'Length"),
        (ir.First("X", MSG_TY), "X'First"),
        (ir.Last("X", MSG_TY), "X'Last"),
        (ir.ValidChecksum("X", MSG_TY), "X'Valid_Checksum"),
        (ir.Valid("X", MSG_TY), "X'Valid"),
        (ir.Present("X", MSG_TY), "X'Present"),
        (ir.HasData("X", MSG_TY), "X'Has_Data"),
        (ir.Head("X", SEQ_TY), "X'Head"),
        (ir.Opaque("X", MSG_TY), "X'Opaque"),
    ],
)
def test_attr_str(attribute: ir.Attr, expected: str) -> None:
    assert str(attribute) == expected


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.Size("X", MSG_TY), ty.BIT_LENGTH),
        (ir.Size("X", ENUM_TY), ty.UNIVERSAL_INTEGER),
        (ir.Length("X", MSG_TY), ty.UNIVERSAL_INTEGER),
        (ir.First("X", MSG_TY), ty.UNIVERSAL_INTEGER),
        (ir.Last("X", MSG_TY), ty.UNIVERSAL_INTEGER),
        (ir.ValidChecksum("X", MSG_TY), ty.BOOLEAN),
        (ir.Valid("X", MSG_TY), ty.BOOLEAN),
        (ir.Present("X", MSG_TY), ty.BOOLEAN),
        (ir.HasData("X", MSG_TY), ty.BOOLEAN),
        (ir.Head("X", SEQ_TY), MSG_TY),
        (ir.Opaque("X", MSG_TY), ty.OPAQUE),
    ],
)
def test_attr_type(attribute: ir.Attr, expected: ty.Type) -> None:
    assert attribute.type_ == expected


@pytest.mark.parametrize(
    ("attribute"),
    [
        (ir.Size("X", MSG_TY)),
        (ir.Length("X", MSG_TY)),
        (ir.First("X", MSG_TY)),
        (ir.Last("X", MSG_TY)),
        (ir.ValidChecksum("X", MSG_TY)),
        (ir.Valid("X", MSG_TY)),
        (ir.Present("X", MSG_TY)),
        (ir.HasData("X", MSG_TY)),
        (ir.Head("X", SEQ_TY)),
        (ir.Opaque("X", MSG_TY)),
    ],
)
def test_attr_accessed_vars(attribute: ir.Attr) -> None:
    assert attribute.accessed_vars == [ID("X")]


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.Size("X", MSG_TY), ir.Size("Y", MSG_TY)),
        (ir.Length("X", MSG_TY), ir.Length("Y", MSG_TY)),
        (ir.First("X", MSG_TY), ir.First("Y", MSG_TY)),
        (ir.Last("X", MSG_TY), ir.Last("Y", MSG_TY)),
        (ir.ValidChecksum("X", MSG_TY), ir.ValidChecksum("Y", MSG_TY)),
        (ir.Valid("X", MSG_TY), ir.Valid("Y", MSG_TY)),
        (ir.Present("X", MSG_TY), ir.Present("Y", MSG_TY)),
        (ir.HasData("X", MSG_TY), ir.HasData("Y", MSG_TY)),
        (ir.Head("X", SEQ_TY), ir.Head("Y", SEQ_TY)),
        (ir.Opaque("X", MSG_TY), ir.Opaque("Y", MSG_TY)),
    ],
)
def test_attr_substituted(attribute: ir.Attr, expected: ir.Attr) -> None:
    assert (
        attribute.substituted(
            {
                ID("X"): ID("Y"),
                ID("Y"): ID("Z"),
            },
        )
        == expected
    )


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.Size("X", MSG_TY), z3.Int("X'Size")),
        (ir.Length("X", MSG_TY), z3.Int("X'Length")),
        (ir.First("X", MSG_TY), z3.Int("X'First")),
        (ir.Last("X", MSG_TY), z3.Int("X'Last")),
        (ir.ValidChecksum("X", MSG_TY), z3.Bool("X'Valid_Checksum")),
        (ir.Valid("X", MSG_TY), z3.Bool("X'Valid")),
        (ir.Present("X", MSG_TY), z3.Bool("X'Present")),
        (ir.HasData("X", MSG_TY), z3.Bool("X'Has_Data")),
    ],
)
def test_attr_z3_expr(attribute: ir.Attr, expected: z3.ExprRef) -> None:
    assert attribute.prefix == ID("X")
    assert attribute.to_z3_expr() == expected


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.FieldValidNext("X", "Y", MSG_TY), "X.Y'Valid_Next"),
        (ir.FieldValid("X", "Y", MSG_TY), "X.Y'Valid"),
        (ir.FieldPresent("X", "Y", MSG_TY), "X.Y'Present"),
        (ir.FieldSize("X", "Y", MSG_TY), "X.Y'Size"),
    ],
)
def test_field_access_attr_str(attribute: ir.FieldAccessAttr, expected: str) -> None:
    assert str(attribute) == expected


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.FieldValidNext("X", "Y", MSG_TY), ty.BOOLEAN),
        (ir.FieldValid("X", "Y", MSG_TY), ty.BOOLEAN),
        (ir.FieldPresent("X", "Y", MSG_TY), ty.BOOLEAN),
        (ir.FieldSize("X", "Y", MSG_TY), ty.BIT_LENGTH),
    ],
)
def test_field_access_attr_type(attribute: ir.FieldAccessAttr, expected: ty.Type) -> None:
    assert attribute.type_ == expected


def test_field_access_attr_field_type() -> None:
    assert ir.FieldValid("X", "I", MSG_TY).field_type == INT_TY


@pytest.mark.parametrize(
    ("attribute"),
    [
        ir.FieldValidNext("X", "Y", MSG_TY),
        ir.FieldValid("X", "Y", MSG_TY),
        ir.FieldPresent("X", "Y", MSG_TY),
        ir.FieldSize("X", "Y", MSG_TY),
    ],
)
def test_field_access_attr_accessed_vars(attribute: ir.FieldAccessAttr) -> None:
    assert attribute.accessed_vars == [ID("X")]


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.FieldValidNext("X", "Y", MSG_TY), ir.FieldValidNext("Y", "Y", MSG_TY)),
        (ir.FieldValid("X", "Y", MSG_TY), ir.FieldValid("Y", "Y", MSG_TY)),
        (ir.FieldPresent("X", "Y", MSG_TY), ir.FieldPresent("Y", "Y", MSG_TY)),
        (ir.FieldSize("X", "Y", MSG_TY), ir.FieldSize("Y", "Y", MSG_TY)),
    ],
)
def test_field_access_attr_substituted(
    attribute: ir.FieldAccessAttr,
    expected: ir.FieldAccessAttr,
) -> None:
    assert (
        attribute.substituted(
            {
                ID("X"): ID("Y"),
                ID("Y"): ID("Z"),
            },
        )
        == expected
    )


@pytest.mark.parametrize(
    ("attribute", "expected"),
    [
        (ir.FieldValidNext("X", "Y", MSG_TY), z3.Bool("X.Y'Valid_Next")),
        (ir.FieldValid("X", "Y", MSG_TY), z3.Bool("X.Y'Valid")),
        (ir.FieldPresent("X", "Y", MSG_TY), z3.Bool("X.Y'Present")),
        (ir.FieldSize("X", "Y", MSG_TY), z3.Int("X.Y'Size")),
    ],
)
def test_field_access_attr_z3_expr(attribute: ir.FieldAccessAttr, expected: z3.ExprRef) -> None:
    assert attribute.to_z3_expr() == expected


@pytest.mark.parametrize(
    "binary_expr",
    [
        ir.Add,
        ir.Sub,
        ir.Mul,
        ir.Div,
        ir.Pow,
        ir.Mod,
        ir.And,
        ir.Or,
        ir.Less,
        ir.LessEqual,
        ir.Equal,
        ir.GreaterEqual,
        ir.Greater,
        ir.NotEqual,
    ],
)
def test_binary_expr_location(binary_expr: type[ir.BinaryExpr]) -> None:
    assert binary_expr(ir.IntVar("X", INT_TY), ir.IntVal(1)).location == NO_LOCATION
    assert binary_expr(
        ir.IntVar("X", INT_TY),
        ir.IntVal(1),
        origin=ir.ConstructedOrigin("Z", Location((1, 2))),
    ).location == Location((1, 2))
    assert binary_expr(
        ir.IntVar("X", INT_TY, origin=ir.ConstructedOrigin("X", Location((1, 2)))),
        ir.IntVal(1),
    ).location == Location((1, 2))


@pytest.mark.parametrize(
    "binary_expr",
    [
        ir.Add,
        ir.Sub,
        ir.Mul,
        ir.Div,
        ir.Pow,
        ir.Mod,
        ir.And,
        ir.Or,
        ir.Less,
        ir.LessEqual,
        ir.Equal,
        ir.GreaterEqual,
        ir.Greater,
        ir.NotEqual,
    ],
)
def test_binary_expr_origin_str(binary_expr: type[ir.BinaryExpr]) -> None:
    assert binary_expr(ir.IntVar("X", INT_TY), ir.IntVal(1)).origin_str == str(
        binary_expr(ir.IntVar("X", INT_TY), ir.IntVal(1)),
    )
    assert (
        binary_expr(
            ir.IntVar("X", INT_TY),
            ir.IntVal(1),
            origin=ir.ConstructedOrigin("Z", NO_LOCATION),
        ).origin_str
        == "Z"
    )


@pytest.mark.parametrize(
    "binary_expr",
    [
        ir.Add,
        ir.Sub,
        ir.Mul,
        ir.Div,
        ir.Pow,
        ir.Mod,
        ir.And,
        ir.Or,
        ir.Less,
        ir.LessEqual,
        ir.Equal,
        ir.GreaterEqual,
        ir.Greater,
        ir.NotEqual,
    ],
)
def test_binary_expr_accessed_vars(binary_expr: type[ir.BinaryExpr]) -> None:
    assert binary_expr(ir.IntVar("X", INT_TY), ir.IntVar("Y", INT_TY)).accessed_vars == [
        ID("X"),
        ID("Y"),
    ]
    assert (
        binary_expr(
            ir.IntVar("X", INT_TY),
            ir.IntVal(1),
            origin=ir.ConstructedOrigin("Z", NO_LOCATION),
        ).origin_str
        == "Z"
    )


@pytest.mark.parametrize(
    "binary_expr",
    [
        ir.Add,
        ir.Sub,
        ir.Mul,
        ir.Div,
        ir.Pow,
        ir.Mod,
        ir.And,
        ir.Or,
        ir.Less,
        ir.LessEqual,
        ir.Equal,
        ir.GreaterEqual,
        ir.Greater,
        ir.NotEqual,
    ],
)
def test_binary_expr_substituted(
    binary_expr: Callable[[ir.BasicIntExpr, ir.BasicIntExpr], ir.Relation],
) -> None:
    assert binary_expr(ir.IntVar("X", INT_TY), ir.IntVal(1)).substituted(
        {
            ID("X"): ID("Y"),
        },
    ) == binary_expr(ir.IntVar("Y", INT_TY), ir.IntVal(1))


def test_neg_str() -> None:
    assert str(ir.Neg(ir.IntVar("X", INT_TY))) == "-X"


def test_neg_type() -> None:
    assert ir.Neg(ir.IntVar("X", INT_TY)).type_ == INT_TY


def test_neg_accessed_vars() -> None:
    assert ir.Neg(ir.IntVar("X", INT_TY)).accessed_vars == [ID("X")]


def test_neg_to_z3_expr() -> None:
    assert ir.Neg(ir.IntVar("X", INT_TY)).to_z3_expr() == -z3.Int("X")


def test_add_str() -> None:
    assert str(ir.Add(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X + 1"


def test_add_to_z3_expr() -> None:
    assert ir.Add(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (z3.Int("X") + z3.IntVal(1))


def test_add_preconditions() -> None:
    assert ir.Add(
        ir.IntVar("X", ty.BASE_INTEGER),
        ir.IntVal(1),
        origin=expr.Add(expr.Variable("X"), expr.Number(1)),
    ).preconditions(id_generator()) == [
        ir.Cond(
            ir.LessEqual(ir.IntVar("X", INT_TY), ir.IntVar("T_0", INT_TY)),
            [
                ir.VarDecl("T_0", ty.BASE_INTEGER),
                ir.Assign("T_0", ir.Sub(ir.IntVal(ir.INT_MAX), ir.IntVal(1)), ty.BASE_INTEGER),
            ],
        ),
    ]


def test_sub_str() -> None:
    assert str(ir.Sub(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X - 1"


def test_sub_to_z3_expr() -> None:
    assert ir.Sub(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (z3.Int("X") - z3.IntVal(1))


def test_sub_preconditions() -> None:
    assert ir.Sub(ir.IntVar("X", INT_TY), ir.IntVal(1)).preconditions(id_generator()) == [
        ir.Cond(
            ir.GreaterEqual(ir.IntVar("X", INT_TY), ir.IntVal(1)),
        ),
    ]


def test_mul_str() -> None:
    assert str(ir.Mul(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X * 1"


def test_mul_to_z3_expr() -> None:
    assert ir.Mul(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (z3.Int("X") * z3.IntVal(1))


def test_mul_preconditions() -> None:
    assert ir.Mul(ir.IntVar("X", INT_TY), ir.IntVal(1)).preconditions(id_generator()) == [
        ir.Cond(
            ir.LessEqual(ir.IntVar("X", INT_TY), ir.IntVar("T_0", INT_TY)),
            [
                ir.VarDecl("T_0", ty.BASE_INTEGER),
                ir.Assign("T_0", ir.Div(ir.IntVal(ir.INT_MAX), ir.IntVal(1)), ty.BASE_INTEGER),
            ],
        ),
    ]


def test_div_str() -> None:
    assert str(ir.Div(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X / 1"


def test_div_to_z3_expr() -> None:
    assert ir.Div(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (z3.Int("X") / z3.IntVal(1))


def test_div_preconditions() -> None:
    assert ir.Div(ir.IntVar("X", INT_TY), ir.IntVal(1)).preconditions(id_generator()) == [
        ir.Cond(ir.NotEqual(ir.IntVal(1), ir.IntVal(0))),
    ]


def test_pow_str() -> None:
    assert str(ir.Pow(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X ** 1"


def test_pow_to_z3_expr() -> None:
    assert ir.Pow(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") ** z3.IntVal(1)
    )


def test_pow_preconditions() -> None:
    assert ir.Pow(ir.IntVar("X", INT_TY), ir.IntVal(1)).preconditions(id_generator()) == [
        ir.Cond(
            ir.LessEqual(ir.IntVar("T_0", INT_TY), ir.IntVal(ir.INT_MAX)),
            [
                ir.VarDecl("T_0", ty.BASE_INTEGER),
                ir.Assign("T_0", ir.Pow(ir.IntVar("X", INT_TY), ir.IntVal(1)), ty.BASE_INTEGER),
            ],
        ),
    ]


def test_mod_str() -> None:
    assert str(ir.Mod(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X mod 1"


def test_mod_to_z3_expr() -> None:
    assert ir.Mod(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (z3.Int("X") % z3.IntVal(1))


def test_mod_preconditions() -> None:
    assert ir.Mod(ir.IntVar("X", INT_TY), ir.IntVal(1)).preconditions(id_generator()) == [
        ir.Cond(ir.NotEqual(ir.IntVal(1), ir.IntVal(0))),
    ]


@pytest.mark.parametrize(
    "unary_expr",
    [
        ir.Not,
    ],
)
def test_unary_expr_substituted(
    unary_expr: Callable[[ir.BasicIntExpr, ir.BasicIntExpr], ir.Relation],
) -> None:
    assert unary_expr(ir.IntVar("X", INT_TY), ir.IntVal(1)).substituted(
        {
            ID("X"): ID("Y"),
        },
    ) == unary_expr(ir.IntVar("Y", INT_TY), ir.IntVal(1))


def test_not_str() -> None:
    assert str(ir.Not(ir.BoolVar("X"))) == "not X"


def test_not_to_z3_expr() -> None:
    assert ir.Not(ir.BoolVar("X")).to_z3_expr() == z3.Not(z3.Bool("X"))


def test_and_str() -> None:
    assert str(ir.And(ir.BoolVar("X"), ir.BoolVal(value=True))) == "X and True"


def test_and_to_z3_expr() -> None:
    assert ir.And(ir.BoolVar("X"), ir.BoolVal(value=True)).to_z3_expr() == z3.And(
        z3.Bool("X"),
        z3.BoolVal(val=True),
    )


def test_or_str() -> None:
    assert str(ir.Or(ir.BoolVar("X"), ir.BoolVal(value=True))) == "X or True"


def test_or_to_z3_expr() -> None:
    assert ir.Or(ir.BoolVar("X"), ir.BoolVal(value=True)).to_z3_expr() == z3.Or(
        z3.Bool("X"),
        z3.BoolVal(val=True),
    )


def test_less_str() -> None:
    assert str(ir.Less(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X < 1"


def test_less_to_z3_expr() -> None:
    assert ir.Less(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") < z3.IntVal(1)
    )


def test_less_equal_str() -> None:
    assert str(ir.LessEqual(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X <= 1"


def test_less_equal_to_z3_expr() -> None:
    assert ir.LessEqual(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") <= z3.IntVal(1)
    )


def test_equal_str() -> None:
    assert str(ir.Equal(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X = 1"


def test_equal_to_z3_expr() -> None:
    assert ir.Equal(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") == z3.IntVal(1)
    )


def test_greater_equal_str() -> None:
    assert str(ir.GreaterEqual(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X >= 1"


def test_greater_equal_to_z3_expr() -> None:
    assert ir.GreaterEqual(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") >= z3.IntVal(1)
    )


def test_greater_str() -> None:
    assert str(ir.Greater(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X > 1"


def test_greater_to_z3_expr() -> None:
    assert ir.Greater(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") > z3.IntVal(1)
    )


def test_not_equal_str() -> None:
    assert str(ir.NotEqual(ir.IntVar("X", INT_TY), ir.IntVal(1))) == "X /= 1"


def test_not_equal_to_z3_expr() -> None:
    assert ir.NotEqual(ir.IntVar("X", INT_TY), ir.IntVal(1)).to_z3_expr() == (
        z3.Int("X") != z3.IntVal(1)
    )


def test_int_call_str() -> None:
    assert (
        str(ir.IntCall("X", [ir.IntVar("Y", INT_TY), ir.BoolVal(value=True)], [], INT_TY))
        == "X (Y, True)"
    )


def test_int_call_substituted() -> None:
    assert ir.IntCall("X", [ir.IntVar("Y", INT_TY)], [], INT_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")},
    ) == ir.IntCall("X", [ir.IntVar("Z", INT_TY)], [], INT_TY)


def test_int_call_to_z3_expr() -> None:
    assert ir.IntCall(
        "X",
        [ir.IntVar("Y", INT_TY), ir.BoolVal(value=True)],
        [],
        INT_TY,
    ).to_z3_expr() == z3.Int("X")


def test_bool_call_str() -> None:
    assert str(ir.BoolCall("X", [ir.BoolVar("Y"), ir.BoolVal(value=True)], [])) == "X (Y, True)"


def test_bool_call_type() -> None:
    assert ir.BoolCall("X", [ir.BoolVar("Y")], [], ir.BoolVal(value=True)).type_ == ty.BOOLEAN


def test_bool_call_substituted() -> None:
    assert ir.BoolCall("X", [ir.BoolVar("Y")], []).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")},
    ) == ir.BoolCall("X", [ir.BoolVar("Z")], [])


def test_bool_call_to_z3_expr() -> None:
    assert ir.BoolCall("X", [ir.BoolVar("Y")], [], ir.BoolVal(value=True)).to_z3_expr() == z3.Bool(
        "X",
    )


def test_obj_call_str() -> None:
    assert (
        str(ir.ObjCall("X", [ir.ObjVar("Y", MSG_TY), ir.BoolVal(value=True)], [], ENUM_TY))
        == "X (Y, True)"
    )


def test_obj_call_type() -> None:
    assert (
        ir.ObjCall("X", [ir.ObjVar("Y", MSG_TY), ir.BoolVal(value=True)], [], ENUM_TY).type_
        == ENUM_TY
    )


def test_obj_call_substituted() -> None:
    assert ir.ObjCall("X", [ir.BoolVar("Y")], [], ENUM_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z")},
    ) == ir.ObjCall("X", [ir.BoolVar("Z")], [], ENUM_TY)


def test_call_accessed_vars() -> None:
    assert ir.IntCall(
        "X",
        [ir.IntVar("Y", INT_TY), ir.BoolVal(value=True)],
        [],
        INT_TY,
    ).accessed_vars == [ID("Y")]


def test_call_preconditions() -> None:
    call = ir.IntCall("X", [ir.IntVar("Y", INT_TY), ir.BoolVal(value=True)], [], INT_TY)
    assert not call.preconditions(id_generator())
    call.set_preconditions([ir.Cond(ir.Greater(ir.IntVar("Y", INT_TY), ir.IntVal(0)))])
    assert call.preconditions(id_generator()) == [
        ir.Cond(ir.Greater(ir.IntVar("Y", INT_TY), ir.IntVal(0))),
    ]


def test_int_field_access_str() -> None:
    assert str(ir.IntFieldAccess("M", "F", MSG_TY)) == "M.F"


def test_int_field_access_type() -> None:
    assert ir.IntFieldAccess("M", "I", MSG_TY).type_ == INT_TY


def test_int_field_access_accessed_vars() -> None:
    assert ir.IntFieldAccess("M", "I", MSG_TY).accessed_vars == [ID("M")]


def test_int_field_access_substituted() -> None:
    assert ir.IntFieldAccess("M", "F", MSG_TY).substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")},
    ) == ir.IntFieldAccess("X", "F", MSG_TY)


def test_int_field_access_to_z3_expr() -> None:
    assert ir.IntFieldAccess("M", "F", MSG_TY).to_z3_expr() == z3.Int("M.F")


def test_bool_field_access_str() -> None:
    assert str(ir.BoolFieldAccess("M", "F", MSG_TY)) == "M.F"


def test_bool_field_access_substituted() -> None:
    assert ir.BoolFieldAccess("M", "F", MSG_TY).substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")},
    ) == ir.BoolFieldAccess("X", "F", MSG_TY)


def test_bool_field_access_to_z3_expr() -> None:
    assert ir.BoolFieldAccess("M", "F", MSG_TY).to_z3_expr() == z3.Bool("M.F")


def test_obj_field_access_str() -> None:
    assert str(ir.ObjFieldAccess("M", "F", MSG_TY)) == "M.F"


def test_obj_field_access_type() -> None:
    assert ir.ObjFieldAccess("M", "E", MSG_TY).type_ == ENUM_TY


def test_obj_field_access_substituted() -> None:
    assert ir.ObjFieldAccess("M", "F", MSG_TY).substituted(
        {ID("M"): ID("X"), ID("F"): ID("Y")},
    ) == ir.ObjFieldAccess("X", "F", MSG_TY)


def test_obj_field_access_to_z3_expr() -> None:
    with pytest.raises(NotImplementedError):
        ir.ObjFieldAccess("M", "F", MSG_TY).to_z3_expr()


def test_int_if_expr_str() -> None:
    assert (
        str(
            ir.IntIfExpr(
                ir.BoolVar("X"),
                ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
                ir.ComplexIntExpr([], ir.IntVal(1)),
                INT_TY,
            ),
        )
        == "(if X then {Y} else {1})"
    )


def test_int_if_expr_type() -> None:
    assert (
        ir.IntIfExpr(
            ir.BoolVar("X"),
            ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
            ir.ComplexIntExpr([], ir.IntVal(1)),
            INT_TY,
        ).type_
        == INT_TY
    )


def test_int_if_expr_accessed_vars() -> None:
    assert ir.IntIfExpr(
        ir.BoolVar("X"),
        ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
        ir.ComplexIntExpr([], ir.IntVal(1)),
        INT_TY,
    ).accessed_vars == [ID("X"), ID("Y")]


def test_int_if_expr_substituted() -> None:
    assert ir.IntIfExpr(
        ir.BoolVar("X"),
        ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
        ir.ComplexIntExpr([], ir.IntVal(1)),
        INT_TY,
    ).substituted({ID("X"): ID("Y"), ID("Y"): ID("Z")}) == ir.IntIfExpr(
        ir.BoolVar("Y"),
        ir.ComplexIntExpr([], ir.IntVar("Z", INT_TY)),
        ir.ComplexIntExpr([], ir.IntVal(1)),
        INT_TY,
    )


def test_int_if_expr_to_z3_expr() -> None:
    assert ir.IntIfExpr(
        ir.BoolVar("X"),
        ir.ComplexIntExpr([], ir.IntVar("Y", INT_TY)),
        ir.ComplexIntExpr([], ir.IntVal(1)),
        INT_TY,
    ).to_z3_expr() == z3.If(z3.Bool("X"), z3.Int("Y"), z3.IntVal(1))


def test_bool_if_expr_str() -> None:
    assert (
        str(
            ir.BoolIfExpr(
                ir.BoolVar("X"),
                ir.ComplexBoolExpr([], ir.BoolVar("Y")),
                ir.ComplexBoolExpr([], ir.BoolVal(value=False)),
            ),
        )
        == "(if X then {Y} else {False})"
    )


def test_bool_if_expr_type() -> None:
    assert (
        ir.BoolIfExpr(
            ir.BoolVar("X"),
            ir.ComplexBoolExpr([], ir.BoolVar("Y")),
            ir.ComplexBoolExpr([], ir.BoolVal(value=False)),
        ).type_
        == ty.BOOLEAN
    )


def test_bool_if_expr_accessed_vars() -> None:
    assert ir.BoolIfExpr(
        ir.BoolVar("X"),
        ir.ComplexBoolExpr([], ir.BoolVar("Y")),
        ir.ComplexBoolExpr([], ir.BoolVal(value=False)),
    ).accessed_vars == [ID("X"), ID("Y")]


def test_bool_if_expr_substituted() -> None:
    assert ir.BoolIfExpr(
        ir.BoolVar("X"),
        ir.ComplexBoolExpr([], ir.BoolVar("Y")),
        ir.ComplexBoolExpr([], ir.BoolVal(value=False)),
    ).substituted({ID("X"): ID("Y"), ID("Y"): ID("Z")}) == ir.BoolIfExpr(
        ir.BoolVar("Y"),
        ir.ComplexBoolExpr([], ir.BoolVar("Z")),
        ir.ComplexBoolExpr([], ir.BoolVal(value=False)),
    )


def test_bool_if_expr_to_z3_expr() -> None:
    assert ir.BoolIfExpr(
        ir.BoolVar("X"),
        ir.ComplexBoolExpr([], ir.BoolVar("Y")),
        ir.ComplexBoolExpr([], ir.BoolVal(value=False)),
    ).to_z3_expr() == z3.If(z3.Bool("X"), z3.Bool("Y"), z3.BoolVal(val=False))


def test_conversion_str() -> None:
    assert str(ir.Conversion(INT_TY, ir.IntVar("Y", INT_TY))) == "I (Y)"


def test_conversion_type() -> None:
    assert ir.Conversion(INT_TY, ir.IntVar("Y", INT_TY)).type_ == INT_TY


def test_conversion_accessed_vars() -> None:
    assert ir.Conversion(INT_TY, ir.IntVar("Y", INT_TY)).accessed_vars == [ID("Y")]


def test_conversion_substituted() -> None:
    assert ir.Conversion(INT_TY, ir.IntVar("Y", INT_TY)).substituted(
        {ID("I"): ID("Y"), ID("Y"): ID("Z")},
    ) == ir.Conversion(INT_TY, ir.IntVar("Z", INT_TY))


def test_conversion_to_z3_expr() -> None:
    assert ir.Conversion(INT_TY, ir.IntVar("Y", INT_TY)).to_z3_expr() == z3.BoolVal(val=True)


def test_int_conversion_str() -> None:
    assert str(ir.IntConversion(INT_TY, ir.IntVar("Y", INT_TY))) == "I (Y)"


def test_int_conversion_type() -> None:
    assert ir.IntConversion(INT_TY, ir.IntVar("Y", INT_TY)).type_ == INT_TY


def test_int_conversion_substituted() -> None:
    assert ir.IntConversion(INT_TY, ir.IntVar("Y", INT_TY)).substituted(
        {ID("I"): ID("Y"), ID("Y"): ID("Z")},
    ) == ir.IntConversion(INT_TY, ir.IntVar("Z", INT_TY))


def test_int_conversion_to_z3_expr() -> None:
    assert ir.IntConversion(INT_TY, ir.IntVar("Y", INT_TY)).to_z3_expr() == z3.Int("Y")


def test_comprehension_str() -> None:
    assert (
        str(
            ir.Comprehension(
                "X",
                ir.ObjVar("Y", SEQ_TY),
                ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
                ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
            ),
        )
        == "[for X in Y if {True} => {X}]"
    )
    assert (
        str(
            ir.Comprehension(
                "X",
                ir.ObjVar("Y", SEQ_TY),
                ir.ComplexExpr(
                    [ir.Assign("A", ir.Add(ir.IntVar("X", INT_TY), ir.IntVal(1)), INT_TY)],
                    ir.IntVar("A", INT_TY),
                ),
                ir.ComplexBoolExpr(
                    [
                        ir.Assign(
                            "B",
                            ir.Greater(ir.IntVar("X", INT_TY), ir.IntVar("0", INT_TY)),
                            INT_TY,
                        ),
                    ],
                    ir.BoolVar("B"),
                ),
            ),
        )
        == "[for X in Y if {B := X > 0; B} => {A := X + 1; A}]"
    )


def test_comprehension_type() -> None:
    assert ir.Comprehension(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).type_ == ty.Aggregate(MSG_TY)


def test_comprehension_accessed_vars() -> None:
    assert ir.Comprehension(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).accessed_vars == [ID("Y"), ID("X")]


def test_comprehension_substituted() -> None:
    assert ir.Comprehension(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).substituted({ID("X"): ID("Y"), ID("Y"): ID("Z")}) == ir.Comprehension(
        "Y",
        ir.ObjVar("Z", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("Y", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    )


def test_comprehension_precondition() -> None:
    assert not ir.Comprehension(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).preconditions(id_generator())
    assert not ir.Comprehension(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr(
            [ir.Assign("A", ir.Add(ir.IntVar("X", INT_TY), ir.IntVal(1)), INT_TY)],
            ir.IntVar("A", INT_TY),
        ),
        ir.ComplexBoolExpr(
            [ir.Assign("B", ir.Greater(ir.IntVar("X", INT_TY), ir.IntVar("0", INT_TY)), INT_TY)],
            ir.BoolVar("B"),
        ),
    ).preconditions(id_generator())


def test_find_str() -> None:
    assert (
        str(
            ir.Find(
                "X",
                ir.ObjVar("Y", SEQ_TY),
                ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
                ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
            ),
        )
        == "Find (for X in Y if {True} => {X})"
    )
    assert (
        str(
            ir.Find(
                "X",
                ir.ObjVar("Y", SEQ_TY),
                ir.ComplexExpr(
                    [ir.Assign("A", ir.Add(ir.IntVar("X", INT_TY), ir.IntVal(1)), INT_TY)],
                    ir.IntVar("A", INT_TY),
                ),
                ir.ComplexBoolExpr(
                    [
                        ir.Assign(
                            "B",
                            ir.Greater(ir.IntVar("X", INT_TY), ir.IntVar("0", INT_TY)),
                            INT_TY,
                        ),
                    ],
                    ir.BoolVar("B"),
                ),
            ),
        )
        == "Find (for X in Y if {B := X > 0; B} => {A := X + 1; A})"
    )


def test_find_type() -> None:
    assert (
        ir.Find(
            "X",
            ir.ObjVar("Y", SEQ_TY),
            ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
            ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
        ).type_
        == MSG_TY
    )


def test_find_accessed_vars() -> None:
    assert ir.Find(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).accessed_vars == [ID("Y"), ID("X")]


def test_find_substituted() -> None:
    assert ir.Find(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).substituted({ID("X"): ID("Y"), ID("Y"): ID("Z")}) == ir.Find(
        "Y",
        ir.ObjVar("Z", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("Y", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    )


def test_find_precondition() -> None:
    assert not ir.Find(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr([], ir.ObjVar("X", MSG_TY)),
        ir.ComplexBoolExpr([], ir.BoolVal(value=True)),
    ).preconditions(id_generator())
    assert not ir.Find(
        "X",
        ir.ObjVar("Y", SEQ_TY),
        ir.ComplexExpr(
            [ir.Assign("A", ir.Add(ir.IntVar("X", INT_TY), ir.IntVal(1)), INT_TY)],
            ir.IntVar("A", INT_TY),
        ),
        ir.ComplexBoolExpr(
            [ir.Assign("B", ir.Greater(ir.IntVar("X", INT_TY), ir.IntVar("0", INT_TY)), INT_TY)],
            ir.BoolVar("B"),
        ),
    ).preconditions(id_generator())


def test_agg_str() -> None:
    assert str(ir.Agg([ir.IntVar("X", INT_TY), ir.IntVal(1)])) == "[X, 1]"


def test_agg_type() -> None:
    assert ir.Agg([ir.IntVar("X", INT_TY), ir.IntVal(10)]).type_ == ty.Aggregate(ty.BASE_INTEGER)


def test_agg_accessed_vars() -> None:
    assert ir.Agg([ir.IntVar("X", INT_TY), ir.IntVal(10)]).accessed_vars == [ID("X")]


def test_agg_substituted() -> None:
    assert ir.Agg([ir.IntVar("X", INT_TY), ir.IntVal(1)]).substituted({ID("X"): ID("Y")}) == ir.Agg(
        [ir.IntVar("Y", INT_TY), ir.IntVal(1)],
    )


def test_agg_preconditions() -> None:
    assert not ir.Agg([ir.IntVar("X", INT_TY), ir.IntVal(1)]).preconditions(id_generator())


def test_named_agg_str() -> None:
    assert (
        str(ir.NamedAgg([(ID("X"), ir.IntVar("Z", INT_TY)), (ID("Y"), ir.IntVal(1))]))
        == "[X => Z, Y => 1]"
    )


def test_named_agg_accessed_vars() -> None:
    assert ir.NamedAgg(
        [(ID("X"), ir.IntVar("Z", INT_TY)), (ID("Y"), ir.IntVal(1))],
    ).accessed_vars == [ID("Z")]


def test_str_str() -> None:
    assert str(ir.Str("X")) == '"X"'


def test_str_type() -> None:
    assert ir.Str("X").type_ == ty.OPAQUE


def test_str_accessed_vars() -> None:
    assert ir.Str("X").accessed_vars == []


def test_str_substituted() -> None:
    assert ir.Str("X").substituted({ID("X"): ID("Y")}) == ir.Str("X")


def test_str_preconditions() -> None:
    assert not ir.Str("X").preconditions(id_generator())


def test_msg_agg_str() -> None:
    assert str(ir.MsgAgg("X", {}, MSG_TY)) == "X'(null message)"
    assert str(ir.MsgAgg("X", {ID("Y"): ir.IntVal(1)}, MSG_TY)) == "X'(Y => 1)"


def test_msg_agg_type() -> None:
    assert ir.MsgAgg("X", {}, MSG_TY).type_ == MSG_TY


def test_msg_agg_accessed_vars() -> None:
    assert ir.MsgAgg("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).accessed_vars == [ID("Z")]


def test_msg_agg_substituted() -> None:
    assert ir.MsgAgg("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z"), ID("Z"): ID("A")},
    ) == ir.MsgAgg("X", {ID("Y"): ir.IntVar("A", INT_TY)}, MSG_TY)


def test_msg_agg_preconditions() -> None:
    assert not ir.MsgAgg("X", {ID("Y"): ir.IntVal(1)}, MSG_TY).preconditions(id_generator())


def test_delta_msg_agg_str() -> None:
    assert str(ir.DeltaMsgAgg("X", {}, MSG_TY)) == "X with delta null message"
    assert str(ir.DeltaMsgAgg("X", {ID("Y"): ir.IntVal(1)}, MSG_TY)) == "X with delta Y => 1"


def test_delta_msg_agg_type() -> None:
    assert ir.DeltaMsgAgg("X", {}, MSG_TY).type_ == MSG_TY


def test_delta_msg_agg_accessed_vars() -> None:
    assert ir.DeltaMsgAgg("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).accessed_vars == [ID("Z")]


def test_delta_msg_agg_substituted() -> None:
    assert ir.DeltaMsgAgg("X", {ID("Y"): ir.IntVar("Z", INT_TY)}, MSG_TY).substituted(
        {ID("X"): ID("Y"), ID("Y"): ID("Z"), ID("Z"): ID("A")},
    ) == ir.DeltaMsgAgg("X", {ID("Y"): ir.IntVar("A", INT_TY)}, MSG_TY)


def test_delta_msg_agg_preconditions() -> None:
    assert not ir.DeltaMsgAgg("X", {ID("Y"): ir.IntVal(1)}, MSG_TY).preconditions(id_generator())


def test_case_expr_str() -> None:
    assert str(
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
    ) == ("(case X is\n      when 1 | 3 => 0,\n      when 2 => X)")


def test_case_expr_type() -> None:
    assert (
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
        ).type_
        == INT_TY
    )


def test_case_expr_accessed_vars() -> None:
    assert ir.CaseExpr(
        ir.IntVar("X", INT_TY),
        [
            (
                [ir.IntVal(1), ir.IntVal(3)],
                ir.IntVal(0),
            ),
            (
                [ir.IntVal(2)],
                ir.IntVar("Y", INT_TY),
            ),
        ],
        INT_TY,
    ).accessed_vars == [ID("X"), ID("Y")]


def test_case_expr_substituted() -> None:
    assert ir.CaseExpr(
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
    ).substituted(
        {
            ID("X"): ID("Y"),
            ID("Y"): ID("Z"),
        },
    ) == ir.CaseExpr(
        ir.IntVar("Y", INT_TY),
        [
            (
                [ir.IntVal(1), ir.IntVal(3)],
                ir.IntVal(0),
            ),
            (
                [ir.IntVal(2)],
                ir.IntVar("Y", INT_TY),
            ),
        ],
        INT_TY,
    )


def test_case_expr_precondition() -> None:
    assert not ir.CaseExpr(
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
    ).preconditions(id_generator())


def test_sufficient_space_to_z3_expr() -> None:
    assert ir.SufficientSpace("X", "Y", MSG_TY).to_z3_expr() == z3.Bool("X.Y'Sufficient_Space")


def test_has_element_to_z3_expr() -> None:
    assert ir.HasElement("X", SEQ_TY).to_z3_expr() == z3.Bool("X'Has_Element")


def test_add_required_checks() -> None:
    assert ir.add_required_checks(
        [
            ir.Assign(
                "X",
                ir.Div(
                    ir.IntVar("Y", INT_TY),
                    ir.IntVar("Z", INT_TY),
                    origin=expr.Variable(ID("X", location=Location((1, 2)))),
                ),
                INT_TY,
            ),
        ],
        id_generator(),
    ) == [
        ir.Check(
            ir.NotEqual(
                ir.IntVar("Z", INT_TY),
                ir.IntVal(0),
                origin=expr.Variable(ID("X", location=Location((1, 2)))),
            ),
        ),
        ir.Assign(
            "X",
            ir.Div(
                ir.IntVar("Y", INT_TY),
                ir.IntVar("Z", INT_TY),
                origin=expr.Variable(ID("X", location=Location((1, 2)))),
            ),
            INT_TY,
        ),
    ]
    assert [
        str(s)
        for s in ir.add_required_checks(
            [
                ir.Assign(
                    "A",
                    ir.Add(ir.IntVar("Y", ty.BASE_INTEGER), ir.IntVal(1)),
                    ty.BASE_INTEGER,
                ),
                ir.Assign(
                    "B",
                    ir.Div(ir.IntVar("A", ty.BASE_INTEGER), ir.IntVar("Z", ty.BASE_INTEGER)),
                    ty.BASE_INTEGER,
                ),
                ir.Assign(
                    "X",
                    ir.Sub(ir.IntVar("B", ty.BASE_INTEGER), ir.IntVal(1)),
                    ty.BASE_INTEGER,
                ),
                ir.Assign("Z", ir.IntVal(0), ty.BASE_INTEGER),
                ir.Assign(
                    "C",
                    ir.Add(ir.IntVar("Z", ty.BASE_INTEGER), ir.IntVal(1)),
                    ty.BASE_INTEGER,
                ),
            ],
            id_generator(),
        )
    ] == [
        "Var T_0 : __BUILTINS__::Base_Integer",
        "T_0 := 9223372036854775807 - 1",
        "Check Y <= T_0",
        "A := Y + 1",
        "Check Z /= 0",
        "B := A / Z",
        "Check B >= 1",
        "X := B - 1",
        "Z := 0",
        "Var T_1 : __BUILTINS__::Base_Integer",
        "T_1 := 9223372036854775807 - 1",
        "Check Z <= T_1",
        "C := Z + 1",
    ]


def test_add_conversions() -> None:
    assert [
        str(s)
        for s in ir.add_conversions(
            [
                ir.Assign(
                    "A",
                    ir.Add(ir.IntVar("Y", ty.BASE_INTEGER), ir.IntVal(10)),
                    INT_TY,
                ),
                ir.Assign(
                    "B",
                    ir.Div(ir.IntVar("A", ty.BASE_INTEGER), ir.IntVar("Z", ty.BASE_INTEGER)),
                    INT_TY,
                ),
                ir.Assign(
                    "X",
                    ir.Sub(ir.IntVar("B", ty.BASE_INTEGER), ir.IntVal(10)),
                    INT_TY,
                ),
                ir.Assign("Z", ir.IntVal(10), INT_TY),
                ir.Assign(
                    "C",
                    ir.Add(ir.IntVar("Z", ty.BASE_INTEGER), ir.IntVal(10)),
                    INT_TY,
                ),
            ],
        )
    ] == [
        "A := I (Y) + 10",
        "B := I (A) / I (Z)",
        "X := I (B) - 10",
        "Z := 10",
        "C := I (Z) + 10",
    ]
