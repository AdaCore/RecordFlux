import z3

from rflx import tac
from rflx.identifier import ID


def test_assign() -> None:
    assign = tac.Assign("X", tac.IntVar("Y"))
    assert assign.target == ID("X")
    assert assign.expression == tac.IntVar("Y")


def test_assign_z3expr() -> None:
    assert tac.Assign("X", tac.IntVar("Y")).z3expr() == (z3.Int("X") == z3.Int("Y"))
    assert tac.Assign("X", tac.BoolVar("Y")).z3expr() == (z3.Bool("X") == z3.Bool("Y"))


def test_assert_z3expr() -> None:
    assert tac.Assert(tac.BoolVar("X")).z3expr() == z3.Bool("X")


def test_int_var() -> None:
    var = tac.IntVar("X")
    assert var.identifier == ID("X")


def test_int_var_z3expr() -> None:
    assert tac.IntVar("X").z3expr() == z3.Int("X")


def test_int_var_preconditions() -> None:
    assert not tac.IntVar("X").preconditions


def test_bool_var() -> None:
    var = tac.BoolVar("X")
    assert var.identifier == ID("X")


def test_bool_var_z3expr() -> None:
    assert tac.BoolVar("X").z3expr() == z3.Bool("X")


def test_bool_var_preconditions() -> None:
    assert not tac.BoolVar("X").preconditions


def test_msg_var_z3expr() -> None:
    assert tac.MsgVar("X").z3expr() == z3.Const("X", z3.DeclareSort("Msg"))


def test_seq_var_z3expr() -> None:
    assert tac.SeqVar("X").z3expr() == z3.Const("X", z3.DeclareSort("Seq"))


def test_enum_lit_z3expr() -> None:
    assert tac.EnumLit("Lit").z3expr() == z3.Int("Lit")


def test_int_val_z3expr() -> None:
    assert tac.IntVal(1).z3expr() == z3.IntVal(1)


def test_bool_val_z3expr() -> None:
    assert tac.BoolVal(True).z3expr() == z3.BoolVal(True)
    assert tac.BoolVal(False).z3expr() == z3.BoolVal(False)


def test_add_z3expr() -> None:
    assert tac.Add(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") + z3.IntVal(1))


def test_add_preconditions() -> None:
    assert tac.Add(tac.IntVar("X"), tac.IntVal(1)).preconditions == [
        tac.Assign("D", tac.Sub(tac.IntVar("Type'Last"), tac.IntVal(1))),
        tac.Assert(tac.LessEqual(tac.IntVar("X"), tac.IntVar("D"))),
    ]


def test_sub_z3expr() -> None:
    assert tac.Sub(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") - z3.IntVal(1))


def test_sub_preconditions() -> None:
    assert tac.Sub(tac.IntVar("X"), tac.IntVal(1)).preconditions == [
        tac.Assign("S", tac.Add(tac.IntVar("Type'First"), tac.IntVal(1))),
        tac.Assert(tac.GreaterEqual(tac.IntVar("X"), tac.IntVar("S"))),
    ]


def test_mul_z3expr() -> None:
    assert tac.Mul(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") * z3.IntVal(1))


def test_mul_preconditions() -> None:
    assert tac.Mul(tac.IntVar("X"), tac.IntVal(1)).preconditions == [
        tac.Assign("D", tac.Div(tac.IntVar("Type'Last"), tac.IntVal(1))),
        tac.Assert(tac.LessEqual(tac.IntVar("X"), tac.IntVar("D"))),
    ]


def test_div_z3expr() -> None:
    assert tac.Div(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") / z3.IntVal(1))


def test_div_preconditions() -> None:
    assert tac.Div(tac.IntVar("X"), tac.IntVal(1)).preconditions == [
        tac.Assert(tac.NotEqual(tac.IntVal(1), tac.IntVal(0))),
    ]


def test_pow_z3expr() -> None:
    assert tac.Pow(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") ** z3.IntVal(1))


def test_pow_preconditions() -> None:
    assert tac.Pow(tac.IntVar("X"), tac.IntVal(1)).preconditions == [
        tac.Assign("P", tac.Pow(tac.IntVar("X"), tac.IntVal(1))),
        tac.Assert(tac.LessEqual(tac.IntVar("P"), tac.IntVar("Type'Last"))),
    ]


def test_mod_z3expr() -> None:
    assert tac.Mod(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") % z3.IntVal(1))


def test_mod_preconditions() -> None:
    assert tac.Mod(tac.IntVar("X"), tac.IntVal(1)).preconditions == [
        tac.Assert(tac.NotEqual(tac.IntVal(1), tac.IntVal(0))),
    ]


def test_not_z3expr() -> None:
    assert tac.Not(tac.BoolVar("X")).z3expr() == z3.Not(z3.Bool("X"))


def test_and_z3expr() -> None:
    assert tac.And(tac.BoolVar("X"), tac.BoolVal(True)).z3expr() == z3.And(
        z3.Bool("X"), z3.BoolVal(True)
    )


def test_or_z3expr() -> None:
    assert tac.Or(tac.BoolVar("X"), tac.BoolVal(True)).z3expr() == z3.Or(
        z3.Bool("X"), z3.BoolVal(True)
    )


def test_less_z3expr() -> None:
    assert tac.Less(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") < z3.IntVal(1))


def test_less_equal_z3expr() -> None:
    assert tac.LessEqual(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") <= z3.IntVal(1))


def test_equal_z3expr() -> None:
    assert tac.Equal(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") == z3.IntVal(1))


def test_greater_equal_z3expr() -> None:
    assert tac.GreaterEqual(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (
        z3.Int("X") >= z3.IntVal(1)
    )


def test_greater_z3expr() -> None:
    assert tac.Greater(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") > z3.IntVal(1))


def test_not_equal_z3expr() -> None:
    assert tac.NotEqual(tac.IntVar("X"), tac.IntVal(1)).z3expr() == (z3.Int("X") != z3.IntVal(1))


def test_int_call_z3expr() -> None:
    assert tac.IntCall("X", tac.IntVar("Y"), tac.BoolVal(True)).z3expr() == z3.Int("X")


def test_bool_call_z3expr() -> None:
    assert tac.BoolCall("X", tac.BoolVar("Y"), tac.BoolVal(True)).z3expr() == z3.Bool("X")


def test_call_preconditions() -> None:
    call = tac.IntCall("X", tac.IntVar("Y"), tac.BoolVal(True))
    assert not call.preconditions
    call.preconditions = [tac.Assert(tac.Greater(tac.IntVar("Y"), tac.IntVal(0)))]
    assert call.preconditions == [tac.Assert(tac.Greater(tac.IntVar("Y"), tac.IntVal(0)))]


def test_int_field_access_z3expr() -> None:
    assert tac.IntFieldAccess("M", "F").z3expr() == z3.Int("M.F")


def test_bool_field_access_z3expr() -> None:
    assert tac.BoolFieldAccess("M", "F").z3expr() == z3.Bool("M.F")


def test_int_if_expr_z3expr() -> None:
    assert tac.IntIfExpr(tac.BoolVar("X"), tac.IntVar("Y"), tac.IntVal(1)).z3expr() == z3.If(
        z3.Bool("X"), z3.Int("Y"), z3.IntVal(1)
    )


def test_bool_if_expr_z3expr() -> None:
    assert tac.BoolIfExpr(tac.BoolVar("X"), tac.BoolVar("Y"), tac.BoolVal(False)).z3expr() == z3.If(
        z3.Bool("X"), z3.Bool("Y"), z3.BoolVal(False)
    )
