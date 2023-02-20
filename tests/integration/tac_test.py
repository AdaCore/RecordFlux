import pytest

from rflx import expression as expr, tac, typing_ as rty
from rflx.error import Location, RecordFluxError
from rflx.identifier import id_generator
from rflx.model import statement as stmt

PROOF_MANAGER = tac.ProofManager(2)


def test_add_checks() -> None:
    variable_id = id_generator()
    statements = [
        stmt.VariableAssignment("X", expr.Variable("Y")),
        stmt.VariableAssignment(
            "X", expr.Add(expr.Variable("X", type_=rty.Integer("I")), expr.Number(1))
        ),
        stmt.VariableAssignment("X", expr.Number(0)),
    ]
    ir = tac.to_ssa([t for s in statements for t in s.to_tac(variable_id)])
    modified_ir = tac.add_checks(ir, PROOF_MANAGER, variable_id)
    assert modified_ir == [
        tac.Assign("S_X_0", tac.IntVar("Y")),
        tac.Assign("T_2", tac.Sub(tac.IntVal(tac.INT_MAX), tac.IntVal(1))),
        tac.Assert(tac.LessEqual(tac.IntVar("S_X_0"), tac.IntVar("T_2"))),
        tac.Assign("S_X_1", tac.Add(tac.IntVar("S_X_0"), tac.IntVal(1))),
        tac.Assign("S_X_2", tac.IntVal(0)),
    ]


def test_check_preconditions() -> None:
    variable_id = id_generator()
    statements = [
        stmt.VariableAssignment(
            "R",
            expr.Sub(
                expr.Variable("W", type_=rty.Integer("I")),
                expr.Add(
                    expr.Variable("X", type_=rty.Integer("I"), location=Location((2, 3))),
                    expr.Variable("Y", type_=rty.Integer("I"), location=Location((3, 4))),
                    expr.Call(
                        "F", [expr.Variable("Z", type_=rty.Integer("I"))], type_=rty.Integer("I")
                    ),
                ),
                location=Location((1, 2)),
            ),
        ),
    ]
    ir = tac.to_ssa([t for s in statements for t in s.to_tac(variable_id)])
    with pytest.raises(
        RecordFluxError,
        match=(
            r"^"
            r"<stdin>:3:4: model: error: precondition might fail,"
            r" cannot prove Y <= 9223372036854775807 - F \(Z\)\n"
            r"<stdin>:2:3: model: error: precondition might fail,"
            r" cannot prove X <= 9223372036854775807 - Y \+ F \(Z\)\n"
            r"<stdin>:1:2: model: error: precondition might fail,"
            r" cannot prove W >= X \+ Y \+ F \(Z\)"
            r"$"
        ),
    ):
        tac.check_preconditions(ir, PROOF_MANAGER, variable_id).propagate()
