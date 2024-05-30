from rflx.expr_proof import Proof, ProofResult
from rflx.expression import (
    Less,
    Mod,
    Number,
    Variable,
)


def test_proof_invalid_logic() -> None:
    expr = Less(Mod(Variable("X"), Variable("Y")), Number(100))
    p = Proof(expr, logic="QF_IDL")
    assert p.result == ProofResult.UNKNOWN
    assert p.error == [
        (
            "Benchmark is not in QF_IDL (integer difference logic).",
            None,
        ),
    ]
