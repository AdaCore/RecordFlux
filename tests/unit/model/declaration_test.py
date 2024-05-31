import pytest

from rflx import expr
from rflx.identifier import id_generator
from rflx.model import declaration as decl
from rflx.rapidflux import Location, RecordFluxError


def test_renaming_declaration_to_ir() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:1:2: error: renaming declarations not yet supported$",
    ):
        decl.RenamingDeclaration(
            "X",
            "Y",
            expr.Selected(expr.Variable("M"), "F"),
            location=Location((1, 2)),
        ).to_ir(id_generator())
