import pytest

from rflx import expression as expr
from rflx.error import Location, RecordFluxError
from rflx.identifier import id_generator
from rflx.model import declaration as decl


def test_renaming_declaration_to_ir() -> None:
    with pytest.raises(
        RecordFluxError,
        match=r"^<stdin>:1:2: model: error: renaming declarations not yet supported$",
    ):
        decl.RenamingDeclaration(
            "X",
            "Y",
            expr.Selected(expr.Variable("M"), "F"),
            location=Location((1, 2)),
        ).to_ir(id_generator())
