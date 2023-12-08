from pathlib import Path

import pytest

import rflx_lang as lang
from tests.const import EX_SPEC_DIR, SPEC_DIR


@pytest.mark.parametrize("spec", [*SPEC_DIR.rglob("*.rflx"), *EX_SPEC_DIR.rglob("*.rflx")])
def test_file(spec: Path) -> None:
    ctx = lang.AnalysisContext()
    unit = ctx.get_from_file(str(spec))
    del ctx
    if spec.name.startswith("incorrect_") and not spec.name.startswith("incorrect_name"):
        assert len(unit.diagnostics) > 0, f"{spec}"
    else:
        assert len(unit.diagnostics) == 0, f"{spec}: " + "\n".join(
            f"{spec}:{d}" for d in unit.diagnostics
        )
        if unit.root:
            assert unit.root.kind_name == "Specification"
