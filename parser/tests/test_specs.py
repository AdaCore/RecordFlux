from pathlib import Path

import librflxlang
import pytest


@pytest.mark.parametrize(
    "spec",
    [*(Path("tests") / "data").rglob("*.rflx")]
    + [*(Path("contrib") / "RecordFlux-specifications").rglob("*.rflx")],
)
def test_file(spec: Path) -> None:
    ctx = librflxlang.AnalysisContext()
    unit = ctx.get_from_file(str(spec))
    del ctx
    if spec.name.startswith("incorrect_"):
        assert len(unit.diagnostics) > 0, f"{spec}"
    else:
        assert len(unit.diagnostics) == 0, f"{spec}: " + "\n".join(
            f"{spec}:{d}" for d in unit.diagnostics
        )
        if unit.root:
            assert unit.root.kind_name == "Specification"
