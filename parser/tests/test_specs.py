from pathlib import Path

import librecordfluxdsllang as rflxdsl  # type: ignore
import pytest

ctx = rflxdsl.AnalysisContext()


@pytest.mark.parametrize(
    "spec",
    (Path("tests") / "data").rglob("*.rflx"),
)
def test_file(spec: Path) -> None:
    unit = ctx.get_from_file(str(spec))
    if spec.name.startswith("incorrect_"):
        assert len(unit.diagnostics) > 0, f"{spec}"
    else:
        assert len(unit.diagnostics) == 0, f"{spec}: " + "\n".join(
            f"{spec}:{d}" for d in unit.diagnostics
        )
        if unit.root:
            assert unit.root.kind_name == "Specification"
