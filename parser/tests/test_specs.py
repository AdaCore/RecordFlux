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
    assert unit.root is not None, "\n".join(f"{spec}:{d}" for d in unit.diagnostics)
    assert unit.root.kind_name == "PackageDeclarationNode"
