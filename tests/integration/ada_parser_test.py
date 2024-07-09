from pathlib import Path

import pytest

from rflx import ada_parser
from rflx.generator import const


def read_spec_and_body(spec: Path) -> str:
    body = spec.with_suffix(".adb")
    return spec.read_text() + (body.read_text() if body.exists() else "")


@pytest.mark.parametrize(
    "data",
    [
        pytest.param(read_spec_and_body(spec), id=str(spec.stem))
        for spec in const.TEMPLATE_DIR.glob("*.ads")
    ],
)
def test_templates(data: str) -> None:
    result = ada_parser.parse(data.replace("{prefix}", ""))
    assert result.ads + result.adb == data, data
