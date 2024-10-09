from __future__ import annotations

import re
from pathlib import Path

import pytest

from rflx import ada_parser
from rflx.generator import const


def read_spec_and_body(spec_file: Path) -> tuple[str, str]:
    body_file = spec_file.with_suffix(".adb")
    spec = "".join(
        l for l in spec_file.read_text().splitlines(keepends=True) if re.match(r"^\s*--", l) is None
    )
    body = (
        "".join(
            l
            for l in body_file.read_text().splitlines(keepends=True)
            if re.match(r"^\s*--", l) is None
        )
        if body_file.exists()
        else ""
    )

    return (spec.rstrip("\n"), body.rstrip("\n"))


@pytest.mark.parametrize(
    "data",
    [
        pytest.param(read_spec_and_body(spec), id=str(spec.stem))
        for spec in const.TEMPLATE_DIR.glob("*.ads")
    ],
)
def test_templates(data: tuple[str, str]) -> None:
    spec, body = data
    result = ada_parser.parse(spec + body)

    assert result.ads == spec
    assert result.adb == body
